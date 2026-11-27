//! A capture, held in memory, through a flow classifier, as `bench.exe flow`
//! runs it on the OCaml side (`bench/rust/bench.ml` describes the work). Each
//! row parses every packet with the checks of `Packet_stack.capture`: each
//! layer bounded by the bytes captured, each length field covering its header
//! but bounding nothing, and a packet counted only when every header it has
//! passes. Its 5-tuple is hashed into `Table`, which counts the packets and
//! the IP bytes of each flow, and every row prints the number of flows and a
//! checksum of the table, read after the timed passes, which must agree with
//! the OCaml and C rows.
//!
//! Usage: `wire-bench-rust flow FILE RUNS [ROW | --list]`, FILE a pcap file of
//! Ethernet frames, held whole in one buffer with an offset and length index.

use std::fmt;
use std::hint::black_box;

use crate::timing;
use crate::{eth, flow_scan, ipv4, ipv6, tcp, udp};

const IPV4: u16 = 0x0800;
const IPV6: u16 = 0x86dd;
// The 64-bit golden-ratio multiplier, whose low 63 bits the OCaml rows use.
const K: u64 = 0x9E37_79B9_7F4A_7C15;
const MASK62: u64 = (1 << 62) - 1;

fn mix(h: u64, w: u32) -> u64 {
    (h ^ w as u64).wrapping_mul(K)
}

/// What a row hands each packet's key to: `Table` for the classifier, `Sum`
/// to time the parse alone, `Keys` to record the keys, as the sinks of
/// `bench/rust/bench.ml` do.
pub trait Sink {
    fn add4(&mut self, src: u32, dst: u32, proto: u8, ports: u32, bytes: u64);
    fn add6(&mut self, s: [u32; 4], d: [u32; 4], proto: u8, ports: u32, bytes: u64);
}

/// A slot of the table: 64 bytes, laid out as `bench/rust/flow_table.std.ml` and
/// `bench/rust/everparse/stack_c.c` lay theirs out. `meta` is
/// `(version << 8) | protocol`, `ports` `(src << 16) | dst` or 0; an IPv4
/// address is in the first word, the others 0. A slot is live when its epoch is
/// the table's, so no pass clears the table.
#[repr(C)]
#[derive(Clone, Copy, Default)]
struct Slot {
    epoch: u32,
    meta: u32,
    ports: u32,
    src: [u32; 4],
    dst: [u32; 4],
    packets: u32,
    bytes: u64,
    _unused: u64,
}

const _: () = assert!(std::mem::size_of::<Slot>() == 64);

pub struct Table {
    slots: Vec<Slot>,
    mask: usize,
    epoch: u32,
}

impl Table {
    fn new(capacity: usize) -> Table {
        let n = capacity.next_power_of_two();
        Table { slots: vec![Slot::default(); n], mask: n - 1, epoch: 0 }
    }

    fn reset(&mut self) {
        self.epoch = self.epoch.wrapping_add(1);
    }

    fn add(&mut self, h: u64, meta: u32, ports: u32, src: [u32; 4], dst: [u32; 4], bytes: u64) {
        let mut i = (h >> 32) as usize & self.mask;
        loop {
            let s = &mut self.slots[i];
            if s.epoch != self.epoch {
                *s = Slot { epoch: self.epoch, meta, ports, src, dst, packets: 1, bytes, _unused: 0 };
                return;
            }
            if s.meta == meta && s.ports == ports && s.src == src && s.dst == dst {
                s.packets += 1;
                s.bytes += bytes;
                return;
            }
            i = (i + 1) & self.mask;
        }
    }

    fn summary(&self) -> Summary {
        let mut flows = 0;
        let mut sum = 0u64;
        for (i, s) in self.slots.iter().enumerate() {
            if s.epoch != self.epoch {
                continue;
            }
            flows += 1;
            let mut h = mix(mix(0, s.meta), s.ports);
            for w in s.src.iter().chain(s.dst.iter()) {
                h = mix(h, *w);
            }
            let v = h.wrapping_add((s.packets as u64) << 32).wrapping_add(s.bytes);
            sum = sum.wrapping_add(v.wrapping_mul(i as u64 + 1));
        }
        Summary { flows, sum: sum & MASK62 }
    }
}

impl Sink for Table {
    fn add4(&mut self, src: u32, dst: u32, proto: u8, ports: u32, bytes: u64) {
        let meta = 0x400 | proto as u32;
        let h = mix(mix(mix(mix(0, src), dst), ports), meta);
        self.add(h, meta, ports, [src, 0, 0, 0], [dst, 0, 0, 0], bytes);
    }

    fn add6(&mut self, s: [u32; 4], d: [u32; 4], proto: u8, ports: u32, bytes: u64) {
        let meta = 0x600 | proto as u32;
        let mut h = 0;
        for w in s.iter().chain(d.iter()) {
            h = mix(h, *w);
        }
        let h = mix(mix(h, ports), meta);
        self.add(h, meta, ports, s, d, bytes);
    }
}

/// The sink of the rows that time the parse without the table, as
/// `bench/rust/flow_sum.ml`: a key folds into
/// `(w ^ ports) + (proto << 32) + (bytes << 40) + (version << 57)`, `w` the
/// xor of its address words.
#[derive(Default)]
pub struct Sum {
    keys: usize,
    sum: u64,
}

impl Sum {
    fn fold(&mut self, version: u64, proto: u8, ports: u32, bytes: u64, w: u32) {
        self.keys += 1;
        let v = ((w ^ ports) as u64) + ((proto as u64) << 32) + (bytes << 40) + (version << 57);
        self.sum = self.sum.wrapping_add(v);
    }

    fn summary(&self) -> String {
        format!("{} keys, {}", self.keys, self.sum & MASK62)
    }
}

impl Sink for Sum {
    fn add4(&mut self, src: u32, dst: u32, proto: u8, ports: u32, bytes: u64) {
        self.fold(4, proto, ports, bytes, src ^ dst);
    }

    fn add6(&mut self, s: [u32; 4], d: [u32; 4], proto: u8, ports: u32, bytes: u64) {
        let w = s[0] ^ s[1] ^ s[2] ^ s[3] ^ d[0] ^ d[1] ^ d[2] ^ d[3];
        self.fold(6, proto, ports, bytes, w);
    }
}

/// The keys of a pass, recorded once, untimed, for the rows that time the
/// table alone: 12 words per key, as `Flow_keys` of `bench/rust/bench.ml`
/// lays them out (the IP version, the protocol, the ports, the IP length,
/// then the source and destination words).
const STRIDE: usize = 12;

#[derive(Default)]
struct Keys(Vec<u64>);

impl Sink for Keys {
    fn add4(&mut self, src: u32, dst: u32, proto: u8, ports: u32, bytes: u64) {
        self.0.extend([4, proto as u64, ports as u64, bytes, src as u64, 0, 0, 0, dst as u64, 0, 0, 0]);
    }

    fn add6(&mut self, s: [u32; 4], d: [u32; 4], proto: u8, ports: u32, bytes: u64) {
        let w = |a: [u32; 4]| a.map(u64::from);
        self.0.extend([6, proto as u64, ports as u64, bytes]);
        self.0.extend(w(s));
        self.0.extend(w(d));
    }
}

// The keys `keys` of `Keys`, handed to `t` without a parse.
fn replay<S: Sink>(t: &mut S, keys: &[u64]) {
    for k in keys.chunks_exact(STRIDE) {
        let w = |i: usize| k[i] as u32;
        if k[0] == 4 {
            t.add4(w(4), w(8), k[1] as u8, w(2), k[3]);
        } else {
            t.add6([w(4), w(5), w(6), w(7)], [w(8), w(9), w(10), w(11)], k[1] as u8, w(2), k[3]);
        }
    }
}

pub struct Summary {
    flows: usize,
    sum: u64,
}

impl fmt::Display for Summary {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} flows, {}", self.flows, self.sum)
    }
}

fn words(a: [u8; 16]) -> [u32; 4] {
    let w = |i: usize| u32::from_be_bytes([a[i], a[i + 1], a[i + 2], a[i + 3]]);
    [w(0), w(4), w(8), w(12)]
}

fn ports(sp: u16, dp: u16) -> u32 {
    (sp as u32) << 16 | dp as u32
}

/// The frames of a pcap file: the offset and captured length of each.
fn index(buf: &[u8]) -> Vec<(usize, usize)> {
    assert!(buf.len() >= 24, "truncated pcap header");
    let magic = u32::from_le_bytes(buf[0..4].try_into().unwrap());
    assert!(magic == 0xa1b2_c3d4 || magic == 0xa1b2_3c4d, "not a little-endian pcap file");
    assert_eq!(u32::from_le_bytes(buf[20..24].try_into().unwrap()), 1, "not Ethernet");
    let mut frames = Vec::new();
    let mut o = 24;
    while o < buf.len() {
        assert!(o + 16 <= buf.len(), "truncated pcap record");
        let caplen = u32::from_le_bytes(buf[o + 8..o + 12].try_into().unwrap()) as usize;
        assert!(o + 16 + caplen <= buf.len(), "truncated pcap packet");
        frames.push((o + 16, caplen));
        o += 16 + caplen;
    }
    frames
}

type Frames<'a> = &'a [(usize, usize)];

/// The visitor of the fold `classify` of `Packet_stack.capture`, which
/// hands each packet's key to the sink.
pub struct Classify<'s, S: Sink>(pub &'s mut S);

impl<S: Sink> flow_scan::classify::Visitor for Classify<'_, S> {
    #[inline(always)]
    #[allow(clippy::too_many_arguments)]
    fn valid(
        &mut self,
        ipv4_present: bool,
        ipv4_total_length: u16,
        ipv4_protocol: u8,
        ipv4_src: u32,
        ipv4_dst: u32,
        ipv4_tcp_present: bool,
        ipv4_tcp_src_port: u16,
        ipv4_tcp_dst_port: u16,
        ipv4_udp_present: bool,
        ipv4_udp_src_port: u16,
        ipv4_udp_dst_port: u16,
        ipv6_present: bool,
        ipv6_payload_length: u16,
        ipv6_next_header: u8,
        ipv6_src0: u32,
        ipv6_src1: u32,
        ipv6_src2: u32,
        ipv6_src3: u32,
        ipv6_dst0: u32,
        ipv6_dst1: u32,
        ipv6_dst2: u32,
        ipv6_dst3: u32,
        ipv6_tcp_present: bool,
        ipv6_tcp_src_port: u16,
        ipv6_tcp_dst_port: u16,
        ipv6_udp_present: bool,
        ipv6_udp_src_port: u16,
        ipv6_udp_dst_port: u16,
    ) {
        if ipv4_present {
            let p = if ipv4_tcp_present {
                ports(ipv4_tcp_src_port, ipv4_tcp_dst_port)
            } else if ipv4_udp_present {
                ports(ipv4_udp_src_port, ipv4_udp_dst_port)
            } else {
                0
            };
            self.0.add4(ipv4_src, ipv4_dst, ipv4_protocol, p, ipv4_total_length as u64);
        } else if ipv6_present {
            let p = if ipv6_tcp_present {
                ports(ipv6_tcp_src_port, ipv6_tcp_dst_port)
            } else if ipv6_udp_present {
                ports(ipv6_udp_src_port, ipv6_udp_dst_port)
            } else {
                0
            };
            let s = [ipv6_src0, ipv6_src1, ipv6_src2, ipv6_src3];
            let d = [ipv6_dst0, ipv6_dst1, ipv6_dst2, ipv6_dst3];
            self.0.add6(s, d, ipv6_next_header, p, 40 + ipv6_payload_length as u64);
        }
    }

    #[inline(always)]
    fn invalid(&mut self) {}
}

// The fold `classify` of `Packet_stack.capture`, handing out only the
// classifier's fields, over each frame.
fn wire_rust_scanner<S: Sink>(t: &mut S, buf: &[u8], frames: Frames) {
    let mut v = Classify(t);
    for &(o, n) in frames {
        flow_scan::classify(&mut v, buf, o, n);
    }
}

// The fixed header codecs, linked here: the ports of the transport header at
// `o`, Some(0) for another protocol, None for a header that fails its checks.
fn linked_ports(p: &[u8], o: usize, proto: u8) -> Option<u32> {
    match proto {
        6 => {
            let t = tcp::check(p, o).get()?;
            if o + t.data_offset() as usize * 4 > p.len() {
                return None;
            }
            Some(ports(t.src_port(), t.dst_port()))
        }
        17 => {
            let u = udp::check(p, o).get()?;
            Some(ports(u.src_port(), u.dst_port()))
        }
        _ => Some(0),
    }
}

fn wire_rust_linked<S: Sink>(t: &mut S, buf: &[u8], frames: Frames) {
    for &(o, n) in frames {
        let p = &buf[o..o + n];
        let Some(e) = eth::check(p, 0).get() else { continue };
        match e.ether_type() {
            IPV4 => {
                let Some(ip) = ipv4::check(p, eth::MIN_SIZE).get() else { continue };
                let hlen = ip.ihl() as usize * 4;
                let total = ip.total_length();
                if (total as usize) < hlen || eth::MIN_SIZE + hlen > p.len() {
                    continue;
                }
                let proto = ip.protocol();
                let pt = if ip.flags() & 1 != 0 || ip.fragment_offset() != 0 {
                    0
                } else {
                    let Some(pt) = linked_ports(p, eth::MIN_SIZE + hlen, proto) else { continue };
                    pt
                };
                t.add4(ip.src_addr(), ip.dst_addr(), proto, pt, total as u64);
            }
            IPV6 => {
                let Some(ip) = ipv6::check(p, eth::MIN_SIZE).get() else { continue };
                let proto = ip.next_header();
                let Some(pt) = linked_ports(p, eth::MIN_SIZE + ipv6::MIN_SIZE, proto) else { continue };
                let s = [ip.src0(), ip.src1(), ip.src2(), ip.src3()];
                let d = [ip.dst0(), ip.dst1(), ip.dst2(), ip.dst3()];
                t.add6(s, d, proto, pt, 40 + ip.payload_length() as u64);
            }
            _ => {}
        }
    }
}

// etherparse's lax slicing, its API for truncated captures: where a length
// field runs past the capture, the payload is what was captured. A packet
// whose IP, TCP or UDP header fails stops it with an error; the adapter
// drops it there, and makes the two checks lax slicing leaves out, an IPv4
// total length covering the header and a UDP length of at least 8.
fn etherparse<S: Sink>(t: &mut S, buf: &[u8], frames: Frames) {
    use etherparse::err::Layer;
    use etherparse::{LaxNetSlice, LaxSlicedPacket, TransportSlice};
    for &(o, n) in frames {
        let Ok(s) = LaxSlicedPacket::from_ethernet(&buf[o..o + n]) else { continue };
        if let Some((_, Layer::TcpHeader | Layer::UdpHeader)) = s.stop_err {
            continue;
        }
        let pt = match &s.transport {
            Some(TransportSlice::Tcp(t)) => ports(t.source_port(), t.destination_port()),
            Some(TransportSlice::Udp(u)) => {
                if u.length() < 8 {
                    continue;
                }
                ports(u.source_port(), u.destination_port())
            }
            _ => 0,
        };
        match &s.net {
            Some(LaxNetSlice::Ipv4(ip)) => {
                let h = ip.header();
                if (h.total_len() as usize) < h.slice().len() {
                    continue;
                }
                t.add4(u32::from_be_bytes(h.source()), u32::from_be_bytes(h.destination()), h.protocol().0, pt, h.total_len() as u64);
            }
            Some(LaxNetSlice::Ipv6(ip)) => {
                let h = ip.header();
                t.add6(words(h.source()), words(h.destination()), h.next_header().0, pt, 40 + h.payload_length() as u64);
            }
            _ => {}
        }
    }
}

// smoltcp: its checked constructors bound each IP layer by its length field
// (`Ipv4Packet::new_checked` rejects a total length past the buffer), which a
// header-only capture fails. The IPv4, IPv6 and UDP layers are read through
// `new_unchecked` after the adapter's capture-bounded checks; TCP through
// `new_checked`, whose checks are exactly these.
fn smoltcp<S: Sink>(t: &mut S, buf: &[u8], frames: Frames) {
    use smoltcp::wire::{EthernetFrame, Ipv4Packet, Ipv6Packet, TcpPacket, UdpPacket};
    let transport = |l4: &[u8], proto: u8| -> Option<u32> {
        match proto {
            6 => {
                let tp = TcpPacket::new_checked(l4).ok()?;
                Some(ports(tp.src_port(), tp.dst_port()))
            }
            17 => {
                if l4.len() < 8 {
                    return None;
                }
                let u = UdpPacket::new_unchecked(l4);
                if u.len() < 8 {
                    return None;
                }
                Some(ports(u.src_port(), u.dst_port()))
            }
            _ => Some(0),
        }
    };
    for &(o, n) in frames {
        let Ok(e) = EthernetFrame::new_checked(&buf[o..o + n]) else { continue };
        let body = e.payload();
        match u16::from(e.ethertype()) {
            IPV4 => {
                if body.len() < 20 {
                    continue;
                }
                let ip = Ipv4Packet::new_unchecked(body);
                let hlen = ip.header_len() as usize;
                if ip.version() != 4 || hlen < 20 || hlen > body.len() || (ip.total_len() as usize) < hlen {
                    continue;
                }
                let proto = u8::from(ip.next_header());
                let pt = if ip.more_frags() || ip.frag_offset() != 0 {
                    0
                } else {
                    let Some(pt) = transport(&body[hlen..], proto) else { continue };
                    pt
                };
                t.add4(u32::from(ip.src_addr()), u32::from(ip.dst_addr()), proto, pt, ip.total_len() as u64);
            }
            IPV6 => {
                if body.len() < 40 {
                    continue;
                }
                let ip = Ipv6Packet::new_unchecked(body);
                if ip.version() != 6 {
                    continue;
                }
                let proto = u8::from(ip.next_header());
                let Some(pt) = transport(&body[40..], proto) else { continue };
                t.add6(words(ip.src_addr().octets()), words(ip.dst_addr().octets()), proto, pt, 40 + ip.payload_len() as u64);
            }
            _ => {}
        }
    }
}

/// The packets of the swap rows that make no IPv4 flow, counted by kind, as
/// `bench/rust/flow_mix.ml` counts them.
#[derive(Default)]
pub struct Mix {
    ipv6: usize,
    vlan: usize,
    other: usize,
    rejected: usize,
}

impl fmt::Display for Mix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "IPv6 {}, VLAN {}, other {}, IPv4 rejected {}", self.ipv6, self.vlan, self.other, self.rejected)
    }
}

// smoltcp with the checks of the swap rows of `bench/rust/flow_swap.ml`:
// only IPv4 makes a flow, and the other packets are counted by kind. The
// layers are read as in `smoltcp`; its transport is repeated here so that
// the row of the classifier keeps its code.
fn smoltcp_ipv4_flows<S: Sink>(t: &mut S, mix: &mut Mix, buf: &[u8], frames: Frames) {
    use smoltcp::wire::{EthernetFrame, Ipv4Packet, TcpPacket, UdpPacket};
    let transport = |l4: &[u8], proto: u8| -> Option<u32> {
        match proto {
            6 => {
                let tp = TcpPacket::new_checked(l4).ok()?;
                Some(ports(tp.src_port(), tp.dst_port()))
            }
            17 => {
                if l4.len() < 8 {
                    return None;
                }
                let u = UdpPacket::new_unchecked(l4);
                if u.len() < 8 {
                    return None;
                }
                Some(ports(u.src_port(), u.dst_port()))
            }
            _ => Some(0),
        }
    };
    for &(o, n) in frames {
        let Ok(e) = EthernetFrame::new_checked(&buf[o..o + n]) else {
            mix.other += 1;
            continue;
        };
        let body = e.payload();
        match u16::from(e.ethertype()) {
            IPV4 => {
                if body.len() < 20 {
                    mix.rejected += 1;
                    continue;
                }
                let ip = Ipv4Packet::new_unchecked(body);
                let hlen = ip.header_len() as usize;
                if ip.version() != 4 || hlen < 20 || hlen > body.len() || (ip.total_len() as usize) < hlen {
                    mix.rejected += 1;
                    continue;
                }
                let proto = u8::from(ip.next_header());
                let pt = if ip.more_frags() || ip.frag_offset() != 0 {
                    0
                } else {
                    let Some(pt) = transport(&body[hlen..], proto) else {
                        mix.rejected += 1;
                        continue;
                    };
                    pt
                };
                t.add4(u32::from(ip.src_addr()), u32::from(ip.dst_addr()), proto, pt, ip.total_len() as u64);
            }
            IPV6 => mix.ipv6 += 1,
            0x8100 | 0x88a8 | 0x9100 => mix.vlan += 1,
            _ => mix.other += 1,
        }
    }
}

// The name of the smoltcp row of the swap corpora.
const SMOLTCP_IPV4_FLOWS: &str = "smoltcp 0.14, IPv4 flows";

// pnet_packet: `new` checks a layer's minimum header length only, so the
// adapter makes the other checks, as in the header benchmark.
fn pnet<S: Sink>(t: &mut S, buf: &[u8], frames: Frames) {
    use pnet_packet::ethernet::EthernetPacket;
    use pnet_packet::ipv4::Ipv4Packet;
    use pnet_packet::ipv6::Ipv6Packet;
    use pnet_packet::tcp::TcpPacket;
    use pnet_packet::udp::UdpPacket;
    use pnet_packet::Packet;
    let transport = |l4: &[u8], proto: u8| -> Option<u32> {
        match proto {
            6 => {
                let tp = TcpPacket::new(l4)?;
                let off = tp.get_data_offset();
                if off < 5 || off as usize * 4 > l4.len() {
                    return None;
                }
                Some(ports(tp.get_source(), tp.get_destination()))
            }
            17 => {
                let u = UdpPacket::new(l4)?;
                if u.get_length() < 8 {
                    return None;
                }
                Some(ports(u.get_source(), u.get_destination()))
            }
            _ => Some(0),
        }
    };
    for &(o, n) in frames {
        let Some(e) = EthernetPacket::new(&buf[o..o + n]) else { continue };
        let body = e.payload();
        match e.get_ethertype().0 {
            IPV4 => {
                let Some(ip) = Ipv4Packet::new(body) else { continue };
                let hlen = ip.get_header_length() as usize * 4;
                let total = ip.get_total_length();
                if ip.get_version() != 4 || hlen < 20 || hlen > body.len() || (total as usize) < hlen {
                    continue;
                }
                let proto = ip.get_next_level_protocol().0;
                let pt = if ip.get_flags() & 1 != 0 || ip.get_fragment_offset() != 0 {
                    0
                } else {
                    let Some(pt) = transport(&body[hlen..], proto) else { continue };
                    pt
                };
                t.add4(u32::from(ip.get_source()), u32::from(ip.get_destination()), proto, pt, total as u64);
            }
            IPV6 => {
                let Some(ip) = Ipv6Packet::new(body) else { continue };
                if ip.get_version() != 6 {
                    continue;
                }
                let proto = ip.get_next_header().0;
                let Some(pt) = transport(&body[40..], proto) else { continue };
                t.add6(words(ip.get_source().octets()), words(ip.get_destination().octets()), proto, pt, 40 + ip.get_payload_length() as u64);
            }
            _ => {}
        }
    }
}

type Row<S> = fn(&mut S, &[u8], Frames);

// The rows of the classifier over the sink `S`: `Table` in this binary,
// `Sum` in `wire-bench-flow-parse`, which times the parse alone. A binary
// builds the rows for one sink only, so that no crate function a row calls
// has more callers than in a binary of the classifier alone, and the
// compiler inlines it as it would there.
fn rows<S: Sink>() -> [(&'static str, Row<S>); 5] {
    [
        ("wire.rust scanner, selected outputs", wire_rust_scanner::<S>),
        ("wire.rust, linked headers", wire_rust_linked::<S>),
        ("etherparse 0.21, lax", etherparse::<S>),
        ("smoltcp 0.14", smoltcp::<S>),
        ("pnet_packet 0.35", pnet::<S>),
    ]
}

// A capture held in memory, from `FILE RUNS [ROW | --list]`.
struct Capture {
    name: String,
    buf: Vec<u8>,
    frames: Vec<(usize, usize)>,
    runs: usize,
    filter: timing::Filter,
}

fn capture(args: &[String]) -> Capture {
    let path = std::path::Path::new(&args[0]);
    let buf = std::fs::read(path).expect("capture");
    let runs = args[1].parse().expect("runs");
    let filter = timing::Filter::new(args.get(2).map(String::as_str));
    let frames = index(&buf);
    let name = path.file_stem().and_then(|s| s.to_str()).expect("capture name").to_string();
    Capture { name, buf, frames, runs, filter }
}

// The keys of the capture, for the rows that take them from an array.
fn keys(c: &Capture) -> Vec<u64> {
    let mut k = Keys::default();
    wire_rust_scanner(&mut k, &c.buf, &c.frames);
    k.0
}

/// `wire-bench-rust flow FILE RUNS [ROW | --list]`: the classifier, and the
/// table alone on corpus `<capture> table`.
pub fn main(args: &[String]) {
    let c = capture(args);
    let n = c.frames.len();
    let mut table = Table::new(2 * n);
    for (name, f) in rows::<Table>() {
        let pass = |t: &mut Table| {
            t.reset();
            f(t, black_box(&c.buf), &c.frames);
        };
        timing::measure_state(&c.name, name, &c.filter, n, c.runs, &mut table, pass, Table::summary);
    }
    let mut mix = Mix::default();
    let pass = |(t, m): &mut (&mut Table, &mut Mix)| {
        t.reset();
        **m = Mix::default();
        smoltcp_ipv4_flows(*t, *m, black_box(&c.buf), &c.frames);
    };
    let corpus = format!("{} swap", c.name);
    let summary = |(t, m): &(&mut Table, &mut Mix)| format!("{}; {}", t.summary(), m);
    timing::measure_state(&corpus, SMOLTCP_IPV4_FLOWS, &c.filter, n, c.runs, &mut (&mut table, &mut mix), pass, summary);
    let keys = keys(&c);
    let pass = |t: &mut Table| {
        t.reset();
        replay(t, black_box(&keys));
    };
    let corpus = format!("{} table", c.name);
    timing::measure_state(&corpus, "flow table, keys from an array", &c.filter, n, c.runs, &mut table, pass, Table::summary);
    c.filter.finish();
}

/// `wire-bench-flow-parse FILE RUNS [ROW | --list]`: the parse alone, each
/// row handing its keys to `Sum`, on corpus `<capture> parse`.
pub fn parse(args: &[String]) {
    let c = capture(args);
    let n = c.frames.len();
    let corpus = format!("{} parse", c.name);
    let mut sum = Sum::default();
    for (name, f) in rows::<Sum>() {
        let pass = |s: &mut Sum| {
            *s = Sum::default();
            f(s, black_box(&c.buf), &c.frames);
        };
        let name = format!("{name}, parse only");
        timing::measure_state(&corpus, &name, &c.filter, n, c.runs, &mut sum, pass, Sum::summary);
    }
    let pass = |(s, m): &mut (Sum, Mix)| {
        *s = Sum::default();
        *m = Mix::default();
        smoltcp_ipv4_flows(s, m, black_box(&c.buf), &c.frames);
    };
    let swap = format!("{} swap parse", c.name);
    let name = format!("{SMOLTCP_IPV4_FLOWS}, parse only");
    let summary = |(s, m): &(Sum, Mix)| format!("{}; {}", s.summary(), m);
    timing::measure_state(&swap, &name, &c.filter, n, c.runs, &mut (Sum::default(), Mix::default()), pass, summary);
    let keys = keys(&c);
    let pass = |s: &mut Sum| {
        *s = Sum::default();
        replay(s, black_box(&keys));
    };
    timing::measure_state(&corpus, "keys from an array, parse only", &c.filter, n, c.runs, &mut sum, pass, Sum::summary);
    c.filter.finish();
}
