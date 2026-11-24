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

/// A slot of the table: 64 bytes, laid out as `bench/rust/flow_table.ml` and
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

// The scanner of `Packet_stack.capture`, handing out only the classifier's
// fields.
fn wire_rust_scanner(t: &mut Table, buf: &[u8], frames: Frames) {
    for &(o, n) in frames {
        let Some(f) = flow_scan::scan(&buf[o..o + n]) else { continue };
        if f.ipv4_present {
            let p = if f.ipv4_tcp_present {
                ports(f.ipv4_tcp_src_port, f.ipv4_tcp_dst_port)
            } else if f.ipv4_udp_present {
                ports(f.ipv4_udp_src_port, f.ipv4_udp_dst_port)
            } else {
                0
            };
            t.add4(f.ipv4_src, f.ipv4_dst, f.ipv4_protocol, p, f.ipv4_total_length as u64);
        } else if f.ipv6_present {
            let p = if f.ipv6_tcp_present {
                ports(f.ipv6_tcp_src_port, f.ipv6_tcp_dst_port)
            } else if f.ipv6_udp_present {
                ports(f.ipv6_udp_src_port, f.ipv6_udp_dst_port)
            } else {
                0
            };
            let s = [f.ipv6_src0, f.ipv6_src1, f.ipv6_src2, f.ipv6_src3];
            let d = [f.ipv6_dst0, f.ipv6_dst1, f.ipv6_dst2, f.ipv6_dst3];
            t.add6(s, d, f.ipv6_next_header, p, 40 + f.ipv6_payload_length as u64);
        }
    }
}

// The fixed header codecs, linked here: the ports of the transport header at
// `o`, Some(0) for another protocol, None for a header that fails its checks.
fn linked_ports(p: &[u8], o: usize, proto: u8) -> Option<u32> {
    match proto {
        6 => {
            let t = tcp::Valid::at(p, o).ok()?;
            if o + t.data_offset() as usize * 4 > p.len() {
                return None;
            }
            Some(ports(t.src_port(), t.dst_port()))
        }
        17 => {
            let u = udp::Valid::at(p, o).ok()?;
            Some(ports(u.src_port(), u.dst_port()))
        }
        _ => Some(0),
    }
}

fn wire_rust_linked(t: &mut Table, buf: &[u8], frames: Frames) {
    for &(o, n) in frames {
        let p = &buf[o..o + n];
        let Ok(e) = eth::Valid::new(p) else { continue };
        match e.ether_type() {
            IPV4 => {
                let Ok(ip) = ipv4::Valid::at(p, eth::SIZE) else { continue };
                let hlen = ip.ihl() as usize * 4;
                let total = ip.total_length();
                if (total as usize) < hlen || eth::SIZE + hlen > p.len() {
                    continue;
                }
                let proto = ip.protocol();
                let pt = if ip.flags() & 1 != 0 || ip.fragment_offset() != 0 {
                    0
                } else {
                    let Some(pt) = linked_ports(p, eth::SIZE + hlen, proto) else { continue };
                    pt
                };
                t.add4(ip.src_addr(), ip.dst_addr(), proto, pt, total as u64);
            }
            IPV6 => {
                let Ok(ip) = ipv6::Valid::at(p, eth::SIZE) else { continue };
                let proto = ip.next_header();
                let Some(pt) = linked_ports(p, eth::SIZE + ipv6::SIZE, proto) else { continue };
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
fn etherparse(t: &mut Table, buf: &[u8], frames: Frames) {
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
fn smoltcp(t: &mut Table, buf: &[u8], frames: Frames) {
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

// pnet_packet: `new` checks a layer's minimum header length only, so the
// adapter makes the other checks, as in the header benchmark.
fn pnet(t: &mut Table, buf: &[u8], frames: Frames) {
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

type Row = fn(&mut Table, &[u8], Frames);

pub fn main(args: &[String]) {
    let path = std::path::Path::new(&args[0]);
    let buf = std::fs::read(path).expect("capture");
    let runs: usize = args[1].parse().expect("runs");
    let filter = timing::Filter::new(args.get(2).map(String::as_str));
    let frames = index(&buf);
    let mut table = Table::new(2 * frames.len());
    let c = path.file_stem().and_then(|s| s.to_str()).expect("capture name");
    let rows: [(&str, Row); 5] = [
        ("wire.rust scanner, selected outputs", wire_rust_scanner),
        ("wire.rust, linked headers", wire_rust_linked),
        ("etherparse 0.21, lax", etherparse),
        ("smoltcp 0.14", smoltcp),
        ("pnet_packet 0.35", pnet),
    ];
    for (name, f) in rows {
        let pass = |t: &mut Table| {
            t.reset();
            f(t, black_box(&buf), &frames);
        };
        timing::measure_state(c, name, &filter, frames.len(), runs, &mut table, pass, Table::summary);
    }
    filter.finish();
}
