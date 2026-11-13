//! Reads the fields of every Ethernet/IPv4/TCP-or-UDP header in a corpus of
//! packets, four ways: through the Rust that `Wire_rust` generates from the
//! Wire codecs of `bench/rust/headers.ml`, through `etherparse`, through
//! `smoltcp::wire` and through `pnet_packet`. Each does the work `etherparse::SlicedPacket` does, as
//! `bench/rust/bench.ml` describes: the IPv4 payload bounded by the total
//! length, no transport header read from a fragment, the TCP header length or
//! UDP length bounded by the payload. A packet counts only when every header
//! it has passes, as with etherparse; one that is not IPv4 counts its
//! EtherType. Each reads the same fields and folds them the same way, so
//! every implementation, OCaml ones included, must print the same checksum.
//!
//! Usage: `wire-bench-rust CORPUS SLOT RUNS [FILTER]`, reading CORPUS as
//! SLOT-byte packets (`bench.exe corpus` writes the corpora) and printing one
//! `RESULT <corpus> <name> <cpu best> <cpu p10> <cpu median> <cpu p90>
//! <wall best> <wall p10> <wall median> <wall p90> <checksum>` line
//! (ns/packet) per implementation, over RUNS timed passes; `<corpus>` is the
//! file name without its extension. An optional FILTER runs only the rows
//! whose name contains it.

use std::hint::black_box;
use std::time::Instant;

#[path = "gen/eth.rs"]
mod eth;
#[path = "gen/ipv4.rs"]
mod ipv4;
#[path = "gen/tcp.rs"]
mod tcp;
#[path = "gen/udp.rs"]
mod udp;

const IPV4: u16 = 0x0800;

// The fold every implementation applies to the fields it reads. Only `+` and
// `^`, so its low 62 bits match OCaml's 63-bit `int`.
#[inline(always)]
fn ip_fields(acc: u64, src: u32, dst: u32, ttl: u8, proto: u8, len: u16) -> u64 {
    acc.wrapping_add((src ^ dst) as u64)
        .wrapping_add(ttl as u64 + proto as u64 + len as u64)
}

#[allow(clippy::too_many_arguments)]
#[inline(always)]
fn tcp_fields(acc: u64, sp: u16, dp: u16, seq: u32, ack: u32, win: u16, flags: u16, off: u8) -> u64 {
    acc.wrapping_add(((sp as u64) << 16) | dp as u64)
        .wrapping_add((seq ^ ack) as u64)
        .wrapping_add(win as u64 + flags as u64 + off as u64)
}

#[inline(always)]
fn udp_fields(acc: u64, sp: u16, dp: u16, len: u16) -> u64 {
    acc.wrapping_add(((sp as u64) << 16) | dp as u64).wrapping_add(len as u64)
}

fn wire_rust(corpus: &[u8], slot: usize) -> u64 {
    let mut acc = 0u64;
    for p in corpus.chunks_exact(slot) {
        let Ok(e) = eth::Valid::new(p) else { continue };
        let ty = e.ether_type();
        if ty != IPV4 {
            acc = acc.wrapping_add(ty as u64);
            continue;
        }
        let Ok(ip) = ipv4::Valid::at(p, eth::SIZE) else { continue };
        let hlen = ip.ihl() as usize * 4;
        let len = ip.total_length() as usize;
        if len < hlen || len > p.len() - eth::SIZE {
            continue;
        }
        let a = ip_fields(acc.wrapping_add(ty as u64), ip.src_addr(), ip.dst_addr(), ip.ttl(), ip.protocol(), ip.total_length());
        if ip.flags() & 1 != 0 || ip.fragment_offset() != 0 {
            acc = a;
            continue;
        }
        let off = eth::SIZE + hlen;
        let payload = len - hlen;
        match ip.protocol() {
            6 => {
                let Ok(t) = tcp::Valid::at(p, off) else { continue };
                if t.data_offset() as usize * 4 > payload {
                    continue;
                }
                acc = tcp_fields(a, t.src_port(), t.dst_port(), t.seq(), t.ack(), t.window(), t.flags(), t.data_offset() as u8);
            }
            17 => {
                let Ok(u) = udp::Valid::at(p, off) else { continue };
                if u.length() as usize > payload {
                    continue;
                }
                acc = udp_fields(a, u.src_port(), u.dst_port(), u.length());
            }
            _ => acc = a,
        }
    }
    acc
}

fn etherparse(corpus: &[u8], slot: usize) -> u64 {
    use etherparse::{LinkSlice, NetSlice, SlicedPacket, TransportSlice};
    let mut acc = 0u64;
    for p in corpus.chunks_exact(slot) {
        let Ok(s) = SlicedPacket::from_ethernet(p) else { continue };
        let Some(LinkSlice::Ethernet2(e)) = &s.link else { continue };
        let ty = e.ether_type().0;
        acc = acc.wrapping_add(ty as u64);
        let Some(NetSlice::Ipv4(ip)) = &s.net else { continue };
        let h = ip.header();
        acc = ip_fields(
            acc,
            u32::from_be_bytes(h.source()),
            u32::from_be_bytes(h.destination()),
            h.ttl(),
            h.protocol().0,
            h.total_len(),
        );
        match &s.transport {
            Some(TransportSlice::Tcp(t)) => {
                let flags = (t.ns() as u16) << 8
                    | (t.cwr() as u16) << 7
                    | (t.ece() as u16) << 6
                    | (t.urg() as u16) << 5
                    | (t.ack() as u16) << 4
                    | (t.psh() as u16) << 3
                    | (t.rst() as u16) << 2
                    | (t.syn() as u16) << 1
                    | t.fin() as u16;
                acc = tcp_fields(acc, t.source_port(), t.destination_port(), t.sequence_number(), t.acknowledgment_number(), t.window_size(), flags, t.data_offset());
            }
            Some(TransportSlice::Udp(u)) => {
                acc = udp_fields(acc, u.source_port(), u.destination_port(), u.length());
            }
            _ => {}
        }
    }
    acc
}

fn smoltcp(corpus: &[u8], slot: usize) -> u64 {
    use smoltcp::wire::{EthernetFrame, IpProtocol, Ipv4Packet, TcpPacket, UdpPacket};
    let mut acc = 0u64;
    for p in corpus.chunks_exact(slot) {
        let Ok(e) = EthernetFrame::new_checked(p) else { continue };
        let ty = u16::from(e.ethertype());
        if ty != IPV4 {
            acc = acc.wrapping_add(ty as u64);
            continue;
        }
        let Ok(ip) = Ipv4Packet::new_checked(e.payload()) else { continue };
        // [new_checked] checks the lengths only.
        if ip.version() != 4 {
            continue;
        }
        let proto = ip.next_header();
        let a = ip_fields(acc.wrapping_add(ty as u64), u32::from(ip.src_addr()), u32::from(ip.dst_addr()), ip.hop_limit(), u8::from(proto), ip.total_len());
        // etherparse reads no transport header from a fragment.
        if ip.more_frags() || ip.frag_offset() != 0 {
            acc = a;
            continue;
        }
        match proto {
            IpProtocol::Tcp => {
                let Ok(t) = TcpPacket::new_checked(ip.payload()) else { continue };
                let flags = (t.ns() as u16) << 8
                    | (t.cwr() as u16) << 7
                    | (t.ece() as u16) << 6
                    | (t.urg() as u16) << 5
                    | (t.ack() as u16) << 4
                    | (t.psh() as u16) << 3
                    | (t.rst() as u16) << 2
                    | (t.syn() as u16) << 1
                    | t.fin() as u16;
                acc = tcp_fields(a, t.src_port(), t.dst_port(), t.seq_number().0 as u32, t.ack_number().0 as u32, t.window_len(), flags, t.header_len() / 4);
            }
            IpProtocol::Udp => {
                let Ok(u) = UdpPacket::new_checked(ip.payload()) else { continue };
                acc = udp_fields(a, u.src_port(), u.dst_port(), u.len());
            }
            _ => acc = a,
        }
    }
    acc
}

// pnet_packet: the `#[packet]` derive generates an accessor per field over a
// slice whose length `new` checks against the header's minimum. Like smoltcp,
// it leaves the version, header-length and total-length checks to the caller,
// which makes the same ones etherparse makes; the TCP NS flag is the low bit
// of its `reserved` field.
fn pnet(corpus: &[u8], slot: usize) -> u64 {
    use pnet_packet::ethernet::EthernetPacket;
    use pnet_packet::ipv4::Ipv4Packet;
    use pnet_packet::tcp::TcpPacket;
    use pnet_packet::udp::UdpPacket;
    use pnet_packet::Packet;
    let mut acc = 0u64;
    for p in corpus.chunks_exact(slot) {
        let Some(e) = EthernetPacket::new(p) else { continue };
        let ty = e.get_ethertype().0;
        if ty != IPV4 {
            acc = acc.wrapping_add(ty as u64);
            continue;
        }
        let body = e.payload();
        let Some(ip) = Ipv4Packet::new(body) else { continue };
        let hlen = ip.get_header_length() as usize * 4;
        let len = ip.get_total_length() as usize;
        if ip.get_version() != 4 || hlen < 20 || len < hlen || len > body.len() {
            continue;
        }
        let proto = ip.get_next_level_protocol().0;
        let a = ip_fields(acc.wrapping_add(ty as u64), u32::from(ip.get_source()), u32::from(ip.get_destination()), ip.get_ttl(), proto, ip.get_total_length());
        if ip.get_flags() & 1 != 0 || ip.get_fragment_offset() != 0 {
            acc = a;
            continue;
        }
        let l4 = &body[hlen..len];
        match proto {
            6 => {
                let Some(t) = TcpPacket::new(l4) else { continue };
                let off = t.get_data_offset();
                if off < 5 || off as usize * 4 > l4.len() {
                    continue;
                }
                let flags = ((t.get_reserved() as u16 & 1) << 8) | t.get_flags() as u16;
                acc = tcp_fields(a, t.get_source(), t.get_destination(), t.get_sequence(), t.get_acknowledgement(), t.get_window(), flags, off);
            }
            17 => {
                let Some(u) = UdpPacket::new(l4) else { continue };
                let ulen = u.get_length();
                if ulen < 8 || ulen as usize > l4.len() {
                    continue;
                }
                acc = udp_fields(a, u.get_source(), u.get_destination(), ulen);
            }
            _ => acc = a,
        }
    }
    acc
}

// Process CPU time, user and system, in seconds: `getrusage`, as OCaml's
// `Unix.times` reads it.
fn cpu_time() -> f64 {
    let mut ru = std::mem::MaybeUninit::<libc::rusage>::uninit();
    // SAFETY: getrusage fills the whole struct when it returns 0.
    let ru = unsafe {
        assert_eq!(libc::getrusage(libc::RUSAGE_SELF, ru.as_mut_ptr()), 0);
        ru.assume_init()
    };
    let secs = |t: libc::timeval| t.tv_sec as f64 + t.tv_usec as f64 / 1e6;
    secs(ru.ru_utime) + secs(ru.ru_stime)
}

// Best, 10th percentile, median and 90th percentile.
fn quantiles(mut times: Vec<f64>) -> [f64; 4] {
    times.sort_by(|a, b| a.partial_cmp(b).unwrap());
    let pct = |p: usize| times[(times.len() - 1) * p / 100];
    [times[0], pct(10), pct(50), pct(90)]
}

// One warm-up pass, which also gives the checksum, then RUNS timed passes.
// Each pass reads the wall clock and the process CPU time (user and system)
// around the call; CPU time leaves out the time the process spends
// descheduled. Only rows whose name contains FILTER run, so a process can be
// measured whole for one row. The OCaml harness (bench.ml) does exactly the
// same.
#[allow(clippy::too_many_arguments)]
fn measure(corpus_name: &str, name: &str, filter: &str, corpus: &[u8], slot: usize, runs: usize, f: fn(&[u8], usize) -> u64) {
    if !name.contains(filter) {
        return;
    }
    let packets = (corpus.len() / slot) as f64;
    let sum = f(black_box(corpus), slot);
    let mut cpu = Vec::with_capacity(runs);
    let mut wall = Vec::with_capacity(runs);
    for _ in 0..runs {
        let w0 = Instant::now();
        let c0 = cpu_time();
        black_box(f(black_box(corpus), slot));
        let c1 = cpu_time();
        wall.push(w0.elapsed().as_nanos() as f64 / packets);
        cpu.push((c1 - c0) * 1e9 / packets);
    }
    let [c0, c1, c2, c3] = quantiles(cpu);
    let [w0, w1, w2, w3] = quantiles(wall);
    println!(
        "RESULT\t{corpus_name}\t{name}\t{c0:.2}\t{c1:.2}\t{c2:.2}\t{c3:.2}\t{w0:.2}\t{w1:.2}\t{w2:.2}\t{w3:.2}\t{}",
        sum & ((1 << 62) - 1)
    );
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let path = std::path::Path::new(&args[1]);
    let corpus = std::fs::read(path).expect("corpus");
    let slot: usize = args[2].parse().expect("slot");
    let runs: usize = args[3].parse().expect("runs");
    let filter = args.get(4).map(String::as_str).unwrap_or("");
    assert!(corpus.len() % slot == 0, "the corpus is not a whole number of slots");
    let c = path.file_stem().and_then(|s| s.to_str()).expect("corpus name");
    measure(c, "wire.rust", filter, &corpus, slot, runs, wire_rust);
    measure(c, "etherparse 0.21", filter, &corpus, slot, runs, etherparse);
    measure(c, "smoltcp 0.14", filter, &corpus, slot, runs, smoltcp);
    measure(c, "pnet_packet 0.35", filter, &corpus, slot, runs, pnet);
}
