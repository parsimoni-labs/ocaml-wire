//! Reads the fields of every Ethernet/IPv4/TCP-or-UDP header in a corpus of
//! packets, three ways: through the Rust that `Wire_rust` generates from the
//! Wire codecs of `bench/rust/headers.ml`, through `etherparse` and through
//! `smoltcp::wire`. Each does the work `etherparse::SlicedPacket` does, as
//! `bench/rust/bench.ml` describes: the IPv4 payload bounded by the total
//! length, no transport header read from a fragment, the TCP header length or
//! UDP length bounded by the payload. A packet counts only when every header
//! it has passes, as with etherparse; one that is not IPv4 counts its
//! EtherType. Each reads the same fields and folds them the same way, so
//! every implementation, OCaml ones included, must print the same checksum.
//!
//! Usage: `wire-bench-rust CORPUS SLOT RUNS`, printing one
//! `RESULT <name> <best ns/packet> <median ns/packet> <checksum>` line per
//! implementation, over RUNS timed passes.

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

fn measure(name: &str, corpus: &[u8], slot: usize, runs: usize, f: fn(&[u8], usize) -> u64) {
    let packets = corpus.len() / slot;
    let sum = f(black_box(corpus), slot);
    let mut times: Vec<f64> = (0..runs)
        .map(|_| {
            let t = Instant::now();
            black_box(f(black_box(corpus), slot));
            t.elapsed().as_nanos() as f64 / packets as f64
        })
        .collect();
    times.sort_by(|a, b| a.partial_cmp(b).unwrap());
    println!("RESULT\t{}\t{:.2}\t{:.2}\t{}", name, times[0], times[runs / 2], sum & ((1 << 62) - 1));
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let corpus = std::fs::read(&args[1]).expect("corpus");
    let slot: usize = args[2].parse().expect("slot");
    let runs: usize = args[3].parse().expect("runs");
    measure("wire.rust", &corpus, slot, runs, wire_rust);
    measure("etherparse 0.21", &corpus, slot, runs, etherparse);
    measure("smoltcp 0.14", &corpus, slot, runs, smoltcp);
}
