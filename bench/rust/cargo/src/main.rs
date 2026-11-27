//! Reads the fields of every Ethernet/IPv4/TCP-or-UDP header in a corpus of
//! packets: through the Rust that `Wire_rust` generates, from the fixed
//! header codecs of `bench/rust/headers.ml` with the layers linked here, and
//! from `Packet_stack.codec` as one module whose fold hands every output, or
//! only the ones the checksum reads, to the checksum visitor of
//! `stack_step.rs` or `selected_step.rs`; through `etherparse`, through `smoltcp::wire`
//! and through `pnet_packet`. Each does the work `etherparse::SlicedPacket` does, as
//! `bench/rust/bench.ml` describes: the IPv4 payload bounded by the total
//! length, no transport header read from a fragment, the TCP header length or
//! UDP length bounded by the payload. A packet counts only when every header
//! it has passes, as with etherparse; one that is not IPv4 counts its
//! EtherType. Each reads the same fields and folds them the same way, so
//! every implementation, OCaml ones included, must print the same checksum.
//!
//! Every row takes the slot size at run time, from the command line, as a
//! reader of fixed-size frames that learns their size from its input does.
//!
//! Usage: `wire-bench-rust CORPUS SLOT RUNS [ROW | --list]`, reading CORPUS as
//! SLOT-byte packets (`bench.exe corpus` writes the corpora) and printing
//! one `RESULT <corpus> <name> <cpu best> <cpu p10> <cpu median> <cpu p90>
//! <wall best> <wall p10> <wall median> <wall p90> <checksum>` line
//! (ns/packet) per implementation, over RUNS timed passes; `<corpus>` is the
//! file name without its extension. An optional ROW runs only the row of
//! that name, and fails when there is none; `--list` prints the row names.
//!
//! `wire-bench-rust flow FILE RUNS [ROW | --list]` runs the flow classifier of
//! `flow.rs` on a capture instead; `wire-bench-flow-parse` runs its parsers
//! without the table.

use std::hint::black_box;

mod timing;

#[path = "gen/eth.rs"]
mod eth;
#[path = "gen/ipv4.rs"]
mod ipv4;
#[path = "gen/ipv6.rs"]
mod ipv6;
#[path = "gen/tcp.rs"]
mod tcp;
#[path = "gen/udp.rs"]
mod udp;
#[path = "gen/stack.rs"]
mod stack;
#[path = "gen/stack_selected.rs"]
mod stack_selected;
#[path = "gen/flow.rs"]
mod flow_scan;
mod selected_step;
mod stack_step;

// This binary runs `flow::main` only.
#[allow(dead_code)]
mod flow;

const IPV4: u16 = 0x0800;

// The fold every implementation applies to the fields it reads. Only `+` and
// `^`, so its low 62 bits match OCaml's 63-bit `int`.
fn ip_fields(acc: u64, src: u32, dst: u32, ttl: u8, proto: u8, len: u16) -> u64 {
    acc.wrapping_add((src ^ dst) as u64)
        .wrapping_add(ttl as u64 + proto as u64 + len as u64)
}

#[allow(clippy::too_many_arguments)]
fn tcp_fields(acc: u64, sp: u16, dp: u16, seq: u32, ack: u32, win: u16, flags: u16, off: u8) -> u64 {
    acc.wrapping_add(((sp as u64) << 16) | dp as u64)
        .wrapping_add((seq ^ ack) as u64)
        .wrapping_add(win as u64 + flags as u64 + off as u64)
}

fn udp_fields(acc: u64, sp: u16, dp: u16, len: u16) -> u64 {
    acc.wrapping_add(((sp as u64) << 16) | dp as u64).wrapping_add(len as u64)
}

fn wire_rust(corpus: &[u8], slot: usize) -> u64 {
    let mut acc = 0u64;
    for p in corpus.chunks_exact(slot) {
        let Some(e) = eth::check(p, 0).get() else { continue };
        let ty = e.ether_type();
        if ty != IPV4 {
            acc = acc.wrapping_add(ty as u64);
            continue;
        }
        let Some(ip) = ipv4::check(p, eth::MIN_SIZE).get() else { continue };
        let hlen = ip.ihl() as usize * 4;
        let len = ip.total_length() as usize;
        if len < hlen || len > p.len() - eth::MIN_SIZE {
            continue;
        }
        let a = ip_fields(acc.wrapping_add(ty as u64), ip.src_addr(), ip.dst_addr(), ip.ttl(), ip.protocol(), ip.total_length());
        if ip.flags() & 1 != 0 || ip.fragment_offset() != 0 {
            acc = a;
            continue;
        }
        let off = eth::MIN_SIZE + hlen;
        let payload = len - hlen;
        match ip.protocol() {
            6 => {
                let Some(t) = tcp::check(p, off).get() else { continue };
                if t.data_offset() as usize * 4 > payload {
                    continue;
                }
                acc = tcp_fields(a, t.src_port(), t.dst_port(), t.seq(), t.ack(), t.window(), t.flags(), t.data_offset() as u8);
            }
            17 => {
                let Some(u) = udp::check(p, off).get() else { continue };
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

// The fold of `Packet_stack.codec` makes every check the linked headers
// make, and hands the fields of each accepted frame to the checksum visitor,
// over the frames of a stride, as the OCaml rows fold them.
fn wire_rust_stack(corpus: &[u8], slot: usize) -> u64 {
    let mut v = stack_step::StackStep(0);
    stack::checksum_frames(&mut v, corpus, 0, corpus.len(), slot);
    v.0
}

fn wire_rust_selected(corpus: &[u8], slot: usize) -> u64 {
    let mut v = selected_step::SelectedStep(0);
    stack_selected::checksum_frames(&mut v, corpus, 0, corpus.len(), slot);
    v.0
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

// Times `f` on the corpus of `slot`-byte packets with `timing::measure`,
// which the OCaml harness's `Bench_lib.measure` mirrors. `f` is the row's
// function itself, so each pass calls it directly.
#[allow(clippy::too_many_arguments)]
fn measure(corpus_name: &str, name: &str, filter: &timing::Filter, corpus: &[u8], slot: usize, runs: usize, f: impl Fn(&[u8], usize) -> u64) {
    timing::measure(corpus_name, name, filter, corpus.len() / slot, runs, || f(black_box(corpus), slot) & ((1 << 62) - 1));
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.get(1).map(String::as_str) == Some("flow") {
        return flow::main(&args[2..]);
    }
    let path = std::path::Path::new(&args[1]);
    let corpus = std::fs::read(path).expect("corpus");
    let slot: usize = args[2].parse().expect("slot");
    let runs: usize = args[3].parse().expect("runs");
    let filter = timing::Filter::new(args.get(4).map(String::as_str));
    assert!(slot > 0 && corpus.len().is_multiple_of(slot), "the corpus is not a whole number of slots");
    let c = path.file_stem().and_then(|s| s.to_str()).expect("corpus name");
    let f = &filter;
    measure(c, "wire.rust, linked headers", f, &corpus, slot, runs, wire_rust);
    measure(c, "wire.rust fused stack", f, &corpus, slot, runs, wire_rust_stack);
    measure(c, "wire.rust selected outputs", f, &corpus, slot, runs, wire_rust_selected);
    measure(c, "etherparse 0.21", f, &corpus, slot, runs, etherparse);
    measure(c, "smoltcp 0.14", f, &corpus, slot, runs, smoltcp);
    measure(c, "pnet_packet 0.35", f, &corpus, slot, runs, pnet);
    filter.finish();
}
