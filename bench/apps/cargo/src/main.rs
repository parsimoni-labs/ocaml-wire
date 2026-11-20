//! The application benchmarks of `bench/routing`, `bench/gateway` and
//! `bench/clcw`, over the Rust that `Wire_rust` generates from the codecs
//! they use (`Space.packet_codec`, `Space.tm_frame_codec`,
//! `Space.clcw_codec`) and over released crates. Each workload runs the
//! application logic of its OCaml workload on the same input, as a user of
//! each library writes it: one safe loop per workload, the rows differing
//! only in the header reader and writer they call.
//!
//! Reading: routing (`Routing_workload.run_packets`), gateway
//! (`Gateway_workload.checksum`) and CLCW (`Clcw_workload.run_all`). The
//! generated Rust reads through `Valid::at`, one bounds check per header,
//! then its accessors, for routing and gateway; for CLCW through one
//! `get_<field>_word` and a `<field>_of_word` per field.
//!
//! Writing: routing re-stamps every packet's header with a sequence count
//! kept per route (`Routing_workload.restamp`), gateway builds TM frames
//! from a packet stream (`Gateway_workload.frame`), and CLCW builds the word
//! a FARM reports for every frame (`Clcw_workload.report`). Each row writes
//! the same bytes into its own output buffer, and its result is the FNV-1a
//! hash of that buffer.
//!
//! Released crates, each the way its docs show:
//! - space packet headers: `spacepackets`, `ccsds_primary_header` and
//!   `gaia-ccsds-c2a`'s `PrimaryHeader`, for routing and for the packets of
//!   gateway;
//! - TM transfer frame primary headers: `chronus-gateway`'s
//!   `TmFrameHeader`, with `spacepackets` for the packets;
//! - CLCW words: `gaia-ccsds-c2a`'s `CLCW`.
//!
//! Every row, OCaml ones included, must print the same result.
//!
//! Usage: `wire-bench-apps [run | write] FILE ITEMS RUNS [ROW | --list]`, as
//! `bench.exe (run | write)`, running the workload the file name of FILE
//! names (`routing`, `gateway` or `clcw`, as `bench.exe inputs` writes them),
//! reading it (`run`, the default) or, with `write`, writing, on
//! its ITEMS items, and printing one `RESULT` line per row over RUNS timed
//! passes. An optional ROW runs only the row of that name, and fails when
//! there is none; `--list` prints the row names.

use std::cell::RefCell;
use std::hint::black_box;
use std::rc::Rc;

#[path = "../../../rust/cargo/src/timing.rs"]
mod timing;

#[path = "gen/clcw.rs"]
mod clcw;
#[path = "gen/space_packet.rs"]
mod space_packet;
#[path = "gen/tm_frame.rs"]
mod tm_frame;

use arbitrary_int::{u11, u14};

// APID demux: as `Routing_workload`, `n` packets from offset 0, wrapping to 0
// when a header does not fit, each counted on the route of its APID.
fn routing_table() -> [u8; 2048] {
    let mut table = [0u8; 2048];
    for (apid, r) in table.iter_mut().enumerate() {
        *r = if apid < 256 {
            0
        } else if apid < 1024 {
            1
        } else if apid < 1536 {
            2
        } else {
            3
        };
    }
    table
}

// The counts of the housekeeping, science, diagnostics and idle routes,
// printed as `Routing_workload.pp_counts` prints them.
struct Counts([u64; 4]);

impl std::fmt::Display for Counts {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let [hk, sci, diag, idle] = self.0;
        write!(f, "(hk={hk} sci={sci} diag={diag} idle={idle})")
    }
}

// The output buffer of a write workload, printed as the FNV-1a hash of its
// bytes, as `Bench_lib` prints the OCaml rows' buffers. The hash is taken
// when the result is printed, outside the timed passes.
struct Written(Rc<RefCell<Vec<u8>>>);

impl std::fmt::Display for Written {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut h = FNV_INIT;
        for &b in self.0.borrow().iter() {
            h = hash_int(h, b as u64);
        }
        write!(f, "{h:016x}")
    }
}

// Runs `f` over the output buffer `out`, cleared once, and gives the buffer
// back as the row's result.
fn written(out: &Rc<RefCell<Vec<u8>>>, f: impl FnOnce(&mut [u8])) -> Written {
    f(&mut out.borrow_mut()[..]);
    Written(out.clone())
}

fn output(len: usize) -> Rc<RefCell<Vec<u8>>> {
    Rc::new(RefCell::new(vec![0u8; len]))
}

// {1 Space packet headers}
//
// `header(buf, off)` reads the APID and data length of the header at `off`,
// which the loop has checked to fit; `stamp(out, off, apid, seq, dlen)`
// writes the header of a telemetry packet with a secondary header,
// unsegmented, of these values at `off`.

fn sp_wire_rust(buf: &[u8], off: usize) -> Option<(u16, u16)> {
    let v = space_packet::Valid::at(buf, off).ok()?;
    Some((v.apid(), v.datalength()))
}

fn sp_spacepackets(buf: &[u8], off: usize) -> Option<(u16, u16)> {
    use spacepackets::{CcsdsPacket, SpHeader};
    let (h, _) = SpHeader::from_be_bytes(&buf[off..]).ok()?;
    Some((h.apid().value(), h.data_len()))
}

fn sp_ccsds_primary_header(buf: &[u8], off: usize) -> Option<(u16, u16)> {
    use ccsds_primary_header::primary_header::PrimaryHeader;
    let h = PrimaryHeader::from_slice(&buf[off..])?;
    Some((h.control.apid(), h.length.length_field()))
}

fn sp_gaia(buf: &[u8], off: usize) -> Option<(u16, u16)> {
    use gaia_ccsds_c2a::ccsds::space_packet::PrimaryHeader;
    let (h, _) = zerocopy::LayoutVerified::<_, PrimaryHeader>::new_unaligned_from_prefix(&buf[off..])?;
    Some((h.apid(), h.packet_data_length_raw()))
}

fn stamp_wire_rust(out: &mut [u8], off: usize, apid: u16, seq: u16, dlen: u16) -> Option<()> {
    space_packet::write(out, off, 0, 0, 1, apid, 3, seq, dlen).ok()
}

fn stamp_spacepackets(out: &mut [u8], off: usize, apid: u16, seq: u16, dlen: u16) -> Option<()> {
    use spacepackets::{PacketType, SequenceFlags, SpHeader};
    let h = SpHeader::new_from_fields(PacketType::Tm, true, u11::new(apid), SequenceFlags::Unsegmented, u14::new(seq), dlen);
    h.write_to_slice(&mut out[off..]).ok()?;
    Some(())
}

fn stamp_ccsds_primary_header(out: &mut [u8], off: usize, apid: u16, seq: u16, dlen: u16) -> Option<()> {
    use ccsds_primary_header::primary_header::{PacketType, PrimaryHeader, SecondaryHeaderFlag, SeqFlag};
    let mut h = PrimaryHeader::default();
    h.control.set_version(0);
    h.control.set_packet_type(PacketType::Data);
    h.control.set_secondary_header_flag(SecondaryHeaderFlag::Present);
    h.control.set_apid(apid);
    h.sequence.set_sequence_type(SeqFlag::Unsegmented);
    h.sequence.set_sequence_count(seq);
    h.length.set_length_field(dlen);
    let b = out.get_mut(off..off + 6)?;
    b[0..2].copy_from_slice(&h.control.0);
    b[2..4].copy_from_slice(&h.sequence.0);
    b[4..6].copy_from_slice(&h.length.0);
    Some(())
}

fn stamp_gaia(out: &mut [u8], off: usize, apid: u16, seq: u16, dlen: u16) -> Option<()> {
    use gaia_ccsds_c2a::ccsds::space_packet::{PacketType, PrimaryHeader, SequenceFlag};
    let h = PrimaryHeader::new()
        .with_version_number(0)
        .with_packet_type(PacketType::Telemetry)
        .with_secondary_header_flag(true)
        .with_apid(apid)
        .with_sequence_flag(SequenceFlag::Unsegmented)
        .with_sequence_count(seq)
        .with_packet_data_length_raw(dlen);
    out.get_mut(off..off + 6)?.copy_from_slice(&h.into_bytes());
    Some(())
}

// {1 Routing}

// Reads: every row shares this loop.
fn routing(table: &[u8; 2048], buf: &[u8], n: usize, header: impl Fn(&[u8], usize) -> Option<(u16, u16)>) -> Counts {
    let hdr = space_packet::SIZE;
    let mut counts = [0u64; 4];
    let mut off = 0;
    for _ in 0..n {
        if off + hdr > buf.len() {
            off = 0;
        }
        let Some((apid, dlen)) = header(buf, off) else { break };
        counts[table[apid as usize] as usize] += 1;
        off += hdr + dlen as usize + 1;
    }
    Counts(counts)
}

// Writes: as `Routing_workload.restamp`, the header of each packet the
// routing loop reads, written at the same offset of `out` with the packet's
// APID and data length and the next sequence count of its route.
fn restamp(
    table: &[u8; 2048],
    buf: &[u8],
    out: &mut [u8],
    n: usize,
    header: impl Fn(&[u8], usize) -> Option<(u16, u16)>,
    stamp: impl Fn(&mut [u8], usize, u16, u16, u16) -> Option<()>,
) {
    let hdr = space_packet::SIZE;
    let mut seqs = [0u16; 4];
    let mut off = 0;
    for _ in 0..n {
        if off + hdr > buf.len() {
            off = 0;
        }
        let Some((apid, dlen)) = header(buf, off) else { break };
        let r = table[apid as usize] as usize;
        let seq = seqs[r];
        seqs[r] = (seq + 1) & 0x3FFF;
        if stamp(out, off, apid, seq, dlen).is_none() {
            break;
        }
        off += hdr + dlen as usize + 1;
    }
}

// {1 Gateway}

// TM frame reassembly: as `Gateway_workload.checksum`, each frame's virtual
// channel and first header pointer, then the APID and sequence count of each
// 70-byte packet starting in its data field, folded into an FNV-1a hash.
const CADU_SIZE: usize = 1115;
const PKT_SIZE: usize = space_packet::SIZE + 64;

// FNV-1a modulo 2^63, as `Gateway_workload.hash_int` computes it in OCaml's
// 63-bit `int`: the 64-bit basis and prime reduced modulo 2^63, each product
// masked to its low 63 bits.
const FNV_INIT: u64 = 0x4bf29ce484222325;
const FNV_PRIME: u64 = 0x100000001B3;
const MASK63: u64 = (1 << 63) - 1;

fn hash_int(state: u64, v: u64) -> u64 {
    (state ^ v).wrapping_mul(FNV_PRIME) & MASK63
}

fn gateway(buf: &[u8], frame: impl Fn(&[u8], usize) -> Option<(u8, u16)>, packet: impl Fn(&[u8], usize) -> Option<(u16, u16)>) -> u64 {
    let hdr = tm_frame::SIZE;
    let data_field_size = CADU_SIZE - hdr;
    let mut checksum = FNV_INIT;
    for f in 0..buf.len() / CADU_SIZE {
        let base = f * CADU_SIZE;
        let Some((vcid, fhp)) = frame(buf, base) else { break };
        checksum = hash_int(hash_int(checksum, vcid as u64), fhp as u64);
        let data_start = base + hdr;
        let mut off = data_start + fhp as usize;
        while off + PKT_SIZE <= data_start + data_field_size {
            let Some((apid, seq)) = packet(buf, off) else { break };
            checksum = hash_int(hash_int(checksum, apid as u64), seq as u64);
            off += PKT_SIZE;
        }
    }
    checksum
}

fn frame_wire_rust(buf: &[u8], base: usize) -> Option<(u8, u16)> {
    let f = tm_frame::Valid::at(buf, base).ok()?;
    Some((f.vcid(), f.firsthdrptr()))
}

fn frame_chronus(buf: &[u8], base: usize) -> Option<(u8, u16)> {
    let h = chronus_gateway::frames::TmFrameHeader::parse(&buf[base..])?;
    Some((h.vcid, h.first_header_pointer))
}

fn packet_wire_rust(buf: &[u8], off: usize) -> Option<(u16, u16)> {
    let p = space_packet::Valid::at(buf, off).ok()?;
    Some((p.apid(), p.seqcount()))
}

fn packet_spacepackets(buf: &[u8], off: usize) -> Option<(u16, u16)> {
    use spacepackets::{CcsdsPacket, SpHeader};
    let (h, _) = SpHeader::from_be_bytes(&buf[off..]).ok()?;
    Some((h.apid().value(), h.seq_count().value()))
}

// Writes: as `Gateway_workload.frame`, `frames` frames from a stream of
// 70-byte packets: each frame's primary header (spacecraft 0x1FF, virtual
// channel the frame number modulo 8, OCF flag set, master and virtual
// channel counts, first header pointer), then the header of each packet
// starting in its data field, packet `p` with APID `p mod 2048`, sequence
// count `p mod 16384` and 64 bytes of data. A packet that does not fit the
// frame carries over, as `Gateway_workload.generate_frames` lays them out.
fn frame(
    out: &mut [u8],
    frames: usize,
    frame_header: impl Fn(&mut [u8], usize, u8, u8, u8, u16) -> Option<()>,
    stamp: impl Fn(&mut [u8], usize, u16, u16, u16) -> Option<()>,
) {
    let hdr = tm_frame::SIZE;
    let data_field_size = CADU_SIZE - hdr;
    let mut carry = 0usize;
    let mut p = 0usize;
    for f in 0..frames {
        let base = f * CADU_SIZE;
        if frame_header(out, base, (f % 8) as u8, (f % 256) as u8, (f / 8 % 256) as u8, carry as u16).is_none() {
            break;
        }
        let mut off = base + hdr + carry;
        while off + PKT_SIZE <= base + CADU_SIZE {
            if stamp(out, off, (p % 2048) as u16, (p % 16384) as u16, (PKT_SIZE - space_packet::SIZE - 1) as u16).is_none() {
                return;
            }
            off += PKT_SIZE;
            p += 1;
        }
        let trailing = data_field_size - (off - (base + hdr));
        carry = if trailing == 0 { 0 } else { PKT_SIZE - trailing };
    }
}

fn frame_header_wire_rust(out: &mut [u8], base: usize, vcid: u8, mc: u8, vc: u8, fhp: u16) -> Option<()> {
    tm_frame::write(out, base, 0, 0x1FF, vcid, 1, mc, vc, 0, 0, 0, 3, fhp).ok()
}

// `TmFrameHeader` is `#[non_exhaustive]`: a header is parsed once from six
// zero bytes, then its fields set for each frame. `to_bytes` writes the
// packet order flag 0 and the segment length identifier 3, as this stream
// has them.
fn frame_header_chronus(template: chronus_gateway::frames::TmFrameHeader) -> impl Fn(&mut [u8], usize, u8, u8, u8, u16) -> Option<()> {
    move |out, base, vcid, mc, vc, fhp| {
        let mut h = template;
        h.vcid = vcid;
        h.mc_frame_count = mc;
        h.vc_frame_count = vc;
        h.first_header_pointer = fhp;
        out.get_mut(base..base + 6)?.copy_from_slice(&h.to_bytes());
        Some(())
    }
}

fn chronus_template() -> chronus_gateway::frames::TmFrameHeader {
    let mut h = chronus_gateway::frames::TmFrameHeader::parse(&[0; 6]).expect("six bytes");
    h.spacecraft_id = 0x1FF;
    h.ocf_present = true;
    h
}

// {1 CLCW}

// CLCW polling: as `Clcw_workload.run_all`, an anomaly for each word with
// lockout, wait or retransmit set or a report value other than one past the
// previous word's.
fn clcw(buf: &[u8]) -> u64 {
    let mut anomalies = 0u64;
    let mut expected = 0u32;
    for i in 0..buf.len() / clcw::SIZE {
        let Ok(w) = clcw::get_lockout_word(buf, i * clcw::SIZE) else { break };
        let report = clcw::reportvalue_of_word(w) as u32;
        if clcw::lockout_of_word(w) != 0 || clcw::wait_of_word(w) != 0 || clcw::retransmit_of_word(w) != 0 || report != expected & 0xFF {
            anomalies += 1;
        }
        expected = (report + 1) & 0xFF;
    }
    anomalies
}

// The same over `gaia-ccsds-c2a`'s `CLCW`, one word at a time.
fn clcw_gaia(buf: &[u8]) -> u64 {
    use gaia_ccsds_c2a::ccsds::tc::clcw::CLCW;
    let mut anomalies = 0u64;
    let mut expected = 0u32;
    for c in buf.chunks_exact(4) {
        let w = CLCW::from_bytes([c[0], c[1], c[2], c[3]]);
        let report = w.report_value() as u32;
        if w.lockout() != 0 || w.wait() != 0 || w.retransmit() != 0 || report != expected & 0xFF {
            anomalies += 1;
        }
        expected = (report + 1) & 0xFF;
    }
    anomalies
}

// Writes: as `Clcw_workload.report`, the CLCW a FARM reports with frame `i`:
// COP-1 in effect on virtual channel 7, report value `i mod 256`, lockout
// when the 12 low bits of `i` are all set, wait when its 6 low bits are,
// retransmit from bit 13 of `i`; every other field 0.
fn farm(i: usize) -> (u8, u8, u8, u8) {
    let lockout = (((i & 0xFFF) + 1) >> 12) as u8;
    let wait = (((i & 0x3F) + 1) >> 6) as u8;
    let retransmit = ((i >> 13) & 1) as u8;
    (lockout, wait, retransmit, (i & 0xFF) as u8)
}

fn report_wire_rust(out: &mut [u8]) {
    for i in 0..out.len() / clcw::SIZE {
        let (lockout, wait, retransmit, report) = farm(i);
        if clcw::write(out, i * clcw::SIZE, 0, 0, 0, 1, 7, 0, 0, 0, lockout, wait, retransmit, 0, 0, report).is_err() {
            break;
        }
    }
}

fn report_gaia(out: &mut [u8]) {
    use gaia_ccsds_c2a::ccsds::tc::clcw::CLCW;
    for (i, c) in out.chunks_exact_mut(4).enumerate() {
        let (lockout, wait, retransmit, report) = farm(i);
        let w = CLCW::new()
            .with_cop_in_effect(1)
            .with_virtual_channel_identification(7)
            .with_lockout(lockout)
            .with_wait(wait)
            .with_retransmit(retransmit)
            .with_report_value(report);
        c.copy_from_slice(&w.into_bytes());
    }
}

fn read(w: &str, buf: &[u8], items: usize, runs: usize, filter: &timing::Filter, file: &str) {
    match w {
        "routing" => {
            let table = routing_table();
            timing::measure(w, "wire.rust", filter, items, runs, || routing(&table, black_box(buf), items, sp_wire_rust));
            timing::measure(w, "spacepackets 0.19", filter, items, runs, || routing(&table, black_box(buf), items, sp_spacepackets));
            timing::measure(w, "ccsds_primary_header 0.15", filter, items, runs, || {
                routing(&table, black_box(buf), items, sp_ccsds_primary_header)
            });
            timing::measure(w, "gaia-ccsds-c2a 1.2", filter, items, runs, || routing(&table, black_box(buf), items, sp_gaia))
        }
        "gateway" => {
            assert_eq!(buf.len() / CADU_SIZE, items, "{file} does not hold {items} frames");
            timing::measure(w, "wire.rust", filter, items, runs, || gateway(black_box(buf), frame_wire_rust, packet_wire_rust));
            timing::measure(w, "chronus-gateway 1.0.0-rc.1 + spacepackets 0.19", filter, items, runs, || {
                gateway(black_box(buf), frame_chronus, packet_spacepackets)
            })
        }
        "clcw" => {
            assert_eq!(buf.len() / clcw::SIZE, items, "{file} does not hold {items} words");
            timing::measure(w, "wire.rust", filter, items, runs, || clcw(black_box(buf)));
            timing::measure(w, "gaia-ccsds-c2a 1.2", filter, items, runs, || clcw_gaia(black_box(buf)))
        }
        _ => panic!("no workload {w}"),
    }
}

fn write(w: &str, buf: &[u8], items: usize, runs: usize, filter: &timing::Filter, file: &str) {
    let corpus = format!("{w}_write");
    let c = corpus.as_str();
    match w {
        "routing" => {
            let table = routing_table();
            macro_rules! row {
                ($name:expr, $header:expr, $stamp:expr) => {{
                    let out = output(buf.len());
                    timing::measure(c, $name, filter, items, runs, || {
                        written(&out, |o| restamp(&table, black_box(buf), o, items, $header, $stamp))
                    })
                }};
            }
            row!("wire.rust", sp_wire_rust, stamp_wire_rust);
            row!("spacepackets 0.19", sp_spacepackets, stamp_spacepackets);
            row!("ccsds_primary_header 0.15", sp_ccsds_primary_header, stamp_ccsds_primary_header);
            row!("gaia-ccsds-c2a 1.2", sp_gaia, stamp_gaia)
        }
        "gateway" => {
            assert_eq!(buf.len() / CADU_SIZE, items, "{file} does not hold {items} frames");
            let out = output(items * CADU_SIZE);
            timing::measure(c, "wire.rust", filter, items, runs, || {
                written(&out, |o| frame(o, items, frame_header_wire_rust, stamp_wire_rust))
            });
            let out = output(items * CADU_SIZE);
            let template = chronus_template();
            timing::measure(c, "chronus-gateway 1.0.0-rc.1 + spacepackets 0.19", filter, items, runs, || {
                written(&out, |o| frame(o, items, frame_header_chronus(template), stamp_spacepackets))
            })
        }
        "clcw" => {
            assert_eq!(buf.len() / clcw::SIZE, items, "{file} does not hold {items} words");
            let out = output(items * clcw::SIZE);
            timing::measure(c, "wire.rust", filter, items, runs, || written(&out, report_wire_rust));
            let out = output(items * clcw::SIZE);
            timing::measure(c, "gaia-ccsds-c2a 1.2", filter, items, runs, || written(&out, report_gaia))
        }
        _ => panic!("no workload {w}"),
    }
}

fn main() {
    let mut args: Vec<String> = std::env::args().skip(1).collect();
    let mode = args.first().map(String::as_str);
    let writing = mode == Some("write");
    if writing || mode == Some("run") {
        args.remove(0);
    }
    let path = std::path::Path::new(&args[0]);
    let buf = std::fs::read(path).expect("input");
    let items: usize = args[1].parse().expect("items");
    let runs: usize = args[2].parse().expect("runs");
    let filter = timing::Filter::new(args.get(3).map(String::as_str));
    let w = path.file_stem().and_then(|s| s.to_str()).expect("workload");
    if writing {
        write(w, &buf, items, runs, &filter, &args[0])
    } else {
        read(w, &buf, items, runs, &filter, &args[0])
    }
    filter.finish();
}
