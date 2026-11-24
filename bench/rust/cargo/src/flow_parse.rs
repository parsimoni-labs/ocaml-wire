//! `wire-bench-flow-parse FILE RUNS [ROW | --list]`: the parsers of the flow
//! classifier of `flow.rs` without its table, each handing the keys it reads
//! to a fold of a few instructions, so that a row of `wire-bench-rust flow`
//! splits into its parse and its table. A binary of its own, so that no row
//! of `wire-bench-rust` calls a crate function with more callers than it has
//! there.

// The flow rows use `timing::measure_state` only.
#[allow(dead_code)]
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
#[path = "gen/flow.rs"]
mod flow_scan;

// This binary runs `flow::parse` only.
#[allow(dead_code)]
mod flow;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    flow::parse(&args[1..]);
}
