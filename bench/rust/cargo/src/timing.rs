//! The timing of the rows measured in their own process, shared by the Rust
//! harnesses (`bench/rust/cargo` and `bench/apps/cargo`). It does exactly
//! what `Bench_lib.measure` does on the OCaml side.

use std::cell::Cell;
use std::fmt::Display;
use std::hint::black_box;
use std::time::Instant;

/// The rows a harness runs, from its optional FILTER argument, as
/// `Bench_lib.filter` reads it: every row without one, only the row named
/// exactly FILTER with one, and no row for `--list`, which prints each row's
/// name instead, one per line.
pub enum Filter {
    All,
    List,
    Row { name: String, matched: Cell<bool> },
}

impl Filter {
    pub fn new(arg: Option<&str>) -> Filter {
        match arg {
            None => Filter::All,
            Some("--list") => Filter::List,
            Some(name) => Filter::Row { name: name.to_string(), matched: Cell::new(false) },
        }
    }

    // Whether the row `name` runs; under `List`, prints `name`.
    fn selects(&self, name: &str) -> bool {
        match self {
            Filter::All => true,
            Filter::List => {
                println!("{name}");
                false
            }
            Filter::Row { name: n, matched } => {
                let m = n == name;
                if m {
                    matched.set(true);
                }
                m
            }
        }
    }

    /// Exits with status 1, naming the row on stderr, when the filter names
    /// a row that no `measure` call had.
    pub fn finish(&self) {
        if let Filter::Row { name, matched } = self {
            if !matched.get() {
                eprintln!("no row is named {name:?}");
                std::process::exit(1);
            }
        }
    }
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

/// One warm-up pass, which also gives the row's result, then `runs` timed
/// passes of `items` items each. Each pass reads the wall clock and the
/// process CPU time (user and system) around the call; CPU time leaves out
/// the time the process spends descheduled. Prints one `RESULT <corpus>
/// <name> <cpu best> <cpu p10> <cpu median> <cpu p90> <wall best> <wall p10>
/// <wall median> <wall p90> <result>` line, in ns per item. Only the rows
/// `filter` selects run, so a process can be measured whole for one row.
pub fn measure<R: Display>(corpus: &str, name: &str, filter: &Filter, items: usize, runs: usize, mut f: impl FnMut() -> R) {
    if !filter.selects(name) {
        return;
    }
    let items = items as f64;
    let result = f();
    let mut cpu = Vec::with_capacity(runs);
    let mut wall = Vec::with_capacity(runs);
    for _ in 0..runs {
        let w0 = Instant::now();
        let c0 = cpu_time();
        black_box(f());
        let c1 = cpu_time();
        wall.push(w0.elapsed().as_nanos() as f64 / items);
        cpu.push((c1 - c0) * 1e9 / items);
    }
    let [c0, c1, c2, c3] = quantiles(cpu);
    let [w0, w1, w2, w3] = quantiles(wall);
    println!("RESULT\t{corpus}\t{name}\t{c0:.2}\t{c1:.2}\t{c2:.2}\t{c3:.2}\t{w0:.2}\t{w1:.2}\t{w2:.2}\t{w3:.2}\t{result}");
}
