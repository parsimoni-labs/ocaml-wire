/* Shared utilities for C benchmark loops. */

#ifndef BENCH_COMMON_H
#define BENCH_COMMON_H

#include <stdatomic.h>
#include <stdint.h>
#include <time.h>

static inline int64_t now_ns(void) {
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return ts.tv_sec * 1000000000LL + ts.tv_nsec;
}

/* Compiler barrier: keeps [v] alive across the call and forbids the
   compiler from reordering memory ops past it. Single-threaded
   benches don't need a CPU fence -- this compiles to no instructions. */
static inline void wire_compiler_barrier(uint64_t v) {
  *(volatile uint64_t *)&v = v;
  atomic_signal_fence(memory_order_seq_cst);
}

static void bench_err(const char *t, const char *f, const char *r,
  uint64_t c, uint8_t *ctx, EVERPARSE_INPUT_BUFFER i, uint64_t p) {
  (void)t; (void)f; (void)r; (void)c; (void)ctx; (void)i; (void)p;
}

#endif
