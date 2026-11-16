/* Packet-stack benchmark, EverParse C row.

   The validator EverParse generates and verifies for Packet_stack.codec,
   called once per corpus slot with the slot as its input, as the OCaml rows
   bound each frame by its slot. The fields come from the default
   EthernetStack_Fields plug, zeroed before each validation, and are folded
   into the checksum bench.ml and cargo/src/main.rs compute. No field is read
   from the bytes here: every value is a named plug member the validator set.

   Each pass is timed in process CPU time (getrusage, user and system) and
   wall time, after one warm-up pass that gives the checksum, and printed as
   the RESULT line [bench.exe report] reads, as the other harnesses do. */

#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>
#include <time.h>

#include "EverParse.h"
#include "EthernetStack.h"
#include "EthernetStack_Fields.h"

#include "bench_common.h"

#define STR_(x) #x
#define STR(x) STR_(x)
#if defined(__clang__) && defined(__apple_build_version__)
#define CC_NAME                                                               \
  "Apple clang " STR(__clang_major__) "." STR(__clang_minor__) "." STR(        \
      __clang_patchlevel__)
#elif defined(__clang__)
#define CC_NAME                                                               \
  "clang " STR(__clang_major__) "." STR(__clang_minor__) "." STR(              \
      __clang_patchlevel__)
#elif defined(__GNUC__)
#define CC_NAME "gcc " STR(__GNUC__) "." STR(__GNUC_MINOR__)
#else
#define CC_NAME "cc"
#endif
#define ROW_NAME "EverParse C, default plug (" CC_NAME ")"

static uint64_t stack_fold(uint8_t *buf, size_t len, uint32_t stride) {
  uint64_t acc = 0;
  size_t n = len / stride;
  for (size_t i = 0; i < n; i++) {
    EthernetStackFields f;
    memset(&f, 0, sizeof f);
    uint64_t r = EthernetStackValidateWireEthernetStack(
        (WIRECTX *)&f, NULL, bench_err, buf + i * stride, stride, 0);
    if (!EverParseIsSuccess(r))
      continue;
    acc += f.ether_type;
    if (!f.ipv4_present)
      continue;
    acc += (uint64_t)(f.ipv4_src ^ f.ipv4_dst) +
           ((uint64_t)f.ipv4_ttl + f.ipv4_protocol + f.ipv4_total_length);
    if (f.ipv4_tcp_present)
      acc += (((uint64_t)f.ipv4_tcp_src_port << 16) | f.ipv4_tcp_dst_port) +
             (uint64_t)(f.ipv4_tcp_seq ^ f.ipv4_tcp_ack) +
             ((uint64_t)f.ipv4_tcp_window + f.ipv4_tcp_flags +
              f.ipv4_tcp_data_offset);
    else if (f.ipv4_udp_present)
      acc += (((uint64_t)f.ipv4_udp_src_port << 16) | f.ipv4_udp_dst_port) +
             f.ipv4_udp_length;
  }
  return acc;
}

static double cpu_time(void) {
  struct rusage ru;
  getrusage(RUSAGE_SELF, &ru);
  return (double)ru.ru_utime.tv_sec + (double)ru.ru_utime.tv_usec / 1e6 +
         (double)ru.ru_stime.tv_sec + (double)ru.ru_stime.tv_usec / 1e6;
}

static double wall_time(void) {
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return (double)ts.tv_sec + (double)ts.tv_nsec / 1e9;
}

static int compare_double(const void *a, const void *b) {
  double x = *(const double *)a, y = *(const double *)b;
  return (x > y) - (x < y);
}

/* Best, 10th percentile, median and 90th percentile, sorted in place. */
static void quantiles(double *t, size_t n, double q[4]) {
  qsort(t, n, sizeof *t, compare_double);
  q[0] = t[0];
  q[1] = t[(n - 1) * 10 / 100];
  q[2] = t[(n - 1) * 50 / 100];
  q[3] = t[(n - 1) * 90 / 100];
}

CAMLprim value ep_stack_run(value v_corpus, value v_buf, value v_stride,
                            value v_runs, value v_filter) {
  CAMLparam5(v_corpus, v_buf, v_stride, v_runs, v_filter);
  if (strstr(ROW_NAME, String_val(v_filter)) == NULL)
    CAMLreturn(Val_unit);
  uint8_t *buf = (uint8_t *)Bytes_val(v_buf);
  size_t len = caml_string_length(v_buf);
  uint32_t stride = (uint32_t)Long_val(v_stride);
  size_t runs = (size_t)Long_val(v_runs);
  double packets = (double)(len / stride);
  double *cpu = malloc(runs * sizeof *cpu);
  double *wall = malloc(runs * sizeof *wall);
  if (cpu == NULL || wall == NULL) {
    fprintf(stderr, "everparse.exe: out of memory\n");
    exit(1);
  }
  volatile uint64_t sink;
  uint64_t sum = stack_fold(buf, len, stride);
  for (size_t r = 0; r < runs; r++) {
    double w0 = wall_time();
    double c0 = cpu_time();
    sink = stack_fold(buf, len, stride);
    double c1 = cpu_time();
    double w1 = wall_time();
    cpu[r] = (c1 - c0) * 1e9 / packets;
    wall[r] = (w1 - w0) * 1e9 / packets;
  }
  (void)sink;
  double c[4], w[4];
  quantiles(cpu, runs, c);
  quantiles(wall, runs, w);
  printf("RESULT\t%s\t%s\t%.2f\t%.2f\t%.2f\t%.2f\t%.2f\t%.2f\t%.2f\t%.2f\t%llu\n",
         String_val(v_corpus), ROW_NAME, c[0], c[1], c[2], c[3], w[0], w[1],
         w[2], w[3], (unsigned long long)(sum & ((UINT64_C(1) << 62) - 1)));
  fflush(stdout);
  free(cpu);
  free(wall);
  CAMLreturn(Val_unit);
}
