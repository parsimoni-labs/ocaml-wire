/* Packet-stack benchmark, EverParse C rows.

   The validator EverParse generates and verifies for Packet_stack.codec,
   called once per corpus slot with the slot as its input, as the OCaml rows
   bound each frame by its slot. In the default-visitor row the fields come from
   the default EthernetStack_Fields visitor, zeroed before each validation, and
   are folded into the checksum bench.ml and cargo/src/main.rs compute; the
   inline-visitor row (stack_inline.c) folds the same members of a custom visitor.
   No field is read from the bytes here: every value is a named visitor member
   the validator set.

   Each pass is timed in process CPU time (getrusage, user and system) and
   wall time, after one warm-up pass that gives the checksum, and printed as
   the RESULT line [bench.exe report] reads, as the other harnesses do. */

#include <caml/alloc.h>
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
#include "CapturedStack.h"
#include "CapturedStack_Fields.h"

#include "bench_common.h"
#include "stack_fold.h"

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

/* Called with the stride the command line gives, as the OCaml and Rust rows
   take it. */
static uint64_t stack_default_fold(uint8_t *buf, size_t len, uint32_t stride) {
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

struct row {
  const char *name;
  uint64_t (*fold)(uint8_t *, size_t, uint32_t);
};

static const struct row rows[] = {
    {"EverParse C, default visitor", stack_default_fold},
    {"EverParse C, inline visitor", stack_inline_fold},
};

#define ROWS (sizeof rows / sizeof rows[0])

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

CAMLprim value ep_stack_rows(value v_unit) {
  CAMLparam1(v_unit);
  CAMLreturn(Val_long(ROWS));
}

static const struct row *row_of(value v_row) {
  long i = Long_val(v_row);
  if (i < 0 || (size_t)i >= ROWS) {
    fprintf(stderr, "everparse.exe: no row %ld\n", i);
    exit(1);
  }
  return &rows[i];
}

/* The name a filter selects row [v_row] by. */
CAMLprim value ep_stack_row(value v_row) {
  CAMLparam1(v_row);
  CAMLreturn(caml_copy_string(row_of(v_row)->name));
}

CAMLprim value ep_stack_run(value v_row, value v_corpus, value v_buf,
                            value v_stride, value v_runs) {
  CAMLparam5(v_row, v_corpus, v_buf, v_stride, v_runs);
  const struct row *row = row_of(v_row);
  uint8_t *buf = (uint8_t *)Bytes_val(v_buf);
  size_t len = caml_string_length(v_buf);
  uint32_t stride = (uint32_t)Long_val(v_stride);
  size_t runs = (size_t)Long_val(v_runs);
  if (stride == 0) {
    fprintf(stderr, "everparse.exe: a stride of 0 bytes\n");
    exit(1);
  }
  double packets = (double)(len / stride);
  double *cpu = malloc(runs * sizeof *cpu);
  double *wall = malloc(runs * sizeof *wall);
  if (cpu == NULL || wall == NULL) {
    fprintf(stderr, "everparse.exe: out of memory\n");
    exit(1);
  }
  volatile uint64_t sink;
  uint64_t sum = row->fold(buf, len, stride);
  for (size_t r = 0; r < runs; r++) {
    double w0 = wall_time();
    double c0 = cpu_time();
    sink = row->fold(buf, len, stride);
    double c1 = cpu_time();
    double w1 = wall_time();
    cpu[r] = (c1 - c0) * 1e9 / packets;
    wall[r] = (w1 - w0) * 1e9 / packets;
  }
  (void)sink;
  double c[4], w[4];
  quantiles(cpu, runs, c);
  quantiles(wall, runs, w);
  printf("RESULT\t%s\t%s (%s)\t%.2f\t%.2f\t%.2f\t%.2f\t%.2f\t%.2f\t%.2f\t%.2f\t%llu\n",
         String_val(v_corpus), row->name, CC_NAME, c[0], c[1], c[2], c[3],
         w[0], w[1], w[2], w[3], (unsigned long long)(sum & ((UINT64_C(1) << 62) - 1)));
  fflush(stdout);
  free(cpu);
  free(wall);
  CAMLreturn(Val_unit);
}

/* The flow classifier on a capture, as bench.exe flow and wire-bench-rust
   flow run it: the validator of Packet_stack.capture once per frame, bounded
   by its captured length, the 5-tuple and IP length read from the default
   CapturedStack_Fields visitor, zeroed per frame, and counted in a flow table of
   the layout and hash of bench/rust/flow_table.std.ml and cargo/src/flow.rs. The
   table's checksum is read after the timed passes, as theirs is. */

#define FLOW_ROW "EverParse C, default visitor"
#define FLOW_ROW_NAME FLOW_ROW " (" CC_NAME ")"

typedef struct {
  uint32_t epoch, meta, ports, src[4], dst[4], packets;
  uint64_t bytes, unused;
} flow_slot;

_Static_assert(sizeof(flow_slot) == 64, "a flow slot is 64 bytes");

typedef struct {
  flow_slot *slots;
  size_t mask;
  uint32_t epoch;
} flow_table;

#define FLOW_K UINT64_C(0x9E3779B97F4A7C15)

static inline uint64_t flow_mix(uint64_t h, uint32_t w) {
  return (h ^ w) * FLOW_K;
}

static inline void flow_add(flow_table *t, uint64_t h, uint32_t meta,
                            uint32_t ports, const uint32_t src[4],
                            const uint32_t dst[4], uint64_t bytes) {
  size_t i = (size_t)(h >> 32) & t->mask;
  for (;;) {
    flow_slot *s = &t->slots[i];
    if (s->epoch != t->epoch) {
      s->epoch = t->epoch;
      s->meta = meta;
      s->ports = ports;
      memcpy(s->src, src, sizeof s->src);
      memcpy(s->dst, dst, sizeof s->dst);
      s->packets = 1;
      s->bytes = bytes;
      return;
    }
    if (s->meta == meta && s->ports == ports &&
        memcmp(s->src, src, sizeof s->src) == 0 &&
        memcmp(s->dst, dst, sizeof s->dst) == 0) {
      s->packets++;
      s->bytes += bytes;
      return;
    }
    i = (i + 1) & t->mask;
  }
}

static inline uint32_t flow_ports(uint8_t tcp, uint32_t tsp, uint32_t tdp,
                                  uint8_t udp, uint32_t usp, uint32_t udp_) {
  return tcp ? (tsp << 16 | tdp) : udp ? (usp << 16 | udp_) : 0;
}

static void flow_pass(flow_table *t, uint8_t *buf, const uint32_t *offs,
                      const uint32_t *lens, size_t n) {
  t->epoch++;
  for (size_t i = 0; i < n; i++) {
    CapturedStackFields f;
    memset(&f, 0, sizeof f);
    uint64_t r = CapturedStackValidateWireCapturedStack(
        (WIRECTX *)&f, NULL, bench_err, buf + offs[i], lens[i], 0);
    if (!EverParseIsSuccess(r))
      continue;
    if (f.ipv4_present) {
      uint32_t meta = 0x400 | f.ipv4_protocol;
      uint32_t ports =
          flow_ports(f.ipv4_tcp_present, f.ipv4_tcp_src_port,
                     f.ipv4_tcp_dst_port, f.ipv4_udp_present,
                     f.ipv4_udp_src_port, f.ipv4_udp_dst_port);
      uint32_t src[4] = {f.ipv4_src, 0, 0, 0}, dst[4] = {f.ipv4_dst, 0, 0, 0};
      uint64_t h = flow_mix(
          flow_mix(flow_mix(flow_mix(0, f.ipv4_src), f.ipv4_dst), ports), meta);
      flow_add(t, h, meta, ports, src, dst, f.ipv4_total_length);
    } else if (f.ipv6_present) {
      uint32_t meta = 0x600 | f.ipv6_next_header;
      uint32_t ports =
          flow_ports(f.ipv6_tcp_present, f.ipv6_tcp_src_port,
                     f.ipv6_tcp_dst_port, f.ipv6_udp_present,
                     f.ipv6_udp_src_port, f.ipv6_udp_dst_port);
      uint32_t src[4] = {f.ipv6_src0, f.ipv6_src1, f.ipv6_src2, f.ipv6_src3};
      uint32_t dst[4] = {f.ipv6_dst0, f.ipv6_dst1, f.ipv6_dst2, f.ipv6_dst3};
      uint64_t h = 0;
      for (int w = 0; w < 4; w++)
        h = flow_mix(h, src[w]);
      for (int w = 0; w < 4; w++)
        h = flow_mix(h, dst[w]);
      h = flow_mix(flow_mix(h, ports), meta);
      flow_add(t, h, meta, ports, src, dst, 40 + (uint64_t)f.ipv6_payload_length);
    }
  }
}

static void flow_summary(const flow_table *t, size_t *flows, uint64_t *sum) {
  *flows = 0;
  *sum = 0;
  for (size_t i = 0; i <= t->mask; i++) {
    const flow_slot *s = &t->slots[i];
    if (s->epoch != t->epoch)
      continue;
    (*flows)++;
    uint64_t h = flow_mix(flow_mix(0, s->meta), s->ports);
    for (int w = 0; w < 4; w++)
      h = flow_mix(h, s->src[w]);
    for (int w = 0; w < 4; w++)
      h = flow_mix(h, s->dst[w]);
    *sum += (h + ((uint64_t)s->packets << 32) + s->bytes) * (i + 1);
  }
  *sum &= (UINT64_C(1) << 62) - 1;
}

static void die(const char *msg) {
  fprintf(stderr, "everparse.exe: %s\n", msg);
  exit(1);
}

static uint32_t le32(const uint8_t *p) {
  return (uint32_t)p[0] | (uint32_t)p[1] << 8 | (uint32_t)p[2] << 16 |
         (uint32_t)p[3] << 24;
}

CAMLprim value ep_flow_row(value v_unit) {
  CAMLparam1(v_unit);
  CAMLreturn(caml_copy_string(FLOW_ROW));
}

CAMLprim value ep_flow_run(value v_corpus, value v_buf, value v_runs) {
  CAMLparam3(v_corpus, v_buf, v_runs);
  uint8_t *buf = (uint8_t *)Bytes_val(v_buf);
  size_t len = caml_string_length(v_buf);
  size_t runs = (size_t)Long_val(v_runs);
  if (len < 24)
    die("truncated pcap header");
  uint32_t magic = le32(buf);
  if ((magic != 0xa1b2c3d4 && magic != 0xa1b23c4d) || le32(buf + 20) != 1)
    die("not a little-endian pcap file of Ethernet frames");
  size_t n = 0;
  for (size_t o = 24; o < len; n++) {
    if (o + 16 > len || o + 16 + le32(buf + o + 8) > len)
      die("truncated pcap record");
    o += 16 + le32(buf + o + 8);
  }
  uint32_t *offs = malloc(n * sizeof *offs), *lens = malloc(n * sizeof *lens);
  size_t cap = 1;
  while (cap < 2 * n)
    cap *= 2;
  flow_table t = {calloc(cap, sizeof(flow_slot)), cap - 1, 0};
  double *cpu = malloc(runs * sizeof *cpu), *wall = malloc(runs * sizeof *wall);
  if (!offs || !lens || !t.slots || !cpu || !wall)
    die("out of memory");
  size_t o = 24;
  for (size_t i = 0; i < n; i++) {
    offs[i] = (uint32_t)(o + 16);
    lens[i] = le32(buf + o + 8);
    o += 16 + lens[i];
  }
  flow_pass(&t, buf, offs, lens, n);
  for (size_t r = 0; r < runs; r++) {
    double w0 = wall_time();
    double c0 = cpu_time();
    flow_pass(&t, buf, offs, lens, n);
    double c1 = cpu_time();
    double w1 = wall_time();
    cpu[r] = (c1 - c0) * 1e9 / (double)n;
    wall[r] = (w1 - w0) * 1e9 / (double)n;
  }
  size_t flows;
  uint64_t sum;
  flow_summary(&t, &flows, &sum);
  double c[4], w[4];
  quantiles(cpu, runs, c);
  quantiles(wall, runs, w);
  printf("RESULT\t%s\t%s\t%.2f\t%.2f\t%.2f\t%.2f\t%.2f\t%.2f\t%.2f\t%.2f\t%zu "
         "flows, %llu\n",
         String_val(v_corpus), FLOW_ROW_NAME, c[0], c[1], c[2], c[3], w[0],
         w[1], w[2], w[3], flows, (unsigned long long)sum);
  fflush(stdout);
  free(offs);
  free(lens);
  free(t.slots);
  free(cpu);
  free(wall);
  CAMLreturn(Val_unit);
}
