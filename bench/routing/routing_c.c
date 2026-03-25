/* Hand-written C APID routing — same logic as the OCaml benchmark.

   Reads APID (11-bit bitfield in big-endian uint16), looks up a routing table,
   dispatches to handler. This is the C baseline for comparing against Wire's
   staged Codec.get.

   SpacePacket primary header layout (48 bits):
     Version:3 Type:1 SecHdrFlag:1 APID:11
     SeqFlags:2 SeqCount:14
     DataLength:16 */

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <stdint.h>
#include <string.h>
#include <time.h>

static inline int64_t now_ns(void) {
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return ts.tv_sec * 1000000000LL + ts.tv_nsec;
}

static int routing_table[2048];
static int handler_counts[4];

static void init_routing_table(void) {
  for (int i = 0; i < 2048; i++) {
    if (i < 256) routing_table[i] = 0;
    else if (i < 1024) routing_table[i] = 1;
    else if (i < 1536) routing_table[i] = 2;
    else routing_table[i] = 3;
  }
}

static void route_counts(uint8_t *buf, int total_bytes, int n, int counts[4]) {
  int hdr = 6;
  int off = 0;
  for (int i = 0; i < n; i++) {
    if (off + hdr > total_bytes) off = 0;
    uint16_t w0 = ((uint16_t)buf[off] << 8) | buf[off + 1];
    int apid = w0 & 0x7FF;
    uint16_t w2 = ((uint16_t)buf[off + 4] << 8) | buf[off + 5];
    int dlen = w2;
    counts[routing_table[apid]]++;
    off += hdr + dlen + 1;
  }
}

CAMLprim value c_apid_route(value v_buf, value v_off, value v_n) {
  uint8_t *buf = (uint8_t *)Bytes_val(v_buf) + Int_val(v_off);
  int total_bytes = caml_string_length(v_buf) - Int_val(v_off);
  int n = Int_val(v_n);
  init_routing_table();
  memset(handler_counts, 0, sizeof(handler_counts));
  int64_t t0 = now_ns();
  route_counts(buf, total_bytes, n, handler_counts);
  int64_t t1 = now_ns();
  return Val_int(t1 - t0);
}

/* Result for verification: returns (hk, sci, diag, idle) */
CAMLprim value c_apid_route_counts(value v_buf, value v_off, value v_n) {
  CAMLparam3(v_buf, v_off, v_n);
  CAMLlocal1(v_counts);
  uint8_t *buf = (uint8_t *)Bytes_val(v_buf) + Int_val(v_off);
  int total_bytes = caml_string_length(v_buf) - Int_val(v_off);
  int n = Int_val(v_n);
  init_routing_table();
  int counts[4] = {0, 0, 0, 0};
  route_counts(buf, total_bytes, n, counts);
  v_counts = caml_alloc_tuple(4);
  Store_field(v_counts, 0, Val_int(counts[0]));
  Store_field(v_counts, 1, Val_int(counts[1]));
  Store_field(v_counts, 2, Val_int(counts[2]));
  Store_field(v_counts, 3, Val_int(counts[3]));
  CAMLreturn(v_counts);
}
