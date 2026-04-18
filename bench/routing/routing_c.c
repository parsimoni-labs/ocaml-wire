/* APID routing — application logic with EverParse field extraction.

   Uses EverParse-generated SpacePacketValidateSpacePacket to extract fields
   into a typed SpacePacketFields struct via the default plug. Application
   logic (routing dispatch) reads named members. No hand-written bitfield
   manipulation. */

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <stdint.h>
#include <string.h>

#include "EverParse.h"
#include "SpacePacket.h"
#include "SpacePacket_Fields.h"

#include "bench_common.h"

static int routing_table[2048];

static void init_routing_table(void) {
  for (int i = 0; i < 2048; i++) {
    if (i < 256) routing_table[i] = 0;
    else if (i < 1024) routing_table[i] = 1;
    else if (i < 1536) routing_table[i] = 2;
    else routing_table[i] = 3;
  }
}

static void route_counts(uint8_t *buf, int total_bytes, int n, int counts[4]) {
  int hdr = 6;  /* Wire.Codec.wire_size Space.packet_codec */
  int off = 0;
  SpacePacketFields fields = {0};

  for (int i = 0; i < n; i++) {
    if (off + hdr > total_bytes) off = 0;
    SpacePacketValidateSpacePacket((WIRECTX *)&fields, NULL, bench_err,
        buf + off, hdr, 0);
    counts[routing_table[fields.APID]]++;
    off += hdr + (int)fields.DataLength + 1;
  }
}

CAMLprim value c_apid_route(value v_buf, value v_off, value v_n) {
  uint8_t *buf = (uint8_t *)Bytes_val(v_buf) + Int_val(v_off);
  int total_bytes = caml_string_length(v_buf) - Int_val(v_off);
  int n = Int_val(v_n);
  init_routing_table();
  int counts[4] = {0, 0, 0, 0};
  int64_t t0 = now_ns();
  route_counts(buf, total_bytes, n, counts);
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
