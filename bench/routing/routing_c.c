/* APID routing — application logic with EverParse field extraction.

   Uses EverParse-generated SpacePacketValidateSpacePacket to extract fields
   via WireSet callbacks into a C array. Application logic (routing dispatch)
   uses the extracted values. No hand-written bitfield manipulation. */

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <stdint.h>
#include <string.h>
#include <time.h>

/* WIRECTX definition — must match wire_stubs generated version */
#ifndef WIRECTX_DEFINED
#define WIRECTX_DEFINED
typedef struct { int64_t *fields; } WIRECTX;
#endif

/* WireSet functions provided by c_stubs_c at link time */
void WireSetU8(WIRECTX *ctx, uint32_t idx, uint8_t v);
void WireSetU16(WIRECTX *ctx, uint32_t idx, uint16_t v);
void WireSetU16be(WIRECTX *ctx, uint32_t idx, uint16_t v);
void WireSetU32(WIRECTX *ctx, uint32_t idx, uint32_t v);
void WireSetU32be(WIRECTX *ctx, uint32_t idx, uint32_t v);
void WireSetU64(WIRECTX *ctx, uint32_t idx, uint64_t v);
void WireSetU64be(WIRECTX *ctx, uint32_t idx, uint64_t v);

/* EverParse generated headers (implementation linked from c_stubs_c) */
#include "EverParse.h"
#include "SpacePacket.h"

static void sp_err(const char *t, const char *f, const char *r,
  uint64_t c, uint8_t *ctx, EVERPARSE_INPUT_BUFFER i, uint64_t p) {
  (void)t; (void)f; (void)r; (void)c; (void)ctx; (void)i; (void)p;
}

/* SpacePacket field indices (declaration order) */
enum {
  SP_VERSION = 0,
  SP_TYPE,
  SP_SECHDRFLAG,
  SP_APID,
  SP_SEQFLAGS,
  SP_SEQCOUNT,
  SP_DATALENGTH,
  SP_N_FIELDS
};

static inline int64_t now_ns(void) {
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return ts.tv_sec * 1000000000LL + ts.tv_nsec;
}

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
  int hdr = 6;  /* SpacePacket primary header size */
  int off = 0;
  int64_t fields[SP_N_FIELDS];
  WIRECTX ctx = { fields };

  for (int i = 0; i < n; i++) {
    if (off + hdr > total_bytes) off = 0;
    SpacePacketValidateSpacePacket(&ctx, NULL, sp_err,
        buf + off, hdr, 0);
    int apid = (int)fields[SP_APID];
    int dlen = (int)fields[SP_DATALENGTH];
    counts[routing_table[apid]]++;
    off += hdr + dlen + 1;
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
