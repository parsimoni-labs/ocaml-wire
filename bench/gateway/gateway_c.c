/* TM frame reassembly — application logic with EverParse field extraction.

   Uses EverParse-generated TmframeValidateTmframe and
   SpacePacketValidateSpacePacket to extract fields via WireSet callbacks
   into C arrays. Application logic (checksum computation) uses the extracted
   values. No hand-written bitfield manipulation. */

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <stdint.h>
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
#include "TMFrame.h"
#include "SpacePacket.h"

static void gw_err(const char *t, const char *f, const char *r,
  uint64_t c, uint8_t *ctx, EVERPARSE_INPUT_BUFFER i, uint64_t p) {
  (void)t; (void)f; (void)r; (void)c; (void)ctx; (void)i; (void)p;
}

/* TMFrame field indices (declaration order) */
enum {
  TF_VERSION = 0, TF_SCID, TF_VCID, TF_OCFFLAG,
  TF_MCCOUNT, TF_VCCOUNT, TF_SECHDRFLAG, TF_SYNCFLAG,
  TF_PACKETORDER, TF_SEGLENID, TF_FIRSTHDRPTR,
  TF_N_FIELDS
};

/* SpacePacket field indices (declaration order) */
enum {
  SP_VERSION = 0, SP_TYPE, SP_SECHDRFLAG, SP_APID,
  SP_SEQFLAGS, SP_SEQCOUNT, SP_DATALENGTH,
  SP_N_FIELDS
};

static const uint64_t CHECKSUM_INIT = 0xCBF29CE484222325ULL;
static const uint64_t CHECKSUM_PRIME = 0x100000001B3ULL;

static inline int64_t now_ns(void) {
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return ts.tv_sec * 1000000000LL + ts.tv_nsec;
}

static inline uint64_t hash_int(uint64_t state, int value) {
  return (state ^ (uint64_t)value) * CHECKSUM_PRIME;
}

static void walk_frame(uint8_t *frame, int tm_hdr, int pkt_size,
                        int data_field_size, uint64_t *checksum) {
  int64_t tf[TF_N_FIELDS];
  WIRECTX tf_ctx = { tf };
  TmframeValidateTmframe(&tf_ctx, NULL, gw_err, frame, tm_hdr, 0);

  int vcid = (int)tf[TF_VCID];
  int fhp = (int)tf[TF_FIRSTHDRPTR];
  if (checksum != NULL) {
    *checksum = hash_int(hash_int(*checksum, vcid), fhp);
  } else {
    volatile int keep = vcid + fhp;
    (void)keep;
  }

  int64_t sp[SP_N_FIELDS];
  WIRECTX sp_ctx = { sp };
  int sp_hdr = 6;
  int off = tm_hdr + fhp;
  while (off + pkt_size <= tm_hdr + data_field_size) {
    SpacePacketValidateSpacePacket(&sp_ctx, NULL, gw_err,
        frame + off, sp_hdr, 0);
    int apid = (int)sp[SP_APID];
    int seq = (int)sp[SP_SEQCOUNT];
    if (checksum != NULL) {
      *checksum = hash_int(hash_int(*checksum, apid), seq);
    } else {
      volatile int keep = apid + seq;
      (void)keep;
    }
    off += pkt_size;
  }
}

CAMLprim value c_tm_reassemble(value v_buf, value v_off, value v_n) {
  uint8_t *buf = (uint8_t *)Bytes_val(v_buf) + Int_val(v_off);
  int buf_len = caml_string_length(v_buf) - Int_val(v_off);
  int n = Int_val(v_n);
  int cadu_size = 1115;
  int tm_hdr = 6;
  int pkt_size = 70;
  int data_field_size = cadu_size - tm_hdr;
  int n_frames = buf_len / cadu_size;
  int64_t t0 = now_ns();
  for (int i = 0; i < n; i++) {
    uint8_t *frame = buf + (i % n_frames) * cadu_size;
    walk_frame(frame, tm_hdr, pkt_size, data_field_size, NULL);
  }
  int64_t t1 = now_ns();
  return Val_int(t1 - t0);
}

CAMLprim value c_tm_reassemble_checksum(value v_buf, value v_off) {
  CAMLparam2(v_buf, v_off);
  uint8_t *buf = (uint8_t *)Bytes_val(v_buf) + Int_val(v_off);
  int buf_len = caml_string_length(v_buf) - Int_val(v_off);
  int cadu_size = 1115;
  int tm_hdr = 6;
  int pkt_size = 70;
  int data_field_size = cadu_size - tm_hdr;
  int n_frames = buf_len / cadu_size;
  uint64_t checksum = CHECKSUM_INIT;
  for (int i = 0; i < n_frames; i++) {
    uint8_t *frame = buf + i * cadu_size;
    walk_frame(frame, tm_hdr, pkt_size, data_field_size, &checksum);
  }
  CAMLreturn(caml_copy_int64((int64_t)checksum));
}
