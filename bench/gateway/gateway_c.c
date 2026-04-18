/* TM frame reassembly -- application logic with EverParse field extraction.

   Uses EverParse-generated TmframeValidateTmframe and
   SpacePacketValidateSpacePacket to extract fields into typed struct plugs.
   Application logic (checksum computation) reads named struct members.
   No hand-written bitfield manipulation. */

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <stdint.h>

#include "EverParse.h"
#include "TMFrame.h"
#include "TMFrame_Fields.h"
#include "SpacePacket.h"
#include "SpacePacket_Fields.h"

#include "bench_common.h"

/* Must match gateway_bench.ml -- derived from Space.tm_frame_codec / Space.packet_codec */
static const int CADU_SIZE = 1115;
static const int TM_HDR = 6;       /* Wire.Codec.wire_size Space.tm_frame_codec */
static const int PKT_SIZE = 70;    /* sp_hdr (6) + pkt_payload (64) */
static const int DATA_FIELD_SIZE = 1115 - 6;  /* CADU_SIZE - TM_HDR */

static const uint64_t CHECKSUM_INIT = 0xCBF29CE484222325ULL;
static const uint64_t CHECKSUM_PRIME = 0x100000001B3ULL;

static inline uint64_t hash_int(uint64_t state, int value) {
  return (state ^ (uint64_t)value) * CHECKSUM_PRIME;
}

static void walk_frame(uint8_t *frame, int tm_hdr, int pkt_size,
                        int data_field_size, uint64_t *checksum) {
  TMFrameFields tf = {0};
  TmframeValidateTmframe((WIRECTX *)&tf, NULL, bench_err, frame, tm_hdr, 0);

  int vcid = (int)tf.VCID;
  int fhp = (int)tf.FirstHdrPtr;
  if (checksum != NULL) {
    *checksum = hash_int(hash_int(*checksum, vcid), fhp);
  } else {
    volatile int keep = vcid + fhp;
    (void)keep;
  }

  SpacePacketFields sp = {0};
  int sp_hdr = 6;
  int off = tm_hdr + fhp;
  while (off + pkt_size <= tm_hdr + data_field_size) {
    SpacePacketValidateSpacePacket((WIRECTX *)&sp, NULL, bench_err,
        frame + off, sp_hdr, 0);
    int apid = (int)sp.APID;
    int seq = (int)sp.SeqCount;
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
  int n_frames = buf_len / CADU_SIZE;
  int64_t t0 = now_ns();
  for (int i = 0; i < n; i++) {
    uint8_t *frame = buf + (i % n_frames) * CADU_SIZE;
    walk_frame(frame, TM_HDR, PKT_SIZE, DATA_FIELD_SIZE, NULL);
  }
  int64_t t1 = now_ns();
  return Val_int(t1 - t0);
}

CAMLprim value c_tm_reassemble_checksum(value v_buf, value v_off) {
  CAMLparam2(v_buf, v_off);
  uint8_t *buf = (uint8_t *)Bytes_val(v_buf) + Int_val(v_off);
  int buf_len = caml_string_length(v_buf) - Int_val(v_off);
  int n_frames = buf_len / CADU_SIZE;
  uint64_t checksum = CHECKSUM_INIT;
  for (int i = 0; i < n_frames; i++) {
    uint8_t *frame = buf + i * CADU_SIZE;
    walk_frame(frame, TM_HDR, PKT_SIZE, DATA_FIELD_SIZE, &checksum);
  }
  CAMLreturn(caml_copy_int64((int64_t)checksum));
}
