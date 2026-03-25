/* Hand-written C TM frame reassembly — same logic as the OCaml benchmark.

   Reads VCID and FirstHdrPtr from TM frame header, walks embedded
   SpacePackets extracting APID and SeqCount. This is the C baseline for
   comparing against Wire's staged Codec.get.

   TMFrame header (48 bits):
     Version:2 SCID:10 VCID:3 OCFFlag:1
     MCCount:8 VCCount:8
     SecHdrFlag:1 SyncFlag:1 PacketOrder:1 SegLenId:2 FirstHdrPtr:11 */

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <stdint.h>
#include <time.h>

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

static void walk_frame(uint8_t *frame, int tm_hdr, int pkt_size, int data_field_size,
                       uint64_t *checksum) {
  uint16_t w0 = ((uint16_t)frame[0] << 8) | frame[1];
  int vcid = (w0 >> 1) & 0x7;
  uint16_t w2 = ((uint16_t)frame[4] << 8) | frame[5];
  int fhp = w2 & 0x7FF;
  if (checksum != NULL) {
    *checksum = hash_int(hash_int(*checksum, vcid), fhp);
  } else {
    volatile int keep_vcid = vcid;
    volatile int keep_fhp = fhp;
    (void)keep_vcid;
    (void)keep_fhp;
  }

  int off = tm_hdr + fhp;
  while (off + pkt_size <= tm_hdr + data_field_size) {
    uint16_t pw = ((uint16_t)frame[off] << 8) | frame[off + 1];
    int apid = pw & 0x7FF;
    uint16_t sw = ((uint16_t)frame[off + 2] << 8) | frame[off + 3];
    int seq = sw & 0x3FFF;
    if (checksum != NULL) {
      *checksum = hash_int(hash_int(*checksum, apid), seq);
    } else {
      volatile int keep_apid = apid;
      volatile int keep_seq = seq;
      (void)keep_apid;
      (void)keep_seq;
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
  int pkt_size = 70; /* sp_hdr(6) + payload(64) */
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
  int pkt_size = 70; /* sp_hdr(6) + payload(64) */
  int data_field_size = cadu_size - tm_hdr;
  int n_frames = buf_len / cadu_size;
  uint64_t checksum = CHECKSUM_INIT;

  for (int i = 0; i < n_frames; i++) {
    uint8_t *frame = buf + i * cadu_size;
    walk_frame(frame, tm_hdr, pkt_size, data_field_size, &checksum);
  }

  CAMLreturn(caml_copy_int64((int64_t)checksum));
}
