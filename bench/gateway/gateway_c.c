/* Hand-written C TM frame reassembly — same logic as the OCaml benchmark.

   Reads VCID and FirstHdrPtr from TM frame header, walks embedded
   SpacePackets extracting APID and SeqCount. This is the C baseline for
   comparing against Wire's staged Codec.get.

   TMFrame header (48 bits):
     Version:2 SCID:10 VCID:3 OCFFlag:1
     MCCount:8 VCCount:8
     SecHdrFlag:1 SyncFlag:1 PacketOrder:1 SegLenId:2 FirstHdrPtr:11 */

#include <caml/mlvalues.h>
#include <stdint.h>
#include <time.h>

static inline int64_t now_ns(void) {
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return ts.tv_sec * 1000000000LL + ts.tv_nsec;
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
    uint16_t w0 = ((uint16_t)frame[0] << 8) | frame[1];
    volatile int vcid = (w0 >> 1) & 0x7;
    uint16_t w2 = ((uint16_t)frame[4] << 8) | frame[5];
    int fhp = w2 & 0x7FF;
    (void)vcid;
    int off = tm_hdr + fhp;
    while (off + pkt_size <= tm_hdr + data_field_size) {
      uint16_t pw = ((uint16_t)frame[off] << 8) | frame[off+1];
      volatile int apid = pw & 0x7FF;
      uint16_t sw = ((uint16_t)frame[off+2] << 8) | frame[off+3];
      volatile int seq = sw & 0x3FFF;
      (void)apid; (void)seq;
      off += pkt_size;
    }
  }
  int64_t t1 = now_ns();
  return Val_int(t1 - t0);
}
