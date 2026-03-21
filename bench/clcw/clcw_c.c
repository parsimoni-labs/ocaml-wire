/* Hand-written C CLCW polling — same logic as the OCaml benchmark.

   Reads 4 bitfields from a big-endian uint32 CLCW word, checks anomaly
   conditions, tracks expected sequence. This is the C baseline for comparing
   against Wire's staged Codec.get.

   CLCW layout (32 bits, MSB first):
     ControlWordType:1 CLCWVersion:2 StatusField:3 COPInEffect:2
     VCID:6 Spare:2 NoRF:1 NoBitlock:1
     Lockout:1 Wait:1 Retransmit:1 FARMBCounter:2 ReportValue:8 */

#include <caml/mlvalues.h>
#include <stdint.h>
#include <time.h>

static inline int64_t now_ns(void) {
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return ts.tv_sec * 1000000000LL + ts.tv_nsec;
}

CAMLprim value c_clcw_poll(value v_buf, value v_off, value v_n) {
  uint8_t *buf = (uint8_t *)Bytes_val(v_buf) + Int_val(v_off);
  int buf_len = caml_string_length(v_buf) - Int_val(v_off);
  int n = Int_val(v_n);
  int word_size = 4;
  int n_words = buf_len / word_size;
  volatile int anomalies = 0;
  volatile int expected_seq = 0;
  int64_t t0 = now_ns();
  for (int i = 0; i < n; i++) {
    uint8_t *p = buf + (i % n_words) * word_size;
    uint32_t w = ((uint32_t)p[0] << 24) | ((uint32_t)p[1] << 16)
              | ((uint32_t)p[2] << 8) | p[3];
    int lockout    = (w >> 12) & 1;
    int wait_      = (w >> 11) & 1;
    int retransmit = (w >> 10) & 1;
    int report     = w & 0xFF;
    if (lockout || wait_ || retransmit || report != (expected_seq & 0xFF))
      anomalies++;
    expected_seq = report;
  }
  int64_t t1 = now_ns();
  return Val_int(t1 - t0);
}

/* Single-pass result for verification */
CAMLprim value c_clcw_poll_result(value v_buf, value v_off) {
  uint8_t *buf = (uint8_t *)Bytes_val(v_buf) + Int_val(v_off);
  int buf_len = caml_string_length(v_buf) - Int_val(v_off);
  int word_size = 4;
  int n_words = buf_len / word_size;
  int anomalies = 0, expected_seq = 0;
  for (int i = 0; i < n_words; i++) {
    uint8_t *p = buf + i * word_size;
    uint32_t w = ((uint32_t)p[0] << 24) | ((uint32_t)p[1] << 16)
              | ((uint32_t)p[2] << 8) | p[3];
    int lockout    = (w >> 12) & 1;
    int wait_      = (w >> 11) & 1;
    int retransmit = (w >> 10) & 1;
    int report     = w & 0xFF;
    if (lockout || wait_ || retransmit || report != (expected_seq & 0xFF))
      anomalies++;
    expected_seq = report;
  }
  return Val_int(anomalies);
}
