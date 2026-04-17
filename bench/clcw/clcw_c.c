/* CLCW polling loop — application logic with EverParse field extraction.

   Uses EverParse-generated CLCWValidateCLCW to extract bitfields into a typed
   CLCWFields struct via the default plug. Application logic (anomaly
   detection) reads named members of the struct. No hand-written bitfield
   manipulation, no index arithmetic. */

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <stdint.h>

#include "EverParse.h"
#include "CLCW.h"
#include "CLCW_Fields.h"

#include "bench_common.h"

/* Must match clcw_bench.ml — derived from Space.clcw_codec */
static const int WORD_SIZE = 4;  /* Wire.Codec.wire_size Space.clcw_codec */

static int count_anomalies(uint8_t *buf, int n_words, int n_iters) {
  int anomalies = 0;
  int expected_seq = 0;
  CLCWFields fields = {0};

  for (int i = 0; i < n_iters; i++) {
    uint8_t *p = buf + (i % n_words) * WORD_SIZE;
    ClcwValidateClcw((WIRECTX *)&fields, NULL, bench_err, p, WORD_SIZE, 0);

    int expected_report = expected_seq & 0xFF;
    if (fields.Lockout || fields.Wait || fields.Retransmit
        || (int)fields.ReportValue != expected_report)
      anomalies++;
    expected_seq = ((int)fields.ReportValue + 1) & 0xFF;
  }
  return anomalies;
}

CAMLprim value c_clcw_poll(value v_buf, value v_off, value v_n) {
  uint8_t *buf = (uint8_t *)Bytes_val(v_buf) + Int_val(v_off);
  int buf_len = caml_string_length(v_buf) - Int_val(v_off);
  int n = Int_val(v_n);

  int n_words = buf_len / WORD_SIZE;
  int64_t t0 = now_ns();
  volatile int anomalies = count_anomalies(buf, n_words, n);
  (void)anomalies;
  int64_t t1 = now_ns();
  return Val_int(t1 - t0);
}

CAMLprim value c_clcw_poll_result(value v_buf, value v_off) {
  uint8_t *buf = (uint8_t *)Bytes_val(v_buf) + Int_val(v_off);
  int buf_len = caml_string_length(v_buf) - Int_val(v_off);

  int n_words = buf_len / WORD_SIZE;
  return Val_int(count_anomalies(buf, n_words, n_words));
}
