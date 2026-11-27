/* The OCaml side of the Rust backend's single entry point: hands the
   arguments, the buffers' bytes and their lengths to [wire_rs_call] in the
   Rust library. */

#include <stddef.h>
#include <stdint.h>
#include <caml/mlvalues.h>

extern int32_t wire_rs_call(const int64_t *, size_t, const uint8_t *, size_t,
                            uint8_t *, size_t, int64_t *, size_t, uint8_t *,
                            size_t);

CAMLprim value wire_rs_call_stub(value vargs, value vbuf, value vout,
                                 value vres, value vtxt) {
  return Val_int(wire_rs_call(
      (const int64_t *)Bytes_val(vargs), caml_string_length(vargs) / 8,
      Bytes_val(vbuf), caml_string_length(vbuf), Bytes_val(vout),
      caml_string_length(vout), (int64_t *)Bytes_val(vres),
      caml_string_length(vres) / 8, Bytes_val(vtxt),
      caml_string_length(vtxt)));
}
