/* The OCaml side of the Rust backend's single entry point: hands the
   buffers' bytes and lengths to [wire_rs_call] in the Rust library. */

#include <stddef.h>
#include <stdint.h>
#include <caml/mlvalues.h>

extern int32_t wire_rs_call(int32_t, int32_t, int32_t, const uint8_t *, size_t,
                            size_t, uint8_t *, size_t, int64_t *);

static value call(value vcase, value vop, value vfield, value vbuf, value voff,
                  value vout, value vres) {
  return Val_int(wire_rs_call(
      Int_val(vcase), Int_val(vop), Int_val(vfield), Bytes_val(vbuf),
      caml_string_length(vbuf), (size_t)Long_val(voff), Bytes_val(vout),
      caml_string_length(vout), (int64_t *)Bytes_val(vres)));
}

CAMLprim value wire_rs_call_native(value vcase, value vop, value vfield,
                                   value vbuf, value voff, value vout,
                                   value vres) {
  return call(vcase, vop, vfield, vbuf, voff, vout, vres);
}

CAMLprim value wire_rs_call_bytecode(value *argv, int argn) {
  (void)argn;
  return call(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6]);
}

extern int32_t wire_rs_scan(int32_t, const uint8_t *, size_t, int64_t, int64_t,
                            int64_t *, size_t);

/* The frame's offset and length go through as signed, so a negative one
   reaches the Rust side, which refuses it as the OCaml scanner does. */
CAMLprim value wire_rs_scan_stub(value vcase, value vbuf, value voff,
                                 value vlen, value vres) {
  return Val_int(wire_rs_scan(
      Int_val(vcase), Bytes_val(vbuf), caml_string_length(vbuf),
      (int64_t)Long_val(voff), (int64_t)Long_val(vlen),
      (int64_t *)Bytes_val(vres), caml_string_length(vres) / 8));
}
