/* The symbols of the SSE builtins [Vec] declares. OxCaml compiles each
   builtin to its instructions and never calls these; they exist because every
   module that names an external refers to its symbol. The OxCaml test suite
   links the same stubs (oxcaml/tests/simd/amd64/stub_builtins.c). */

#include <assert.h>

#define BUILTIN(name) \
  void name(void) { assert(0); }

BUILTIN(caml_sse2_int32x4_add)
BUILTIN(caml_sse2_int32x4_sub)
BUILTIN(caml_sse_vec128_and)
BUILTIN(caml_sse_vec128_or)
BUILTIN(caml_sse2_int32x4_cmpeq)
BUILTIN(caml_sse2_int32x4_srli)
BUILTIN(caml_sse2_int32x4_slli)
BUILTIN(caml_sse_vec128_shuffle_32)
BUILTIN(caml_sse41_int32x4_extract)
BUILTIN(caml_sse41_int32x4_insert)
BUILTIN(caml_ssse3_vec128_align_right_bytes)
BUILTIN(caml_sse41_vec128_testz)
BUILTIN(caml_int32x4_low_of_int32)
BUILTIN(caml_vec128_cast)
