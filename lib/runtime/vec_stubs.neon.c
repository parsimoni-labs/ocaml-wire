/* The symbols of the NEON builtins [Vec] declares. OxCaml compiles each
   builtin to its instructions and never calls these; they exist because every
   module that names an external refers to its symbol. The OxCaml test suite
   links the same stubs (oxcaml/tests/simd/arm64/stub_builtins.c). */

#include <assert.h>

#define BUILTIN(name) \
  void name(void) { assert(0); }

BUILTIN(caml_neon_int32x4_add)
BUILTIN(caml_neon_int32x4_sub)
BUILTIN(caml_neon_int32x4_bitwise_and)
BUILTIN(caml_neon_int32x4_bitwise_or)
BUILTIN(caml_neon_int32x4_cmpeq)
BUILTIN(caml_neon_int32x4_cmpeqz)
BUILTIN(caml_neon_int32x4_srli)
BUILTIN(caml_neon_int32x4_slli)
BUILTIN(caml_neon_int32x4_dup)
BUILTIN(caml_neon_int32x4_extract)
BUILTIN(caml_neon_int32x4_insert)
BUILTIN(caml_neon_int8x16_ext)
BUILTIN(caml_neon_int64x2_extract)
BUILTIN(caml_int32x4_low_of_int32)
BUILTIN(caml_vec128_cast)
