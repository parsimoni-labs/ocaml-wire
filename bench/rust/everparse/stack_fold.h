/* The checksum fold of the inline-visitor row (stack_inline.c) over a corpus of
   [stride]-byte slots, which stack_c.c times beside its default-visitor row. */

#ifndef STACK_FOLD_H
#define STACK_FOLD_H

#include <stddef.h>
#include <stdint.h>

uint64_t stack_inline_fold(uint8_t *buf, size_t len, uint32_t stride);

#endif
