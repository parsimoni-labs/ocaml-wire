/* Packet-stack benchmark, EverParse C row with a custom plug.

   The same verified validator as the default-plug row, compiled in this
   translation unit after a plug copied from the shipped EthernetStack_Fields.c
   and trimmed to the members the checksum reads. The setters are static
   inline and write into a local struct, so the compiler inlines them into the
   validator and folds each switch on its constant index: what is left is the
   validator's own work. The checksum is the one stack_c.c computes from the
   default plug. */

#include <stdint.h>
#include <string.h>

#include "EthernetStack_Fields.h"

/* The custom WIRECTX the ExternalTypedefs.h guard lets a plug supply. */
#define WIRECTX_DEFINED
typedef struct StackOut {
  uint16_t ether_type;
  uint16_t ipv4_total_length;
  uint8_t ipv4_present;
  uint8_t ipv4_ttl;
  uint8_t ipv4_protocol;
  uint8_t ipv4_tcp_present;
  uint8_t ipv4_udp_present;
  uint32_t ipv4_src;
  uint32_t ipv4_dst;
  uint16_t ipv4_tcp_src_port;
  uint16_t ipv4_tcp_dst_port;
  uint32_t ipv4_tcp_seq;
  uint32_t ipv4_tcp_ack;
  uint16_t ipv4_tcp_data_offset;
  uint16_t ipv4_tcp_flags;
  uint16_t ipv4_tcp_window;
  uint16_t ipv4_udp_src_port;
  uint16_t ipv4_udp_dst_port;
  uint16_t ipv4_udp_length;
} WIRECTX;

/* No member the checksum reads is a byte range. */
static inline void EthernetStackSetBytes(WIRECTX *ctx, uint32_t idx,
                                         uint32_t v) {
  (void)ctx;
  (void)idx;
  (void)v;
}

static inline void EthernetStackSetU16be(WIRECTX *ctx, uint32_t idx,
                                         uint16_t v) {
  switch (idx) {
  case ETHERNETSTACK_IDX_ETHER_TYPE: ctx->ether_type = v; break;
  case ETHERNETSTACK_IDX_IPV4_TOTAL_LENGTH: ctx->ipv4_total_length = v; break;
  case ETHERNETSTACK_IDX_IPV4_TCP_SRC_PORT: ctx->ipv4_tcp_src_port = v; break;
  case ETHERNETSTACK_IDX_IPV4_TCP_DST_PORT: ctx->ipv4_tcp_dst_port = v; break;
  case ETHERNETSTACK_IDX_IPV4_TCP_DATA_OFFSET:
    ctx->ipv4_tcp_data_offset = v;
    break;
  case ETHERNETSTACK_IDX_IPV4_TCP_FLAGS: ctx->ipv4_tcp_flags = v; break;
  case ETHERNETSTACK_IDX_IPV4_TCP_WINDOW: ctx->ipv4_tcp_window = v; break;
  case ETHERNETSTACK_IDX_IPV4_UDP_SRC_PORT: ctx->ipv4_udp_src_port = v; break;
  case ETHERNETSTACK_IDX_IPV4_UDP_DST_PORT: ctx->ipv4_udp_dst_port = v; break;
  case ETHERNETSTACK_IDX_IPV4_UDP_LENGTH: ctx->ipv4_udp_length = v; break;
  default: break;
  }
}

static inline void EthernetStackSetU8(WIRECTX *ctx, uint32_t idx, uint8_t v) {
  switch (idx) {
  case ETHERNETSTACK_IDX_IPV4_PRESENT: ctx->ipv4_present = v; break;
  case ETHERNETSTACK_IDX_IPV4_TTL: ctx->ipv4_ttl = v; break;
  case ETHERNETSTACK_IDX_IPV4_PROTOCOL: ctx->ipv4_protocol = v; break;
  case ETHERNETSTACK_IDX_IPV4_TCP_PRESENT: ctx->ipv4_tcp_present = v; break;
  case ETHERNETSTACK_IDX_IPV4_UDP_PRESENT: ctx->ipv4_udp_present = v; break;
  default: break;
  }
}

static inline void EthernetStackSetU32be(WIRECTX *ctx, uint32_t idx,
                                         uint32_t v) {
  switch (idx) {
  case ETHERNETSTACK_IDX_IPV4_SRC: ctx->ipv4_src = v; break;
  case ETHERNETSTACK_IDX_IPV4_DST: ctx->ipv4_dst = v; break;
  case ETHERNETSTACK_IDX_IPV4_TCP_SEQ: ctx->ipv4_tcp_seq = v; break;
  case ETHERNETSTACK_IDX_IPV4_TCP_ACK: ctx->ipv4_tcp_ack = v; break;
  default: break;
  }
}

/* The validator, under a name of its own: the default-plug row links the
   shipped one from libbenchcodecs.a into the same executable. Its
   ExternalAPI.h declarations of the setters above take their internal
   linkage. */
#define EthernetStackValidateWireEthernetStack ep_inline_validate
#include "EthernetStack.c"

#include "bench_common.h"
#include "stack_fold.h"

uint64_t stack_inline_fold(uint8_t *buf, size_t len, uint32_t stride) {
  uint64_t acc = 0;
  size_t n = len / stride;
  for (size_t i = 0; i < n; i++) {
    WIRECTX f;
    memset(&f, 0, sizeof f);
    uint64_t r = ep_inline_validate(&f, NULL, bench_err, buf + i * stride,
                                    stride, 0);
    if (!EverParseIsSuccess(r))
      continue;
    acc += f.ether_type;
    if (!f.ipv4_present)
      continue;
    acc += (uint64_t)(f.ipv4_src ^ f.ipv4_dst) +
           ((uint64_t)f.ipv4_ttl + f.ipv4_protocol + f.ipv4_total_length);
    if (f.ipv4_tcp_present)
      acc += (((uint64_t)f.ipv4_tcp_src_port << 16) | f.ipv4_tcp_dst_port) +
             (uint64_t)(f.ipv4_tcp_seq ^ f.ipv4_tcp_ack) +
             ((uint64_t)f.ipv4_tcp_window + f.ipv4_tcp_flags +
              f.ipv4_tcp_data_offset);
    else if (f.ipv4_udp_present)
      acc += (((uint64_t)f.ipv4_udp_src_port << 16) | f.ipv4_udp_dst_port) +
             f.ipv4_udp_length;
  }
  return acc;
}
