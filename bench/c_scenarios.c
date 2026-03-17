/* Pure C implementations of scenario benchmarks.
   Same logic as the OCaml benchmarks but with direct pointer arithmetic
   and shift/mask — what you'd write in C for cFS-style packet processing. */

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <stdint.h>
#include <string.h>
#include <time.h>

static inline int64_t now_ns(void) {
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return ts.tv_sec * 1000000000LL + ts.tv_nsec;
}

/* ── APID demux: walk variable-size packets, extract APID, dispatch ── */

static int routing_table[2048];
static int handler_counts[4];

static void init_routing_table(void) {
  for (int i = 0; i < 2048; i++) {
    if (i < 256) routing_table[i] = 0;       /* HK */
    else if (i < 1024) routing_table[i] = 1;  /* science */
    else if (i < 1536) routing_table[i] = 2;  /* diagnostic */
    else routing_table[i] = 3;                 /* idle */
  }
}

/* ep_c_routing(buf, n_packets) -> ns */
CAMLprim value ep_c_routing(value v_buf, value v_n) {
  uint8_t *buf = (uint8_t *)Bytes_val(v_buf);
  int n = Int_val(v_n);
  int hdr = 6; /* Space Packet primary header */

  init_routing_table();
  memset(handler_counts, 0, sizeof(handler_counts));

  int64_t t0 = now_ns();
  int off = 0;
  for (int i = 0; i < n; i++) {
    uint16_t w0 = (buf[off] << 8) | buf[off + 1];
    int apid = w0 & 0x7FF;
    /* uint16_t w1 = (buf[off+2] << 8) | buf[off+3]; */
    /* int seq = w1 & 0x3FFF; */
    uint16_t dlen = (buf[off + 4] << 8) | buf[off + 5];
    handler_counts[routing_table[apid]]++;
    off += hdr + dlen + 1;
  }
  int64_t t1 = now_ns();
  return Val_int(t1 - t0);
}

/* ep_c_routing_counts(index) -> count */
CAMLprim value ep_c_routing_counts(value v_idx) {
  return Val_int(handler_counts[Int_val(v_idx)]);
}

/* ── TM frame reassembly: parse frame header, walk embedded packets ── */

/* ep_c_gateway(buf, n_frames, cadu_size) -> (ns, pkts) encoded as ns */
CAMLprim value ep_c_gateway(value v_buf, value v_n, value v_cadu) {
  uint8_t *buf = (uint8_t *)Bytes_val(v_buf);
  int n = Int_val(v_n);
  int cadu_size = Int_val(v_cadu);
  int tm_hdr = 6;
  int sp_hdr = 6;
  int pkt_payload = 64;
  int pkt_size = sp_hdr + pkt_payload;
  int data_field_size = cadu_size - tm_hdr;
  volatile int pkts = 0;

  int64_t t0 = now_ns();
  for (int frame = 0; frame < n; frame++) {
    int base = frame * cadu_size;
    /* Read VCID: bits [4:1] of first uint16be */
    uint16_t w0 = (buf[base] << 8) | buf[base + 1];
    int vcid = (w0 >> 1) & 0x7;
    (void)vcid;
    /* Read FirstHdrPtr: low 11 bits of third uint16be */
    uint16_t w2 = (buf[base + 4] << 8) | buf[base + 5];
    int fhp = w2 & 0x7FF;
    /* Walk packets */
    int data_start = base + tm_hdr;
    int off = data_start + fhp;
    while (off + pkt_size <= data_start + data_field_size) {
      uint16_t pw0 = (buf[off] << 8) | buf[off + 1];
      int apid = pw0 & 0x7FF;
      (void)apid;
      /* uint16_t pw1 = (buf[off+2] << 8) | buf[off+3]; */
      off += pkt_size;
      pkts++;
    }
  }
  int64_t t1 = now_ns();
  /* Encode both ns and pkts: return ns, pkts via separate call */
  return Val_int(t1 - t0);
}

static volatile int last_gateway_pkts = 0;

CAMLprim value ep_c_gateway_with_pkts(value v_buf, value v_n, value v_cadu) {
  uint8_t *buf = (uint8_t *)Bytes_val(v_buf);
  int n = Int_val(v_n);
  int cadu_size = Int_val(v_cadu);
  int tm_hdr = 6;
  int sp_hdr = 6;
  int pkt_payload = 64;
  int pkt_size = sp_hdr + pkt_payload;
  int data_field_size = cadu_size - tm_hdr;
  int pkts = 0;

  int64_t t0 = now_ns();
  for (int frame = 0; frame < n; frame++) {
    int base = frame * cadu_size;
    uint16_t w0 = (buf[base] << 8) | buf[base + 1];
    int vcid = (w0 >> 1) & 0x7;
    (void)vcid;
    uint16_t w2 = (buf[base + 4] << 8) | buf[base + 5];
    int fhp = w2 & 0x7FF;
    int data_start = base + tm_hdr;
    int off = data_start + fhp;
    while (off + pkt_size <= data_start + data_field_size) {
      off += pkt_size;
      pkts++;
    }
  }
  int64_t t1 = now_ns();
  last_gateway_pkts = pkts;
  return Val_int(t1 - t0);
}

CAMLprim value ep_c_gateway_pkts(value v_unit) {
  (void)v_unit;
  return Val_int(last_gateway_pkts);
}

/* ── CLCW polling: read 4 bitfields from 32-bit word, check anomalies ── */

CAMLprim value ep_c_clcw(value v_bufs, value v_n) {
  /* v_bufs is an array of 4-byte Bytes buffers */
  int n = Int_val(v_n);
  int anomalies = 0;
  int expected_seq = 0;

  int64_t t0 = now_ns();
  for (int i = 0; i < n; i++) {
    value v_buf = Field(v_bufs, i);
    uint8_t *buf = (uint8_t *)Bytes_val(v_buf);
    /* CLCW: 32-bit big-endian word.
       Lockout:    bit 8  (1 bit)
       Wait:       bit 7  (1 bit)
       Retransmit: bit 6  (1 bit)
       ReportValue: bits 0-7 (8 bits) */
    uint32_t w = ((uint32_t)buf[0] << 24) | ((uint32_t)buf[1] << 16) |
                 ((uint32_t)buf[2] << 8) | buf[3];
    int lockout = (w >> 8) & 1;
    int wait = (w >> 7) & 1;
    int retransmit = (w >> 6) & 1;
    int report = w & 0xFF;
    if (lockout || wait || retransmit || report != (expected_seq & 0xFF))
      anomalies++;
    expected_seq = report;
  }
  int64_t t1 = now_ns();
  /* Encode anomalies in high bits, ns in low bits — but ns won't fit.
     Return ns, get anomalies via separate call. */
  last_gateway_pkts = anomalies; /* reuse static */
  return Val_int(t1 - t0);
}

CAMLprim value ep_c_clcw_anomalies(value v_unit) {
  (void)v_unit;
  return Val_int(last_gateway_pkts);
}
