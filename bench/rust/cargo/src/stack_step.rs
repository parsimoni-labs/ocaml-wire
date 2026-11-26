// The checksum visitor of the fold `checksum` of `stack`, which hands it
// every output of `Packet_stack.codec`: it folds the fields of each accepted
// frame into the checksum, as the other rows of `main.rs` do.

use crate::{ip_fields, tcp_fields, udp_fields};

pub struct StackStep(pub u64);

impl crate::stack::checksum::Visitor for StackStep {
    #[inline(always)]
    #[allow(clippy::too_many_arguments)]
    fn valid(
        &mut self,
        ether_type: u16,
        ipv4_present: bool,
        _ipv4_version: u8,
        _ipv4_ihl: u8,
        _ipv4_dscp: u8,
        _ipv4_ecn: u8,
        ipv4_total_length: u16,
        _ipv4_identification: u16,
        _ipv4_flags: u8,
        _ipv4_fragment_offset: u16,
        ipv4_ttl: u8,
        ipv4_protocol: u8,
        _ipv4_checksum: u16,
        ipv4_src: u32,
        ipv4_dst: u32,
        ipv4_tcp_present: bool,
        ipv4_tcp_src_port: u16,
        ipv4_tcp_dst_port: u16,
        ipv4_tcp_seq: u32,
        ipv4_tcp_ack: u32,
        ipv4_tcp_data_offset: u8,
        _ipv4_tcp_reserved: u8,
        ipv4_tcp_flags: u16,
        ipv4_tcp_window: u16,
        _ipv4_tcp_checksum: u16,
        _ipv4_tcp_urgent: u16,
        ipv4_udp_present: bool,
        ipv4_udp_src_port: u16,
        ipv4_udp_dst_port: u16,
        ipv4_udp_length: u16,
        _ipv4_udp_checksum: u16,
        _ipv4_opaque_present: bool,
    ) {
        let acc = self.0.wrapping_add(ether_type as u64);
        if !ipv4_present {
            self.0 = acc;
            return;
        }
        let a = ip_fields(acc, ipv4_src, ipv4_dst, ipv4_ttl, ipv4_protocol, ipv4_total_length);
        self.0 = if ipv4_tcp_present {
            tcp_fields(a, ipv4_tcp_src_port, ipv4_tcp_dst_port, ipv4_tcp_seq, ipv4_tcp_ack, ipv4_tcp_window, ipv4_tcp_flags, ipv4_tcp_data_offset)
        } else if ipv4_udp_present {
            udp_fields(a, ipv4_udp_src_port, ipv4_udp_dst_port, ipv4_udp_length)
        } else {
            a
        };
    }

    #[inline(always)]
    fn invalid(&mut self) {}
}
