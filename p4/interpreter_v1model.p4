//
// Based on Walt O'Connor's telem_switch.p4
//

#include <core.p4>
#include <v1model.p4>

#define NUM_QUERIES 8
#define HASH_TABLE_SIZE 10
#define AGE_OUT_TIME_MS 20000

#define MAX_NUM_TRANSITIONS 100
#define MAX_NUM_REGISTERS 100
#define UPDATE_OP_WIDTH 1000

#define SCRATCH_SPACE_SIZE 1000
#define SCRATCH_SPACE_STRIDE 32

const bit<16> TYPE_IPV4 = 0x800;
typedef bit<8> ip_protocol_t;
const ip_protocol_t IP_PROTOCOLS_TCP = 6;
const ip_protocol_t IP_PROTOCOLS_UDP = 17;

typedef bit<9>  egressSpec_t;
typedef bit<48> macAddr_t;
typedef bit<32> ip4Addr_t;

typedef bit<104> fivetuple_bits_t;
typedef bit<NUM_QUERIES> query_flag_t;
typedef bit<32> reg_data_t;
typedef bit<48> timestamp_t;

typedef bit<8> field_id_t;
typedef bit<8> op_id_t;

typedef bit<8> state_t;
typedef bit<UPDATE_OP_WIDTH> load_cmd_t;
typedef bit<UPDATE_OP_WIDTH> update_cmd_t;
typedef bit<UPDATE_OP_WIDTH> store_cmd_t;

header ethernet_t {
    macAddr_t dstAddr;
    macAddr_t srcAddr;
    bit<16>   etherType;
}

header ipv4_t {
    bit<4>    version;
    bit<4>    ihl;
    bit<8>    diffserv;
    bit<16>   totalLen;
    bit<16>   identification;
    bit<3>    flags;
    bit<13>   fragOffset;
    bit<8>    ttl;
    bit<8>    protocol;
    bit<16>   hdrChecksum;
    ip4Addr_t srcAddr;
    ip4Addr_t dstAddr;
}

header tcp_t {
    bit<16> src_port;
    bit<16> dst_port;
    
    bit<32> seq_no;
    bit<32> ack_no;
    bit<4> data_offset;
    bit<4> res;
    bit<8> flags;
    bit<16> window;
    bit<16> checksum;
    bit<16> urgent_ptr;
}

header udp_t {
    bit<16> src_port;
    bit<16> dst_port;
    bit<16> hdr_lenght;
    bit<16> checksum;
}

struct fivetuple_t {
    bit<32> src_ip;
    bit<32> dst_ip;
    bit<16> src_port;
    bit<16> dst_port;
    bit<8> proto;
};

struct metadata {
    bit matched;
    query_flag_t queries;
    fivetuple_t ft;
    bit<16> hash;
    bit<104> hash_mask;

    state_t cur_state;
    bit<32> sym;
    bit<32> val;
}

header_union port_layer_t {
    tcp_t tcp;
    udp_t udp;
}

struct headers {
    ethernet_t ethernet;
    ipv4_t ipv4;
    port_layer_t port_layer;
}

fivetuple_t extract_ft_data(in fivetuple_bits_t ft) {
    fivetuple_t ft_res;
    ft_res.src_ip = ft[31:0];
    ft_res.dst_ip = ft[63:32];
    ft_res.src_port = ft[79:64];
    ft_res.dst_port = ft[95:80];
    ft_res.proto = ft[103:96];
    return ft_res;
}

fivetuple_bits_t pack_ft_data(in fivetuple_t ft) {
    return ft.proto ++ ft.dst_port ++ ft.src_port ++ ft.dst_ip ++ ft.src_ip;
}

reg_data_t get_field(in field_id_t fid, in headers hdr, in metadata meta, in standard_metadata_t std_meta) {
    reg_data_t ret = 0;
    if (fid == 0) {
        ret = 1;
    }
    else if (fid == 1) {
        ret = (reg_data_t)std_meta.ingress_global_timestamp;
    }
    else if (fid == 2) {
        ret = (reg_data_t)hdr.ipv4.totalLen;
    }
    else if (fid == 3) {
        ret = (reg_data_t)hdr.port_layer.tcp.seq_no;
    }
    else if (fid == 4) {
        ret = (reg_data_t)hdr.port_layer.tcp.ack_no;
    }
    else {
        ret = 0;
    }
    return ret;
}

reg_data_t apply_op(in op_id_t op, in reg_data_t left, in reg_data_t right) {
    reg_data_t res = 0;

    if (op == 0) { // const zero
        res = 0;
    }
    else if (op == 1) { // const one
        res = 1;
    }
    else if (op == 2) { // increment
        res = right + 1;
    }
    else if (op == 3) { // decrement
        res = right - 1;
    }
    else if (op == 4) { // add
        res = left + right;
    }
    else if (op == 5) { // subtract
        res = left - right;
    }
    else if (op == 6) { // minimum
        if (right < left) {
          res = right;
        } else {
          res = left;
        }
    }
    else if (op == 7) { // maximum
        if (right > left) {
          res = right;
        } else {
          res = left;
        }
    }
    else {
        res = 0;
    }
    return res;
}

parser TestParser(packet_in packet, 
                  out headers hdr, 
                  inout metadata meta, 
                  inout standard_metadata_t standard_metadata) {
    state start {
        transition parse_ethernet;
    }

    state parse_ethernet {
        packet.extract(hdr.ethernet);
        transition select(hdr.ethernet.etherType) {
            TYPE_IPV4: parse_ipv4;
            default: accept;
        }
    }

    state parse_ipv4 {
        packet.extract(hdr.ipv4);
        transition select(hdr.ipv4.protocol) {
            IP_PROTOCOLS_TCP: parse_tcp;
            IP_PROTOCOLS_UDP: parse_udp;
            default: accept;
        }
    }

    state parse_tcp {
        packet.extract(hdr.port_layer.tcp);
        transition accept;
    }

    state parse_udp {
        packet.extract(hdr.port_layer.udp);
        transition accept;
    }
}

control TestVerifyChecksum(inout headers hdr, inout metadata meta) {   
    apply {  }
}


control TestIngress(inout headers hdr,
                    inout metadata meta,
                    inout standard_metadata_t standard_metadata) {

    register<reg_data_t>(MAX_NUM_REGISTERS) registers;

    register<state_t>(1) cur_state;

    // ---------------------------------------------------------
    // Initial map and read cur_state
    // ---------------------------------------------------------

    action map_sym_val(reg_data_t sym, field_id_t val_fid) {
        cur_state.read(meta.cur_state, (bit<32>)0);
        meta.sym = sym;
        meta.val = get_field(val_fid, hdr, meta, standard_metadata);
        meta.matched = 1;
    }

    action no_op() {
        meta.matched = 0;
    }

    table filter_match {
        key = {
            hdr.ipv4.srcAddr: ternary;
            hdr.ipv4.dstAddr: ternary;
            hdr.ipv4.protocol: ternary;
            hdr.port_layer.tcp.src_port: ternary;
            hdr.port_layer.tcp.dst_port: ternary;
            hdr.port_layer.tcp.flags: ternary;
        }
        actions = {
            map_sym_val;
            no_op();
        }
        size = 1;
        default_action = no_op();
    }

    // ---------------------------------------------------------
    // Transition table
    // ---------------------------------------------------------

    //
    // Applies a transition including the register update operation
    //
    action do_transition(state_t next_state, load_cmd_t load, update_cmd_t update, store_cmd_t store) {

      bit<SCRATCH_SPACE_SIZE> scratch_space = 0;
      reg_data_t tmp;
      op_id_t op;
      reg_data_t tmp1;
      reg_data_t tmp2;


      // load is a list of (reg_id, addr)
      // we load each reg_id into the given addresses in scratch space
      // note that the first 32 bits of scratch_space are reserved for the data value.
      // also not that each address is a single byte and indexes into scratch space in 32-bit increments

      registers.read(tmp, (bit<32>)(load[7:0]));
      scratch_space = scratch_space | ((bit<SCRATCH_SPACE_SIZE>)tmp << (load[15:8] * SCRATCH_SPACE_STRIDE));

      registers.read(tmp, (bit<32>)(load[23:16]));
      scratch_space = scratch_space | ((bit<SCRATCH_SPACE_SIZE>)tmp << (load[31:24] * SCRATCH_SPACE_STRIDE));

      // Load data value into first 32 bits of scratch space.
      scratch_space[31:0] = meta.val;
      
      // update is a list of (op_code, left_addr, right_addr, out_addr)
      // op codes with arity less than two simply ignore one (left) or both arguments

      op = (op_id_t)update[7:0];
      tmp1 = (reg_data_t)(scratch_space >> (update[15:8] * SCRATCH_SPACE_STRIDE));
      tmp2 = (reg_data_t)(scratch_space >> (update[23:16] * SCRATCH_SPACE_STRIDE));
      tmp = apply_op(op, tmp1, tmp2);
      scratch_space = (scratch_space & ~((bit<SCRATCH_SPACE_SIZE>)0xFFFF << (update[31:24] * SCRATCH_SPACE_STRIDE)))
                    | ((bit<SCRATCH_SPACE_SIZE>)tmp << (update[31:24] * SCRATCH_SPACE_STRIDE));

      // store is a list of (addr, reg_id)
      // we write each addr in to the given reg_id

      tmp = (reg_data_t)(scratch_space >> (store[7:0] * SCRATCH_SPACE_STRIDE));
      registers.write((bit<32>)store[15:8], tmp);

      tmp = (reg_data_t)(scratch_space >> (store[23:16] * SCRATCH_SPACE_STRIDE));
      registers.write((bit<32>)store[31:24], tmp);

      cur_state.write(0, next_state);
    }

    action no_transition() {
    }

    table transitions {
      key = {
        meta.cur_state : exact;
        meta.sym : exact;
        meta.matched : exact;
      }
      actions = {
        do_transition;
        no_transition;
      }
      size = MAX_NUM_TRANSITIONS;
      default_action = no_transition();
    }


    // ---------------------------------------------------------
    // Simple ip forwarding table
    // ---------------------------------------------------------

    action drop() {
        mark_to_drop(standard_metadata);
    }

    action ipv4_forward_to_mac(macAddr_t dstAddr, egressSpec_t port) {
        standard_metadata.egress_spec = port;
        hdr.ethernet.srcAddr = hdr.ethernet.dstAddr; //REMITS FROM SAME PORT
        hdr.ethernet.dstAddr = dstAddr;
        hdr.ipv4.ttl = hdr.ipv4.ttl - 1;
    }

    table ipv4_lpm {
        key = {
            hdr.ipv4.dstAddr: lpm;
        }
        actions = {
            ipv4_forward_to_mac;
            drop;
            NoAction;
        }
        size = 1024;
        default_action = drop();
    }

    apply {
        filter_match.apply();
        transitions.apply();
        if (hdr.ipv4.isValid()){
            ipv4_lpm.apply();
        }
    }
}

control TestEgress(inout headers hdr, inout metadata meta, inout standard_metadata_t standard_metadata){
    apply { }
}

control TestDeparser(packet_out packet, in headers hdr) {
    apply {
        packet.emit(hdr.ethernet);
        packet.emit(hdr.ipv4);
        packet.emit(hdr.port_layer.tcp);
        packet.emit(hdr.port_layer.udp);
    }
}

control TestComputeChecksum(inout headers  hdr, inout metadata meta) {
    apply {
    update_checksum(
    hdr.ipv4.isValid(),
        {
            hdr.ipv4.version,
            hdr.ipv4.ihl,
            hdr.ipv4.diffserv,
            hdr.ipv4.totalLen,
            hdr.ipv4.identification,
            hdr.ipv4.flags,
            hdr.ipv4.fragOffset,
            hdr.ipv4.ttl,
            hdr.ipv4.protocol,
            hdr.ipv4.srcAddr,
            hdr.ipv4.dstAddr
        },
        hdr.ipv4.hdrChecksum,
        HashAlgorithm.csum16);
    }
}

V1Switch(
    TestParser(),
    TestVerifyChecksum(),
    TestIngress(),
    TestEgress(),
    TestComputeChecksum(),
    TestDeparser()
) main;
