pstruct Hdr_v1
{
  endian b_uint16    version : version == 1; 
  b_uint16    count;
  b_uint32    uptime;
  b_uint32    sec;
  b_uint32    usec;
};

pstruct Hdr_v5
{
  endian b_uint16    version : version == 5; 
  b_uint16    count;
  b_uint32    uptime;
  b_uint32    sec;
  b_uint32    usec;
  b_uint32    flow_sequence;
  b_uint8     engine_type;
  b_uint8     engine_id;
  b_uint16    sampling_interval;
};

pstruct Hdr_v7
{
  endian b_uint16    version : version == 7;
  b_uint16    count;
  b_uint32    uptime;
  b_uint32    sec;
  b_uint32    usec;
  b_uint32    flow_sequence;
};

pstruct Data_v1
{
  b_uint32    src_addr;
  b_uint32    dst_addr;
  b_uint32    hop;
  b_uint16    input;
  b_uint16    output;
  b_uint32    packets;
  b_uint32    bytes;
  b_uint32    first;
  b_uint32    last;
  b_uint16    src_port;
  b_uint16    dst_port;
  b_uint16    pad1;
  b_uint8     prot;
  b_uint8     tos;
  b_uint8     flags;
  b_uint8     tcp_retx_cnt;
  b_uint8     tcp_retx_secs;
  b_uint8     tcp_misseq_cnt;
};

pstruct Data_v5
{
  b_uint32    src_addr;
  b_uint32    dst_addr;
  b_uint32    hop;
  b_uint16    input;
  b_uint16    output;
  b_uint32    packets;
  b_uint32    bytes;
  b_uint32    first;
  b_uint32    last;
  b_uint16    src_port;
  b_uint16    dst_port;
  b_uint8     pad1;
  b_uint8     tcp_flags;
  b_uint8     prot;
  b_uint8     tos;
  b_uint16    src_as;
  b_uint16    dst_as;
  b_uint8     src_mask;
  b_uint8     dst_mask;
};

pstruct Data_v7
{
  b_uint32    src_addr;
  b_uint32    dst_addr;
  b_uint32    hop;
  b_uint16    input;
  b_uint16    output;
  b_uint32    packets;
  b_uint32    bytes;
  b_uint32    first;
  b_uint32    last;
  b_uint16    src_port;
  b_uint16    dst_port;
  b_uint8     flags;
  b_uint8     tcp_flags;
  b_uint8     prot;
  b_uint8     tos;
  b_uint16    src_as;
  b_uint16    dst_as;
  b_uint8     src_mask;
  b_uint8     dst_mask;
  b_uint16    pad2;
  b_uint32    router_sc;
};

pstruct Packet_v1 {
  Hdr_v1   h;
  Data_v1  d;
};

pstruct Packet_v5 {
  Hdr_v5   h;
  Data_v5  d;
};

pstruct Packet_v7 {
  Hdr_v7   h;
  Data_v7  d;
};

parray PacketArray_v1 { Packet_v1 []; };
parray PacketArray_v5 { Packet_v5 []; };
parray PacketArray_v7 { Packet_v7 []; };

punion Netflow {
  PacketArray_v1 v1;
  PacketArray_v5 v5;
  PacketArray_v7 v7;
};

