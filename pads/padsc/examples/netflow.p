pstruct Hdr_v1
{
  buint16    version : version == 1; //- PRAGMA CHECK_ENDIAN
  buint16    count;
  buint32    uptime;
  buint32    sec;
  buint32    usec;
};

pstruct Hdr_v5
{
  buint16    version : version == 5; //- PRAGMA CHECK_ENDIAN
  buint16    count;
  buint32    uptime;
  buint32    sec;
  buint32    usec;
  buint32    flow_sequence;
  buint8     engine_type;
  buint8     engine_id;
  buint16    sampling_interval;
};

pstruct Hdr_v7
{
  buint16    version : version == 7; //- PRAGMA CHECK_ENDIAN
  buint16    count;
  buint32    uptime;
  buint32    sec;
  buint32    usec;
  buint32    flow_sequence;
};

pstruct Data_v1
{
  buint32    src_addr;
  buint32    dst_addr;
  buint32    hop;
  buint16    input;
  buint16    output;
  buint32    packets;
  buint32    bytes;
  buint32    first;
  buint32    last;
  buint16    src_port;
  buint16    dst_port;
  buint16    pad1;
  buint8     prot;
  buint8     tos;
  buint8     flags;
  buint8     tcp_retx_cnt;
  buint8     tcp_retx_secs;
  buint8     tcp_misseq_cnt;
};

pstruct Data_v5
{
  buint32    src_addr;
  buint32    dst_addr;
  buint32    hop;
  buint16    input;
  buint16    output;
  buint32    packets;
  buint32    bytes;
  buint32    first;
  buint32    last;
  buint16    src_port;
  buint16    dst_port;
  buint8     pad1;
  buint8     tcp_flags;
  buint8     prot;
  buint8     tos;
  buint16    src_as;
  buint16    dst_as;
  buint8     src_mask;
  buint8     dst_mask;
};

pstruct Data_v7
{
  buint32    src_addr;
  buint32    dst_addr;
  buint32    hop;
  buint16    input;
  buint16    output;
  buint32    packets;
  buint32    bytes;
  buint32    first;
  buint32    last;
  buint16    src_port;
  buint16    dst_port;
  buint8     flags;
  buint8     tcp_flags;
  buint8     prot;
  buint8     tos;
  buint16    src_as;
  buint16    dst_as;
  buint8     src_mask;
  buint8     dst_mask;
  buint16    pad2;
  buint32    router_sc;
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

