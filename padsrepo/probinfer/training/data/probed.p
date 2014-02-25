#include "basetokens.p"
Ptypedef Pstring_ME(:"/:/":) PPpunc_colon
Ptypedef Pstring_ME(:"/,/":) PPpunc_comma
Ptypedef Pstring_ME(:"/[#]/":) PPpunc_hash

Pstruct arrayBody {
  PPint int5;
  PPpunc_comma comma1;
  PPip ip3;
  PPpunc_comma comma2;
  PPint int6;
  PPpunc_colon colon3;
};

Parray eventSeq {
  arrayBody[];
};

Precord Pstruct entry_t {
  PPint int1;
  PPpunc_hash hash1;
  PPfloat float1;
  PPpunc_hash hash2;
  PPip ip1;
  PPpunc_colon colon1;
  PPint int2;
  PPpunc_hash hash3;
  PPip ip2;
  PPpunc_colon colon2;
  PPint int3;
  PPpunc_hash hash4;
  PPint int4;
  PPpunc_hash hash5;
  eventSeq event1;
};

Psource Parray clt_t {
  entry_t [];
}
