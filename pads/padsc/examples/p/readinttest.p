pstruct line {
  a_int8      i8; '|';
  a_int16    i16; '|';
  a_int32    i32; '|';
  a_int64    i64; '|';
  a_uint8    ui8; '|';
  a_uint16  ui16; '|';
  a_uint32  ui32; '|';
  a_uint64  ui64;
};

pstruct rec {
  line l;
  EOR;
};
