pstruct line {
  aint8      i8; '|';
  aint16    i16; '|';
  aint32    i32; '|';
  aint64    i64; '|';
  auint8    ui8; '|';
  auint16  ui16; '|';
  auint32  ui32; '|';
  auint64  ui64;
};

pstruct record {
  line l;
  '\n';
};


