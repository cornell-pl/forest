
// Assume argument pointers point to valid space
void Psbl32_char(Puint32 *src, Pbase_pd *src_pd, Pchar *dest, Pbase_pd *dest_pd){
  *dest_pd = *src_pd;
  *dest = *src;
};

void Pchar_sbl32(Pchar *src, Pbase_pd *src_pd, Puint32 *dest, Pbase_pd *dest_pd){
  *dest_pd = *src_pd;
  *dest = *src;
};

//TODO: for case where transform is identity, should be able to omit functions
Ptrans Psbl_char{
  Psbl32_char : Psbl_uint32(:4:) <=> Pchar : Pchar_sbl32;
};

Pstruct hString{
  Pb_uint32 len;
  Pstring_FW(:len:) name;
};

Pstruct lazyBlock_t{
  Pb_uint32 numBytes;
  Pcompute size_t offset = position.offset;
  Pb_uint8[numBytes - offset] payload;
};

Pstruct version_t {
  Psbl_uint32(:4:) length;  
   Psbl_char[length] nums;
};

Ptypedef Psbl_uint32(:4:) HFastString;

Pstruct module_t{
  HFastString packageID;
  HFastString moduleName;
};

Pstruct occName_t{
  HFastString occNameFS;
  Pb_uint8    namespace; /- (VarName, DataName, TvName, TcClsName)
};

Pstruct typeName_t{
  occName_t tyName;
  Pb_uint8 numPieces;
  occName_t[numPieces] pieces;
}

Punion availInfoPayload_t(:Puint8 tag:) {
  Pswitch (tag){ 
    Pcase 0 : occName_t  avail;
    Pcase 1 : typeName_t availTC;
  }
};

Pstruct genAvailInfo_t{
  Pb_uint8 tag;
  availInfoPayload_t(:tag:) payload;
};

Pstruct export_t{
  module_t module;   
  Pb_uint8 numExports;
  genAvailInfo_t[numExports] genAvail;

}

Pstruct exports_t {
  Pb_uint8 length;
  export_t[length] exports;
};

Pstruct hi{
  Pendian Pb_uint32 id : id == 0x0001face || id == 0x01face64;
  Pb_uint32 numBytes;
  version_t GHCversion; 
  Pb_uint8 way;
  module_t module;
  Pb_uint8 isBoot;
  Pb_uint8 iVersion;
  Pb_uint8 hasOrphan;
  lazyBlock_t dependencies;
  lazyBlock_t usages;
  exports_t   exports;
  Pcompute size_t offsetl = position.offset;
  Pb_uint8[numBytes - offsetl] unknown;
  Pb_uint32 numFunctions;
  hString[numFunctions] functions;
}
