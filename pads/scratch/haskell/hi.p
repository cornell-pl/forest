
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
  Pb_uint32 addressOfEnd;
  Pcompute size_t offset = position.offset;
  Pb_uint8[addressOfEnd - offset] payload;
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
  export_t[length] exps;
};

Pstruct dep_mods_t{
  HFastString moduleName;
  Pb_uint8    isBootInterface;
}

Punion lenRest_t(:Puint8 init:){
  Pswitch(init){
    Pcase 0xff : Psbl_uint32(:4:) longRep;
    Pdefault   : Pcompute Puint32 shortRep = init;
  }
};

Pstruct hiLenRaw_t{
  Pb_uint8 initial;
  lenRest_t(:initial:) rest;
}


void hiLenRaw_uint32(hiLenRaw_t *src, hiLenRaw_t_pd *src_pd, Puint32 *dest, Pbase_pd *dest_pd){
  *dest_pd = src_pd->initial; 
  *dest = src->rest.val.shortRep;  /* short and long are the same */
};

void uint32_hiLenRaw(P_t *pads, Puint32 *src, Pbase_pd *src_pd, hiLenRaw_t *dest, hiLenRaw_t_pd *dest_pd){
  if (*src < 0xff) {
    dest->initial = *src;
    dest->rest.tag = shortRep;
    dest->rest.val.shortRep = *src;
  } else {
    dest->initial = 0xff;
    dest->rest.tag = longRep;
    dest->rest.val.longRep = *src;
  };
  hiLenRaw_t_genPD(pads, dest, dest_pd);
};

void cnvMask(P_t *pads, hiLenRaw_t_m *phy, Pbase_m *log){
  hiLenRaw_t_m_init(pads, phy, *log);
};


Ptrans hiLen_t{
  hiLenRaw_uint32 : hiLenRaw_t <=> Puint32 : uint32_hiLenRaw(:pads:);
  Pmaskmap cnvMask(:pads:);
};

Pstruct depBody_t {
  hiLen_t                   dep_mods_len;
  dep_mods_t[dep_mods_len]  dep_mods;
  hiLen_t                   dep_pkgs_len;
  HFastString[dep_pkgs_len] dep_pkgs;
  hiLen_t                   dep_orphs_len;
  module_t[dep_orphs_len]   dep_orphs;
};

Pstruct dep_t{
  Pb_uint32   addressOfEnd;  /- Address of end
  depBody_t   body;
}

Pstruct hi{
  Pendian Pb_uint32 id : id == 0x0001face || id == 0x01face64;
  Pb_uint32 numBytes;
  version_t GHCversion; 
  Pb_uint8 way;
  module_t module;
  Pb_uint8 isBoot;
  Pb_uint8 iVersion;
  Pb_uint8 hasOrphan;
  dep_t    dependencies;
  lazyBlock_t usages;
  exports_t   exports;
  Pcompute size_t offsetl = position.offset;
  Pb_uint8[numBytes - offsetl] unknown;
  Pb_uint32 numFunctions;
  hString[numFunctions] functions;
}
