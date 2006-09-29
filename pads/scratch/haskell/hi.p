Ptypedef Psbh_uint32(:4:) Version_t;
Ptypedef Psbh_uint32(:4:) FastString_t;

Pstruct Pblob_t(:Puint32 length:){
  Pcompute Puint32 size = length;
  Pomit    Pb_uint8[size] bytes;
};

Pstruct RawString_t{
  Pb_uint32 len;
  Pstring_FW(:len:) name;
};

Pstruct Dictionary_t{
  Pb_uint32               numStrings;
  RawString_t[numStrings] strings;
};

Pstruct Dict_t(:Puint32 offset:){
  Pblob_t(:offset:) skip;
  Dictionary_t      dict;
};

Ptry ForwardDict_t(:Puint32 offset:) Dict_t(:offset);

void uint32_string(P_t *p, Dictionary_t dict, 
		   Puint32 *src, Puint32_pd *src_pd, RawString_t *dst, RawString_t_pd *dst_pd){
  if (*src < dict.numStrings){
    *dst = dict.strings[*src];
    RawString_t_genPD(p, dst, dst_pd);
  } else {  // Error: bad index
    dst_pd->compoundLevel.nerr = 1;
    dst_pd->compoundLevel.errCode = P_TRANSFORM_FAILED;
  };
};

void string_uint32(P_t *p, Dictionary_t dict, 
		   RawString_t *src, RawString_t_pd *src_pd,Puint32 *dst, Puint32_pd *dst_pd ){
  //just place holders.  should search in dict for string.
  *dst = 0;
  dst_pd = src_pd->compoundLevel;  
};

void cnvStringMask(Pbase_m *phy, RawString_t_m *log){
  *phy = log->compoundLevel;
};
  
Ptrans String_t(:Dictionary_t dict:){
  uint32_string(:dict:) : Psbh_uint32(:4:) <=> RawString_t : string_uint32;
  Pmaskmap cnvStringMask;
};

Pstruct Module_t(:Dictionary_t dict:){
  String_t(:dict:) packageID;
  String_t(:dict:) moduleName;
};

Penum Bool_t Pfrom (Pb_uint8) { False, True };

Pstruct lazyBlock_t{
  Pb_uint32 addressOfEnd;
  Pcompute size_t offset = position.offset;
  Pb_uint8[addressOfEnd - offset] body;
};

void Psbh32_char(Puint32 *src, Pbase_pd *src_pd, Pchar *dest, Pbase_pd *dest_pd){
  *dest_pd = *src_pd;
  *dest = *src;
};

void Pchar_sbh32(Pchar *src, Pbase_pd *src_pd, Puint32 *dest, Pbase_pd *dest_pd){
  *dest_pd = *src_pd;
  *dest = *src;
};

Ptrans Psbh_char{
  Psbh32_char : Psbh_uint32(:4:) <=> Pchar : Pchar_sbh32;
};

Pstruct GHCversion_t {
  Psbl_uint8(:1:) length;  
  Psbh_char[length] nums;
};

Punion LenRest_t(:Puint8 init:){
  Pswitch(init){
    Pcase 0xff : Psbl_uint32(:4:) longRep;
    Pdefault   : Pcompute Puint32 shortRep = init;
  }
};

Pstruct HiLenRaw_t{
  Pb_uint8 initial;
  LenRest_t(:initial:) rest;
};

void HiLenRaw_uint32(HiLenRaw_t *src, HiLenRaw_t_pd *src_pd, Puint32 *dest, Pbase_pd *dest_pd){
  *dest_pd = src_pd->initial; 
  *dest = src->rest.val.shortRep;  /* short and long are the same */
};

void uint32_HiLenRaw(P_t *pads, Puint32 *src, Pbase_pd *src_pd, HiLenRaw_t *dest, HiLenRaw_t_pd *dest_pd){
  if (*src < 0xff) {
    dest->initial = *src;
    dest->rest.tag = shortRep;
    dest->rest.val.shortRep = *src;
  } else {
    dest->initial = 0xff;
    dest->rest.tag = longRep;
    dest->rest.val.longRep = *src;
  };
  HiLenRaw_t_genPD(pads, dest, dest_pd);
};

void cnvHiLenMask(P_t *pads, HiLenRaw_t_m *phy, Pbase_m *log){
  HiLenRaw_t_m_init(pads, phy, *log);
};

Ptrans HiLen_t{
  HiLenRaw_uint32 : HiLenRaw_t <=> Puint32 : uint32_HiLenRaw(:pads:);
  Pmaskmap cnvHiLenMask(:pads:);
};



Penum NameSpace_t Pfrom(Pb_uint8){
  VarName, DataName, TvName, TcClasName
};


Pstruct OccName_t{
  NameSpace_t   namespace; 
  FastString_t  occNameFS;
};

Pstruct TypeName_t{
  OccName_t tyName;
  HiLen_t   numPieces;
  OccName_t[numPieces] pieces;
}

Punion AvailInfoBranches_t(:Puint8 tag:) {
  Pswitch (tag){ 
    Pcase 0 : OccName_t  avail;
    Pcase 1 : TypeName_t availTC;
  }
};

Pstruct GenAvailInfo_t{
  Pb_uint8                   tag;
  AvailInfoBranches_t(:tag:) branches;
};

Pstruct Export_t(:Dictionary_t dict:){
  Module_t(:dict:)  module;   
  HiLen_t           numExports;
  GenAvailInfo_t[numExports] genAvail;
}

Pstruct Exports_t(:Dictionary_t dict:) {
  HiLen_t length;
  Export_t(:dict:)[length] exps;
};

Pstruct Dep_mods_t{
  FastString_t moduleName;
  Bool_t       isBootInterface;
}

Pstruct DepBody_t(:Dictionary_t dict:) {
  HiLen_t                    dep_mods_len;
  Dep_mods_t[dep_mods_len]   dep_mods;
  HiLen_t                    dep_pkgs_len;
  FastString_t[dep_pkgs_len] dep_pkgs;
  HiLen_t                    dep_orphs_len;
  Module_t(:dict:)[dep_orphs_len]    dep_orphs;
};

Pstruct Dep_t(:Dictionary_t dict:){
  Pb_uint32           addressOfEnd;  /- Address of end of dependency block
  DepBody_t(:dict:)   body;
};

Pstruct Entity_t{
  OccName_t name;
  Version_t version;
};

Punion VerOptBranches_t(:Puint8 tag:){
  Pswitch (tag) {
    Pcase 0 : Pvoid     none;
    Pcase 1 : Version_t version;
  }
};

Pstruct VerOpt_t{
  Pb_uint8                tag;
  VerOptBranches_t(:tag:) branches;
};

Pstruct Usage_t{
  FastString_t   modName;
  Version_t      modVersion;
  VerOpt_t       exports;
  HiLen_t        numEntities;
  Entity_t[numEntities] entities;
  Version_t      ruleVersion;
}

Pstruct UsagesBody_t{
  HiLen_t         length;
  Usage_t[length] uses;
};

Pstruct Usages_t{
  Pb_uint32    addressOfEnd;
  UsagesBody_t body;
};

Penum FixityDirection_t Pfrom(Pb_uint8) {
  InfixL, InfixR, InfixN
};

Pstruct FixityInfo_t {
  Psbl_uint32(:4:)  precedence;
  FixityDirection_t direction; 
};

Pstruct Fixity_t{
  OccName_t    name;
  FixityInfo_t fixityInfo;
};

Pstruct Fixities_t{
  HiLen_t len;
  Fixity_t[len] fixities; 
};

Pstruct DeprecSomeEntry_t{
  OccName_t    name;
  FastString_t reason;
};

Pstruct DeprecSome_t{
  HiLen_t len;
  DeprecSomeEntry_t[len] deprec;
};

Punion DeprecsBranches_t(:Puint8 tag:){
  Pswitch(tag){
    Pcase 0x00: Pvoid            noDeprecs;
    Pcase 0x01: FastString_t     deprecAll;
    Pcase 0x02: DeprecSome_t     deprecSome;
  }
};

Pstruct DeprecsBody_t{
  Pb_uint8                 tag;
  DeprecsBranches_t(:tag:) branches;
}

Pstruct Deprecs_t{
  Pb_uint32     addressOfEnd;
  DeprecsBody_t body;
};


Penum KindEnum_t Pfrom(Pb_uint8){
  LiftedTypeKind, UnliftedTypeKind, UnboxedTypeKind, OpenTypeKind, ArgTypeKind, UbxTupleKind, FunKind, KindVar
};

Precur Kind_t;

Pstruct FunKindBody_t{
  Kind_t arg;
  Kind_t result;
};

Punion KindBranches_t(:KindEnum_t tag:){
  Pswitch (tag){
    Pcase FunKind: FunKindBody_t funKindBody;
    Pdefault :     Pvoid         other;
  }
};

Precur Pstruct Kind_t{
  KindEnum_t  tag;
  KindBranches_t(:tag:)  branches;
};

Pstruct IfaceTvBndr_t{
  FastString_t name;
  Kind_t  kind;
};

Precur IfaceType_t;

Pstruct ForAllTy_t{
  IfaceTvBndr_t iFaceTvBndr;
  IfaceType_t   iFaceType;
};

Punion IfaceTypeBranches_t(:Puint8 tag:){
  Pswitch (tag){
    Pcase 0x00: ForAllTy_t forAllTy;
       // TODO: more cases to be defined.
  }
};

Precur Pstruct IfaceType_t{
  Pb_uint8                   tag;
  IfaceTypeBranches_t(:tag:) branches;
};


Punion IfaceIdInfoBranches_t(:Puint8 tag:){
  Pswitch (tag){
    Pcase 0x00 : Pvoid       noInfo;
    Pcase 0x01 : lazyBlock_t hasInfo;
  }
};

Pstruct IfaceIdInfo_t{
  Pb_uint8                     tag;
  IfaceIdInfoBranches_t(:tag:) branches;
}

Pstruct IfaceId_t{
  OccName_t      name;
  IfaceType_t    ty;
  IfaceIdInfo_t  idInfo; 
};

Punion IFaceDeclBranches_t(:Puint8 tag:){
  Pswitch(tag){
    Pcase 0x00:  IfaceId_t iFaceId;
    Pcase 0x01:  Pvoid     iFaceForeign;
       // TODO: other cases to be defined
  }
} Pwhere {
  tag != iFaceForeign;
};

Pstruct IFaceDecl_t{
  Pb_uint8                   tag;
  IFaceDeclBranches_t(:tag:) branches; 
};

Pstruct Decl_t{
  Version_t   version;
  IFaceDecl_t iFaceDecl;
};

Pstruct Decls_t{
  HiLen_t        length;
  Decl_t[length] decls;
};

Pstruct Hi_t{
  Pendian Pb_uint32 id : id == 0x0001face || id == 0x01face64;
  Pb_uint32     dictAddress;
  ForwardDict_t(:dictAddress:) dict; 
  GHCversion_t  GHCversion; 
  Pb_uint8      way;  /* what is this? */
  Module_t (:dict:)  module;
  Bool_t             isBoot;
  Version_t          modVersion;
  Bool_t             hasOrphan;
  Dep_t(:dict:)      dependencies;
  Usages_t           usages;
  Exports_t(:dict:)  exports;
  Version_t     exportVersion;
  Fixities_t    fixityInfo; 
  Deprecs_t     deprecs;
  Decls_t       decls;  //need to fix mask
  Pcompute size_t numBytesRead = position.offset;
  Pb_uint8[dictAddress - numBytesRead] unknown;
  Dictionary_t  dictionary;
};

/*
void initHiMask(P_t *pads, Hi_t_m *m, Puint32 initialMask){
  IfaceType_t_m ifaceRoot = m->decls.decls.element.iFaceDecl.branches.iFaceId.ty;
  Kind_t_m      kindRoot  = m->decls.decls.element.iFaceDecl.branches.iFaceId.ty->branches.forAllTy.iFaceTvBndr.kind;

  Hi_t_m_init(pads, m, initialMask);
  P_DynamicMaskInit(ifaceRoot, IfaceType_t_m, _IfaceType_t_m, initialMask, ifaceRoot->branches.forAllTy.iFaceType);
  //  ifaceRoot->branches.forAllTy.iFaceType = ifaceRoot;

  P_DynamicMaskInit(kindRoot, Kind_t_m, _Kind_t_m, initialMask, kindRoot->branches.funKindBody.arg);
  //  kindRoot->branches.funKindBody.arg    = kindRoot;
  kindRoot->branches.funKindBody.result = kindRoot;
};

*/
