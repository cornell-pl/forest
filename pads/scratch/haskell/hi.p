static char WORDSIZE = 4;
Ptypedef Psbh_uint64(:WORDSIZE:) NativeInt_t;

Ptypedef Psbh_uint32(:4:) Version_t;
Ptypedef Psbh_uint32(:4:) CompilerPhase_t;
Ptypedef Psbh_uint32(:4:) FastString_t;

Pstruct Pblob_t(:Puint32 length:){
  Pcompute Puint32 size = length;
  Pomit    Pb_uint8[size] bytes;
};

Pstruct LongInt_t{
  NativeInt_t             size;
  Psbh_uint32(:WORDSIZE:) byteArraySize;
  Pb_uint8[byteArraySize] bytes;
}

Punion IntegerBranches_t(:Puint8 tag:){
  Pswitch(tag){
    Pcase 0x00: NativeInt_t  nativeInt;
    Pcase 0x01: LongInt_t    longInt;
  }
};

Pstruct Integer_t{
  Pb_uint8                 tag;
  IntegerBranches_t(:tag:) branches;
};

/* I couldn't find where these types were printed in GHC */
Ptypedef Psbh_uint32(:4:) Float_t;   /* Prob not right encoding */
Ptypedef Psbh_uint64(:8:) Double_t;  /* Prob not right encoding */
Ptypedef NativeInt_t      Label_t;   /* Prob not right encoding */


Pstruct RawString_t{
  Pb_uint32 len;
  Pstring_FW(:len:) name;
};

/* I couldn't find where these types were printed in GHC */
Ptypedef RawString_t HString_t;

Pstruct Dictionary_t{
  Pb_uint32               numStrings;
  RawString_t[numStrings] strings;
};

Pstruct SDict_t(:Puint32 offset:){
  Pblob_t(:offset:) skip;
  Dictionary_t      dict;
};

Ptry ForwardDict_t(:Puint32 offset:) SDict_t(:offset:);

Pstruct IRawString_t{
  Pb_uint32   index;
  RawString_t rs;
};

void uint32_IRstring(P_t *p, Dictionary_t *dict, 
		     Puint32 *src, Pbase_pd *src_pd, IRawString_t *dst, IRawString_t_pd *dst_pd){
  dst->index = *src;
  if (*src < dict->numStrings){
    //    dst->rs = dict->strings.elts[*src];
    Pstring_share(p, &dst->rs.name, &(dict->strings.elts[*src].name));
    dst->rs.len = dict->strings.elts[*src].len;
    IRawString_t_genPD(p, dst, dst_pd);
  } else {  // Error: bad index
    dst->rs.len = 0;
    dst->rs.name.len = 0;
    IRawString_t_genPD(p, dst, dst_pd);
    dst_pd->nerr++;
    dst_pd->errCode = P_TRANSFORM_FAILED;
  };
};

void IRstring_uint32(IRawString_t *src, IRawString_t_pd *src_pd,Puint32 *dst, Pbase_pd *dst_pd ){
  *dst = src->index;
  *dst_pd = src_pd->index;  
};

void cnvIRStringMask(Pbase_m *phy, IRawString_t_m *log){
  *phy = log->compoundLevel;
};
  
Ptrans String_t(:Dictionary_t *dict:){
  uint32_IRstring(:pads,dict:) : Psbh_uint32(:4:) <=> IRawString_t : IRstring_uint32;
  Pmaskmap cnvIRStringMask;
};

Pstruct Module_t(:Dictionary_t *dict:){
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

Ptypedef Psbh_char Char_t;

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

Pstruct OccName_t(:Dictionary_t *dict:){
  NameSpace_t      namespace; 
  String_t(:dict:) occNameFS;
};

Punion OccNameBranches_t(:Puint8 tag, Dictionary_t *dict:){
  Pswitch(tag){
    Pcase 0x00: Pvoid             noOccName;
    Pcase 0x01: OccName_t(:dict:) someOccName;
  }
};

Pstruct OccNameOpt_t(:Dictionary_t *dict:){
  Pb_uint8                       tag;
  OccNameBranches_t(:tag, dict:) branches;
};



Pstruct TypeName_t(:Dictionary_t *dict:){
  OccName_t(:dict:) tyName;
  HiLen_t           numPieces;
  OccName_t(:dict:)[numPieces] pieces;
}

Punion AvailInfoBranches_t(:Puint8 tag, Dictionary_t *dict:) {
  Pswitch (tag){ 
    Pcase 0 : OccName_t (:dict:) avail;
    Pcase 1 : TypeName_t(:dict:) availTC;
  }
};

Pstruct GenAvailInfo_t(:Dictionary_t *dict:){
  Pb_uint8                        tag;
  AvailInfoBranches_t(:tag,dict:) branches;
};

Pstruct Export_t(:Dictionary_t *dict:){
  Module_t(:dict:)      module;   
  HiLen_t               numExports;
  GenAvailInfo_t(:dict:)[numExports] genAvail;
}

Pstruct Exports_t(:Dictionary_t *dict:) {
  HiLen_t length;
  Export_t(:dict:)[length] exps;
};

Pstruct Dep_mods_t(:Dictionary_t *dict:){
  String_t(:dict:) moduleName;
  Bool_t           isBootInterface;
}

Pstruct DepBody_t(:Dictionary_t *dict:) {
  HiLen_t                           dep_mods_len;
  Dep_mods_t(:dict:)[dep_mods_len]  dep_mods;
  HiLen_t                           dep_pkgs_len;
  String_t(:dict:)  [dep_pkgs_len]  dep_pkgs;
  HiLen_t                           dep_orphs_len;
  Module_t(:dict:)  [dep_orphs_len] dep_orphs;
};

Pstruct Dep_t(:Dictionary_t *dict:){
  Pb_uint32         addressOfEnd;  /- Address of end of dependency block
  DepBody_t(:dict:) body;
};

Pstruct Entity_t(:Dictionary_t *dict:){
  OccName_t(:dict:) name;
  Version_t         version;
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

Pstruct Usage_t(:Dictionary_t *dict:){
  String_t(:dict:) modName;
  Version_t        modVersion;
  VerOpt_t         exports;
  HiLen_t          numEntities;
  Entity_t(:dict:)[numEntities] entities;
  Version_t        ruleVersion;
}

Pstruct UsagesBody_t(:Dictionary_t *dict:){
  HiLen_t         length;
  Usage_t(:dict:)[length] uses;
};

Pstruct Usages_t(:Dictionary_t *dict:){
  Pb_uint32            addressOfEnd;
  UsagesBody_t(:dict:) body;
};

Penum FixityDirection_t Pfrom(Pb_uint8) {
  InfixL, InfixR, InfixN
};

Pstruct FixityInfo_t {
  Psbl_uint32(:4:)  precedence;
  FixityDirection_t direction; 
};

Pstruct Fixity_t(:Dictionary_t *dict:){
  OccName_t(:dict:) name;
  FixityInfo_t      fixityInfo;
};

Pstruct Fixities_t(:Dictionary_t *dict:){
  HiLen_t               len;
  Fixity_t(:dict:)[len] fixities; 
};

Pstruct DeprecSomeEntry_t(:Dictionary_t *dict:){
  OccName_t(:dict:) name;
  String_t(:dict:)  reason;
};

Pstruct DeprecSome_t(:Dictionary_t *dict:){
  HiLen_t                        len;
  DeprecSomeEntry_t(:dict:)[len] deprec;
};

Punion DeprecsBranches_t(:Puint8 tag, Dictionary_t *dict:){
  Pswitch(tag){
    Pcase 0x00: Pvoid                noDeprecs;
    Pcase 0x01: String_t(:dict:)     deprecAll;
    Pcase 0x02: DeprecSome_t(:dict:) deprecSome;
  }
};

Pstruct DeprecsBody_t(:Dictionary_t *dict:){
  Pb_uint8                       tag;
  DeprecsBranches_t(:tag, dict:) branches;
}

Pstruct Deprecs_t(:Dictionary_t *dict:){
  Pb_uint32             addressOfEnd;
  DeprecsBody_t(:dict:) body;
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

Precur IfaceType_t(:Dictionary_t *dict:);

Pstruct IfaceIdBndr_t(:Dictionary_t *dict:){
  String_t   (:dict:) name;
  IfaceType_t(:dict:) type;
};

Pstruct IfaceTvBndr_t(:Dictionary_t *dict:){
  String_t(:dict:) name;
  Kind_t           kind;
};

Pstruct ForAllTy_t(:Dictionary_t *dict:){
  IfaceTvBndr_t(:dict:) iFaceTvBndr;
  IfaceType_t(:dict:)   iFaceType;
};

Pstruct AppTy_t(:Dictionary_t *dict:){
  IfaceType_t(:dict:)  fn;
  IfaceType_t(:dict:)  arg;
};

Pstruct IfaceExtPkg_t(:Dictionary_t *dict:){
  Module_t(:dict:)  mod;
  OccName_t(:dict:) occ;
}

/* Error in reading this type */
Pstruct IfaceHomePkg_t(:Dictionary_t *dict:){
  Module_t(:dict:)  mod;
  OccName_t(:dict:) occ;
  Version_t         version;
};

Punion IfaceExtNameBranches_t(:Puint8 tag, Dictionary_t *dict:){
  Pswitch(tag){
    Pcase 0x00: IfaceExtPkg_t (:dict:)  extPkg;
    Pcase 0x01: IfaceHomePkg_t(:dict:)  homePkg;
    Pcase 0x02: OccName_t     (:dict:)  localTop;
  }
};

Pstruct IfaceExtName_t(:Dictionary_t *dict:){
  Pb_uint8                            tag : sfprintf(sfstdout, "the IfaceExtName_t tag is %d\n", tag) + 1;
  IfaceExtNameBranches_t(:tag, dict:) branches;
};

Pstruct ClassP_t(:Dictionary_t *dict:){
  IfaceExtName_t(:dict:)      name;
  HiLen_t                     length;
  IfaceType_t(:dict:)[length] tys;
};

Punion IPNameBranches_t(:Puint8 tag, Dictionary_t *dict:){
  Pswitch(tag){
    Pcase 0x00: OccName_t(:dict:) dupable;
    Pcase 0x01: OccName_t(:dict:) linear;
  }
};

Pstruct IPName_t(:Dictionary_t *dict:){
  Pb_uint8                      tag;
  IPNameBranches_t(:tag, dict:) branches;
};

Pstruct IParam_t(:Dictionary_t *dict:){
  IPName_t   (:dict:) name;
  IfaceType_t(:dict:) tys;
};

Punion PredTyBranches_t(:Puint8 tag, Dictionary_t *dict:){
  Pswitch (tag){
    Pcase 0x00: ClassP_t(:dict:) classp;
    Pcase 0x01: IParam_t(:dict:) iparam;
  }
};

Pstruct PredTy_t(:Dictionary_t *dict:){
  Pb_uint8                      tag : sfprintf(sfstdout, "The predty_t tag is %d\n", tag) + 1;
  PredTyBranches_t(:tag, dict:) branches;
}

Pstruct PairTy_t(:Dictionary_t *dict:){
  IfaceType_t(:dict:) first;
  IfaceType_t(:dict:) second;
};

Pstruct KnownTcTy_t(:Dictionary_t *dict:){
  IfaceExtName_t(:dict:)       tyCon;
  HiLen_t                      numArgs;
  IfaceType_t(:dict:)[numArgs] args;
};

Penum Boxity_t Pfrom(Pb_uint8){ Boxed, Unboxed };
Ptypedef Psbh_uint32(:4:) Arity_t;

Pstruct IfaceTupTc_t{
  Boxity_t  boxity;
  Arity_t   arity;
}

Punion IfaceTyConBranches_t(:Puint8 tag, Dictionary_t *dict:){
  Pswitch(tag){
    Pcase 0x01: Pvoid                  intTc;
    Pcase 0x02: Pvoid                  boolTc;
    Pcase 0x03: Pvoid                  charTc;
    Pcase 0x04: Pvoid                  listTc;
    Pcase 0x05: Pvoid                  arrayTc;
    Pcase 0x06: IfaceTupTc_t           tupleTc;
    Pcase 0x07: IfaceExtName_t(:dict:) ifaceTc;
  }
};

Pstruct IfaceTyCon_t(:Dictionary_t *dict:){
  Pb_uint8                          tag;
  IfaceTyConBranches_t(:tag, dict:) branches;
};

Punion IfaceTyConOptBranches_t(:Puint8 tag, Dictionary_t *dict:){
  Pswitch(tag){
    Pcase 0x00: Pvoid                noTyCon;
    Pcase 0x01: IfaceTyCon_t(:dict:) someTyCon;
  }
};

Pstruct IfaceTyConOpt_t(:Dictionary_t *dict:){
  Pb_uint8 tag;
  IfaceTyConOptBranches_t(:tag, dict:) branches;
};

Pstruct TyConApp_t(:Dictionary_t *dict:){
  IfaceTyCon_t(:dict:)         tyCon;
  HiLen_t                      numArgs;
  IfaceType_t(:dict:)[numArgs] args;
};

Punion IfaceTypeBranches_t(:Puint8 tag, Dictionary_t *dict:){
  Pswitch (tag){
    Pcase 0x00: ForAllTy_t(:dict:)  forAllTy;
    Pcase 0x01: String_t(:dict:)    tyVar;
    Pcase 0x02: AppTy_t(:dict:)     appTy;
    Pcase 0x03: AppTy_t(:dict:)     funTy;
    Pcase 0x05: PredTy_t(:dict:)    predTy;
    Pcase 0x06: Pvoid               intTy;
    Pcase 0x07: Pvoid               charTy;
    Pcase 0x08: Pvoid               boolTy;
    Pcase 0x09: IfaceType_t(:dict:) listTy;
    Pcase 0x0a: Pvoid               unitTy;
    Pcase 0x0b: PairTy_t(:dict:)    pairTy;
    Pcase 0x0c: KnownTcTy_t(:dict:) knownTcTy;
    Pcase 0x0d: TyConApp_t(:dict:)  tyConTy;
  }
};

Precur Pstruct IfaceType_t(:Dictionary_t *dict:){
  Pb_uint8                         tag : sfprintf(sfstdout, "The tag in IfaceType_t is %d\n", tag) + 1;
  IfaceTypeBranches_t(:tag, dict:) branches;
};

Precur Demand_t;

Pstruct DemandList_t{
  HiLen_t              numDemands;
  Demand_t[numDemands] demands;      /* XXX: recursive loop back */
};

Punion DemandsBranches_t(:Puint8 tag:){
  Pswitch(tag){
    Pcase 0x00: Demand_t     poly;   /* XXX: recursive loop back */
    Pcase 0x01: DemandList_t prod;
  }
};

Pstruct Demands_t{
  Pb_uint8                 tag;
  DemandsBranches_t(:tag:) branches;
};

Punion DemandBranches_t(:Puint8 tag:){
  Pswitch(tag){
    Pcase 0x00: Pvoid     top;
    Pcase 0x01: Pvoid     abstraction;
    Pcase 0x02: Demand_t  call;      /* XXX: recursive loop back */
    Pcase 0x03: Demands_t eval;
    Pcase 0x04: Demands_t defer;
    Pcase 0x05: Demand_t  box;       /* XXX: recursive loop back */
    Pcase 0x06: Pvoid     bot;
  }
};

Precur Pstruct Demand_t{
  Pb_uint8                tag;
  DemandBranches_t(:tag:) branches;
};

Penum DmdResult_t Pfrom(Pb_uint8){ TopRes, RetCPR, BotRes };

Pstruct DmdType_t{
  HiLen_t              numDemands;
  Demand_t[numDemands] demands;
  DmdResult_t          demandResult;
};

Precur IfaceExpr_t(:Dictionary_t *dict:);

Punion IfaceBndrBody_t(:Puint8 tag, Dictionary_t *dict:){
  Pswitch(tag){
    Pcase 0x00: IfaceIdBndr_t(:dict:) idBndr;
    Pcase 0x01: IfaceTvBndr_t(:dict:) tvBndr;
  }
};

Pstruct IfaceBndr_t(:Dictionary_t *dict:){
  Pb_uint8  tag;
  IfaceBndrBody_t(:tag, dict:) branches;
};

Pstruct IfaceTuple_t(:Dictionary_t *dict:){
  Boxity_t            boxity;
  HiLen_t             length;
  IfaceExpr_t(:dict:)[length] tuple;   /* XXX recursive loop back */
};

Pstruct IfaceLam_t(:Dictionary_t *dict:){
  IfaceBndr_t(:dict:) var;
  IfaceExpr_t(:dict:) body;  /* XXX recursive loop back */
};

Pstruct IfaceApp_t(:Dictionary_t *dict:){
  IfaceExpr_t(:dict:)  fun;
  IfaceExpr_t(:dict:) arg;  /* XXX recursive loop back */
};

Punion LiteralBranches_t(:Puint8 tag, Dictionary_t *dict:){
  Pswitch(tag) {
    Pcase 0x00: Char_t           machChar;
    Pcase 0x01: String_t(:dict:) machStr;
    Pcase 0x02: Pvoid            machNullAddr;
    Pcase 0x03: Integer_t        machInt;
    Pcase 0x04: Integer_t        machInt64;
    Pcase 0x05: Integer_t        machWord;
    Pcase 0x06: Integer_t        machWord64;
    Pcase 0x07: Float_t          machFloat;
    Pcase 0x08: Double_t         machDouble;
    Pcase 0x09: Label_t          machLabel;
  }
};

Pstruct Literal_t(:Dictionary_t *dict:){
  Pb_uint8                      tag;
  LiteralBranches_t(:tag,dict:) branches;
};

Punion IfaceConAltBranches_t(:Puint8 tag, Dictionary_t *dict:){
  Pswitch(tag){
    Pcase 0x00: Pvoid             defaultAlt;
    Pcase 0x01: OccName_t(:dict:) dataAlt;
    Pcase 0x02: Boxity_t          tupleAlt;
    Pcase 0x03: Literal_t(:dict:) litAlt;
  }
};

Pstruct IfaceConAlt_t(:Dictionary_t *dict:){
  Pb_uint8                           tag;
  IfaceConAltBranches_t(:tag, dict:) branches;
};

Pstruct IfaceAlt_t(:Dictionary_t *dict:){
  IfaceConAlt_t(:dict:)     info;
  HiLen_t                   numVars;
  String_t(:dict:)[numVars] vars;
  IfaceExpr_t(:dict:)       body;  /* XXX recursive loop back */
};

Pstruct IfaceCase_t(:Dictionary_t *dict:){
  IfaceExpr_t(:dict:) scrut;
  String_t   (:dict:) bndr;
  IfaceType_t(:dict:) ty;
  HiLen_t             numCases;
  IfaceAlt_t (:dict:)[numCases] cases;
};

Pstruct IfaceNonRec_t(:Dictionary_t *dict:){
  IfaceIdBndr_t(:dict:) bndr;
  IfaceExpr_t(:dict:)   rhs;  
};

Pstruct IfaceRec_t(:Dictionary_t *dict:){
  HiLen_t                            numBindings;
  IfaceNonRec_t(:dict:)[numBindings] bindings;
};

Punion IfaceBindingBranches_t(:Puint8 tag, Dictionary_t *dict:){
  Pswitch(tag){
    Pcase 0x00: IfaceNonRec_t(:dict:)  nonRec;
    Pcase 0x01: IfaceRec_t   (:dict:)  rec;
  }
};

Pstruct IfaceBinding_t(:Dictionary_t *dict:){
  Pb_uint8                            tag;
  IfaceBindingBranches_t(:tag, dict:) branches;
};

Pstruct IfaceLet_t(:Dictionary_t *dict:){
  IfaceBinding_t(:dict:) binding;
  IfaceExpr_t(:dict:)    body;      /* XXX recursive loop back */
};

Penum IsDupdCC_t Pfrom(Pb_uint8) {OriginalCC, DupdCC};
Penum IsCafCC_t  Pfrom(Pb_uint8) {CafCC, NotCafCC};


Pstruct NormalCC_t(:Dictionary_t *dict:){
  String_t(:dict:) name;
  Module_t(:dict:) module;
  IsDupdCC_t       isDupdCC;
  IsCafCC_t        isCafCC;
}

Punion CostCentreBranches_t(:Puint8 tag, Dictionary_t *dict:){
  Pswitch(tag){
    Pcase 0x00: Pvoid              noCostCentre;
    Pcase 0x01: NormalCC_t(:dict:) normalCostCentre;
    Pcase 0x02: Module_t(:dict:)   allCafsCostCentre;
  }
};

Pstruct CostCentre_t(:Dictionary_t *dict:){
  Pb_uint8                          tag;
  CostCentreBranches_t(:tag, dict:) branches;
};

Punion IfaceNoteBranches_t(:Puint8 tag, Dictionary_t *dict:){
  Pswitch(tag){
    Pcase 0x00: CostCentre_t(:dict:) costCentre;
    Pcase 0x01: IfaceType_t(:dict:)  coerce;
    Pcase 0x03: Pvoid                inlineMe;
    Pcase 0x04: HString_t            coreNote;
  }
};

Pstruct IfaceNote_t(:Dictionary_t *dict:){
  Pb_uint8 tag;
  IfaceNoteBranches_t(:tag, dict:) branches;
};

Punion CCallTargetBranches_t(:Puint8 tag, Dictionary_t *dict:){
  Pswitch(tag){
    Pcase 0x00: String_t(:dict:) staticTarget;
    Pcase 0x01: Pvoid            dynamicTarget;
  }
};

Pstruct CCallTarget_t(:Dictionary_t *dict:){
  Pb_uint8                          tag;
  CCallTargetBranches_t(:tag,dict:) branches;
};

Penum CCallConv_t Pfrom(Pb_uint8){CCallConv, StdCallConv};

Punion SafetyBranches_t(:Puint8 tag:){
  Pswitch(tag){
    Pcase 0x00:  Bool_t playSafe;
    Pcase 0x01:  Pvoid  playRisky;
  }
};

Pstruct Safety_t{
  Pb_uint8                tag;
  SafetyBranches_t(:tag:) branches;
}

Pstruct CCall_t(:Dictionary_t *dict:){
  CCallTarget_t(:dict:) fun;
  CCallConv_t           cconv; 
  Safety_t              safety; 
};

Penum DNKind_t Pfrom (Pb_uint8) { DNMethod, DNField, DNConstructor};

Pstruct DNCall_t{
  Bool_t    isStatic;
  DNKind_t  kind;
  HString_t ass;
  HString_t nm;
};

Punion ForeignCallBranches_t(:Puint8 tag, Dictionary_t *dict:){
  Pswitch(tag){
    Pcase 0x00: CCall_t(:dict:) cCall;
    Pcase 0x01: DNCall_t        dnCall;
  }
};

Pstruct ForeignCall_t(:Dictionary_t *dict:){
  Pb_uint8 tag;
  ForeignCallBranches_t(:tag, dict:) branches;
};

Pstruct IfaceFCall_t(:Dictionary_t *dict:){
  ForeignCall_t(:dict:) foreignCall;
  IfaceType_t(:dict:)   ty;         /* XXX recursive loop back */
};

Punion IfaceExprBranches_t(:Puint8 tag, Dictionary_t *dict:){
  Pswitch(tag){
    Pcase 0x00: String_t      (:dict:) ifaceLcl;
    Pcase 0x01: IfaceType_t   (:dict:) ifaceType;
    Pcase 0x02: IfaceTuple_t  (:dict:) ifaceTuple;
    Pcase 0x03: IfaceLam_t    (:dict:) ifaceLam;
    Pcase 0x04: IfaceApp_t    (:dict:) ifaceApp;
    Pcase 0x05: IfaceCase_t   (:dict:) ifaceCase;
    Pcase 0x06: IfaceLet_t    (:dict:) ifaceLet;
    Pcase 0x07: IfaceNote_t   (:dict:) ifaceNote;
    Pcase 0x08: Literal_t     (:dict:) ifaceLit;
    Pcase 0x09: IfaceFCall_t  (:dict:) ifaceFcall;
    Pcase 0x0a: IfaceExtName_t(:dict:) ifaceExt;
  }
};

Precur Pstruct IfaceExpr_t(:Dictionary_t *dict:){
  Pb_uint8                        tag;
  IfaceExprBranches_t(:tag,dict:) branches;
};

Punion ActivationBranches_t(:Puint8 tag:){
  Pswitch (tag) {
    Pcase 0x00: Pvoid           neverActive;
    Pcase 0x01: Pvoid           alwaysActive;
    Pcase 0x02: CompilerPhase_t activeBefore;
    Pcase 0x03: CompilerPhase_t activeAfter;
  }
};

Pstruct Activation_t{
  Pb_uint8 tag;
  ActivationBranches_t(:tag:) branches;
};

Pstruct Worker_t(:Dictionary_t *dict:){
  IfaceExtName_t(:dict:) name;
  Arity_t                arity;
};


Punion IfaceInfoItemBranches_t(:Puint8 tag, Dictionary_t *dict:){
  Pswitch(tag){
    Pcase 0x00: Arity_t             arityInfo;
    Pcase 0x01: DmdType_t           strictnessInfo;
    Pcase 0x02: IfaceExpr_t(:dict:) unfoldInfo;
    Pcase 0x03: Activation_t        inlineInfo;
    Pcase 0x04: Pvoid               noCafRefsInfo;
    Pcase 0x05: Worker_t(:dict:)    workerInfo;
  }
};

Pstruct IfaceInfoItem_t(:Dictionary_t *dict:){
  Pb_uint8                             tag;
  IfaceInfoItemBranches_t(:tag, dict:) branches;
};

Pstruct HasInfoBody_t(:Dictionary_t *dict:){
  HiLen_t                 length;
  IfaceInfoItem_t(:dict:)[length] infoItems;
};

Pstruct HasInfo_t(:Dictionary_t *dict:){
  Pb_uint32             addressOfEnd;  /- Address of end of hasInfo block
  HasInfoBody_t(:dict:) body;
};

Punion IfaceIdInfoBranches_t(:Puint8 tag, Dictionary_t *dict:){
  Pswitch (tag){
    Pcase 0x00 : Pvoid             noInfo;
    Pcase 0x01:  HasInfo_t(:dict:) hasInfo;         //    Pcase 0x01 : lazyBlock_t hasInfo;  can read lazily
  }
};

Pstruct IfaceIdInfo_t(:Dictionary_t *dict:){
  Pb_uint8                           tag;
  IfaceIdInfoBranches_t(:tag, dict:) branches;
};

Pstruct IfaceId_t(:Dictionary_t *dict:){
  OccName_t    (:dict:) name;
  IfaceType_t  (:dict:) ty;
  //  IfaceIdInfo_t(:dict:) idInfo; 
};

Pstruct IfaceContext_t(:Dictionary_t *dict:){
  HiLen_t                    numPreds;
  PredTy_t(:dict:)[numPreds] preds;
};

Penum StrictnessMark_t Pfrom(Pb_uint8){MarkedStrict, MarkedUnboxed, NotMarkedStrict};

Pstruct VanillaCon_t(:Dictionary_t *dict:){
  OccName_t(:dict:)                 conOcc;
  Bool_t                            isInfix;
  HiLen_t                           numArgTys;
  IfaceType_t(:dict:)[numArgTys]    argTys;
  HiLen_t                           numStrMarks;
  StrictnessMark_t[numStrMarks]     conStricts; 
  HiLen_t                           numFieldLabels;
  OccName_t(:dict:)[numFieldLabels] fieldLabels;
};

Pstruct GadtCon_t(:Dictionary_t *dict:){
  OccName_t(:dict:)                 occ;
  HiLen_t                           numTyVars;
  IfaceTvBndr_t(:dict:)[numTyVars]  tyVars;
  IfaceContext_t(:dict:)            ctxt;
  HiLen_t                           numArgTys;
  IfaceType_t(:dict:)[numArgTys]    argTys;
  HiLen_t                           numResTys;
  IfaceType_t(:dict:)[numArgTys]    resTys;
  HiLen_t                           numStrMarks;
  StrictnessMark_t[numStrMarks]     conStricts; 
};

Punion IfaceConDeclBranches_t(:Puint8 tag, Dictionary_t *dict:){
  Pswitch(tag){
    Pcase 0x00: VanillaCon_t(:dict:)  vanillaCon;
    Pcase 0x01: GadtCon_t   (:dict:)  gadtCon;
  }
};

Pstruct IfaceConDecl_t(:Dictionary_t *dict:){
  Pb_uint8 tag;
  IfaceConDeclBranches_t(:tag, dict:) branches;
};

Pstruct IfaceConDeclList_t(:Dictionary_t *dict:){
  HiLen_t                          numDecls;
  IfaceConDecl_t(:dict:)[numDecls] decls;
}

Punion IfaceConDeclsBranches_t(:Puint8 tag, Dictionary_t *dict:){
  Pswitch(tag){
    Pcase 0x00: Pvoid                      abstractTyCon;
    Pcase 0x01: IfaceConDeclList_t(:dict:) dataTyCon;
    Pcase 0x02: IfaceConDecl_t(:dict:)     newTyCon;
  }
};

Pstruct IfaceConDecls_t(:Dictionary_t *dict:){
  Pb_uint8 tag;
  IfaceConDeclsBranches_t(:tag, dict:) branches;
};

Penum RecFlag_t Pfrom(Pb_uint8){Recursive, NonRecursive};

Pstruct VarPair_t{
  Bool_t occPos;
  Bool_t occNeg;
}

Pstruct ArgVrcs_t {
  HiLen_t numEntries;
  VarPair_t[numEntries] entries;
};

Pstruct IfaceData_t(:Dictionary_t *dict:){
  OccName_t(:dict:)                 name;
  HiLen_t                           numTvBndrs;
  IfaceTvBndr_t(:dict:)[numTvBndrs] tyVars;
  IfaceContext_t(:dict:)            ctxt;
  IfaceConDecls_t(:dict:)           cons;
  RecFlag_t                         rec;
  ArgVrcs_t                         vrcs;
  Bool_t                            generic;
};

Pstruct IfaceSyn_t(:Dictionary_t *dict:){
  OccName_t(:dict:)                tyConName;
  HiLen_t                          numTyVars;
  IfaceTvBndr_t(:dict:)[numTyVars] tyVars;
  ArgVrcs_t                        vrcs;
  IfaceType_t(:dict:)              synRhs;
};

Pstruct FunDep_t(:Dictionary_t *dict:){
  HiLen_t                  numFst;
  String_t(:dict:)[numFst] fst;
  HiLen_t                  numSnd;
  String_t(:dict:)[numSnd] snd;
};

Penum DefMeth_t Pfrom(Pb_uint8){NoDefMeth, DefMeth, GenDefMeth};

Pstruct ClassOp_t(:Dictionary_t *dict:){
  OccName_t(:dict:)   methodName;
  DefMeth_t           defMeth;
  IfaceType_t(:dict:) methodTy;
};

Pstruct IfaceClass_t(:Dictionary_t *dict:){
  IfaceContext_t(:dict:)           ctxt;
  OccName_t(:dict:)                className;
  HiLen_t                          numTyVars;
  IfaceTvBndr_t(:dict:)[numTyVars] tyVars;
  HiLen_t                          numFunDeps;
  FunDep_t(:dict:)[numFunDeps]     funDependencies;
  HiLen_t                          numClassOps;
  ClassOp_t(:dict:)[numClassOps]   funDeps;
  RecFlag_t                        recFlag;
  ArgVrcs_t                        argVrcs;
};

Punion IfaceDeclBranches_t(:Puint8 tag, Dictionary_t *dict:){
  Pswitch(tag){
    Pcase 0x00:  IfaceId_t   (:dict:) iFaceId;
    Pcase 0x01:  Pvoid                iFaceForeign;
    Pcase 0x02:  IfaceData_t (:dict:) iFaceData;
    Pcase 0x03:  IfaceSyn_t  (:dict:) iFaceSyn;
    Pcase 0x04:  IfaceClass_t(:dict:) iFaceClass;
  }
} 
Pwhere {
  tag != 0x01;
};

Pstruct IfaceDecl_t(:Dictionary_t *dict:){
  Pb_uint8                         tag : sfprintf(sfstdout, "The value of IfaceDecl_t tag is: %d\n", tag) + 1;
  IfaceDeclBranches_t(:tag, dict:) branches; 
};

Pstruct Decl_t(:Dictionary_t *dict:){
  Version_t           version;
  IfaceDecl_t(:dict:) iFaceDecl;
};

Pstruct Decls_t(:Dictionary_t *dict:){
  HiLen_t        len;
  Decl_t(:dict:)[len] decls;
};

Penum OverlapFlag_t Pfrom(Pb_uint8){NoOverlap, OverlapOk, Inconherent};

Pstruct Inst_t(:Dictionary_t *dict:){
  IfaceExtName_t(:dict:)                ifInstCls;
  HiLen_t                               numIfInstTys;
  IfaceTyConOpt_t(:dict:)[numIfInstTys] ifInstTys;
  OccName_t(:dict:)                     ifDFun;
  OverlapFlag_t                         ifOverlapFlag;
  OccNameOpt_t(:dict:)                  ifInstOrph;
};

Pstruct Insts_t(:Dictionary_t *dict:){
  HiLen_t        len;
  Inst_t(:dict:)[len] inst;
};

Pstruct Rule_t(:Dictionary_t *dict:){
  String_t(:dict:)                  ruleName;
  Activation_t                      activation;
  HiLen_t                           numRuleBndrs;
  IfaceBndr_t(:dict:)[numRuleBndrs] ruleBndrs;
  IfaceExtName_t(:dict:)            ruleHead;
  HiLen_t                           numRuleArgs;
  IfaceExpr_t(:dict:)[numRuleArgs]  ruleArgs;
  IfaceExpr_t(:dict:)               ruleRhs;
  OccNameOpt_t(:dict:)              ruleOrph;
};

Pstruct RulesBody_t(:Dictionary_t *dict:){
  HiLen_t             len;
  Rule_t(:dict:)[len] rules;
};

Pstruct Rules_t(:Dictionary_t *dict:){
  Pb_uint32           addressOfEnd;
  RulesBody_t(:dict:) body;
};


int checkId(Puint32 id){
  if (id == 0x01face64) {
    WORDSIZE = 8;
    return 1;
  };
  return (id == 0x0001face);
};

Pstruct Hi_t{
  Pendian Pb_uint32      id : checkId(id);
  Pb_uint32              dictAddress;
  ForwardDict_t(:dictAddress - 8 :) fd; 
  GHCversion_t           GHCversion; 
  Pb_uint8               way;  /* what is this? */
  Module_t (:&fd.dict:)  module;
  Bool_t                 isBoot;
  Version_t              modVersion;
  Bool_t                 hasOrphan;
  Dep_t(:&fd.dict:)      dependencies;
  Usages_t(:&fd.dict:)   usages;
  Exports_t(:&fd.dict:)  exports;
  Version_t              exportVersion;
  Fixities_t(:&fd.dict:) fixityInfo; 
  Deprecs_t(:&fd.dict:)  deprecs;
  Decls_t(:&fd.dict:)    decls;
  //  Insts_t(:&fd.dict:)    insts;
  //  Rules_t(:&fd.dict:)    rules;
  Version_t              ruleVersion;
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
