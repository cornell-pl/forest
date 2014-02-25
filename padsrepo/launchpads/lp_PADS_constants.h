#ifndef _PADS_CLASS_CONSTANTS_H_INCLUDED
#define _PADS_CLASS_CONSTANTS_H_INCLUDED

#define P_NUM_TERMINALS (pEndTerminals - pStartTerminals - 1)
#define P_NUM_NON_TERMINALS (pEndNonTerminals - pStartNonTerminals - 1)
#define P_TERM_SELECTABLE (pCase - pStartTerminals - 1)
#define P_NON_TERM_SELECTABLE (pSwitch - pStartNonTerminals - 1)
#define P_TERM_USER_TREE_SELECTABLE (pUserDef - pChar + 1)
#define P_TERM_USER_TREE_SELECT_START pChar
#define P_TERM_END_NONTERM_START_DISTANCE 3

static const char* PADS_names[] = 
  {
    "", // undef
    "ERROR", // illegalop
    "", //start terminals
    "GUESS", //guess
    "char",
    "int",
    "float",
    "fpoint",
    "b",
    "string",
    "countX",
    "countXtoY",
    "timestamp_explicit",
    "timestamp",
    "date_explicit",
    "date",
    "time_explicit",
    "time",
    "ip",
    "",
    "case",
    "",
    "", // lit
    "", // udef
    "", // default
    "", // end terminals,

    "", // start non terminals
    "Pstruct",
    "Punion",
    "Parray",
    //"Popt",
    "Penum",
    "Pswitch",
    "Popt",
    ""
  };

static const char* PADS_generalized_names[] = 
  {
    "", // undef
    "ERROR", // illegalop
    "StartTerminals", //start terminals
    "[ GUESS ]", //guess
    "Pchar\t",
    "Pint\t\t",
    "Pfloat\t",
    "Pfpoint\t",
    "Pb\t\t",
    "Pstring\t",
    "PcountX\t",
    "PcountXtoY ",
    "Ptimestamp_explicit ",
    "Ptimestamp ",
    "Pdate_explicit ",
    "Pdate\t",
    "Ptime_explicit ",
    "Ptime\t",
    "Pip\t\t",
    "[Enum Field] ",
    "Pcase\t",
    "[Free Input] ",
    "[Literal]\t", // lit
    "[UDef]\t", // udef
    "[Default] ", // default
    "EndTerminals", // end terminals,

    "StartNonTerms", // start non terminals
    "Pstruct\t",
    "Punion\t",
    "Parray\t",
    //"Popt\t\t",
    "Penum\t",
    "Pswitch\t",
    "Popt\t\t",
    "EndNonTerms\t"
  };

static const char* PADS_formalized_names[] = 
  {
    "", // undef
    "ERROR", // illegalop
    "StartTerminals", //start terminals
    "[ GUESS ]", //guess
    "Pchar",
    "Pint",
    "Pfloat",
    "Pfpoint",
    "Pb",
    "Pstring",
    "PcountX",
    "PcountXtoY",
    "Ptimestamp_explicit",
    "Ptimestamp",
    "Pdate_explicit",
    "Pdate",
    "Ptime_explicit",
    "Ptime",
    "Pip",
    "",
    "Pcase",
    "[Free Input]",
    "", // lit
    "[UDef]", // udef
    "[Default]", // default
    "EndTerminals", // end terminals,

    "StartNonTerms", // start non terminals
    "Pstruct",
    "Punion",
    "Parray",
    //"Popt",
    "Penum",
    "Pswitch",
    "Popt",
    "EndNonTerms"
  };

static const char* PADS_labels[] = 
  {
    "Undefined",
    "Illegal Operation",
    "start_terminals",
    "[ guess ]",
    "char",
    "int",
    "float",
    "fpoint (fixed point)",
    "b (raw binary)",
    "string",
    "countX",
    "countXtoY",
    "timestamp (explicit)",
    "timestamp",
    "date (explicit)",
    "date",
    "time (explicit)",
    "time",
    "ip",
    "enum field",
    "case",
    "function (free input)",
    "literal",
    "user defined (terminal)",
    "default",
    "end_terminals",
    "start_nonterminals",
    "struct",
    "union",
    "array",
    //"opt",
    "enum",
    "switch",
    "opt",
    "end_nonterminals"
  };


static const char* PADS_short_labels[] = 
  {
    "",
    "!",
    "start_terminals",
    "?",
    "c",
    "i",
    "f",
    "fp",
    "b",
    "s",
    "cX",
    "cY",
    "tX",
    "t",
    "dX",
    "d",
    "tX",
    "t",
    "ip",
    "e",
    "case",
    "[_]",
    "\'\'",
    "udef",
    "def",
    "end_terminals",
    "start_nonterminals",
    "S",
    "U",
    "A",
    "O",
    "E",
    "Sw",
    "end_nonterminals"
  };

/*
static const char* PADS_reserved_words[] = 
  {

  };
*/
// terminal types
#define P_END_MENU_TERMINAL_TYPES pTypeDefault
enum {

  pUndefined = 0,
  pIllegalOp, // this remains positive for easy error reporting (as opposed to seg faults)

  pStartTerminals,
  pGuess, // this is a new one - we try to guess the best fit for a string of text
  pChar,
  pInt, 
  pFloat,
  pFpoint,
  pB,
  pString,
  pCountX,
  pCountXtoY,
  pTimestamp_explicit,
  pTimestamp,
  pDate_explicit,
  pDate,
  pTime_explicit,
  pTime,
  pIp,
  pEnumField,
  pCase,
  pFunction,
  pLit, //literals
  pUserDef, 
  pTypeDefault,
  pEndTerminals,

  // non-terminal/abstract types
  pStartNonTerminals,
  pStruct,
  pUnion, 
  pArray,
  //pOpt,
  pEnum,
  pSwitch,
  pOpt,
  pEndNonTerminals,

  pRoot = 128,

};

enum {
  // character/number encoding
  pchrDefault = 0,
  pnumDefault = 0,
 
  pbinNone = 0,
 
  pchrASCII,
  pchrEBCDIC,

  pnumEBC, 
  pnumBCD,

  pbinSbl,
  pbinSbh,

  pendDefault = 0,
  pendFW,
  pendME,
  pendSE,


};

enum {
  // byte quantities
  p8  = 8,
  p16 = 16,
  p32 = 32,
  p64 = 64,
};

enum {
  /*
  // PLiteral flags
  pRE_LIT      = 0x1,
  pEOR_LIT     = 0x2,
  pFROM_LIT    = 0x4,
  
  // PComposite flags
  pRECORD_COMP = 0x1,
  pSOURCE_COMP = 0x2,
  */

  /* 
  pTypedefFlag = 0x1,
  pComputeFlag = 0x2,
  pRecordFlag  = 0x4,
  pSourceFlag  = 0x8,
  pCstrainFlag = 0x10,
  pPrsChkFlag  = 0x20,
  pEndianFlag  = 0x40,
  pNoSepFlag   = 0x80,
  pTermFlag    = 0x100,
  pLastFlag    = 0x200,
  pEndedFlag   = 0x400,
  pLongestFlag = 0x800,
  pOmitFlag    = 0x1000,
  pForAllFlag  = 0x2000,
  pInFlag      = 0x4000,
  pWhereFlag   = 0x8000,
  pPrefixFlag  = 0x10000,
  pNoneFlag    = 0x20000,
  pSomeFlag    = 0x40000,
  pSepFlag     = 0x80000,
  pInlineFlag  = 0x100000,
  pFromFlag    = 0x200000,
  pCharClsFlag = 0x400000
  */
  pTypedefFlag = 1 << 0,
  pComputeFlag = 1 << 1,
  pRecordFlag  = 1 << 2,
  pSourceFlag  = 1 << 3,
  pCstrainFlag = 1 << 4,
  pPrsChkFlag  = 1 << 5,
  pEndianFlag  = 1 << 6,
  pNoSepFlag   = 1 << 7,
  pTermFlag    = 1 << 8,
  pLastFlag    = 1 << 9,
  pEndedFlag   = 1 << 10,
  pLongestFlag = 1 << 11,
  pOmitFlag    = 1 << 12,
  pForAllFlag  = 1 << 13,
  pInFlag      = 1 << 14,
  pWhereFlag   = 1 << 15,
  pPrefixFlag  = 1 << 16,
  pNoneFlag    = 1 << 17,
  pSomeFlag    = 1 << 18,
  pSepFlag     = 1 << 19,
  pInlineFlag  = 1 << 20,
  pFromFlag    = 1 << 21,
  pCharClsFlag = 1 << 22,
};

static const char *PADS_flag_labels[] = 
  {
    "Ptypedef",
    "Pcompute",
    "Precord",
    "Psource",
    "Pconstraint",
    "Pparsecheck",
    "Pendian",
    "Pnosep",
    "Pterm",
    "Plast",
    "Pended",
    "Plongest",
    "Pomit",
    "Pforall",
    "Pin",
    "Pwhere",
    "Pprefix ",
    "Pnone",
    "Psome",
    "Psep",
    "Pinline ",
    "Pfrom",
    "Pcharclass"
  };

enum
  {
    pTypedef,
    pCompute,
    pRecord,
    pSource,
    pCstrain,
    pPrsChk,
    pEndian,
    pNoSep,
    pTerm,
    pLast,
    pEnded,
    pLongest,
    pOmit,
    pForAll,
    pIn,
    pWhere,
    pPrefix,
    pNone,
    pSome,
    pSep,
    pInline,
    pFrom,
    pCharCls,
};


#define NUM_QUALIFIERS pEndQualifiersQ
static const char *PADS_qualifiers[] = 
  {
    "[ constraint ]",
    "Pparsecheck",
    "Pendian",
    "Pnosep",
    "Psep",
    "Pterm",
    "Plast",
    "Pended",
    "Plongest",
    "Pomit",
    "Pforall",
    "Pin",
    "Pwhere",
    "Pprefix",
    "Pnone",
    "Psome",
    "Pinline",
    "Pfrom",
    "Pcharclass",
    "Peor",
    ""
  };

enum
  {
    pCstrainQ = 0,
    pParsecheckQ,
    pEndianQ,
    pNosepQ,
    pSepQ,
    pTermQ,
    pLastQ,
    pEndedQ,
    pLongestQ,
    pOmitQ,
    pForallQ,
    pInQ,
    pWhereQ,
    pPprefixQ,
    pNoneQ,
    pSomeQ,
    pInlineQ,
    pFromQ,
    pCharclassQ,
    pEorQ,
    pEndQualifiersQ,
  };


#define NUM_STOP_EXPRESSION_TYPES 8
static const char* PADS_expression_names[] = 
  {
    "[ Clear ]",
    "Default",
    "_FW/fixed width",
    "_ME/_SE",
    "explicit times",
    "countX[toY]",
    "ebc",
    "bcd"    
  };


static const char* PADS_expression_prototypes[] = 
  {
    "",
    "\'%c' /*(terminator)*/",
    "\%d /*(width)*/",
    "\"/%s/\" /*(regular expression)*/",
    "\'%c\' /*(delimiter)*/, \"%s\" /*(date fields)*/, P_cstr2timezone(\"%d\") /*(UTC offset)*/",
    "\"%c\" /*(delimiter)*/, %d /*(EOR requirement)*/, %d /*(max scan length)*/",
    "%d /*(number of digits)*/",
    "%d /*(number of digits)*/"
  };

#define NUM_EBC_BCD_BYTE_RANGES 8

static const char* PADS_ebc_byte_ranges[] = 
  {
    "[1-3])*/", 
    "[1-3])*/", 
    "[1-5])*/", 
    "[1-5])*/", 
    "[1-10])*/", 
    "[1-10])*/", 
    "[1-19])*/", 
    "[1-20])*/"
  };

static const char* PADS_bcd_byte_ranges[] = 
  {
    "[1-3])*/", 
    "[1-3])*/", 
    "[1-5])*/", 
    "[1-5])*/", 
    "[1-11])*/", 
    "[1-10])*/", 
    "[1-19])*/", 
    "[1-20])*/"
  };

#define LP_PADS_ISBASE(i) (i > pStartTerminals && i < pEndTerminals)
#define LP_PADS_ISCOMPOSITE(i) (i > pStartNonTerminals && i < pEndNonTerminals)

#define LP_PADS_ISNUMTYPE(i) (i >= pInt && i <= pFpoint)

#endif
