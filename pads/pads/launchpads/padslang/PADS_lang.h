/* *****************************************************************************
 * Mark Daly
 * June 2006
 * PADS_lang - PADS AST implementation for use in LaunchPADS
 *  Main class definitions
 * ************************************************************************** */

#ifndef PADS_LANG_H_INCLUDED__
#define PADS_LANG_H_INCLUDED__

#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <vector>
#include <exception>
//#include "wx/treebase.h"
//#include "wx/treectrl.h"

//using std::string;
//using std::vector;

using namespace std;

#define UNNAMED_STR "UNDEFINED"

#define PADS_LANG_DEBUG 1
#ifndef _PADSGUI_H_INCLUDED
#define DB_P(a, ...)   fprintf(stderr, a, ## __VA_ARGS__ ) // from RHEL documentation, 6.15
#endif

#define PSTRING std::string

#define DEFAULT_PNLIST_LEN 8
#define PTE_SBUF_LEN 1024

static char* PTEBuffer[PTE_SBUF_LEN];

//typedef char* string;

#define P_UNDEFINED 0

#include "PADS_lang_main_class_deps.h"

enum
  {
    PT_UNDEF = 0,
    PT_PNLIST,
    PT_BEGIN_VALID_TYPES,
    PT_UNIT,
    PT_NAME,
    PT_VAR,
    PT_ID,
    PT_LANGUAGE,
    PT_EXPR,
    PT_PRE,
    PT_STRING, // 10
    PT_CHAR,
    PT_REEXPR,
    PT_EXPR2,
    PT_LITERAL,
    PT_PPARSECHECK,
    PT_TYPENAME,
    PT_VARDECL,
    PT_CONSTRAINTS,
    PT_CONSTRAINTS2,
    PT_FUNCTION, // 20 
    PT_PARAMETER,
    PT_ARGUMENT,
    PT_PSOURCE,
    PT_PRECORD,
    PT_PLONGEST,
    PT_MODIFIERS,
    PT_DECL,
    PT_PTYPE,
    PT_MIN_LOCAL,
    PT_MAX_LOCAL, // 30 
    PT_FORALL,
    PT_SIZE,
    PT_ARRAYCONSTRAINTS,
    PT_NOSEP_LOCAL,
    PT_TERMEXPR,
    PT_SEP_LOCAL,
    PT_TERM_LOCAL,
    PT_LAST_LOCAL,
    PT_ENDED_LOCAL,
    PT_OMIT_LOCAL, // 40
    PT_DELIMITERCONSTRAINTS,
    PT_PARRAY,
    PT_PSOME,
    PT_PNONE,
    PT_POPT,
    PT_POMIT,
    PT_PENDIAN,
    PT_PCOMPUTE,
    PT_STRUCTMODIFIERS,
    PT_FROM, // 50
    PT_FIELD,
    PT_PALTERNATES,
    PT_PCHARCLASS,
    PT_PPREFIX,
    PT_ENUMFIELD,
    PT_PENUM,
    PT_PRECURSIVE,
    PT_PSELECT,
    PT_PSTRUCT,
    PT_PTYPEDEF,  // 60 
    PT_PCASE,
    PT_PDEFAULT,
    PT_PSWITCH,
    PT_PUNION,
    PT_PADS,
    PT_COMMENT,
    PT_NATIVE,
    PT_LAST_TYPE,
    PT_ERROR_TYPE
  };

static const int PT_BaseTypes[] = 
  {
    PT_VAR,
    PT_ID,
    PT_EXPR,
    PT_PRE,
    PT_STRING,
    PT_CHAR,
    PT_EXPR2,
    PT_PPARSECHECK,
    PT_TYPENAME,
    PT_ARGUMENT,
    PT_PSOURCE,
    PT_PRECORD,
    PT_PLONGEST,
    PT_POMIT,
    PT_PENDIAN,
    PT_PCOMPUTE,
  };

static const int PT_NumBaseTypes = 6;

static const char* PT_TypeNames[] = 
  {
    "__UNDEFINED_TYPE",
    "__PNODE_LIST",
    "__BEGIN_VALID_TYPES",
    "()",
    "name",
    "var",
    "id",
    "language",
    "Expr",
    "Pre",
    "string",
    "char",
    "REExpr",
    "expr",
    "literal",
    "Pparsecheck",
    "typename",
    "vardecl",
    "Constraints",
    "constraints",
    "function",
    "parameter",
    "argument",
    "Psource",
    "Precord",
    "Plongest",
    "modifiers",
    "decl",
    "ptype",
    "min",
    "max",
    "forAll",
    "size",
    "arrayConstraints",
    "Pnosep",
    "TermExpr",
    "sep",
    "term",
    "last",
    "ended",
    "omit",
    "delimiterConstraints",
    "Parray",
    "Psome",
    "Pnone",
    "Popt",
    "Pomit",
    "Pendian",
    "Pcompute",
    "structModifiers",
    "from",
    "field",
    "Palternates",
    "Pcharclass",
    "Pprefix",
    "enumField",
    "Penum",
    "Precursive",
    "Pselect",
    "Pstruct",
    "Ptypedef",
    "Pcase",
    "Pdefault",
    "Pswitch",
    "Punion",
    "Pads",
    "comment", 
    "native",
    "__LAST_TYPE",
    "__PT_TYPE_ERROR"
  };

static const char* PT_XMLNames[] = 
  {
    "__UNDEFINED_TYPE",
    "__PNODE_LIST",
    "__BEGIN_VALID_TYPES",
    "()",
    "name",
    "var",
    "id",
    "language",
    "Expr",
    "Pre",
    "string",
    "char",
    "REExpr",
    "expr",
    "literal",
    "Pparsecheck",
    "typename",
    "vardecl",
    "sizeConstraints",
    "constraints",
    "function",
    "parameter",
    "argument",
    "source",
    "record",
    "longest",
    "modifiers",
    "decl",
    "ptype",
    "min",
    "max",
    "forAll",
    "size",
    "sizeConstraints",
    "nosep",
    "TermExpr",
    "sep",
    "term",
    "last",
    "ended",
    "omit",
    "delimiterConstraints",
    "array",
    "some",
    "none",
    "opt",
    "omit",
    "endian",
    "compute",
    "structModifiers",
    "from",
    "field",
    "alternates",
    "charclass",
    "prefix",
    "enumField",
    "enum",
    "recursive",
    "select",
    "struct",
    "typedef",
    "case",
    "default",
    "switch",
    "union",
    "PadsC",
    "comment",
    "native",
    "__LAST_TYPE",
    "__PT_TYPE_ERROR"
  };


#define PTM_LAST_ELEMENT -10
#define PTM_PSTRING -100

/* 
// incomplete attempt at a type manifest for dynamic interface generation
static const int* PT_Manifest[] = 
  {
    {PTM_LAST_ELEMENT}, // PT_UNDEF = 0,
    {PTM_LAST_ELEMENT}, // PT_PNLIST,
    {PTM_LAST_ELEMENT}, // PT_BEGIN_VALID_TYPES,
    {PTM_LAST_ELEMENT}, // PT_UNIT,
    {PTM_PSTRING, PTM_LAST_ELEMENT}, // PT_NAME,
    {PTM_PSTRING, PTM_LAST_ELEMENT}, // PT_VAR,
    {PTM_PSTRING, PTM_LAST_ELEMENT}, // PT_ID,
    {PTM_PSTRING, PTM_LAST_ELEMENT}, // PT_LANGUAGE,
    {PTM_PSTRING, PTM_LAST_ELEMENT}, // PT_EXPR,
    {PTM_PSTRING, PTM_LAST_ELEMENT}, // PT_PRE,
    {PTM_PSTRING, PTM_LAST_ELEMENT}, // PT_STRING, // 10
    {PTM_PSTRING, PTM_LAST_ELEMENT}, // PT_CHAR,
    {PTM_PSTRING, PTM_LAST_ELEMENT}, // PT_REEXPR,
    {PTM_PSTRING, PTM_LAST_ELEMENT}, // PT_EXPR2,
    {PTM_PSTRING, PTM_LAST_ELEMENT}, // PT_LITERAL,
    {PTM_PSTRING, PTM_LAST_ELEMENT}, // PT_PPARSECHECK,
    {PTM_PSTRING, PTM_LAST_ELEMENT}, // PT_TYPENAME,
    {PT_TYPENAME, PT_VAR, PTM_LAST_ELEMENT}, // PT_VARDECL,
    {PTM_LAST_ELEMENT}, // PT_CONSTRAINTS,
    {PTM_LAST_ELEMENT}, // PT_CONSTRAINTS2,
    {PTM_LAST_ELEMENT}, // PT_FUNCTION, // 20 
    {PTM_LAST_ELEMENT}, // PT_PARAMETER,
    {PTM_LAST_ELEMENT}, // PT_ARGUMENT,
    {PTM_LAST_ELEMENT}, // PT_PSOURCE,
    {PTM_LAST_ELEMENT}, // PT_PRECORD,
    {PTM_LAST_ELEMENT}, // PT_PLONGEST,
    {PTM_LAST_ELEMENT}, // PT_MODIFIERS,
    {PTM_LAST_ELEMENT}, // PT_DECL,
    {PTM_LAST_ELEMENT}, // PT_PTYPE,
    {PTM_LAST_ELEMENT}, // PT_MIN_LOCAL,
    {PTM_LAST_ELEMENT}, // PT_MAX_LOCAL, // 30 
    {PTM_LAST_ELEMENT}, // PT_FORALL,
    {PTM_LAST_ELEMENT}, // PT_SIZE,
    {PTM_LAST_ELEMENT}, // PT_ARRAYCONSTRAINTS,
    {PTM_LAST_ELEMENT}, // PT_NOSEP_LOCAL,
    {PTM_LAST_ELEMENT}, // PT_TERMEXPR,
    {PTM_LAST_ELEMENT}, // PT_SEP_LOCAL,
    {PTM_LAST_ELEMENT}, // PT_TERM_LOCAL,
    {PTM_LAST_ELEMENT}, // PT_LAST_LOCAL,
    {PTM_LAST_ELEMENT}, // PT_ENDED_LOCAL,
    {PTM_LAST_ELEMENT}, // PT_OMIT_LOCAL, // 40
    {PTM_LAST_ELEMENT}, // PT_DELIMITERCONSTRAINTS,
    {PTM_LAST_ELEMENT}, // PT_PARRAY,
    {PTM_LAST_ELEMENT}, // PT_PSOME,
    {PTM_LAST_ELEMENT}, // PT_PNONE,
    {PTM_LAST_ELEMENT}, // PT_POPT,
    {PTM_LAST_ELEMENT}, // PT_POMIT,
    {PTM_LAST_ELEMENT}, // PT_PENDIAN,
    {PTM_LAST_ELEMENT}, // PT_PCOMPUTE,
    {PTM_LAST_ELEMENT}, // PT_STRUCTMODIFIERS,
    {PTM_LAST_ELEMENT}, // PT_FROM, // 50
    {PTM_LAST_ELEMENT}, // PT_FIELD,
    {PTM_LAST_ELEMENT}, // PT_PALTERNATES,
    {PTM_LAST_ELEMENT}, // PT_PCHARCLASS,
    {PTM_LAST_ELEMENT}, // PT_PPREFIX,
    {PTM_LAST_ELEMENT}, // PT_ENUMFIELD,
    {PTM_LAST_ELEMENT}, // PT_PENUM,
    {PTM_LAST_ELEMENT}, // PT_PRECURSIVE,
    {PTM_LAST_ELEMENT}, // PT_PSELECT,
    {PTM_LAST_ELEMENT}, // PT_PSTRUCT,
    {PTM_LAST_ELEMENT}, // PT_PTYPEDEF,  // 60 
    {PTM_LAST_ELEMENT}, // PT_PCASE,
    {PTM_LAST_ELEMENT}, // PT_PDEFAULT,
    {PTM_LAST_ELEMENT}, // PT_PSWITCH,
    {PTM_LAST_ELEMENT}, // PT_PUNION,
    {PTM_LAST_ELEMENT}, // PT_PADS,
    {PTM_LAST_ELEMENT}, // PT_LAST_TYPE,
    {PTM_LAST_ELEMENT}, // PT_ERROR_TYPE
  };
*/

// PADS AST Exceptions
enum 
  {
    PTE_UNDEFINED = 0,
    PTE_NOT_COMPOSITE_TYPE,
    PTE_NOT_BASE_TYPE,
    PTE_CHILD_ERR,
    PTE_VAL_ERR,
    PTE_CHILD_TYPE_ERR,
    PTE_NO_ATTR_ERR,
    PTE_FACTORY_UNDEF_ERR,
    PTE_NO_PRINT_FUN_DEFINED_ERR,
    PTE_CHILD_UNDEF_ERR,
    PTE_NO_VALIDATE_FUN_DEFINED_ERR,
    PTE_LAST_MSG,
  };

static const char* PTE_HEADER = "PADS AST Exception: ";
static const char* PTE_MSGS[] = 
  {
    "undefined error",
    "type is not composite (cannot add children)",
    "type is not base (cannot get value)",
    "illegal child element addition attempted",
    "illegal value specification attempted",
    "addition of illegal child type",
    "addition of atribute when type has no attributes",
    "factory asked to build undefined type",
    "no printing function defined for contents of this type",
    "cannot print undefined child element", 
    "no validation function defined",
    ""
  };

class PTException : public exception
{
  char* message;
  int errorCode;
  int lineNum;
  bool hasMsg;
  bool hasCode;
  int throwerType;
 public:
  PTException(char* msg, int type, int line);
  PTException(int code, int type, int line);
  PTException(char* msg, int code, int type, int line);
  
  char* printReport(char* sbuf, int len);

};

#define XML_ADD_LEVEL_SPACE 3
#define CODE_ADD_LEVEL_SPACE 3

class PNodeP
{
 public:
  int nodeType;
  int nodeFlags;
  PSTRING nodeLabel;
  int nodeID;

  int refCount;

  PNodeP* parent;

  PNodeP* nextSibling;
  PNodeP* previousSibling;

  PADSValContainer valContainer;

  //wxTreeItemData* treeItem;

  PNodeP();
  PNodeP(int which);
  PNodeP(int which, PSTRING name);
  PNodeP(int which, int flags);
  PNodeP(int which, PSTRING name, int flags);
  PNodeP(PSTRING name);
  ~PNodeP();

  int getType();
  int getFlags();
  PSTRING getName();
  int getRefCount();

  void setType(int type);
  void setFlags(int flags);
  void setName(PSTRING name);
  void setRefCount(int count);

  int incRefCount();
  int decRefCount();

  virtual PNodeP* addChild(PNodeP* newChild);
  virtual PNodeP* addValue(PSTRING& newVal);
  virtual bool    getValue(PSTRING& printStr);
  virtual int     setAttribute(PSTRING attrName, PSTRING attrValue);

  static int  comparePStrings(PSTRING& a, PSTRING& b);
  static bool isBaseType(int type);
  bool isBaseTypeNode();
  bool isCompositeTypeNode();

  static bool stringContainsIllegalXMLEntities(const char* c);
  static bool isStringWhitespace(char* c);

  virtual void serializeXMLEmpty(int spaceDepth, PSTRING& printStr);
  virtual void serializeXMLOpen(int spaceDepth, PSTRING& printStr);
  virtual void serializeXMLClose(int spaceDepth, PSTRING& printStr);
  virtual void serializeXMLContents(int spaceDepth, PSTRING& printStr);
  virtual void serializeXMLAttributes(PSTRING& printStr);

  virtual void serializeXMLRepresentation(int spaceDepth, PSTRING& printStr);

  virtual bool validate(PNodeP** errorNode);

  virtual void codifyOpen(int spaceDepth, PSTRING& printStr);
  virtual void codifyClose(int spaceDepth, PSTRING& printStr);
  virtual void codifyContents(int spaceDepth, PSTRING& printStr);

  virtual void codifyRepresentation(int spaceDepth, PSTRING& printStr);
  //void setTreeElement(wxTreeItemData* newElement);
  //wxTreeItemData* getTreeElement();

  virtual bool canChangeToType(int newType);
  virtual PNodeP* convertAndCopy(int newType, PNodeP* copyTo);

  virtual int getTextualRepresentation(PSTRING& nameStr);

  virtual PADSPNodeTreeNode* makeBasicTree(PADSPNodeTreeNode* parent);

  virtual void getTypeStr(PSTRING& typeStr);
  virtual void getNameStr(PSTRING& nameStr);

  virtual int canAddChildTypes(int* typeList, int max);
  virtual int removeChildNode(PNodeP* removeThis);
  virtual int makeCompleteNode();

  virtual int insertIntoNameMap(PADSTypeMap* nameMap);
  virtual int insertIntoPathMap(PADSTypeMap* pathMap, PSTRING& pathStr);
  virtual int insertIntoTypeMap(PADSTypeMap* typeMap);
  // new_methods_point
  virtual PADSUniTreeNode* makeUnifiedTree(PADSUniTreeNode* parent, PSTRING& varStr, PADSTypeMap* typeMap);
};



class unitS : public PNodeP
{
 public:
  unitS();
  void serializeXMLRepresentation(int spaceDepth, PSTRING& printStr);
  bool validate(PNodeP** errorNode);
  void codifyRepresentation(int spaceDepth, PSTRING& printStr);
  PADSPNodeTreeNode* makeBasicTree(PADSPNodeTreeNode* parent);
  // new_methods_point

  bool unit;
};
typedef unitS* unitT;

class nameS : public PNodeP
{
 public:
  nameS();
  PNodeP* addValue(PSTRING& newVal);
  bool    getValue(PSTRING& printStr);
  void serializeXMLContents(int spaceDepth, PSTRING& printStr);
  bool validate(PNodeP** errorNode);
  void codifyRepresentation(int spaceDepth, PSTRING& printStr);
  bool canChangeToType(int newType);
  PNodeP* convertAndCopy(int newType, PNodeP* copyTo);
  int getTextualRepresentation(PSTRING& nameStr);
  void getTypeStr(PSTRING& typeStr);
  void getNameStr(PSTRING& nameStr);
  PADSPNodeTreeNode* makeBasicTree(PADSPNodeTreeNode* parent);
  // new_methods_point

  PSTRING val;
};
typedef nameS* nameT;

class varS : public PNodeP
{
 public:
  varS();
  PNodeP* addValue(PSTRING& newVal);
  bool    getValue(PSTRING& printStr);
  void serializeXMLContents(int spaceDepth, PSTRING& printStr);
  bool validate(PNodeP** errorNode);
  void codifyRepresentation(int spaceDepth, PSTRING& printStr);
  bool canChangeToType(int newType);
  PNodeP* convertAndCopy(int newType, PNodeP* copyTo);
  int getTextualRepresentation(PSTRING& nameStr);
  void getTypeStr(PSTRING& typeStr);
  void getNameStr(PSTRING& nameStr);
  PADSPNodeTreeNode* makeBasicTree(PADSPNodeTreeNode* parent);
  // new_methods_point

  PSTRING val;
};
typedef varS* varT;

class idS : public PNodeP
{
 public:
  idS();
  PNodeP* addValue(PSTRING& newVal);
  bool    getValue(PSTRING& printStr);
  void serializeXMLContents(int spaceDepth, PSTRING& printStr);
  bool validate(PNodeP** errorNode);
  void codifyRepresentation(int spaceDepth, PSTRING& printStr);
  bool canChangeToType(int newType);
  PNodeP* convertAndCopy(int newType, PNodeP* copyTo);
  int getTextualRepresentation(PSTRING& nameStr);
  void getTypeStr(PSTRING& typeStr);
  void getNameStr(PSTRING& nameStr);
  PADSPNodeTreeNode* makeBasicTree(PADSPNodeTreeNode* parent);
  // new_methods_point

  PSTRING val;
};
typedef idS* idT;

/*
class languageS : public PNodeP
{
 public:
  languageS();

  PSTRING val;
};
typedef languageS* languageT;
*/

class ExprS : public PNodeP
{
 public:
  ExprS();
  PNodeP* addValue(PSTRING& newVal);
  bool    getValue(PSTRING& printStr);
  int setAttribute(PSTRING attrName, PSTRING attrValue);
  void serializeXMLContents(int spaceDepth, PSTRING& printStr);
  void serializeXMLAttributes(PSTRING& printStr);
  bool validate(PNodeP** errorNode);
  void codifyRepresentation(int spaceDepth, PSTRING& printStr);
  bool canChangeToType(int newType);
  PNodeP* convertAndCopy(int newType, PNodeP* copyTo);
  int getTextualRepresentation(PSTRING& nameStr);
  void getTypeStr(PSTRING& typeStr);
  void getNameStr(PSTRING& nameStr);
  PADSPNodeTreeNode* makeBasicTree(PADSPNodeTreeNode* parent);
  // new_methods_point

  bool has_language;
  PSTRING language;
  //languageT language;

  PSTRING textBlock;
};
typedef ExprS* ExprT;

typedef ExprS PreS;
typedef PreS* PreT;

class stringS : public PNodeP
{
 public:
  stringS();
  PNodeP* addValue(PSTRING& newVal);
  bool    getValue(PSTRING& printStr);
  void serializeXMLContents(int spaceDepth, PSTRING& printStr);
  bool validate(PNodeP** errorNode);
  void codifyRepresentation(int spaceDepth, PSTRING& printStr);
  bool canChangeToType(int newType);
  PNodeP* convertAndCopy(int newType, PNodeP* copyTo);
  int getTextualRepresentation(PSTRING& nameStr);
  void getTypeStr(PSTRING& typeStr);
  void getNameStr(PSTRING& nameStr);
  // new_methods_point

  PSTRING val;
};
typedef stringS* stringT;

class charS : public PNodeP
{
 public:
  charS();
  PNodeP* addValue(PSTRING& newVal);
  bool    getValue(PSTRING& printStr);
  void serializeXMLContents(int spaceDepth, PSTRING& printStr);
  bool validate(PNodeP** errorNode);
  void codifyRepresentation(int spaceDepth, PSTRING& printStr);
  bool canChangeToType(int newType);
  PNodeP* convertAndCopy(int newType, PNodeP* copyTo);
  int getTextualRepresentation(PSTRING& nameStr);
  void getTypeStr(PSTRING& typeStr);
  void getNameStr(PSTRING& nameStr);
  // new_methods_point

  PSTRING val;  // this should be only one character, but we won't be picky
};
typedef charS* charT;

class REExprS : public PNodeP
{
 public:
  REExprS();
  PNodeP* addValue(PSTRING& newVal);
  bool    getValue(PSTRING& printStr);
  PNodeP* addChild(PNodeP* newChild);
  void serializeXMLContents(int spaceDepth, PSTRING& printStr);
  bool validate(PNodeP** errorNode);
  void codifyRepresentation(int spaceDepth, PSTRING& printStr);
  bool canChangeToType(int newType);
  PNodeP* convertAndCopy(int newType, PNodeP* copyTo);
  int getTextualRepresentation(PSTRING& nameStr);
  void getTypeStr(PSTRING& typeStr);
  void getNameStr(PSTRING& nameStr);
  PADSPNodeTreeNode* makeBasicTree(PADSPNodeTreeNode* parent);
  int makeCompleteNode();
  // new_methods_point

  enum REExprEnum
    {
      UNDEFINED = 0x0,
      PRE = 0x1,
      STRING = 0x2,
      CHAR = 0x4,
      ID = 0x8    
    };
  enum REExprEnum which;
  PreT Pre;
  stringT str;
  charT chr;
  idT id;
};
typedef REExprS* REExprT;

typedef REExprS literalS;
typedef literalS* literalT;

typedef ExprS exprS;
typedef exprS* exprT;

typedef ExprS PparsecheckS;  // pPC
typedef PparsecheckS* PparsecheckT;

class typenameS : public PNodeP
{
 public:
  typenameS();
  PNodeP* addValue(PSTRING& newVal);
  bool    getValue(PSTRING& printStr);
  void serializeXMLContents(int spaceDepth, PSTRING& printStr);
  bool validate(PNodeP** errorNode);
  void codifyRepresentation(int spaceDepth, PSTRING& printStr);
  bool canChangeToType(int newType);
  PNodeP* convertAndCopy(int newType, PNodeP* copyTo);
  int getTextualRepresentation(PSTRING& nameStr);
  void getTypeStr(PSTRING& typeStr);
  void getNameStr(PSTRING& nameStr);
  PADSPNodeTreeNode* makeBasicTree(PADSPNodeTreeNode* parent);
  // new_methods_point

  PSTRING val;
};
typedef typenameS* typenameT;

class vardeclS : public PNodeP
{
 public:
  vardeclS();
  PNodeP* addChild(PNodeP* newChild);
  void serializeXMLContents(int spaceDepth, PSTRING& printStr);
  bool validate(PNodeP** errorNode);
  void codifyRepresentation(int spaceDepth, PSTRING& printStr);
  bool canChangeToType(int newType);
  PNodeP* convertAndCopy(int newType, PNodeP* copyTo);
  int getTextualRepresentation(PSTRING& nameStr);
  void getTypeStr(PSTRING& typeStr);
  void getNameStr(PSTRING& nameStr);
  PADSPNodeTreeNode* makeBasicTree(PADSPNodeTreeNode* parent);
  int makeCompleteNode();
  // new_methods_point

  typenameT typname;
  varT var;
};
typedef vardeclS* vardeclT;


typedef union
{
  exprT expr;
  PparsecheckT Pparsecheck;
} expr_PparsecheckU;

class expr_PparsecheckS
{
 public:
  enum expr_PparsecheckEnum
    {
      UNDEFINED = 0x0,
      EXPR = 0x1,
      PPARSECHECK = 0x2
    };
  enum expr_PparsecheckEnum which;
  expr_PparsecheckU expr_Pparsecheck;
};
typedef expr_PparsecheckS* expr_PparsecheckT;

class ConstraintsS : public PNodeP
{
 public:
  ConstraintsS();
  PNodeP* addChild(PNodeP* newChild);
  void serializeXMLContents(int spaceDepth, PSTRING& printStr);
  bool validate(PNodeP** errorNode);
  void codifyRepresentation(int spaceDepth, PSTRING& printStr);
  bool canChangeToType(int newType);
  PNodeP* convertAndCopy(int newType, PNodeP* copyTo);
  PADSPNodeTreeNode* makeBasicTree(PADSPNodeTreeNode* parent);
  int canAddChildTypes(int* typeList, int max);
  int removeChildNode(PNodeP* removeThis);
  // new_methods_point

  vector<vardeclT> vardecl;
  vector<expr_PparsecheckT> expr_Pparsecheck;
};
typedef ConstraintsS* ConstraintsT;

typedef ConstraintsS constraintsS;
typedef constraintsS* constraintsT;

class functionS : public PNodeP
{
 public:
  functionS();
  PNodeP* addChild(PNodeP* newChild);
  void serializeXMLContents(int spaceDepth, PSTRING& printStr);
  bool validate(PNodeP** errorNode);
  void codifyContents(int spaceDepth, PSTRING& printStr);
  bool canChangeToType(int newType);
  PNodeP* convertAndCopy(int newType, PNodeP* copyTo);
  int getTextualRepresentation(PSTRING& nameStr);
  void getNameStr(PSTRING& nameStr);
  PADSPNodeTreeNode* makeBasicTree(PADSPNodeTreeNode* parent);
  int makeCompleteNode();
  // new_methods_point

  nameT name;
  exprT expr;
};
typedef functionS* functionT;


class parameterS : public PNodeP
{
 public:
  parameterS();
  PNodeP* addChild(PNodeP* newChild);
  void serializeXMLContents(int spaceDepth, PSTRING& printStr);
  bool validate(PNodeP** errorNode);
  void codifyContents(int spaceDepth, PSTRING& printStr);
  bool canChangeToType(int newType);
  PNodeP* convertAndCopy(int newType, PNodeP* copyTo);
  void getTypeStr(PSTRING& typeStr);
  void getNameStr(PSTRING& nameStr);
  PADSPNodeTreeNode* makeBasicTree(PADSPNodeTreeNode* parent);
  int makeCompleteNode();
  // new_methods_point

  typenameT typname;  // Should be typeSpecifier
  varT var;  // Parameter name
};
typedef parameterS* parameterT;

typedef ExprS argumentS;
typedef argumentS* argumentT;

// a concession to a unit-less type system... alas
typedef unitS PsourceS;
typedef unitS PrecordS;
typedef unitS PlongestS;

typedef PsourceS*  PsourceT;
typedef PrecordS*  PrecordT;
typedef PlongestS* PlongestT;

class modifiersS : public PNodeP
{
 public:
  modifiersS();
  PNodeP* addChild(PNodeP* newChild);
  void serializeXMLRepresentation(int spaceDepth, PSTRING& printStr);
  void codifyRepresentation(int spaceDepth, PSTRING& printStr);
  bool canChangeToType(int newType);
  PNodeP* convertAndCopy(int newType, PNodeP* copyTo);
  PADSPNodeTreeNode* makeBasicTree(PADSPNodeTreeNode* parent);
  int canAddChildTypes(int* typeList, int max);
  int removeChildNode(PNodeP* removeThis);
  // new_methods_point

  bool has_Psource;
  PsourceT Psource;

  bool has_Precord;
  PrecordT Precord;

  bool has_Plongest;
  PlongestT Plongest;
};
typedef modifiersS* modifiersT;

// hack to fix mistake -> group != element in XSchema!!!
/*
class modifiersT : public PNodeP
{
 public:
  PNodeP* addChild(PNodeP* newChild);
  void init();
  void serializeXMLRepresentation(int spaceDepth, PSTRING& printStr);

  bool has_Psource;
  PsourceT Psource;

  bool has_Precord;
  PrecordT Precord;

  bool has_Plongest;
  PlongestT Plongest;
};
*/
class declS : public PNodeP
{
 public:
  declS();
  PNodeP* addChild(PNodeP* newChild);
  void serializeXMLContents(int spaceDepth, PSTRING& printStr);
  void codifyRepresentation(int spaceDepth, PSTRING& printStr);
  bool canChangeToType(int newType);
  PNodeP* convertAndCopy(int newType, PNodeP* copyTo);
  int getTextualRepresentation(PSTRING& nameStr);
  void getNameStr(PSTRING& nameStr);
  PADSPNodeTreeNode* makeBasicTree(PADSPNodeTreeNode* parent);
  int canAddChildTypes(int* typeList, int max);
  int removeChildNode(PNodeP* removeThis);
  int makeCompleteNode();
  // new_methods_point

  nameT name;
  vector<parameterT> parameter;  // pTyParams
};
typedef declS* declT;


class ptypeS : public PNodeP
{
 public:
  ptypeS();
  PNodeP* addChild(PNodeP* newChild);
  void serializeXMLContents(int spaceDepth, PSTRING& printStr);
  void codifyRepresentation(int spaceDepth, PSTRING& printStr);
  bool canChangeToType(int newType);
  PNodeP* convertAndCopy(int newType, PNodeP* copyTo);
  int getTextualRepresentation(PSTRING& nameStr);
  void getNameStr(PSTRING& nameStr);
  PADSPNodeTreeNode* makeBasicTree(PADSPNodeTreeNode* parent);
  int canAddChildTypes(int* typeList, int max);
  int removeChildNode(PNodeP* removeThis);
  int makeCompleteNode();
  // new_methods_point

  nameT name;
  vector<argumentT> argument;  // pOptActParams
};
typedef ptypeS* ptypeT;


class forAllS : public PNodeP
{
 public:
  forAllS();
  PNodeP* addChild(PNodeP* newChild);
  void serializeXMLContents(int spaceDepth, PSTRING& printStr);
  void codifyContents(int spaceDepth, PSTRING& printStr);
  bool canChangeToType(int newType);
  PNodeP* convertAndCopy(int newType, PNodeP* copyTo);
  PADSPNodeTreeNode* makeBasicTree(PADSPNodeTreeNode* parent);
  int makeCompleteNode();
  // new_methods_point

  varT var;
  ExprT min;
  ExprT max;
  constraintsT constraints;
};
typedef forAllS* forAllT;


class sizeS : public PNodeP
{
 public:
  sizeS();
  PNodeP* addChild(PNodeP* newChild);
  void serializeXMLContents(int spaceDepth, PSTRING& printStr);
  void codifyRepresentation(int spaceDepth, PSTRING& printStr);
  bool canChangeToType(int newType);
  PNodeP* convertAndCopy(int newType, PNodeP* copyTo);
  PADSPNodeTreeNode* makeBasicTree(PADSPNodeTreeNode* parent);
  int canAddChildTypes(int* typeList, int max);
  int removeChildNode(PNodeP* removeThis);
  // new_methods_point

  bool has_min;
  ExprT min;

  bool has_max;
  ExprT max;
};
typedef sizeS* sizeT;


typedef union
{
  forAllT forAll;
  PparsecheckT Pparsecheck;
} forAll_PparsecheckU;

class forAll_PparsecheckS
{
 public:
  enum forAll_PparsecheckEnum
    {
      UNDEFINED = 0x0,
      FORALL = 0x1,
      PPARSECHECK = 0x2
    };
  enum forAll_PparsecheckEnum which;
  forAll_PparsecheckU forAll_Pparsecheck;
};
typedef forAll_PparsecheckS* forAll_PparsecheckT;

class arrayConstraintsS : public PNodeP
{
 public:
  arrayConstraintsS();
  PNodeP* addChild(PNodeP* newChild);
  void serializeXMLContents(int spaceDepth, PSTRING& printStr);
  void codifyRepresentation(int spaceDepth, PSTRING& printStr);
  bool canChangeToType(int newType);
  PNodeP* convertAndCopy(int newType, PNodeP* copyTo);
  PADSPNodeTreeNode* makeBasicTree(PADSPNodeTreeNode* parent);
  int canAddChildTypes(int* typeList, int max);
  int removeChildNode(PNodeP* removeThis);
  // new_methods_point

  bool has_size;
  sizeT size;
  vector<forAll_PparsecheckT> forAll_Pparsecheck;
};
typedef arrayConstraintsS* arrayConstraintsT;


class TermExprS : public PNodeP
{
 public:
  TermExprS();
  PNodeP* addChild(PNodeP* newChild);
  void serializeXMLContents(int spaceDepth, PSTRING& printStr);
  void codifyRepresentation(int spaceDepth, PSTRING& printStr);
  bool canChangeToType(int newType);
  PNodeP* convertAndCopy(int newType, PNodeP* copyTo);
  int getTextualRepresentation(PSTRING& nameStr);
  PADSPNodeTreeNode* makeBasicTree(PADSPNodeTreeNode* parent);
  int canAddChildTypes(int* typeList, int max);
  // new_methods_point

  enum Pnosep_regexpEnum
    {
      UNDEFINED = 0x0,
      PNOSEP = 0x1,
      REGEXP = 0x2
    };
  enum Pnosep_regexpEnum which;
  unitT Pnosep;
  REExprT regexp;
};
typedef TermExprS* TermExprT;


class delimiterConstraintsS : public PNodeP
{
 public:
  delimiterConstraintsS();
  PNodeP* addChild(PNodeP* newChild);
  void serializeXMLContents(int spaceDepth, PSTRING& printStr);
  void codifyContents(int spaceDepth, PSTRING& printStr);
  bool canChangeToType(int newType);
  PNodeP* convertAndCopy(int newType, PNodeP* copyTo);
  PADSPNodeTreeNode* makeBasicTree(PADSPNodeTreeNode* parent);
  int canAddChildTypes(int* typeList, int max);
  int removeChildNode(PNodeP* removeThis);
  // new_methods_point

  bool has_sep;
  REExprT sep;

  bool has_term;
  TermExprT term;

  bool has_last;
  ConstraintsT last;

  bool has_ended;
  ConstraintsT ended;

  bool has_omit;
  ConstraintsT omit;

  bool has_longest;
  PlongestT Plongest;
};
typedef delimiterConstraintsS* delimiterConstraintsT;


class ParrayS : public PNodeP
{
 public:
  ParrayS();
  PNodeP* addChild(PNodeP* newChild);
  void serializeXMLContents(int spaceDepth, PSTRING& printStr);
  void codifyContents(int spaceDepth, PSTRING& printStr);
  bool canChangeToType(int newType);
  PNodeP* convertAndCopy(int newType, PNodeP* copyTo);
  int getTextualRepresentation(PSTRING& nameStr);
  void getNameStr(PSTRING& nameStr);
  PADSPNodeTreeNode* makeBasicTree(PADSPNodeTreeNode* parent);
  int canAddChildTypes(int* typeList, int max);
  int removeChildNode(PNodeP* removeThis);
  int makeCompleteNode();
  PADSUniTreeNode* makeUnifiedTree(PADSUniTreeNode* parent, PSTRING& varStr, PADSTypeMap* typeMap);
  // new_methods_point

  modifiersT modifiers;
  declT decl;
  ptypeT ptype;

  bool has_delimiterConstraints;
  delimiterConstraintsT delimiterConstraints;

  bool has_arrayConstraints;  // pArrayConstraintOpt
  arrayConstraintsT arrayConstraints;  // pArrayPostConds
};
typedef ParrayS* ParrayT;


class PsomeS : public PNodeP
{
 public:
  PsomeS();
  PNodeP* addChild(PNodeP* newChild);
  void serializeXMLContents(int spaceDepth, PSTRING& printStr);
  void codifyContents(int spaceDepth, PSTRING& printStr);
  bool canChangeToType(int newType);
  PNodeP* convertAndCopy(int newType, PNodeP* copyTo);
  PADSPNodeTreeNode* makeBasicTree(PADSPNodeTreeNode* parent);
  int makeCompleteNode();
  // new_methods_point

  varT var;
  constraintsT constraints;
};
typedef PsomeS* PsomeT;


class PnoneS : public PNodeP
{
 public:
  PnoneS();
  PNodeP* addChild(PNodeP* newChild);
  void serializeXMLContents(int spaceDepth, PSTRING& printStr);
  void codifyContents(int spaceDepth, PSTRING& printStr);
  bool canChangeToType(int newType);
  PNodeP* convertAndCopy(int newType, PNodeP* copyTo);
  PADSPNodeTreeNode* makeBasicTree(PADSPNodeTreeNode* parent);
  int makeCompleteNode();
  // new_methods_point

  constraintsT constraints;
};
typedef PnoneS* PnoneT;


class PoptS : public PNodeP
{
 public:
  PoptS();
  PNodeP* addChild(PNodeP* newChild);
  void serializeXMLContents(int spaceDepth, PSTRING& printStr);
  void serializeXMLRepresentation(int spaceDepth, PSTRING& printStr);
  void codifyRepresentation(int spaceDepth, PSTRING& printStr);
  bool canChangeToType(int newType);
  PNodeP* convertAndCopy(int newType, PNodeP* copyTo);
  int getTextualRepresentation(PSTRING& nameStr);
  PADSPNodeTreeNode* makeBasicTree(PADSPNodeTreeNode* parent);
  int canAddChildTypes(int* typeList, int max);
  int removeChildNode(PNodeP* removeThis);
  int makeCompleteNode();
  // new_methods_point

  enum constraints_PsomePnoneEnum
    {
      UNDEFINED = 0x0,
      CONSTRAINTS = 0x1,
      SOMENONE = 0x2
    };
  enum constraints_PsomePnoneEnum which;

  constraintsT constraints;  // pConstraintsWComma

  bool has_Psome;
  PsomeT Psome;

  bool has_Pnone;
  PnoneT Pnone;
};
typedef PoptS* PoptT;

typedef unitS PomitS;  // pStructQualifier
typedef unitS PendianS;  // pStructQualifier
typedef ExprS PcomputeS;  // pMainfestField

typedef PomitS* PomitT;
typedef PendianS* PendianT;
typedef PcomputeS* PcomputeT;

class structModifiersS : public PNodeP
{
 public:
  structModifiersS();
  PNodeP* addChild(PNodeP* newChild);
  void serializeXMLContents(int spaceDepth, PSTRING& printStr);
  void codifyContents(int spaceDepth, PSTRING& printStr);
  bool canChangeToType(int newType);
  PNodeP* convertAndCopy(int newType, PNodeP* copyTo);
  // new_methods_point

  bool has_Pomit;
  PomitT Pomit;

  bool has_Pendian;
  PendianT Pendian;

  bool has_Popt;
  PoptT Popt;

  bool has_Pcompute;
  PcomputeT Pcompute;
};
typedef structModifiersS* structModifiersT;

typedef REExprS fromS;
typedef fromS* fromT;

class fieldS : public PNodeP
{
 public:
  fieldS();
  PNodeP* addChild(PNodeP* newChild);
  void serializeXMLContents(int spaceDepth, PSTRING& printStr);
  //void serializeXMLClose(int spaceDepth, PSTRING& printStr);
  void codifyContents(int spaceDepth, PSTRING& printStr);
  bool canChangeToType(int newType);
  PNodeP* convertAndCopy(int newType, PNodeP* copyTo);
  int getTextualRepresentation(PSTRING& nameStr);
  void getTypeStr(PSTRING& typeStr);
  void getNameStr(PSTRING& nameStr);
  PADSPNodeTreeNode* makeBasicTree(PADSPNodeTreeNode* parent);
  int canAddChildTypes(int* typeList, int max);
  int removeChildNode(PNodeP* removeThis);
  int makeCompleteNode();
  PADSUniTreeNode* makeUnifiedTree(PADSUniTreeNode* parent, PSTRING& varStr, PADSTypeMap* typeMap);
  // new_methods_point

  bool has_Pomit;
  PomitT Pomit;

  bool has_Pendian;
  PendianT Pendian;

  bool has_Popt;
  PoptT Popt;

  bool has_Pcompute;
  PcomputeT Pcompute;

  ptypeT ptype;
  nameT name;

  bool has_from;
  fromT from;

  bool has_arrayConstraints;
  arrayConstraintsT arrayConstraints;

  bool has_constraints;
  constraintsT constraints;
};
typedef fieldS* fieldT;

typedef union
{
  fieldT field;
  literalT literal;
} field_literalU;

struct field_literalS
{
 public:
  enum field_literalEnum
    {
      UNDEFINED = 0x0,
      FIELD = 0x1,
      LITERAL = 0x2
    };
  enum field_literalEnum which;
  field_literalU field_literal;
};
typedef field_literalS* field_literalT;

class PalternatesS : public PNodeP
{
 public:
  PalternatesS();
  PNodeP* addChild(PNodeP* newChild);
  void serializeXMLContents(int spaceDepth, PSTRING& printStr);
  void codifyContents(int spaceDepth, PSTRING& printStr);
  bool canChangeToType(int newType);
  PNodeP* convertAndCopy(int newType, PNodeP* copyTo);
  int getTextualRepresentation(PSTRING& nameStr);
  void getNameStr(PSTRING& nameStr);
  PADSPNodeTreeNode* makeBasicTree(PADSPNodeTreeNode* parent);
  int canAddChildTypes(int* typeList, int max);
  int removeChildNode(PNodeP* removeThis);
  int makeCompleteNode();
  PADSUniTreeNode* makeUnifiedTree(PADSUniTreeNode* parent, PSTRING& varStr, PADSTypeMap* typeMap);
  // new_methods_point

  modifiersT modifiers;
  declT decl;
  vector<field_literalT> field_literal;  // pStructFieldList
  
  bool has_constraints;
  constraintsT constraints;  // pPostCondOpt
};
typedef PalternatesS* PalternatesT;


class PcharclassS : public PNodeP
{
 public:
  PcharclassS();
  PNodeP* addChild(PNodeP* newChild);
  void serializeXMLContents(int spaceDepth, PSTRING& printStr);
  void codifyContents(int spaceDepth, PSTRING& printStr);
  bool canChangeToType(int newType);
  PNodeP* convertAndCopy(int newType, PNodeP* copyTo);
  int getTextualRepresentation(PSTRING& nameStr);
  void getNameStr(PSTRING& nameStr);
  PADSPNodeTreeNode* makeBasicTree(PADSPNodeTreeNode* parent);
  int makeCompleteNode();
  // new_methods_point

  nameT name;
  exprT expr;
};
typedef PcharclassS* PcharclassT;

class enumFieldS : public PNodeP
{
 public:
  enumFieldS();
  PNodeP* addChild(PNodeP* newChild);
  void serializeXMLContents(int spaceDepth, PSTRING& printStr);
  void codifyRepresentation(int spaceDepth, PSTRING& printStr);
  bool canChangeToType(int newType);
  PNodeP* convertAndCopy(int newType, PNodeP* copyTo);
  int getTextualRepresentation(PSTRING& nameStr);
  void getNameStr(PSTRING& nameStr);
  PADSPNodeTreeNode* makeBasicTree(PADSPNodeTreeNode* parent);
  int canAddChildTypes(int* typeList, int max);
  int removeChildNode(PNodeP* removeThis);
  int makeCompleteNode();
  // new_methods_point

  nameT name;
  
  bool has_from;
  fromT from;

  bool has_expr;
  exprT expr;
};
typedef enumFieldS* enumFieldT;


typedef stringS PprefixS;
typedef PprefixS* PprefixT;

class PenumS : public PNodeP
{
 public:
  PenumS();
  PNodeP* addChild(PNodeP* newChild);
  //PNodeP* addValue(PSTRING& newVal);
  void serializeXMLContents(int spaceDepth, PSTRING& printStr);
  void codifyContents(int spaceDepth, PSTRING& printStr);
  bool canChangeToType(int newType);
  PNodeP* convertAndCopy(int newType, PNodeP* copyTo);
  int getTextualRepresentation(PSTRING& nameStr);
  void getNameStr(PSTRING& nameStr);
  PADSPNodeTreeNode* makeBasicTree(PADSPNodeTreeNode* parent);
  int canAddChildTypes(int* typeList, int max);
  int removeChildNode(PNodeP* removeThis);
  int makeCompleteNode();
  // new_methods_point

  modifiersT modifiers;
  declT decl;

  bool has_Pprefix;
  PprefixT Pprefix;
  vector<enumFieldT> enumField;
};
typedef PenumS* PenumT;


class PrecursiveS : public PNodeP
{
 public:
  PrecursiveS();
  PNodeP* addChild(PNodeP* newChild);
  void serializeXMLContents(int spaceDepth, PSTRING& printStr);
  void codifyContents(int spaceDepth, PSTRING& printStr);
  bool canChangeToType(int newType);
  PNodeP* convertAndCopy(int newType, PNodeP* copyTo);
  int getTextualRepresentation(PSTRING& nameStr);
  void getNameStr(PSTRING& nameStr);
  PADSPNodeTreeNode* makeBasicTree(PADSPNodeTreeNode* parent);
  int makeCompleteNode();
  // new_methods_point

  modifiersT modifiers;
  declT decl;
};
typedef PrecursiveS* PrecursiveT;


class PselectS : public PNodeP
{
 public:
  PselectS();
  PNodeP* addChild(PNodeP* newChild);
  void serializeXMLContents(int spaceDepth, PSTRING& printStr);
  void codifyContents(int spaceDepth, PSTRING& printStr);
  bool canChangeToType(int newType);
  PNodeP* convertAndCopy(int newType, PNodeP* copyTo);
  int getTextualRepresentation(PSTRING& nameStr);
  void getNameStr(PSTRING& nameStr);
  PADSPNodeTreeNode* makeBasicTree(PADSPNodeTreeNode* parent);
  int makeCompleteNode();
  // new_methods_point

  nameT name;
  typenameT typname;
  varT var;
  exprT expr;
};
typedef PselectS* PselectT;


class PstructS : public PNodeP  // pStruct
{
 public:
  PstructS();
  PNodeP* addChild(PNodeP* newChild);
  void serializeXMLContents(int spaceDepth, PSTRING& printStr);
  void codifyContents(int spaceDepth, PSTRING& printStr);
  bool canChangeToType(int newType);
  PNodeP* convertAndCopy(int newType, PNodeP* copyTo);
  int getTextualRepresentation(PSTRING& nameStr);
  void getNameStr(PSTRING& nameStr);
  PADSPNodeTreeNode* makeBasicTree(PADSPNodeTreeNode* parent);
  int canAddChildTypes(int* typeList, int max);
  int removeChildNode(PNodeP* removeThis);
  int makeCompleteNode();
  int insertIntoNameMap(PADSTypeMap* nameMap);
  int insertIntoPathMap(PADSTypeMap* pathMap, PSTRING& pathStr);
  PADSUniTreeNode* makeUnifiedTree(PADSUniTreeNode* parent, PSTRING& varStr, PADSTypeMap* typeMap); 
  // new_methods_point

  modifiersT modifiers;
  declT decl;
  vector<field_literalT> field_literal;  // pStructFieldList
  
  bool has_constraints;
  constraintsT constraints;  // pPostCondOpt
};
typedef PstructS* PstructT;


class PtypedefS : public PNodeP // pTypedef
{
 public:
  PtypedefS();
  PNodeP* addChild(PNodeP* newChild);
  void serializeXMLContents(int spaceDepth, PSTRING& printStr);
  void codifyContents(int spaceDepth, PSTRING& printStr);
  bool canChangeToType(int newType);
  PNodeP* convertAndCopy(int newType, PNodeP* copyTo);
  int getTextualRepresentation(PSTRING& nameStr);
  void getNameStr(PSTRING& nameStr);
  PADSPNodeTreeNode* makeBasicTree(PADSPNodeTreeNode* parent);
  int canAddChildTypes(int* typeList, int max);
  int removeChildNode(PNodeP* removeThis);
  int makeCompleteNode();
  PADSUniTreeNode* makeUnifiedTree(PADSUniTreeNode* parent, PSTRING& varStr, PADSTypeMap* typeMap);
  // new_methods_point

  modifiersT modifiers;
  declT decl;
  ptypeT ptype;
  
  bool has_var;
  varT var;

  bool has_constraints;
  constraintsT constraints;
};
typedef PtypedefS* PtypedefT;


class PcaseS : public PNodeP  // pBranch
{
 public:
  PcaseS();
  PNodeP* addChild(PNodeP* newChild);
  void serializeXMLContents(int spaceDepth, PSTRING& printStr);
  void codifyContents(int spaceDepth, PSTRING& printStr);
  bool canChangeToType(int newType);
  PNodeP* convertAndCopy(int newType, PNodeP* copyTo);
  int getTextualRepresentation(PSTRING& nameStr);
  PADSPNodeTreeNode* makeBasicTree(PADSPNodeTreeNode* parent);
  int makeCompleteNode();
  // new_methods_point

  exprT expr;
  fieldT field;
};
typedef PcaseS* PcaseT;

class PdefaultS : public PNodeP
{
 public:
  PdefaultS();
  PNodeP* addChild(PNodeP* newChild);
  void serializeXMLContents(int spaceDepth, PSTRING& printStr);
  void codifyContents(int spaceDepth, PSTRING& printStr);
  bool canChangeToType(int newType);
  PNodeP* convertAndCopy(int newType, PNodeP* copyTo);
  int getTextualRepresentation(PSTRING& nameStr);
  PADSPNodeTreeNode* makeBasicTree(PADSPNodeTreeNode* parent);
  int makeCompleteNode();
  // new_methods_point

  fieldT field;
};
typedef PdefaultS* PdefaultT;

class PswitchS : public PNodeP // pVariants
{
 public:
  PswitchS();
  PNodeP* addChild(PNodeP* newChild);
  void serializeXMLContents(int spaceDepth, PSTRING& printStr);
  void codifyContents(int spaceDepth, PSTRING& printStr);
  bool canChangeToType(int newType);
  PNodeP* convertAndCopy(int newType, PNodeP* copyTo);
  int getTextualRepresentation(PSTRING& nameStr);
  PADSPNodeTreeNode* makeBasicTree(PADSPNodeTreeNode* parent);
  int canAddChildTypes(int* typeList, int max);
  int removeChildNode(PNodeP* removeThis);
  int makeCompleteNode();
  // new_methods_point

  exprT expr;
  vector<PcaseT> Pcase;
  PdefaultT Pdefault;
};
typedef PswitchS* PswitchT;


class PunionS : public PNodeP 
{
 public:
  PunionS();
  PNodeP* addChild(PNodeP* newChild);
  void serializeXMLContents(int spaceDepth, PSTRING& printStr);
  void codifyContents(int spaceDepth, PSTRING& printStr);
  bool canChangeToType(int newType);
  PNodeP* convertAndCopy(int newType, PNodeP* copyTo);
  int getTextualRepresentation(PSTRING& nameStr);
  void getNameStr(PSTRING& nameStr);
  PADSPNodeTreeNode* makeBasicTree(PADSPNodeTreeNode* parent);
  int canAddChildTypes(int* typeList, int max);
  int removeChildNode(PNodeP* removeThis);
  int makeCompleteNode();
  int insertIntoNameMap(PADSTypeMap* nameMap);
  int insertIntoPathMap(PADSTypeMap* pathMap, PSTRING& pathStr);
  PADSUniTreeNode* makeUnifiedTree(PADSUniTreeNode* parent, PSTRING& varStr, PADSTypeMap* typeMap);
  // new_methods_point

  modifiersT modifiers;
  declT decl;
  
  enum switch_fieldliteralEnum
    {
      UNDEFINED = 0x0,
      SWITCH = 0x1,
      FIELDLITERAL = 0x2
    };
  enum switch_fieldliteralEnum which;  // pVariants :: Pswitch | pStructFieldList
  PswitchT Pswitch;
  vector<field_literalT> field_literal;  // pStructFieldList

  bool has_constraints;
  constraintsT constraints;  // pPostCondOpt
};
typedef PunionS* PunionT;


typedef union
{
  PalternatesT Palternates;
  ParrayT Parray;
  PcharclassT Pcharclass;
  PenumT Penum;
  functionT function;
  PrecursiveT Precursive;
  PselectT Pselect;
  PstructT Pstruct;
  PtypedefT Ptypedef;
  PunionT Punion;
} PADStypeU;

class PADStypeS
{
 public:
  enum PADStypeEnum
    {
      UNDEFINED = 0x0,
      PALTERNATES = 0x1,
      PARRAY = 0x2,
      PCHARCLASS = 0x4,
      PENUM = 0x8,
      FUNCTION = 0x10,
      PRECURSIVE = 0x20,
      PSELECT = 0x40,
      PSTRUCT = 0x80,
      PTYPEDEF = 0x100,
      PUNION = 0x200
    };
  enum PADStypeEnum which;
  PADStypeU PADStype;
};
typedef PADStypeS* PADStypeT;

class Pads : public PNodeP
{
 public:
  Pads();
  PNodeP* addChild(PNodeP* newChild);
  void serializeXMLContents(int spaceDepth, PSTRING& printStr);
  void codifyRepresentation(int spaceDepth, PSTRING& printStr);
  bool canChangeToType(int newType);
  PNodeP* convertAndCopy(int newType, PNodeP* copyTo);
  PADSPNodeTreeNode* makeBasicTree(PADSPNodeTreeNode* parent);
  int canAddChildTypes(int* typeList, int max);
  int removeChildNode(PNodeP* removeThis);
  int insertIntoNameMap(PADSTypeMap* nameMap);
  int insertIntoPathMap(PADSTypeMap* pathMap, PSTRING& pathStr);
  int insertIntoTypeMap(PADSTypeMap* nameMap);
  PADSUniTreeNode* makeUnifiedTree(PADSUniTreeNode* parent, PSTRING& varStr, PADSTypeMap* typeMap);
  // new_methods_point

  vector<PADStypeT> PADStype;
};

#endif
