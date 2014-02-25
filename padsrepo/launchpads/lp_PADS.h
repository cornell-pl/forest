/* **************************************
 * PADS GUI project (preliminary tests)
 * Internal representation of PADS language (declaration)
 * *********************************** */

#include "lp_includes.h"

#ifndef _PADS_CLASS_H_INCLUDED
#define _PADS_CLASS_H_INCLUDED

#include "wx/wx.h"
#include "wx/treectrl.h"
#include "wx/string.h"
#include "wx/arrstr.h"
#include "wx/dynarray.h"
#include "wx/regex.h"
#include "wx/textctrl.h"

// 128 characters should be enough...
#define MAX_NAME_SIZE 128
#define PRINT_BUFFER_SIZE 1<<10

//#define NUM_TERMINALS (pEndTerminals    - pStartTerminals    - 1)
//#define NUM_UDEF      (pEndNonTerminals - pStartNonTerminals - 1)

#define pSigned      true
#define pUnsigned    false
#define pTerminal    true
#define pNonterminal false

#define PADS_PACKAGE_PATH_PADSC "scripts/padsc"
#define PADS_PACKAGE_PATH_INC "padsc/include/"
#define PADS_PACKAGE_PATH_ASTINC "include/ast/"
#define PADS_PACKAGE_PATH_ASTLIB "lib/"

#define P_NUM_LIBS_FOR_CC 2


/*
//debug macros
#define DB_P(a, ...)   fprintf(stderr, a, ## __VA_ARGS__ ) // from RHEL documentation, 6.15
#define DBS // start debug
#define DBE // end debug
//post debug macros
//#define DB_P(a, ...)  // a, ## __VA_ARGS__
//#define DB_OFF 1
//#define DBS goto post_debug;
//#define DBE post_debug:
*/

#include "lp_PADS_constants.h"

#define P_FIND_RECORD_NONTERMINAL_NAMES "P(struct|union|array)[[:space:]]+([[:alnum:]_]+)[[:space:]]*"

static const char* PADS_libnames[] =
  {
    "libpads.a",
    "libast.a"
  };

// this is a bit crude, but it'll save time at startup
static const wxChar *PADS_name_chars[] = 
  {
    "_", 
    "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", 
    "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", 
    "U", "V", "W", "X", "Y", "Z", 
    "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", 
    "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", 
    "u", "v", "w", "x", "y", "z", 
    "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
    " "
  };

/* ******************************************* */
class PElement : public wxTreeItemData
{
 public:
  /* PElement(int type, wxString& name_in, 
	   int enct, bool sign, int termt, int bytesize, bool istypedef, 
	   bool args, int num_args, wxString& arg_list, 
	   bool clause, int clauset, wxString& clause_name, 
	   long PFlags);
  */
  PElement(int type,
	   wxString &name_in,
	   long flags,
	   int enct = pnumDefault, 
	   bool sign = true, 
	   int termt = pendDefault, 
	   int bytesize = p32, 
	   bool istypedef = false,
	   bool hasstop = false,
	   const wxString &stopexpr = _T(""),
	   bool args = false,
	   const wxString &arg_list = _T(""),
	   bool clause = false,
	   int clauset = pUndefined,
	   const wxString &clause_name = _T("")
	   );
  ~PElement();

  bool ChangeName(wxString &name_in);

  bool ChangeParams(wxString& name_in, 
		    int flags = 0,
		    int enct = -1, 
		    bool sign = true, 
		    int termt = -1, 
		    int bytesize = -1, 
		    bool istypedef = false);
  bool ChangeParams(int flags = 0,
		    int enct = -1, 
		    bool sign = true, 
		    int termt = -1, 
		    int bytesize = -1, 
		    bool istypedef = false);

  bool ChangeType(int new_type);

  bool AddArg(wxString& arg_in);
  bool RemoveArg(int index, wxString& arg_out);
  int  GetNumArgs(void);
  bool HasArgs(void);
  bool GetArgAtIndex(int i, wxString& string);
  void ClearArgs(void);

  bool AddClause(wxString& clause_in);
  bool RemoveClause(wxString& clause_out);
  bool HasClause(void);
  bool GetClause(wxString& string);
  bool ReplaceClause(wxString& new_clause);
  void ClearClause(void);

  bool SetStopExpr(wxString& stop_e);
  bool GetStopExpr(wxString& stop_e);
  bool HasStopExpr(void);
  void ClearStopExpr(void);

  int  GetType(void);
  void GetName(wxString& name);
  int  GetEnctype(void);
  int  GetTermtype(void);
  bool GetSign(void);
  int  GetByteSize(void);
  bool GetIsTypedef(void);

  long GetFlags(void);
  void SetFlags(long flags);

  long GetLineNumber(void);
  void SetLineNumber(long num);

  static bool IsPTerminal(int i);
  static bool IsPPrintableTerminal(int i);
  static bool IsPNonTerminal(int i);
  static bool IsPNumType(int i);
  static void GetPTypeAttributes(int type, 
				 bool &sign,
				 bool &enctype,
				 bool &bytes, 
				 bool &termtype);

  bool IsPTerminal();
  bool IsPPrintableTerminal();
  bool IsPNonTerminal();
  bool IsPNumType();
  void GetPTypeAttributes(bool &sign,
			  bool &enctype,
			  bool &bytes, 
			  bool &termtype);

 protected:
  bool PCheckName(wxString& name);


  int  PType;
  bool PTerminal;

  int  PEnctype;
  bool PSigned;
  int  PBytesize;
  int  PTermType;

  bool PIsTypedef;

  wxString PName;
  int PNameLen;

  bool PHasStopExpr;
  wxString PStopExpr;

  bool PHasArgs;
  int PNumArgs;
  wxArrayString* PArgList;

  bool PHasClause;
  wxString PClauseName;

  long PFlags;

  wxTextPos PLineNumber;

};

#endif
