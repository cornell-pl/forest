/* **************************************
 * PADS GUI project (preliminary tests)
 * Tool template auto-generation module
 * *********************************** */

#include "lp_includes.h"

#ifndef _LP_CODEGEN_H_INCLUDED
#define _LP_CODEGEN_H_INCLUDED

#include "wx/wx.h"

#define WIZ_NUM_STAT_LINES 4

static wxChar *lpGeneric_DefineStr = "1";

enum
  {
    CG_NONE = 0,
    CG_ACC,
    CG_FMT,
    CG_XML,
  };

enum 
  {
    ACC_DATE_IN_FMT = 0, 
    ACC_DATE_OUT_FMT,
    ACC_DEF_INPUT_FILE,
    ACC_EXTRA_BAD_READ_CODE,
    ACC_EXTRA_BEGIN_CODE,
    ACC_EXTRA_DECLS,
    ACC_EXTRA_DONE_CODE,
    ACC_EXTRA_GOOD_READ_CODE,
    ACC_EXTRA_HDR_READ_ARGS,
    ACC_EXTRA_READ_ARGS,
    ACC_IN_TIME_ZONE,
    ACC_IO_DISC_MK,
    ACC_MAX_RECS,
    ACC_OUT_TIME_ZONE,
    ACC_PADS_HDR_TY,
    ACC_PADS_TY,
    ACC_READ_MASK,
    ACC_TIME_IN_FMT,
    ACC_TIME_OUT_FMT,
    ACC_TIMESTAMP_IN_FMT,
    ACC_TIMESTAMP_OUT_FMT,
    ACC_WSPACE_OK,
    ACC_NUM_OPTS,
  };

static wxChar *lpAccum_Labels[] = 
  {
    "DATE_IN_FMT", 
    "DATE_OUT_FMT",
    "DEF_INPUT_FILE",
    "EXTRA_BAD_READ_CODE",
    "EXTRA_BEGIN_CODE",
    "EXTRA_DECLS",
    "EXTRA_DONE_CODE",
    "EXTRA_GOOD_READ_CODE",
    "EXTRA_HEADER_READ_ARGS",
    "EXTRA_READ_ARGS",
    "IN_TIME_ZONE",
    "IO_DISC_MK",
    "MAX_RECS",
    "OUT_TIME_ZONE",
    "PADS_HDR_TY(suf)",
    "PADS_TY(suf)",
    "READ_MASK",
    "TIME_IN_FMT",
    "TIME_OUT_FMT",
    "TIMESTAMP_IN_FMT",
    "TIMESTAMP_OUT_FMT",
    "WSPACE_OK"
  };

static wxChar *lpAccum_DefHeaderString = "summary_header_t ## suf";

static wxChar *lpAccum_DefaultValues[] = 
   {
    "",   // ACC_DATE_IN_FMT 
    "",   // ACC_DATE_OUT_FMT
    "",   // ACC_DEF_INPUT_FILE
    "",   // ACC_EXTRA_BAD_READ_CODE
    "",   // ACC_EXTRA_BEGIN_CODE
    "",   // ACC_EXTRA_DECLS
    "",   // ACC_EXTRA_DONE_CODE
    "",   // ACC_EXTRA_GOOD_READ_CODE
    "",   // ACC_EXTRA_HDR_READ_ARGS
    "",   // ACC_EXTRA_READ_ARGS
    "",   // ACC_IN_TIME_ZONE
    "P_nlrec_make(0)",   // ACC_IO_DISC_MK
    "",   // ACC_MAX_RECS
    "",   // ACC_OUT_TIME_ZONE
    "",   // ACC_PADS_HDR_TY(suf)
    " ## suf",   // ACC_PADS_TY(suf)
    "",   // ACC_READ_MASK
    "",   // ACC_TIME_IN_FMT
    "",   // ACC_TIME_OUT_FMT
    "",   // ACC_TIMESTAMP_IN_FMT
    "",   // ACC_TIMESTAMP_OUT_FMT
    ""    // ACC_WSPACE_OK
   };

enum
  {
    FMT_DATE_IN_FMT = 0,
    FMT_DATE_OUT_FMT,
    FMT_DEF_INPUT_FILE,
    FMT_DEF_OUTPUT_FILE,
    FMT_DELIMS,
    FMT_EXTRA_BAD_READ_CODE,
    FMT_EXTRA_GOOD_READ_CODE,
    FMT_EXTRA_HDR_READ_ARGS,
    FMT_EXTRA_READ_ARGS,
    FMT_FMT_DELIMS,
    FMT_FMT_ERROR_CASES,
    FMT_FMT_MASK,
    FMT_IN_TIME_ZONE,
    FMT_IO_DISC_DESCR,
    FMT_IO_DISC_MK,
    FMT_MAX_RECS,
    FMT_OUT_TIME_ZONE,
    FMT_PADS_HDR_TY,
    FMT_PADS_TY,
    FMT_PRE_LIT_LWS,
    FMT_READ_MASK,
    FMT_TIME_IN_FMT,
    FMT_TIME_OUT_FMT,
    FMT_TIMESTAMP_IN_FMT,
    FMT_TIMESTAMP_OUT_FMT,
    FMT_WSPACE_OK, 
    FMT_NUM_OPTS,
  };

static wxChar *lpFmt_Labels[] = 
  {
    "DATE_IN_FMT",
    "DATE_OUT_FMT",
    "DEF_INPUT_FILE",
    "DEF_OUTPUT_FILE",
    "DELIMS",
    "EXTRA_BAD_READ_CODE",
    "EXTRA_GOOD_READ_CODE",
    "EXTRA_HDR_READ_ARGS",
    "EXTRA_READ_ARGS",
    "DELIMS",
    "ERROR_CASES",
    "MASK",
    "IN_TIME_ZONE",
    "IO_DISC_DESCR",
    "IO_DISC_MK",
    "MAX_RECS",
    "OUT_TIME_ZONE",
    "PADS_HDR_TY(suf)",
    "PADS_TY(suf)",
    "PRE_LIT_LWS",
    "READ_MASK",
    "TIME_IN_FMT",
    "TIME_OUT_FMT",
    "TIMESTAMP_IN_FMT",
    "TIMESTAMP_OUT_FMT",
    "WSPACE_OK"
  };

static wxChar *lpFmt_DefHeaderString = "summary_header_t ## suf";

static wxChar *lpFmt_DefaultValues[] = 
   {
     "",  // FMT_DATE_IN_FMT
     "",  // FMT_DATE_OUT_FMT
     "",  // FMT_DEF_INPUT_FILE
     "",  // FMT_DEF_OUTPUT_FILE
     "\\n",  // FMT_DELIMS
     "",  // FMT_EXTRA_BAD_READ_CODE
     "",  // FMT_EXTRA_GOOD_READ_CODE
     "",  // FMT_EXTRA_HDR_READ_ARGS
     "",  // FMT_EXTRA_READ_ARGS
     "",  // FMT_FMT_DELIMS
     "",  // FMT_FMT_ERROR_CASES
     "",  // FMT_FMT_MASK
     "",  // FMT_IN_TIME_ZONE
     "",  // FMT_IO_DISC_DESCR
     "P_nlrec_make(0)",  // FMT_IO_DISC_MK
     "",  // FMT_MAX_RECS
     "",  // FMT_OUT_TIME_ZONE
     "",  // FMT_PADS_HDR_TY
     "entry_t ## suf",  // FMT_PADS_TY
     "",  // FMT_PRE_LIT_LWS
     "",  // FMT_READ_MASK
     "",  // FMT_TIME_IN_FMT
     "",  // FMT_TIME_OUT_FMT
     "",  // FMT_TIMESTAMP_IN_FMT
     "",  // FMT_TIMESTAMP_OUT_FMT
     ""   // FMT_WSPACE_OK 
   };

enum
  {
    XML_DATE_IN_FMT = 0,
    XML_DATE_OUT_FMT,
    XML_DEF_INPUT_FILE,
    XML_DEF_OUTPUT_FILE,
    XML_EXTRA_HDR_READ_ARGS,
    XML_EXTRA_READ_ARGS,
    XML_IN_TIME_ZONE,
    XML_IO_DISC_MK,
    XML_MAX_RECS,
    XML_OUT_TIME_ZONE,
    XML_PADS_HDR_TY,
    XML_PADS_TY,
    XML_POST_SKIP_BYTES,
    XML_PRE_SKIP_BYTES,
    XML_READ_MASK,
    XML_TIME_IN_FMT,
    XML_TIME_OUT_FMT,
    XML_TIMESTAMP_IN_FMT,
    XML_TIMESTAMP_OUT_FMT,
    XML_WSPACE_OK,
    XML_NUM_OPTS,
  };

static wxChar *lpXml_Labels[] = 
  {
    "DATE_IN_FMT",
    "DATE_OUT_FMT",
    "DEF_INPUT_FILE",
    "DEF_OUTPUT_FILE",
    "EXTRA_HDR_READ_ARGS",
    "EXTRA_READ_ARGS",
    "IN_TIME_ZONE",
    "IO_DISC_MK",
    "MAX_RECS",
    "OUT_TIME_ZONE",
    "PADS_HDR_TY(suf)",
    "PADS_TY(suf)",
    "POST_SKIP_BYTES",
    "PRE_SKIP_BYTES",
    "READ_MASK",
    "TIME_IN_FMT",
    "TIME_OUT_FMT",
    "TIMESTAMP_IN_FMT",
    "TIMESTAMP_OUT_FMT",
    "WSPACE_OK"
  };

static wxChar *lpXml_DefHeaderString = "summary_header_t ## suf";

static wxChar *lpXml_DefaultValues[] = 
   {
    "",  // XML_DATE_IN_FMT
    "",  // XML_DATE_OUT_FMT
    "",  // XML_DEF_INPUT_FILE
    "",  // XML_DEF_OUTPUT_FILE
    "",  // XML_EXTRA_HDR_READ_ARGS
    "",  // XML_EXTRA_READ_ARGS
    "",  // XML_IN_TIME_ZONE
    "P_nlrec_make(0)",  // XML_IO_DISC_MK
    "",  // XML_MAX_RECS
    "",  // XML_OUT_TIME_ZONE
    "",  // XML_PADS_HDR_TY
    "entry_t ## suf",  // XML_PADS_TY
    "",  // XML_POST_SKIP_BYTES
    "",  // XML_PRE_SKIP_BYTES
    "",  // XML_READ_MASK
    "",  // XML_TIME_IN_FMT
    "",  // XML_TIME_OUT_FMT
    "",  // XML_TIMESTAMP_IN_FMT
    "",  // XML_TIMESTAMP_OUT_FMT
    ""   // XML_WSPACE_OK
   };

enum
  {
    GMT_UTC,
    HST,
    YST,
    YDT,
    PST,
    PDT,
    MST,
    MDT,
    CST,
    CDT,
    EST,
    EDT,
    AST,
    ADT,
    NST,
    BST,
    WET,
    CET,
    MET,
    EET,
    IST,
    ITD,
    IST2,
    HKT,
    KST,
    SST,
    JST,
    AWST,
    WST,
    ACST,
    CST2,
    AEST,
    EST2,
    NZST, 
    NUM_TZONES
  };

static wxChar *lpTimeZone_Labels[] = 
  {
    "GMT/UTC",
    "HST",
    "YST",
    "YDT",
    "PST",
    "PDT",
    "MST",
    "MDT",
    "CST",
    "CDT",
    "EST",
    "EDT",
    "AST",
    "ADT",
    "NST",
    "BST",
    "WET",
    "CET",
    "MET",
    "EET",
    "IST",
    "ITD",
    "IST",
    "HKT",
    "KST",
    "SST",
    "JST",
    "AWST",
    "WST",
    "ACST",
    "CST",
    "AEST",
    "EST",
    "NZST"
  };
/*
static int *lpTimeZone_Offsets[] = 
  {
    0,// UCT
    -1000,// HST
    -0900,// YST
    -0800,// YDT
    -0800,// PST
    -0700,// PDT
    -0700,// MST
    -0600,// MDT
    -0600,// CST
    -0500,// CDT
    -0500,// EST
    -0400,// EDT
    -0400,// AST
    -0300,// ADT
    -0330,// NST
    +0100,// BST
    ,// WET
    ,// CET
    ,// MET
    ,// EET
    ,// IST
    ,// ITD
    ,// IST
    ,// HKT
    ,// KST
    ,// SST
    ,// JST
    ,// AWST
    ,// WST
    ,// ACST
    ,// CST
    ,// AEST
    ,// EST
    ,// NZST
  };
*/
/* ************************** */

static wxChar *lpCode_Test = 
  {
"\
/* PADS gui - autogenerated code - hello world */ \n\
#include <stdio.h>\n\
\n\
int main(int argc, char** argv)\n\
{\n\
   printf(\"Hello World!\\n\");\n\
   return 0;\n\
}\n" 
  };

static wxChar *lpCode_Fmt = 
  {
"\
/* PADS gui - autogenerated code - acc */ \n\
%s\n\
#include \"%s\"\n\
#include \"template/read_format.h\"\n\
\n"
  };

static wxChar *lpCode_Xml = 
  {
"\
/* PADS gui - autogenerated code - fmt */ \n\
%s\n\
#include \"%s\"\n\
#include \"template/read_orig_write_xml.h\"\n\
\n"
  };

static wxChar *lpCode_Accum = 
  {
"\
/* PADS gui - autogenerated code - xml */ \n\
%s\n\
#include \"%s\"\n\
#include \"template/accum_report.h\"\n\
\n"
  };

//"
static const char *accWizHelpString = 
    "Please enter the parameters for the PADS accumulator template.";

static const char *fmtWizHelpString =  
    "Please enter the parameters for the PADS formatter template.";

static const char *xmlWizHelpString =  
    "Please enter the parameters for the PADS XML converter template.";

/* **Wizard Classes for the tool components** */
/* ************************************ */
class AccWizard : public wxWizard
{
DECLARE_CLASS(AccWizard)

 public:
  AccWizard(wxWindow* parent, wxBitmap bitmap, 
	    wxArrayString &strs, 
	    wxString &incPath, wxString &incName, 
	    wxArrayString& nonterminalElements);
  ~AccWizard();

  void wGetStrings(wxArrayString &strs, wxString &incPath, wxString &incName);
  bool wRunWizard(void);

  void OnExpertModeCheck(wxCommandEvent &event);
  void OnNumRecsSpin(wxSpinEvent &event);
  void OnSetIncludePath(wxCommandEvent &event);
  void OnResetToDefault(wxCommandEvent &event);

 protected:
  wxBoxSizer       *accSizer;
  wxGridBagSizer   *accGridSizer;
  wxGridBagSizer   *accGridSizer_2;
  wxStaticText     *accInfo;
  wxGridBagSizer   *accSetControlsSizer;
  wxButton         *accSetDefaults;
  wxCheckBox       *accExpertMode;
  bool              accExpertModeOn;

  wxStaticText     *accLabels[ACC_NUM_OPTS];
  wxTextCtrl       *accInput[ACC_NUM_OPTS];

  wxStaticText     *accNumRecsLabel;
  wxBoxSizer       *accNumRecsSizer;
  //wxTextValidator  *accNumRecsValidator;
  wxTextCtrl       *accNumRecsText;
  wxSpinButton     *accNumRecs;
  wxCheckBox       *accWsOk;
  wxCheckBox       *accUseHeader;
  wxStaticText     *accTZlabel;
  wxChoice         *accTZ;
  wxStaticText     *accNTermLabel;
  wxComboBox       *accNTerm;
  wxStaticLine     *accStatLines[WIZ_NUM_STAT_LINES];

  wxStaticBoxSizer *accIncludeFileSizer;
  wxButton         *accSelectIncludeFile;
  wxStaticText     *accIncludeFilePath;
  wxString          includePath;
  wxString          includeName;

  wxChoice         *accHelpChoice;
  wxStaticBoxSizer *accHelpBox;
  wxStaticText     *accHelpText;

  long numRecs;

  wxArrayString nontElems;

  wxWizardPageSimple *accPageFirst;
  wxWizardPageSimple *accPageLast;

DECLARE_EVENT_TABLE()
};


/* ************************************ */
class FmtWizard : public wxWizard
{
DECLARE_CLASS(FmtWizard)

 public:
  FmtWizard(wxWindow* parent, wxBitmap bitmap, 
	    wxArrayString &strs, 
	    wxString &incPath, wxString &incName,
	    wxArrayString& nonterminalElements);
  ~FmtWizard();

  void wGetStrings(wxArrayString &strs, wxString &incPath, wxString &incName);
  bool wRunWizard(void);

  void OnExpertModeCheck(wxCommandEvent &event);
  void OnNumRecsSpin(wxSpinEvent &event);
  void OnSetIncludePath(wxCommandEvent &event);
  void OnResetToDefault(wxCommandEvent &event);

 protected:
  wxBoxSizer       *fmtSizer;
  wxGridBagSizer   *fmtGridSizer;
  wxGridBagSizer   *fmtGridSizer_2;
  wxStaticText     *fmtInfo;
  wxGridBagSizer   *fmtSetControlsSizer;
  wxButton         *fmtSetDefaults;
  wxCheckBox       *fmtExpertMode;
  bool              fmtExpertModeOn;

  wxStaticText     *fmtLabels[FMT_NUM_OPTS];
  wxTextCtrl       *fmtInput[FMT_NUM_OPTS];

  wxStaticText     *fmtDelimLabel;
  wxTextCtrl       *fmtDelim;
  wxStaticText     *fmtNumRecsLabel;
  wxBoxSizer       *fmtNumRecsSizer;
  //wxTextValidator  *fmtNumRecsValidator;
  wxTextCtrl       *fmtNumRecsText;
  wxSpinButton     *fmtNumRecs;
  wxCheckBox       *fmtWsOk;
  wxCheckBox       *fmtUseHeader;
  wxStaticText     *fmtTZlabel;
  wxChoice         *fmtTZ;
  wxStaticText     *fmtNTermLabel;
  wxComboBox       *fmtNTerm;
  wxStaticLine     *fmtStatLines[WIZ_NUM_STAT_LINES+1];

  wxStaticBoxSizer *fmtIncludeFileSizer;
  wxButton         *fmtSelectIncludeFile;
  wxStaticText     *fmtIncludeFilePath;
  wxString          includePath;
  wxString          includeName;

  wxChoice         *fmtHelpChoice;
  wxStaticBoxSizer *fmtHelpBox;
  wxStaticText     *fmtHelpText;

  long numRecs;

  wxArrayString nontElems;

  wxWizardPageSimple *fmtPageFirst;
  wxWizardPageSimple *fmtPageLast;

DECLARE_EVENT_TABLE()
};

/* ************************************ */
// copy/past/find-replace "xml" from here down
// do the same for a ToolProcess function in 
// pads_gui.cpp and change the filetypes to 
// whatever is appropriate
/* ************************************ */
class XmlWizard : public wxWizard
{
DECLARE_CLASS(XmlWizard)

 public:
  XmlWizard(wxWindow* parent, wxBitmap bitmap, 
	    wxArrayString &strs, 
	    wxString &incPath, wxString &incName, 
	    wxArrayString& nonterminalElements);
  ~XmlWizard();

  void wGetStrings(wxArrayString &strs, wxString &incPath, wxString &incName);
  bool wRunWizard(void);

  void OnExpertModeCheck(wxCommandEvent &event);
  void OnNumRecsSpin(wxSpinEvent &event);
  void OnSetIncludePath(wxCommandEvent &event);
  void OnResetToDefault(wxCommandEvent &event);

 protected:
  wxBoxSizer       *xmlSizer;
  wxGridBagSizer   *xmlGridSizer;
  wxGridBagSizer   *xmlGridSizer_2;
  wxStaticText     *xmlInfo;
  wxGridBagSizer   *xmlSetControlsSizer;
  wxButton         *xmlSetDefaults;
  wxCheckBox       *xmlExpertMode;
  bool              xmlExpertModeOn;

  wxStaticText     *xmlLabels[XML_NUM_OPTS];
  wxTextCtrl       *xmlInput[XML_NUM_OPTS];

  wxStaticText     *xmlNumRecsLabel;
  wxBoxSizer       *xmlNumRecsSizer;
  //wxTextValidator  *xmlNumRecsValidator;
  wxTextCtrl       *xmlNumRecsText;
  wxSpinButton     *xmlNumRecs;
  wxCheckBox       *xmlWsOk;
  wxCheckBox       *xmlUseHeader;
  wxStaticText     *xmlTZlabel;
  wxChoice         *xmlTZ;
  wxStaticText     *xmlNTermLabel;
  wxComboBox       *xmlNTerm;
  wxStaticLine     *xmlStatLines[WIZ_NUM_STAT_LINES];

  wxStaticBoxSizer *xmlIncludeFileSizer;
  wxButton         *xmlSelectIncludeFile;
  wxStaticText     *xmlIncludeFilePath;
  wxString          includePath;
  wxString          includeName;

  wxChoice         *xmlHelpChoice;
  wxStaticBoxSizer *xmlHelpBox;
  wxStaticText     *xmlHelpText;

  long numRecs;

  wxArrayString nontElems;

  wxWizardPageSimple *xmlPageFirst;
  wxWizardPageSimple *xmlPageLast;

DECLARE_EVENT_TABLE()
};

/* ************************************ */
#endif
