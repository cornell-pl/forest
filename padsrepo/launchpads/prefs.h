/* **************************************
 * PADS GUI project (preliminary tests)
 * Prefs dialog class header
 * *********************************** */

#include "lp_includes.h"

#ifndef _LP_PREFS_INCLUDED
#define _LP_PREFS_INCLUDED

class PrefsDialog : public wxPropertySheetDialog
{
DECLARE_CLASS(PrefsDiag)
  /* **************************** */
 public:
  /* **************************** */
  PrefsDialog(wxWindow* parent, wxWindowID id,
	      const wxString& title, 
	      LaunchPADS* par);
  ~PrefsDialog();

  void OnSaveLayoutNow(wxCommandEvent &event);

  void OnChangeTopNumLines(wxSpinEvent &event);
  void OnChangeTopFont(wxCommandEvent &event);

  void OnChangeTermColour(wxCommandEvent &event);
  void OnChangeNontermColour(wxCommandEvent &event);
  void OnChangeLitColour(wxCommandEvent &event);
  void OnChangeErrorColour(wxCommandEvent &event);
  void OnChangeOtherColour(wxCommandEvent &event);

  void OnChangeBottomFont(wxCommandEvent &event);
  
  /* **************************** */
 protected:
  /* **************************** */
  void pdCreateGeneralPrefsPage(wxWindow* panel);
  void pdCreateTopPrefsPage(wxWindow* panel);
  void pdCreateMidPrefsPage(wxWindow* panel);
  void pdCreateLeftPrefsPage(wxWindow* panel);
  void pdCreateBottomPrefsPage(wxWindow* panel);
  void pdCreateToolsPrefsPage(wxWindow* panel);

  LaunchPADS* parent;

  bool saveLayoutOnExit_new;
  bool colourModeOn_new;
  bool midGridColNumberLabels_new;
  int  topNumLinesOnImport_new;
  bool midTermErrorMBoxOn_new;
  bool midUDefErrorMBoxOn_new;
  bool rightBuildTreeMBoxOn_new;
  bool leftTreeBackgroundColourOn_new;
  bool leftControlsWarnings_new;
  bool leftAttributeChangeWarnings_new;
  bool midGridWriteTypeOnNext_new;
  bool leftTreeTextColourOn_new;
  bool xmlMessagesOnSave_new;
  wxString PADSCpath_new;
  wxString PADSCinc_new;
  wxString ASTinc_new;
  wxString CCpath_new;
  wxString CCargs_new;
  bool leftControlSafeMode_new;
  wxString PADXPath_new;

  wxPanel *pdGenPanel;
  wxPanel *pdTopPanel;
  wxPanel *pdMidPanel;
  wxPanel *pdLeftPanel;
  wxPanel *pdBottomPanel;
  wxPanel *pdToolPanel;

  wxBoxSizer        *pdGeneralMainSizer;
  wxGridBagSizer    *pdGeneralButtonSizer;
  wxCheckBox        *pdGeneralSaveLayoutOnExit;
  wxButton          *pdGeneralSaveLayoutNow;
  wxCheckBox        *pdGeneralNoColourMode;
  wxCheckBox        *pdGeneralXmlMessagesOnSave;
  wxBoxSizer        *pdTopOverSizer;
  wxGridBagSizer    *pdTopSizer;
  wxStaticBoxSizer  *pdTopSpinButtonSizer;
  wxTextCtrl        *pdTopNumLines;
  wxSpinButton      *pdTopSetNumLines;
  wxButton          *pdTopSetFont;

  wxBoxSizer        *pdMidOverSizer;
  wxGridBagSizer    *pdMidSizer;
  wxRadioBox        *pdMidColumnLabels;
  wxCheckBox        *pdMidTermActiveWarnings;
  wxCheckBox        *pdMidNontermActiveWarnings;
  wxCheckBox        *pdMidBuildTreeWarning;
  wxCheckBox        *pdMidGridWriteTypeOnNext;

  wxBoxSizer        *pdLeftOverSizer;
  wxGridBagSizer    *pdLeftSizer;
  wxCheckBox        *pdLeftActiveWarnings;
//wxCheckBox        *pdLeftTreeBGColour;
  wxRadioBox        *pdLeftTreeColour;
  wxCheckBox        *pdLeftControlSafeMode;

  wxBoxSizer        *pdBottomOverSizer;
  wxGridBagSizer    *pdBottomSizer;
  wxButton          *pdBottomSetTermColour;
  wxButton          *pdBottomSetNonTermColour;
  wxButton          *pdBottomSetLitColour;
  wxButton          *pdBottomSetErrorColour;
  wxButton          *pdBottomSetOtherColour;
  wxButton          *pdBottomSetFont;

  wxBoxSizer        *pdToolsOverSizer;
  wxFlexGridSizer   *pdToolsSizer;
  wxStaticText      *pdToolsPADSCpathText;
  wxBoxSizer        *pdToolsPADSCpathSizer;
  wxTextCtrl        *pdToolsPADSCpath;
  wxStaticText      *pdToolsPADSCincludeText;
  wxBoxSizer        *pdToolsPADSCincludeSizer;
  wxTextCtrl        *pdToolsPADSCinc;
  wxStaticText      *pdToolsASTincludeText;
  wxBoxSizer        *pdToolsASTincludeSizer;
  wxTextCtrl        *pdToolsASTinc;
  wxStaticText      *pdToolsCCpathText;
  wxBoxSizer        *pdToolsCCpathSizer;
  wxTextCtrl        *pdToolsCCpath;
  wxStaticText      *pdToolsCCargsText;
  wxBoxSizer        *pdToolsCCargsSizer;
  wxTextCtrl        *pdToolsCCargs;
  wxStaticText      *pdToolsPADXPathText;
  wxBoxSizer        *pdToolsPADXPathSizer;
  wxTextCtrl        *pdToolsPADXPath;


DECLARE_EVENT_TABLE()
};

#endif
