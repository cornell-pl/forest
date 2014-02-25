/* **************************************
 * PADS GUI project (preliminary tests)
 * Preferences Dialog code
 * *********************************** */
#include "lp_includes.h"

/* **************************************
 * Functions:
 *  --class constructor/destructor
 *  PrefsDialog
 *  ~PrefsDialog
 *
 *  --init
 *  pdCreateGeneralPrefsPage
 *  pdCreateTopPrefsPage
 *  pdCreateMidPrefsPage
 *  pdCreateLeftPrefsPage
 *  pdCreateBottomPrefsPage
 *  pdCreateToolsPrefsPage
 *
 *  --event handlers
 *  OnSaveLayoutNow
 *  OnChangeTopNumLines
 *  OnChangeTopFont
 *  OnChangeBottomFont
 *  OnChangeTermColour
 *  OnChangeNontermColour
 *  OnChangeLitColour
 *  OnChangeErrorColour
 *  OnChangeOtherColour
 * *********************************** */

// PrefsDialog class: creates separate dialog to set interface options


IMPLEMENT_CLASS(PrefsDialog, wxPropertySheetDialog)

BEGIN_EVENT_TABLE(PrefsDialog, wxPropertySheetDialog)
  EVT_BUTTON(PREFS_SAVE_LAYOUT_NOW, PrefsDialog::OnSaveLayoutNow)
  EVT_SPIN(PREFS_TOP_SET_NUM_LINES, PrefsDialog::OnChangeTopNumLines)
  EVT_BUTTON(PREFS_TOP_SET_FONT, PrefsDialog::OnChangeTopFont)
  EVT_BUTTON(PREFS_BOTTOM_COLOUR_TERM, PrefsDialog::OnChangeTermColour)
  EVT_BUTTON(PREFS_BOTTOM_COLOUR_NONTERM, PrefsDialog::OnChangeNontermColour)
  EVT_BUTTON(PREFS_BOTTOM_COLOUR_LIT, PrefsDialog::OnChangeLitColour)
  EVT_BUTTON(PREFS_BOTTOM_COLOUR_ERROR, PrefsDialog::OnChangeErrorColour)
  EVT_BUTTON(PREFS_BOTTOM_COLOUR_OTHER, PrefsDialog::OnChangeOtherColour)
  EVT_BUTTON(PREFS_BOTTOM_SET_FONT, PrefsDialog::OnChangeBottomFont)
  // add function for wxID_HELP - launch the default browser and send the user to the manual
END_EVENT_TABLE()


PrefsDialog::PrefsDialog(wxWindow* par_win, wxWindowID id,
			 const wxString& title, 
			 LaunchPADS* par)
  // taken from the wxPropertySheet documentation
  : wxPropertySheetDialog(par_win, id, title, 
			  wxDefaultPosition,
			  wxDefaultSize,
			  wxDEFAULT_DIALOG_STYLE

#ifdef LP_OSX_METAL
			  | wxDIALOG_EX_METAL
#endif
#ifndef __WXWINCE__
			  | wxRESIZE_BORDER
#endif
			  )
{
  
  parent = par;

  parent->GetSettings(
		      saveLayoutOnExit_new,
		      colourModeOn_new,
		      midGridColNumberLabels_new,
		      topNumLinesOnImport_new,
		      midUDefErrorMBoxOn_new,
		      midTermErrorMBoxOn_new,
		      rightBuildTreeMBoxOn_new,
		      leftTreeBackgroundColourOn_new,
		      leftControlsWarnings_new,
		      leftAttributeChangeWarnings_new,
		      midGridWriteTypeOnNext_new, 
		      leftTreeTextColourOn_new, 
		      xmlMessagesOnSave_new, 
		      PADSCpath_new,
		      PADSCinc_new,
		      ASTinc_new,
		      CCpath_new,
		      CCargs_new, 
		      leftControlSafeMode_new,
		      PADXPath_new);

  CreateButtons(wxOK|wxCANCEL /*|wxHELP*/);

  // taken from the wxPropertySheet documentation
  wxPanel* panel;

  panel = new wxPanel(GetBookCtrl());
  GetBookCtrl()->AddPage(panel, _T("General"));
  pdCreateGeneralPrefsPage(panel);
  pdGenPanel = panel;

  panel = new wxPanel(GetBookCtrl());
  GetBookCtrl()->AddPage(panel, _T("Import"));
  pdCreateTopPrefsPage(panel);
  pdTopPanel = panel;

  panel = new wxPanel(GetBookCtrl());
  GetBookCtrl()->AddPage(panel, _T("Grid"));
  pdCreateMidPrefsPage(panel);
  pdMidPanel = panel;

  panel = new wxPanel(GetBookCtrl());
  GetBookCtrl()->AddPage(panel, _T("Tree"));
  pdCreateLeftPrefsPage(panel);
  pdLeftPanel = panel;

  panel = new wxPanel(GetBookCtrl());
  GetBookCtrl()->AddPage(panel, _T("Code View"));
  pdCreateBottomPrefsPage(panel);
  pdBottomPanel = panel;

  panel = new wxPanel(GetBookCtrl());
  GetBookCtrl()->AddPage(panel, _T("Tools"));
  pdCreateToolsPrefsPage(panel);
  pdToolPanel = panel;

  LayoutDialog();
}

PrefsDialog::~PrefsDialog()
{

  saveLayoutOnExit_new = pdGeneralSaveLayoutOnExit->GetValue();
  colourModeOn_new     = !pdGeneralNoColourMode->GetValue();
  midGridColNumberLabels_new = pdMidColumnLabels->GetSelection() == 0 ? true : false;
  // topNumLinesOnImport_new // taken care of in the prefs event handler
  midUDefErrorMBoxOn_new = pdMidTermActiveWarnings->GetValue();
  midTermErrorMBoxOn_new = pdMidNontermActiveWarnings->GetValue();
  rightBuildTreeMBoxOn_new = pdMidBuildTreeWarning->GetValue();
  leftControlsWarnings_new = pdLeftActiveWarnings->GetValue();
  leftControlSafeMode_new  = pdLeftControlSafeMode->GetValue();
  leftAttributeChangeWarnings_new = pdLeftActiveWarnings->GetValue();
  midGridWriteTypeOnNext_new = pdMidGridWriteTypeOnNext->GetValue();
  
  leftTreeBackgroundColourOn_new = pdLeftTreeColour->GetSelection() == TREE_COLOUR_BG   ? true : false;
  leftTreeTextColourOn_new       = pdLeftTreeColour->GetSelection() == TREE_COLOUR_TEXT ? true : false;

  xmlMessagesOnSave_new          = pdGeneralXmlMessagesOnSave->GetValue();

  PADSCpath_new = pdToolsPADSCpath->GetValue();
  //PADSCinc_new = pdToolsPADSCinc->GetValue();
  ASTinc_new = pdToolsASTinc->GetValue();
  CCpath_new = pdToolsCCpath->GetValue();
  CCargs_new = pdToolsCCargs->GetValue();
  PADXPath_new = pdToolsPADXPath->GetValue();

  DB_P("Saving settings on prefsdialog destructor\n");
  parent->ChangeSettings( 
			 saveLayoutOnExit_new,
			 colourModeOn_new,
			 midGridColNumberLabels_new,
			 topNumLinesOnImport_new,
			 midUDefErrorMBoxOn_new,
			 midTermErrorMBoxOn_new,
			 rightBuildTreeMBoxOn_new,
			 leftTreeBackgroundColourOn_new,
			 leftControlsWarnings_new,
			 leftAttributeChangeWarnings_new, 
			 midGridWriteTypeOnNext_new, 
			 leftTreeTextColourOn_new, 
			 xmlMessagesOnSave_new, 
			 PADSCpath_new,
			 PADSCinc_new,
			 ASTinc_new,
			 CCpath_new,
			 CCargs_new, 
			 leftControlSafeMode_new,
			  PADXPath_new);
  /*
  pdGenPanel->Destroy();
  pdTopPanel->Destroy();
  pdMidPanel->Destroy();
  pdLeftPanel->Destroy();
  pdBottomPanel->Destroy();
  pdToolPanel->Destroy();
  */
}

void PrefsDialog::pdCreateGeneralPrefsPage(wxWindow* panel)
{
  pdGeneralButtonSizer = new wxGridBagSizer(5, 5);
  pdGeneralMainSizer = new wxBoxSizer(wxVERTICAL);

  pdGeneralSaveLayoutOnExit = new wxCheckBox(panel, 
					     PREFS_SAVE_LAYOUT_EXIT, 
					     _T("Save Layout On Exit"));
  pdGeneralSaveLayoutNow = new wxButton(panel,
					PREFS_SAVE_LAYOUT_NOW,
					_T("Save Layout Now"),
					wxDefaultPosition,
					wxDefaultSize,
					wxBU_EXACTFIT);
  pdGeneralNoColourMode = new wxCheckBox(panel, 
					 PREFS_NO_COLOUR, 
					 _T("Grayscale Mode"));
  pdGeneralXmlMessagesOnSave = new wxCheckBox(panel,
					      PREFS_XML_MESSAGE_ON_SAVE,
					      _T("Show Messages When Saving an XML State"));
  pdGeneralButtonSizer->Add(pdGeneralSaveLayoutOnExit, wxGBPosition(0, 0), wxDefaultSpan, wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT, 2);
  pdGeneralButtonSizer->Add(pdGeneralSaveLayoutNow, wxGBPosition(0, 1), wxDefaultSpan, wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT, 2);
  pdGeneralButtonSizer->Add(pdGeneralNoColourMode, wxGBPosition(1, 0), wxDefaultSpan, wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT, 2);
  pdGeneralButtonSizer->Add(pdGeneralXmlMessagesOnSave, wxGBPosition(2, 0), wxGBSpan(1, 2), wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT, 2);

  //pdGeneralMainSizer->Add(pdGeneralSaveLayoutOnExit, 0, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT, 2);
  //pdGeneralMainSizer->Add(pdGeneralSaveLayoutNow,  0, wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT, 2);
  //pdGeneralMainSizer->Add(pdGeneralNoColourMode, 0, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT, 2);

  pdGeneralMainSizer->Add(pdGeneralButtonSizer, 1, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 5);

  //panel->SetSizer(pdGeneralButtonSizer);
  //pdGeneralButtonSizer->Fit(panel);

  panel->SetSizer(pdGeneralMainSizer);
  pdGeneralMainSizer->Fit(panel);

  pdGeneralSaveLayoutOnExit->SetValue(saveLayoutOnExit_new);
  pdGeneralNoColourMode->SetValue(!colourModeOn_new);
  pdGeneralXmlMessagesOnSave->SetValue(xmlMessagesOnSave_new);
}

void PrefsDialog::pdCreateTopPrefsPage(wxWindow* panel)
{
  pdTopOverSizer = new wxBoxSizer(wxVERTICAL);
  pdTopSizer = new wxGridBagSizer(5, 5);

  pdTopSetFont = new wxButton(panel,
			      PREFS_TOP_SET_FONT,
			      _T("Set Font"),
			      wxDefaultPosition,
			      wxDefaultSize,
			      wxBU_EXACTFIT);

  pdTopSpinButtonSizer = new wxStaticBoxSizer(wxHORIZONTAL,
					      panel, 
					      _T("Number of lines to import"));

  pdTopNumLines = new wxTextCtrl(panel,
				 PREFS_TOP_EDIT_NUM_LINES,
				 _T(""),
				 wxDefaultPosition,
				 wxDefaultSize,
				 wxTE_READONLY);
  
  pdTopSetNumLines = new wxSpinButton(panel, 
				      PREFS_TOP_SET_NUM_LINES,
				      wxDefaultPosition,
				      wxDefaultSize,
				      wxVERTICAL | wxSP_ARROW_KEYS);
  pdTopSetNumLines->SetRange(1, 10000);
  pdTopSetNumLines->SetValue(topNumLinesOnImport_new);

  pdTopSpinButtonSizer->Add(pdTopNumLines, 1, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT, 2);
  pdTopSpinButtonSizer->Add(pdTopSetNumLines, 1,  wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT, 2);
  pdTopSizer->Add(pdTopSpinButtonSizer, wxGBPosition(0, 0), wxGBSpan(1, 1), wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  pdTopSizer->Add(pdTopSetFont, wxGBPosition(1, 0), wxDefaultSpan, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);

  pdTopOverSizer->Add(pdTopSizer, 1, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 5);

  panel->SetSizer(pdTopOverSizer);
  pdTopOverSizer->Fit(panel);

  wxString str;
  str.Printf("%d", topNumLinesOnImport_new);
  pdTopNumLines->SetValue(str);

}

void PrefsDialog::pdCreateMidPrefsPage(wxWindow* panel)
{
  wxString stStrings[2];
  stStrings[0] = "Numbers";
  stStrings[1] = "Letters";

  pdMidOverSizer = new wxBoxSizer(wxVERTICAL);
  pdMidSizer     = new wxGridBagSizer(2, 2);

  pdMidColumnLabels = new wxRadioBox(panel,
				     PREFS_MID_COLUMN_LABELS,
				     wxString("Column Labels"),
				     wxDefaultPosition,
				     wxDefaultSize,
				     2,
				     stStrings,
				     1,
				     wxRA_SPECIFY_ROWS);

  pdMidTermActiveWarnings = new wxCheckBox(panel, 
					   PREFS_MID_TERM_WARNINGS, 
					   _T("Grid Control Terminal Type Warnings"));
  
  pdMidNontermActiveWarnings = new wxCheckBox(panel, 
					      PREFS_MID_NONTERM_WARNINGS, 
					      _T("Grid Control Nonterminal Type Warnings"));

  pdMidBuildTreeWarning = new wxCheckBox(panel, 
					 PREFS_MID_BUILD_WARNING,
					 _T("Tree Construction Warning"));

  pdMidGridWriteTypeOnNext = new wxCheckBox(panel, 
					    PREFS_MID_WRITE_NEW_TYPE,
					    _T("Write Selected Types On Grid Fragmentation"));

  pdMidSizer->Add(pdMidColumnLabels, wxGBPosition(0, 0), wxDefaultSpan, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  pdMidSizer->Add(pdMidTermActiveWarnings, wxGBPosition(1, 0), wxDefaultSpan, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  pdMidSizer->Add(pdMidNontermActiveWarnings, wxGBPosition(2, 0), wxDefaultSpan, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  pdMidSizer->Add(pdMidBuildTreeWarning, wxGBPosition(3, 0), wxDefaultSpan, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  pdMidSizer->Add(pdMidGridWriteTypeOnNext, wxGBPosition(4, 0), wxDefaultSpan, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  pdMidOverSizer->Add(pdMidSizer, 1, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 5);

  panel->SetSizer(pdMidOverSizer);
  pdMidOverSizer->Fit(panel);

  pdMidColumnLabels->SetSelection(midGridColNumberLabels_new ? 0 : 1);
  pdMidTermActiveWarnings->SetValue(midTermErrorMBoxOn_new);
  pdMidNontermActiveWarnings->SetValue(midUDefErrorMBoxOn_new);
  pdMidBuildTreeWarning->SetValue(rightBuildTreeMBoxOn_new);
  pdMidGridWriteTypeOnNext->SetValue(midGridWriteTypeOnNext_new);
}

void PrefsDialog::pdCreateLeftPrefsPage(wxWindow* panel)
{
  pdLeftOverSizer = new wxBoxSizer(wxVERTICAL);
  pdLeftSizer     = new wxGridBagSizer(2, 2);

  pdLeftActiveWarnings = new wxCheckBox(panel,
					PREFS_LEFT_ACTIVE_WARNINGS,
					_T("Tree Control Warnings"));

  pdLeftControlSafeMode = new wxCheckBox(panel,
					 PREFS_LEFT_CONTROL_SAFE_MODE,
					 _T("Activate Only Type-Appropriate Tree Controls"));

  /*
  pdLeftTreeBGColour = new wxCheckBox(panel,
				      PREFS_LEFT_BG_COLOUR,
				      _T("Tree Background Colors"));
  */
  wxString leftTreeOpts[3];
  leftTreeOpts[0] = _T("None");
  leftTreeOpts[1] = _T("Background");
  leftTreeOpts[2] = _T("Text");

  pdLeftTreeColour  = new wxRadioBox(panel,
				     PREFS_LEFT_BG_COLOUR,
				     _T("Tree Color Options"),
				     wxDefaultPosition,
				     wxDefaultSize,
				     WXSIZEOF(leftTreeOpts),
				     leftTreeOpts);


  pdLeftSizer->Add(pdLeftActiveWarnings, wxGBPosition(0, 0));
  pdLeftSizer->Add(pdLeftControlSafeMode, wxGBPosition(1, 0));
  //pdLeftSizer->Add(pdLeftTreeBGColour, wxGBPosition(1, 0));
  pdLeftSizer->Add(pdLeftTreeColour, wxGBPosition(2, 0));

  pdLeftOverSizer->Add(pdLeftSizer, 1, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 5);

  panel->SetSizer(pdLeftOverSizer);
  pdLeftOverSizer->Fit(panel);

  pdLeftActiveWarnings->SetValue(leftControlsWarnings_new);
  pdLeftControlSafeMode->SetValue(leftControlSafeMode_new);
  //pdLeftTreeBGColour->SetValue(leftTreeBackgroundColourOn_new);
  if(leftTreeTextColourOn_new)
    pdLeftTreeColour->SetSelection(TREE_COLOUR_TEXT);
  else if(leftTreeBackgroundColourOn_new)
    pdLeftTreeColour->SetSelection(TREE_COLOUR_BG);
  else
    pdLeftTreeColour->SetSelection(TREE_COLOUR_NONE);
}

void PrefsDialog::pdCreateBottomPrefsPage(wxWindow* panel)
{
  pdBottomOverSizer = new wxBoxSizer(wxVERTICAL);
  pdBottomSizer     = new wxGridBagSizer(2, 2);

  pdBottomSetTermColour = new wxButton(panel,
				       PREFS_BOTTOM_COLOUR_TERM,
				       _T("Set Terminal Syntax Color"),
				       wxDefaultPosition,
				       wxDefaultSize,
				       wxBU_EXACTFIT);
  
  pdBottomSetNonTermColour = new wxButton(panel,
				       PREFS_BOTTOM_COLOUR_NONTERM,
				       _T("Set Nonterminal Syntax Color"),
				       wxDefaultPosition,
				       wxDefaultSize,
				       wxBU_EXACTFIT);

  pdBottomSetLitColour = new wxButton(panel,
				       PREFS_BOTTOM_COLOUR_LIT,
				       _T("Set Literal Sytax Color"),
				       wxDefaultPosition,
				       wxDefaultSize,
				       wxBU_EXACTFIT);

  pdBottomSetErrorColour = new wxButton(panel,
					PREFS_BOTTOM_COLOUR_ERROR,
				       _T("Set Error Syntax Color"),
				       wxDefaultPosition,
				       wxDefaultSize,
				       wxBU_EXACTFIT);

  pdBottomSetOtherColour = new wxButton(panel,
					PREFS_BOTTOM_COLOUR_OTHER,
				       _T("Set Default Syntax Color"),
				       wxDefaultPosition,
				       wxDefaultSize,
				       wxBU_EXACTFIT);
  /*
  pdBottomSetFont = new wxButton(panel,
				 PREFS_BOTTOM_SET_FONT,
				 _T("Set Code Font"),
				 wxDefaultPosition,
				 wxDefaultSize,
				 wxBU_EXACTFIT);
  */
  
  pdBottomSizer->Add(pdBottomSetTermColour,    wxGBPosition(0, 0), wxDefaultSpan, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  pdBottomSizer->Add(pdBottomSetNonTermColour, wxGBPosition(0, 1), wxDefaultSpan, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  pdBottomSizer->Add(pdBottomSetLitColour,     wxGBPosition(1, 0), wxDefaultSpan, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  pdBottomSizer->Add(pdBottomSetErrorColour,   wxGBPosition(1, 1), wxDefaultSpan, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  pdBottomSizer->Add(pdBottomSetOtherColour,   wxGBPosition(2, 0), wxDefaultSpan, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  //pdBottomSizer->Add(pdBottomSetFont,          wxGBPosition(2, 1), wxDefaultSpan, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  

  /*
  pdBottomSizer->Add(pdBottomSetTermColour,    0, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  pdBottomSizer->Add(pdBottomSetNonTermColour, 0, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  pdBottomSizer->Add(pdBottomSetLitColour,     0, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  pdBottomSizer->Add(pdBottomSetErrorColour,   0, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  pdBottomSizer->Add(pdBottomSetOtherColour,   0, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  pdBottomSizer->Add(pdBottomSetFont,          0, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  */
  pdBottomOverSizer->Add(pdBottomSizer, 1, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 5);

  panel->SetSizer(pdBottomOverSizer);
  pdBottomOverSizer->Fit(panel);
}

void PrefsDialog::pdCreateToolsPrefsPage(wxWindow* panel)
{
  pdToolsOverSizer = new wxBoxSizer(wxVERTICAL);
  pdToolsSizer     = new wxFlexGridSizer(2, 2, 2);

  //pdToolsPADSCpathSizer = new wxBoxSizer(wxHORIZONTAL);
  pdToolsPADSCpathText  = new wxStaticText(panel,
					  wxID_ANY, 
					  _T("PADS home"));
  pdToolsPADSCpath      = new wxTextCtrl(panel,
					wxID_ANY);
  /*
  //pdToolsPADSCincludeSizer = new wxBoxSizer(wxHORIZONTAL);
  pdToolsPADSCincludeText  = new wxStaticText(panel,
					  wxID_ANY, 
					  _T("PADSC \"include\""));
  pdToolsPADSCinc          = new wxTextCtrl(panel,
					wxID_ANY);
  */

  //pdToolsASTincludeSizer = new wxBoxSizer(wxHORIZONTAL);
  pdToolsASTincludeText  = new wxStaticText(panel,
					  wxID_ANY, 
					  _T("libast \"include\""));
  pdToolsASTinc          = new wxTextCtrl(panel,
					wxID_ANY);

  //pdToolsCCpathSizer    = new wxBoxSizer(wxHORIZONTAL);
  pdToolsCCpathText     = new wxStaticText(panel,
					  wxID_ANY, 
					  _T("CC path"));
  pdToolsCCpath         = new wxTextCtrl(panel,
					  wxID_ANY);

  //pdToolsCCargsSizer    = new wxBoxSizer(wxHORIZONTAL);
  pdToolsCCargsText     = new wxStaticText(panel,
					  wxID_ANY, 
					  _T("CC args"));
  pdToolsCCargs         = new wxTextCtrl(panel,
					 wxID_ANY);

  pdToolsPADXPathText   = new wxStaticText(panel,
					   wxID_ANY,
					   _T("PADX Path"));
  pdToolsPADXPath       = new wxTextCtrl(panel,
					 wxID_ANY);

  // there is more than one way to do this... unfortunately it took me 4 tries to find the right one.
  /*
  pdToolsSizer->Add(pdToolsPADSCpathText, wxGBPosition(0, 0), wxDefaultSpan, wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  pdToolsSizer->Add(pdToolsPADSCpath,     wxGBPosition(0, 1), wxDefaultSpan, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  pdToolsSizer->Add(pdToolsCCpathText,    wxGBPosition(1, 0), wxDefaultSpan, wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  pdToolsSizer->Add(pdToolsCCpath,        wxGBPosition(1, 1), wxDefaultSpan, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  pdToolsSizer->Add(pdToolsCCargsText,    wxGBPosition(2, 0), wxDefaultSpan, wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  pdToolsSizer->Add(pdToolsCCargs,        wxGBPosition(2, 1), wxDefaultSpan, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  */
  /*
  pdToolsPADSCpathSizer->Add(pdToolsPADSCpath,  1, wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT, 2);
  pdToolsCCpathSizer->Add(pdToolsCCpath,        1, wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT, 2);
  pdToolsCCargsSizer->Add(pdToolsCCargs,        1, wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT, 2);

  pdToolsSizer->Add(pdToolsPADSCpathText,  0, wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  //  pdToolsSizer->Add(pdToolsPADSCpath,      1, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  pdToolsSizer->Add(pdToolsPADSCpathSizer, 1, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);

  pdToolsSizer->Add(pdToolsCCpathText,     0, wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  //  pdToolsSizer->Add(pdToolsCCpath,         1, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  pdToolsSizer->Add(pdToolsCCpathSizer,    1, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);

  pdToolsSizer->Add(pdToolsCCargsText,     0, wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  //  pdToolsSizer->Add(pdToolsCCargs,         1, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  pdToolsSizer->Add(pdToolsCCargsSizer,    1, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  */

  pdToolsSizer->AddGrowableCol(1, 1);

  pdToolsSizer->Add(pdToolsPADSCpathText,    0, wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT, 2);
  pdToolsSizer->Add(pdToolsPADSCpath,        1, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT, 2);
  /*
  pdToolsSizer->Add(pdToolsPADSCincludeText, 0, wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT, 2);
  pdToolsSizer->Add(pdToolsPADSCinc,         1, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT, 2);
  */
  pdToolsSizer->Add(pdToolsASTincludeText,   0, wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT, 2);
  pdToolsSizer->Add(pdToolsASTinc,           1, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT, 2);
  
  pdToolsSizer->Add(pdToolsCCpathText,       0, wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT, 2);
  pdToolsSizer->Add(pdToolsCCpath,           1, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT, 2);

  pdToolsSizer->Add(pdToolsCCargsText,       0, wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT, 2);
  pdToolsSizer->Add(pdToolsCCargs,           1, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT, 2);

  pdToolsSizer->Add(pdToolsPADXPathText,     0, wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT, 2);
  pdToolsSizer->Add(pdToolsPADXPath,         1, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT, 2);
  /*
  pdToolsPADSCpathSizer->Add(pdToolsPADSCpathText,  0, wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  pdToolsPADSCpathSizer->Add(pdToolsPADSCpath,      1, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT, 2);  
  pdToolsOverSizer->Add(pdToolsPADSCpathSizer,      0, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT, 2);

  pdToolsCCpathSizer->Add(pdToolsCCpathText,        0, wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  pdToolsCCpathSizer->Add(pdToolsCCpath,            1, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT, 2);
  pdToolsOverSizer->Add(pdToolsCCpathSizer,         0, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT, 2);

  pdToolsCCargsSizer->Add(pdToolsCCargsText,        0, wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  pdToolsCCargsSizer->Add(pdToolsCCargs,            1, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT, 2);
  pdToolsOverSizer->Add(pdToolsCCargsSizer,         0, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT, 2);
  */
  pdToolsSizer->SetFlexibleDirection(wxHORIZONTAL);

  pdToolsOverSizer->Add(pdToolsSizer, 1, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 5);

  panel->SetSizer(pdToolsOverSizer);
  pdToolsOverSizer->Fit(panel);

  pdToolsPADSCpath->SetValue(PADSCpath_new);
  //pdToolsPADSCinc->SetValue(PADSCinc_new);
  pdToolsASTinc->SetValue(ASTinc_new);
  pdToolsCCpath->SetValue(CCpath_new);
  pdToolsCCargs->SetValue(CCargs_new);
  pdToolsPADXPath->SetValue(PADXPath_new);
}

/* ************************************ */

void PrefsDialog::OnSaveLayoutNow(wxCommandEvent &event)
{
  parent->SaveLayout();
}

void PrefsDialog::OnChangeTopNumLines(wxSpinEvent &event)
{
  topNumLinesOnImport_new = event.GetPosition();

  wxString str;
  str.Printf("%d", topNumLinesOnImport_new);
  pdTopNumLines->SetValue(str);
}

void PrefsDialog::OnChangeTopFont(wxCommandEvent &event)
{
  wxFontData fontData;
  wxFont font;

  DB_P("in OnChangeTopFont\n");

  parent->TopGetFont(font);
  fontData.SetInitialFont(font);

#ifdef __WXMSW__
  fontData.SetAllowSymbols(false);
  fontData.SetEnableEffects(false);
#endif

  wxFontDialog fontDialog(parent, fontData);
  if(fontDialog.ShowModal() == wxID_OK)
    {    
      fontData = fontDialog.GetFontData();
      font = fontData.GetChosenFont();
      parent->TopSetFont(font);
    }

}


void PrefsDialog::OnChangeBottomFont(wxCommandEvent &event)
{
  wxFontData fontData;
  wxFont font;

  DB_P("in OnChangeBottomFont\n");

  parent->BottomGetFont(font);
  fontData.SetInitialFont(font);

#ifdef __WXMSW__
  fontData.SetAllowSymbols(false);
  fontData.SetEnableEffects(false);
#endif

  wxFontDialog fontDialog(parent, fontData);
  if(fontDialog.ShowModal() == wxID_OK)
    {    
      fontData = fontDialog.GetFontData();
      font = fontData.GetChosenFont();
      parent->BottomSetFont(font);
    }

}




void PrefsDialog::OnChangeTermColour(wxCommandEvent &event)
{
  wxColour colour;
  wxColourData colourData;

  colour = parent->BottomGetTermColour();
  colourData.SetColour(colour);

  wxColourDialog colourDialog(parent, &colourData);
  if(colourDialog.ShowModal() == wxID_OK)
    {
      DB_P("setting terminal syntax colour\n");
      colourData = colourDialog.GetColourData();
      colour = colourData.GetColour();
      parent->BottomSetTermColour(colour);
    }
}

void PrefsDialog::OnChangeNontermColour(wxCommandEvent &event)
{
  wxColour colour;
  wxColourData colourData;

  colour = parent->BottomGetNontermColour();
  colourData.SetColour(colour);

  wxColourDialog colourDialog(parent, &colourData);
  if(colourDialog.ShowModal() == wxID_OK)
    {
      colourData = colourDialog.GetColourData();
      colour = colourData.GetColour();
      parent->BottomSetNontermColour(colour);
    }
}

void PrefsDialog::OnChangeLitColour(wxCommandEvent &event)
{
  wxColour colour;
  wxColourData colourData;

  colour = parent->BottomGetLitColour();
  colourData.SetColour(colour);

  wxColourDialog colourDialog(parent, &colourData);
  if(colourDialog.ShowModal() == wxID_OK)
    {
      colourData = colourDialog.GetColourData();
      colour = colourData.GetColour();
      parent->BottomSetLitColour(colour);
    }
}

void PrefsDialog::OnChangeErrorColour(wxCommandEvent &event)
{
  wxColour colour;
  wxColourData colourData;

  colour = parent->BottomGetErrorColour();
  colourData.SetColour(colour);

  wxColourDialog colourDialog(parent, &colourData);
  if(colourDialog.ShowModal() == wxID_OK)
    {
      colourData = colourDialog.GetColourData();
      colour = colourData.GetColour();
      parent->BottomSetErrorColour(colour);
    }
}

void PrefsDialog::OnChangeOtherColour(wxCommandEvent &event)
{
  wxColour colour;
  wxColourData colourData;

  colour = parent->BottomGetOtherColour();
  colourData.SetColour(colour);

  wxColourDialog colourDialog(parent, &colourData);
  if(colourDialog.ShowModal() == wxID_OK)
    {
      colourData = colourDialog.GetColourData();
      colour = colourData.GetColour();
      parent->BottomSetOtherColour(colour);
    }
}



/* ************************************ */
