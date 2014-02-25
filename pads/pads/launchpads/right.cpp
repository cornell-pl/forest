/* **************************************
 * PADS GUI project (preliminary tests)
 * Right frame related code
 * *********************************** */
#include "lp_includes.h"
#include "pads_gui.h"

/* **************************************
 * Functions:
 *  --init
 *  RightInitFrame
 *  RightMakeButtons_1
 *  RightMakeButtons_2
 *  RightMakeRadio_1
 *  RightMakeComboBox_1
 *  RightMakeComboBox_2
 *
 *  --event handlers
 *  OnRightModeSelect
 *  OnRightFragment
 *  OnRightBack
 *  OnRightReset
 *  OnRightBuildTree
 *  OnRightMakeCode
 *  OnRightSaveState
 *  OnRightExportCode
 *
 *  --interface
 *  RightEnableComboBox_1  --en/disable terminal type list
 *  RightEnableComboBox_2  --en/disable nonterminal type list 
 *  RightGetTermType       --get selected terminal type
 *  RightGetNontermType    --get selected nonterminal type
 *  RightSetAttr           --set control state for right frame
 *  RightSetRadioMode      --set current edit mode (note: bug under GTK prevents radio box updates)
 *  RightCycleOptions      --cycle through type menus depending on current mode
 * *********************************** */

void LaunchPADS::RightInitFrame(long panelflags, 
			       long buttonflags, 
			       long radioflags,
			       long comboflags)
{
  m_rightWin->SetMinimumSizeX(RIGHT_WIN_DEFAULT_X);
  
  m_rightWinPanel = new wxPanel(m_rightWin, 
				 wxID_ANY, 
				 wxDefaultPosition,
				 m_rightWin->GetSize(),
				 panelflags);
  
  //m_rightWinPanel->SetBackgroundColour(wxColour(222, 222, 222));

  // another major mess... best to leave this alone too.

  RightMakeButtons_1(buttonflags);
  RightMakeButtons_2(buttonflags);

  rightWinButtonSizer_1 = new wxGridBagSizer(0, 0);
  rightWinButtonSizer_1->Add(rightFragment,     wxGBPosition(0, 0),  wxGBSpan(1, 2), wxGROW | wxALL | wxALIGN_TOP | wxALIGN_CENTER, 2);
  rightWinButtonSizer_1->Add(rightBackOneLevel, wxGBPosition(1, 0),  wxDefaultSpan,  wxGROW | wxALL | wxALIGN_TOP | wxALIGN_CENTER, 2);
  rightWinButtonSizer_1->Add(rightReset,        wxGBPosition(1, 1),  wxDefaultSpan,  wxGROW | wxALL | wxALIGN_TOP | wxALIGN_CENTER, 2);

  rightStatLine_1 = new wxStaticLine(m_rightWinPanel);

  RightMakeRadio_1(radioflags);
  RightMakeComboBox_1(comboflags);
  RightMakeComboBox_2(comboflags);

  rightWinButtonSizer_2 = new wxBoxSizer(wxVERTICAL);
  rightWinButtonSizer_2->Add(rightWinButtonSizer_1, 0,        wxBOTTOM | wxALIGN_TOP | wxALIGN_CENTER, 4);

  rightWinButtonSizer_2->Add(rightStatLine_1,       0,  wxGROW | wxALL | wxALIGN_TOP | wxALIGN_CENTER, 2);
  rightWinButtonSizer_2->Add(rightModeSelect,       0,  wxGROW | wxALL | wxALIGN_TOP | wxALIGN_CENTER, 2);

  rightStatBox_1->Add(rightTypeSelect,              0,  wxGROW | wxALL | wxALIGN_TOP | wxALIGN_CENTER, 2);
  rightWinButtonSizer_2->Add(rightStatBox_1,        0,  wxGROW | wxALL | wxALIGN_TOP | wxALIGN_CENTER, 2);
  rightStatBox_2->Add(rightStructSelect,            0,  wxGROW | wxALL | wxALIGN_TOP | wxALIGN_CENTER, 2);
  rightWinButtonSizer_2->Add(rightStatBox_2,        0,  wxGROW | wxALL | wxALIGN_TOP | wxALIGN_CENTER, 2);

  rightStatLine_2 = new wxStaticLine(m_rightWinPanel);
  rightWinButtonSizer_2->Add(rightStatLine_2, 0,  wxGROW | wxALL | wxALIGN_TOP | wxALIGN_CENTER, 4);

  rightWinButtonSizer_3 = new wxFlexGridSizer(1);
  rightWinButtonSizer_3->Add(rightMakeTree,    0,   wxGROW | wxALL | wxALIGN_TOP | wxALIGN_CENTER, 2);
  rightStatLine_3       = new wxStaticLine(m_rightWinPanel);
  rightWinButtonSizer_3->Add(rightStatLine_3,  0,   wxGROW | wxALL | wxALIGN_TOP | wxALIGN_CENTER, 2);
  rightWinButtonSizer_3->Add(rightMakeCode,    0,   wxGROW | wxALL | wxALIGN_TOP | wxALIGN_CENTER, 2);
  rightWinButtonSizer_3->Add(rightExportCode,  0,   wxGROW | wxALL | wxALIGN_TOP | wxALIGN_CENTER, 2);
  //rightWinButtonSizer_3->Add(rightSaveSession, 0,   wxGROW | wxALL | wxALIGN_TOP | wxALIGN_CENTER, 2);
  rightWinButtonSizer_2->Add(rightWinButtonSizer_3, 0, wxALL | wxALIGN_TOP | wxALIGN_CENTER, 0);

  rightWinSizer = new wxBoxSizer(wxVERTICAL);
  //  rightWinSizer->Add(rightWinButtonSizer_1, 0,  wxGROW | wxALL | wxALIGN_TOP | wxALIGN_CENTER, 5);
  rightWinSizer->Add(rightWinButtonSizer_2, 1,  wxGROW | wxALL | wxALIGN_TOP | wxALIGN_CENTER, 5);


  RightSetAttr(0, 0, 0);

  m_rightWinPanel->SetSizer(rightWinSizer);
  m_rightWinPanel->SetAutoLayout(true);
  rightWinSizer->SetSizeHints(m_rightWinPanel);

}

void LaunchPADS::RightMakeButtons_1(long flags)
{
  rightFragment = new wxButton(m_rightWinPanel, 
			       RIGHT_FRAGMENT,
			       _T("Fr&agment"), 
			       wxDefaultPosition, 
			       wxDefaultSize, 
			       flags);
  rightFragment->SetToolTip(wxString("Divide fields along set delimiters"));

  rightBackOneLevel = new wxButton(m_rightWinPanel,
				   RIGHT_BACK,
				   _T("  &Back  "),
				   wxDefaultPosition,
				   wxDefaultSize,
				   flags);
  rightBackOneLevel->SetToolTip(wxString("Undo grid fragmentation by one level"));


  rightReset = new wxButton(m_rightWinPanel, 
			       RIGHT_RESET,
			       _T("  Reset  "), 
			       wxDefaultPosition, 
			       wxDefaultSize, 
			       flags);
  rightReset->SetToolTip(wxString("Reset delimiters (start over)"));

}


void LaunchPADS::RightMakeButtons_2(long flags)
{
  rightMakeTree = new wxButton(m_rightWinPanel, 
			       RIGHT_MAKE_TREE,
			       _T("B&uild Tree"), 
			       wxDefaultPosition, 
			       wxDefaultSize, 
			       flags);
  rightMakeTree->SetToolTip(wxString("Generate code tree from grid structure"));


  rightMakeCode = new wxButton(m_rightWinPanel, 
			       RIGHT_MAKE_CODE,
			       _T("&Make Code"), 
			       wxDefaultPosition, 
			       wxDefaultSize, 
			       flags);
  rightMakeCode->SetToolTip(wxString("Generate PADS definition from code tree"));

  rightExportCode = new wxButton(m_rightWinPanel, 
				 RIGHT_EXPORT_CODE,
				 _T("Save Code"), 
				 wxDefaultPosition, 
				 wxDefaultSize, 
				 flags);
  rightExportCode->SetToolTip(wxString("Save PADS code as a .p file"));

  /*  rightSaveSession = new wxButton(m_rightWinPanel, 
				 RIGHT_SAVE_SESSION,
				 _T("Save Session"), 
				 wxDefaultPosition, 
				 wxDefaultSize, 
				 flags);
  rightSaveSession->SetToolTip(wxString("Save current session and settings"));
  */
}


void LaunchPADS::RightMakeRadio_1(long flags)
{

  wxString stStrings[4]; 

  stStrings[0] = _T("None");
  stStrings[1] = _T("&Delimiter"); 
  stStrings[2] = _T("&Terminal");
  stStrings[3] = _T("&Nonterminal");

  rightModeSelect = new wxRadioBox(m_rightWinPanel,
				   RIGHT_MODE_SELECT,
				   wxString("Selection Mode"),
				   wxDefaultPosition,
				   wxDefaultSize,
				   4,
				   stStrings,
				   1,
				   wxRA_SPECIFY_COLS);
				   
  rightModeSelect->SetToolTip(wxString("Selection modes for grid-based definition construction"));
}

void LaunchPADS::RightMakeComboBox_1(long flags)
{
  
  rightStatBox_1 = new wxStaticBoxSizer(wxVERTICAL,
				       m_rightWinPanel,
				       wxString("Terminal Types"));

  wxString stStrings[P_TERM_SELECTABLE];

  // provide options only for the types we want to be able to use in the grid (not case, function, etc.)
  int i, j;
  for(i = pStartTerminals + 1, j = 0; 
      j < P_TERM_SELECTABLE; i++, j++)
    {
      stStrings[j] = PADS_labels[i];
    }

  int chString_size = WXSIZEOF(stStrings);

  rightTypeSelect = new wxComboBox(m_rightWinPanel,
				    RIGHT_FIELD_SELECT,
				    _T(""),
				    wxDefaultPosition,
				    wxDefaultSize,
				    chString_size,
				    stStrings,
				    flags
				    & (~wxCB_SORT));

  RightEnableComboBox_1(false);
  return;
}

void LaunchPADS::RightMakeComboBox_2(long flags)
{
  
  rightStatBox_2 = new wxStaticBoxSizer(wxVERTICAL,
				       m_rightWinPanel,
				       wxString("Nonterminal Types"));

  wxString stStrings[P_NON_TERM_SELECTABLE];

  int i, j;
  for(i = pStartNonTerminals + 1, j = 0; 
      j < P_NON_TERM_SELECTABLE; i++, j++)
    {
      stStrings[j] = PADS_labels[i];
    }

  int stString_size = WXSIZEOF(stStrings); 

  rightStructSelect = new wxComboBox(m_rightWinPanel,
				    RIGHT_FIELD_SELECT,
				    _T(""),
				    wxDefaultPosition,
				    wxDefaultSize,
				    stString_size,
				    stStrings,
				    flags
				    & (~wxCB_SORT));

  RightEnableComboBox_2(false);
  return;
}

/* ************************************ */

// right control rerouting functions 
void LaunchPADS::OnRightModeSelect(wxCommandEvent &event)
{
  MidSetEditMode(event.GetInt());
  return;
}


void LaunchPADS::OnRightFragment(wxCommandEvent &event)
{
  MidGridNextLevel();
}

void LaunchPADS::OnRightBack(wxCommandEvent &event)
{
  MidGridLastLevel();
}

void LaunchPADS::OnRightReset(wxCommandEvent &event)
{
  if(wxMessageBox(_T("This will reset the grid to the first step.\nContinue?"), 
		  _T("Confirm Grid Reset"),
		  wxICON_QUESTION | wxYES_NO) == wxYES)
    {
      MidGridStartOver();
    }
  return;
}

void LaunchPADS::OnRightBuildTree(wxCommandEvent &event)
{

#ifdef DB_ON
  for(int i = 0; i < midGridTextLength; i++)
    {
      DB_P("%3c", midGridText[i]);
    }
  DB_P("\n");
  for(int i = 0; i <= midCurrentRow; i++)
    {
      int j;
      for(j = 0; j < midGridTextLength; j++)
	{
	  DB_P("%3d", midGridTypeLevels[i][j]);
	}
      DB_P("\n");
      for(j = 0; j < midGridTextLength; j++)
	{
	  DB_P("%3d", midGridUDefIDLevels[i][j]);
	}
      DB_P("\n\n");
    }

#endif

  if(!MidGridReadyForTree())
    {
      if(rightBuildTreeMBoxOn)
	{
	  wxMessageBox(_T("No non-terminal or non-delimiter grid cells may remain for source tree construction."),
		     _T("Cannot Build Source Tree!"),
		     wxICON_ERROR | wxOK);
	  return;
	}
    }


  if(wxMessageBox(_T("This will destroy the current tree.\nContinue?"), 
		  _T("Confirm Tree Construction"),
		  wxICON_QUESTION | wxYES_NO) == wxYES)
    {
      LeftBuildTree();
    }
  
  return;
}

void LaunchPADS::OnRightMakeCode(wxCommandEvent &event)
{
  if(wxMessageBox(_T("This will overwrite the contents of the code view without saving.\nContinue?"), 
		  _T("Confirm Code Generation"),
		  wxICON_QUESTION | wxYES_NO) == wxYES)
    {
      BottomClearText();
      LeftMakeCodeFromTree();  // cross your fingers...
      bottomCodeMade = true;
    }

}

// this is now handled by the toolbar
void LaunchPADS::OnRightSaveState(wxCommandEvent &event)
{

}

void LaunchPADS::OnRightExportCode(wxCommandEvent &event)
{
  wxFileDialog saveFileDialog (this, 
			       "Save PADS code",
			       "",
			       "",
			       CODE_FILETYPES,
			       wxSAVE | wxOVERWRITE_PROMPT,
			       wxDefaultPosition);

    if(saveFileDialog.ShowModal() == wxID_OK)
      {
	wxString fileName = saveFileDialog.GetPath();
	wxRegEx regEx("[[:space:]]$");
	if(!regEx.IsValid())
	  {
	    wxLogError("Code save regular expression is invalid.");
	    return;
	  }
	DB_P("replacing trailing spaces\n");
	if(regEx.Matches(fileName))
	  {
	    wxLogError(_T("Pads definition filenames may not have trailing spaces (code was not saved)."));
	    return;
	  }

	wxFFile defFile(fileName, "w");
	wxString str = BottomGetText();
	DB_P("Opening file %s\n", saveFileDialog.GetFilename().c_str());

	if(!defFile.Write(str))
	  {
	    wxLogError(_T("ERROR: could not write PADS code to file."));
	    bottomCodeSaved = false;
	    bottomCodePath = "";
	    bottomCodeName = "";
	  }
	else
	  {
	    bottomCodePath = saveFileDialog.GetPath();
	    bottomCodeName = saveFileDialog.GetFilename();
	    bottomCodeSaved = true;
	  }

	defFile.Close();
      }
    return;
}



void LaunchPADS::RightEnableComboBox_1(bool choice)
{
  rightTypeSelect->Enable(choice);
}

void LaunchPADS::RightEnableComboBox_2(bool choice)
{
  rightStructSelect->Enable(choice);
}

// convert combo box selections to lp_PADS enum types
int LaunchPADS::RightGetTermType(void)
{
  return (rightTypeSelect->GetSelection() + pStartTerminals + 1);
}

int LaunchPADS::RightGetNontermType(void)
{
  return (rightStructSelect->GetSelection() + pStartNonTerminals + 1);
}

// set control attributes to match parameters
void LaunchPADS::RightSetAttr(int mode = 0, int type = 0, int udef = 0)
{

  if(mode == MID_MODE_NONE  ||
     mode == MID_MODE_DELIM ||
     mode == MID_MODE_TERM  ||
     mode == MID_MODE_UDEF)
    {
      rightModeSelect->SetSelection(mode);
      MidSetEditMode(mode);
    }
  else
    {
      return;
    }

  if(mode == MID_MODE_DELIM)
    return;

  if(type < rightTypeSelect->GetCount())
    {
      DB_P("in type select with %d max, %d type\n",
	   rightTypeSelect->GetCount(),
	   type);
      rightTypeSelect->SetSelection(type);
    }

  if(udef < rightStructSelect->GetCount())
    {
      DB_P("in udef select with %d max, %d type\n",
	   rightTypeSelect->GetCount(),
	   type);
      rightStructSelect->SetSelection(udef);
    }
}


bool LaunchPADS::RightSetRadioMode(int mode)
{

  if(mode >= MID_MODE_NONE && mode <= MID_MODE_UDEF)
    {
      rightModeSelect->SetSelection(mode);
      return true;
    }
  return false;
}

// switch terminal/nonterminal options on repeated accelerator press
void LaunchPADS::RightCycleOptions(int mode)
{
  // picks next option for variable type modes
  switch(mode)
    {
    case MID_MODE_TERM:
	  rightTypeSelect->SetSelection((rightTypeSelect->GetSelection() + 1) 
					% rightTypeSelect->GetCount());
    case MID_MODE_UDEF:
      if(MidGridGetMode() == MID_MODE_UDEF)
	{
	  rightStructSelect->SetSelection((rightStructSelect->GetSelection() + 1) 
					  % rightStructSelect->GetCount());
	}
      MidSetEditMode(mode);
      RightSetRadioMode(MID_MODE_UDEF);
      break;
    }
  return;
}

/* ************************************ */

