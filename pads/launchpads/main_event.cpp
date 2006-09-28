/* **************************************
 * PADS GUI project (preliminary tests)
 * Main event handling code
 * *********************************** */
#include "lp_includes.h"

/* **************************************
 * Functions:
 *  --event handlers
 *  OnKeyPress
 *  OnContextMenu
 *  OnSize
 *  OnOpenFile
 *  OnSaveFile
 *  OnSaveAsFile
 *  OnAbout
 *  OnExit
 *  OnSashDrag
 *  OnWinSwap
 *  OnChangeCheck
 *  OnViewReset
 *  OnToolMenu
 *  OnToolbarPress
 *  OnPrefsDialog
 *  SetDefaultSettings
 *  ChangeSettings
 *  GetSettings
 *  SaveLayout
 * *********************************** */

// backend for the accelerator functions for the mid grid
void LaunchPADS::OnKeyPress(wxCommandEvent &event)
{
  int mode = event.GetId();

  DB_P("Got accel event id %d\n", mode);

  DB_P("select_get = %d\n", MID_SELECT_GET);

  if(mode >= MID_SELECT_NONE &&
     mode <= MID_SELECT_UDEF)
    {

      if(mode == MID_SELECT_TERM &&
	 MidGridGetMode() == MID_MODE_TERM)
	{ 
	  RightCycleOptions(MID_MODE_TERM); 
	}
      else if(mode == MID_SELECT_UDEF &&
	      MidGridGetMode() == MID_MODE_UDEF)
	{ 
	  RightCycleOptions(MID_MODE_UDEF); 
	}
      mode -= MID_SELECT_NONE;
      MidSetEditMode(mode);
      RightSetRadioMode(mode);
    }

  if(mode == MID_SELECT_GET)
    {
      DB_P("Got grid select id %d\n", mode);
      MidGridApplySelectedStats();
    }

  return;
}

// context menu routing functions for all subframes
void LaunchPADS::OnContextMenu(wxContextMenuEvent &event)
{
  int x = event.GetPosition().x;
  int y = event.GetPosition().y;
  int winx = this->GetPosition().x;
  int winy = this->GetPosition().y;


  DB_P("X: %d\tY: %d\n", 
       x, 
       y);

  DB_P("offset X: %d\tY: %d\n", 
       x - winx,
       y - winy);
  wxObject* obj = event.GetEventObject();
  /*
  DB_P("event obj = %d\n", obj);
  DB_P("frame obj = %d\n", (int)this);
  DB_P("sash opj = %d\n", (int)m_leftWin);
  DB_P("root opj = %d\n", (int)leftTreeCtrl);
  */
  if(obj == leftTreeCtrl)
    DB_P("tree menu\n");
  else if(obj == topCheckList)
    DB_P("checkbox menu\n");
  else if(obj == bottomCodeDisplay)
    DB_P("text menu\n");
  else if(obj == m_rightWinPanel)
    DB_P("button menu\n");


  if(obj == leftArgInput_1)
    { 
      int argx = leftArgInput_1->GetPosition().x;
      int argy = leftArgInput_1->GetPosition().y;
      
      DB_P("Menu: X %d  Y %d\ninput: X %d  Y %d\nsizer: X %d  Y %d\nwindow: X %d  Y %d\n\n",
	   x  - winx,
	   y  - winy,
	   argx, argy,
	   0, 0,
	   winx, winy
	   // newx, newy
	   );

      // let the GUI handle the menu placement itself - it looks *much* better
      leftArgInput_1->PopupMenu(leftClauseTypeMenu);
      //leftArgInput_1->PopupMenu(leftClauseTypeMenu, 
      //			      argx,
      //			      argy);
    }

  if(obj == leftStopExpr)
    {
      DB_P("stop expr context menu\n");

      leftArgInput_1->PopupMenu(leftStopExprMenu);
    }

  if(obj == leftTreeCtrl)
    {
      DB_P("tree ctrl context menu\n");

      leftTreeCtrl->PopupMenu(leftTreeMenu);
    }

}


void LaunchPADS::OnSize(wxSizeEvent &event)
{

  //m_midWin->SetDefaultSize(wxSize(200, event.GetSize().GetHeight()));

  wxLayoutAlgorithm layout;
  layout.LayoutFrame(this);
}

void LaunchPADS::OnOpenFile(wxCommandEvent &event)
{
  LoadState(0, false);
  
  // another defunct test function

  /*  wxFileDialog openFileDialog (this, 
			       "Open file",
			       "",
			       "",
			       FILETYPES,
			       wxOPEN,
			       wxDefaultPosition);
  */
}

void LaunchPADS::OnSaveFile(wxCommandEvent &event)
{
  if(savePathSet)
    {
      SaveState(0, false);
    }
  else
    {
      SaveStateAs(0, false);
    }
}

void LaunchPADS::OnSaveAsFile(wxCommandEvent &event)
{
  SaveStateAs(0, false);
}

void LaunchPADS::OnAbout(wxCommandEvent &event)
{
  wxString t = TITLE;

  t.append( _T("\nMD 2005"));

  wxMessageDialog aboutDialog(this, t, "About LaunchPADS", wxOK);
  aboutDialog.ShowModal();
}

void LaunchPADS::OnExit(wxCommandEvent &event)
{
  Close(TRUE);
}


// resize the sashes, as described in the WX sample sashtest code
void LaunchPADS::OnSashDrag(wxSashEvent& event)
{
  if(event.GetDragStatus() == wxSASH_STATUS_OUT_OF_RANGE)
    return;
 
  switch(event.GetId())
    {

    case ID_WINDOW_BOTTOM:
      {
	m_bottomWin->SetDefaultSize(wxSize(BOTTOM_WIN_DEFAULT_X, 
					   event.GetDragRect().height));
	bottomWinY = event.GetDragRect().height;
	break;
      }
    case ID_WINDOW_TOP:
      {
	m_topWin->SetDefaultSize(wxSize(TOP_WIN_DEFAULT_X, 
					event.GetDragRect().height));
	topWinY = event.GetDragRect().height;
	break;
      }
    case ID_WINDOW_LEFT:
      {
	m_leftWin->SetDefaultSize(wxSize(event.GetDragRect().width, 
					 LEFT_WIN_DEFAULT_Y));
	leftWinX = event.GetDragRect().width;
	break;
      }
    case ID_WINDOW_MID:
      {
	m_midWin->SetDefaultSize(wxSize(MID_WIN_DEFAULT_X, 
					event.GetDragRect().height));
	midWinY = event.GetDragRect().height;
	break;
      } 
    case ID_WINDOW_RIGHT:
      {
	m_rightWin->SetDefaultSize(wxSize(event.GetDragRect().width, 
					  RIGHT_WIN_DEFAULT_Y));
	rightWinX = event.GetDragRect().width;
	break;
      }
    default:
      return;
    }
 
  wxLayoutAlgorithm layout;
  layout.LayoutFrame(this);    
  this->Refresh();
}


// allow the user to show/hide frames
void LaunchPADS::OnWinSwap(wxCommandEvent &event)
{
  if(leftTreeControlState)
    {
        wxMessageBox( _T("Only the tree view may be active\nwhen tree controls are visible."), 
		      _T("Control State Error"),
		      wxICON_EXCLAMATION | wxOK);
	return;
    }


  switch(event.GetId())
    {
    case WIN_SWAP_TOP:
      {
	if(m_topWin->IsShown())
	  { m_topWin->Show(false);   }
	else
	  { m_topWin->Show(true);    }
	break;
      }
    case WIN_SWAP_MID:
      {
	if(m_midWin->IsShown())
	  { m_midWin->Show(false);   }
	else
	  { m_midWin->Show(true);    }
	break;
      }
    case WIN_SWAP_LEFT:
      {
	if(m_leftWin->IsShown())
	  { m_leftWin->Show(false);  }
	else
	  { m_leftWin->Show(true);   }
	break;
      }
    case WIN_SWAP_RIGHT:
      {
	if(m_rightWin->IsShown())
	  { m_rightWin->Show(false); }
	else
	  { m_rightWin->Show(true);  }
	break;
      }
    case WIN_SWAP_BOTTOM:
      {
	if(m_bottomWin->IsShown())
	  { m_bottomWin->Show(false);}
	else
	  { m_bottomWin->Show(true); }
	break;
      }
    }
  wxLayoutAlgorithm layout;
  layout.LayoutFrame(this);
  return;
}


// hide all but one sashframe
void LaunchPADS::OnWinExclusive  (wxCommandEvent &event)
{
  if(leftTreeControlState)
    {
        wxMessageBox( _T("Only the tree view may be active\nwhen tree controls are visible."), 
		      _T("Controls State Error"),
		      wxICON_EXCLAMATION | wxOK);
	return;
    }

  switch(event.GetId())
    {
    case WIN_EXCLUSIVE_TOP:
      {
	m_topWin->Show(true);
	m_midWin->Show(false);
	m_leftWin->Show(false);
	m_rightWin->Show(false);
	m_bottomWin->Show(false);
	break;
      }
    case WIN_EXCLUSIVE_MID:
      {
	m_topWin->Show(false);
	m_midWin->Show(true);
	m_leftWin->Show(false);
	m_rightWin->Show(false);
	m_bottomWin->Show(false);
	break;
      }
    case WIN_EXCLUSIVE_LEFT:
      {
	m_topWin->Show(false);
	m_midWin->Show(false);
	m_leftWin->Show(true);
	m_rightWin->Show(false);
	m_bottomWin->Show(false);
	break;
      }
    case WIN_EXCLUSIVE_RIGHT:
      {
	m_topWin->Show(false);
	m_midWin->Show(false);
	m_leftWin->Show(false);
	m_rightWin->Show(true);
	m_bottomWin->Show(false);
	break;
      }
    case WIN_EXCLUSIVE_BOTTOM:
      {
	m_topWin->Show(false);
	m_midWin->Show(false);
	m_leftWin->Show(false);
	m_rightWin->Show(false);
	m_bottomWin->Show(true);
	break;
      }
    default:
      break;
    }
  wxLayoutAlgorithm layout;
  layout.LayoutFrame(this);
  return;
}


// record which key the user chose as a prototype
void LaunchPADS::OnChangeCheck(wxCommandEvent &event)
{
  if(topCheckedEntry != -1)
    {
      topCheckList->Check(topCheckedEntry, false);
    }
  topCheckedEntry = event.GetInt();
}


// set sash positions to default
void LaunchPADS::OnViewReset(wxCommandEvent &event)
{

  LeftEnableControls(false);
  leftTreeControlState = false;

  m_leftWin->SetDefaultSize(wxSize(LEFT_WIN_DEFAULT_X,
				   LEFT_WIN_DEFAULT_Y)); 

  m_topWin->SetDefaultSize(wxSize(TOP_WIN_DEFAULT_X,
				   TOP_WIN_DEFAULT_Y)); 
  
  m_rightWin->SetDefaultSize(wxSize(RIGHT_WIN_DEFAULT_X,
				   RIGHT_WIN_DEFAULT_Y)); 
  
  m_midWin->SetDefaultSize(wxSize(MID_WIN_DEFAULT_X,
				   MID_WIN_DEFAULT_Y)); 
  
  m_bottomWin->SetDefaultSize(wxSize(BOTTOM_WIN_DEFAULT_X,
				   BOTTOM_WIN_DEFAULT_Y)); 

  m_leftWin->Show(true);
  m_topWin->Show(true);
  m_rightWin->Show(true);
  m_midWin->Show(true);
  m_bottomWin->Show(true);

  topWinX = TOP_WIN_DEFAULT_X;
  topWinY = TOP_WIN_DEFAULT_Y;
  midWinX = MID_WIN_DEFAULT_X;
  midWinY = MID_WIN_DEFAULT_Y;
  leftWinX = LEFT_WIN_DEFAULT_X;
  leftWinY = LEFT_WIN_DEFAULT_Y;
  rightWinX = RIGHT_WIN_DEFAULT_X;
  rightWinY = RIGHT_WIN_DEFAULT_Y;
  bottomWinX = BOTTOM_WIN_DEFAULT_X;
  bottomWinY = BOTTOM_WIN_DEFAULT_Y;
  
  wxLayoutAlgorithm layout;
  layout.LayoutFrame(this);    
}

void LaunchPADS::OnToolMenu(wxCommandEvent &event)
{
  int id = event.GetId();
  wxArrayString cmds;

  if(id == TOOLS_RECORD_DEMO)
    {
      wxString str = _T("State Recorder");
      int state = 0;
      /*
      StateSequenceDialog ssd(this, 
			      wxID_ANY,
			      str, 
			      this, 
			      SSD_RECORD,
			      &state);
      */
      wxFileDialog saveFileDialog(this, 
				  "Save state sequence to XML",
				  "",
				  "",
				  STATE_FILETYPES,
				  wxSAVE |
				  wxOVERWRITE_PROMPT,
				  wxDefaultPosition);

      if(saveFileDialog.ShowModal() != wxID_OK)
	{	  
	  return;
	}
      savePath = saveFileDialog.GetPath();
      savePathSet = true;

      StateSequenceDialog *ssd = new StateSequenceDialog(this, 
							 wxID_ANY,
							 str, 
							 this, 
							 SSD_RECORD,
							 &state, 
							 xmlNumLoadSteps);

      if(ssd->Show(true))
	{
	  ssd->Raise();
	}
    }
  else if(id == TOOLS_PLAYBACK_DEMO)
    {
      wxString str = _T("State Playback");
      int state = 0;

      wxFileDialog openFileDialog(this, 
				  "Open XML state file",
				  "",
				  "",
				  STATE_FILETYPES,
				  wxOPEN | 
				  wxFILE_MUST_EXIST,	
				  wxDefaultPosition);

      if(openFileDialog.ShowModal() != wxID_OK)
	{
	  DB_P("xml state playback: show modal != wxOK\n");
	  return;
	}
      if(!wxFile::Access(openFileDialog.GetPath(), wxFile::read))
	{
	  wxMessageBox(_T("Selected XML state file could not be opened!"), 
		       _T("Could Not Open XML State"),
		       wxICON_ERROR | wxCANCEL);
	  return;
	}
      loadPath = openFileDialog.GetPath();
      loadPathSet = true;

      if(!GetLoadStateNumSteps())
	return;      
    
      StateSequenceDialog *ssd = new StateSequenceDialog(this, 
							 wxID_ANY,
							 str, 
							 this, 
							 SSD_PLAYBACK,
							 &state, 
							 xmlNumLoadSteps);
      if(ssd->Show(true))
	{
	  ssd->Raise();
	}
    }
  switch(id)
    {
    case TOOLS_PADSC:
      ExecPADSC(false);
      break;
    case TOOLS_ACCUM:
      ToolProcessAcc(TP_FROM_MENU);
      break;
    case TOOLS_TOFMT:
      ToolProcessFmt(TP_FROM_MENU);
      break;
    case TOOLS_TOXML:
      ToolProcessXml(TP_FROM_MENU);
      break;
    }

  // cc test functions
  /* 
  cmds.Add(_T("sleep 1"));
  cmds.Add(_T("ls /home/mdaly/"));
  ShellDialog shell(this, wxID_ANY, _T("Tools I/O"), this, cmds);
  shell.ShowModal();
  */
  return;
}

void LaunchPADS::OnToolbarPress(wxCommandEvent &event)
{
  int id = event.GetId();
  
  DB_P("event == %d\n", id);
  switch(id)
    {
    case TOOLBAR_OPEN:
      DB_P("toolbar open\n");
      LoadState(0, false);
      break;
    case TOOLBAR_SAVE:
      DB_P("toolbar save\n");
      SaveState(0, false);
      break;
    case TOOLBAR_SAVEAS:
      DB_P("toolbar save\n");
      SaveStateAs(0, false);
      break;
    case TOOLBAR_PADS_MANUAL:
      DB_P("toolbar pads manual\n");
      if(!wxLaunchDefaultBrowser(_T(PADS_MANUAL_URI)))
	{
	  wxMessageBox( _T("Please visit \n<http://www.padsproj.org/doc/index.html>\n to view the \
most recent revision of the PADS manual."), 
		    _T("Could Not Open Default Browser"),
		    wxICON_EXCLAMATION | wxCANCEL);
	  toolBar->EnableTool(TOOLBAR_PADS_MANUAL, 0);
	}
      break;
    case TOOLBAR_HELP:
      DB_P("toolbar help\n");
      break;
    case TOOLBAR_LAUNCH:
      DB_P("toolbar launch\n");
      ExecPADSC(true);
      //ExecCCTest();
      break;
    case TOOLBAR_XML:
      DB_P("toolbar xml\n");
      ToolProcessXml(TP_FROM_TOOLBAR);
      break;
    case TOOLBAR_FMT:
      DB_P("toolbar format\n");
      ToolProcessFmt(TP_FROM_TOOLBAR);
      break;
    case TOOLBAR_ACCUM:
      DB_P("toolbar accum\n");
      ToolProcessAcc(TP_FROM_TOOLBAR);
      break;
    case TOOLBAR_PADX:
      DB_P("toolbar padx\n");
      if(true)
	{
	  wxArrayString arraystr;
	  PADXDialog *padxD = new PADXDialog(this, 
					     wxID_ANY, 
					     _T("PADX Dialog"),
					     this,
					     arraystr);
	  int padxreturn;
	  padxreturn = padxD->ShowModal();
	  padxD->~PADXDialog();
	}
      break;
    case TOOLBAR_SUM:
      DB_P("toolbar sum\n");
      break;
    case TOOLBAR_FILTER:
      DB_P("toolbar filter\n");
      break;
    case TOOLBAR_LOADPXML:
      {
	DB_P("load P-XML file\n");
	wxFileDialog openFileDialog(this, 
				    "Open PXML file",
				    "",
				    "",
				    XML_FILETYPES,
				    wxOPEN | 
				    wxFILE_MUST_EXIST,	
				    wxDefaultPosition);

	if(openFileDialog.ShowModal() != wxID_OK)
	  {
	    DB_P("pxml: show modal != wxOK\n");
	    return;
	  }
	DB_P("openFileDialog returned with wxID_OK with '%s'\n", openFileDialog.GetPath().c_str());
	DB_P("trying wxFile: %d\n", wxFile::Access(openFileDialog.GetPath(), wxFile::read));
	if(!wxFile::Access(openFileDialog.GetPath(), wxFile::read))
	  {
	    wxMessageBox(_T("Selected XML state file could not be opened!"), 
			 _T("Could Not Open XML State"),
			 wxICON_ERROR | wxCANCEL);
	    return;
	  }
	wxString xmldefpath;
	DB_P("file access worked okay, moving on to set pXMLDefPath\n");
	xmldefpath = openFileDialog.GetPath();
	DB_P("calling LoadPXMLFile %s\n", pXMLDefPath.c_str());
	LoadPXMLFile(xmldefpath);
      }
      break;
    case TOOLBAR_SAVEPXML:
      {
	DB_P("save P-XML file\n");
	wxFileDialog saveFileDialog(this, 
				    "Save state sequence to XML",
				    "",
				    "",
				    XML_FILETYPES,
				    wxSAVE |
				    wxOVERWRITE_PROMPT,
				    wxDefaultPosition);

	if(saveFileDialog.ShowModal() != wxID_OK)
	  {
	    DB_P("pxml: show modal != wxOK\n");
	    return;
	  }
	DB_P("saveFileDialog returned with wxID_OK with '%s'\n", saveFileDialog.GetPath().c_str());
	//DB_P("trying wxFile: %d\n", wxFile::Access(saveFileDialog.GetPath(), wxFile::read));

	wxString xmldefpath;
	DB_P("file access worked okay, moving on to set pXMLDefPath\n");
	xmldefpath = saveFileDialog.GetPath();
	DB_P("calling SavePXMLFile %s\n", pXMLDefPath.c_str());
	SavePXMLFile(xmldefpath);
      }
      break;
    }
     
  return;
}


void LaunchPADS::OnPrefsDialog(wxCommandEvent &event)
{
  PrefsDialog pref(this, wxID_ANY, _T("Preferences"), this);
  pref.ShowModal();
  return;
}


/* ************************************ */


void LaunchPADS::SetDefaultSettings(void)
{  
  wxString padscpath("${HOME}/pads/");
  wxString ccpath("cc");
  wxString ccargs("-Wall");
  wxString padscinc("${HOME}/pads/padsc/include/");
  wxString astinc("${HOME}/pads/ast-ast/arch/linux.i386/");
  wxString padxpath("");

  PADSCcomppath = PADSCpath;
  PADSCcomppath.Append(_T(PADS_PACKAGE_PATH_PADSC));
  PADSCinc = PADSCpath;
  PADSCinc.Append(_T(PADS_PACKAGE_PATH_INC));
  ASTincludedir = ASTinc;
  ASTincludedir.Append(_T(PADS_PACKAGE_PATH_ASTINC));
  LIBdir = ASTinc;
  LIBdir.Append(_T(PADS_PACKAGE_PATH_ASTLIB));

  ChangeSettings(
		 false,             //  saveLayoutOnExit
		 true,              //  colourModeOn
		 true,              //  midGridColNumberLabels
		 TOP_NUM_LINES_MAX, //  topNumLinesOnImport
		 true,              //  midUDefErrorMBoxOn
		 true,              //  midTermErrorMBoxOn
		 true,              //  rightBuildTreeMBoxOn  
		 true,              //  leftTreeBackgroundColourOn
		 true,              //  leftControlsWarnings
		 true,              //  leftAttributeChangeWarnings
		 true,              //  midGridWriteTypeOnNext;
		 false,             //  leftTreeTextColourOn
		 false,             //  xmlMessagesOnSave
		 padscpath,
		 padscinc,
		 astinc,
		 ccpath,
		 ccargs,
		 false,             // leftControlSafeMode
		 padxpath
		 );
  
  bottomTermColour.Set(BOTTOM_TERM_COLOUR);
  bottomNontermColour.Set( BOTTOM_NONTERM_COLOUR);
  bottomLitColour.Set( BOTTOM_LIT_COLOUR);
  bottomErrorColour.Set( BOTTOM_ERROR_COLOUR);
  bottomOtherColour.Set( BOTTOM_OTHER_COLOUR);
  
  topWinX = TOP_WIN_DEFAULT_X;
  topWinY = TOP_WIN_DEFAULT_Y;
  midWinX = MID_WIN_DEFAULT_X;
  midWinY = MID_WIN_DEFAULT_Y;
  leftWinX = LEFT_WIN_DEFAULT_X;
  leftWinY = LEFT_WIN_DEFAULT_Y;
  rightWinX = RIGHT_WIN_DEFAULT_X;
  rightWinY = RIGHT_WIN_DEFAULT_Y;
  bottomWinX = BOTTOM_WIN_DEFAULT_X;
  bottomWinY = BOTTOM_WIN_DEFAULT_Y;

  leftTreeBackgroundColourOn = true;
  leftTreeTextColourOn       = false;
}

void LaunchPADS::ChangeSettings(
 bool saveLayoutOnExit_new,
 bool colourModeOn_new,
 bool midGridColNumberLabels_new,
 int  topNumLinesOnImport_new,
 
 bool midUDefErrorMBoxOn_new,
 bool midTermErrorMBoxOn_new,
 
 bool rightBuildTreeMBoxOn_new,
 
 bool leftTreeBackgroundColourOn_new,
 bool leftControlsWarnings_new,
 bool leftAttributeChangeWarnings_new, 
 bool midGridWriteTypeOnNext_new, 
 bool leftTreeTextColourOn_new,
 bool xmlMessagesOnSave_new, 
 wxString &PADSCpath_new,
 wxString &PADSCinc_new,
 wxString &ASTinc_new,
 wxString &CCpath_new,
 wxString &CCargs_new, 
 bool leftControlSafeMode_new,
 wxString &PADXPath_new)
{
  saveLayoutOnExit = saveLayoutOnExit_new;
  
  colourModeOn = colourModeOn_new;

  midGridColNumberLabels = midGridColNumberLabels_new;

  topNumLinesOnImport = topNumLinesOnImport_new;

  midUDefErrorMBoxOn = midUDefErrorMBoxOn_new;
  midTermErrorMBoxOn = midTermErrorMBoxOn_new;

  rightBuildTreeMBoxOn = rightBuildTreeMBoxOn_new;

  leftTreeBackgroundColourOn = leftTreeBackgroundColourOn_new;
  leftControlsWarnings = leftControlsWarnings_new;
  leftControlSafeMode = leftControlSafeMode_new;
  leftAttributeChangeWarnings = leftAttributeChangeWarnings_new;
  leftTreeTextColourOn = leftTreeTextColourOn_new;

  midGridWriteTypeOnNext = midGridWriteTypeOnNext_new;

  xmlMessagesOnSave = xmlMessagesOnSave_new;

  PADSCpath = PADSCpath_new;
  if(PADSCpath.Last() != '/')
    PADSCpath.Append('/');
  // PADSCinc = PADSCinc_new;
  ASTinc = ASTinc_new;
  if(ASTinc.Last() != '/')
    ASTinc.Append('/');
  CCpath = CCpath_new;
  CCargs = CCargs_new;
  PADXPath = PADXPath_new;

  PADSCcomppath = PADSCpath;
  PADSCcomppath.Append(_T(PADS_PACKAGE_PATH_PADSC));
  PADSCinc = PADSCpath;
  PADSCinc.Append(_T(PADS_PACKAGE_PATH_INC));
  ASTincludedir = ASTinc;
  ASTincludedir.Append(_T(PADS_PACKAGE_PATH_ASTINC));
  LIBdir = ASTinc;
  LIBdir.Append(_T(PADS_PACKAGE_PATH_ASTLIB));

  DB_P("new cc path: %s\n", CCpath.c_str());

  if(colourModeOn)
    {
      midNoneColour.Set(MID_MODE_NONE_COLOUR);
      midDelimColour.Set(MID_MODE_DELIM_COLOUR);
      midTermColour.Set(MID_MODE_TERM_COLOUR);
      midUDefColour.Set(MID_MODE_UDEF_COLOUR);
    }
  else   
    {
      leftTreeBackgroundColourOn = false;
      leftTreeTextColourOn = false;

      midNoneColour.Set(MID_MODE_NONE_NO_COLOUR);
      midDelimColour.Set(MID_MODE_DELIM_NO_COLOUR);
      midTermColour.Set(MID_MODE_TERM_NO_COLOUR);
      midUDefColour.Set(MID_MODE_UDEF_NO_COLOUR);
    }
}

void LaunchPADS::GetSettings(
 bool &saveLayoutOnExit_new,
 bool &colourModeOn_new,
 bool &midGridColNumberLabels_new,
 int  &topNumLinesOnImport_new,
 
 bool &midUDefErrorMBoxOn_new,
 bool &midTermErrorMBoxOn_new,
 
 bool &rightBuildTreeMBoxOn_new,
 
 bool &leftTreeBackgroundColourOn_new,
 bool &leftControlsWarnings_new,
 bool &leftAttributeChangeWarnings_new, 
 bool &midGridWriteTypeOnNext_new, 
 bool &leftTreeTextColourOn_new, 
 bool &xmlMessagesOnSave_new,
 wxString &PADSCpath_new,
 wxString &PADSCinc_new,
 wxString &ASTinc_new,
 wxString &CCpath_new,
 wxString &CCargs_new, 
 bool &leftControlSafeMode_new,
 wxString &PADXPath_new)
{
  
  saveLayoutOnExit_new = saveLayoutOnExit;

  colourModeOn_new = colourModeOn;

  midGridColNumberLabels_new = midGridColNumberLabels;

  topNumLinesOnImport_new = topNumLinesOnImport;

  midUDefErrorMBoxOn_new = midUDefErrorMBoxOn;
  midTermErrorMBoxOn_new = midTermErrorMBoxOn;

  rightBuildTreeMBoxOn_new = rightBuildTreeMBoxOn;

  leftTreeBackgroundColourOn_new  = leftTreeBackgroundColourOn;
  leftControlsWarnings_new        = leftControlsWarnings;
  leftControlSafeMode_new         = leftControlSafeMode;
  leftAttributeChangeWarnings_new = leftAttributeChangeWarnings;

  midGridWriteTypeOnNext_new = midGridWriteTypeOnNext;

  leftTreeTextColourOn_new = leftTreeTextColourOn;

  xmlMessagesOnSave_new = xmlMessagesOnSave;

  PADSCpath_new = PADSCpath;
  PADSCinc_new = PADSCinc;
  ASTinc_new = ASTinc;
  CCpath_new = CCpath;
  CCargs_new = CCargs;
  PADXPath_new = PADXPath;
}


void LaunchPADS::SaveLayout(void)
{
  fConfig->Write(_T("top/win_X"),    topWinX);
  fConfig->Write(_T("top/win_Y"),    topWinY);
  fConfig->Write(_T("mid/win_X"),    midWinX);
  fConfig->Write(_T("mid/win_Y"),    midWinY);
  fConfig->Write(_T("left/win_X"),   leftWinX);
  fConfig->Write(_T("left/win_Y"),   leftWinY);
  fConfig->Write(_T("right/win_X"),  rightWinX);
  fConfig->Write(_T("right/win_Y"),  rightWinY);
  fConfig->Write(_T("bottom/win_X"), bottomWinX);
  fConfig->Write(_T("bottom/win_Y"), bottomWinY);
}

wxString LaunchPADS::GetPADXPath(void)
{
  return PADXPath;
}
/* ************************************ */
