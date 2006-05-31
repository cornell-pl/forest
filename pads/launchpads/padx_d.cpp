/* **************************************
 * PADS GUI project (preliminary tests)
 * PADX Interface
 * *********************************** */
#include "lp_includes.h"

/* **************************************
 * Functions:
 *  --class constructor/destructor
 *
 *  --event handlers
 *
 *  --interface
 *
 * *********************************** */

IMPLEMENT_CLASS(PADXDialog, wxDialog)

BEGIN_EVENT_TABLE(PADXDialog, wxDialog)
  EVT_SIZE (PADXDialog::OnSize)

  EVT_BUTTON(PXD_EXECUTE,     PADXDialog::OnUserExec)
  EVT_BUTTON(PXD_SAVE,        PADXDialog::OnUserSave)
  EVT_BUTTON(PXD_QUERYLOAD,   PADXDialog::OnUserLoadQueries)
  EVT_BUTTON(PXD_QUERYCLEAR,  PADXDialog::OnUserClearQueries)
  EVT_BUTTON(PXD_SOURCELOAD,  PADXDialog::OnUserLoadSources)
  EVT_BUTTON(PXD_SOURCECLEAR, PADXDialog::OnUserClearSources)

EVT_TEXT(PXD_QUERY_CHANGE,    PADXDialog::OnUserChangeText)

  EVT_CHECKLISTBOX(PXD_QUERYLIST,  PADXDialog::OnUserQueryCheck)
  EVT_CHECKLISTBOX(PXD_SOURCELIST, PADXDialog::OnUserSourceCheck)
END_EVENT_TABLE()


PADXDialog::PADXDialog(wxWindow* par_win, wxWindowID id,
			 const wxString& title, 
			 LaunchPADS* par, 
			 wxArrayString &execCmd)
  // taken from the wxPropertySheet documentation
  : wxDialog(par_win, id, title, 
	     wxDefaultPosition,
	     wxSize(975, 775),
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
    
    //pxdQuerySourceSizer = new wxGridBagSizer(4, 4);
    pxdMainSizer = new wxGridBagSizer(8, 8);

    pxdOutputSizer = new wxStaticBoxSizer(wxHORIZONTAL, this,  _T("Query Results"));

    pxdExecutableSizer = new wxStaticBoxSizer(wxVERTICAL, this, _T("Executable Path"));

    pxdTextQuerySizer = new wxStaticBoxSizer(wxHORIZONTAL, this, _T("Active Query"));
    pxdTextButtonSizer = new wxBoxSizer(wxVERTICAL);

    pxdQueryControlSizer = new wxStaticBoxSizer(wxVERTICAL, this, _T("Query Selection"));
    pxdSourceControlSizer = new wxStaticBoxSizer(wxVERTICAL, this, _T("Data Source Selection"));

    pxdQueryButtonSizer = new wxBoxSizer(wxHORIZONTAL);
    pxdSourceButtonSizer = new wxBoxSizer(wxHORIZONTAL);


    pxdExecutable = new wxTextCtrl(this,
				   wxID_ANY,
				   _T(""),
				   wxDefaultPosition,
				   wxSize(400, 20),
				   wxTE_DONTWRAP
				   | wxHSCROLL);

    pxdQuery = new wxTextCtrl(this, 
			      PXD_QUERY_CHANGE, 
			      _T(""),
			      wxDefaultPosition,
			      wxSize(400, 200),
			        wxTE_MULTILINE
			      | wxTE_RICH
			      | wxTE_BESTWRAP);

    pxdOutput = new wxTextCtrl(this, 
			       wxID_ANY, 
			       _T(""),
			       wxDefaultPosition,
		 	       wxSize(400, 200),
			       wxTE_READONLY
			       | wxTE_MULTILINE
			       | wxTE_RICH
			       | wxTE_BESTWRAP);
    
    /*    wxFont pxdFont(10,
		  wxFONTFAMILY_TELETYPE,
		  wxFONTSTYLE_NORMAL,
		  wxFONTWEIGHT_NORMAL);
    if(pxdFont.Ok())
      pxdOutput->SetFont(pxdFont);
    */

    pxdExec = new wxButton(this, PXD_EXECUTE, _T("&Run Query"));
    pxdSave = new wxButton(this, PXD_SAVE, _T("&Save"));;
    pxdExit = new wxButton(this, wxID_CANCEL, _T("E&xit"));
    pxdLoadQueries  = new wxButton(this, PXD_QUERYLOAD, _T("Load"));
    pxdClearQueries = new wxButton(this, PXD_QUERYCLEAR, _T("Clear"));
    pxdLoadSources  = new wxButton(this, PXD_SOURCELOAD, _T("Load"));
    pxdClearSources = new wxButton(this, PXD_SOURCECLEAR, _T("Clear"));

    pxdQueryList = new wxCheckListBox(this, PXD_QUERYLIST, wxDefaultPosition, wxSize(400, 200), 0, NULL, wxLB_HSCROLL | wxLB_SINGLE);
    pxdSourceList = new wxCheckListBox(this, PXD_SOURCELIST, wxDefaultPosition, wxSize(400, 200), 0, NULL, wxLB_HSCROLL | wxLB_SINGLE);

    pxdExecutableSizer->Add(pxdExecutable, 0, wxEXPAND | wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 0);

    pxdTextButtonSizer->Add(pxdExec, 1, wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 0);
    pxdTextButtonSizer->Add(pxdSave, 1, wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 0);
    pxdTextButtonSizer->Add(pxdExit, 1, wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 0);

    pxdTextQuerySizer->Add(pxdQuery, 1, wxEXPAND | wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 0);
    pxdTextQuerySizer->Add(pxdTextButtonSizer, 0, wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 0);

    pxdQueryButtonSizer->Add(pxdLoadQueries,  0, wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 0);
    pxdQueryButtonSizer->Add(pxdClearQueries, 0, wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 0);

    pxdSourceButtonSizer->Add(pxdLoadSources,  0, wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 0);
    pxdSourceButtonSizer->Add(pxdClearSources, 0, wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 0);

    pxdQueryControlSizer->Add(pxdQueryList, 1, wxEXPAND | wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 0);
    pxdQueryControlSizer->Add(pxdQueryButtonSizer, 0, wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 0);

    pxdSourceControlSizer->Add(pxdSourceList, 1, wxEXPAND | wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 0);
    pxdSourceControlSizer->Add(pxdSourceButtonSizer, 0, wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 0);

    pxdOutputSizer->Add(pxdOutput, 1, wxEXPAND | wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 0);
    
    //pxdQuerySourceSizer->Add(pxdQueryControlSizer, wxGBPosition(0, 0), wxDefaultSpan);
    //pxdQuerySourceSizer->Add(pxdSourceControlSizer, wxGBPosition(0, 1), wxDefaultSpan);
 
    //pxdMainSizer->Add(pxdQuery, wxGBPosition(0, 0), wxDefaultSpan, wxEXPAND | wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER);
    //pxdMainSizer->Add(pxdTextButtonSizer, wxGBPosition(0, 1), wxDefaultSpan, wxEXPAND | wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER);
    pxdMainSizer->Add(pxdExecutableSizer, wxGBPosition(0, 0), wxGBSpan(1, 2), wxEXPAND | wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER);
    pxdMainSizer->Add(pxdTextQuerySizer, wxGBPosition(1, 0), wxGBSpan(1, 2), wxEXPAND | wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER);
    pxdMainSizer->Add(pxdQueryControlSizer, wxGBPosition(2, 0), wxDefaultSpan, wxEXPAND | wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER);
    pxdMainSizer->Add(pxdSourceControlSizer, wxGBPosition(2, 1), wxDefaultSpan, wxEXPAND | wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER);
    //pxdMainSizer->Add(pxdTextQuerySizer, wxGBPosition(0, 0), wxDefaultSpan, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER);
    //pxdMainSizer->Add(pxdQuerySourceSizer, wxGBPosition(1, 0), wxGBSpan(1, 2), wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER);
    //pxdMainSizer->Add(pxdOutput, wxGBPosition(2, 0), wxGBSpan(1, 2), wxEXPAND | wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER);
    pxdMainSizer->Add(pxdOutputSizer, wxGBPosition(3, 0), wxGBSpan(1, 2), wxEXPAND | wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER);
    //pxdMainSizer->Add(pxdTextQuerySizer, 0, wxEXPAND | wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 0);
    //pxdMainSizer->Add(pxdQuerySourceSizer, 0, wxEXPAND | wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 0);
    //pxdMainSizer->Add(pxdOutputSizer, 0, wxEXPAND | wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 0);

    this->SetSizer(pxdMainSizer);
    pxdMainSizer->Fit(this);

    wxLayoutAlgorithm layout;
    layout.LayoutWindow(this);

    cmds.Clear();

    queryChecked = -1;
    sourceChecked = -1;

    textChanged = false;

    nl.Printf(_T("\n")); // newline for text controls - must be \n regardless of OS

    sourcePath = "";
    queryPath  = "";
    execPath   = "";

    lastQueryDir = "./";
    lastSourceDir = "./";
  }

PADXDialog::~PADXDialog()
{
  pxdExec->Destroy();
  pxdSave->Destroy();
  pxdExit->Destroy();
  pxdLoadQueries->Destroy();
  pxdClearQueries->Destroy();
  pxdLoadSources->Destroy();
  pxdClearSources->Destroy();
   
  pxdExecutable->Destroy();
  pxdQuery->Destroy();
  pxdOutput->Destroy();

  pxdQueryList->Destroy();
  pxdSourceList->Destroy();

  pxdExecutableSizer->~wxStaticBoxSizer();
  pxdTextButtonSizer->~wxBoxSizer();
  pxdTextQuerySizer->~wxStaticBoxSizer();
  pxdQueryButtonSizer->~wxBoxSizer();
  pxdSourceButtonSizer->~wxBoxSizer();
  pxdQueryControlSizer->~wxStaticBoxSizer();
  pxdSourceControlSizer->~wxStaticBoxSizer();
  pxdOutputSizer->~wxStaticBoxSizer();

  //pxdQuerySourceSizer->~wxGridBagSizer();
  pxdMainSizer->~wxGridBagSizer();

}

/* ************* */
void PADXDialog::OnSize(wxSizeEvent &event)
{
  wxString printThis;
  wxSize mySize;
  mySize = this->GetSize();
  printThis.Printf(_T("PADX_D resizing to %d %d\n"), mySize.GetWidth(), mySize.GetHeight());
  DB_P(printThis.c_str());
  wxLayoutAlgorithm layout;
  layout.LayoutWindow(this); 
  pxdMainSizer->RecalcSizes();
  pxdMainSizer->Fit(this);
  mySize = pxdMainSizer->GetSize();
  printThis.Printf(_T("PADX_D sizer dimensions: %d %d\n"), mySize.GetWidth(), mySize.GetHeight());
  DB_P(printThis.c_str());
  this->Refresh(); 
}

void PADXDialog::OnUserExec(wxCommandEvent &event)
{
  pxdDisableButtons();
  pxdExecute();
  pxdEnableButtons();
}

void PADXDialog::OnUserSave(wxCommandEvent &event)
{
  int returnVal;
  wxString fPath;
  wxString queryData;
  wxFFile saveFile;
  wxFileDialog *saveQueryD = new wxFileDialog(this,
					      _T("Save XQuery As..."),
					      lastQueryDir,
					      _T(""),
					      _T("XQuery Files (*.xq)|*.xq|All Files|*"),
					      wxSAVE | wxOVERWRITE_PROMPT);
  
  returnVal = saveQueryD->ShowModal();
  fPath = saveQueryD->GetPath();
  queryData = pxdQuery->GetValue();
  saveQueryD->~wxFileDialog();


  if(returnVal == wxID_CANCEL)
      return;

  if(!saveFile.Open(fPath, "w"))
    {
      wxLogError(_T("Could not save to selected file:\n%s"), fPath.c_str());
      return;
    }
  saveFile.Write(queryData);
  saveFile.Close();
  return;
}

void PADXDialog::OnUserLoadQueries(wxCommandEvent &event)
{
  int returnVal;
  wxArrayString fPaths;
  wxString thisPath;
  int numPaths;
  wxFileDialog *loadQueryD = new wxFileDialog(this,
					      _T("Load XQueries"),
					      lastQueryDir,
					      _T(""),
					      _T("XQuery Files (*.xq)|*.xq|All Files|*"),
					      wxOPEN | wxMULTIPLE);
  fPaths.clear();
  returnVal = loadQueryD->ShowModal();
  loadQueryD->GetPaths(fPaths);
  lastQueryDir = loadQueryD->GetDirectory();
  DB_P("New Last Query Dir: ");
  DB_P(lastQueryDir.c_str());
  DB_P("\n");
  
  loadQueryD->~wxFileDialog();

  if(returnVal == wxID_CANCEL || fPaths.GetCount() == 0)
      return;
 
  pxdQueryList->Append(fPaths);
  return;
}

void PADXDialog::OnUserClearQueries(wxCommandEvent &event)
{
  if(wxMessageBox(_T("This will clear all entries in the query list.\nContinue?"), 
		  _T("Clear Query List?"),
		  wxICON_QUESTION | wxYES_NO) == wxNO)
    {
      return;
    }
  pxdQueryList->Clear();
  return;
}

void PADXDialog::OnUserLoadSources(wxCommandEvent &event)
{
  int returnVal;
  wxArrayString fPaths;
  wxString thisPath;
  int numPaths;
  wxFileDialog *loadSourceD = new wxFileDialog(this,
					      _T("Load Data Sources"),
					      lastSourceDir,
					      _T(""),
					      _T("All Files|*"),
					      wxOPEN | wxMULTIPLE);
  fPaths.clear();
  returnVal = loadSourceD->ShowModal();
  loadSourceD->GetPaths(fPaths);
  lastSourceDir = loadSourceD->GetDirectory();
  DB_P("New Last Source Dir: ");
  DB_P(lastSourceDir.c_str());
  DB_P("\n");

  loadSourceD->~wxFileDialog();

  if(returnVal == wxID_CANCEL || fPaths.GetCount() == 0)
      return;
 
  pxdSourceList->Append(fPaths);
  return;
}

void PADXDialog::OnUserClearSources(wxCommandEvent &event)
{
  if(wxMessageBox(_T("This will clear all entries in the source list.\nContinue?"), 
		  _T("Clear Source List?"),
		  wxICON_QUESTION | wxYES_NO) == wxNO)
    {
      return;
    }
  pxdSourceList->Clear();
  return;
}

void PADXDialog::OnUserChangeText(wxCommandEvent &event)
{
  textChanged = true;
}

void PADXDialog::OnUserQueryCheck(wxCommandEvent &event)
{
  if(queryChecked != -1)
    {
      pxdQueryList->Check(queryChecked, false);
    }
  queryChecked = event.GetInt();
  if(pxdQueryList->IsChecked((long)queryChecked))
    pxdSetQuery(pxdQueryList->GetString((long)queryChecked));
  else
    queryChecked = -1;
}

void PADXDialog::OnUserSourceCheck(wxCommandEvent &event)
{
  if(sourceChecked != -1)
    {
      pxdSourceList->Check(sourceChecked, false);
    }
  sourceChecked = event.GetInt();
  if(pxdSourceList->IsChecked((long)sourceChecked))
    pxdSetSource(pxdSourceList->GetString((long)sourceChecked));
  else
    queryChecked = -1;
}

/* ************* */
void PADXDialog::pxdSetQuery(wxString queryFilePath)
{
  wxFFile queryFile;
  wxString queryStr;
  if(!queryFile.Open(queryFilePath, "r"))
    {
      wxLogError(_T("Could not open selected file:\n%s"), queryFilePath.c_str());
     return;
    }
  
  if(!queryFile.ReadAll(&queryStr))
    {
      wxLogError(_T("Could not read data from selected file:\n%s"), queryFilePath.c_str());
      queryFile.Close();
      return;
    }

  queryFile.Close();
  pxdQuery->SetValue(queryStr);
  textChanged = false;
  queryPath = queryFilePath;
}

void PADXDialog::pxdSetSource(wxString sourceFilePath)
{
  wxFile sourceFile;
  wxString sourceStr;
  if(!sourceFile.Exists(sourceFilePath.c_str()))
    {
      wxLogError(_T("Could not open selected file:\n%s"), sourceFilePath.c_str());
      return;
    }
  sourcePath = sourceFilePath;
}

void PADXDialog::pxdAddText(wxString &str)
{
  pxdOutput->AppendText(str);
  if(!(str.Contains(nl)))
    pxdOutput->AppendText(nl);
}

void PADXDialog::pxdAddText(wxArrayString &arrStr)
{
  int numStrings = arrStr.GetCount();
  for(int i = 0; i < numStrings; i++)
    {
      pxdOutput->AppendText(arrStr.Item(i));
      if(!(arrStr.Item(i).Contains(nl)))
	 pxdOutput->AppendText(nl);
    }
}

void PADXDialog::pxdEnableButtons(void)
{
  pxdExec->Enable(true);
  pxdSave->Enable(true);
  pxdExit->Enable(true);
  pxdLoadQueries->Enable(true);
  pxdClearQueries->Enable(true);
  pxdLoadSources->Enable(true);
  pxdClearSources->Enable(true);
   
  pxdQuery->Enable(true);
  pxdOutput->Enable(true);

  pxdQueryList->Enable(true);
  pxdSourceList->Enable(true);

  pxdExec->Refresh();
  pxdSave->Refresh();
  pxdExit->Refresh();
  pxdLoadQueries->Refresh();
  pxdClearQueries->Refresh();
  pxdLoadSources->Refresh();
  pxdClearSources->Refresh();
   
  pxdQuery->Refresh();
  pxdOutput->Refresh();

  pxdQueryList->Refresh();
  pxdSourceList->Refresh();

  Refresh();

}

void PADXDialog::pxdDisableButtons(void)
{

  pxdExec->Enable(false);
  pxdSave->Enable(false);
  pxdExit->Enable(false);
  pxdLoadQueries->Enable(false);
  pxdClearQueries->Enable(false);
  pxdLoadSources->Enable(false);
  pxdClearSources->Enable(false);
   
  pxdQuery->Enable(false);
  pxdOutput->Enable(false);

  pxdQueryList->Enable(false);
  pxdSourceList->Enable(false);

  pxdExec->Refresh();
  pxdSave->Refresh();
  pxdExit->Refresh();
  pxdLoadQueries->Refresh();
  pxdClearQueries->Refresh();
  pxdLoadSources->Refresh();
  pxdClearSources->Refresh();
   
  pxdQuery->Refresh();
  pxdOutput->Refresh();

  pxdQueryList->Refresh();
  pxdSourceList->Refresh();

  Refresh();
}


void PADXDialog::pxdExecute()
{
  int numCmds = cmds.GetCount();
  long returnCode;
  int queryChangeMsgResult;

  wxString str;
  wxArrayString output;
  wxArrayString errors;

  if(textChanged)
    {
      queryChangeMsgResult = wxMessageBox(_T("Query text has been changed.\nSave to new file before use?\n(Query will be saved to cache otherwise.)"),
					  _T("Save Query Before Use?"),
					  wxYES_NO | wxCANCEL | wxICON_QUESTION);

      if(queryChangeMsgResult == wxCANCEL)
	return;
      else if(queryChangeMsgResult == wxYES)
	{
	  int returnVal;
	  wxString fPath;
	  wxString queryData;
	  wxFFile saveFile;
	  wxFileDialog *saveQueryD = new wxFileDialog(this,
						      _T("Save XQuery As..."),
						      _T(""),
						      _T(""),
						      _T("XQuery Files (*.xq)|*.xq|All Files|*"),
						      wxSAVE | wxOVERWRITE_PROMPT);
	  returnVal = saveQueryD->ShowModal();
	  fPath = saveQueryD->GetPath();
	  queryData = pxdQuery->GetValue();
	  saveQueryD->~wxFileDialog();

	  if(returnVal != wxID_CANCEL)
	    {
	      if(!saveFile.Open(fPath, "w"))
		{
		  wxLogError(_T("Could not save to selected file:\n%s"), fPath.c_str());
		}
	      saveFile.Write(queryData);
	      saveFile.Close();
	      queryPath = fPath;
	    }
	}
      else //queryChangeMsgResult == wxNO
	{
	  int overWriteCacheMsgResult = wxYES;
	  wxFile queryFile;
	  wxFFile saveFile;
	  wxString queryStr;
	  wxString fPath;
	  wxString queryData;
	  fPath = PXD_DEFAULT_QUERY_PATH;
	  queryData = pxdQuery->GetValue();

	  DB_P("In no-save changed query");
	  if(queryFile.Exists(fPath.c_str()))
	    {
	      overWriteCacheMsgResult = 
		wxMessageBox(_T("Query cache already exists.\nOverwrite?"),
			     _T("Overwrite Query Cache?"),
			     wxYES_NO | wxCANCEL | wxICON_QUESTION);
	      if(overWriteCacheMsgResult == wxCANCEL)
		return;
	    }
	  if(overWriteCacheMsgResult == wxYES)
	    {
	      if(!saveFile.Open(fPath, "w"))
		{
		  wxLogError(_T("Could not save to selected file:\n%s"), fPath.c_str());
		}
	      DB_P("Writing to query cache");
	      saveFile.Write(queryData);
	      saveFile.Close();
	      queryPath = fPath;	  
	    }
	}
    }
  //queryPath now points to path of active query, source path points to proper source => 
  // generate commands and run PADX from here

  int i = 0;
  wxString queryCmd;
  wxArrayString arrStr;
  wxFFile xmlOutputFile;
  wxString xmlOutputFilePath;
  wxString xmlOutputData;

  int returnVal;
  wxFileDialog *saveOutputD = new wxFileDialog(this,
					      _T("Save XML Output As..."),
					      _T(""),
					      _T(""),
					      _T("XML Files (*.xml)|*.xq|All Files|*"),
					      wxSAVE | wxOVERWRITE_PROMPT);
  returnVal = saveOutputD->ShowModal();
  xmlOutputFilePath = saveOutputD->GetPath();
  saveOutputD->~wxFileDialog();

  if(returnVal == wxID_CANCEL)
    xmlOutputFilePath = PXD_DEFAULT_XML_PATH;

  execPath = pxdExecutable->GetValue();
  queryCmd.Printf(_T("%s %s %s -output-xml %s\n"), 
		  execPath.c_str(),
		  sourcePath.c_str(),
		  queryPath.c_str(),
		  xmlOutputFilePath.c_str());
  arrStr.Clear();
  arrStr.Add(queryCmd);

  ShellDialog shelld(parent, 
	      wxID_ANY, 
	      _T("Execute PADX Query"),
	      parent,
	      arrStr);
  shelld.ShowModal();

if(wxMessageBox(_T("Display XML output in the output text box?"), 
		  _T("Display XML Output?"),
		  wxICON_QUESTION | wxYES_NO) == wxYES)
    {  
      if(!xmlOutputFile.Open(xmlOutputFilePath, "r"))
	{
	  wxLogError(_T("Could not open XML data file:\n%s"), xmlOutputFilePath.c_str());
	  return;
	}
      
      if(!xmlOutputFile.ReadAll(&xmlOutputData))
	{
	  wxLogError(_T("Could not read data from XML data file:\n%s"), xmlOutputFilePath.c_str());
	  xmlOutputFile.Close();
	  return;
	}

      xmlOutputFile.Close();
      pxdOutput->SetValue(xmlOutputData);
    }
  
  if(wxMessageBox(_T("Launch default web browser with XML output?"), 
		  _T("Launch Web Browser?"),
		  wxICON_QUESTION | wxYES_NO) == wxYES)
    {
      wxString xmlOutputURL;
      xmlOutputURL.Printf(_T("file://%s"), xmlOutputFilePath.c_str());
      DB_P(xmlOutputURL.c_str());
      DB_P(" %d\n", wxLaunchDefaultBrowser(xmlOutputURL));
    }

  return;
}

/* ************************************ */
