/* **************************************
 * PADS GUI project (preliminary tests)
 * Top frame related code
 * *********************************** */
#include "lp_includes.h"
#include "pads_gui.h"

/* **************************************
 * Functions:
 *  --init
 *  TopInitFrame
 *  TopMakeCheckList
 *  TopMakeButtons
 *
 *  --event handlers
 *  OnTopOpen
 *  OnTopClear
 *  OnTopImport
 *
 *  --customization
 *  TopGetFont
 *  TopSetFont
 * *********************************** */

void LaunchPADS::TopInitFrame(long panelflags, 
			     long listflags, 
			     long buttonflags)
{
  m_topWin->SetMinimumSizeY(TOP_WIN_DEFAULT_Y);

  int topx, topy;
  m_topWin->GetSize(&topx, &topy);

  m_topWinPanel = new wxPanel(m_topWin, 
			      wxID_ANY, 
			      wxDefaultPosition,
			      wxDefaultSize,
			      //m_topWin->GetSize(),
			      panelflags);
  assert(m_topWinPanel != NULL);

  topWinSizer = new wxBoxSizer(wxHORIZONTAL);
  assert(topWinSizer != NULL);

  topWinButtonSizer = new wxGridSizer(3, 1, 0, 0);
  assert(topWinButtonSizer != NULL);

  TopMakeCheckList(listflags);
  TopMakeButtons(buttonflags);

  topWinSizer->Add(topWinButtonSizer, 0, wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT, 3);

  //TopSetFont(topFont);
  
  m_topWinPanel->SetSizer(topWinSizer);
  m_topWinPanel->SetAutoLayout(true);
  topWinSizer->SetSizeHints(m_topWinPanel);
}

void LaunchPADS::TopMakeCheckList(long flags)
{
  int topx, topy;
  m_topWinPanel->GetSize(&topx, &topy);

  topCheckList = new wxCheckListBox(m_topWinPanel, 
				    TOP_LIST_BOX_CHECK,
				    wxDefaultPosition,
				    wxSize(-1, topy-100),
				    0,
				    NULL,
				    flags);
  assert(topCheckList != NULL);			  
  //topCheckList->SetBackgroundColour(wxColour(255, 128, 128));
  topWinSizer->Add(topCheckList, 1,  wxGROW | wxLEFT | wxTOP | wxBOTTOM | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 5);
}

void LaunchPADS::TopMakeButtons(long flags)
{
  topOpenFile = new wxButton(m_topWinPanel, 
			     TOP_OPEN_FILE, 
			     _T(" Open "), 
			     wxDefaultPosition, 
			     wxDefaultSize, 
			     flags);
  assert(topOpenFile != NULL);
  topOpenFile->SetToolTip(wxString("Open file as a deconstruction data source"));
  topWinButtonSizer->Add(topOpenFile, 0, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);

  topClear    = new wxButton(m_topWinPanel,
			     TOP_CLEAR,
			     _T(" Clear "),
			     wxDefaultPosition,
			     wxDefaultSize,
			     flags);
  assert(topClear != NULL);
  topClear->SetToolTip(wxString("Clear entries in source window"));
  topWinButtonSizer->Add(topClear,   0, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);

  topImport   = new wxButton(m_topWinPanel, 
			     TOP_IMPORT_STRING, 
			     _T("Select"), 
			     wxDefaultPosition,
			     wxDefaultSize, 
			     flags);
  assert(topImport != NULL);
  topImport->SetToolTip(wxString("Import string into deconstruction mechanism"));
  topWinButtonSizer->Add(topImport,   0, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);

}

/* ************************************ */

// create open file dialog for top sash frame, and import 
// a portion of the selected file into the checklistbox
void LaunchPADS::OnTopOpen(wxCommandEvent &event)
{
 
  wxFileDialog openFileDialog(this, 
			      "Open file",
			      "",
			      "",
			      IMPORT_FILETYPES,
			      wxOPEN,
			      wxDefaultPosition);

  if(openFileDialog.ShowModal() == wxID_OK)
    {

      if(wxFile::Access(openFileDialog.GetPath(), wxFile::read))
	{
	  DB_P( "access->okay\n");
	  wxString str;
	  wxString delim;
	  wxString delimFormatted;

	  int numLinesRead = 0;

	  str.Printf(_T("Set record delimiter text - use standard print formatting conventions \n(\\n for new line, \\t for tab, \\\\ for \\, %%%% for %%, etc.)."));
	  delim = wxGetTextFromUser(str, _T("Record Delimiter Entry"));
	  if(delim.IsEmpty())
	    return;

	  DB_P("Delim = %s\n", delim.c_str());

	  str.Empty();

          delim.Replace("\%", "%", true); // make sure we treat all the characters equally 
	  delim.Replace("%", "\%", true);
	  DB_P("Fixed delim = \"%s\"\n", delim.c_str());

	  delim.Replace("\\n", "\n");
	  delim.Replace("\\t", "\t");
	  delim.Replace("\\%", "\%");

	  if(delimFormatted.Printf(delim.c_str()) <= 0)
	    {
	      wxLogError(_T("String \"%s\" is not a valid delimiter string."), delim.c_str());
	      return;
	    }

	  DB_P("Formatted delim = \"%s\"\n", delim.c_str());

	  wxFileInputStream iStream(openFileDialog.GetPath());
	  if(!iStream.Ok())
	    {
	      wxLogError(_T("File %s could not be opened properly."), openFileDialog.GetPath().c_str());
	      return;
	    }

	  wxTextInputStream tiStream(iStream);
	  tiStream.SetStringSeparators(delimFormatted);

	  wxProgressDialog pdiag(_T("Importing Data"),
				 _T("Importing Entries..."),
				 topNumLinesOnImport,
				 NULL,
				 wxPD_APP_MODAL | wxPD_SMOOTH | wxPD_AUTO_HIDE);

	  
	  for(int i = 0; i < topNumLinesOnImport; i++)
	    {
	      str = tiStream.ReadWord();
	      if(str.IsEmpty())
		break;
	      if(str.Length() > 0)
		topCheckList->InsertItems(1, &str, topCheckList->GetCount());
	      pdiag.Update(i);
	    }

	  pdiag.Update(topNumLinesOnImport, _T("Done"));
	  
	  topCheckList->Refresh();
	}
      else
	{
	  wxString error_string;
	  error_string.Printf("Selected file %s could not be opened for reading.", 
			      openFileDialog.GetPath().c_str());
	  wxMessageBox(error_string,
		       _T("Could Not Read File!"),
		       wxICON_EXCLAMATION | wxOK);

	}
    }
  return;
}

void LaunchPADS::OnTopClear(wxCommandEvent &event)
{
  if(wxMessageBox(_T("This will clear all entries in the data source window.\nContinue?"), 
		  _T("Confirm Clear List"),
		  wxICON_QUESTION | wxYES_NO) == wxYES)
    {
      topCheckList->Clear();
      topCheckedEntry = -1;
    }

}

// transfer the line selected in the top checklistbox to the grid for 
// region selection
void LaunchPADS::OnTopImport(wxCommandEvent &event)
{
  int checked = -1;
  int num_items = topCheckList->GetCount();
  
  for(int i = 0; i < num_items; i++)
    {
      if(topCheckList->IsChecked(i))
	{
	  checked = i;
	  break;
	}
    }
  if(checked == -1)
    {
      wxMessageBox( _T("Please select (check) a template before continuing!"), 
		    _T("Error"),
		    wxICON_EXCLAMATION | wxCANCEL);
      return;
    }

  if(wxMessageBox(_T("This will delete the preexisting grid.\nContinue?"), 
		  _T("Confirm String Import"),
		  wxICON_QUESTION | wxYES_NO) == wxYES)
    {
      wxString sendstr(topCheckList->GetString(checked)); 
      MidGridUseAsInput(sendstr);
    }

  return;
}


void LaunchPADS::TopGetFont(wxFont &font)
{
  font = topCheckList->GetFont();
}

void LaunchPADS::TopSetFont(wxFont &font)
{
  DB_P("setting top checklist font\n");
  fConfig->Write(_T("/top/font_name"), font.GetFaceName());
  fConfig->Write(_T("/top/font_size"), font.GetPointSize());
  topCheckList->SetFont(font);
  topCheckList->Refresh();
}

/* ************************************ */
