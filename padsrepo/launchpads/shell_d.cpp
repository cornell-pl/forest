/* **************************************
 * PADS GUI project (preliminary tests)
 * Non-interactive shell code
 * *********************************** */
#include "lp_includes.h"

/* **************************************
 * Functions:
 *  --class constructor/destructor
 *  ShellDialog
 *  ~ShellDialog
 *
 *  --event handlers
 *  OnUserStart
 *
 *  --interface
 *  sdListCommands    --print command chain so user knows what's about to happen
 *  sdAddText         --print line in shell (overloaded: single string)
 *  sdAddText         --print line in shell (overloaded: multiple strings)
 *  sdClearText       --clear text in shell
 *  sdEnableButtons   --enable start/exit buttons
 *  sdDisableButtons  --disable start/exit buttons
 *  sdExecute         --executed command chain
 * *********************************** */

IMPLEMENT_CLASS(ShellDialog, wxDialog)

BEGIN_EVENT_TABLE(ShellDialog, wxDialog)
  EVT_BUTTON(SD_EXECUTE, ShellDialog::OnUserStart)
END_EVENT_TABLE()


ShellDialog::ShellDialog(wxWindow* par_win, wxWindowID id,
			 const wxString& title, 
			 LaunchPADS* par, 
			 wxArrayString &execCmd)
  // taken from the wxPropertySheet documentation
  : wxDialog(par_win, id, title, 
	     wxDefaultPosition,
	     wxSize(832, 400),
	     wxCAPTION
#ifdef LP_OSX_METAL
	     | wxDIALOG_EX_METAL
#endif
#ifndef __WXWINCE__
	     | wxRESIZE_BORDER
#endif
	     )
  {
    parent = par;
    
    sdTextSizer = new wxBoxSizer(wxVERTICAL);
    sdText = new wxTextCtrl(this, 
			    wxID_ANY, 
			    _T(""),
			    wxDefaultPosition,
			    wxSize(832, 400),
			    wxTE_READONLY
			    | wxTE_MULTILINE
			    | wxTE_RICH
			    | wxTE_BESTWRAP);
    wxFont sdFont(10,
		  wxFONTFAMILY_TELETYPE,
		  wxFONTSTYLE_NORMAL,
		  wxFONTWEIGHT_NORMAL);
    if(sdFont.Ok())
      sdText->SetFont(sdFont);

    sdTextSizer->Add(sdText, 1, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 0);

    //sdButtons = CreateStdDialogButtonSizer(0);
    //sdButtonSizer = new wxBoxSizer(wxVERTICAL);
    sdStart = new wxButton(this, SD_EXECUTE, _T("&Start"));
    sdOK = new wxButton(this, wxID_CANCEL, _T("E&xit"));
    //sdButtons->AddButton(sdOK);
    //sdButtons->Realize();

    sdTextSizer->Add(sdStart,     0,          wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT, 2);
    sdTextSizer->Add(sdOK,        0,          wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT, 2);
    //sdTextSizer->Add(sdButtonSizer, 1, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT, 0);

    this->SetSizer(sdTextSizer);
    sdTextSizer->Fit(this);

    cmds = execCmd;
 
    nl.Printf(_T("\n")); // newline for text controls - must be \n regardless of OS

    sdListCommands();
  }

ShellDialog::~ShellDialog()
{
  sdOK->Destroy();
  sdStart->Destroy();
  //sdButtons->~wxStdDialogButtonSizer();
  sdText->Destroy();
  //wxButtonSizer->~wxBoxSizer();
  sdTextSizer->~wxBoxSizer();
}

/* ************* */
void ShellDialog::OnUserStart(wxCommandEvent &event)
{
  sdDisableButtons();
  sdExecute();
  sdEnableButtons();
}

/* ************* */

void ShellDialog::sdListCommands(void)
{
  int numCmds = cmds.GetCount();
  wxString str;

  str.Printf(_T("<<<  ready to execute  >>>\n"));
  sdAddText(str);
  str.Printf(_T("--command queue:\n"));
  sdAddText(str);
  for(int i = 0; i < numCmds; i++)
    {
      str.Printf(_T("#%d:> %s\n"), i, cmds.Item(i).Trim(true).c_str());
      sdAddText(str);
    }
  str.Printf(_T("<<<press start to begin>>>\n"));
  sdAddText(str);
  str.Printf(_T("<<<press exit to cancel>>>\n"));
  sdAddText(str);
  str = "";
  sdAddText(str);

}

void ShellDialog::sdAddText(wxString &str)
{
  sdText->AppendText(str);
  if(!(str.Contains(nl)))
    sdText->AppendText(nl);
}

void ShellDialog::sdAddText(wxArrayString &arrStr)
{
  int numStrings = arrStr.GetCount();
  for(int i = 0; i < numStrings; i++)
    {
      sdText->AppendText(arrStr.Item(i));
      if(!(arrStr.Item(i).Contains(nl)))
	 sdText->AppendText(nl);
    }
}

void ShellDialog::sdClearText(void)
{
  sdText->Clear();
}

void ShellDialog::sdEnableButtons(void)
{
  sdOK->Enable(true);
  sdOK->Refresh();
  sdStart->Enable(true);
  sdStart->Refresh();
  Refresh();
}

void ShellDialog::sdDisableButtons(void)
{
  sdOK->Enable(false);
  sdOK->Refresh();
  sdStart->Enable(false);
  sdStart->Refresh();
  Refresh();
}


void ShellDialog::sdExecute()
{
  int numCmds = cmds.GetCount();
  long returnCode;

  wxString str;
  wxArrayString output;
  wxArrayString errors;

  for(int i = 0; i < numCmds; i++)
    {
      str.Printf(_T("---executing command %d of %d---\n"), i+1, numCmds);
      sdAddText(str);
      str.Printf(_T(">>> %s"), cmds.Item(i).c_str());
      sdAddText(str);
      Refresh();
      wxTheApp->Yield();
      returnCode = wxExecute(cmds.Item(i), output, errors);
      str.Printf(_T("[returned with code: %d]\n"), returnCode);
      sdAddText(str);
      str.Printf(_T("---output---\n"));
      sdAddText(str);
      sdAddText(output);
      str.Printf(_T("---errors---\n"));
      output.Clear();
      sdAddText(str);
      sdAddText(errors);
      str.Printf(_T("\n\n"));
      sdAddText(str);
      errors.Clear();
    }
  str.Printf(_T("<<< execution complete >>>\n"));
  sdAddText(str);
  str.Printf(_T("<<< press exit to exit >>>\n"));
  sdAddText(str);
  str.Printf(_T("<<< or start to re-run >>>\n"));
  sdAddText(str);

  return;
}

/* ************************************ */
