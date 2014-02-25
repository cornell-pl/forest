/* **************************************
 * PADS GUI project (preliminary tests)
 * Shell dialog class header
 * *********************************** */

#include "lp_includes.h"

#ifndef _LP_SHELL_INCLUDED
#define _LP_SHELL_INCLUDED


class ShellDialog : public wxDialog
{
DECLARE_CLASS(ShellDialog)

 public:
 ShellDialog(wxWindow* parent, wxWindowID id,
	     const wxString& title, 
	     LaunchPADS* par, 
	     wxArrayString &execCmd);
 ~ShellDialog();

 void OnUserStart(wxCommandEvent &event);

 protected:

 LaunchPADS *parent;

 //wxStdDialogButtonSizer *sdButtons;
 //wxBoxSizer *sdButtonSizer;
 wxButton *sdOK;
 wxButton *sdStart;
 
 wxBoxSizer *sdTextSizer;
 wxTextCtrl *sdText;

 wxArrayString cmds;

 wxString nl;

 void sdAddText(wxString &str);
 void sdAddText(wxArrayString &arrStr);
 void sdClearText(void);
 void sdEnableButtons(void);
 void sdDisableButtons(void);
 void sdListCommands(void);
 void sdExecute(void);

DECLARE_EVENT_TABLE()
};

#endif
