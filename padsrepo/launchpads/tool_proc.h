/* **************************************
 * PADS GUI project (preliminary tests)
 * Tool process choice class header
 * *********************************** */

#include "lp_includes.h"

#ifndef _LP_TOOL_PROC_INCLUDED
#define _LP_TOOL_PROC_INCLUDED

#define tpd_TOOL_LABEL1 "Generate code for a new tool"
#define tpd_TOOL_LABEL2 "Compile a tool"
#define tpd_TOOL_LABEL3 "Run a tool"

class ToolProcessDialog : public wxDialog
{
DECLARE_CLASS(ToolProcessDialog)

 public:
  ToolProcessDialog(wxWindow* parent, wxWindowID id,
		    const wxString& title,
		    LaunchPADS* par, 
		    wxString &label,
		    bool *opt1,
		    bool *opt2,
		    bool *opt3);
  ~ToolProcessDialog();
 
  void OnCheckboxCheck(wxCommandEvent &event);

 protected:
  LaunchPADS *parent;

  bool *b1;
  bool *b2;
  bool *b3;

  wxBoxSizer   *mainSizer;
  wxStaticText *tpdLabel;
  wxCheckBox *cb1;
  wxCheckBox *cb2;
  wxCheckBox *cb3;

  wxStdDialogButtonSizer *buttons;

DECLARE_EVENT_TABLE()
};

#endif
