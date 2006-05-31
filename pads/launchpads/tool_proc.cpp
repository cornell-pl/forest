/* **************************************
 * PADS GUI project (preliminary tests)
 * Tool sequence choice dialog code
 * *********************************** */

#include "lp_includes.h"

/* **************************************
 * Functions:
 *  --class constructor/destructor
 *  ToolProcessDialog
 *  ~ToolProcessDialog
 *
 *  --event handlers
 *  OnCheckboxCheck
 * *********************************** */

IMPLEMENT_CLASS(ToolProcessDialog, wxDialog)

BEGIN_EVENT_TABLE(ToolProcessDialog, wxDialog)
  EVT_CHECKBOX(TOOL_DIAG_CHECKBOX, ToolProcessDialog::OnCheckboxCheck)
END_EVENT_TABLE()


ToolProcessDialog::ToolProcessDialog(wxWindow* parent, 
				     wxWindowID id,
				     const wxString& title,
				     LaunchPADS* par, 
				     wxString &label,
				     bool *opt1,
				     bool *opt2,
				     bool *opt3) :
  wxDialog(parent, id, title, 
	   wxDefaultPosition,
	   wxDefaultSize,
	   wxCAPTION
#ifdef LP_OSX_METAL
	     | wxDIALOG_EX_METAL
#endif
	   )

{
  mainSizer = new wxBoxSizer(wxVERTICAL);
  tpdLabel = new wxStaticText(this, wxID_ANY, label);
  cb1 = new wxCheckBox(this, TOOL_DIAG_CHECKBOX, _T(tpd_TOOL_LABEL1));
  cb2 = new wxCheckBox(this, TOOL_DIAG_CHECKBOX, _T(tpd_TOOL_LABEL2));
  cb3 = new wxCheckBox(this, TOOL_DIAG_CHECKBOX, _T(tpd_TOOL_LABEL3));

  buttons = CreateStdDialogButtonSizer(wxOK | wxCANCEL);

  mainSizer->Add(tpdLabel, 0, wxALL | wxALIGN_CENTER | wxALIGN_CENTER_VERTICAL, 8);
  mainSizer->Add(cb1, 0, wxALL | wxALIGN_CENTER | wxALIGN_CENTER_VERTICAL, 4);
  mainSizer->Add(cb2, 0, wxALL | wxALIGN_CENTER | wxALIGN_CENTER_VERTICAL, 4);
  mainSizer->Add(cb3, 0, wxALL | wxALIGN_CENTER | wxALIGN_CENTER_VERTICAL, 4);
  mainSizer->Add(buttons, 0, wxALL | wxALIGN_CENTER | wxALIGN_CENTER_VERTICAL, 8);

  b1 = opt1;
  b2 = opt2;
  b3 = opt3;

  cb1->SetValue(*b1);
  cb2->SetValue(*b2);
  cb3->SetValue(*b3);

  this->SetSizer(mainSizer);
  mainSizer->Fit(this);
}

ToolProcessDialog::~ToolProcessDialog()
{
  // be really careful about this since this dialog will get used a lot
  cb1->Destroy();
  cb2->Destroy();
  cb3->Destroy();

  tpdLabel->Destroy();
}

void ToolProcessDialog::OnCheckboxCheck(wxCommandEvent &event)
{
  *b1 = cb1->GetValue();
  *b2 = cb2->GetValue();
  *b3 = cb3->GetValue();
  DB_P("toolprocessdiag: checkbox update\n");
  return;
}

/* ************************************ */
