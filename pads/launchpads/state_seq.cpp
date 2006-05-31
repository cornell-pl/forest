/* **************************************
 * PADS GUI project (preliminary tests)
 * State sequence record/playback dialog code
 * *********************************** */
#include "lp_includes.h"

/* **************************************
 * Functions:
 *  --class constructor/destructor
 *  StateSequenceDialog
 *  ~StateSequenceDialog
 *
 *  --event handlers
 *  OnSpinCtrlPress
 *  OnStateButtonPress
 *  OnNextState
 *  OnLastState
 * *********************************** */

IMPLEMENT_CLASS(StateSequenceDialog, wxFrame)

BEGIN_EVENT_TABLE(StateSequenceDialog, wxFrame)
  EVT_SPIN(SSD_SPIN_BUTTON, StateSequenceDialog::OnSpinCtrlPress)
  EVT_BUTTON(SSD_STATE_BUTTON, StateSequenceDialog::OnStateButtonPress)
  EVT_BUTTON(SSD_LAST_STATE, StateSequenceDialog::OnLastState)
  EVT_BUTTON(SSD_NEXT_STATE, StateSequenceDialog::OnNextState)
END_EVENT_TABLE()

StateSequenceDialog::StateSequenceDialog(wxWindow* parent, wxWindowID id,
					 const wxString& title,
					 LaunchPADS* par, 
					 int type,
					 int *current_state, 
					 int num_states) : 
  wxFrame(parent, id, title, 
	  wxPoint(800, 400),
	  wxDefaultSize,
	  wxDEFAULT_FRAME_STYLE
#ifdef LP_OSX_METAL
	  | wxFRAME_EX_METAL
#endif
	   )
{
  dialogType = type;
  currentState = *current_state;
  DB_P("current state in ssd = %d\n", currentState);

  lpPar = par;

  ssdOverSizer = new wxGridBagSizer(5, 5);
  if(dialogType == SSD_PLAYBACK)
    ssdMainLabel = new wxStaticText(this, wxID_ANY, _T(SSD_PLAYBACK_LABEL));
  else
    ssdMainLabel = new wxStaticText(this, wxID_ANY, _T(SSD_RECORD_LABEL));

  ssdSpinButtonSizer = new wxBoxSizer(wxHORIZONTAL);

  for(int i = 0; i < 10; i++)
    ssdValidatorIncludes.Add(lpNum_Chars[i]);

  ssdTextValidator = new wxTextValidator(wxFILTER_INCLUDE_LIST);
  ssdTextValidator->SetIncludes(ssdValidatorIncludes);

  long textFlags = wxTE_RIGHT | wxTE_READONLY;

  ssdTextCtrl = new wxTextCtrl(this, SSD_TEXT_ENTERED, _T(""), 
			       wxDefaultPosition,
			       wxDefaultSize,
			       textFlags 
			       //*ssdTextValidator
			       );
  ssdSpinButton = new wxSpinButton(this, SSD_SPIN_BUTTON);
  if(dialogType == SSD_PLAYBACK)
    ssdStateButton = new wxButton(this, SSD_STATE_BUTTON, _T("Load"));
  else
    ssdStateButton = new wxButton(this, SSD_STATE_BUTTON, _T("Save"));

  ssdSpinButton->SetRange(0, num_states);
  max = ssdSpinButton->GetMax();
  min = ssdSpinButton->GetMin();

  ssdSpinButton->SetValue(currentState);
  wxString str;
  str.Printf(_T("%d"), currentState);
  ssdTextCtrl->SetValue(str);


  ssdPrevious = new wxButton(this, SSD_LAST_STATE, _T("<- Previous"));
  ssdNext     = new wxButton(this, SSD_NEXT_STATE, _T("Next ->"));

  if(currentState >= max)
    ssdNext->Enable(false);
  else if(currentState <= min)
    ssdPrevious->Enable(false);

  // buttons = CreateStdDialogButtonSizer(wxOK);

  int gridRow = -1;
  ssdOverSizer->Add(ssdMainLabel, wxGBPosition(++gridRow, 0), wxGBSpan(1, 2), wxALL | wxALIGN_CENTER | wxALIGN_CENTER_VERTICAL, 2);
  ssdSpinButtonSizer->Add(ssdTextCtrl, 1, wxGROW | wxALL | wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL, 2);
  ssdSpinButtonSizer->Add(ssdSpinButton, 0, wxALL | wxALIGN_CENTER | wxALIGN_CENTER_VERTICAL, 2);
  ssdOverSizer->Add(ssdSpinButtonSizer, wxGBPosition(++gridRow, 0), wxDefaultSpan, wxALL | wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL, 2);
  ssdOverSizer->Add(ssdStateButton, wxGBPosition(gridRow, 1), wxDefaultSpan, wxALL | wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL, 5);
  ssdOverSizer->Add(ssdPrevious, wxGBPosition(++gridRow, 0), wxDefaultSpan, wxALL | wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL, 5);
  ssdOverSizer->Add(ssdNext, wxGBPosition(gridRow, 1), wxDefaultSpan, wxALL | wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL, 5);
  // ssdOverSizer->Add(buttons, wxGBPosition(++gridRow, 1), wxDefaultSpan, wxALL | wxALIGN_CENTER | wxALIGN_CENTER_VERTICAL, 5);

  this->SetSizer(ssdOverSizer);
  ssdOverSizer->Fit(this);

  SetMinSize(GetSize());
  SetMaxSize(GetSize());

  DB_P("making ssd with type %d\n", dialogType);
  if(dialogType == SSD_RECORD)
    {
      ssdNext->Enable(false);
      //ssdNext->Show(false);
      ssdPrevious->Enable(false);
      //ssdPrevious->Show(false);
      ssdSpinButton->Enable(false);
      //ssdSpinButton->Show(false);
      ssdSpinButton->SetRange(0, 100000);
    }
}

StateSequenceDialog::~StateSequenceDialog()
{
  ssdMainLabel->Destroy();
  ssdTextCtrl->Destroy();
  ssdSpinButton->Destroy();
  ssdStateButton->Destroy();
  ssdPrevious->Destroy();
  ssdNext->Destroy();
  DB_P("Destroying SSD\n");
}

void StateSequenceDialog::OnSpinCtrlPress(wxSpinEvent &event)
{
  currentState = event.GetPosition();
  
  wxString str;
  str.Printf("%d", currentState);
  ssdTextCtrl->SetValue(str);

  if(currentState == ssdSpinButton->GetMax())
    ssdNext->Enable(false);
  else
    ssdNext->Enable(true);

  if(currentState == ssdSpinButton->GetMin())
    ssdPrevious->Enable(false);
  else
    ssdPrevious->Enable(true);
}

void StateSequenceDialog::OnStateButtonPress(wxCommandEvent &event)
{
  if(dialogType == SSD_RECORD)
    {
      lpPar->SaveState(currentState, true);
      currentState++;
      wxString str;
      str.Printf("%d", currentState);
      ssdTextCtrl->SetValue(str);
    }
  if(dialogType == SSD_PLAYBACK)
    {
      lpPar->LoadState(currentState, true);
      DB_P("loading state %d, with max states %d\n", currentState, max);
    }
}

void StateSequenceDialog::OnNextState(wxCommandEvent &event)
{

  if(dialogType == SSD_RECORD)
    return;

  if(currentState + 1 > max)
    {
      ssdNext->Enable(false);
      return;
    }

  ssdSpinButton->SetValue(currentState + 1);
  currentState = ssdSpinButton->GetValue();

  if(currentState == max)
    ssdNext->Enable(false);

  if(currentState != min)
    ssdPrevious->Enable(true);

  if(dialogType == SSD_PLAYBACK)
    {
      lpPar->LoadState(currentState, true);
      DB_P("loading state %d, with max states %d\n", currentState, max);
    }

  wxString str;
  str.Printf("%d", currentState);
  ssdTextCtrl->SetValue(str);

  // do state loading/saving stuff
}

void StateSequenceDialog::OnLastState(wxCommandEvent &event)
{
  if(dialogType == SSD_RECORD)
    return;

  if(currentState - 1 < min)
    {
      ssdPrevious->Enable(false);
      return;
    }

  ssdSpinButton->SetValue(currentState - 1);
  currentState = ssdSpinButton->GetValue();

  if(currentState == min)
    ssdPrevious->Enable(false);

  if(currentState != max)
    ssdNext->Enable(true);

  if(dialogType == SSD_PLAYBACK)
    {
      lpPar->LoadState(currentState, true);
      DB_P("loading state %d, with max states %d\n", currentState, max);
    }

  wxString str;
  str.Printf("%d", currentState);
  ssdTextCtrl->SetValue(str);

  // do state loading/saving stuff


}

/* ************************************ */
