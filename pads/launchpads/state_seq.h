/* **************************************
 * PADS GUI project (preliminary tests)
 * State sequence record/playback dialog header
 * *********************************** */

#include "lp_includes.h"

#ifndef _LP_STATE_SEQ_INCLUDED
#define _LP_STATE_SEQ_INCLUDED

#define SSD_PLAYBACK_LABEL "State Playback"
#define SSD_RECORD_LABEL "State Recorder"


#define SSD_PLAYBACK_LABEL "State Playback"
#define SSD_RECORD_LABEL "State Recorder"

class StateSequenceDialog : public wxFrame
{
DECLARE_CLASS(StateSequenceDialog)
  
 public:
  StateSequenceDialog(wxWindow* parent, wxWindowID id,
		      const wxString& title,
		      LaunchPADS* par, 
		      int type,
		      int *current_state, 
		      int num_states);
  ~StateSequenceDialog();

  void OnSpinCtrlPress(wxSpinEvent &event);
  void OnStateButtonPress(wxCommandEvent &event);

  void OnNextState(wxCommandEvent &Event);
  void OnLastState(wxCommandEvent &Event);

 protected:
  
  int dialogType;
  int currentState;
  int min, max;

  LaunchPADS* lpPar;

  wxGridBagSizer         *ssdOverSizer;
  wxStaticText           *ssdMainLabel;
  wxBoxSizer             *ssdSpinButtonSizer;
  wxTextValidator        *ssdTextValidator;
  wxArrayString           ssdValidatorIncludes;
  wxTextCtrl             *ssdTextCtrl;
  wxSpinButton           *ssdSpinButton;
  wxButton               *ssdStateButton;
  wxButton               *ssdPrevious;
  wxButton               *ssdNext;
  // wxStdDialogButtonSizer *buttons;

DECLARE_EVENT_TABLE()
};

#endif
