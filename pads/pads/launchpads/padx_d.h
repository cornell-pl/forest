/* **************************************
 * PADS GUI project (preliminary tests)
 * Shell dialog class header
 * *********************************** */

#include "lp_includes.h"

#ifndef _LP_PADXD_INCLUDED
#define _LP_PADXD_INCLUDED

#define PXD_DEFAULT_QUERY_PATH "./LP__QUERY__CACHE.xq"
#define PXD_DEFAULT_XML_PATH   "./LP__XML__OUTPUT__CACHE.xml"

class PADXDialog : public wxDialog
{
DECLARE_CLASS(PADXDialog)

 public:
 PADXDialog(wxWindow* parent, wxWindowID id,
	     const wxString& title, 
	     LaunchPADS* par, 
	     wxArrayString &execCmd);
 ~PADXDialog();

 void OnSize(wxSizeEvent &event);
 void OnUserExec(wxCommandEvent &event);
 void OnUserSave(wxCommandEvent &event);
 void OnUserLoadQueries(wxCommandEvent &event);
 void OnUserClearQueries(wxCommandEvent &event);
 void OnUserLoadSources(wxCommandEvent &event);
 void OnUserClearSources(wxCommandEvent &event);

 void OnUserChangeText(wxCommandEvent &event);

 void OnUserQueryCheck(wxCommandEvent &event);
 void OnUserSourceCheck(wxCommandEvent &event);

 protected:

 LaunchPADS *parent;

 //wxStdDialogButtonSizer *sdButtons;
 //wxBoxSizer *sdButtonSizer;
 wxButton *pxdExec;
 wxButton *pxdSave;
 wxButton *pxdExit;
 wxButton *pxdLoadQueries;
 wxButton *pxdClearQueries;
 wxButton *pxdLoadSources;
 wxButton *pxdClearSources;
 
 wxTextCtrl *pxdExecutable;
 wxTextCtrl *pxdQuery;
 wxTextCtrl *pxdOutput;

 wxCheckListBox *pxdQueryList;
 wxCheckListBox *pxdSourceList;

 wxStaticBoxSizer *pxdExecutableSizer;
 wxBoxSizer *pxdTextButtonSizer;
 wxStaticBoxSizer *pxdTextQuerySizer;
 wxBoxSizer *pxdQueryButtonSizer;
 wxBoxSizer *pxdSourceButtonSizer;
 wxStaticBoxSizer *pxdQueryControlSizer;
 wxStaticBoxSizer *pxdSourceControlSizer;
 wxStaticBoxSizer *pxdOutputSizer;
 wxGridBagSizer *pxdQuerySourceSizer;
 wxGridBagSizer *pxdMainSizer;

 wxArrayString cmds;

 wxString nl;

 int queryChecked;
 int sourceChecked;

 bool textChanged;

 wxString queryPath;
 wxString sourcePath;
 wxString execPath;

 wxString lastQueryDir;
 wxString lastSourceDir;
 
 void pxdSetQuery(wxString queryFilePath);
 void pxdSetSource(wxString sourceFilePath);
 void pxdExecute(void);
 void pxdEnableButtons(void);
 void pxdDisableButtons(void);
 void pxdAddText(wxString &str);
 void pxdAddText(wxArrayString &arrStr);

DECLARE_EVENT_TABLE()
};

#endif
