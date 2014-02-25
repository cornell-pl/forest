/* *****************************************************************************
 * Mark Daly
 * June 2006
 * PADS_inspector - tree inspector for LaunchPADS, only allows changes to AST 
 *  s.t. AST will remain valid after changes
 * ************************************************************************** */

#ifndef PADS_LANG_INSPECTOR_H_INCLUDED__
#define PADS_LANG_INSPECTOR_H_INCLUDED__

#include "wx/minifram.h"
#include "padslang/PADS_lang.h"
#include "padslang/pads_lang_includes.h"
#include "lp_PNode_Container.h"

#include "wx/toolbar.h"
#include "wx/sizer.h"
#include "wx/laywin.h"
#include "wx/treebase.h"
#include "wx/treectrl.h"
#include "wx/gbsizer.h"
#include "wx/dynarray.h"

class PADSLangInspector: public wxMiniFrame
{
public:
    PADSLangInspector(wxFrame *parent, wxWindowID id = wxID_ANY, const wxString& title = _T("PADS Inspector"),
		      const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize,
		      wxTreeCtrl* setTreeCtrl = NULL, bool colorMode = true);

    void OnCloseWindow(wxCloseEvent& event);
    void OnTestTypeSelect(wxCommandEvent& event);
    void OnAddNewChild(wxCommandEvent& WXUNUSED(event));
    void OnDeleteSelectedElement(wxCommandEvent& WXUNUSED(event));
    void OnTextChanged(wxCommandEvent& WXUNUSED(event));
    void OnSelectRootNode(wxCommandEvent& WXUNUSED(event));

    void buildControls();

    void setControlsForSelectedType();

    void setSelectedPNodePandId(PNodeP* newSelected, wxTreeItemId newId);
    PNodeP* getSelectedPNodeP();
    //void setSelectedTreeItemId(wxTreeItemId newId);
    wxTreeItemId getSelectedTreeItemId();

    void setCanDeleteSelectedElement(bool canDelete);

    void updateTreeStartingHere(wxTreeItemId updateThis);

    void setTreeControl(wxTreeCtrl* treeCtrl);

    wxTreeItemId RecurThroughSimpleTree(PADSPNodeTreeNode* thisNode, wxTreeItemId& parentId);
    void SetTreeElementColourFromType(int type, wxColour& setMe);

    void setValueOfSelectedPNode();
    void getValueFromSelectedPNode();

    bool colourModeOn;

    bool textChanged;

    wxTreeCtrl *mainTreeCtrl;

    PNodeP* selectedPNodeP;
    wxTreeItemId selectedTreeId;

    wxStaticBoxSizer *mainSizer;
    wxComboBox       *typeComboBox;
    wxGridBagSizer   *controlSizer;

    //wxStaticBoxSizer *text1Sizer, *text2Sizer, *expr1Sizer, *expr2Sizer, *exprMultiSizer;
    //wxStaticBoxSizer *modifiersCheckListSizer, *parameterListSizer, *argumentListSizer;

    //wxTextCtrl       *text1, *text2;
    //wxTextCtrl       *expr1, *expr2, *exprMulti;
    //wxCheckListBox   *modifiersCheckList;
    //wxListBox        *parameterList, *argumentList;
    //wxStaticBoxSizer   *currentTypeNameSizer;
    //wxStaticText       *currentTypeName;

    wxStaticBoxSizer   *valInputSizer;
    wxTextCtrl         *valInput;

    wxStaticBoxSizer   *exprInputSizer;
    wxTextCtrl         *exprInput;

    wxStaticBoxSizer   *addChildSizer;
    wxStaticText       *addChildListLabel;
    wxListBox          *addChildList;
    wxButton           *addChildButton;
    wxButton           *deleteThisNodeButton;
    wxButton           *selectRootButton;

    wxStaticBoxSizer   *availableTypesSizer;
    wxListBox          *availableTypesList;

    // testing variables
    PNodeFactory pFactory;

DECLARE_EVENT_TABLE()
};

#endif
