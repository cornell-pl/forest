/* *****************************************************************************
 * Mark Daly
 * June 2006
 * PADS_inspector - tree inspector for LaunchPADS, only allows changes to AST 
 *  s.t. AST will remain valid after changes
 * ************************************************************************** */

#include "wx/toolbar.h"
#include "wx/sizer.h"
#include "wx/laywin.h"
#include "wx/treebase.h"
#include "wx/treectrl.h"
#include "wx/gbsizer.h"
#include "wx/dynarray.h"

#include "PADS_inspector.h"
#include "lp_event_ids.h"

#include "lp_constants.h"

BEGIN_EVENT_TABLE(PADSLangInspector, wxMiniFrame)
    EVT_CLOSE  (PADSLangInspector::OnCloseWindow)
    EVT_COMBOBOX (INSP_TYPECOMBOBOX, PADSLangInspector::OnTestTypeSelect)
    EVT_BUTTON (INSP_ADDCHILD,  PADSLangInspector::OnAddNewChild)
    EVT_BUTTON (INSP_DELETETHISNODE,  PADSLangInspector::OnDeleteSelectedElement)
    EVT_TEXT   (INSP_VAR_TEXT,  PADSLangInspector::OnTextChanged)
    EVT_TEXT   (INSP_EXPR_TEXT, PADSLangInspector::OnTextChanged)
    EVT_BUTTON (INSP_GETROOTNODE, PADSLangInspector::OnSelectRootNode)
END_EVENT_TABLE()

PADSLangInspector::PADSLangInspector(wxFrame* parent, wxWindowID id, const wxString& title, const wxPoint& pos,
				     const wxSize& size, wxTreeCtrl* setTreeCtrl, bool colorMode) :
  wxMiniFrame(parent, id, title, pos, size )
{
  buildControls();
  valInputSizer->Show(false);
  wxLayoutAlgorithm layout;
  layout.LayoutFrame(this);

  mainTreeCtrl = setTreeCtrl;
  colourModeOn = colorMode;

  textChanged = false;

  this->SetSizer(mainSizer);
  this->SetAutoLayout(true);
  mainSizer->SetSizeHints(this);
  exprInputSizer->Show(false);
}

void PADSLangInspector::buildControls()
{
  /*
  text1Sizer = new wxStaticBoxSizer(wxHORIZONTAL, 
				    this,
				    _T("text1Sizer"));

  text2Sizer = new wxStaticBoxSizer(wxHORIZONTAL, 
				    this,
				    _T("text2Sizer"));

  expr1Sizer = new wxStaticBoxSizer(wxHORIZONTAL, 
				    this,
				    _T("expr1Sizer"));

  expr2Sizer = new wxStaticBoxSizer(wxHORIZONTAL, 
				    this,
				    _T("expr2Sizer"));

  exprMultiSizer = new wxStaticBoxSizer(wxHORIZONTAL, 
					this,
					_T("exprMultiSizer"));

  */
  /*
  currentTypeNameSizer = new wxStaticBoxSizer(wxHORIZONTAL,
					      this,
					      _T("Selected Node Type"));
  currentTypeName = new wxStaticText(this, 
				     wxID_ANY,
				     _T(PT_TypeNames[PT_PADS]),
				     wxDefaultPosition,
				     wxDefaultSize,
				     wxALIGN_CENTER | wxST_NO_AUTORESIZE);
  currentTypeNameSizer->Add(currentTypeName, 1, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT, 3);
  */

  mainSizer = new wxStaticBoxSizer(wxVERTICAL, 
				   this,
				   _T("PADS Type Inspector"));
  //  controlSizer = new wxGridBagSizer(4, 4);
  wxArrayString typeNames;
  for(int i = 0; i < PT_LAST_TYPE; i++)
    {
      typeNames.Add(_T(PT_TypeNames[i]));
    }
  typeComboBox = new wxComboBox(this,
				INSP_TYPECOMBOBOX,
				_T(""),
				wxDefaultPosition,
				wxDefaultSize,
				typeNames,
				wxCB_DROPDOWN | wxCB_READONLY);
  mainSizer->Add(typeComboBox, 0, wxGROW  | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT, 3);
  typeComboBox->Show(false);

  valInputSizer = new wxStaticBoxSizer(wxVERTICAL,
				       this,
				       _T("Text Input"));
  valInput = new wxTextCtrl(this,
			    INSP_VAR_TEXT,
			    _T(""),
			    wxDefaultPosition,
			    wxDefaultSize,
			    wxTE_LEFT | wxTE_DONTWRAP);
  valInputSizer->Add(valInput, 0, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT, 2);


  exprInputSizer = new wxStaticBoxSizer(wxHORIZONTAL,
				       this,
				       _T("Expr Input"));
  exprInput = new wxTextCtrl(this,
			     INSP_EXPR_TEXT,
			     _T(""),
			     wxDefaultPosition,
			     wxDefaultSize,
			    wxTE_LEFT | wxTE_MULTILINE | wxTE_PROCESS_ENTER | wxTE_PROCESS_TAB);
  exprInputSizer->Add(exprInput, 1, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT, 2);
  
  addChildSizer = new wxStaticBoxSizer(wxVERTICAL,
					   this,
					   _T("Add/Remove Node Controls"));
  addChildListLabel = new wxStaticText(this,
				       wxID_ANY,
				       _T("Available Child Types:"),
				       wxDefaultPosition,
				       wxDefaultSize,
				       wxALIGN_CENTER | wxST_NO_AUTORESIZE);
  addChildList = new wxListBox(this,
			       wxID_ANY,
			       wxDefaultPosition,
			       wxSize(150, 100),
			       0,
			       NULL,
			       wxLB_SINGLE | wxLB_HSCROLL | wxLB_SORT | wxLB_ALWAYS_SB);
  addChildButton = new wxButton(this,
				INSP_ADDCHILD,
				_T("Add New Child"));
  deleteThisNodeButton = new wxButton(this,
				      INSP_DELETETHISNODE,
				      _T("Delete This Node"));
  selectRootButton = new wxButton(this,
				  INSP_GETROOTNODE,
				  _T("Select PADS Root"));
  
  addChildSizer->Add(addChildListLabel, 0, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT, 4);
  addChildSizer->Add(addChildList, 0, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT, 4);
  addChildSizer->Add(addChildButton, 0, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT, 4);
  addChildSizer->Add(deleteThisNodeButton, 0, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT, 4);
  addChildSizer->Add(selectRootButton, 0, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT, 4);

  /*
  availableTypesSizer = new wxStaticBoxSizer(wxHORIZONTAL,
					     this,
					     _T("Declarable Types"));
  availableTypesList = new wxListBox(this,
				     wxID_ANY,
				     wxDefaultPosition,
				     wxDefaultSize,
				     0,
				     NULL,
				     wxLB_SINGLE | wxLB_HSCROLL | wxLB_SORT | wxLB_ALWAYS_SB);
  availableTypesSizer->Add(availableTypesList, 1, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT, 2);
  */

  mainSizer->Add(valInputSizer, 0, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT, 3);
  mainSizer->Add(exprInputSizer, 0, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT, 3);
  mainSizer->Add(addChildSizer, 1, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT, 3);
  //mainSizer->Add(availableTypesSizer, 0, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT, 3);
}

void PADSLangInspector::OnCloseWindow(wxCloseEvent& WXUNUSED(event))
{
  // destroy the inspector whenever it's closed
  Destroy();
  // note that this should be set to just make the inspector invisible
  //  so we never have to recreate the object
}

void PADSLangInspector::OnTestTypeSelect(wxCommandEvent& event)
{
  int thisType = event.GetInt();
  selectedPNodeP = pFactory.makeEmptyPNodeFromType(thisType);
  setControlsForSelectedType();
}

void PADSLangInspector::OnAddNewChild(wxCommandEvent& WXUNUSED(event))
{
  DB_P("adding new child element\n");
  wxString childTypeString;
  PNodeP* newNode;
  childTypeString = addChildList->GetStringSelection();
  if(childTypeString.Cmp(_T("")) == 0)
    return;
  DB_P("child type string = %s\n", childTypeString.c_str());

  int newNodeType = pFactory.getTypeFromString((char*)childTypeString.c_str());
  newNode = pFactory.makeEmptyPNodeFromType(newNodeType);
  newNode->makeCompleteNode();

  DB_P("complete node...\n");

  wxTreeItemId parentId = mainTreeCtrl->GetItemParent(selectedTreeId);
  DB_P("selected id = %d\n", (long int)selectedTreeId);
  DB_P("parent id = %d\n", (long int)parentId);
  PNodeWXTreeItem* treeItemData = (PNodeWXTreeItem*)mainTreeCtrl->GetItemData(selectedTreeId);
  if(treeItemData == NULL)
    {
      DB_P("treeItemData returned as NULL!\n");
      return;
    }
  PNodeP* parentNode = treeItemData->linkNode;
  DB_P("got parent node of type %s\n", PT_TypeNames[parentNode->nodeType]);
  if(parentNode == NULL)
    {
      DB_P("parentNode returned as NULL!\n");
      return;
    }

  DB_P("attempting to add child\n");
  try
    {
      parentNode->addChild(newNode);
    }
  catch (...)
    {
      DB_P("adding to parent node from inspector threw exception!\n");
    }
  DB_P("child node of type :%s: added, rebuilding tree\n", PT_TypeNames[newNode->nodeType]);
  PADSPNodeTreeNode* newPNodeTreeNode = parentNode->makeBasicTree(NULL);
  mainTreeCtrl->DeleteChildren(selectedTreeId);
  RecurThroughSimpleTree((PADSPNodeTreeNode*)newPNodeTreeNode->firstChild, selectedTreeId);
  setControlsForSelectedType();
}

void PADSLangInspector::OnDeleteSelectedElement(wxCommandEvent& WXUNUSED(event))
{
  DB_P("deleting selected element\n");

  wxTreeItemId parentId = mainTreeCtrl->GetItemParent(selectedTreeId);
  DB_P("selected id = %d\n", (long int)selectedTreeId);
  DB_P("parent id = %d\n", (long int)parentId);
  PNodeWXTreeItem* treeItemData = (PNodeWXTreeItem*)mainTreeCtrl->GetItemData(selectedTreeId);
  if(treeItemData == NULL)
    {
      DB_P("treeItemData returned as NULL!\n");
      return;
    }
  PNodeWXTreeItem* parentItemData = (PNodeWXTreeItem*)mainTreeCtrl->GetItemData(parentId);
  if(treeItemData == NULL)
    {
      DB_P("Parent treeItemData returned as NULL!\n");
      return;
    }
  PNodeP* thisNode = treeItemData->linkNode;
  DB_P("got this node of type %s\n", PT_TypeNames[thisNode->nodeType]);
  if(thisNode == NULL)
    {
      DB_P("thisNode returned as NULL!\n");
      return;
    }

  PNodeP* parentNode = parentItemData->linkNode;
  DB_P("got parent node of type %s\n", PT_TypeNames[parentNode->nodeType]);
  if(parentNode == NULL)
    {
      DB_P("parentNode returned as NULL!\n");
      return;
    }

  int deleteRetVal;
  DB_P("attempting to delete child\n");
  try
    {
      deleteRetVal = parentNode->removeChildNode(thisNode);
    }
  catch (...)
    {
      DB_P("deleting node from inspector threw exception!\n");
    }
  if(deleteRetVal != 0)
    {
      DB_P("could not find child node\n");
      wxBell();
      return;
    }

  DB_P("child node of type :%s: removed w/ val %d, rebuilding tree\n", PT_TypeNames[thisNode->nodeType], 
       deleteRetVal);
  PADSPNodeTreeNode* newPNodeTreeNode = parentNode->makeBasicTree(NULL);
  mainTreeCtrl->DeleteChildren(parentId);
  if(newPNodeTreeNode->firstChild != NULL)
    RecurThroughSimpleTree((PADSPNodeTreeNode*)newPNodeTreeNode->firstChild, parentId);
  mainTreeCtrl->SelectItem(parentId);
  setControlsForSelectedType();
}

void PADSLangInspector::OnTextChanged(wxCommandEvent& WXUNUSED(event))
{
  textChanged = true;
  DB_P("text changed = %d\n", textChanged);
}

void PADSLangInspector::OnSelectRootNode(wxCommandEvent& WXUNUSED(event))
{
  mainTreeCtrl->SelectItem(mainTreeCtrl->GetRootItem());
}

// *************************

void PADSLangInspector::updateTreeStartingHere(wxTreeItemId updateThis)
{
  if(updateThis == mainTreeCtrl->GetRootItem())
    return;
  wxTreeItemId parentId = updateThis;
  DB_P("selected id = %d\n", (long int)selectedTreeId);
  DB_P("parent id = %d\n", (long int)parentId);
  PNodeWXTreeItem* treeItemData = (PNodeWXTreeItem*)mainTreeCtrl->GetItemData(parentId);
  if(treeItemData == NULL)
    {
      DB_P("treeItemData returned as NULL!\n");
      return;
    }
  if(treeItemData->getUpdateParentOnChange())
    {
      DB_P("updateStartingHere: recurring on type %s\n", PT_TypeNames[treeItemData->linkNode->nodeType]);
      updateTreeStartingHere(mainTreeCtrl->GetItemParent(updateThis));
      return;
    }
  PNodeP* parentNode = treeItemData->linkNode;
  DB_P("got parent node of type %s\n", PT_TypeNames[parentNode->nodeType]);
  if(parentNode == NULL)
    {
      DB_P("parentNode returned as NULL!\n");
      return;
    }

  PADSPNodeTreeNode* newPNodeTreeNode = parentNode->makeBasicTree(NULL);

  wxString tempStr = newPNodeTreeNode->nodeName.c_str();
  mainTreeCtrl->SetItemText(parentId, tempStr);

  mainTreeCtrl->DeleteChildren(parentId);
  if(newPNodeTreeNode->firstChild != NULL)
    RecurThroughSimpleTree((PADSPNodeTreeNode*)newPNodeTreeNode->firstChild, parentId);
}

void PADSLangInspector::setControlsForSelectedType()
{
  addChildList->Clear();
  mainSizer->GetStaticBox()->SetLabel(wxString(_T(PT_TypeNames[selectedPNodeP->nodeType])));
  switch(selectedPNodeP->nodeType)
    {
    case PT_UNIT:
    case PT_POMIT:
    case PT_PENDIAN:
    case PT_NOSEP_LOCAL:
      valInputSizer->Show(false);
      exprInputSizer->Show(false);
      addChildSizer->Show(true);
      //availableTypesSizer->Show(false);
      break;
    case PT_NAME:
    case PT_VAR:
    case PT_ID:
    case PT_CHAR:
    case PT_STRING:
    case PT_TYPENAME:
    case PT_PPREFIX:
      valInputSizer->Show(true);
      exprInputSizer->Show(false);
      addChildSizer->Show(true);
      //availableTypesSizer->Show(false);
      break;
    case PT_EXPR:
    case PT_EXPR2:
    case PT_PRE:
    case PT_PPARSECHECK:
    case PT_ARGUMENT:
    case PT_PCOMPUTE:
    case PT_MIN_LOCAL:
    case PT_MAX_LOCAL:
      valInputSizer->Show(false);
      exprInputSizer->Show(true);
      addChildSizer->Show(true);
      //availableTypesSizer->Show(false);
      break;
    case PT_REEXPR:
    case PT_LITERAL:
    case PT_FROM:
    case PT_SEP_LOCAL:
      // do other stuff here to determine nested type!
      valInputSizer->Show(false);
      exprInputSizer->Show(true);
      addChildSizer->Show(true);
      //availableTypesSizer->Show(false);
      break;
    default:
      {
	valInputSizer->Show(false);
	exprInputSizer->Show(false);
	addChildSizer->Show(true);
	//availableTypesSizer->Show(false);
	// testing mess!
	PNodeP* newNode = selectedPNodeP;
	int typeList[32]; 
	int numChildTypes;
	wxArrayString typeNames;
	numChildTypes = newNode->canAddChildTypes(typeList, 32);
	addChildList->Clear();
	for(int i = 0; i < numChildTypes; i++)
	  {
	    typeNames.Add(PT_TypeNames[typeList[i]]);
	  }
	addChildList->InsertItems(typeNames, 0);
      }
    }
  wxLayoutAlgorithm layout;
  layout.LayoutFrame(this);
  mainSizer->Fit(this);
  this->SendSizeEvent();
}

void PADSLangInspector::setValueOfSelectedPNode()
{
  if(selectedPNodeP == NULL)
    return;

  PSTRING sendStr = "";
  wxString getStr;
  switch(selectedPNodeP->nodeType)
    {
    case PT_NAME:
    case PT_VAR:
    case PT_ID:
    case PT_CHAR:
    case PT_STRING:
    case PT_TYPENAME:
    case PT_PPREFIX:
      getStr = valInput->GetValue();
      sendStr = getStr.c_str();
      selectedPNodeP->addValue(sendStr);
      break;
    case PT_EXPR:
    case PT_EXPR2:
    case PT_PRE:
    case PT_PPARSECHECK:
    case PT_ARGUMENT:
    case PT_PCOMPUTE:
    case PT_MIN_LOCAL:
    case PT_MAX_LOCAL:
      getStr = exprInput->GetValue();
      sendStr = getStr.c_str();
      selectedPNodeP->addValue(sendStr);
      break;
    case PT_REEXPR:
    case PT_LITERAL:
    case PT_FROM:
    case PT_SEP_LOCAL:
      // do other stuff here to determine nested type!
      getStr = exprInput->GetValue();
      sendStr = getStr.c_str();
      selectedPNodeP->addValue(sendStr);
      //availableTypesSizer->Show(false);
      break;
    default:
      break;
    }
}

void PADSLangInspector::getValueFromSelectedPNode()
{
  if(selectedPNodeP == NULL)
    return;

  PSTRING printStr = "";
  if(!selectedPNodeP->getValue(printStr))
    return;
  switch(selectedPNodeP->nodeType)
    {
    case PT_NAME:
    case PT_VAR:
    case PT_ID:
    case PT_CHAR:
    case PT_STRING:
    case PT_TYPENAME:
    case PT_PPREFIX:
      valInput->SetValue(_T(printStr.c_str()));
      exprInput->SetValue("");
      break;
    case PT_EXPR:
    case PT_EXPR2:
    case PT_PRE:
    case PT_PPARSECHECK:
    case PT_ARGUMENT:
    case PT_PCOMPUTE:
    case PT_MIN_LOCAL:
    case PT_MAX_LOCAL:
      valInput->SetValue("");
      exprInput->SetValue(_T(printStr.c_str()));
      break;
    case PT_REEXPR:
    case PT_LITERAL:
    case PT_FROM:
    case PT_SEP_LOCAL:
      // do other stuff here to determine nested type!
      valInput->SetValue("");
      exprInput->SetValue(_T(printStr.c_str()));
      //availableTypesSizer->Show(false);
      break;
    default:
      break;
    }
}

void PADSLangInspector::setSelectedPNodePandId(PNodeP* newSelected, wxTreeItemId newId)
{
  // pre-chage
  DB_P("text changed = %d\n", textChanged);
  if(textChanged)
    {
      setValueOfSelectedPNode();
      wxTreeItemId updateMe;
      PNodeWXTreeItem* treeItemData = (PNodeWXTreeItem*)mainTreeCtrl->GetItemData(selectedTreeId);
      DB_P("textchanged: update parent on change = %d\n", treeItemData->getUpdateParentOnChange());
      updateMe = selectedTreeId;
      updateTreeStartingHere(updateMe);
    }
  // post-change
  selectedPNodeP = newSelected;
  selectedTreeId = newId;
  if(selectedPNodeP != NULL)
    {
      setControlsForSelectedType();
    }
  getValueFromSelectedPNode();
  textChanged = false;
}

PNodeP* PADSLangInspector::getSelectedPNodeP()
{
  return selectedPNodeP;
}

/*void PADSLangInspector::setSelectedTreeItemId(wxTreeItemId newId)
{
  selectedTreeId = newId;
}
*/
wxTreeItemId PADSLangInspector::getSelectedTreeItemId()
{
  return selectedTreeId;
}

void PADSLangInspector::setCanDeleteSelectedElement(bool canDelete)
{
  deleteThisNodeButton->Enable(canDelete);
}

void PADSLangInspector::setTreeControl(wxTreeCtrl* treeCtrl)
{
  mainTreeCtrl = treeCtrl;
}

// ***************************
// tree mess!

void PADSLangInspector::SetTreeElementColourFromType(int type, wxColour& setMe)
{
  switch(type)
    {
    case PT_LITERAL:
     if(colourModeOn)
       {  
	 setMe.Set(MID_MODE_DELIM_COLOUR);
       }
     else
       {
	 setMe.Set(MID_MODE_DELIM_NO_COLOUR);
       }
     break;
    case PT_NAME:
    case PT_VAR:
    case PT_ID:
    case PT_EXPR:
    case PT_PRE:
    case PT_STRING:
    case PT_CHAR:
    case PT_EXPR2:
    case PT_PPARSECHECK:
    case PT_TYPENAME:
    case PT_ARGUMENT:
    case PT_PCOMPUTE:
    case PT_FROM:
    case PT_PPREFIX:
    case PT_SEP_LOCAL:
     if(colourModeOn)
       {  
	 setMe.Set(255, 172, 64);
       }
     else
       {
	 setMe.Set(128, 128, 128);
       }
      break;
    case PT_PSOURCE:
    case PT_PRECORD:
    case PT_PLONGEST:
    case PT_PARRAY:
    case PT_PSOME:
    case PT_PNONE:
    case PT_POPT:
    case PT_POMIT:
    case PT_PENDIAN:
    case PT_PALTERNATES:
    case PT_PCHARCLASS:
    case PT_PENUM:
    case PT_PRECURSIVE:
    case PT_PSELECT:
    case PT_PSTRUCT:
    case PT_PTYPEDEF:
    case PT_PSWITCH:
    case PT_PUNION:
     if(colourModeOn)
       {  
	 setMe.Set(255, 
		   ((192+ ((type % 32) * 51)) %224), 
		   ((192+ ((type % 8)  * 51)) %224));
       }
     else
       {
	 setMe.Set(152+ (((type % 8) * 51) %63), 
		   152+ (((type % 8) * 51) %63), 
		   152+ (((type % 8) * 51) %63));
       }
      break;
    default:
      if(colourModeOn)
	{  
	  setMe.Set((64 + ((96   * type) / 51)      % 64), 
			    (96 + (((255 * type  * 2) / 51) % 159)), 
			    255);
	}
      else
	{
	  setMe.Set(96 + (((255 * type * 2) / 51) % 63), 
			    96 + (((255 * type * 2) / 51) % 63), 
			    96 + (((255 * type * 2) / 51) % 63));
	}
      break;
    }
}

wxTreeItemId PADSLangInspector::RecurThroughSimpleTree(PADSPNodeTreeNode* thisNode, wxTreeItemId& parentID)
{
  if(thisNode == NULL)
    return parentID;
  wxColour bgColor;
  PNodeP* newLinkPNode = thisNode->linkNode;
  PNodeWXTreeItem* newLink = new PNodeWXTreeItem(thisNode->linkNode);
  newLink->setUpdateParentOnChange(thisNode->getUpdateParentOnChange());
  newLink->setElementIsRequired(thisNode->getElementIsRequired());
  wxString tempStr = thisNode->nodeName.c_str();

  wxTreeItemId thisID = mainTreeCtrl->AppendItem(parentID, tempStr, -1, -1, newLink);
  SetTreeElementColourFromType(newLinkPNode->nodeType, bgColor);
  mainTreeCtrl->SetItemBackgroundColour(thisID, bgColor);
  newLink->SetId(thisID);

  if(thisNode->firstChild != NULL)
    RecurThroughSimpleTree((PADSPNodeTreeNode*)thisNode->firstChild, thisID);
  if(thisNode->nextSibling != NULL)
    RecurThroughSimpleTree((PADSPNodeTreeNode*)thisNode->nextSibling, parentID);
  return thisID;
}

// ******************
