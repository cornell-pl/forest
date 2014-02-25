/* **************************************
 * PADS GUI project (preliminary tests)
 * Left frame related code
 * *********************************** */
#include "lp_includes.h"
#include "pads_gui.h"

/* **************************************
 * Functions:
 *  --init
 *  LeftInitFrame
 *  LeftTreeTest
 *  LeftTreeInit
 *  LeftBuildControlPanel
 *
 *  --event handlers
 *  OnLeftEnableControls
 *  OnLeftTreeContextMenu
 *  OnLeftStopExprContextMenu
 *  OnLeftArgContextMenu
 *  OnLeftSelectType
 *  OnLeftSetAttr
 *  OnLeftGetAttr
 *  OnLeftClearControls
 *  OnLeftDeleteTree
 *  OnLeftNewChild
 *  OnLeftNewSibling
 *  OnLeftDeleteChildren
 *  OnLeftDeleteNode
 *  OnLeftMoveTop
 *  OnLeftMoveUp
 *  OnLeftMoveDown
 *  OnLeftMoveBottom
 *  OnLeftTreeCtrlKeys
 *  OnLeftSetArg
 *  OnLeftClearArg
 *  OnLeftSetClause
 *  OnLeftClearClause
 *
 *  --interface
 *  LeftEnableControls          --toggle tree control panel visibility
 *  LeftGetTreeRoot             --get root element in tree view (invisible)
 *  LeftBuildTree               --wrapper/prep function for MidGridMakeTreeFromParse (see mid.cpp)
 *  LeftSetTreeElemAttr         --set all attributes of tree node
 *  LeftSetTreeElemName         --change only name of tree node
 *  LeftSetTreeElemType         --change only type of tree node
 *  LeftMakeCodeFromTree        --generate PADS definition code from tree parse
 *  LeftMakeCodeRecur           --*DO NOT CALL DIRECTLY* auxiliary function for LeftMakeCodeFromTree
 *  LeftCodeOptRecur            --*DO NOT CALL DIRECTLY*  | (for Popt)
 *  LeftPrintTermElem           --*DO NOT CALL DIRECTLY*  | (for printing single non-typedef terminal element)
 *  LeftSetItemBackgroundColour --set background color of tree node
 * *********************************** */


void LaunchPADS::LeftInitFrame(long panelflags, 
			      long treeflags,
			      long buttonflags)
{
  m_leftWin->SetMinimumSizeX(LEFT_WIN_DEFAULT_X); 

  m_leftWinPanel = new wxPanel(m_leftWin, 
			       wxID_ANY, 
			       wxDefaultPosition,
			       m_leftWin->GetSize(),
			       panelflags);
  assert(m_leftWinPanel != NULL);

  leftTreeCtrl   = new wxTreeCtrl(m_leftWinPanel,
				  LEFT_TREE_CTRL,
				  wxDefaultPosition,
				  wxDefaultSize,//wxSize(20, 500),
				  treeflags);
  assert(leftTreeCtrl != NULL);
  leftTreeCtrl->SetBackgroundColour(wxColour(255, 255, 255));
  
  // build context menu for tree
  leftTreeMenu = new wxMenu;
  leftTreeMenu->Append(LEFT_FIND_IN_CODE_MENU,
		       _T("&Find in Code"),
		       _T("Find this element in the PADS definition"));
  leftTreeMenu->AppendSeparator();
  leftTreeMenu->Append(LEFT_CHANGE_NAME_MENU,
		       _T("Change &Name"),
		       _T("Change the name of this element"));
  leftTreeMenu->Append(LEFT_CHANGE_TYPE_MENU,
		       _T("Change &Type"),
		       _T("Change the type of this element"));
  leftTreeMenu->AppendSeparator();
  leftTreeMenu->Append(LEFT_GET_ATTR_MENU,
		       _T("&Get Attributes"),
		       _T("Get attributes of selected element"));
  leftTreeMenu->Append(LEFT_SET_ATTR_MENU,
		       _T("&Set Attributes"),
		       _T("Set attributes of selected element"));
  leftTreeMenu->AppendSeparator();
  leftTreeMenu->Append(LEFT_MAKE_CHILD_MENU,
		       _T("Make &Child"),
		       _T("Make new child of selected node using current control settings"));
  leftTreeMenu->Append(LEFT_MAKE_SIBLING_MENU,
		       _T("Make &Sibling"),
		       _T("Make new sibling of selected node using current control settings"));
  leftTreeMenu->AppendSeparator();
  leftTreeMenu->Append(LEFT_MOVE_NODE_TOP_MENU,
		       _T("Move to T&op"),
		       _T("Move selected node to top of sibling group"));
  leftTreeMenu->Append(LEFT_MOVE_NODE_UP_MENU,
		       _T("Move &Up"),
		       _T("Move selected node up in sibling group"));
  leftTreeMenu->Append(LEFT_MOVE_NODE_DOWN_MENU,
		       _T("Move &Down"),
		       _T("Move selected node down sibling group"));
  leftTreeMenu->Append(LEFT_MOVE_NODE_BOTTOM_MENU,
		       _T("Move to &Bottom"),
		       _T("Move selected node to bottom of sibling group"));

  DB_P("done building tree, initalizing...\n");
  LeftTreeTest();
  LeftTreeInit();
  DB_P("done building tree, initalizing...\n");
  
  leftEnableControls = new wxButton(m_leftWinPanel, 
				    LEFT_ENABLE_CONTROLS,
				    _T("Toggle &Tree Controls"), 
				    wxDefaultPosition,
				    wxDefaultSize, 
				    buttonflags);
  assert(leftEnableControls != NULL);
  leftStatBoxSizer_1 = new wxStaticBoxSizer(wxVERTICAL,
					    m_leftWinPanel,
					    wxString("Tree Controls"));
  assert(leftStatBoxSizer_1 != NULL);
  
  // create sizers for component groups
  // done out of order to keep z-ordering proper
  leftSubSizer_1 = new wxBoxSizer(wxHORIZONTAL);
  assert(leftSubSizer_1 != NULL);
  leftSubSizer_3 = new wxBoxSizer(wxVERTICAL);
  assert(leftSubSizer_3 != NULL);
  leftSubSizer_4 = new wxBoxSizer(wxHORIZONTAL);
  assert(leftSubSizer_4 != NULL);
  leftSubSizer_2 = new wxGridBagSizer(4, 0);
  assert(leftSubSizer_2 != NULL);
  leftSubSizer_5 = new wxStaticBoxSizer(wxHORIZONTAL, m_leftWinPanel, _T("Predicate Specification"));
  assert(leftSubSizer_5 != NULL);
  leftSubSizer_6 = new wxStaticBoxSizer(wxHORIZONTAL, m_leftWinPanel, _T("Custom Text Entry"));
  assert(leftSubSizer_6 != NULL);
  //  leftSubSizer_7 = new wxBoxSizer(wxHORIZONTAL);
  
  LeftBuildControlPanel(0, 0, 0, 0);
  
  // place all the components for the tree control panel
  // this is a mess - it's probably best to leave this part alone...
  leftStatBoxSizer_1->Add(leftSubSizer_1, 0, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT, 2);
  leftStatBoxSizer_1->Add(leftSubSizer_3, 0, wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  leftStatBoxSizer_1->Add(leftSubSizer_4, 0, wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 5);
  leftStatBoxSizer_1->Add(leftSubSizer_2, 0, wxALL | wxALIGN_BOTTOM | wxALIGN_CENTER, 2);
  leftStatBoxSizer_1->Add(leftSubSizer_5, 0, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT, 5);
  leftStatBoxSizer_1->Add(leftSubSizer_6, 0, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT, 5);
  //  leftStatBoxSizer_1->Add(leftSubSizer_7, 0, wxALL | wxALIGN_BOTTOM | wxALIGN_CENTER, 2);
  
  leftWinSizer   = new wxBoxSizer(wxVERTICAL);
  assert(leftWinSizer != NULL);
  leftWinSizer_2 = new wxBoxSizer(wxHORIZONTAL);
  assert(leftWinSizer_2 != NULL);
  leftWinSizer_2->Add(leftTreeCtrl, 1,  wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT, 3);
  leftWinSizer_2->Add(leftStatBoxSizer_1, 0, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT, 2);
  leftWinSizer->Add(leftWinSizer_2, 1,  wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT, 2);
  leftWinSizer->Add(leftEnableControls, 0, wxGROW | wxALL | wxALIGN_BOTTOM | wxALIGN_CENTER, 7);

  DB_P("doing show/hide modifications\n");
  leftStatBoxSizer_1->Show(false);
  leftSubSizer_1->Show(false);
  leftSubSizer_2->Show(false);
  leftSubSizer_3->Show(false);
  leftSubSizer_4->Show(false);
  leftSubSizer_5->Show(false);
  leftSubSizer_6->Show(false);

  // I don't understand why this doesn't work... oh well
  /*
  wxAcceleratorEntry accels[8];

  accels[0].Set(wxACCEL_CTRL, (int)'H', LEFT_MAKE_CHILD_KEY);
  accels[1].Set(wxACCEL_CTRL, (int)'B', LEFT_MAKE_SIBLING_KEY);
  accels[2].Set(wxACCEL_CTRL, (int)'G', LEFT_GET_ATTR_KEY);
  accels[3].Set(wxACCEL_CTRL, (int)'E', LEFT_SET_ATTR_KEY);
  accels[4].Set(wxACCEL_CTRL, (int) WXK_UP, LEFT_MOVE_NODE_UP_KEY);
  accels[5].Set(wxACCEL_CTRL, (int) WXK_DOWN, LEFT_MOVE_NODE_DOWN_KEY);
  accels[6].Set(wxACCEL_CTRL | wxACCEL_SHIFT, (int) WXK_UP, LEFT_MOVE_NODE_TOP_KEY);
  accels[7].Set(wxACCEL_CTRL | wxACCEL_SHIFT, (int) WXK_DOWN, LEFT_MOVE_NODE_BOTTOM_KEY);

  accels[0].Set(wxACCEL_CTRL, (int)'H', LEFT_MAKE_CHILD);
  accels[1].Set(wxACCEL_CTRL, (int)'B', LEFT_MAKE_SIBLING);
  accels[2].Set(wxACCEL_CTRL, (int)'G', LEFT_GET_ATTR);
  accels[3].Set(wxACCEL_CTRL, (int)'E', LEFT_SET_ATTR);
  accels[4].Set(wxACCEL_CTRL, (int) WXK_UP, LEFT_MOVE_NODE_UP);
  accels[5].Set(wxACCEL_CTRL, (int) WXK_DOWN, LEFT_MOVE_NODE_DOWN);
  accels[6].Set(wxACCEL_CTRL | wxACCEL_SHIFT, (int) WXK_UP, LEFT_MOVE_NODE_TOP);
  accels[7].Set(wxACCEL_CTRL | wxACCEL_SHIFT, (int) WXK_DOWN, LEFT_MOVE_NODE_BOTTOM);

  wxAcceleratorTable accel_table(8, accels);  
  //m_leftWinPanel->SetAcceleratorTable(accel_table);
  m_leftWinPanel->SetAcceleratorTable(accel_table);
  */

  m_leftWin->SetSashVisible(wxSASH_RIGHT, true);
  wxLayoutAlgorithm layout;
  layout.LayoutFrame(this);

  m_leftWinPanel->SetSizer(leftWinSizer);
  m_leftWinPanel->SetAutoLayout(true);
  leftWinSizer->SetSizeHints(m_leftWinPanel);

  // new additions for updated type system 
  leftInspector = new PADSLangInspector(this, 
					wxID_ANY,
					_T("Tree Inspector"),
					wxPoint(800, 150), 
					wxSize(270, 100), 
					leftTreeCtrl, 
					colourModeOn);
  leftInspector->Show(false);
  // *****************
}

void LaunchPADS::LeftTreeTest(void)
{

  // another defunct widget test function, left here in case we want to run more test

  /*
  wxTreeItemId items[7];
  wxTreeItemId root = leftTreeCtrl->GetRootItem();

  leftTreeCtrl->SetItemBold(root, true);
  items[0] = leftTreeCtrl->AppendItem(leftTreeCtrl->GetRootItem(), 
				      _T("Item0"));
  items[1] = leftTreeCtrl->AppendItem(leftTreeCtrl->GetRootItem(), 
				      _T("Item1"));
  items[2] = leftTreeCtrl->AppendItem(items[1], 
				      _T("Item2"));
  items[3] = leftTreeCtrl->AppendItem(items[2], 
				      _T("Item3"));
  items[4] = leftTreeCtrl->AppendItem(items[1], 
				      _T("Item4"));
  items[5] = leftTreeCtrl->AppendItem(items[3], 
				      _T("Item5"));
  items[6] = leftTreeCtrl->AppendItem(items[5], 
				      _T("Item6"));
  */			   
}

void LaunchPADS::LeftTreeInit(void)
{
  DB_P("deleting all tree items...\n");
  leftTreeCtrl->DeleteAllItems();
  PNodeFactory pFactory;
  DB_P("making new root element\n");
  PNodeP* rootElement = pFactory.makeEmptyPNodeFromType(PT_PADS);
  rootElement->makeCompleteNode();
  DB_P("rootElement value = %d\n", rootElement);
  DB_P("calling LeftMakeVisualTreeFromAST =>\n");
  LeftMakeVisualTreeFromAST(rootElement);

  DB_P("Returning from tree init\n");
  /*
  wxString str;
  str = "Root";
  PElement *elem = new PElement((int)pStruct, str, 0);
  assert(elem != NULL);
  wxTreeItemId root = leftTreeCtrl->AddRoot(str, -1, -1,elem);

  str = "Source_t";
  elem = new PElement((int)pStruct, str, pSourceFlag);
  assert(elem != NULL);
  str.Printf("%s %s", PADS_generalized_names[pStruct], "Source_t");
  wxTreeItemId source = leftTreeCtrl->AppendItem(root, str, -1, -1, elem);
  DB_P("source type: %d\n", elem->GetType());
  if(leftTreeBackgroundColourOn)
    leftTreeCtrl->SetItemBackgroundColour(source, wxColour(MID_MODE_UDEF_COLOUR));
  else if(leftTreeTextColourOn)
    leftTreeCtrl->SetItemTextColour(source, wxColour(MID_MODE_UDEF_COLOUR));
  else
    leftTreeCtrl->SetItemBackgroundColour(source, wxColour(MID_MODE_UDEF_NO_COLOUR));
  */
}

void LaunchPADS::LeftBuildControlPanel(long buttonflags, long comboflags, 
				      long radioflags,  long textflags)
{

  // type menu / name input
 
  int num_total = P_TERM_USER_TREE_SELECTABLE + P_NUM_NON_TERMINALS;
  int i, j;
  leftTypeStrings.Alloc(num_total);

  // only pick out the types we want to make available to the user
  for(i = P_TERM_USER_TREE_SELECT_START, j = 0; 
      j < P_TERM_USER_TREE_SELECTABLE; 
      i++, j++)
    {
      leftTypeStrings.Add(PADS_labels[i]);
    }
  for(i = pStartNonTerminals + 1; j < num_total; i++, j++)
    {
      leftTypeStrings.Add(PADS_labels[i]);
    }


  leftTypeSelect = new wxComboBox(m_leftWinPanel,
				  LEFT_TYPE_SELECT,
				  wxString("PADS Type Selection"),
				  wxDefaultPosition,
				  wxDefaultSize,
				  leftTypeStrings,
				  comboflags 
				  & (~wxCB_SORT)
				  | wxCB_READONLY 
				  | wxCB_DROPDOWN);
  leftSubSizer_1->Add(leftTypeSelect, 1,  wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  leftTypeSelect->SetToolTip(_T("Select PADS type for new element"));
  
  // restrict user's input for PADS elements names to [a-zA-Z0-9_]
  //leftNameValid = new wxTextValidator(wxFILTER_INCLUDE_CHAR_LIST);
  //leftNameValidIncludes = new wxArrayString(WXSIZEOF(PADS_name_chars),
  //                                          PADS_name_chars);

  //leftNameValid->SetIncludes(*leftNameValidIncludes);

  leftNameInput = new wxTextCtrl(m_leftWinPanel, 
				 LEFT_NAME_INPUT,
				 _T("New_Element_Name"),
				 wxDefaultPosition,
				 wxDefaultSize,
				 textflags 
				 & ~wxTE_MULTILINE 
				 & ~wxTE_PROCESS_ENTER
				 & ~wxTE_PROCESS_TAB
				 //*leftNameValid
				 );
  leftSubSizer_1->Add(leftNameInput, 1, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  leftNameInput->SetToolTip(_T("Select name for new element"));  

  /* *********** */
  // encoding/type-specific options

  leftSubSizer_3sub1 = new wxBoxSizer(wxHORIZONTAL);
  leftSubSizer_3sub2 = new wxBoxSizer(wxHORIZONTAL);
  //leftSubSizer_3sub1 = leftSubSizer_3;
  //leftSubSizer_3sub2 = leftSubSizer_3;

  wxString Options[7];
  Options[0] = "Unsigned";
  Options[1] = "Signed";
  leftSignSelect = new wxChoice(m_leftWinPanel,
				LEFT_SIGN_SELECT,
				//wxString("Signed/Unsigned"),
				wxDefaultPosition,
				wxDefaultSize,
				2,
				Options);
  leftSubSizer_3sub1->Add(leftSignSelect, 0, wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);    
  leftSignSelect->SetToolTip(_T("Select signed/unsigned element"));  
  leftSignSelect->SetSelection(0);
  if(leftControlSafeMode)
    leftSignSelect->Enable(false);

  Options[0] = "Default";
  Options[1] = "ASCII";
  Options[2] = "EBCDIC";
  Options[3] = "EBC";
  Options[4] = "BCD";
  Options[5] = "SBL";
  Options[6] = "SBH";
  leftEncodingSelect = new wxChoice(m_leftWinPanel,
				    LEFT_ENCODING_SELECT,
				    //wxString("Encoding Type"),
				    wxDefaultPosition,
				    wxDefaultSize,
				    7,
				    Options);
  leftSubSizer_3sub1->Add(leftEncodingSelect, 0, wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);    
  leftEncodingSelect->SetToolTip(_T("Select encoding type for element"));  
  leftEncodingSelect->SetSelection(0);
  if(leftControlSafeMode)
    leftEncodingSelect->Enable(false);

  Options[0] = "8";
  Options[1] = "16"; 
  Options[2] = "32";
  Options[3] = "64"; 
  leftByteSelect = new wxChoice(m_leftWinPanel,
				  LEFT_BYTE_SELECT,
				// wxString("Byte Size"),
				  wxDefaultPosition,
				  wxDefaultSize,
				  4,
				  Options);
  leftSubSizer_3sub1->Add(leftByteSelect, 0, wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  leftByteSelect->SetToolTip(_T("Select size of element in bytes"));  
  leftByteSelect->SetSelection(0);
  if(leftControlSafeMode)
    leftByteSelect->Enable(false);
/*
  Options[0] = "Default";
  Options[1] = "FW (Fixed Width)"; 
  Options[2] = "ME (Match Expression)";
  Options[3] = "SE (Stop Expression)"; 
  */
  Options[0] = "Default";
  Options[1] = "FW"; 
  Options[2] = "ME";
  Options[3] = "SE"; 

  leftTerminationSelect = new wxChoice(m_leftWinPanel,
					 LEFT_TERM_SELECT,
				       // wxString("Termination Type"),
					 wxDefaultPosition,
					 wxDefaultSize,
					 4,
					 Options);
  leftSubSizer_3sub1->Add(leftTerminationSelect, 0, wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  leftTerminationSelect->SetToolTip(_T("Select element termination method"));  
  leftTerminationSelect->SetSelection(0);
  if(leftControlSafeMode)
    leftTerminationSelect->Enable(false);

  leftLParen = new wxStaticText(m_leftWinPanel,
				wxID_ANY,
				_T("(:"),
				wxDefaultPosition,
				wxDefaultSize,
				wxALIGN_LEFT);
  leftSubSizer_3sub2->Add(leftLParen, 0, wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT, 1);

  leftStopExpr = new wxTextCtrl(m_leftWinPanel, 
				LEFT_STOP_EXPR,
				_T("Stop/Format Expression"),
				wxDefaultPosition,
				wxDefaultSize,
				textflags 
				& ~wxTE_MULTILINE 
				& ~wxTE_PROCESS_ENTER
				& ~wxTE_PROCESS_TAB
				// ADD VALIDATOR!!!
				);
  leftSubSizer_3sub2->Add(leftStopExpr, 1, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT, 2);
  leftStopExpr->SetToolTip(_T("Specify stop statement for selected termination method with automatically pre/appended (: :)"));  

  leftRParen = new wxStaticText(m_leftWinPanel,
				wxID_ANY,
				_T(":)"),
				wxDefaultPosition,
				wxDefaultSize,
				wxALIGN_LEFT);
  leftSubSizer_3sub2->Add(leftRParen, 0, wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT, 1);


  leftSubSizer_3->Add(leftSubSizer_3sub1, 1, wxGROW | wxALL | wxALIGN_TOP | wxALIGN_CENTER, 2);
  leftSubSizer_3->Add(leftSubSizer_3sub2, 1, wxGROW | wxALL | wxALIGN_TOP | wxALIGN_CENTER, 2);


  // add strings for stop expression prototypes
  
  leftStopExprMenu = new wxMenu(_T("Stop Expression Prototypes"));
  for(int i = 0; i < NUM_STOP_EXPRESSION_TYPES; i++)
    {
      leftStopExprMenu->Append(LEFT_SET_SE_DEFAULT + i, 
			       PADS_expression_names[i], 
			       _T("Insert stop expression prototype"));
    }

  /* ************** */

  // general options (check boxes)

  leftTypedef = new wxCheckBox(m_leftWinPanel, 
			       wxID_ANY,
			       wxString("Ptypedef"),
			       wxDefaultPosition,
			       wxDefaultSize,
			       wxCHK_2STATE);
  leftSubSizer_4->Add(leftTypedef, 0, wxALL | wxALIGN_TOP | wxALIGN_CENTER, 2);

  leftCompute = new wxCheckBox(m_leftWinPanel, 
			       wxID_ANY,
			       wxString("Pcompute"),
			       wxDefaultPosition,
			       wxDefaultSize,
			       wxCHK_2STATE);
  leftSubSizer_4->Add(leftCompute, 0, wxALL | wxALIGN_TOP | wxALIGN_CENTER, 2);


  leftRecord  = new wxCheckBox(m_leftWinPanel, 
			       wxID_ANY,
			       wxString("Precord"),
			       wxDefaultPosition,
			       wxDefaultSize,
			       wxCHK_2STATE);
  leftSubSizer_4->Add(leftRecord, 0, wxALL | wxALIGN_TOP | wxALIGN_CENTER, 2);

  leftSource  = new wxCheckBox(m_leftWinPanel, 
			       wxID_ANY,
			       wxString("Psource"),
			       wxDefaultPosition,
			       wxDefaultSize,
			       wxCHK_2STATE);
  leftSubSizer_4->Add(leftSource, 0, wxALL | wxALIGN_TOP | wxALIGN_CENTER, 2);

/* ************** */

  // make attribute buttons 

  leftSetAttr       = new wxButton(m_leftWinPanel, 
				   LEFT_SET_ATTR,
				   _T("Set Attributes"), 
				   wxDefaultPosition,
				   wxDefaultSize, 
				   buttonflags | wxBU_EXACTFIT);
  leftSetAttr->SetToolTip(wxString("Set attributes of currently selected node to match control values"));


  leftGetAttr       = new wxButton(m_leftWinPanel, 
				   LEFT_GET_ATTR,
				   _T("Get Attributes"), 
				   wxDefaultPosition,
				   wxDefaultSize, 
				   buttonflags | wxBU_EXACTFIT);
  leftGetAttr->SetToolTip(wxString("Set control values to match attributes of currently selected node"));

  leftClearControls  = new wxButton(m_leftWinPanel, 
				    LEFT_CLEAR_CONTROLS,
				    _T("Clear"), 
				    wxDefaultPosition,
				    wxDefaultSize, 
				    buttonflags | wxBU_EXACTFIT);
  // wait to add so we can get the z-ordering right
  //leftSubSizer_2sub2->Add(leftClearControls, 0, wxGROW | wxLEFT | wxRIGHT | wxALIGN_TOP | wxALIGN_CENTER, 2);    
  leftClearControls->SetToolTip(wxString("Reset controls to an empty state"));

  leftDeleteTree  = new wxButton(m_leftWinPanel, 
				 LEFT_DELETE_TREE,
				 _T("Delete Tree"), 
				 wxDefaultPosition,
				 wxDefaultSize, 
				 buttonflags | wxBU_EXACTFIT);
  //leftSubSizer_2sub2->Add(leftClearControls, 0, wxGROW | wxLEFT | wxRIGHT | wxALIGN_TOP | wxALIGN_CENTER, 2);    
  leftDeleteTree->SetToolTip(wxString("Delete all nodes in tree"));


  // make/delete buttons

  //  leftSubSizer_2sub1 = new wxBoxSizer(wxHORIZONTAL);
  //leftSubSizer_2sub2 = new wxBoxSizer(wxHORIZONTAL);
  leftSubSizer_2sub1 = leftSubSizer_2;
  leftSubSizer_2sub2 = leftSubSizer_2;

  leftMakeChild = new wxButton(m_leftWinPanel, 
			       LEFT_MAKE_CHILD,
			       _T("  New &Child  "), 
			       wxDefaultPosition,
			       wxDefaultSize, 
			       buttonflags | wxBU_EXACTFIT);
  //  leftSubSizer_2sub1->Add(leftMakeChild, 0, wxGROW | wxLEFT | wxRIGHT | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);  
  leftMakeChild->SetToolTip(wxString("Use current settings to make new definition entry as child (next depth) of selected entry"));


  leftMakeSibling = new wxButton(m_leftWinPanel, 
				 LEFT_MAKE_SIBLING,
				 _T("New Si&bling"), 
				 wxDefaultPosition,
				 wxDefaultSize, 
				 buttonflags | wxBU_EXACTFIT);
  // leftSubSizer_2sub1->Add(leftMakeSibling, 0, wxGROW | wxLEFT | wxRIGHT | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);  
  leftMakeSibling->SetToolTip(wxString("Use current settings to make new definition entry as sibling (same depth) of selected entry"));
  
  leftDeleteNode  = new wxButton(m_leftWinPanel, 
				 LEFT_DELETE_NODE,
				 _T("Delete Node"), 
				 wxDefaultPosition,
				 wxDefaultSize, 
				 buttonflags | wxBU_EXACTFIT);
  // leftSubSizer_2sub2->Add(leftDeleteNode, 0, wxGROW | wxLEFT | wxRIGHT | wxALIGN_TOP | wxALIGN_CENTER, 2);    
  leftDeleteNode->SetToolTip(wxString("Delete selected node"));


  leftDeleteChildren  = new wxButton(m_leftWinPanel, 
				 LEFT_DELETE_CHILDREN,
				 _T("Delete Children"), 
				     wxDefaultPosition,
				     wxDefaultSize, 
				     buttonflags | wxBU_EXACTFIT);
  // leftSubSizer_2sub2->Add(leftDeleteNode, 0, wxGROW | wxLEFT | wxRIGHT | wxALIGN_TOP | wxALIGN_CENTER, 2);    
  leftDeleteNode->SetToolTip(wxString("Delete children of selected node but not selected node itself"));


  /*
  leftSubSizer2StatLine_1 = new wxStaticLine(m_leftWinPanel, 
					     wxID_ANY,
					     wxDefaultPosition,
					     wxDefaultSize,
					     wxLI_HORIZONTAL); 
  leftSubSizer_2->Add(leftSubSizer2StatLine_1, wxGBPosition(1, 0), wxGBSpan(1, 4), wxGROW | wxALIGN_TOP | wxALIGN_CENTER, 0);
  */

  // leftSubSizer_2->Add(leftSubSizer_2sub1, 0, wxGROW | wxLEFT | wxRIGHT | wxALIGN_TOP | wxALIGN_CENTER, 2);    
  //leftSubSizer_2->Add(leftSubSizer2StatLine_1, 0, wxGROW | wxLEFT | wxRIGHT | wxALIGN_TOP | wxALIGN_CENTER, 0);    
  //leftSubSizer_2->Add(leftSubSizer_2sub2, 0, wxGROW | wxLEFT | wxRIGHT | wxALIGN_TOP | wxALIGN_CENTER, 2);    

/* ************** */

  

  // leftSubSizer_7sub1 = new wxBoxSizer(wxVERTICAL);
  //leftSubSizer_7sub2 = new wxBoxSizer(wxVERTICAL);
  //leftSubSizer_7sub3 = new wxBoxSizer(wxVERTICAL);
  leftSubSizer_7sub1 = leftSubSizer_2; // this is a terrible hack to make it possible to place
  leftSubSizer_7sub2 = leftSubSizer_2; // all the buttons in one grid sizer without having to
  leftSubSizer_7sub3 = leftSubSizer_2; // rewrite all the ->Add code -- laziness trumps style...



  leftMoveNodeLeft  = new wxButton(m_leftWinPanel, 
				   LEFT_MOVE_NODE_TOP,
				   _T("Move Top"), 
				   wxDefaultPosition,
				   wxDefaultSize, 
				   buttonflags | wxBU_EXACTFIT);
  leftMoveNodeLeft->SetToolTip(wxString("Reduce depth of currently selected node"));
  
  leftMoveNodeUp    = new wxButton(m_leftWinPanel, 
				   LEFT_MOVE_NODE_UP,
				   _T("  Move Up  "), 
				   wxDefaultPosition,
				   wxDefaultSize, 
				   buttonflags | wxBU_EXACTFIT);
  leftMoveNodeUp->SetToolTip(wxString("Place node before previous sibling"));
  
  leftMoveNodeDown  = new wxButton(m_leftWinPanel, 
				   LEFT_MOVE_NODE_DOWN,
				   _T("Move Down"), 
				   wxDefaultPosition,
				   wxDefaultSize, 
				   buttonflags | wxBU_EXACTFIT);
  leftMoveNodeDown->SetToolTip(wxString("Place node after next sibling"));
  
  leftMoveNodeRight = new wxButton(m_leftWinPanel, 
				   LEFT_MOVE_NODE_BOTTOM,
				   _T("Move Bottom"), 
				   wxDefaultPosition,
				   wxDefaultSize, 
				   buttonflags | wxBU_EXACTFIT);
  leftMoveNodeRight->SetToolTip(wxString("Increase depth of currently selected node (becomes child of previous sibling)"));

  leftSubSizer_7sub1->Add(leftSetAttr,        wxGBPosition(0, 0), wxDefaultSpan, wxGROW | wxLEFT | wxRIGHT | wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT, 2);
  leftSubSizer_7sub2->Add(leftGetAttr,        wxGBPosition(0, 1), wxDefaultSpan, wxGROW | wxLEFT | wxRIGHT | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  leftSubSizer_7sub2->Add(leftClearControls,  wxGBPosition(0, 2), wxDefaultSpan, wxGROW | wxLEFT | wxRIGHT | wxALIGN_TOP | wxALIGN_CENTER, 2);    
  leftSubSizer_7sub2->Add(leftDeleteTree,     wxGBPosition(0, 3), wxDefaultSpan, wxGROW | wxLEFT | wxRIGHT | wxALIGN_TOP | wxALIGN_CENTER, 2);

  leftSubSizer_7sub1->Add(leftMakeChild,      wxGBPosition(1, 0), wxDefaultSpan, wxGROW | wxLEFT | wxRIGHT | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  leftSubSizer_7sub1->Add(leftMakeSibling,    wxGBPosition(1, 1), wxDefaultSpan, wxGROW | wxLEFT | wxRIGHT | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  leftSubSizer_7sub2->Add(leftDeleteNode,     wxGBPosition(1, 2), wxDefaultSpan, wxGROW | wxLEFT | wxRIGHT | wxALIGN_TOP | wxALIGN_CENTER, 2);
  leftSubSizer_7sub2->Add(leftDeleteChildren, wxGBPosition(1, 3), wxDefaultSpan, wxGROW | wxLEFT | wxRIGHT | wxALIGN_TOP | wxALIGN_CENTER, 2);

  leftSubSizer_7sub1->Add(leftMoveNodeLeft,   wxGBPosition(2, 0), wxDefaultSpan, wxGROW | wxLEFT | wxRIGHT | wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT, 2);
  leftSubSizer_7sub2->Add(leftMoveNodeUp,     wxGBPosition(2, 1), wxDefaultSpan, wxGROW | wxLEFT | wxRIGHT | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  leftSubSizer_7sub2->Add(leftMoveNodeDown,   wxGBPosition(2, 2), wxDefaultSpan, wxGROW | wxLEFT | wxRIGHT | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  leftSubSizer_7sub3->Add(leftMoveNodeRight,  wxGBPosition(2, 3), wxDefaultSpan, wxGROW | wxLEFT | wxRIGHT | wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT, 2);


  //leftSubSizer_7sub1->Add(leftMoveNodeLeft,  0, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT, 2);
  //leftSubSizer_7sub2->Add(leftMoveNodeUp,    0, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  //leftSubSizer_7sub2->Add(leftMoveNodeDown,  0, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  //leftSubSizer_7sub3->Add(leftMoveNodeRight, 0, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT, 2);

  //leftSubSizer_7->Add(leftSubSizer_7sub1, 1, wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  //leftSubSizer_7->Add(leftSubSizer_7sub2, 1, wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  //leftSubSizer_7->Add(leftSubSizer_7sub3, 1, wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);


  /* ************** */

  // predicate input

  leftClauseTypeMenu = new wxMenu(_T("Qualifier Types"));
  for(int i = 0; i < NUM_QUALIFIERS; i++)
    {
      leftClauseTypeMenu->Append(LEFT_QUALIFIER_MENU_0 + i, 
				 PADS_qualifiers[i], 
				 _T("Insert qualifier string"));
    }
  leftArgInput_1 = new wxTextCtrl(m_leftWinPanel, 
				  LEFT_ARG_INPUT_1,
				  _T(""),
				  wxDefaultPosition,
				  wxDefaultSize,
				  textflags 
				  //& ~wxTE_MULTILINE 
				  | wxTE_MULTILINE
				  //& ~wxTE_PROCESS_ENTER
				  & ~wxTE_PROCESS_TAB
				  | wxTE_RICH);
  leftSubSizer_5->Add(leftArgInput_1, 1, wxGROW | wxALL | wxALIGN_TOP | wxALIGN_CENTER, 2);  
  leftArgInput_1->SetToolTip(wxString("Specify predicate for entry of this type"));

  leftSubSizer_5sub1 = new wxGridSizer(1, 2);
  leftAppendArgument  = new wxButton(m_leftWinPanel, 
				     LEFT_APPEND_ARGUMENT,
				     _T("Add"), 
				     wxDefaultPosition,
				     wxDefaultSize, 
				     buttonflags | wxBU_EXACTFIT);

  leftRemoveArgument  = new wxButton(m_leftWinPanel, 
				     LEFT_REMOVE_ARGUMENT,
				     _T(" Remove "), 
				     wxDefaultPosition,
				     wxDefaultSize, 
				     buttonflags | wxBU_EXACTFIT);
  leftRemoveArgument->SetToolTip(wxString("Delete last argument"));


  leftSubSizer_5sub1->Add(leftAppendArgument, 0, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);    
  leftSubSizer_5sub1->Add(leftRemoveArgument, 0, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);    
  leftSubSizer_5->Add(leftSubSizer_5sub1, 0,  wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);  


  /* ************** */

  // custom text/function input

  leftTextSpecMain = new wxTextCtrl(m_leftWinPanel, 
				    LEFT_TEXTSPEC_MAIN,
				    _T(""),
				    wxDefaultPosition,
				    wxDefaultSize,
				    textflags 
				    | wxTE_MULTILINE 
				    | wxTE_PROCESS_ENTER
				    // | wxTE_PROCESS_TAB
				    | wxTE_RICH);
  leftTextSpecMain->SetToolTip(_T("Create custom functions, constraints, etc."));
  leftSubSizer_6->Add(leftTextSpecMain, 1, wxGROW | wxALL | wxALIGN_TOP | wxALIGN_CENTER, 2);  

  leftSubSizer_6sub1 =  new wxGridSizer(1, 2);
  leftSetClause   = new wxButton(m_leftWinPanel, 
				 LEFT_SET_CLAUSE,
				 _T(" Set Text "), 
				 wxDefaultPosition,
				 wxDefaultSize, 
				 buttonflags | wxBU_EXACTFIT);

  leftClearClause = new wxButton(m_leftWinPanel, 
				 LEFT_CLEAR_CLAUSE,
				 _T("Clear Text"),
				 wxDefaultPosition,
				 wxDefaultSize, 
				 buttonflags | wxBU_EXACTFIT);

  leftSubSizer_6sub1->Add(leftSetClause,   0, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  leftSubSizer_6sub1->Add(leftClearClause, 0, wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);
  leftSubSizer_6->Add(leftSubSizer_6sub1,  0,          wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER, 2);


}

/* ************************************ */

// toggle tree control panel visible/invisible
void LaunchPADS::OnLeftEnableControls(wxCommandEvent &event)
{
  //leftInspector->Show(!leftInspector->IsShown());
  LeftEnableControls(!leftInspector->IsShown());
}

// tree context menu command router
void LaunchPADS::OnLeftTreeContextMenu(wxCommandEvent &event)
{
  int id = event.GetId();
  wxCommandEvent ev2 = event;
  wxTreeItemId selected;
  PElement* elem;
  DB_P("Got tree context menu event %d\n", id);
  wxString str;
  wxString name;
  int type;

  switch(id)
    {
    case LEFT_FIND_IN_CODE_MENU:
      selected = leftTreeCtrl->GetSelection();
      if(selected.IsOk())
	{
	  elem = (PElement*)leftTreeCtrl->GetItemData(selected);
	  BottomSetTextVisible(elem->GetLineNumber());
	}
      else
	{
	  wxBell();
	}
      break;
    case LEFT_CHANGE_NAME_MENU:
      selected = leftTreeCtrl->GetSelection();

      if(selected.IsOk())
	{
	  elem = (PElement*)leftTreeCtrl->GetItemData(selected);	  
	  elem->GetName(name);
	  str = wxGetTextFromUser(_T("Enter new item name."),
				  _T("Item Name Entry"),
				  name);
	  if(!str.IsEmpty())
	    LeftSetTreeElemName(selected, str);
	}
      break;
    case LEFT_CHANGE_TYPE_MENU:
      selected = leftTreeCtrl->GetSelection();
      if(selected.IsOk())
	{
	  type = wxGetSingleChoiceIndex(_T("Enter new item type."), 
					_T("Item Type Entry"), 
					leftTypeStrings);

	  if(type == -1)
	    return;

	  // coppied from LeftSetTreeElemAttr
	  type += P_TERM_USER_TREE_SELECT_START; // adjust for menu offset - starts at char
	  if(type >= P_END_MENU_TERMINAL_TYPES)
	    type += P_TERM_END_NONTERM_START_DISTANCE; // two placeholders missing before first nonterminal

	  LeftSetTreeElemType(selected, type);
	}
      break;
    case LEFT_GET_ATTR_MENU:
      OnLeftGetAttr(ev2);
      break;
    case LEFT_SET_ATTR_MENU:
      OnLeftSetAttr(ev2);
      break;
    case LEFT_MAKE_CHILD_MENU:
      OnLeftNewChild(ev2);
      break;
    case LEFT_MAKE_SIBLING_MENU:
      OnLeftNewSibling(ev2);
      break;
    case LEFT_MOVE_NODE_TOP_MENU:
      OnLeftMoveTop(ev2);
      break;
    case LEFT_MOVE_NODE_UP_MENU:
      OnLeftMoveUp(ev2);
      break;
    case LEFT_MOVE_NODE_DOWN_MENU:
      OnLeftMoveDown(ev2);
      break;
    case LEFT_MOVE_NODE_BOTTOM_MENU:
      OnLeftMoveBottom(ev2);
      break;    
    }
  return;
}

// context menu to insert prototypes into stop expression field
void LaunchPADS::OnLeftStopExprContextMenu(wxCommandEvent &event)
{
  int id = event.GetId();
  DB_P("OnLeftStopExprContextMenu Envoked\n");

  leftStopExpr->SetValue(PADS_expression_prototypes[id - LEFT_SET_SE_DEFAULT]);

  if(id == LEFT_SET_SE_EBC ||
     id == LEFT_SET_SE_BCD)
    {
      int byte_selection = leftByteSelect->GetSelection();
      int sign = leftSignSelect->GetSelection() == 1;
      
      if(byte_selection * 2 + sign < NUM_EBC_BCD_BYTE_RANGES)
	{
	  if(id == LEFT_SET_SE_EBC)
	    {   
	      leftStopExpr->AppendText(PADS_ebc_byte_ranges[byte_selection * 2 + sign]);
	    }
	  else if(id == LEFT_SET_SE_BCD)
	    {
	      leftStopExpr->AppendText(PADS_bcd_byte_ranges[byte_selection * 2 + sign]);
	    }
	}
    }
  return;
}

// context menu to insert qualifiers into predicate field
void LaunchPADS::OnLeftArgContextMenu(wxCommandEvent &event)
{
  int id = event.GetId() - LEFT_QUALIFIER_MENU_0;
  wxString str;

  if(id == pCstrainQ)
    {
      str = " : ";
      leftArgInput_1->WriteText(str);
    }
  else if(id > pCstrainQ && id < pEndQualifiersQ)
    {
      str.Printf(" %s ", PADS_qualifiers[id]);
      leftArgInput_1->WriteText(str);
    }
  DB_P("ARG1 menu: printing %s with id %d\n", str.c_str(), id);

}


// change controls to reflect availability of controls for each type
void LaunchPADS::OnLeftSelectType(wxCommandEvent &event)
{
  if(leftControlSafeMode)
    {
      leftSafeModeAltered = true;
      int type = leftTypeSelect->GetSelection();

      type += P_TERM_USER_TREE_SELECT_START; // adjust for menu offset - starts at char
      if(type >= P_END_MENU_TERMINAL_TYPES)
	type += P_TERM_END_NONTERM_START_DISTANCE; // two placeholders missing before first nonterminal

      bool sign, enctype, bytes, termtype;

      PElement::GetPTypeAttributes(type, sign, enctype, bytes, termtype);

      if(type == pFunction)
	{
	  leftArgInput_1->Enable(false);
	  leftAppendArgument->Enable(false);
	  leftRemoveArgument->Enable(false);
	  leftStopExpr->Enable(false);
	  leftStopExpr->SetValue(_T(""));
	}
      else
	{
	  leftArgInput_1->Enable(true);
	  leftAppendArgument->Enable(true);
	  leftRemoveArgument->Enable(true);
	  leftStopExpr->Enable(true);
	}

      if(sign == false)
	{
	  leftSignSelect->SetSelection(0);
	  leftSignSelect->Enable(false);
	}
      else
	leftSignSelect->Enable(true);

      if(enctype == false)
	{
	  leftEncodingSelect->SetSelection(0);
	  leftEncodingSelect->Enable(false);
	}
      else
	leftEncodingSelect->Enable(true);

      if(bytes == false)
	{
	  leftByteSelect->SetSelection(0);
	  leftByteSelect->Enable(false);
	}
      else
	leftByteSelect->Enable(true);

      if(termtype == false)
	{
	  leftTerminationSelect->SetSelection(0);
	  leftTerminationSelect->Enable(false);
	}
      else
	leftTerminationSelect->Enable(true);

      if(PElement::IsPTerminal(type))
	{
	  leftTypedef->Enable(false);
	  leftTypedef->SetValue(false);
	  if(type != pFunction)
	    leftCompute->Enable(true);
	  else
	    leftCompute->Enable(false);

	  leftRecord->Enable(false);
	  leftRecord->SetValue(false);
	  leftSource->Enable(false);
	  leftSource->SetValue(false);
	}
      else
	{
	  leftTypedef->Enable(true);
      
	  leftCompute->Enable(false);
	  leftCompute->SetValue(false);
	  leftRecord->Enable(true);

	  leftSource->Enable(true);

	}
    }
  else
    {
      if(leftSafeModeAltered)
	{
	  leftSignSelect->Enable(true);
	  leftEncodingSelect->Enable(true);
	  leftByteSelect->Enable(true);
	  leftTerminationSelect->Enable(true);

	  leftTypedef->Enable(true);
	  leftCompute->Enable(true);
	  leftRecord->Enable(true);
	  leftSource->Enable(true);

	  leftArgInput_1->Enable(true);
	  leftAppendArgument->Enable(true);
	  leftRemoveArgument->Enable(true);
	  leftStopExpr->Enable(true);

	  leftSafeModeAltered = false;
	}
    }
  
  return;
}

// * tree interaction functions *
// apply current control settings to selected node
void LaunchPADS::OnLeftSetAttr(wxCommandEvent &event)
{
  wxTreeItemId selected;
  selected = leftTreeCtrl->GetSelection();

  if(selected.IsOk())
    {

      if(leftAttributeChangeWarnings)
	{
	  if(wxMessageBox(_T("This will overwrite the attributes of the currently selected node.\nContinue?"), 
			  _T("Confirm Attribute Set"),
			  wxICON_QUESTION | wxYES_NO) == wxNO)
	    {
	      return;
	    }
	}  
      LeftSetTreeElemAttr(selected);
    }
  else
    {
      wxBell();
    }
  return;

}

// set controls to match attributes of selected node
void LaunchPADS::OnLeftGetAttr(wxCommandEvent &event)
{
  wxTreeItemId selected;
  selected = leftTreeCtrl->GetSelection();
  if(selected.IsOk())
    {
      DB_P("received good onleftgetattr call\n");

      if(leftAttributeChangeWarnings)
	{
	  if(wxMessageBox(_T("This will overwrite the current contents of the controls.\nContinue?"), 
			  _T("Confirm Attribute Get"),
			  wxICON_QUESTION | wxYES_NO) == wxNO)
	    {
	      return;
	    }
	}

      wxString str;

      PElement* elem = (PElement*)leftTreeCtrl->GetItemData(selected);
      int type = elem->GetType();

      // do the same thing as setattr, but backwards
      if(type >= P_END_MENU_TERMINAL_TYPES)
	type -= P_TERM_END_NONTERM_START_DISTANCE; // two placeholders missing before first nonterminal
      type -= P_TERM_USER_TREE_SELECT_START; // adjust for menu offset - starts at char

      leftTypeSelect->SetSelection(type);

      elem->GetName(str);

      leftNameInput->SetValue(str);     
      leftSignSelect->SetSelection(elem->GetSign() == true ? 1 : 0);
      leftEncodingSelect->SetSelection(elem->GetEnctype());
      leftTerminationSelect->SetSelection(elem->GetTermtype());
      leftByteSelect->SetSelection((elem->GetByteSize() >> 4));
      
      wxString stopexpr;
      if(elem->HasStopExpr())
	  elem->GetStopExpr(stopexpr);
      else
	  stopexpr = "";
      leftStopExpr->SetValue(stopexpr);


      long flags = elem->GetFlags();
      leftTypedef->SetValue((elem->GetFlags() & pTypedefFlag) != 0);
      leftCompute->SetValue((elem->GetFlags() & pComputeFlag) != 0);
      leftRecord->SetValue((elem->GetFlags()  & pRecordFlag)  != 0);
      leftSource->SetValue((elem->GetFlags()  & pSourceFlag)  != 0);


      DB_P("getting: \ntype %d %s\nname %s\nflags %x\nbytes %d\n", 
	   elem->GetType(), PADS_labels[elem->GetType()], str.c_str(), flags, elem->GetByteSize() >> 4);
      DB_P("flags: %d %d %d %d\n", 
	   elem->GetFlags() & pTypedefFlag,
	   elem->GetFlags() & pComputeFlag,
	   elem->GetFlags() & pRecordFlag,
	   elem->GetFlags() & pSourceFlag);
      DB_P("type translation number: %d\n", type);

      leftArgInput_1->Clear();
      int num_args = elem->GetNumArgs();
      for(int i = 0; i < num_args; i++)
	{
	  elem->GetArgAtIndex(i, str);
	  leftArgInput_1->AppendText(str);
	}
      
      leftTextSpecMain->Clear();
      if(elem->HasClause())
	{
	  elem->GetClause(str);
	  leftTextSpecMain->AppendText(str);
	}
    }
  else
    {
      DB_P("in get attr: selected != OK\n");
      wxBell();
    }
  return;
}


void LaunchPADS::OnLeftClearControls(wxCommandEvent &event)
{
  if(leftControlsWarnings)
    {
      if(wxMessageBox(_T("This will clear the tree controls.\nContinue?"), 
		      _T("Confirm Control Reset"),
		      wxICON_QUESTION | wxYES_NO) != wxYES)
	{
	  return;
	}
    }
  
  leftTypeSelect->SetSelection(0);
  leftNameInput->Clear();
  leftSignSelect->SetSelection(0);
  leftEncodingSelect->SetSelection(0);
  leftTerminationSelect->SetSelection(0);
  leftByteSelect->SetSelection(0);
  leftStopExpr->Clear();
  leftTypedef->SetValue(false);
  leftCompute->SetValue(false);
  leftRecord->SetValue(false);
  leftSource->SetValue(false);
  leftArgInput_1->Clear();
  leftTextSpecMain->Clear();

  return;
}


void LaunchPADS::OnLeftDeleteTree(wxCommandEvent &event)
{
  if(wxMessageBox(_T("This will delete all nodes in the tree.\nContinue?"), 
		  _T("Confirm Tree Reset"),
		  wxICON_QUESTION | wxYES_NO) == wxYES)
    {
      LeftTreeInit();
    }
  return;

}


/* ************ */

// add new child of selected node with attributes from control settings
void LaunchPADS::OnLeftNewChild(wxCommandEvent &event)
{
  wxTreeItemId parent_id;
  parent_id = leftTreeCtrl->GetSelection();

  if(parent_id.IsOk())
    {

      if(parent_id == leftTreeCtrl->GetRootItem())
	{
	  wxMessageBox(_T("Please select a parent element in the tree."),
		       _T("Cannot Create Child!"),
		       wxICON_ERROR | wxOK);
	  return;
	}

      PElement* parent_elem = (PElement*)leftTreeCtrl->GetItemData(parent_id);
      int parent_type = parent_elem->GetType();

      if(!PElement::IsPNonTerminal(parent_type))
      {
	wxMessageBox(_T("New nodes may not be genereated as children of nonterminal elements."),
		     _T("Cannot Create Child!"),
		     wxICON_ERROR | wxOK);
	return;
      }

      int new_type = leftTypeSelect->GetSelection();
      if(new_type < 0)
	{
	  wxMessageBox(_T("Please select a valid type."),
		       _T("Cannot Change Attributes!"),
		       wxICON_ERROR | wxOK);
	  return;

	}

      // coppied from LeftSetTreeElemAttr
      new_type += P_TERM_USER_TREE_SELECT_START; // adjust for menu offset - starts at char
      if(new_type >= P_END_MENU_TERMINAL_TYPES)
	new_type += P_TERM_END_NONTERM_START_DISTANCE; // two placeholders missing before first nonterminal


      if((parent_type == pOpt || parent_type == pArray) &&
	 (new_type != pFunction) &&
	 leftTreeCtrl->GetChildrenCount(parent_id, false) > 0)
	{
	  wxMessageBox(_T("Elements of type Popt and Parray may have at most 1 child."),
		       _T("Cannot Create Child!"),
		       wxICON_ERROR | wxOK);
	  return;

	}

      if(parent_type == pEnum && new_type != pEnumField)
	{
	  wxMessageBox(_T("Cannot create element not of type enum field with parent of type enum."),
		       _T("Cannot Change Attributes!"),
		       wxICON_ERROR | wxOK);
	  return;
	}
      else if(parent_type != pEnum && new_type == pEnumField)
	{
	  DB_P("in NewChild - parent_type: %d %s, new_type: %d %s\n",
	       parent_type, PADS_labels[parent_type], 
	       new_type, PADS_labels[new_type]);

	  wxMessageBox(_T("Cannot create element of type enum field with parent not of type enum."),
		       _T("Cannot Change Attributes!"),
		       wxICON_ERROR | wxOK);
	  return;
	}
      else if(parent_type != pSwitch && new_type == pCase)
	{
	  DB_P("in NewChild - parent_type: %d %s, new_type: %d %s\n",
	       parent_type, PADS_labels[parent_type], 
	       new_type, PADS_labels[new_type]);

	  wxMessageBox(_T("Cannot create element of type case with parent not of type switch."),
		       _T("Cannot Change Attributes!"),
		       wxICON_ERROR | wxOK);
	  return;
	}
      else if(parent_type == pSwitch && new_type != pCase)
	{
	  DB_P("in NewChild - parent_type: %d %s, new_type: %d %s\n",
	       parent_type, PADS_labels[parent_type], 
	       new_type, PADS_labels[new_type]);

	  wxMessageBox(_T("Cannot create element not of type case with parent of type switch."),
		       _T("Cannot Change Attributes!"),
		       wxICON_ERROR | wxOK);
	  return;
	}
      else if(parent_type == pOpt && new_type == pOpt)
	{
	  wxMessageBox(_T("Cannot create element of type Popt with parent of type Popt."),
		       _T("Cannot Change Attributes!"),
		       wxICON_ERROR | wxOK);
	  return;
	}

      wxString name = "new_child";
      PElement *elem = new PElement((int)new_type, name, 0);

      wxTreeItemId new_node = leftTreeCtrl->AppendItem(parent_id, 
			       name, // this will be changed soon enough
			       -1, -1,
			       elem);
      if(!LeftSetTreeElemAttr(new_node))
	{
	  wxMessageBox(_T("ERROR: New node could not be created."),
		       _T("Cannot Create Child!"),
		       wxICON_ERROR | wxOK);
	}

      leftTreeCtrl->Refresh();
    }
  else
    {
      wxBell();
    }
  return;
}

// add new sibling of selected node with attributes from control settings
void LaunchPADS::OnLeftNewSibling(wxCommandEvent &event)
{
  wxTreeItemId parent_id, sibling_id;
  sibling_id = leftTreeCtrl->GetSelection();

  if(sibling_id.IsOk())
    {
      parent_id = leftTreeCtrl->GetItemParent(sibling_id);
    }
  else
    {
	  wxMessageBox(_T("Selected sibling is invalid."),
		       _T("Cannot Create Sibling!"),
		       wxICON_ERROR | wxOK);
	  return;
    }
  if(parent_id == leftTreeCtrl->GetRootItem())
    {
      wxMessageBox(_T("Cannot create sibling to root element."),
		   _T("Cannot Create Sibling!"),
		   wxICON_ERROR | wxOK);
    }
  else if(parent_id.IsOk())
    {
      DB_P("sibling and parent id's okay...\n");

      PElement* parent_elem = (PElement*)leftTreeCtrl->GetItemData(parent_id);
      int parent_type = parent_elem->GetType();
      // we're going to assume that we're not making a sibling of a child of a terminal type...
      // that would be very bad.

      if(parent_type == pOpt || parent_type == pArray)
	{
	  wxMessageBox(_T("Elements of type Popt and PArray may have at most 1 child."),
		       _T("Cannot Create Sibling!"),
		       wxICON_ERROR | wxOK);
	  return;
	}

      int new_type = leftTypeSelect->GetSelection();
      if(new_type < 0)
	{
	  wxMessageBox(_T("Please select a valid type."),
		       _T("Cannot Change Attributes!"),
		       wxICON_ERROR | wxOK);
	  return;
	}

      // coppied from LeftSetTreeElemAttr
      new_type += P_TERM_USER_TREE_SELECT_START; // adjust for menu offset - starts at char
      if(new_type >= P_END_MENU_TERMINAL_TYPES)
	new_type += P_TERM_END_NONTERM_START_DISTANCE; // two placeholders missing before first nonterminal

      DB_P("new type = %d %s\n", new_type, PADS_labels[new_type]);

      if(parent_type == pEnum && new_type != pEnumField)
	{
	  wxMessageBox(_T("Cannot create element not of type enum field with parent of type enum."),
		       _T("Cannot Change Attributes!"),
		       wxICON_ERROR | wxOK);
	  return;
	}
      else if(new_type == pEnumField && parent_type != pEnum)
	{
	  wxMessageBox(_T("Cannot create element of type enum field with parent not of type enum."),
		       _T("Cannot Change Attributes!"),
		       wxICON_ERROR | wxOK);
	  return;
	}
      else if(parent_type != pSwitch && new_type == pCase)
	{
	  DB_P("in NewChild - parent_type: %d %s, new_type: %d %s\n",
	       parent_type, PADS_labels[parent_type], 
	       new_type, PADS_labels[new_type]);

	  wxMessageBox(_T("Cannot create element of type case with parent not of type switch."),
		       _T("Cannot Change Attributes!"),
		       wxICON_ERROR | wxOK);
	  return;
	}
      else if(parent_type == pSwitch && new_type != pCase)
	{
	  DB_P("in NewChild - parent_type: %d %s, new_type: %d %s\n",
	       parent_type, PADS_labels[parent_type], 
	       new_type, PADS_labels[new_type]);

	  wxMessageBox(_T("Cannot create element of not type case with parent of type switch."),
		       _T("Cannot Change Attributes!"),
		       wxICON_ERROR | wxOK);
	  return;
	}

      wxString name = "new_sibling";
      PElement *elem = new PElement(new_type, name, 0);

      wxTreeItemId new_node = leftTreeCtrl->InsertItem(parent_id, 
						       sibling_id,
						       name, // this will be changed soon enough
						       -1, -1,
						       elem);

      if(!LeftSetTreeElemAttr(new_node))
	{
	  wxMessageBox(_T("ERROR: New node could not be created."),
		       _T("Cannot Create Child!"),
		       wxICON_ERROR | wxOK);
	}

      // time to do the positioning.  here we go...
      // DB_P("working on positioning\n");
      /* 
      // *** oops!  I didn't see that there were built in routines for this... oh well ***

      // this is a bit too complicated to fit in a for loop...
      wxTreeItemId append_id = leftTreeCtrl->GetNextSibling(sibling_id);
      PElement* append_elem = (PElement*)leftTreeCtrl->GetItemData(append_id);
      wxString append_name = leftTreeCtrl->GetItemText(append_id);
      wxColour append_colour = leftTreeCtrl->GetItemBackgroundColour(append_id);
      wxTreeItemId new_appended_item;

      while(append_id != new_node)
	{
	  DB_P("iterating on %s\n", leftTreeCtrl->GetItemText(append_id).c_str());

	  leftTreeCtrl->SetItemData(append_id, NULL); // unhitch the data from the element for temporary deletion
	  leftTreeCtrl->Delete(append_id); // delete for just a moment...
	  new_appended_item = leftTreeCtrl->AppendItem(parent_id, append_name, -1, -1, append_elem);
	  leftTreeCtrl->SetItemBackgroundColour(new_appended_item, append_colour);

	  //next node...
	  append_id = leftTreeCtrl->GetNextSibling(sibling_id);
	  append_elem = (PElement*)leftTreeCtrl->GetItemData(append_id);
	  append_name = leftTreeCtrl->GetItemText(append_id);
	  append_colour = leftTreeCtrl->GetItemBackgroundColour(append_id);
	}
      */

      leftTreeCtrl->Refresh();

    }
  else
    {
      wxBell();
    }
  return;
}

// remove ALL children of selected node
void LaunchPADS::OnLeftDeleteChildren(wxCommandEvent &event)
{

  if(wxMessageBox(_T("This will delete the selected node and all its descendants.\nContinue?"), 
		  _T("Confirm Node Deletion"),
		  wxICON_QUESTION | wxYES_NO) == wxYES)
    {
      wxTreeItemId selected = leftTreeCtrl->GetSelection();

      if(selected.IsOk())
	{
	  wxTreeItemId root = leftTreeCtrl->GetRootItem();
	  if(selected == root)
	    {
	      wxMessageBox(_T("Cannot delete source element."),
			   _T("Cannot Delete Element!"),
			   wxICON_ERROR | wxOK);
	      return;
	    }
	  leftTreeCtrl->DeleteChildren(selected);
	  leftTreeCtrl->SetItemHasChildren(selected, false);
	}
    }
  return;
}
 
void LaunchPADS::OnLeftDeleteNode(wxCommandEvent &event)
{

  if(wxMessageBox(_T("This will delete the selected node and all its descendants.\nContinue?"), 
		  _T("Confirm Node Deletion"),
		  wxICON_QUESTION | wxYES_NO) == wxYES)
    {
      wxTreeItemId selected = leftTreeCtrl->GetSelection();

      if(selected.IsOk())
	{
	  wxTreeItemId root = leftTreeCtrl->GetRootItem();
	  wxTreeItemIdValue cookie;
	  if(selected == root ||
	     selected == leftTreeCtrl->GetFirstChild(root, cookie))
	    {
	      wxMessageBox(_T("Cannot delete source element."),
			   _T("Cannot Delete Element!"),
			   wxICON_ERROR | wxOK);
	      return;
	    }
	  leftTreeCtrl->DeleteChildren(selected);
	  leftTreeCtrl->Delete(selected);
	}
    }
  return;
}

/* ************ */
// node transit functions
// works by grabbing node attributes, deleting, then recreating in the proper location
void LaunchPADS::OnLeftMoveTop(wxCommandEvent &event)
{
  wxTreeItemId parent_id, this_id;
  this_id = leftTreeCtrl->GetSelection();

  if(!this_id.IsOk())
    {
      DB_P("this id was bad in move top\n");
      wxBell();
      return;
    }   

  if(leftTreeCtrl->GetChildrenCount(this_id) > 0)
    {
      wxMessageBox(_T("Elements with children cannot be moved."),
		   _T("Cannot Move Element!"),
		   wxICON_ERROR | wxOK);
      return;
    }
  
  parent_id = leftTreeCtrl->GetItemParent(this_id);

  if(!parent_id.IsOk())
    {
      DB_P("parent id was bad in move top\n");
      wxBell();
      return;
    }   

  PElement* prepend_elem = (PElement*)leftTreeCtrl->GetItemData(this_id);
  wxString prepend_name = leftTreeCtrl->GetItemText(this_id);

  wxColour prepend_colour;
  if(leftTreeBackgroundColourOn)
    prepend_colour = leftTreeCtrl->GetItemBackgroundColour(this_id);
  else if(leftTreeTextColourOn)
    prepend_colour = leftTreeCtrl->GetItemTextColour(this_id);

  wxTreeItemId new_prepended_item;
  leftTreeCtrl->SetItemData(this_id, NULL); // unhitch the data from the element for temporary deletion

  new_prepended_item = leftTreeCtrl->PrependItem(parent_id, 
						 prepend_name,
						 -1, -1,
						 prepend_elem);
  if(!new_prepended_item.IsOk())
    {
      wxMessageBox(_T("ERROR: element copy failed - could not move element."),
		   _T("Cannot Move Element!"),
		   wxICON_ERROR | wxOK);
      leftTreeCtrl->SetItemData(this_id, prepend_elem); // rehitch in event of an error
      return;
    }

  leftTreeCtrl->Delete(this_id);

  if(leftTreeBackgroundColourOn)
    {
      leftTreeCtrl->SetItemBackgroundColour(new_prepended_item, prepend_colour);
      leftTreeCtrl->SetItemTextColour(new_prepended_item, DefaultTextColour);
    }
  else if(leftTreeTextColourOn)
    {
      leftTreeCtrl->SetItemBackgroundColour(new_prepended_item, DefaultBGColour);
      leftTreeCtrl->SetItemTextColour(new_prepended_item, prepend_colour);
    }

  leftTreeCtrl->SelectItem(new_prepended_item);
}

void LaunchPADS::OnLeftMoveUp(wxCommandEvent &event)
{
  wxTreeItemId prev_sib_id, this_id, parent_id;
  this_id = leftTreeCtrl->GetSelection();

  if(!this_id.IsOk())
    {
      DB_P("this id was bad in move up\n");
      wxBell();
      return;
    }   

  if(leftTreeCtrl->GetChildrenCount(this_id) > 0)
    {
      wxMessageBox(_T("Elements with children cannot be moved."),
		   _T("Cannot Move Element!"),
		   wxICON_ERROR | wxOK);
      return;
    }

  parent_id = leftTreeCtrl->GetItemParent(this_id);

  if(!parent_id.IsOk())
    {
      DB_P("parent id was bad in move up\n");
      wxBell();
      return;
    }   


  prev_sib_id = leftTreeCtrl->GetPrevSibling(this_id);

  if(!prev_sib_id.IsOk())
    {
      DB_P("sibling id was bad in move up\n");
      wxBell();
      return;
    }   

  wxTreeItemId child_iterator;
  wxTreeItemIdValue child_cookie;
  child_iterator = leftTreeCtrl->GetFirstChild(parent_id, child_cookie);
  size_t num_children_found;
  
  for(num_children_found = 0;
      child_iterator != this_id;
      num_children_found++, 
	child_iterator = leftTreeCtrl->GetNextSibling(child_iterator))
    ;    

  DB_P("Inserting item at \"before\" = %d\n", num_children_found);

  PElement* insert_elem = (PElement*)leftTreeCtrl->GetItemData(this_id);
  wxString insert_name = leftTreeCtrl->GetItemText(this_id);

  wxColour insert_colour;
  if(leftTreeBackgroundColourOn)
    insert_colour = leftTreeCtrl->GetItemBackgroundColour(this_id);
  else if(leftTreeTextColourOn)
    insert_colour = leftTreeCtrl->GetItemTextColour(this_id);

  wxTreeItemId new_inserted_item;
  leftTreeCtrl->SetItemData(this_id, NULL); // unhitch the data from the element for temporary deletion
  
  new_inserted_item = leftTreeCtrl->InsertItem(parent_id, 
					       num_children_found-1,
					       insert_name,
					       -1, -1,
					       insert_elem);
  if(!new_inserted_item.IsOk())
    {
      wxMessageBox(_T("ERROR: element copy failed - could not move element."),
		   _T("Cannot Move Element!"),
		   wxICON_ERROR | wxOK);
      leftTreeCtrl->SetItemData(this_id, insert_elem); // rehitch in event of an error
      return;
    }

  leftTreeCtrl->Delete(this_id);

  if(leftTreeBackgroundColourOn)
    {
      leftTreeCtrl->SetItemBackgroundColour(new_inserted_item, insert_colour);
      leftTreeCtrl->SetItemTextColour(new_inserted_item, DefaultTextColour);
    }
  else if(leftTreeTextColourOn)
    {
      leftTreeCtrl->SetItemBackgroundColour(new_inserted_item, DefaultBGColour);
      leftTreeCtrl->SetItemTextColour(new_inserted_item, insert_colour);
    }

  leftTreeCtrl->SelectItem(new_inserted_item);
}

void LaunchPADS::OnLeftMoveDown(wxCommandEvent &event)
{
  wxTreeItemId next_sib_id, this_id, parent_id;
  this_id = leftTreeCtrl->GetSelection();

  if(!this_id.IsOk())
    {
      DB_P("this id was bad in move down\n");
      wxBell();
      return;
    }   

  if(leftTreeCtrl->GetChildrenCount(this_id) > 0)
    {
      wxMessageBox(_T("Elements with children cannot be moved."),
		   _T("Cannot Move Element!"),
		   wxICON_ERROR | wxOK);
      return;
    }

  parent_id = leftTreeCtrl->GetItemParent(this_id);

  if(!parent_id.IsOk())
    {
      DB_P("parent id was bad in move down\n");
      wxBell();
      return;
    }   


  next_sib_id = leftTreeCtrl->GetNextSibling(this_id);

  if(!next_sib_id.IsOk())
    {
      DB_P("sibling id was bad in move down\n");
      wxBell();
      return;
    }   


  PElement* insert_elem = (PElement*)leftTreeCtrl->GetItemData(this_id);
  wxString insert_name = leftTreeCtrl->GetItemText(this_id);

  wxColour insert_colour;
  if(leftTreeBackgroundColourOn)
    insert_colour = leftTreeCtrl->GetItemBackgroundColour(this_id);
  else if(leftTreeTextColourOn)
    insert_colour = leftTreeCtrl->GetItemTextColour(this_id);

  wxTreeItemId new_inserted_item;
  leftTreeCtrl->SetItemData(this_id, NULL); // unhitch the data from the element for temporary deletion
  
  new_inserted_item = leftTreeCtrl->InsertItem(parent_id,
					       next_sib_id,
					       insert_name,
					       -1, -1,
					       insert_elem);
  if(!new_inserted_item.IsOk())
    {
      wxMessageBox(_T("ERROR: element copy failed - could not move element."),
		   _T("Cannot Move Element!"),
		   wxICON_ERROR | wxOK);
      leftTreeCtrl->SetItemData(this_id, insert_elem); // rehitch in event of an error
      return;
    }

  leftTreeCtrl->Delete(this_id);

  if(leftTreeBackgroundColourOn)
    {
      leftTreeCtrl->SetItemBackgroundColour(new_inserted_item, insert_colour);
      leftTreeCtrl->SetItemTextColour(new_inserted_item, DefaultTextColour);
    }
  else if(leftTreeTextColourOn)
    {
      leftTreeCtrl->SetItemBackgroundColour(new_inserted_item, DefaultBGColour);
      leftTreeCtrl->SetItemTextColour(new_inserted_item, insert_colour);
    }

  leftTreeCtrl->SelectItem(new_inserted_item);
}

void LaunchPADS::OnLeftMoveBottom(wxCommandEvent &event)
{

  wxTreeItemId parent_id, this_id;
  this_id = leftTreeCtrl->GetSelection();

  if(!this_id.IsOk())
    {
      DB_P("this id was bad in move bottom\n");
      wxBell();
      return;
    }   


  if(leftTreeCtrl->GetChildrenCount(this_id) > 0)
    {
      wxMessageBox(_T("Elements with children cannot be moved."),
		   _T("Cannot Move Element!"),
		   wxICON_ERROR | wxOK);
      return;
    }
  
  parent_id = leftTreeCtrl->GetItemParent(this_id);

  if(!parent_id.IsOk())
    {
      DB_P("parent id was bad in move bottom\n");
      wxBell();
      return;
    }   

  PElement* append_elem = (PElement*)leftTreeCtrl->GetItemData(this_id);
  wxString append_name = leftTreeCtrl->GetItemText(this_id);

  wxColour append_colour;
  if(leftTreeBackgroundColourOn)
    append_colour = leftTreeCtrl->GetItemBackgroundColour(this_id);
  else if(leftTreeTextColourOn)
    append_colour = leftTreeCtrl->GetItemTextColour(this_id);

  wxTreeItemId new_appended_item;
  leftTreeCtrl->SetItemData(this_id, NULL); // unhitch the data from the element for temporary deletion

  new_appended_item = leftTreeCtrl->AppendItem(parent_id, 
					       append_name,
					       -1, -1,
					       append_elem);
  if(!new_appended_item.IsOk())
    {
      wxMessageBox(_T("ERROR: element copy failed - could not move element."),
		   _T("Cannot Move Element!"),
		   wxICON_ERROR | wxOK);
      leftTreeCtrl->SetItemData(this_id, append_elem); // rehitch in event of an error
      return;
    }

  leftTreeCtrl->Delete(this_id);

  if(leftTreeBackgroundColourOn)
    {
      leftTreeCtrl->SetItemBackgroundColour(new_appended_item, append_colour);
      leftTreeCtrl->SetItemTextColour(new_appended_item, DefaultTextColour);
    }
  else if(leftTreeTextColourOn)
    {
      leftTreeCtrl->SetItemBackgroundColour(new_appended_item, DefaultBGColour);
      leftTreeCtrl->SetItemTextColour(new_appended_item, append_colour);
    }

  leftTreeCtrl->SelectItem(new_appended_item);
}

/* ********* */
// accelerator router function
void LaunchPADS::OnLeftTreeCtrlKeys(wxCommandEvent &event)
{
  int id = event.GetId();

  switch(id)
    {

    case LEFT_MAKE_CHILD_KEY: 
      OnLeftNewChild(event);
      break;
    case LEFT_MAKE_SIBLING_KEY: 
      OnLeftNewSibling(event);
      break;
    case LEFT_GET_ATTR_KEY:
      OnLeftGetAttr(event);
      break;
    case LEFT_SET_ATTR_KEY:
      OnLeftSetAttr(event); 
      break;
    case LEFT_MOVE_NODE_TOP_KEY: 
      OnLeftMoveTop(event);
      break;
    case LEFT_MOVE_NODE_UP_KEY:       
      OnLeftMoveUp(event);
      break;
    case LEFT_MOVE_NODE_DOWN_KEY: 
      OnLeftMoveDown(event);
      break;  
    case LEFT_MOVE_NODE_BOTTOM_KEY: 
      OnLeftMoveBottom(event);
      break;     
    }
  return;

}

/* ********* */
// predicate functions
void LaunchPADS::OnLeftSetArg(wxCommandEvent &event)
{
  wxTreeItemId selected = leftTreeCtrl->GetSelection();
  PElement* elem;

  if(!selected.IsOk())
    {
      DB_P("couldn't add arg to bad selection\n");
      wxBell();
      return;
    }
  
  elem = (PElement*)leftTreeCtrl->GetItemData(selected);

  long num_lines = leftArgInput_1->GetNumberOfLines();
  wxString line;
  elem->ClearArgs();

  for(long i = 0; i < num_lines; i++)
    {
      line = leftArgInput_1->GetLineText(i);
      if(!elem->AddArg(line))
	{
	  wxLogError(_T("ERROR: could not add predicate."));
	  return;
	}
    }
  return;
}

void LaunchPADS::OnLeftClearArg(wxCommandEvent &event)
{
  wxTreeItemId selected = leftTreeCtrl->GetSelection();
  PElement* elem;

  if(!selected.IsOk())
    {
      DB_P("couldn't clear arg from bad selection\n");
      wxBell();
      return;
    }
  elem = (PElement*)leftTreeCtrl->GetItemData(selected);
  elem->ClearArgs();
  leftArgInput_1->Clear();
}

// custom entry functions
void LaunchPADS::OnLeftSetClause(wxCommandEvent &event)
{
  wxTreeItemId selected = leftTreeCtrl->GetSelection();
  PElement* elem;

  if(!selected.IsOk())
    {
      DB_P("couldn't add clause to bad selection\n");
      wxBell();
      return;
    }
  
  elem = (PElement*)leftTreeCtrl->GetItemData(selected);

  wxString text = leftTextSpecMain->GetValue();
  elem->ClearClause();

  if(!elem->AddClause(text))
    {
      wxLogError(_T("ERROR: could not add custom text entry."));
    }
  return;
}

void LaunchPADS::OnLeftClearClause(wxCommandEvent &event)
{
  wxTreeItemId selected = leftTreeCtrl->GetSelection();
  PElement* elem;

  if(!selected.IsOk())
    {
      DB_P("couldn't clear clause from bad selection\n");
      wxBell();
      return;
    }
  elem = (PElement*)leftTreeCtrl->GetItemData(selected);
  elem->ClearClause();
  leftTextSpecMain->Clear();
}


/* ********* */
// show/hide tree controls
void LaunchPADS::LeftEnableControls(bool show)
{
  //DB_P("show box = %s\n", leftStatBoxSizer_1->IsShown() ? "true" : "false");
  if(show)
    {
      leftStatBoxSizer_1->Show(false);
      leftInspector->Show(true);
      m_topWin->Show(false);
      m_midWin->Show(false);
      m_leftWin->Show(true);
      m_rightWin->Show(false);
      m_bottomWin->Show(false);
      m_leftWin->SetSashVisible(wxSASH_RIGHT, false);
      leftTreeControlState = true;
    }
  else
    {
      leftStatBoxSizer_1->Show(false);
      leftInspector->Show(false);
      m_topWin->Show(true);
      m_midWin->Show(true);
      m_leftWin->Show(true);
      m_rightWin->Show(true);
      m_bottomWin->Show(true);
      m_leftWin->SetSashVisible(wxSASH_RIGHT, true);
      leftTreeControlState = false;
    }

  wxLayoutAlgorithm layout;
  layout.LayoutFrame(this);
}

wxTreeItemId LaunchPADS::LeftGetTreeRoot(void)
{
  return leftTreeCtrl->GetRootItem();
}

void LaunchPADS::LeftBuildTree(void)
{
  wxBeginBusyCursor();
  DB_P("left build tree called\n");
  leftTreeCtrl->DeleteAllItems();
  //MidGridMakeTreeFromParse(leftTreeCtrl);
  PADSGridNode* rootGridNode = MidMakeGridNodeTreeFromGridData();
  DB_P("MidMakeGridNodeTreeFromGridData returned %d\n", rootGridNode);
  PNodeFactory pFactory;
  PSTRING gridText = midGridText.c_str();
  DB_P("gridText == %s\n", gridText.c_str());

  PNodeP* newPRoot = (Pads*)pFactory.makeEmptyPNodeFromType(PT_PADS);
  newPRoot->makeCompleteNode();
  DB_P("calling convertGridTreeToAST on %d\n", newPRoot);
  rootGridNode->convertGridTreeToAST(newPRoot, gridText);
  newPRoot->serializeXMLRepresentation(0, gridText);
  DB_P("**new XML output**\n%s\n***\n", gridText.c_str());
  leftDefTree = newPRoot;
  LeftMakeVisualTreeFromAST(leftDefTree);
  wxEndBusyCursor();
  return;
}

/* *********** */
// change tree element attributes to match controls (actually does the work)
bool LaunchPADS::LeftSetTreeElemAttr(wxTreeItemId selected)
{
  if(selected.IsOk())
    {
      wxString str;
      
      PElement* elem = (PElement*)leftTreeCtrl->GetItemData(selected);
      int type = leftTypeSelect->GetSelection();

      type += P_TERM_USER_TREE_SELECT_START; // adjust for menu offset - starts at char
      if(type >= P_END_MENU_TERMINAL_TYPES)
	type += P_TERM_END_NONTERM_START_DISTANCE; // two placeholders missing before first nonterminal

      int parent_type = elem->GetType();

      DB_P("in setattr - old type: %d %s, new type: %d %s\n", 
	   parent_type, PADS_labels[parent_type], 
	   type, PADS_labels[type]);

      if(type == pOpt || type == pArray)
	{
	  if(leftTreeCtrl->GetChildrenCount(selected, false) > 1)
	    {
	      wxMessageBox(_T("Elements of type Popt and Parray may have at most 1 child."),
			   _T("Cannot Change Attributes!"),
			   wxICON_ERROR | wxOK);
	      return false;
	    }
	}

      if(!elem->ChangeType(type))
	{
	  wxMessageBox(_T("Could not change type."),
		     _T("Cannot Change Attributes!"),
		     wxICON_ERROR | wxOK);
	  return false;
	}


      bool change_name;
      wxString new_name; 
      if(leftNameInput->GetLineLength(0) > 0)
	{
	  new_name = leftNameInput->GetValue();
	  change_name = true;
	  DB_P("Changing name in set attr to %s\n", 
	       new_name.c_str());
	}
      else
	{
	  DB_P("Not changing name in set attr....");
	  change_name = false;
	}

      bool new_sign = leftSignSelect->GetSelection() == 1 ? true : false;
      int encoding_select = leftEncodingSelect->GetSelection();
      int byte_size = (8 << (leftByteSelect->GetSelection())); // hack to change 0, 1, 2, 3 to 8, 16, 32, 64
      int term_type = leftTerminationSelect->GetSelection();

      // quick hack to make sure int is only default or fixed width
      if(type == pInt &&
	 (term_type == pendME || term_type == pendSE))
	{
	  wxMessageBox(_T("Could not change type."),
		     _T("Cannot Change Attributes!"),
		     wxICON_ERROR | wxOK);
	  return false;  
	}

      wxString stop_expr;
      bool has_stop_expr;
      if(leftStopExpr->GetLineLength(0) > 0)
	{
	  stop_expr = leftStopExpr->GetValue();
	  has_stop_expr = true;
	  elem->SetStopExpr(stop_expr);
	}
      else
	{
	  has_stop_expr = false;
	  elem->ClearStopExpr();
	}

      bool is_typedef, is_compute, is_source, is_record;
      is_typedef = leftTypedef->GetValue();
      is_compute = leftCompute->GetValue();
      is_record  = leftRecord->GetValue();
      is_source  = leftSource->GetValue();

      int flags = 0;

      flags |= is_typedef ? pTypedefFlag : 0;
      flags |= is_compute ? pComputeFlag : 0;
      flags |= is_record  ? pRecordFlag  : 0;
      flags |= is_source  ? pSourceFlag  : 0;

      if(change_name)
	{
	  DB_P("changing name to %s\n", new_name.c_str());
	  if(!elem->ChangeParams(new_name,
				 flags,
				 encoding_select, 
				 new_sign, 
				 term_type, 
				 byte_size, 
				 is_typedef
				 ))
	  {
	    wxMessageBox(_T("ERROR: could not change attributes."),
			 _T("Cannot Change Attributes!"),
			 wxICON_ERROR | wxOK);
	  }
	}
      else
	{
	  if(!elem->ChangeParams(flags,
				 encoding_select, 
				 new_sign, 
				 term_type, 
				 byte_size, 
				 is_typedef
				 ))
	  {
	    wxMessageBox(_T("ERROR: could not change attributes."),
			 _T("Cannot Change Attributes!"),
			 wxICON_ERROR | wxOK);
	  }
	}
      long num_arg_lines = leftArgInput_1->GetNumberOfLines();
      elem->ClearArgs();
      DB_P("num arg lines = %d\n", num_arg_lines);
      if(leftArgInput_1->GetLineLength(0) > 0)
	{
	  DB_P("arg input line 0 length = %d\n", leftArgInput_1->GetLineLength(0));
	  for(long i = 0; i < num_arg_lines; i++)
	    {
	      str = leftArgInput_1->GetLineText(i);
	      if(!elem->AddArg(str))
		{
		  wxMessageBox(_T("ERROR: could not change predicate."),
			       _T("Cannot Change Attributes!"),
			       wxICON_ERROR | wxOK);
		}
	    }
	}

      str = leftTextSpecMain->GetValue();
      elem->ClearClause();
      DB_P("num clause lines = %d\n", leftTextSpecMain->GetNumberOfLines());
      if(leftTextSpecMain->GetLineLength(0) > 0)
	{
	  DB_P("clause line 0 length = %d\n", leftTextSpecMain->GetLineLength(0));
	  if(!elem->ReplaceClause(str))
	    {
	      wxMessageBox(_T("ERROR: could not change custom text."),
			   _T("Cannot Change Attributes!"),
			   wxICON_ERROR | wxOK);
	    }
	}

      wxString new_name_print;
      elem->GetName(new_name);   
      if(elem->HasStopExpr())
	{
	  wxString stop_expr;
	  elem->GetStopExpr(stop_expr);
	  new_name_print.Printf("%s (:%s:) %s", PADS_generalized_names[type], stop_expr.c_str(), new_name.c_str());
	}
      else
	{
	  new_name_print.Printf("%s %s", PADS_generalized_names[type], new_name.c_str());
	}
      leftTreeCtrl->SetItemText(selected, new_name_print);

      if(elem->GetType() == pLit)
	{
	  leftTreeCtrl->SetItemBackgroundColour(selected, midDelimColour);
	}
      else if(elem->IsPTerminal())
	{
	  MidGridNewTermColour(elem->GetType(), MID_MODE_TERM_COL_STEPS);
	  leftTreeCtrl->SetItemBackgroundColour(selected, midTermColour);
	}
      else if(elem->IsPNonTerminal())
	{
	  MidGridNewUDefColour((elem->GetType() - pStartNonTerminals) * 2, MID_MODE_UDEF_COL_STEPS);
	  leftTreeCtrl->SetItemBackgroundColour(selected, midUDefColour);
	}
      
      leftTreeCtrl->Refresh();
      return true;
    }
  else
    {
      wxBell();
    }

  DB_P("selected !IsOk in set elem attr\n");
  return false;
}

bool LaunchPADS::LeftSetTreeElemName(wxTreeItemId selected, wxString &newName)
{
  // coppied from LeftSetTreeElemAttr
  PElement* elem = (PElement*)leftTreeCtrl->GetItemData(selected);
  int type = elem->GetType();
 
  wxRegEx regEx("^[[:alnum:]][[:alnum:]_]*$");
  DB_P("left context menu regex validation: %d\n", regEx.IsValid());
  if(!regEx.IsValid())
    {
      wxLogError("Could not validate name - name has not been changed.");
      return false;
    }
  if(!regEx.Matches(newName))
    {
      wxLogError("\"%s\" is not a valid element name (must include only [A-Za-z0-9_]).");
      return false;
    }
  else
    if(!elem->ChangeName(newName))
      {
	wxLogError("Could not change name.");
	return false;
      }

  wxString new_name_print;
  if(elem->HasStopExpr())
    {
      wxString stop_expr;
      elem->GetStopExpr(stop_expr);
      new_name_print.Printf("%s (:%s:) %s", PADS_generalized_names[type], stop_expr.c_str(), newName.c_str());
    }
  else
    {
      new_name_print.Printf("%s %s", PADS_generalized_names[type], newName.c_str());
    }
  leftTreeCtrl->SetItemText(selected, new_name_print);

  leftTreeCtrl->Refresh();
  return true;
}


bool LaunchPADS::LeftSetTreeElemType(wxTreeItemId selected, int newType)
{
  wxTreeItemId parentId = leftTreeCtrl->GetItemParent(selected);
  PElement* elem = (PElement*)leftTreeCtrl->GetItemData(selected);
  int parentType = pStruct;

  if(elem->IsPTerminal() && !PElement::IsPTerminal(newType))
    {
      wxLogError(_T("Elements of terminal type cannot be changed to nonterminal types."));
      return false;
    }
  else if(elem->IsPNonTerminal() && !PElement::IsPNonTerminal(newType))
    {
      wxLogError(_T("Elements of nonterminal type cannot be changed to terminal types."));
      return false;
    }

  if(newType == pOpt)
    {
      if(leftTreeCtrl->GetChildrenCount(selected, false) > 1)
	{
	  wxLogError(_T("Elements of type Popt may have at most 1 child."));
	  return false;
	}
    }


  if(parentId.IsOk())
    {
      PElement* parentElem = (PElement*)leftTreeCtrl->GetItemData(parentId);
      parentType = parentElem->GetType();
    }

  if(newType == pEnumField && parentType != pEnum)
    {
      wxMessageBox(_T("Element of type enum field must have parent of type enum."),
		   _T("Cannot Change Attributes!"),
		   wxICON_ERROR | wxOK);
      return false;
    }

  if(!elem->ChangeType(newType))
    {
      wxMessageBox(_T("Could not change type."),
		   _T("Cannot Change Attributes!"),
		   wxICON_ERROR | wxOK);
      return false;
    }

  wxString new_name;
  elem->GetName(new_name);
  wxString new_name_print;
  if(elem->HasStopExpr())
    {
      wxString stop_expr;
      elem->GetStopExpr(stop_expr);
      new_name_print.Printf("%s %s (:%s:)", PADS_generalized_names[newType], new_name.c_str(), stop_expr.c_str());
    }
  else
    {
      new_name_print.Printf("%s %s", PADS_generalized_names[newType], new_name.c_str());
    }
  leftTreeCtrl->SetItemText(selected, new_name_print);
  
  if(leftTreeBackgroundColourOn)
    {
      if(newType == pLit)
	{
	  leftTreeCtrl->SetItemBackgroundColour(selected, midDelimColour);
	}
      else if(PElement::IsPTerminal(newType))
	{
	  MidGridNewTermColour(newType, MID_MODE_TERM_COL_STEPS);
	  leftTreeCtrl->SetItemBackgroundColour(selected, midTermColour);
	}
      else if(PElement::IsPNonTerminal(newType))
	{
	  MidGridNewUDefColour((newType - pStartNonTerminals)*2, MID_MODE_UDEF_COL_STEPS);
	  leftTreeCtrl->SetItemBackgroundColour(selected, midUDefColour);
	}
      
      leftTreeCtrl->SetItemTextColour(selected, DefaultTextColour);
    }
  else if(leftTreeTextColourOn)
    {
      
      if(newType == pLit)
	{
	  leftTreeCtrl->SetItemTextColour(selected, midDelimColour);
	}
      else if(PElement::IsPTerminal(newType))
	{
	  MidGridNewTermColour(newType, MID_MODE_TERM_COL_STEPS);
	  leftTreeCtrl->SetItemTextColour(selected, midTermColour);
	    }
      else if(PElement::IsPNonTerminal(newType))
	{
	  MidGridNewUDefColour((newType - pStartNonTerminals)*2, MID_MODE_UDEF_COL_STEPS);
	  leftTreeCtrl->SetItemTextColour(selected, midUDefColour);
	}
      
      leftTreeCtrl->SetItemBackgroundColour(selected, DefaultBGColour);
    }


  leftTreeCtrl->Refresh();
  return true;
}


/* *********** */

// build code from tree structure
void LaunchPADS::LeftMakeCodeFromTree(void)
{
  // #ifdef DB_ON
  //for(int i = 0; i < pEndTerminals; i++)
  //  DB_P("Type %s %s printable\n", PADS_labels[i], 
  //	 PElement::IsPPrintableTerminal(i) ? "true" : "false");
  // #endif

  wxBeginBusyCursor();
  // the root should only have one child, so we just do this once
  //LeftMakeCodeRecur(leftTreeCtrl->GetFirstChild(root, cookie));
  wxTreeItemId selectedId = leftTreeCtrl->GetRootItem();
  PNodeWXTreeItem* newLink = (PNodeWXTreeItem*)leftTreeCtrl->GetItemData(selectedId);
  PNodeP* selectedNode = newLink->linkNode;
  PSTRING codeStr;
  selectedNode->codifyRepresentation(0, codeStr);
  wxString writeThisStr = codeStr.c_str();
  BottomClearText();
  BottomAppendText(writeThisStr, pUndefined);
  wxEndBusyCursor();
  return;
}

// recursively traverse tree and generate code from bottom up
void LaunchPADS::LeftMakeCodeRecur(wxTreeItemId this_id)
{
  wxTreeItemIdValue cookie;
  PElement *elem, *child_elem;
  elem = (PElement*)leftTreeCtrl->GetItemData(this_id);
  int type = elem->GetType();
  wxTreeItemId child_id;

  if(PElement::IsPTerminal(type))
    {
      wxString str;
      str = "CRITICAL TREE PARSE ERROR";
      BottomAppendText(str, pIllegalOp);
      return;
    }

  for(child_id = leftTreeCtrl->GetFirstChild(this_id, cookie);
      child_id.IsOk();
      child_id = leftTreeCtrl->GetNextChild(this_id, cookie) )
    {
      child_elem = (PElement*)leftTreeCtrl->GetItemData(child_id);
      // recur through all children before printing a single character
      // this will make sure definitions for nested nonterminals 
      // appear before they are used in other nonterminals

      // this is much easier than building a dependancy tree, but 
      // I imagine that will have to happen sooner or later
      if(child_elem->IsPNonTerminal())
	LeftMakeCodeRecur(child_id);

      if(child_elem->GetType() == pFunction ||
	 child_elem->GetIsTypedef())
	LeftPrintTermElem(*child_elem, type);
    }

  if(type == pOpt)
    return;


#ifdef DB_ON
  wxString name_test;
  elem->GetName(name_test);
  DB_P("printing nonterm: %s \n", name_test.c_str());
  DB_P("with type: %d %s \n", elem->GetType(), PADS_labels[elem->GetType()]);
#endif


  // write the header for this nonterminal type
  elem->SetLineNumber(BottomGetTextEnd());

  // add flag printing functionality here
  wxString str = "";
  long flags = elem->GetFlags();
  DB_P("flags = %d\n", flags);
  DB_P("pRecordFlag = %d\n", pRecordFlag);
  DB_P("flags & pRecordFlag = %d\n", (flags & pRecordFlag));


  if((flags & pTypedefFlag) != 0)
    {
      str.Printf("%s ", PADS_flag_labels[pTypedef]);
      BottomAppendText(str, type);
    }
  if((flags & pSourceFlag) != 0)
    {
      str.Printf("%s ", PADS_flag_labels[pSource]);
      BottomAppendText(str, type);
    }
  if((flags & pRecordFlag) != 0)
    {
      str.Printf("%s ", PADS_flag_labels[pRecord]);
      BottomAppendText(str, type);
    }

  if(type != pOpt)
    {
      str.Printf("%s ", PADS_names[type]);
      BottomAppendText(str, type);
      elem->GetName(str);
      str.Trim();
      str.Append("_t ");
      BottomAppendText(str, pUndefined);
      str.Printf(" {\n\t\t");
      BottomAppendText(str, pUndefined);
    }
  // fill the nonterminal type with whatever we find in the tree
  for(child_id = leftTreeCtrl->GetFirstChild(this_id, cookie);
      child_id.IsOk();
      child_id = leftTreeCtrl->GetNextChild(this_id, cookie) )
    {
      child_elem = (PElement*)leftTreeCtrl->GetItemData(child_id);
      if(child_elem->IsPTerminal() && 
	 child_elem->GetType() != pFunction
	 && child_elem->GetIsTypedef() != true)
	{
	  LeftPrintTermElem(*child_elem, type);
	}
      else if (child_elem->GetType() == pOpt) // if(child_elem->IsPNonTerminal())
	{
	  LeftCodeOptRecur(child_id);
	}
      else if (child_elem->IsPNonTerminal())// child_elem is nonterminal type
	{
	  wxString tempStr;

	  child_elem->GetName(str);
	  str.Trim();
	  str.Append("_t ");
	  BottomAppendText(str, type);
	  if(type == pArray)
	    str = "[]";
	  else
	    child_elem->GetName(str);
	  BottomAppendText(str, pUndefined);

	  if(child_elem->HasArgs())
	    {
	      int num_args = child_elem->GetNumArgs();
	      int num_args_1 = num_args-1;
	      for(int i = 0; i < num_args; i++)
		{
		  if(!child_elem->GetArgAtIndex(i, tempStr))
		    break;

		  str.Append(tempStr);
		  BottomAppendText(tempStr, pUndefined);

		  if(i != num_args_1)
		    {
		      tempStr = nl;
		      str.Append(tempStr);
		      BottomAppendText(tempStr, pUndefined);
		    }
		}
	    }

	  tempStr = ";\n\t";
	  str.Append(tempStr);
	  BottomAppendText(tempStr, pUndefined);
	}
      //else
      //{
      //  DB_P("Element does not have printable type.\n");
      //}
    }


  // close the nonterminal type
  if(type == pEnum)
    {
      wxString this_name;
      elem->GetName(this_name);

      str.Printf("%s__LP__FAKE__", this_name.c_str());
      BottomAppendText(str, pEnumField);
    }

  if(type != pOpt)
    {
      str.Printf("\n}");
      BottomAppendText(str, pUndefined);
    }

  if(elem->HasClause())
    {
      elem->GetClause(str);
      BottomAppendText(str, pUndefined);
    }
  if(type != pOpt)
    {     
      str = ";\n\n";
      BottomAppendText(str, pUndefined);
    }

}

void LaunchPADS::LeftCodeOptRecur(wxTreeItemId this_id)
{
  wxString str;
  wxTreeItemIdValue cookie;
  PElement *child_elem;
  wxTreeItemId child_id;
  int child_type;

  child_id = leftTreeCtrl->GetFirstChild(this_id, cookie);
  if(!child_id.IsOk())
    return;

  child_elem = (PElement*)leftTreeCtrl->GetItemData(child_id);
  if(child_elem == NULL)
    return;

  child_type = child_elem->GetType();

  str.Printf("%s ", PADS_names[pOpt]);
  BottomAppendText(str, child_type);
  
  DB_P("print Popt in leftcodeoptrecur\n");

  DB_P("child has type %d: %s\n", child_type, PADS_labels[child_type]);

  if(child_elem->IsPTerminal())
    {
      LeftPrintTermElem(*child_elem, pOpt);
    }
  else
    {
      child_elem->GetName(str);
      str.Trim();
      str.Append("_t ");
      BottomAppendText(str, child_type);
      child_elem->GetName(str);
      str.Append(";\n\t");
      BottomAppendText(str, pUndefined);
    }

  return;
}

// print a single terminal element using proper formatting
void LaunchPADS::LeftPrintTermElem(PElement& elem, int parent_type)
{
  int type;
  int enctype;
  bool sign;
  int bytesize;
  int termtype; 
  bool is_typedef;
  bool has_args;
  bool has_clause;

  wxString name;
  wxString str = "";
  // buffer for doing syntax highlighting on non-typedef elements
  wxString tempStr = "";

  elem.GetName(name);

  type = elem.GetType();
  enctype = elem.GetEnctype();
  termtype = elem.GetTermtype();
  sign = elem.GetSign();
  bytesize = elem.GetByteSize();
  is_typedef = elem.GetIsTypedef();
  has_args = elem.HasArgs();
  has_clause = elem.HasClause();

#ifdef DB_ON
  wxString name_test;
  elem.GetName(name_test);
  DB_P("printing term: %s \n", name_test.c_str());
  DB_P("with type: %s \n", PADS_labels[elem.GetType()]);
#endif

  elem.SetLineNumber(BottomGetTextEnd());

  if(is_typedef)
    {
      tempStr.Printf("%s ", PADS_flag_labels[pTypedef]);
      str.Append(tempStr);
    }

  if(elem.IsPPrintableTerminal())
    {
      DB_P("element %s with type %s %d is printable\n", 
	   name.c_str(), PADS_labels[type], type);
      if(type != pUserDef)
	str.Append("P");

      if(type == pB)
	{
	  str.Append("b_");
	  type = pInt; // Pb_ can only be int typed
	  // so we take a bit of a short cut
	}
      else
	{
	  switch(enctype)
	    {
	    case pchrASCII:
	      str.Append("a_");
	      break;
	    case pchrEBCDIC:
	      str.Append("e_");
	      break;
	    case pnumEBC:
	      str.Append("ebc_");
	      break;
	    case pnumBCD:
	      str.Append("bcd_");
	      break;
	    case pbinSbl:
	      str.Append("sbl_");
	      break;
	    case pbinSbh:
	      str.Append("sbh_");
	      break;
	    default:
	      break;
	    }
	}
      
      if(elem.IsPNumType())
	{
	  if(!elem.GetSign())
	    str.Append("u");
	  str.Append(PADS_names[type]);
	  if(type == pFloat)
	    {
	      if(bytesize == p64)
		str.Append("64");
	      else
		str.Append("32");
	    }
	  else
	    {
	      switch(bytesize)
		{
		case p8:
		  str.Append("8");
		  break;
		case p16:
		  str.Append("16");
		  break;
		case p32:
		  str.Append("32");
		  break;
		case p64:
		  str.Append("64");
		  break;
		default:
		  str.Append("32");
		}
	    }
	}
      else if(type == pUserDef)
	{
	  int udef_type_end;
	  int udef_name_begin;

	  udef_type_end   = name.Find(' ', false);
	  udef_name_begin = name.Find(' ', true);

	  if(udef_type_end == -1)
	    {
	      str.Append(name);
	    }
	  else
	    {
	      wxString typeStr = name.Mid(0, udef_type_end);
	      wxString nameStr = name.Mid(udef_name_begin);
	      DB_P("appending string typestr: %s and namestr: %s\n", 
		   typeStr.c_str(), nameStr.c_str());
	      name = nameStr;
	      str.Append(typeStr);
	    }

	}
      else
	{
	  str.Append(PADS_names[type]);
	}

      switch(termtype)
	{
	case pendFW:
	  str.Append("_FW ");
	  break;
	case pendME:
	  str.Append("_ME ");
	  break;
	case pendSE:
	  str.Append("_SE ");
	  break;
	default:
	  str.Append(" ");
	  break;
	}     

      if(is_typedef == false)
	BottomAppendText(str, type);

      if(elem.HasStopExpr())
	{
	  wxString stopexpr;
	  elem.GetStopExpr(stopexpr);
	  tempStr.Printf("(:");
	  str.Append(tempStr);
	  if(!is_typedef)
	    BottomAppendText(tempStr, pUndefined);
	  
	  tempStr.Printf("%s", stopexpr.c_str());
	  str.Append(tempStr);
	  if(!is_typedef)
	    BottomAppendText(tempStr, pLit);
	  
	  tempStr.Printf(":) ");
	  str.Append(tempStr);
	  if(!is_typedef)
	    BottomAppendText(tempStr, pUndefined);
	}
      else
	{
	  tempStr = " ";
	  str.Append(tempStr);
	  if(!is_typedef)
	    BottomAppendText(tempStr, pUndefined);
	}

      if(parent_type == pArray)
	tempStr = "[]";
      else
	tempStr = name;

      str.Append(tempStr);
      if(!is_typedef)
	BottomAppendText(tempStr, pUndefined);

      if(elem.HasArgs())
	{
	  // the flexibility of parameterization offered by 
	  // PADS makes me think that trying to restrict it 
	  // by enforced formatting/interfacing would end up
	  // hindering the user's efforts
	  // to this end, I'm just going to print out what
	  // they say and let them handle the errors when the
	  // time comes
	  int num_args = elem.GetNumArgs();
	  int num_args_1 = num_args-1;
	  for(int i = 0; i < num_args; i++)
	    {
	      if(!elem.GetArgAtIndex(i, tempStr))
		break;

	      str.Append(tempStr);
	      if(!is_typedef)
		BottomAppendText(tempStr, pUndefined);

	      if(i != num_args_1)
		{
		  tempStr = nl;
		  str.Append(tempStr);
		    if(!is_typedef)
		      BottomAppendText(tempStr, pUndefined);
		}
	    }
	}

      tempStr = "; ";
      str.Append(tempStr);
      if(!is_typedef)
	BottomAppendText(tempStr, pUndefined);

      if(elem.HasClause()) 
	{
	  if(elem.GetClause(tempStr))
	    {
	      tempStr.Prepend(" ");
	      str.Append(tempStr);
	      if(!is_typedef)
		BottomAppendText(tempStr, pUndefined);
	    }
	}

      if(is_typedef)
	{
	  tempStr = "\n\n";
	  str.Append(tempStr);
	}
      else
	{
	  tempStr = "\n\t";
	  BottomAppendText(tempStr, pUndefined);
	}

      if(is_typedef)
	BottomAppendText(str, pUndefined);

    }
  else if(type == pLit)
    {
      wxString tempName = name;
      if(name.Length() <= 1)
	{
	  tempName.Replace(_T("'"), _T("\\'"));
	  tempName.Replace(_T("\""), _T("\\\""));
	  str.Printf("\'%s\';\t", tempName.c_str());
	}
      else
	{
	  tempName.Replace(_T("\""), _T("\\\""));
	  str.Printf("\"%s\";\t", tempName.c_str());
	}
      BottomAppendText(str, pLit);
    }
  else if(type == pEnumField)
    {
      str.Printf("%s,\n\t", name.c_str());
      BottomAppendText(str, type);
    }
  else if(type == pCase)
    {
      str.Printf("case %s:\n\t", name.c_str());
      BottomAppendText(str, type);
      if(has_clause)
	{
	  elem.GetClause(str);
	  BottomAppendText(str, type);
	}
    }
  /*
  else if(type == pUserDef)
    {
      str.Printf("%s;\n", name.c_str());
      BottomAppendText(str, type);
    }
  */
  else if(type == pFunction)
    {
      if(elem.GetClause(str))
	{
	  /*
	  BottomPrependText(str, pStartTerminals);
	  str = "\n\n";
	  BottomPrependText(str, pStartTerminals);
	  */
	  BottomAppendText(str, pStartTerminals);
	  str = "\n\n";
	  BottomAppendText(str, pStartTerminals);

	}
    }
  
  return;
}

void LaunchPADS::LeftSetItemBackgroundColour(wxTreeItemId this_id, int type)
{
      if(leftTreeBackgroundColourOn)
	{
	  if(type == pLit)
	    {
	      leftTreeCtrl->SetItemBackgroundColour(this_id, midDelimColour);
	    }
	  else if(PElement::IsPTerminal(type))
	    {
	      MidGridNewTermColour(type, MID_MODE_TERM_COL_STEPS);
	      leftTreeCtrl->SetItemBackgroundColour(this_id, midTermColour);
	    }
	  else if(PElement::IsPNonTerminal(type))
	    {
	      MidGridNewUDefColour((type - pStartNonTerminals)*2, MID_MODE_UDEF_COL_STEPS);
	      leftTreeCtrl->SetItemBackgroundColour(this_id, midUDefColour);
	    }

	  leftTreeCtrl->SetItemTextColour(this_id, DefaultTextColour);
	}
      else if(leftTreeTextColourOn)
	{

	  if(type == pLit)
	    {
	      leftTreeCtrl->SetItemTextColour(this_id, midDelimColour);
	    }
	  else if(PElement::IsPTerminal(type))
	    {
	      MidGridNewTermColour(type, MID_MODE_TERM_COL_STEPS);
	      leftTreeCtrl->SetItemTextColour(this_id, midTermColour);
	    }
	  else if(PElement::IsPNonTerminal(type))
	    {
	      MidGridNewUDefColour((type - pStartNonTerminals)*2, MID_MODE_UDEF_COL_STEPS);
	      leftTreeCtrl->SetItemTextColour(this_id, midUDefColour);
	    }

	  leftTreeCtrl->SetItemBackgroundColour(this_id, DefaultBGColour);
	}

}

/* ************************************ */
