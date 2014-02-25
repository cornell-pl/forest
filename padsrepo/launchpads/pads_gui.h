/* **************************************
 * Mark Daly
 * 2005.8.18
 * v. 0.0.21
 * PADS GUI project (preliminary tests)
 * Borrows from wxWidgets documentation and sample files
 * *********************************** */

#ifndef _PADSGUI_H_INCLUDED
#define _PADSGUI_H_INCLUDED

// from the wxWiki
#if defined(__GNUG__) && ! defined(__APPLE__)
#pragma interface "pads_gui.h"
#endif

//debug macros
#define DB_P(a, ...)   fprintf(stderr, a, ## __VA_ARGS__ ) // from RHEL documentation, 6.15
  // doesn't work with vc++!!!
//#define DBS // start debug
//#define DBE // end debug
//post debug macros
//#define DB_P(a, ...)  // a, ## __VA_ARGS__
//#define DB_ON 1
//#define DBS goto post_debug;
//#define DBE post_debug:

#ifdef __WXMSW__
  const static long mywxBORDER = wxSIMPLE_BORDER;
#else
  const static long mywxBORDER = wxSUNKEN_BORDER;
#endif

static const wxChar *lpNum_Chars[] = 
  {
    "1", "2", "3", "4", "5", 
    "6", "7", "8", "9", "0"
  };

#include "lp_event_ids.h"
#include "lp_constants.h"

/* ***Core Application Classes *** */

// create application wrapper from virtual wxApp class
class LaunchPADSApplication : public wxApp
{
 public:
  virtual bool OnInit();  
};

// create frame wrapper from virtual wxFrame class
class LaunchPADS : public wxFrame
{
  /* **************************** */
 public:
  /* **************************** */
  LaunchPADS(const wxChar *title,
	     wxString argv0,
	     int xpos, int ypos,
	     int width, int height);
  ~LaunchPADS();

  wxString appPath; // path to the executable - to load/save config file
  wxString configFilePath;  // path to config file in app directory

  // I/O for configuration file - doesn't ever touch MSW registry
  wxFileConfig *fConfig;

  // main frame controls 
  wxMenuBar    *menuBar;

  wxMenu       *fileMenu;
  //wxMenu       *editMenu;
  wxMenu       *windowMenu;
  wxMenu       *toolsMenu;
  wxMenu       *optionsMenu;
  wxMenu       *helpMenu;

  wxStatusBar  *statusBar;

  wxToolBar    *toolBar;
  //wxButton     *toolBarOpen;

  // sash layout size variables
  int topWinX;
  int topWinY;
  int midWinX;
  int midWinY;
  int leftWinX;
  int leftWinY;
  int rightWinX;
  int rightWinY;
  int bottomWinX;
  int bottomWinY;

  void OnKeyPress(wxCommandEvent &event);
  void OnContextMenu(wxContextMenuEvent &event);
  void OnSize (wxSizeEvent    &event);
  void OnSashDrag (wxSashEvent    &event);
  void OnOpenFile (wxCommandEvent &event);
  void OnSaveFile (wxCommandEvent &event);
  void OnSaveAsFile (wxCommandEvent &event);
  void OnAbout    (wxCommandEvent &event);
  void OnExit     (wxCommandEvent &event);
  void OnWinSwap  (wxCommandEvent &event);
  void OnWinExclusive  (wxCommandEvent &event);
  void OnChangeCheck   (wxCommandEvent &event);
  void OnViewReset     (wxCommandEvent &event);
  void OnToolMenu      (wxCommandEvent &event);
  void OnToolbarPress  (wxCommandEvent &event);
  void OnPrefsDialog   (wxCommandEvent &event);
  void OnTopOpen(wxCommandEvent &event);
  void OnTopImport(wxCommandEvent &event);
  void OnTopClear(wxCommandEvent &event);

  void SetDefaultSettings(void);
  void ChangeSettings( 
		      bool saveLayoutOnExit_new,
		      bool colourModeOn_new,
		      bool midGridColNumberLabels_new,
		      int  topNumLinesOnImport_new,
		      bool midUDefErrorMBoxOn_new,
		      bool midTermErrorMBoxOn_new,
		      bool rightBuildTreeMBoxOn_new,
		      bool leftTreeBackgroundColourOn_new,
		      bool leftControlsWarnings_new,
		      bool leftAttributeChangeWarnings_new, 
		      bool midGridWriteTypeOnNext_new, 
		      bool leftTreeTextColourOn_new, 
		      bool xmlMessagesOnSave_new, 
		      wxString &PADSCpath_new,
		      wxString &PADSCinc_new,
		      wxString &ASTinc_new,
		      wxString &CCpath_new,
		      wxString &CCargs_new, 
		      bool leftControlSafeMode_new,
		      wxString &PADXPath_new);
  void GetSettings(
		   bool &saveLayoutOnExit_new,
		   bool &colourModeOn_new,
		   bool &midGridColNumberLabels_new,
		   int  &topNumLinesOnImport_new,
		   bool &midUDefErrorMBoxOn_new,
		   bool &midTermErrorMBoxOn_new,
		   bool &rightBuildTreeMBoxOn_new,
		   bool &leftTreeBackgroundColourOn_new,
		   bool &leftControlsWarnings_new,
		   bool &leftAttributeChangeWarnings_new, 
		   bool &midGridWriteTypeOnNext_new, 
		   bool &leftTreeTextColourOn_new, 
		   bool &xmlMessagesOnSave_new,
		   wxString &PADSCpath_new,
		   wxString &PADSCinc_new,
		   wxString &ASTinc_new,
		   wxString &CCpath_new,
		   wxString &CCargs_new, 
		   bool &leftControlSafeMode,
		   wxString &PADXPath_new);
  void SaveLayout(void);

  void ToolProcessAcc(int whereFrom);
  void ToolProcessFmt(int whereFrom);
  void ToolProcessXml(int whereFrom);
  bool ToolGetNonterminalNames(wxArrayString& arrStr, bool useMadeCode,
			       wxString& headerPath);

  void ExecPADSC(bool useGeneratedCode);
  void ExecCCTest(void);
  void ExecCCTool(int numArgs, 
		  wxString &filePath,
		  wxString &fileName,
		  wxString &inputPath,
		  wxString &outputPath,
		  wxArrayString &includeDirs, 
		  wxString &incPath);

  void TopGetFont(wxFont &font);
  void TopSetFont(wxFont &font);

  void OnMidGridSelect(wxGridRangeSelectEvent &event);
  void OnMidGridDClick(wxGridEvent &event);
  void OnMidGridRightDClick(wxGridEvent &event);

  void MidSetEditMode(int new_mode);
  void MidGridUseAsInput(wxString& wxstr);
  void MidGridNextLevel(void);
  void MidGridLastLevel(void);
  void MidGridStartOver(void);
  bool MidGridReadyForTree(void);
  void MidGridMakeTreeFromParse(wxTreeCtrl* tree);
  static bool IsSymbol(char c);
  static bool IsFloat(char c);
  static bool IsHex(char c);
  static bool IsSign(char c);
  int  MidGridGetMode(void);
  void MidGridApplySelectedStats(void);
  wxColour    midNoneColour;
  wxColour    midDelimColour;
  wxColour    midTermColour;
  wxColour    midUDefColour;
  void MidGridNewUDefColour(int a, int b);
  void MidGridNewTermColour(int a, int b);


  void OnLeftEnableControls(wxCommandEvent &event);
  void OnLeftTreeContextMenu(wxCommandEvent &event);
  void OnLeftStopExprContextMenu(wxCommandEvent &event);
  void OnLeftArgContextMenu(wxCommandEvent &event);
  void OnLeftSelectType(wxCommandEvent &event);
  void OnLeftSetAttr(wxCommandEvent &event);
  void OnLeftGetAttr(wxCommandEvent &event);
  void OnLeftClearControls(wxCommandEvent &event);
  void OnLeftDeleteTree(wxCommandEvent &event);
  void OnLeftNewChild(wxCommandEvent &event);
  void OnLeftNewSibling(wxCommandEvent &event);
  void OnLeftDeleteNode(wxCommandEvent &event);
  void OnLeftDeleteChildren(wxCommandEvent &event);
  void OnLeftMoveTop(wxCommandEvent &event);
  void OnLeftMoveUp(wxCommandEvent &event);
  void OnLeftMoveDown(wxCommandEvent &event);
  void OnLeftMoveBottom(wxCommandEvent &event);
  void OnLeftTreeCtrlKeys(wxCommandEvent &event);
  void OnLeftSetArg(wxCommandEvent &event);
  void OnLeftClearArg(wxCommandEvent &event);
  void OnLeftSetClause(wxCommandEvent &event);
  void OnLeftClearClause(wxCommandEvent &event);

  void OnLeftTreeCtrlSelect(wxTreeEvent &event);
  void OnLeftTreeBeginDrag(wxTreeEvent &event);
  void OnLeftTreeEndDrag(wxTreeEvent &event);

  void LeftEnableControls(bool show);
  wxTreeItemId LeftGetTreeRoot(void);
  void LeftBuildTree(void);
  void LeftMakeCodeFromTree(void);
  void LeftMakeVisualTreeFromAST(PNodeP* rootASTNode);

  void OnRightModeSelect(wxCommandEvent &event);
  void OnRightFragment(wxCommandEvent &event);
  void OnRightBack(wxCommandEvent &event);
  void OnRightReset(wxCommandEvent &event);
  void OnRightBuildTree(wxCommandEvent &event);
  void OnRightMakeCode(wxCommandEvent &event);
  void OnRightSaveState(wxCommandEvent &event);
  void OnRightExportCode(wxCommandEvent &event);

  void RightEnableComboBox_1(bool choice);
  void RightEnableComboBox_2(bool choice);
  int  RightGetTermType(void);
  int  RightGetNontermType(void);
  void RightSetAttr(int mode, int type, int udef);
  bool RightSetRadioMode(int mode);
  void RightCycleOptions(int);


  void BottomAppendText(wxString &text, int mode = pUndefined);
  void BottomPrependText(wxString &text, int mode = pUndefined);
  long BottomGetTextEnd(void);
  void BottomSetTextVisible(wxTextPos pos);
  void BottomClearText(void);
  wxString BottomGetText();

  void BottomSetTermColour(wxColour &TermColour_new);
  void BottomSetNontermColour(wxColour &NontermColour_new);
  void BottomSetLitColour(wxColour &LitColour_new);
  void BottomSetErrorColour(wxColour &ErrorColour_new);
  void BottomSetOtherColour(wxColour &OtherColour_new);
  wxColour BottomGetTermColour();
  wxColour BottomGetNontermColour();
  wxColour BottomGetLitColour();
  wxColour BottomGetErrorColour();
  wxColour BottomGetOtherColour();  

  void BottomGetFont(wxFont &font);
  void BottomSetFont(wxFont &font);

  wxColour DefaultTextColour;
  wxColour DefaultBGColour;

  friend class StateSequenceDialog;
  friend class PADXDialog;
  friend class PADSLangInspector;
  /* **************************** */
 protected:
  /* **General******************* */
  bool savePathSet;
  wxString savePath;

  bool loadPathSet;
  wxString loadPath;

  bool saveLayoutOnExit; // write sash configuration to config file on exit
  bool colourModeOn;     // use/do not use color on all controls

  wxString nl;

  /* **EXEC********************** */
  wxString PADSCpath;
  wxString PADSCcomppath;
  wxString PADSCinc;
  wxString ASTinc;
  wxString ASTincludedir;
  wxString CCpath;
  wxString CCargs;
  wxString LIBdir;

  wxString PADXPath;

  wxString workingFileName;
  wxString workingFilePath;

  wxString GetPADXPath(void);

  /* **XML*********************** */
  bool xmlMessagesOnSave;

  int xmlNumLoadSteps;
  int xmlCurrentState;

  int  ISO_8859_1_process(wxString &str);
  int  TreeLoadXMLRecur(wxTreeItemId parent, 
			TiXmlElement *sibling, 
			int depth);
  void TreeBuildXMLRecur(wxTreeItemId firstSibling,
			 TiXmlElement *parent,
			 wxTreeItemIdValue cookie);

  // 
  void LoadState(int stateNum, bool loadPlayback);
  void LoadStateWrapped(int stateNum, bool loadPlayback);
  int  GetLoadStateNumSteps();
  void SaveStateAs(int stateNum, bool catState);
  void SaveState(int stateNum, bool catState);
  void SaveStateWrapped(int stateNum, bool catState);

  int LoadPXMLFile(wxString& loadPath);
  int SavePXMLFile(wxString& savePat);

  /* **TOOLS********************* */
  wxBitmap      *wizBitmap;
  wxArrayString  AccStrings;
  //FmtWizard     *FmtWiz;
  wxArrayString  FmtStrings;
  //XmlWizard     *XmlWiz;
  wxArrayString  XmlStrings;

  bool CgGenerateCodeFromParams(int genType,
				wxString &filePath, 
				wxString &fileName,
				wxString &includePath, 
				wxString &includeName);

  bool tGenCode;
  bool tCompileCode;
  bool tRunTool;

  /* **TOP*********************** */
  wxSashLayoutWindow *m_topWin;

  wxPanel            *m_topWinPanel; 
  wxBoxSizer         *topWinSizer;
  wxGridSizer        *topWinButtonSizer;
  wxButton           *topOpenFile;
  wxButton           *topImport;
  wxButton           *topClear;
  wxCheckListBox     *topCheckList;

  wxFont              topFont;

  int topCheckedEntry;  // entry most recently checked by user
  
  void TopInitFrame(long panelflags, long listflags, long buttonflags);
  void TopMakeCheckList(long flags);
  void TopMakeButtons(long flags);

  int topNumLinesOnImport; // number of lines to import into top checklistbox

  /* **MID*********************** */
  wxSashLayoutWindow* m_midWin;

  wxPanel     *m_midWinPanel;
  wxBoxSizer  *midWinSizer;
  wxGrid      *midGrid;

  wxColour    midHighlightColour;
  wxColour    midNormalColour;

  int  midCurrentRow;
  int  midEditMode;
  bool midGridExists; // mid grid has been created and can be written to

  wxString midGridText;
  int  midGridTextLength;

  int  midCurrentID;

  int *midGridDepth;  // last fragmentation depth at which each cell was highlighted
  int *midGridMode;
  // int *midGridTermType;
  int *midGridType;   // types (terminal and nonterminal) for each cell
  int *midGridUDefID; // user-selected-region identifiers (for separating contiguous regions of similar types) for each cell

  int **midGridTypeLevels;   // types for each fragmentation step
  int **midGridUDefIDLevels; // region identifiers for each fragmentation step
  int midGridMaxLevels;  // maximum number of fragmentation steps - this could be made leaner, but...
  
  void MidInitFrame(long panelflags, 
		    long gridflags);
  void MidGridTest(void);
  void MidGridRemake(wxString& wxstr,
		     long gridflags);
  void MidGridLevelsResize(void);
  void MidGridRecur(wxTreeItemId parent, 
		    wxTreeCtrl* tree,
		    int my_type,
		    int depth,
		    int left, 
		    int right);
  int MidGridTermGuessMode(int left, int right);

  PADSGridNode* MidMakeGridNodeTreeFromGridData();
  PADSGridNode* MidMakeGridNodeTreeRecur(PADSGridNode* parent, int left, int right, int depth);

  PADSGridNode* gridTreeRoot; 

  bool midUDefErrorMBoxOn;     // show message box on nonterminal type selection errors in grid
  bool midTermErrorMBoxOn;     // show message box on terminal type selection errors in grid
  bool midGridColNumberLabels; // use numbers as labels for grid columns
  bool midGridWriteTypeOnNext; // write type of selected regions in previous level after each step of fragmentation

  /* **LEFT********************** */
  wxSashLayoutWindow* m_leftWin;

  wxPanel          *m_leftWinPanel;
  wxBoxSizer       *leftWinSizer;
  wxBoxSizer       *leftWinSizer_2;
  wxTreeCtrl       *leftTreeCtrl;

  bool              leftTreeControlState;  // tree controls shown/not shown
  
  wxButton         *leftEnableControls;

  wxMenu           *leftTreeMenu;
  wxArrayString     leftTypeStrings;

  wxStaticBoxSizer *leftStatBoxSizer_1;
  wxBoxSizer       *leftSubSizer_1;
  wxComboBox       *leftTypeSelect;
  wxTextCtrl       *leftNameInput;
  wxTextValidator  *leftNameValid; 
  wxArrayString    *leftNameValidIncludes;

  wxBoxSizer       *leftSubSizer_7;
  wxGridBagSizer   *leftSubSizer_7sub1;
  wxGridBagSizer   *leftSubSizer_7sub2;
  wxGridBagSizer   *leftSubSizer_7sub3;
  wxButton         *leftClearControls;
  wxButton         *leftSetAttr;
  wxButton         *leftGetAttr;
  wxButton         *leftDeleteTree;

  wxButton         *leftMoveNodeUp;
  wxButton         *leftMoveNodeRight;
  wxButton         *leftMoveNodeDown;
  wxButton         *leftMoveNodeLeft;
  wxGridBagSizer   *leftSubSizer_2;
  wxGridBagSizer   *leftSubSizer_2sub1;
  wxButton         *leftMakeChild;
  wxButton         *leftMakeSibling;
  wxStaticLine     *leftSubSizer2StatLine_1;
  wxGridBagSizer   *leftSubSizer_2sub2;
  wxButton         *leftDeleteNode;
  wxButton         *leftDeleteChildren;

  wxBoxSizer       *leftSubSizer_3;
  wxBoxSizer       *leftSubSizer_3sub1;
  wxChoice         *leftSignSelect;
  wxChoice         *leftEncodingSelect;
  wxChoice         *leftTerminationSelect;
  wxChoice         *leftByteSelect;
  wxBoxSizer       *leftSubSizer_3sub2;
  wxChoice         *leftBinaryEnc;
  wxStaticText     *leftLParen;
  wxTextCtrl       *leftStopExpr;
  wxStaticText     *leftRParen;
  wxTextValidator  *leftStopExprValid;
  wxMenu           *leftStopExprMenu;


  wxBoxSizer       *leftSubSizer_4;
  wxCheckBox       *leftTypedef;
  wxCheckBox       *leftCompute;
  wxCheckBox       *leftRecord;
  wxCheckBox       *leftSource;

  wxStaticBoxSizer *leftSubSizer_5;
  wxTextCtrl       *leftArgInput_1; 
  wxValidator      *leftArg1Valid;
  wxGridSizer      *leftSubSizer_5sub1;
  wxButton         *leftAppendArgument;
  wxButton         *leftRemoveArgument;
  wxChoice         *leftAvailableClauseTypes;
  wxMenu           *leftClauseTypeMenu;

  wxStaticBoxSizer *leftSubSizer_6;
  wxTextCtrl       *leftTextSpecMain;
  wxGridSizer      *leftSubSizer_6sub1;
  wxButton         *leftSetClause;
  wxButton         *leftClearClause;

  // ADD members for record modifier flags (exclusive/inclusive? - must learn)
  
  wxComboBox       *leftClauseSelect; 
  wxButton         *leftAddClause;
  wxTextCtrl       *leftClauseInput;

  wxString           pXMLDefPath;
  PNodeP            *leftDefTree;
  PADSLangInspector *leftInspector;

  void LeftBuildControlPanel(long buttonflags, long comboflags, 
			     long radioflags,  long textflags);
  void LeftInitFrame(long panelflags, long treeflags, long buttonflags);
  void LeftTreeTest(void);
  void LeftTreeInit(void);
  bool LeftSetTreeElemAttr(wxTreeItemId selected);
  bool LeftSetTreeElemName(wxTreeItemId selected, wxString &newName);
  bool LeftSetTreeElemType(wxTreeItemId selected, int newType);
  void LeftSetItemBackgroundColour(wxTreeItemId this_id, int type);

  void LeftMakeCodeRecur(wxTreeItemId this_id);
  void LeftCodeOptRecur(wxTreeItemId this_id);
  void LeftPrintTermElem(PElement& elem, int parent_type);

  wxTreeItemId LeftRecurThroughSimpleTree(PADSPNodeTreeNode* thisNode, wxTreeItemId& parentID);
  void LeftSetTreeElementColourFromType(int type, wxColour& setMe);

  bool leftTreeBackgroundColourOn;   // change background colors of tree items to match item type
  bool leftTreeTextColourOn;         // change text colors of tree items to match item type
  bool leftControlsWarnings;         // show message box on tree control warnings/errors
  bool leftAttributeChangeWarnings;  // show message box on attribute get/set
  bool leftControlSafeMode;          // deactivate controls not appropriate for selected type

  bool leftSafeModeAltered;  // flag that controls should be reset to active if safe mode is turned off

  /* **RIGHT********************* */
  wxSashLayoutWindow* m_rightWin;

  wxPanel           *m_rightWinPanel;
  wxBoxSizer        *rightWinSizer;
  wxGridBagSizer    *rightWinButtonSizer_1;
  wxBoxSizer        *rightWinButtonSizer_2;
  wxButton          *rightFragment;
  wxButton          *rightReset;
  wxButton          *rightBackOneLevel;
  wxStaticLine      *rightStatLine_1;
  //wxButton     *rightDelimMode;
  //wxButton     *rightFieldMode;
  wxRadioBox        *rightModeSelect;
  wxStaticBoxSizer  *rightStatBox_1;
  wxComboBox        *rightTypeSelect;
  wxStaticBoxSizer  *rightStatBox_2;
  wxComboBox        *rightStructSelect;
  wxStaticLine      *rightStatLine_2;
  wxFlexGridSizer   *rightWinButtonSizer_3;
  wxButton          *rightMakeTree;
  wxStaticLine      *rightStatLine_3;
  wxButton          *rightMakeCode;
  wxButton          *rightExportCode;
  wxButton          *rightSaveSession;

  void RightInitFrame(long panelflags, long buttonflags, 
		      long radioflags, long comboflags);
  void RightMakeButtons_1 (long flags);
  void RightMakeButtons_2 (long flags);
  void RightMakeRadio_1 (long flags);
  void RightMakeComboBox_1(long flags);
  void RightMakeComboBox_2(long flags);

  bool rightBuildTreeMBoxOn;      // show message box before building tree (and erasing previous tree)

  /* **BOTTOM******************** */
  wxSashLayoutWindow *m_bottomWin;

  wxPanel    *m_bottomWinPanel; 
  wxBoxSizer *bottomWinSizer;
  wxTextCtrl *bottomCodeDisplay; 

  wxFont      bottomFont;

  bool        bottomCodeMade;
  bool        bottomCodeSaved;
  wxString    bottomCodePath;
  wxString    bottomCodeName;

  void BottomInitFrame(long panelflags, 
		       long textflags);

  wxColour bottomTermColour;
  wxColour bottomNontermColour;
  wxColour bottomLitColour;
  wxColour bottomErrorColour;
  wxColour bottomOtherColour;

  /* **************************** */
DECLARE_EVENT_TABLE()
};

#endif
