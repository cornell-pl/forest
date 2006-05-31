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

#define APP_NAME_STRING "LaunchPADS"
#define VENDOR_NAME_STRING "PADS_Project_Group"
#define CONFIG_FILE_STRING "LaunchPADS_Config.txt"

#define GUI_VERSION "0.0.21"

#define DEFAULT_BUFFER_SIZE 0x800
#define MAX_FILE_READ_SIZE 1<<22

#define MAIN_WIN_HEIGHT 832
#define MAIN_WIN_WIDTH  616

#define TOP_WIN_DEFAULT_X 100
#define TOP_WIN_DEFAULT_Y 125
#define TOP_NUM_LINES_MAX 100

#define BOTTOM_WIN_DEFAULT_X 500
#define BOTTOM_WIN_DEFAULT_Y 50

#define MID_WIN_DEFAULT_X 200
#define MID_WIN_DEFAULT_Y 250
#define MID_MODE_NONE_COLOUR 255, 255, 255
#define MID_MODE_DELIM_COLOUR 128, 255, 128
#define MID_MODE_TERM_COLOUR 128, 128, 255
#define MID_MODE_TERM_VARCOL(a, b) \
(64 + ((96 * a) / b)  % 64), \
(96 + (((255 * a * 2) / b) % 159)), \
255
#define MID_MODE_TERM_COL_STEPS ((pCase - pStartTerminals))
#define MID_MODE_UDEF_COLOUR 255, 152, 152
#define MID_MODE_UDEF_VARCOL(a, b) 255, \
((192+ ((a % 32) * b)) %224), \
((192+ ((a % 8) * b)) %224)
//((192+ ((a * ((int)(3.141592654 / 2.718281828)) % 32) * b)) %255), \
//((192+ ((a * ((int)(1.618033989) << 1 ) % 8) * b)) %255)
//((192+ (a / (a % 7 + 1)) * b)%255), \
//((192+ (a / ((a + 31) % 3 + 1)) * b) %255)
#define MID_MODE_UDEF_COL_STEPS 51
#define MID_GRID_INIT_MAX_STEPS 1024
#define MID_GRID_STEPS_MULTIPLIER 4
#define MID_NO_ID -1
#define MID_MODE_NONE_NO_COLOUR 255, 255, 255
#define MID_MODE_DELIM_NO_COLOUR 224, 224, 224
#define MID_MODE_TERM_NO_COLOUR 128, 128, 128
#define MID_MODE_UDEF_NO_COLOUR 192, 192, 192

#define RIGHT_WIN_DEFAULT_X 155
#define RIGHT_WIN_DEFAULT_Y 300

#define LEFT_WIN_DEFAULT_X 175
#define LEFT_WIN_DEFAULT_Y 300

#define BOTTOM_TERM_COLOUR    16, 16, 152
#define BOTTOM_NONTERM_COLOUR 224, 158, 64
#define BOTTOM_LIT_COLOUR     16, 152, 16
#define BOTTOM_ERROR_COLOUR   255, 0, 0
#define BOTTOM_OTHER_COLOUR   0, 0, 0

#define PADS_MANUAL_URI "http://www.padsproj.org/doc/index.html"

#define XML_ENCODING_VER "1.0"
#define XML_ENCTYPE     "ISO-8859-1"
#define XML_STANDALONE   "no"

#define LP_OSX_METAL

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

enum
  {
    LP_EXIT = wxID_HIGHEST + 1,
    LP_OPEN,
    LB_SAVE,
    LP_ABOUT,
    LP_UNDO, 
    LP_CUT,
    LP_COPY,
    LP_PASTE,

    ID_WINDOW_TOP,
    ID_WINDOW_MID,
    ID_WINDOW_LEFT,
    ID_WINDOW_RIGHT,
    ID_WINDOW_BOTTOM,

    WIN_SWAP_TOP,
    WIN_SWAP_MID,
    WIN_SWAP_LEFT,
    WIN_SWAP_RIGHT,
    WIN_SWAP_BOTTOM,
    WIN_EXCLUSIVE_TOP,
    WIN_EXCLUSIVE_MID,
    WIN_EXCLUSIVE_LEFT,
    WIN_EXCLUSIVE_RIGHT,
    WIN_EXCLUSIVE_BOTTOM,
    WIN_RESET_VIEW,

    TOOLS_PADSC,
    TOOLS_TOXML,
    TOOLS_TOFMT,
    TOOLS_ACCUM,
    TOOLS_FILTER,
    TOOLS_RECORD_DEMO,
    TOOLS_PLAYBACK_DEMO,

    WIN_PREFS_DIALOG,
    WIN_OPEN_HELP,
    WIN_OPEN_PADS_MANUAL,

    TOOLBAR_OPEN,
    TOOLBAR_SAVE,
    TOOLBAR_SAVEAS,
    TOOLBAR_PADS_MANUAL,
    TOOLBAR_HELP,
    TOOLBAR_LAUNCH,
    TOOLBAR_XML,
    TOOLBAR_FMT,
    TOOLBAR_ACCUM,
    TOOLBAR_PADX,
    TOOLBAR_SUM,
    TOOLBAR_FILTER,
    TOOLBAR_END_CMDS, //convenience place holder

    TOP_OPEN_FILE,
    TOP_CLEAR,
    TOP_IMPORT_STRING,
    TOP_LIST_BOX_CHECK,

    MID_GRID_EVENT,
    MID_SELECT_NONE,
    MID_SELECT_DELIM,    
    MID_SELECT_TERM,
    MID_SELECT_UDEF,
    MID_SELECT_GET,

    LEFT_ENABLE_CONTROLS,
    LEFT_TYPE_SELECT,
    LEFT_NAME_INPUT,
    LEFT_SIGN_SELECT,
    LEFT_ENCODING_SELECT,
    LEFT_TERM_SELECT,
    LEFT_BIN_SELECT,
    LEFT_BYTE_SELECT,
    LEFT_TYPEDEF_CHECK,
    LEFT_STOP_EXPR,
    LEFT_SET_SE_DEFAULT,
    LEFT_SET_SE_FW,
    LEFT_SET_SE_REGEX,
    LEFT_SET_SE_DATE,
    LEFT_SET_SE_EBC,
    LEFT_SET_SE_BCD,
    LEFT_SELECT_CLAUSE_TYPE,
    LEFT_APPEND_ARGUMENT,
    LEFT_REMOVE_ARGUMENT,
    LEFT_ARG_INPUT_1,
    LEFT_ARG1_RCLICK,
    LEFT_QUALIFIER_MENU_0, 
    LEFT_QUALIFIER_MENU_1, 
    LEFT_QUALIFIER_MENU_2, 
    LEFT_QUALIFIER_MENU_3, 
    LEFT_QUALIFIER_MENU_4, 
    LEFT_QUALIFIER_MENU_5, 
    LEFT_QUALIFIER_MENU_6, 
    LEFT_QUALIFIER_MENU_7, 
    LEFT_QUALIFIER_MENU_8, 
    LEFT_QUALIFIER_MENU_9, 
    LEFT_QUALIFIER_MENU_10,
    LEFT_QUALIFIER_MENU_11, 
    LEFT_QUALIFIER_MENU_12, 
    LEFT_QUALIFIER_MENU_13, 
    LEFT_QUALIFIER_MENU_14, 
    LEFT_QUALIFIER_MENU_15, 
    LEFT_QUALIFIER_MENU_16, 
    LEFT_QUALIFIER_MENU_17, 
    LEFT_QUALIFIER_MENU_18,
    LEFT_QUALIFIER_MENU_19, 
    LEFT_TEXTSPEC_MAIN,
    LEFT_SET_CLAUSE,
    LEFT_CLEAR_CLAUSE,

    LEFT_CLEAR_CONTROLS,
    LEFT_SET_ATTR,
    LEFT_GET_ATTR,
    LEFT_DELETE_TREE,
    LEFT_MAKE_CHILD,
    LEFT_MAKE_SIBLING,
    LEFT_DELETE_NODE,
    LEFT_DELETE_CHILDREN,
    LEFT_MOVE_NODE_TOP,
    LEFT_MOVE_NODE_UP,
    LEFT_MOVE_NODE_DOWN,
    LEFT_MOVE_NODE_BOTTOM,

    LEFT_MAKE_CHILD_KEY,
    LEFT_MAKE_SIBLING_KEY,
    LEFT_GET_ATTR_KEY,
    LEFT_SET_ATTR_KEY,
    LEFT_MOVE_NODE_TOP_KEY,
    LEFT_MOVE_NODE_UP_KEY,
    LEFT_MOVE_NODE_DOWN_KEY,  
    LEFT_MOVE_NODE_BOTTOM_KEY,

    LEFT_FIND_IN_CODE_MENU,
    LEFT_CHANGE_NAME_MENU,
    LEFT_CHANGE_TYPE_MENU,
    LEFT_SET_ATTR_MENU,
    LEFT_GET_ATTR_MENU,
    LEFT_MAKE_CHILD_MENU,
    LEFT_MAKE_SIBLING_MENU,
    LEFT_MOVE_NODE_TOP_MENU,
    LEFT_MOVE_NODE_UP_MENU,
    LEFT_MOVE_NODE_DOWN_MENU,
    LEFT_MOVE_NODE_BOTTOM_MENU,


    RIGHT_FRAGMENT,
    RIGHT_RESET,
    RIGHT_BACK,
    //RIGHT_DELIM_MODE,
    //RIGHT_FIELD_MODE,
    RIGHT_MODE_SELECT,
    RIGHT_FIELD_SELECT,
    RIGHT_MAKE_TREE,
    RIGHT_MAKE_CODE,
    RIGHT_EXPORT_CODE,
    RIGHT_SAVE_SESSION,

    PREFS_SAVE_LAYOUT_EXIT,
    PREFS_SAVE_LAYOUT_NOW,
    PREFS_NO_COLOUR,
    PREFS_XML_MESSAGE_ON_SAVE,

    PREFS_TOP_SET_FONT,
    PREFS_TOP_EDIT_NUM_LINES,
    PREFS_TOP_SET_NUM_LINES,

    PREFS_MID_TERM_WARNINGS,
    PREFS_MID_NONTERM_WARNINGS,
    PREFS_MID_COLUMN_LABELS,
    PREFS_MID_BUILD_WARNING,
    PREFS_MID_WRITE_NEW_TYPE,

    PREFS_LEFT_ACTIVE_WARNINGS,
    PREFS_LEFT_CONTROL_SAFE_MODE,
    PREFS_LEFT_BG_COLOUR,

    PREFS_BOTTOM_COLOUR_TERM,
    PREFS_BOTTOM_COLOUR_NONTERM,
    PREFS_BOTTOM_COLOUR_LIT,
    PREFS_BOTTOM_COLOUR_ERROR,
    PREFS_BOTTOM_COLOUR_OTHER,
    PREFS_BOTTOM_SET_FONT,

    SD_EXECUTE, 

    ACC_WIZ_EXPERT_MODE,
    ACC_WIZ_HELP_SELECT,
    ACC_WIZ_NUM_RECS_SPIN,
    ACC_WIZ_SET_INCLUDE_PATH,
    ACC_WIZ_RESET_DEFAULTS,
    FMT_WIZ_EXPERT_MODE,
    FMT_WIZ_HELP_SELECT,
    FMT_WIZ_NUM_RECS_SPIN,
    FMT_WIZ_SET_INCLUDE_PATH,
    FMT_WIZ_RESET_DEFAULTS,
    XML_WIZ_EXPERT_MODE,
    XML_WIZ_HELP_SELECT,
    XML_WIZ_NUM_RECS_SPIN,
    XML_WIZ_SET_INCLUDE_PATH,
    XML_WIZ_RESET_DEFAULTS,

    TOOL_DIAG_CHECKBOX,

    SSD_PLAYBACK, 
    SSD_RECORD,
    SSD_SPIN_BUTTON,
    SSD_STATE_BUTTON,
    SSD_TEXT_ENTERED,
    SSD_NEXT_STATE,
    SSD_LAST_STATE,

    PXD_QUERY_CHANGE,
    PXD_EXECUTE,
    PXD_SAVE,
    PXD_QUERYLOAD,
    PXD_QUERYCLEAR,
    PXD_SOURCELOAD,
    PXD_SOURCECLEAR,
    PXD_QUERYLIST,
    PXD_SOURCELIST,
    
  };

enum
  {
    MID_MODE_NONE = 0,
    MID_MODE_DELIM,
    MID_MODE_TERM,
    MID_MODE_UDEF,
  };

enum
  {
    TREE_COLOUR_NONE = 0,
    TREE_COLOUR_BG,
    TREE_COLOUR_TEXT,
    
  };

enum
  {
    EXEC_USE_INPUT = 0x1,
    EXEC_USE_IO = 0x2,
    EXEC_COMPILE = 0x4,    
  };

enum
  {
    TP_FROM_TOOLBAR = 1,
    TP_FROM_MENU
  };


static const wxChar *TITLE = _T(APP_NAME_STRING);

static const wxChar *ANY_FILETYPES = _T("All Files (*.*)|*");

static const wxChar *IMPORT_FILETYPES = _T("Text files (*.txt)|*.txt|"
					   "All files (*.*)|*"
					   );

static const wxChar *CODE_FILETYPES   = _T("PADS definitions (*.p)|*.p|"
					   "All files (*.*)|*.*"
					   );

static const wxChar *STATE_FILETYPES  = _T("XML Files (*.xml)|*.xml|"
					   "All files (*.*)|*.*"
					   );

static const wxChar *XML_FILETYPES  = _T("XML Files (*.xml)|*.xml|"
					   "All files (*.*)|*.*"
					   );

static const wxChar *HEADER_FILETYPES = _T("Header Files (*.h)|*.h");
static const wxChar *C_FILETYPES = _T("C Files (*.c)|*.c");

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

  void LeftEnableControls(bool show);
  wxTreeItemId LeftGetTreeRoot(void);
  void LeftBuildTree(void);
  void LeftMakeCodeFromTree(void);

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
