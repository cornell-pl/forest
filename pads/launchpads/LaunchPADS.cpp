/* **************************************
 * Mark Daly
 * 2005.8.18
 * v. 0.0.21
 * PADS GUI project (preliminary tests)
 * Borrows from wxWidgets documentation and sample files
 * *********************************** */

// from the wxWiki
#if defined(__GNUG__) && ! defined(__APPLE__)
#pragma implementation "pads_gui.h"
#endif

#include "lp_includes.h"

#include "bitmaps/bitmap_list.h"


/* **********************************
 * Event-function link table
 * ******************************* */

BEGIN_EVENT_TABLE(LaunchPADS, wxFrame)
  // general inpur processing handlers
  //EVT_KEY_DOWN(LaunchPADS::OnKeyPress)
  EVT_MENU_RANGE(MID_SELECT_NONE, MID_SELECT_GET, LaunchPADS::OnKeyPress)
  EVT_CONTEXT_MENU(LaunchPADS::OnContextMenu)

  // file menu functions
  EVT_MENU (wxID_EXIT,    LaunchPADS::OnExit)
  EVT_MENU (wxID_ABOUT,   LaunchPADS::OnAbout)
  EVT_MENU (wxID_OPEN,    LaunchPADS::OnOpenFile)
  EVT_MENU (wxID_SAVE,    LaunchPADS::OnSaveFile)
  EVT_MENU (wxID_SAVEAS,  LaunchPADS::OnSaveAsFile)
  // view menu functions
  EVT_MENU (WIN_RESET_VIEW,  LaunchPADS::OnViewReset)
  EVT_MENU_RANGE (WIN_SWAP_TOP, WIN_SWAP_BOTTOM,  LaunchPADS::OnWinSwap)
  EVT_MENU_RANGE (WIN_EXCLUSIVE_TOP, WIN_EXCLUSIVE_BOTTOM,  LaunchPADS::OnWinExclusive)
  // tools menu functions
  EVT_MENU_RANGE (TOOLS_PADSC, TOOLS_PLAYBACK_DEMO, LaunchPADS::OnToolMenu)
  // options menu functions
  EVT_MENU_RANGE(WIN_PREFS_DIALOG, WIN_PREFS_DIALOG, LaunchPADS::OnPrefsDialog)
  // help menu options

  // toolbar button options
  EVT_TOOL_RANGE(TOOLBAR_OPEN, TOOLBAR_END_CMDS, LaunchPADS::OnToolbarPress)

  // size/sash functions
  EVT_SIZE (LaunchPADS::OnSize)
  EVT_SASH_DRAGGED_RANGE(ID_WINDOW_TOP, ID_WINDOW_BOTTOM,
			 LaunchPADS::OnSashDrag)
  // top frame functions
  EVT_CHECKLISTBOX(TOP_LIST_BOX_CHECK, LaunchPADS::OnChangeCheck)
  EVT_BUTTON(TOP_OPEN_FILE, LaunchPADS::OnTopOpen)
  EVT_BUTTON(TOP_CLEAR, LaunchPADS::OnTopClear)
  EVT_BUTTON(TOP_IMPORT_STRING, LaunchPADS::OnTopImport)

  // middle frame functions
  EVT_GRID_CMD_RANGE_SELECT(MID_GRID_EVENT, LaunchPADS::OnMidGridSelect)
  EVT_GRID_CMD_CELL_LEFT_DCLICK(MID_GRID_EVENT, LaunchPADS::OnMidGridDClick) 
  EVT_GRID_CMD_CELL_RIGHT_DCLICK(MID_GRID_EVENT, LaunchPADS::OnMidGridRightDClick) 

  // left frame functions
  EVT_BUTTON(LEFT_ENABLE_CONTROLS, LaunchPADS::OnLeftEnableControls)
  EVT_MENU_RANGE(LEFT_FIND_IN_CODE_MENU, LEFT_MOVE_NODE_BOTTOM_MENU, LaunchPADS::OnLeftTreeContextMenu)
  EVT_MENU_RANGE(LEFT_SET_SE_DEFAULT, LEFT_SET_SE_BCD, LaunchPADS::OnLeftStopExprContextMenu)
  EVT_COMBOBOX(LEFT_TYPE_SELECT, LaunchPADS::OnLeftSelectType)
  EVT_BUTTON(LEFT_SET_ATTR,  LaunchPADS::OnLeftSetAttr)
  EVT_BUTTON(LEFT_GET_ATTR,  LaunchPADS::OnLeftGetAttr)
  EVT_BUTTON(LEFT_CLEAR_CONTROLS,  LaunchPADS::OnLeftClearControls)
  EVT_BUTTON(LEFT_DELETE_TREE,  LaunchPADS::OnLeftDeleteTree)
  EVT_BUTTON(LEFT_MAKE_CHILD, LaunchPADS::OnLeftNewChild)
  EVT_BUTTON(LEFT_MAKE_SIBLING, LaunchPADS::OnLeftNewSibling)
  EVT_BUTTON(LEFT_DELETE_NODE,  LaunchPADS::OnLeftDeleteNode)
  EVT_BUTTON(LEFT_DELETE_CHILDREN,  LaunchPADS::OnLeftDeleteChildren)
  EVT_BUTTON(LEFT_MOVE_NODE_TOP,    LaunchPADS::OnLeftMoveTop)
  EVT_BUTTON(LEFT_MOVE_NODE_UP,     LaunchPADS::OnLeftMoveUp)
  EVT_BUTTON(LEFT_MOVE_NODE_DOWN,   LaunchPADS::OnLeftMoveDown)
  EVT_BUTTON(LEFT_MOVE_NODE_BOTTOM, LaunchPADS::OnLeftMoveBottom)
  EVT_BUTTON(LEFT_APPEND_ARGUMENT,  LaunchPADS::OnLeftSetArg)
  EVT_BUTTON(LEFT_REMOVE_ARGUMENT,  LaunchPADS::OnLeftClearArg)
  EVT_BUTTON(LEFT_SET_CLAUSE,  LaunchPADS::OnLeftSetClause)
  EVT_BUTTON(LEFT_CLEAR_CLAUSE,  LaunchPADS::OnLeftClearClause)
  EVT_MENU_RANGE(LEFT_MAKE_CHILD_KEY, LEFT_MOVE_NODE_BOTTOM_KEY, LaunchPADS::OnLeftTreeCtrlKeys)  
  EVT_MENU_RANGE(LEFT_QUALIFIER_MENU_0, LEFT_QUALIFIER_MENU_19, LaunchPADS::OnLeftArgContextMenu)

  EVT_TREE_SEL_CHANGED(LEFT_TREE_CTRL, LaunchPADS::OnLeftTreeCtrlSelect)
  EVT_TREE_BEGIN_DRAG(LEFT_TREE_CTRL, LaunchPADS::OnLeftTreeBeginDrag)
  EVT_TREE_END_DRAG(LEFT_TREE_CTRL, LaunchPADS::OnLeftTreeEndDrag)
  // right frame functions
  EVT_RADIOBOX(RIGHT_MODE_SELECT, LaunchPADS::OnRightModeSelect)
  EVT_BUTTON(RIGHT_FRAGMENT, LaunchPADS::OnRightFragment)
  EVT_BUTTON(RIGHT_BACK, LaunchPADS::OnRightBack)
  EVT_BUTTON(RIGHT_RESET, LaunchPADS::OnRightReset)
  EVT_BUTTON(RIGHT_MAKE_TREE, LaunchPADS::OnRightBuildTree)
  EVT_BUTTON(RIGHT_MAKE_CODE, LaunchPADS::OnRightMakeCode)
  EVT_BUTTON(RIGHT_EXPORT_CODE, LaunchPADS::OnRightExportCode)
  EVT_BUTTON(RIGHT_SAVE_SESSION, LaunchPADS::OnRightSaveState)
END_EVENT_TABLE()



/* ********************************
 * Main frame init functions 
 * ***************************** */

// macro autogenerates code to invoke application extended from wxApp
IMPLEMENT_APP(LaunchPADSApplication)

bool LaunchPADSApplication::OnInit()
{
  // build the frame in which to contain our app
  wxString argv0 = argv[0];

  LaunchPADS *frame = new LaunchPADS("LaunchPADS", argv0, 100, 84, MAIN_WIN_HEIGHT, MAIN_WIN_WIDTH);
  assert(frame != NULL);

  SetAppName(_T(APP_NAME_STRING));
  SetVendorName(_T(VENDOR_NAME_STRING));
  SetExitOnFrameDelete(true);

  frame->Show(TRUE); // explicitly tell wxW to draw the frame
  SetTopWindow(frame); // move frame to top
  return TRUE;
}

LaunchPADS::LaunchPADS(const wxChar *title,
		       wxString argv0,
		       int xpos, int ypos, 
		       int width, int height)
  : wxFrame ((wxFrame*) NULL, 
	     -1,
	     title,
	     wxPoint(xpos, ypos),
	     wxSize(width, height), 
	     wxDEFAULT_FRAME_STYLE
#ifdef LP_OSX_METAL
	     | wxFRAME_EX_METAL
#endif
	     )
{
  /* **************************************** */
  // init internal variables
  topCheckedEntry = -1;
  midGridExists = false;
  leftTreeControlState = false;
  bottomCodeMade = false;
  savePathSet = false;
  bottomCodeSaved = false;
  savePath = "";
  leftSafeModeAltered = true;

  tGenCode = true;
  tCompileCode = true;
  tRunTool = true;

  nl.Printf(_T("\n"));

  wizBitmap = new wxBitmap(WizardPlaceholder_xpm);
  wxArrayString copyStrs(ACC_NUM_OPTS,  (const wxChar**)lpAccum_DefaultValues);
  AccStrings = copyStrs;
  wxArrayString copyStrs2(FMT_NUM_OPTS, (const wxChar**)lpFmt_DefaultValues);
  FmtStrings = copyStrs2;
  wxArrayString copyStrs3(XML_NUM_OPTS, (const wxChar**)lpXml_DefaultValues);
  XmlStrings = copyStrs3;
  

  DefaultTextColour.Set(0, 0, 0);
  DefaultBGColour.Set(255, 255, 255);

  
  appPath = wxPathOnly(argv0);
  DB_P("app path = %s\n", appPath.c_str());

  configFilePath.Printf("%s/%s", appPath.c_str(), _T(CONFIG_FILE_STRING));
  DB_P("config file path = %s\n", configFilePath.c_str());
  // try to read configuration file
  if(!wxFile::Access(configFilePath.c_str(), wxFile::read))
    {
      DB_P("Couldn't access settings file.\n");
      wxFFile confFile(configFilePath.c_str(), "w");
      confFile.Close();

      wxFFileInputStream  fConfIn(configFilePath.c_str());
      fConfig = new wxFileConfig(fConfIn);  
      SetDefaultSettings();
    }
  else  
    {
      // force use of file configuration methods so we don't need to mess with the registry 
      // on windows machines
      wxFFileInputStream  fConfIn(configFilePath.c_str());
     
      fConfig = new wxFileConfig(fConfIn);  
      
      if(!fConfIn.Ok())
	{
	  DB_P("fconf input string is NOT okay\n");
	  SetDefaultSettings();
	}
      else
	{
	  DB_P("reading config file...\n");
	  // all this could be checked for errors, but it really isn't that important at this point
	  
	  fConfig->Read(_T("/gen/save_layout_on_exit"), &saveLayoutOnExit, false);
	  fConfig->Read(_T("/gen/colour_mode_on"), &colourModeOn, true);
	  fConfig->Read(_T("/mid/grid_col_number_labels"), &midGridColNumberLabels, true);
	  fConfig->Read(_T("/top/num_lines_on_import"), &topNumLinesOnImport, TOP_NUM_LINES_MAX);
	  fConfig->Read(_T("/mid/udef_error_mbox_on"), &midUDefErrorMBoxOn, true);
	  fConfig->Read(_T("/mid/term_error_mbox_on"), &midTermErrorMBoxOn, true);
	  fConfig->Read(_T("/mid/write_type_on_next"), &midGridWriteTypeOnNext, true);
	  fConfig->Read(_T("/right/build_tree_mbox_on"), &rightBuildTreeMBoxOn, true);
	  fConfig->Read(_T("/left/tree_bg_colour_on"), &leftTreeBackgroundColourOn, true);
	  fConfig->Read(_T("/left/tree_text_colour_on"), &leftTreeTextColourOn, false);
	  fConfig->Read(_T("/left/control_warnings_on"), &leftControlsWarnings, true);
	  fConfig->Read(_T("/left/control_safe_mode_on"), &leftControlSafeMode, true);
	  fConfig->Read(_T("/left/attr_change_warnings_on"), &leftAttributeChangeWarnings, true);
	  fConfig->Read(_T("/gen/messages_on_xml_save"), &xmlMessagesOnSave, false);
	  fConfig->Read(_T("/gen/padsc_path"), &PADSCpath, _T("${HOME}/pads/"));
	  //fConfig->Read(_T("/gen/PADSCinc"),  &PADSCinc,  _T("${HOME}/pads/padsc/include/"));
	  fConfig->Read(_T("/gen/ast_inc"),    &ASTinc,    _T("${HOME}/pads/ast-ast/arch/linux.i386/"));
	  fConfig->Read(_T("/gen/cc_path"),    &CCpath,    _T("cc"));
	  fConfig->Read(_T("/gen/cc_args"),    &CCargs,    _T("-Wall"));
	  fConfig->Read(_T("/padx/path"),      &PADXPath,  _T(" "));

	  // init the pads intra-package strings
	  PADSCcomppath = PADSCpath;
	  PADSCcomppath.Append(_T(PADS_PACKAGE_PATH_PADSC));
	  PADSCinc = PADSCpath;
	  PADSCinc.Append(_T(PADS_PACKAGE_PATH_INC));
	  ASTincludedir = ASTinc;
	  ASTincludedir.Append(_T(PADS_PACKAGE_PATH_ASTINC));
	  LIBdir = ASTinc;
	  LIBdir.Append(_T(PADS_PACKAGE_PATH_ASTLIB));
	  
	  // set syntax highlighting colors
	  long red, blue, green;
	  fConfig->Read(_T("/bottom/code_term_red"),   &red,   16);
	  fConfig->Read(_T("/bottom/code_term_green"), &green,  16);
	  fConfig->Read(_T("/bottom/code_term_blue"), &blue, 152);
	  bottomTermColour.Set((unsigned char)red, 
			       (unsigned char)green, 
			       (unsigned char)blue);
	  
	  fConfig->Read(_T("/bottom/code_nonterm_red"),   &red,   244);
	  fConfig->Read(_T("/bottom/code_nonterm_green"), &green, 158);
	  fConfig->Read(_T("/bottom/code_nonterm_blue"),  &blue,  64);
	  bottomNontermColour.Set((unsigned char)red, 
				  (unsigned char)green, 
				  (unsigned char)blue);
	  
	  DB_P("nonterm colour: %d %d %d\n", 
	       bottomNontermColour.Red(),
	       bottomNontermColour.Green(),
	       bottomNontermColour.Blue());
	  
	  fConfig->Read(_T("/bottom/code_lit_red"),   &red,   16);
	  fConfig->Read(_T("/bottom/code_lit_green"), &green, 152);
	  fConfig->Read(_T("/bottom/code_lit_blue"),  &blue,  16);
	  bottomLitColour.Set((unsigned char)red, 
			      (unsigned char)green, 
			      (unsigned char)blue);
	  
	  fConfig->Read(_T("/bottom/code_error_red"),   &red,   255);
	  fConfig->Read(_T("/bottom/code_error_green"), &green, 0);
	  fConfig->Read(_T("/bottom/code_error_blue"),  &blue,  0);
	  bottomErrorColour.Set((unsigned char)red, 
				(unsigned char)green, 
				(unsigned char)blue);
	  
	  fConfig->Read(_T("/bottom/code_other_red"),   &red,   0);
	  fConfig->Read(_T("/bottom/code_other_green"), &green, 0);
	  fConfig->Read(_T("/bottom/code_other_blue"),  &blue,  0);
	  bottomOtherColour.Set((unsigned char)red, 
				(unsigned char)green, 
				(unsigned char)blue);
	  
	  // set window layout attributes
	  fConfig->Read(_T("top/win_X"),    &topWinX,    TOP_WIN_DEFAULT_X);
	  fConfig->Read(_T("top/win_Y"),    &topWinY,    TOP_WIN_DEFAULT_Y);
	  fConfig->Read(_T("mid/win_X"),    &midWinX,    MID_WIN_DEFAULT_X);
	  fConfig->Read(_T("mid/win_Y"),    &midWinY,    MID_WIN_DEFAULT_Y);
	  fConfig->Read(_T("left/win_X"),   &leftWinX,   LEFT_WIN_DEFAULT_X);
	  fConfig->Read(_T("left/win_Y"),   &leftWinY,   LEFT_WIN_DEFAULT_Y);
	  fConfig->Read(_T("right/win_X"),  &rightWinX,  RIGHT_WIN_DEFAULT_X);
	  fConfig->Read(_T("right/win_Y"),  &rightWinY,  RIGHT_WIN_DEFAULT_Y);
	  fConfig->Read(_T("bottom/win_X"), &bottomWinX, BOTTOM_WIN_DEFAULT_X);
	  fConfig->Read(_T("bottom/win_Y"), &bottomWinY, BOTTOM_WIN_DEFAULT_Y);
	  
	  // set font attributes
	  wxString fontName;
	  int fontSize;
	  
	  fConfig->Read(_T("/top/font_family"), &fontName, "");
	  fConfig->Read(_T("/top/font_size"),   &fontSize, 10);
	  if(fontName.CompareTo("") != 0)
	    topFont.SetFaceName(fontName);
	  topFont.SetPointSize(fontSize);
	  
	  
	  fConfig->Read(_T("/bottom/font_family"), &fontName, "");
	  fConfig->Read(_T("/bottom/font_size"),   &fontSize, 10);
	  if(fontName.CompareTo("") != 0)
	    bottomFont.SetFaceName(fontName);
	  bottomFont.SetPointSize(fontSize);
	}
    }

  if(leftControlSafeMode)
    leftSafeModeAltered = true;

  DB_P("done reading config file\n");
  /* **************************************** */
  // adapted from wxWidgets sashtest.cpp sample
  // init all the sashes and build the controls for each subframe
  wxSashLayoutWindow* win;

  win = 
    new wxSashLayoutWindow(this, 
			   ID_WINDOW_LEFT,
			   wxDefaultPosition,
			   wxSize(200, 30),
			   wxNO_BORDER | wxSW_3D | wxCLIP_CHILDREN);
  assert(win != NULL);

  win->SetDefaultSize(wxSize(leftWinX,
			     leftWinY)); 
  win->SetOrientation(wxLAYOUT_VERTICAL);
  win->SetAlignment(wxLAYOUT_LEFT);
  //win->SetBackgroundColour(wxColour(0, 255, 0));
#ifndef __WXMAC__
  win->SetBackgroundColour(wxColour(196, 196, 196));
#endif
  win->SetSashVisible(wxSASH_RIGHT, true);  // picks on which side the dragable sash will appear
  m_leftWin = win;

  LeftInitFrame(wxTAB_TRAVERSAL | mywxBORDER, 
	        wxTR_HAS_BUTTONS | wxTR_SINGLE | mywxBORDER | wxTR_HIDE_ROOT,
		0);

  DB_P("done left init\n");
  /* **************************************** */

  win = 
    new wxSashLayoutWindow(this, 
			   ID_WINDOW_TOP,
			   wxDefaultPosition,
			   wxSize(200, 30),
			   wxNO_BORDER | wxSW_3D | wxCLIP_CHILDREN);
  assert(win != NULL);

  win->SetDefaultSize(wxSize(topWinX,
			     topWinY)); 
  win->SetOrientation(wxLAYOUT_HORIZONTAL);
  win->SetAlignment(wxLAYOUT_TOP);
  //win->SetBackgroundColour(wxColour(255, 0, 0));
#ifndef __WXMAC__
  win->SetBackgroundColour(wxColour(196, 196, 196));
#endif
  win->SetSashVisible(wxSASH_BOTTOM, true);
  m_topWin = win;

  TopInitFrame(wxTAB_TRAVERSAL | mywxBORDER, 
	       wxLB_SINGLE, 
	       wxBU_EXACTFIT 
	       );

  DB_P("done top init\n");
  /* **************************************** */

  win = 
    new wxSashLayoutWindow(this, 
			   ID_WINDOW_RIGHT,
			   wxDefaultPosition,
			   wxSize(200, 30),
			   wxNO_BORDER | wxSW_3D | wxCLIP_CHILDREN);
  assert(win != NULL);

  win->SetDefaultSize(wxSize(rightWinX,
			     rightWinY)); 
  win->SetOrientation(wxLAYOUT_VERTICAL);
  win->SetAlignment(wxLAYOUT_RIGHT);
#ifndef __WXMAC__
  win->SetBackgroundColour(wxColour(196, 196, 196));
#endif
  win->SetSashVisible(wxSASH_LEFT, true);
  m_rightWin = win;

  RightInitFrame(wxTAB_TRAVERSAL | mywxBORDER, 
		 wxBU_EXACTFIT,
		 0,
		 wxCB_DROPDOWN | wxCB_READONLY | wxCB_SORT
		 );

  DB_P("done right init\n");
  /* **************************************** */

  win = 
    new wxSashLayoutWindow(this, 
			   ID_WINDOW_MID,
			   wxDefaultPosition,
			   wxSize(200, 30),
			   wxNO_BORDER | wxSW_3D | wxCLIP_CHILDREN);
  assert(win != NULL);

  win->SetDefaultSize(wxSize(midWinX,
			     midWinY)); 
  win->SetOrientation(wxLAYOUT_HORIZONTAL);
  win->SetAlignment(wxLAYOUT_TOP);
  //win->SetBackgroundColour(wxColour(255, 172, 64));
#ifndef __WXMAC__
  win->SetBackgroundColour(wxColour(196, 196, 196));
#endif
  win->SetSashVisible(wxSASH_BOTTOM, true);
  m_midWin = win;

  MidInitFrame(wxTAB_TRAVERSAL | mywxBORDER,
	       wxTAB_TRAVERSAL | mywxBORDER);
  //  m_midWin->SetMinimumSizeY(MID_WIN_DEFAULT_Y);
  DB_P("done mid init\n");
  /* **************************************** */
  DB_P("starting bottom init\n");
  win = 
    new wxSashLayoutWindow(this, 
			   ID_WINDOW_BOTTOM,
			   wxDefaultPosition,
			   wxSize(200, 30),
			   wxSUNKEN_BORDER | wxSW_3D | wxCLIP_CHILDREN);
  assert(win != NULL);

  win->SetDefaultSize(wxSize(bottomWinX,
			     bottomWinY)); 
  win->SetOrientation(wxLAYOUT_HORIZONTAL);
  win->SetAlignment(wxLAYOUT_TOP);
  //win->SetBackgroundColour(wxColor(255, 255, 0));
#ifndef __WXMAC__
  win->SetBackgroundColour(wxColour(196, 196, 196));
#endif
  win->SetSashVisible(wxSASH_TOP, false);
  m_bottomWin = win;
  DB_P("calling bottom init\n");
  BottomInitFrame(wxTAB_TRAVERSAL | mywxBORDER, 
		  wxTE_MULTILINE | wxTE_RICH | wxHSCROLL 
		  | wxTE_READONLY | mywxBORDER);

  DB_P("done bottom init\n");
  /* **************************************** */

  /* **************************************** */
  DB_P("done frame init, making other controls\n");

  // init the menus, toolbar, and status bar
  fileMenu = new wxMenu;
  assert(fileMenu != NULL);

  // the '&' indicates that the following letter should be the menu item's key shortcut
#ifdef __APPLE__
  fileMenu->Append(wxID_OPEN,  _T("&Open State"), "Open saved LaunchPADS session");
  fileMenu->Append(wxID_SAVE,  _T("&Save State"), "Save LaunchPADS session");
  fileMenu->Append(wxID_SAVEAS,  _T("S&ave State As"), "Save LaunchPADS session");
  fileMenu->AppendSeparator();
  fileMenu->Append(wxID_EXIT,  _T("E&xit"), "Exit LaunchPADS");
#else
  fileMenu->Append(wxID_OPEN,  _T("&Open State\tCTRL+o"), "Open saved LaunchPADS session");
  fileMenu->Append(wxID_SAVE,  _T("&Save State\tCTRL+s"), "Save LaunchPADS session");
  fileMenu->Append(wxID_SAVEAS,  _T("S&ave State As\tCTRL+SHIFT+s"), "Save LaunchPADS session");
  fileMenu->AppendSeparator();
  fileMenu->Append(wxID_EXIT,  _T("E&xit\tALT+F4"), "Exit LaunchPADS");
#endif
  /*
  editMenu = new wxMenu;
  assert(editMenu != NULL);
  editMenu->Append(wxID_UNDO, _T("&Undo\tCTRL+z"),  "Whoops!");
  editMenu->AppendSeparator();
  editMenu->Append(wxID_CUT,  _T("Cu&t\tCTRL+x"),   "Disabled");
  editMenu->Append(wxID_COPY, _T("&Copy\tCTRL+c"),  "Disabled");
  editMenu->Append(wxID_PASTE,_T("&Paste\tCTRL+v"), "Disabled");
  editMenu->Enable(wxID_CUT, false);
  editMenu->Enable(wxID_COPY, false);
  editMenu->Enable(wxID_PASTE, false);
  */
  windowMenu = new wxMenu;
  assert(windowMenu != NULL);
  windowMenu->Append(WIN_SWAP_TOP,         _T("Toggle &Import Frame"), "Turn Key Import Frame (Top) On/Off");
  windowMenu->Append(WIN_SWAP_MID,         _T("Toggle &Grid Frame"), "Turn Grid Design Frame (Middle) On/Off");
  windowMenu->Append(WIN_SWAP_LEFT,        _T("Toggle &Tree Frame"), "Turn Tree View (Left) On/Off");
  windowMenu->Append(WIN_SWAP_RIGHT,       _T("Toggle &Control Frame"), "Turn Control Frame (Right) On/Off");
  windowMenu->Append(WIN_SWAP_BOTTOM,      _T("Toggle &Definition Frame"), "Turn Code View (Bottom) On/Off");
  windowMenu->AppendSeparator();
  windowMenu->Append(WIN_EXCLUSIVE_TOP,    _T("Show Import Frame"), "Show Key Import Frame Exclusively");
  windowMenu->Append(WIN_EXCLUSIVE_MID,    _T("Show Grid Frame"), "Show Grid Frame Exclusively");
  windowMenu->Append(WIN_EXCLUSIVE_LEFT,   _T("Show Tree Frame"), "Show Tree Frame Exclusively");
  windowMenu->Append(WIN_EXCLUSIVE_RIGHT,  _T("Show Control Frame"), "Show Control Frame Exclusively");
  windowMenu->Append(WIN_EXCLUSIVE_BOTTOM, _T("Show Definition Frame"), "Show Code View Exclusively");
  windowMenu->AppendSeparator();
  windowMenu->Append(WIN_RESET_VIEW,       _T("&Reset View"), "Set view to default");

  toolsMenu = new wxMenu( _T(""), wxMENU_TEAROFF);
  assert(toolsMenu != NULL);
  toolsMenu->Append(TOOLS_PADSC,  _T("&Launch PADS"), _T("Launch PADS compiler"));
  toolsMenu->Append(TOOLS_TOXML,  _T("&XML Conversion"), _T("Convert data to XML"));
  toolsMenu->Append(TOOLS_TOFMT,  _T("&Format"), _T("Convert data to simple delimited format"));
  toolsMenu->Append(TOOLS_ACCUM,  _T("&Accumulator"), _T("Run accumulator on data"));
  // toolsMenu->Append(TOOLS_FILTER, _T("&Filter Data"), _T("Apply filter to data"));
  toolsMenu->AppendSeparator();
  toolsMenu->Append(TOOLS_RECORD_DEMO,   _T("&Record Session"), _T("Record a incremental PADS session (for demos)"));
  toolsMenu->Append(TOOLS_PLAYBACK_DEMO, _T("&Playback Session"), _T("Playback a incremental PADS session (for demos)"));

  optionsMenu = new wxMenu;
  assert(optionsMenu != NULL);
  optionsMenu->Append(WIN_PREFS_DIALOG, _T("&Preferences"), _T("Open preference control window"));

  helpMenu = new wxMenu;
  assert(helpMenu != NULL);
#ifdef __APPLE__
  helpMenu->Append(wxID_ABOUT, _T("About"), _T("About this program"));
#else
  helpMenu->Append(wxID_ABOUT, _T("&About\tCTRL+a"), _T("About this program"));
#endif
  helpMenu->AppendSeparator();
  helpMenu->Append(wxID_HELP, _T("LaunchPADS He&lp\tCTRL+L"), _T("Open LaunchPADS help system"));
  helpMenu->Append(WIN_OPEN_PADS_MANUAL, _T("PADS &Manual"), _T("Open the PADS manual"));

  menuBar = new wxMenuBar;
  assert(menuBar != NULL);
  menuBar->Append(fileMenu, "&File");
  // menuBar->Append(editMenu, "&Edit");
  menuBar->Append(windowMenu, "&View");
  menuBar->Append(toolsMenu, "Too&ls");
  menuBar->Append(optionsMenu, "&Options");
  menuBar->Append(helpMenu, "&Help");
  SetMenuBar(menuBar);


  // shortcut function for creating a toolbar within a preexistsing frame
  toolBar = CreateToolBar(wxTB_HORIZONTAL | wxTB_DOCKABLE /* | wxTB_TEXT */);

  // for some reason the sizing functions seem to not work...
  /*
  int x_margin, y_margin;

  x_margin = toolBar->GetMargins().GetWidth();
  y_margin = toolBar->GetMargins().GetHeight();

  toolBar->SetMargins(x_margin + 15, y_margin);
  
  int sep = toolBar->GetToolSeparation();
  toolBar->SetToolSeparation(sep + 20);
  */

  /*
  toolBarOpen = new wxButton(toolBar,
			     wxID_ANY,
			     "Open",
			     wxDefaultPosition,
			     wxDefaultSize,
			     wxBU_EXACTFIT);
  
  toolBar->AddControl(toolBarOpen);
  */

  wxBitmap openIcon(fileopen_xpm);
  toolBar->AddTool(TOOLBAR_OPEN,
		   _T("Open"),
		   openIcon,
		   wxNullBitmap,
		   wxITEM_NORMAL,
		   _T("Open Saved Session"),
		   _T("Open saved LaunchPADS session from LaunchPADS XML file."));
  

  wxBitmap saveIcon(filesave_xpm);
  toolBar->AddTool(TOOLBAR_SAVE,
		   _T("Save"),
		   saveIcon,
		   wxNullBitmap,
		   wxITEM_NORMAL,
		   _T("Save Session"),
		   _T("Save LaunchPADS session to LaunchPADS XML file."));

  wxBitmap saveAsIcon(filesaveas_xpm);
  toolBar->AddTool(TOOLBAR_SAVEAS,
		   _T("Save As"),
		   saveAsIcon,
		   wxNullBitmap,
		   wxITEM_NORMAL,
		   _T("Save Session As"),
		   _T("Save LaunchPADS session to new LaunchPADS XML file."));


  toolBar->AddSeparator();

  wxBitmap loadPXMLIcon(fileopen_xpm);
  toolBar->AddTool(TOOLBAR_LOADPXML,
		   _T("Load P-XML"),
		   loadPXMLIcon,
		   wxNullBitmap,
		   wxITEM_NORMAL,
		   _T("Load PADS XML Definition"),
		   _T("Open saved PADS definition from PADS generated XML definition file."));

  wxBitmap savePXMLIcon(htmpage_xpm);
  toolBar->AddTool(TOOLBAR_SAVEPXML,
		   _T("Save P-XML"),
		   savePXMLIcon,
		   wxNullBitmap,
		   wxITEM_NORMAL,
		   _T("Save PADS XML Definition"),
		   _T("Save PADS definition in XML format."));

  toolBar->AddSeparator();

  wxBitmap launchIcon(launch_icon3_xpm);
  toolBar->AddTool(TOOLBAR_LAUNCH,
		   _T("Launch"),
		   launchIcon,
		   wxNullBitmap,
		   wxITEM_NORMAL,
		   _T("Launch PADS"),
		   _T("Use the PADS compiler to create C library from generated code."));

  wxBitmap xmlIcon(save_icon6_xpm);
  toolBar->AddTool(TOOLBAR_XML,
		   _T("XML"),
		   xmlIcon,
		   wxNullBitmap,
		   wxITEM_NORMAL,
		   _T("Convert to XML"),
		   _T("Use the PADS template XML converters to format data in XML."));


  wxBitmap fmtIcon3(fmt_icon3_xpm);
  toolBar->AddTool(TOOLBAR_FMT,
		   _T("Format"),
		   fmtIcon3,
		   wxNullBitmap,
		   wxITEM_NORMAL,
		   _T("Reformat Data"),
		   _T("Use the PADS format converters to format data by user specification."));

  wxBitmap accumIcon(accum_icon2_xpm);
  toolBar->AddTool(TOOLBAR_ACCUM,
		   _T("Accumulator"),
		   accumIcon,
		   wxNullBitmap,
		   wxITEM_NORMAL,
		   _T("Run Accumulator"),
		   _T("Use the PADS accumulators to analyze data."));

  wxBitmap padxIcon(save_icon2_xpm);
  toolBar->AddTool(TOOLBAR_PADX,
		   _T("PADX"),
		   padxIcon,
		   wxNullBitmap,
		   wxITEM_NORMAL,
		   _T("Launch PADX Interface"),
		   _T("Use PADX to run XQueries on ad hoc data via the GALAX query engine."));
  /*		   
  wxBitmap sumIcon(sum_icon1_xpm);
  toolBar->AddTool(TOOLBAR_SUM,
		   _T("Sum"),
		   sumIcon,
		   wxNullBitmap,
		   wxITEM_NORMAL,
		   _T("Run Summation Tool"),
		   _T("Use the PADS summation tool to analyze data."));
  */
  /*  wxBitmap filterIcon(filter_icon1_xpm);
  toolBar->AddTool(TOOLBAR_FILTER,
		   _T("Filter"),
		   filterIcon,
		   wxNullBitmap,
		   wxITEM_NORMAL,
		   _T("Run Filter"),
		   _T("Use the PADS filter to remove bad data."));
  */
  toolBar->AddSeparator();

  wxBitmap htmIcon(htmbook_xpm);
  toolBar->AddTool(TOOLBAR_PADS_MANUAL,
		   _T("Manual"),
		   htmIcon,
		   wxNullBitmap,
		   wxITEM_NORMAL,
		   _T("Open PADS Manual"),
		   _T("Open PADS reference manual."));

  wxBitmap helpIcon(htmpage_xpm);
  toolBar->AddTool(TOOLBAR_HELP,
		   _T("Help"),
		   helpIcon,
		   wxNullBitmap,
		   wxITEM_NORMAL,
		   _T("Open Online Help"),
		   _T("Open LaunchPADS online help system."));
		   

  toolBar->Realize();   // this is necessary to make the toolbar visible on MSW systems
  toolBar->Show();

  CreateStatusBar(1);   // shortcut function to create a simple status bar in the current frame

  DB_P("done making tools\n");

  //  wxToolTip::Enable(true); // turn on tooltips for all UI elements for which tooltips are set

  SetMinSize(wxSize(320, 240));

  wxLayoutAlgorithm layout; // this must be called at the end of all size changing functions to 
  layout.LayoutFrame(this); // update the sash layout properly

}

LaunchPADS::~LaunchPADS()
{

  // save the user's prefs at the last possible second...

  wxFFileOutputStream  fConfOut(configFilePath.c_str());

  fConfig->SetUmask(0077);

  if(fConfOut.Ok())
    {
      DB_P("saving config file...\n");

      fConfig->Write(_T("/gen/save_layout_on_exit"), saveLayoutOnExit);
      fConfig->Write(_T("/gen/colour_mode_on"), colourModeOn);
      fConfig->Write(_T("/mid/grid_col_number_labels"), midGridColNumberLabels);
      fConfig->Write(_T("/top/num_lines_on_import"), topNumLinesOnImport);
      fConfig->Write(_T("/mid/udef_error_mbox_on"), midUDefErrorMBoxOn);
      fConfig->Write(_T("/mid/term_error_mbox_on"), midTermErrorMBoxOn);
      fConfig->Write(_T("/mid/write_type_on_next"), midGridWriteTypeOnNext);
      fConfig->Write(_T("/right/build_tree_mbox_on"), rightBuildTreeMBoxOn);
      fConfig->Write(_T("/left/tree_bg_colour_on"), leftTreeBackgroundColourOn);
      fConfig->Write(_T("/left/tree_text_colour_on"), leftTreeTextColourOn);
      fConfig->Write(_T("/left/control_warnings_on"), leftControlsWarnings);
      fConfig->Write(_T("/left/control_safe_mode_on"), leftControlSafeMode);
      fConfig->Write(_T("/left/attr_change_warnings_on"), leftAttributeChangeWarnings);
      fConfig->Write(_T("/gen/messages_on_xml_save"), xmlMessagesOnSave);
      fConfig->Write(_T("/gen/padsc_path"), PADSCpath);
      //fConfig->Write(_T("/gen/PADSCinc"),  PADSCinc);
      fConfig->Write(_T("/gen/ast_inc"),    ASTinc);
      fConfig->Write(_T("/gen/cc_path"),    CCpath);
      fConfig->Write(_T("/gen/cc_args"),    CCargs);
      fConfig->Write(_T("/padx/path"),      PADXPath);
      
      long red, blue, green;

      red = (unsigned long)bottomTermColour.Red();
      green = (unsigned long)bottomTermColour.Green();
      blue = (unsigned long)bottomTermColour.Blue();
      fConfig->Write(_T("/bottom/code_term_red"),   red);
      fConfig->Write(_T("/bottom/code_term_green"), green);
      fConfig->Write(_T("/bottom/code_term_blue"),  blue);
      DB_P("writing term color %d %d %d\n", red, green, blue);

      red = (long)bottomNontermColour.Red();
      green = (long)bottomNontermColour.Green();
      blue = (long)bottomNontermColour.Blue();
      fConfig->Write(_T("/bottom/code_nonterm_red"),   red);
      fConfig->Write(_T("/bottom/code_nonterm_green"), green);
      fConfig->Write(_T("/bottom/code_nonterm_blue"),  blue);
      DB_P("writing nonterm color %d %d %d\n", red, green, blue);

      red = (long)bottomLitColour.Red();
      green = (long)bottomLitColour.Green();
      blue = (long)bottomLitColour.Blue();
      fConfig->Write(_T("/bottom/code_lit_red"),   red);
      fConfig->Write(_T("/bottom/code_lit_green"), green);
      fConfig->Write(_T("/bottom/code_lit_blue"),  blue);
      DB_P("writing lit color %d %d %d\n", red, green, blue);

      red = (long)bottomErrorColour.Red();
      green = (long)bottomErrorColour.Green();
      blue = (long)bottomErrorColour.Blue();
      fConfig->Write(_T("/bottom/code_error_red"),   red);
      fConfig->Write(_T("/bottom/code_error_green"), green);
      fConfig->Write(_T("/bottom/code_error_blue"),  blue);
      DB_P("writing error color %d %d %d\n", red, green, blue);

      red = (long)bottomOtherColour.Red();
      green = (long)bottomOtherColour.Green();
      blue = (long)bottomOtherColour.Blue();
      fConfig->Write(_T("/bottom/code_other_red"),   red);
      fConfig->Write(_T("/bottom/code_other_green"), green);
      fConfig->Write(_T("/bottom/code_other_blue"),  blue);
      DB_P("writing other color %d %d %d\n", red, green, blue);

      if(saveLayoutOnExit)
	{
	  fConfig->Write(_T("top/win_X"),    topWinX);
	  fConfig->Write(_T("top/win_Y"),    topWinY);
	  fConfig->Write(_T("mid/win_X"),    midWinX);
	  fConfig->Write(_T("mid/win_Y"),    midWinY);
	  fConfig->Write(_T("left/win_X"),   leftWinX);
	  fConfig->Write(_T("left/win_Y"),   leftWinY);
	  fConfig->Write(_T("right/win_X"),  rightWinX);
	  fConfig->Write(_T("right/win_Y"),  rightWinY);
	  fConfig->Write(_T("bottom/win_X"), bottomWinX);
	  fConfig->Write(_T("bottom/win_Y"), bottomWinY);
	}

      if(!fConfig->Save(fConfOut)) // ->Save actually writes the fields to the file
	{
	  wxMessageBox( _T("ERROR: could not save to preferences file."), 
			_T("Could Not Save Preferences!"),
			wxICON_EXCLAMATION | wxOK);
	}

      fConfig->~wxFileConfig();
      fConfOut.Close();
    }
  else
    {
      wxLogError("Could not write configuation file.");
      wxSleep(2);
    }
}

/* ****************************************************************** */
