#ifndef LAUNCHPADS_CONSTANTS_H_INCLUDED__
#define LAUNCHPADS_CONSTANTS_H_INCLUDED__ 

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


#endif
