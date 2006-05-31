/* **************************************
 * PADS GUI project (preliminary tests)
 * batch include file
 * *********************************** */

#ifndef __LP_INCLUDES_INCLUDED
#define __LP_INCLUDES_INCLUDED

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>
#include <assert.h>
#include "wx/wxprec.h"

//#ifndef WX_PRECOMP // borrowed from wxWidgets sample code
#include "wx/wx.h"
#include "wx/accel.h"
#include "wx/arrstr.h"
#include "wx/bitmap.h"
#include "wx/busyinfo.h"
#include "wx/choicdlg.h"
#include "wx/choice.h"
#include "wx/cmdline.h"
#include "wx/cmndata.h"
#include "wx/colordlg.h"
#include "wx/combobox.h"
#include "wx/datetime.h"
#include "wx/dialog.h"
#include "wx/dynarray.h"
#include "wx/event.h"
#include "wx/ffile.h"
#include "wx/file.h"
#include "wx/fileconf.h"
#include "wx/filedlg.h"
#include "wx/font.h"
#include "wx/fontdlg.h"
#include "wx/grid.h"
#include "wx/gbsizer.h"
#include "wx/image.h"
#include "wx/laywin.h"
#include "wx/log.h"
#include "wx/progdlg.h"
#include "wx/propdlg.h"
#include "wx/radiobox.h"
#include "wx/regex.h"
#include "wx/spinbutt.h"
#include "wx/string.h"
#include "wx/statbox.h"
#include "wx/statline.h"
#include "wx/stattext.h"
#include "wx/tbarbase.h"
#include "wx/textdlg.h"
#include "wx/textfile.h"
#include "wx/txtstrm.h"
#include "wx/toolbar.h"
#include "wx/tooltip.h"
#include "wx/treectrl.h"
#include "wx/utils.h"
#include "wx/valtext.h"
#include "wx/wfstream.h"
#include "wx/wizard.h"
//#endif

#include "tinyxml.h"
#include "lp_XML.h"
#include "lp_PADS.h"
#include "pads_gui.h"
#include "lp_CodeGen.h"
#include "prefs.h"
#include "shell_d.h"
#include "tool_proc.h"
#include "state_seq.h"
#include "padx_d.h"

#endif
