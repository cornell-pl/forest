/*@FILE wsl-accum.tex*/
/*@BEGIN wsl-accum.tex*/
#define DEF_INPUT_FILE "data/wsl"    /* Default data location             */
#define PADS_TY(suf) entry_t ## suf  /* Name of record type = entry_t     */
#define IO_DISC_MK P_nlrec_make(0)   /* Records are new line terminated   */
#include "wsl.h"                     /* Header file for generated library */
#include "template/accum_report.h"   /* Accumulator template program      */
/*@END wsl-accum.tex*/
