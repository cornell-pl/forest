/*@FILE sirius-xml.tex*/
/*@BEGIN sirius-xml.tex */
#define DEF_INPUT_FILE  "data/sirius"       /* Default data source             */
#define PADS_TY(suf) out_sum ## suf         /* Record type = out_sum           */
#define IO_DISC_MK P_nlrec_make(0)          /* Records are new line terminated */
#include "sirius.h"                         /* Header file for data source     */
#include "template/read_orig_write_xml.h"   /* XML Conversion template         */
/*@END sirius-xml.tex */
