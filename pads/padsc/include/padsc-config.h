#ifdef _USE_PROTO
#pragma prototyped
#endif
/*
 * CONFIG file for libpadsc
 *   
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#ifndef __LIBPADSC_CONFIG_H__
#define __LIBPADSC_CONFIG_H__

/* Normal users of the libpadsc library should not modify this file.
 *
 * Power users worried about the footprint of the library can 
 * assign 0 to some of the following defines to obtain a smaller library.
 * After modifying this file, the library can be rebuilt using:
 *
 * cd $PADS_HOME/padsc/libpadsc
 * gmake clean
 * gmake install
 */

/* The following control whether ANY read, write, or accum functions are included */

#define PDC_CONFIG_READ_FUNCTIONS   1 /* need read functions */
#define PDC_CONFIG_WRITE_FUNCTIONS  1 /* need write functions */
#define PDC_CONFIG_ACCUM_FUNCTIONS  1 /* need accum functions */

/* The following control whether functions for specific external reps
 * are included.
 * 
 * IMPORTANT: If you disable an ASCII or EBCDIC type, then the
 * corresponding default type (which uses pdc->disc->char_set to
 * choose the actual type) will not be available.  For example, if you
 * set PDC_CONFIG_A_INT to 0, then this will not only cause ASCII
 * integer read functions (such as PDC_a_int8_read) to be omitted, it
 * also causes the default integer function (such as PDC_int8_read) to
 * be ommitted
 */

#define PDC_CONFIG_A_CHAR_STRING    1 /* need all ASCII character and string-based functions   */
#define PDC_CONFIG_E_CHAR_STRING    1 /* need all EBCDIC character and string-based functions  */
 
#define PDC_CONFIG_A_INT            1 /* need ASCII-based variable-width integers */
#define PDC_CONFIG_A_INT_FW         1 /* need ASCII-based fixed-width integers */

#define PDC_CONFIG_A_FLOAT          1 /* need ASCII-based variable-width floats */
#define PDC_CONFIG_A_FLOAT_FW       1 /* need ASCII-based fixed-width floats */

#define PDC_CONFIG_E_INT            1 /* need EBCDIC-based variable-width integers */
#define PDC_CONFIG_E_INT_FW         1 /* need EBCDIC-based fixed-width integers */

#define PDC_CONFIG_E_FLOAT          1 /* need EBCDIC-based variable-width floats */
#define PDC_CONFIG_E_FLOAT_FW       1 /* need EBCDIC-based fixed-width floats */

#define PDC_CONFIG_B_INT            1 /* need raw binary integers */ 
#define PDC_CONFIG_B_FLOAT          1 /* need raw binary floats */ 

#define PDC_CONFIG_SBL_FLOAT        1 /* need SBL encoding floats */ 
#define PDC_CONFIG_SBH_FLOAT        1 /* need SBH encoding floats */ 

#define PDC_CONFIG_EBC_INT          1 /* need EBC encoding integers */ 
#define PDC_CONFIG_BCD_INT          1 /* need BCD encoding integers */ 
#define PDC_CONFIG_SBL_INT          1 /* need SBL encoding integers */ 
#define PDC_CONFIG_SBH_INT          1 /* need SBH encoding integers */ 

#define PDC_CONFIG_EBC_FPOINT       1 /* need EBC encoding fixed-points */ 
#define PDC_CONFIG_BCD_FPOINT       1 /* need BCD encoding fixed-points */ 
#define PDC_CONFIG_SBL_FPOINT       1 /* need SBL encoding fixed-points */ 
#define PDC_CONFIG_SBH_FPOINT       1 /* need SBH encoding fixed-points */ 

#endif   /*   ! __LIBPADSC_CONFIG_H__  */
