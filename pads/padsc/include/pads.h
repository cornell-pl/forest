#pragma prototyped
/*
 * padsc library interface
 * 
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#ifndef __LIBPADSC_H__
#define __LIBPADSC_H__

#include <ast_common.h>

/* ================================================================================ */
/* CONSTANTS */

#define PDC_VERSION                  20020815L

typedef unsigned long          PDC_flags_t;

/*
 * The following flags are defined using an enum to make them appear in ckit output,
 * but we will want to OR these together so cast as (PDC_flags_t) when using.
 */
typedef enum PDC_ctl_flag_enum_e {
  PDC_NULL_CTL_FLAG                 =    0,
  PDC_WSPACE_OK                     =    1
} PDC_ctl_flag_enum;

typedef enum PDC_error_t_e {
  PDC_OK                            =    0,
  PDC_ERROR                         =   -1
} PDC_error_t;

typedef enum PDC_errCode_t_e {
  PDC_NO_ERROR                      =    0,

  PDC_OUT_OF_MEMORY                 =    1,
  PDC_SYS_ERROR                     =    2,

  PDC_CHKPOINT_FAILURE              =   11,
  PDC_COMMIT_FAILURE                =   12,
  PDC_RESTORE_FAILURE               =   13,
  PDC_ALLOC_FAILURE                 =   14,
  PDC_PANIC_SKIPPED                 =   20,

  PDC_USER_CONSTRAINT_VIOLATION     =  100,
  PDC_MISSING_LITERAL               =  101,
  PDC_ARRAY_ELEM_ERR                =  110,
  PDC_ARRAY_SEP_ERR                 =  111,
  PDC_ARRAY_TERM_ERR                =  112,
  PDC_ARRAY_SIZE_ERR                =  113,
  PDC_ARRAY_USER_CONSTRAINT_ERR     =  114,
  PDC_ARRAY_MIN_BIGGER_THAN_MAX_ERR =  115,
  PDC_ARRAY_MIN_NEGATIVE            =  116,
  PDC_ARRAY_MAX_NEGATIVE            =  117,
  PDC_ARRAY_EXTRA_BEFORE_SEP        =  118,
  PDC_ARRAY_EXTRA_BEFORE_TERM       =  119,

  PDC_STRUCT_FIELD_ERR              =  120,
  PDC_UNION_MATCH_FAILURE           =  130,
  PDC_ENUM_MATCH_FAILURE            =  140,

  PDC_AT_EOF                        =  150,
  PDC_AT_EOR                        =  160,
  PDC_RANGE                         =  170,
  PDC_INVALID_AINT                  =  180,
  PDC_INVALID_AUINT                 =  181,
  PDC_INVALID_BINT                  =  182,
  PDC_INVALID_BUINT                 =  183,
  PDC_CHAR_LIT_NOT_FOUND            =  190,
  PDC_REGEXP_NOT_FOUND              =  200,
  PDC_INVALID_REGEXP                =  210,
  PDC_WIDTH_NOT_AVAILABLE           =  220
} PDC_errCode_t;

/* ================================================================================ */
/* INTERFACE LIBRARY TYPES: FORWARD DECLS */

/*
 *  PDC_t* : runtime library handle (opaque)
 *           initialized with PDC_open, passed as first arg to most library routines
 *
 *  PDC_disc_t* : handle to user-supplied discipline which controls
 *                such things as error reporting, panic stop point, etc.
 *                Passed to PDC_open, also passed as last arg to most library routines
 *
 * Members of PDC_disc_t:
 *
 *   e_rep : error reporting, one of:
 *              PDC_errorRep_None : do not generate descriptive error reports
 *              PDC_errorRep_Min  : minimal reporting: report errCode, error line/char position
 *              PDC_errorRep_Med  : medium reporting:  like Min, but adds descriptive string
 *  [default]   PDC_errorRep_Max  : maximum reporting, like Med, but adds offending line up to error position
 *
 *   p_stop: panic stop 
 *             When searching for a character or string literal (or for some other
 *             target that allows resynching the input stream after a parse error),
 *             how far should the search progress?  Values currently supported:
 *                PDC_Line_Stop : stop at the first newline encountered (or EOF)
 *                PDC_EOF_Stop  : stop at EOF (no more input)
 *
 * The default discipline is PDC_default_disc.  It can be copied and modified, e.g.:
 *
 *     PDC_disc_t my_disc = PDC_default_disc;
 *     my_disc.flags |= (PDC_flags_t)PDC_WSPACE_OK;
 */

typedef struct PDC_s               PDC_t;
typedef struct PDC_disc_s          PDC_disc_t;
typedef struct PDC_loc_s           PDC_loc_t;
typedef struct PDC_base_ed_s       PDC_base_ed;
typedef enum   PDC_base_em_e       PDC_base_em;
typedef enum   PDC_panicStop_e     PDC_panicStop;
typedef enum   PDC_errorRep_e      PDC_errorRep;

extern PDC_disc_t PDC_default_disc;

/* ================================================================================ */
/* BASIC LIBRARY TYPES */

typedef signed _ast_int1_t     PDC_int8;
typedef signed _ast_int2_t     PDC_int16;
typedef signed _ast_int4_t     PDC_int32; 
typedef signed _ast_int8_t     PDC_int64; 

typedef unsigned _ast_int1_t   PDC_uint8;
typedef unsigned _ast_int2_t   PDC_uint16;
typedef unsigned _ast_int4_t   PDC_uint32;
typedef unsigned _ast_int8_t   PDC_uint64;

typedef PDC_int8               PDC_aint8_rep;
typedef PDC_aint8_rep          PDC_aint8;

typedef PDC_base_em            PDC_aint8_em;
typedef PDC_base_ed            PDC_int8_ed;

typedef PDC_uint8              PDC_auint8_rep;
typedef PDC_auint8_rep         PDC_auint8;
typedef PDC_base_em            PDC_auint8_em;
typedef PDC_base_ed            PDC_auint8_ed;

typedef PDC_int32              PDC_aint32_rep;
typedef PDC_aint32_rep         PDC_aint32;
typedef PDC_base_em            PDC_aint32_em;
typedef PDC_base_ed            PDC_aint32_ed;

typedef PDC_uint32             PDC_auint32_rep;
typedef PDC_auint32_rep        PDC_auint32;
typedef PDC_base_em            PDC_auint32_em;
typedef PDC_base_ed            PDC_auint32_ed;

typedef char *                 PDC_string;

#define PDC_MIN_INT8      -128
#define PDC_MAX_INT8       127
#define PDC_MAX_UINT8      256

#define PDC_MIN_INT16   -32768
#define PDC_MAX_INT16    32767
#define PDC_MAX_UINT16   65535

/* ================================================================================ */ 
/* ERROR REPORTING FUNCTIONS */

/* 
 * An error function outputs a formatted error message, where level indicates:
 *     positive #    : warning/error (the larger the number, the greater the severity)
 *     negative # -K : debug msg at debug level K
 */
typedef int (*PDC_error_f)(PDC_t* pdc, PDC_disc_t* disc, int level, ...);

/* ================================================================================ */
/* LIBRARY TYPES */

enum PDC_base_em_e { PDC_CheckAndSet, PDC_Check, PDC_Ignore };

enum PDC_panicStop_e { PDC_Line_Stop /* , PDC_EOF_Stop */ };    /* At the moment, only PDC_Line_Stop is supported */

enum PDC_errorRep_e { PDC_errorRep_Max, PDC_errorRep_Med, PDC_errorRep_Min, PDC_errorRep_None };

/* A position has a beginning and an ending: it marks the first and last
 * character where something interesting happened, e.g., a field with
 * invalid format.  In cases where clearcut boundaries for an error
 * are not known, the parse position where the error was 'found'
 * is used for both the begin and end positions.
 */

struct PDC_loc_s {
  size_t       beginLine;
  size_t       beginChar;
  size_t       endLine;
  size_t       endChar;
};

struct PDC_base_ed_s {
  int            panic;
  PDC_errCode_t  errCode;
  PDC_loc_t      loc;
};

struct PDC_disc_s {
  PDC_flags_t           version;   /* interface version */
  PDC_flags_t           flags;     /* control flags */
  PDC_panicStop         p_stop;    /* controls scope of panic */
  PDC_error_f           errorf;    /* error function using  ... */
  PDC_errorRep          e_rep;     /* controls error reporting */
};

/* ================================================================================ */
/* TOP-LEVEL LIBRARY FUNCTIONS */

PDC_error_t  PDC_open          (PDC_disc_t* disc, PDC_t** pdc_out);
PDC_error_t  PDC_close         (PDC_t* pdc, PDC_disc_t* disc); 

/* ================================================================================ */
/* TOP-LEVEL IO FUNCTIONS */

PDC_error_t  PDC_IO_fopen      (PDC_t* pdc, char* path, PDC_disc_t* disc);
PDC_error_t  PDC_IO_fclose     (PDC_t* pdc, PDC_disc_t* disc);

/* ================================================================================ */
/* STRING READ FUNCTIONS */

/* related helper function: free string allocated by a string read function */
PDC_error_t PDC_free_string(PDC_t* pdc, char* str, PDC_disc_t* disc);

/*
 * The string read functions each has a different way of specifying how much
 * to read: string_fw_read specifies a fixed width, string_stopChar_read specifies
 * a single stop character (can be 0 to specify eof as the stop character),
 * and string_stopRegexp_read specifies a string containing a regular expression**,
 * where a match of the regular expression stops the string.  The stop char(s)
 * are not included in the string.
 *
 *   ** At the moment, the only legal regular expression has the form
 *               [<chars>]
 *      i.e., a set of stop chars can be specified.
 * 
 * If an expected stop char/pattern/width is found, PDC_OK is returned.
 * If !em || *em == PDC_CheckAndSet, then:
 *   + if s_out is non-null, a null-terminated form the string is placed in a
 *     newly-allocated memory buffer and *s_out is set to point to this string copy.
 *     The buffer should eventually be freed using PDC_free_string, as in:
 *         PDC_free_string(pdc, string_ptr, 0);
 *   + if l_out is non-null, *l_out is set to the length of the string
 *     (not including the null terminator).
 *
 * Note that if the IO cursor is already at a stop character, then
 * a string of length zero results.
 * 
 * If an expected stop condition is not encountered, the
 * IO cursor position is unchanged.  Error codes used:
 *     PDC_WIDTH_NOT_AVAILABLE
 *     PDC_STOPCHAR_NOT_FOUND
 *     PDC_STOPREGEXP_NOT_FOUND
 *     PDC_INVALID_REGEXP
 */

PDC_error_t PDC_string_fw_read(PDC_t* pdc, PDC_base_em* em, size_t width,
			       PDC_base_ed* ed, char** s_out, size_t* l_out, PDC_disc_t* disc);

PDC_error_t PDC_string_stopChar_read(PDC_t* pdc, PDC_base_em* em, unsigned char stopChar,
				     PDC_base_ed* ed, char** s_out, size_t* l_out,  PDC_disc_t* disc);

PDC_error_t PDC_string_stopRegexp_read(PDC_t* pdc, PDC_base_em* em, const char* stopRegexp,
				       PDC_base_ed* ed, char** s_out, size_t* l_out,  PDC_disc_t* disc);

/* ================================================================================ */
/* ASCII INTEGER READ FUNCTIONS */

/*
 * Documentation for variable-width ascii integer read functions:
 *
 * An ascii representation of an integer value (a string of digits in [0-9])
 * is assumed to be at the current cursor position, where
 * if the target type is a signed type a leading - or + is allowed and
 * if unsigned a leading + is allowed.  If (disc flags & PDC_WSPACE_OK), leading
 * white space is skipped, otherwise leading white space causes an error.
 * Thus, the string to be converted consists of: optional white space,
 * optional +/-, and all consecutive digits (first nondigit marks end).
 *
 * RETURN VALUE: PDC_error_t
 *
 * Upon success, PDC_OK returned: 
 *   + the IO cursor is advanced to just beyond the last digit
 *   + if !em || *em == PDC_CheckAndSet, the out param is assigned a value
 *
 * PDC_ERROR is returned on error.
 * Cursor advancement/err settings for different error cases:
 *
 * (1) If IO cursor is at EOF
 *     => IO cursor remains at EOF
 *     => If !em || *em < PDC_Ignore:
 *           + ed->errCode set to PDC_AT_EOF
 *           + ed->loc begin/end set to EOF 'location'
 *             (last line number, 1 past last char in line)
 * (2a) There is leading white space and not (disc flags & PDC_WSPACE_OK)
 * (2b) The target is unsigned and the first char is a -
 * (2c) The first character is not a +, -, or in [0-9]
 * (2d) First character is allowable + or -, following by a char that is not a digit
 * For the above 4 cases:
 *     => IO cursor is not advanced
 *     => If !em || *em < PDC_Ignore:
 *          + ed->errCode set to PDC_INVALID_AINT / PDC_INVALID_AUINT (depending on target type)
 *          + ed->loc begin/end set to the IO cursor position.
 * (3) A valid ascii integer string is found, but it describes
 *     an integer that does not fit in the specified target type
 *     => IO cursor is advanced just beyond the last digit
 *     => If !em || *em < PDC_Ignore:
 *          + ed->errCode set to PDC_RANGE
 *          + ed->loc begin/end set to line/char position of start and end of the ascii integer
 */

PDC_error_t PDC_aint8_read(PDC_t* pdc, PDC_base_em* em,
			   PDC_base_ed* ed, PDC_int8* res_out, PDC_disc_t* disc);

PDC_error_t PDC_aint16_read(PDC_t* pdc, PDC_base_em* em,
			    PDC_base_ed* ed, PDC_int16* res_out, PDC_disc_t* disc);

PDC_error_t PDC_aint32_read(PDC_t* pdc, PDC_base_em* em,
			    PDC_base_ed* ed, PDC_int32* res_out, PDC_disc_t* disc);

PDC_error_t PDC_aint64_read(PDC_t* pdc, PDC_base_em* em,
			    PDC_base_ed* ed, PDC_int64* res_out, PDC_disc_t* disc);


PDC_error_t PDC_auint8_read(PDC_t* pdc, PDC_base_em* em,
			    PDC_base_ed* ed, PDC_uint8* res_out, PDC_disc_t* disc);

PDC_error_t PDC_auint16_read(PDC_t* pdc, PDC_base_em* em,
			     PDC_base_ed* ed, PDC_uint16* res_out, PDC_disc_t* disc);

PDC_error_t PDC_auint32_read(PDC_t* pdc, PDC_base_em* em,
			     PDC_base_ed* ed, PDC_uint32* res_out, PDC_disc_t* disc);

PDC_error_t PDC_auint64_read(PDC_t* pdc, PDC_base_em* em,
			     PDC_base_ed* ed, PDC_uint64* res_out, PDC_disc_t* disc);


/*
 * Fixed-width ascii integer read functions:
 *    Like the above, only a fixed width in input characters is specified, and
 *    only those characters are examined.  E.g., input '11112222' could be used
 *    to read two fixed-width ascii integers of width 4.
 *
 * N.B. The APIs require width > 0.  If width <= 0 is given, an immediate error 
 * return occurs, without setting ed's location or error code.
 *
 * Other differences from the variable-width read functions:
 *
 * 1. It is an error if the entire specified width is not an integer, e.g.,
 *    for fixed width 4, input '111|' is an error
 *
 * 2. (disc flags & PDC_WSPACE_OK) indicates whether leading OR trailing spaces are OK, e.g.,
 *    for fixed width 4, input ' 1  ' is not an error is wpace_ok is 1
 *    (trailing white space is not an issue for variable-width routines)
 *
 * 3. If the specified width is available, it is always consumed, even if there is an error.
 *    In this case the location begin/end is set to the first and last character of the
 *    specified fixed-width field. 
 *
 *    If the specified width is *not* available (EOR/EOF hit), IO cursor is not advanced and
 *      if !em || *em < PDC_Ignore:
 *        + ed->errCode set to PDC_WIDTH_NOT_AVAILABLE
 *        + ed->loc begin/end set to line/char position of start/end of the 'too small' field
 */

PDC_error_t PDC_fw_aint8_read(PDC_t* pdc, PDC_base_em* em, size_t width,
			      PDC_base_ed* ed, PDC_int8* res_out, PDC_disc_t* disc);

PDC_error_t PDC_fw_aint16_read(PDC_t* pdc, PDC_base_em* em, size_t width,
			       PDC_base_ed* ed, PDC_int16* res_out, PDC_disc_t* disc);

PDC_error_t PDC_fw_aint32_read(PDC_t* pdc, PDC_base_em* em, size_t width,
			       PDC_base_ed* ed, PDC_int32* res_out, PDC_disc_t* disc);

PDC_error_t PDC_fw_aint64_read(PDC_t* pdc, PDC_base_em* em, size_t width,
			       PDC_base_ed* ed, PDC_int64* res_out, PDC_disc_t* disc);


PDC_error_t PDC_fw_auint8_read(PDC_t* pdc, PDC_base_em* em, size_t width,
			       PDC_base_ed* ed, PDC_uint8* res_out, PDC_disc_t* disc);

PDC_error_t PDC_fw_auint16_read(PDC_t* pdc, PDC_base_em* em, size_t width,
				PDC_base_ed* ed, PDC_uint16* res_out, PDC_disc_t* disc);

PDC_error_t PDC_fw_auint32_read(PDC_t* pdc, PDC_base_em* em, size_t width,
				PDC_base_ed* ed, PDC_uint32* res_out, PDC_disc_t* disc);

PDC_error_t PDC_fw_auint64_read(PDC_t* pdc, PDC_base_em* em, size_t width,
				PDC_base_ed* ed, PDC_uint64* res_out, PDC_disc_t* disc);

/* ================================================================================ */
/* BINARY INTEGER READ FUNCTIONS */

/* These functions parse signed or unsigned binary integers.
 * The number of functions required for a given target size varies, so
 * each set of functions is documented below.
 *
 * For all cases, if the specified number of bytes is available, it is always read.
 * If the width is not available, the IO cursor is not advanced, and
 *    if !em || *em < PDC_Ignore:
 *        + ed->errCode set to PDC_WIDTH_NOT_AVAILABLE
 *        + ed->loc begin/end set to line/char position of start/end of the 'too small' field
 */

/* 8-bit read functions:
 *     For these functions, there is no issue of byte ordering or machine architecture,
 *     thus there is only 1 signed and 1 unsigned version.
 */
   
PDC_error_t PDC_bint8_read(PDC_t* pdc, PDC_base_em* em,
			   PDC_base_ed* ed, PDC_int8* res_out, PDC_disc_t* disc);
PDC_error_t PDC_buint8_read(PDC_t* pdc, PDC_base_em* em,
			    PDC_base_ed* ed, PDC_uint8* res_out, PDC_disc_t* disc);

/* 16-bit read functions:
 *     For these functions, byte ordering matters: rev/norev indicates
 *     whether the 2 bytes that are read are reversed.
 */
   
PDC_error_t PDC_bint16_rev_read(PDC_t* pdc, PDC_base_em* em,
				PDC_base_ed* ed, PDC_int16* res_out, PDC_disc_t* disc);
PDC_error_t PDC_bint16_norev_read(PDC_t* pdc, PDC_base_em* em,
				  PDC_base_ed* ed, PDC_int16* res_out, PDC_disc_t* disc);
PDC_error_t PDC_buint16_rev_read(PDC_t* pdc, PDC_base_em* em,
				 PDC_base_ed* ed, PDC_uint16* res_out, PDC_disc_t* disc);
PDC_error_t PDC_buint16_norev_read(PDC_t* pdc, PDC_base_em* em,
				   PDC_base_ed* ed, PDC_uint16* res_out, PDC_disc_t* disc);
/* 32-bit read functions:
 *     For these functions, byte ordering matters: rev/norev indicates
 *     whether the 4 bytes that are read are reversed.
 */
   
PDC_error_t PDC_bint32_rev_read(PDC_t* pdc, PDC_base_em* em,
				PDC_base_ed* ed, PDC_int32* res_out, PDC_disc_t* disc);
PDC_error_t PDC_bint32_norev_read(PDC_t* pdc, PDC_base_em* em,
				  PDC_base_ed* ed, PDC_int32* res_out, PDC_disc_t* disc);
PDC_error_t PDC_buint32_rev_read(PDC_t* pdc, PDC_base_em* em,
				 PDC_base_ed* ed, PDC_uint32* res_out, PDC_disc_t* disc);
PDC_error_t PDC_buint32_norev_read(PDC_t* pdc, PDC_base_em* em,
				   PDC_base_ed* ed, PDC_uint32* res_out, PDC_disc_t* disc);

/* 64-bit read functions:
 *     For these functions, two transforms may be necessary:
 *        rev/norev indicates whether the 8 bytes are reversed, and
 *        swap/noswap indicates whether the upper and lower 32 bit values are subsequently swapped
 */

PDC_error_t PDC_bint32_rev_swap_read(PDC_t* pdc, PDC_base_em* em,
				     PDC_base_ed* ed, PDC_int32* res_out, PDC_disc_t* disc);
PDC_error_t PDC_bint32_rev_noswap_read(PDC_t* pdc, PDC_base_em* em,
				       PDC_base_ed* ed, PDC_int32* res_out, PDC_disc_t* disc);
PDC_error_t PDC_bint32_norev_swap_read(PDC_t* pdc, PDC_base_em* em,
				       PDC_base_ed* ed, PDC_int32* res_out, PDC_disc_t* disc);
PDC_error_t PDC_bint32_norev_noswap_read(PDC_t* pdc, PDC_base_em* em,
					 PDC_base_ed* ed, PDC_int32* res_out, PDC_disc_t* disc);

PDC_error_t PDC_buint32_rev_swap_read(PDC_t* pdc, PDC_base_em* em,
				      PDC_base_ed* ed, PDC_uint32* res_out, PDC_disc_t* disc);
PDC_error_t PDC_buint32_rev_noswap_read(PDC_t* pdc, PDC_base_em* em,
					PDC_base_ed* ed, PDC_uint32* res_out, PDC_disc_t* disc);
PDC_error_t PDC_buint32_norev_swap_read(PDC_t* pdc, PDC_base_em* em,
					PDC_base_ed* ed, PDC_uint32* res_out, PDC_disc_t* disc);
PDC_error_t PDC_buint32_norev_noswap_read(PDC_t* pdc, PDC_base_em* em,
					  PDC_base_ed* ed, PDC_uint32* res_out, PDC_disc_t* disc);

/* ================================================================================ */

#endif  /* __LIBPADSC_H__ */
