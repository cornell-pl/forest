#pragma prototyped
/*
 * padsc library interface
 * 
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#ifndef __LIBPADSC_H__
#define __LIBPADSC_H__

#include <ast.h>
#include <ast_common.h>
#include <vmalloc.h>
#include <sfio.h>
#include <sfstr.h>
#include <ctype.h>
#include <dt.h>
#include <error.h>
#include "rbuf.h"
/* XXX K: this should not be here, put it in hack include file XXX */
extern void bzero(void *s, size_t n);

/* ================================================================================
 * LIBRARY DISCIPLINE TYPES
 *
 * The Main Discipline Type
 * ------------------------
 *
 * PDC_disc_t is the main discipline type.  This section gives an overview
 * of each field: 
 *
 *   version  : interface version
 *   flags    : control flags: some combination of the following
 *                 PDC_WSPACE_OK: for variable-width ascii integers, indicates
 *                                leading white space is OK; for fixed-width ascii
 *                                integers, indicates leading and/or trailing
 *                                white space is OK
 *
 *   errorf   : error reporting function.  See "DISC FUNCTION FOR ERROR REPORTING" below.
 *
 *   e_rep    : error reporting, one of:
 *                PDC_errorRep_None : do not generate descriptive error reports
 *                PDC_errorRep_Min  : minimal reporting: report errCode, IO elt num/char position
 *                PDC_errorRep_Med  : medium reporting:  like Min, but adds descriptive string
 *                PDC_errorRep_Max  : maximum reporting, like Med, but adds offending IO elt up to error position
 *
 *   m_endian  : machine endian-ness (PDC_bigEndian or PDC_littleEndian)
 *
 *   d_endian  : data endian-ness    (PDC_bigEndian or PDC_littleEndian)
 *                 If m_endian != d_endian, then the byte order of binary integers is swapped 
 *                 by the binary integer read functions.  See comments below about
 *                 the CHECK_ENDIAN pragma.
 *
 *   io_disc  : This field contains a pointer to a sub-discipline obj of type
 *              PDC_IO_dist_t which is used to enable reading different kinds
 *              of data files.  See pdc_io_disc.h for details.
 *
 *  Limiting the scanning scope:
 *
 *  When scanning for a character, string literal, or pattern,
 *  how far should the scan go before giving up?  If a record-based
 *  file read discipline is used, then EOR markers serve to stop a scan.
 *  In addition, the following PDC_disc_t fields can be used to provide
 *  stop conditions.  Specifying such stop conditions is more 
 *  important for read disciplines that are not record based.
 *
 *   stop_regexp : a regular expression specifying a stop pattern:
 *                 if set to NiL, no stop_regexp constraint is imposed.
 *                 See PDC_regexp_compile below.
 *
 *   stop_maxlen : a maximum # of bytes that will be traversed by a scan.
 *                      if set to 0, no stop_maxlen constraint is imposed.
 *
 * The default disc is PDC_default_disc.  It provides the following defaults:
 *    version:      PDC_VERSION (above) 
 *    flags:        0
 *    stop_regexp:  0
 *    stop_maxlen:  0
 *    errorf:       PDC_errorf
 *    e_rep:        PDC_errorRep_Max
 *    m_endian:     PDC_bigEndian
 *    d_endian:     PDC_bigEndian
 *    io_disc:      NULL -- a default IO discipline (nlrec: for newline-terminated records)
 *                  is installed on PDC_open if one is not installed beforehand
 *
 * Here is an example initialization that modifies the default discipline
 * and installs one of the IO disciplines:
 *
 *     PDC_t *pdc;
 *     PDC_disc_t my_disc = PDC_default_disc;
 *     my_disc.flags |= (PDC_flags_t)PDC_WSPACE_OK;
 *     PDC_norec_install(&my_disc, 0);
 *     if (PDC_ERR == PDC_open(&pdc, &my_disc)) {
 *       fprintf(stderr, "Failed to open PDC library handle\n");
 *       exit(-1);
 *     }
 *     -- start using pdc
 */

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
  PDC_ERR                           =   -1
} PDC_error_t;

typedef enum PDC_errCode_t_e {
  PDC_NO_ERR                        =    0,

  PDC_BAD_PARAM                     =    1,
  PDC_SYS_ERR                       =    2,
  PDC_IO_ERR                        =    4,

  PDC_CHKPOINT_ERR                  =   11,
  PDC_COMMIT_ERR                    =   12,
  PDC_RESTORE_ERR                   =   13,
  PDC_ALLOC_ERR                     =   14,
  PDC_FORWARD_ERR                   =   15,
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
  PDC_UNION_MATCH_ERR               =  130,
  PDC_ENUM_MATCH_ERR                =  140,
  PDC_TYPEDEF_CONSTRAINT_ERR        =  150,

  PDC_AT_EOF                        =  160,
  PDC_AT_EOR                        =  161,
  PDC_EXTRA_BEFORE_EOR              =  162,
  PDC_RANGE                         =  170,
  PDC_INVALID_AINT                  =  180,
  PDC_INVALID_AUINT                 =  181,
  PDC_INVALID_BINT                  =  182,
  PDC_INVALID_BUINT                 =  183,
  PDC_CHAR_LIT_NOT_FOUND            =  190,
  PDC_STR_LIT_NOT_FOUND             =  200,
  PDC_REGEXP_NOT_FOUND              =  210,
  PDC_INVALID_REGEXP                =  220,
  PDC_WIDTH_NOT_AVAILABLE           =  230
} PDC_errCode_t;

/* ================================================================================ */
/* INTERFACE LIBRARY TYPES: FORWARD DECLS */

/* The struct and enum decls for these types are in this file:
 *     PDC_t*        : runtime library handle (opaque)
 *                      initialized with PDC_open, passed as first arg to most library routines
 *     PDC_disc_t*   : handle to user-supplied disc, passed as last arg to most library routines
 *     PDC_regexp_t* : handle to a compiled regular expression
 *
 *     PDC_pos_t     : IO position
 *     PDC_loc_t     : IO location / range
 *     PDC_base_ed   : base error descriptor
 *     PDC_base_em   : base error mask
 *     PDC_errorRep  : enum for specifying error reporting level
 *     PDC_endian    : enum for specifying endian-ness
 * 
 * The struct type decls for these types are in pdc_io_disc.h:
 *     PDC_IO_disc_t : sub-discipline type for controlling IO
 *     PDC_IO_elt_t  : element of a linked list managed by the io discipline 
 */

typedef struct PDC_s               PDC_t;
typedef struct PDC_disc_s          PDC_disc_t;
typedef struct PDC_regexp_s        PDC_regexp_t;

typedef struct PDC_pos_s           PDC_pos_t;
typedef struct PDC_loc_s           PDC_loc_t;
typedef struct PDC_base_ed_s       PDC_base_ed;
typedef enum   PDC_base_em_e       PDC_base_em;
typedef enum   PDC_errorRep_e      PDC_errorRep;
typedef enum   PDC_endian_e        PDC_endian;

typedef struct PDC_IO_elt_s        PDC_IO_elt_t;
typedef struct PDC_IO_disc_s       PDC_IO_disc_t;

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

typedef PDC_int64              PDC_aint64_rep;
typedef PDC_aint64_rep         PDC_aint64;
typedef PDC_base_em            PDC_aint64_em;
typedef PDC_base_ed            PDC_aint64_ed;

typedef PDC_uint64             PDC_auint64_rep;
typedef PDC_auint64_rep        PDC_auint64;
typedef PDC_base_em            PDC_auint64_em;
typedef PDC_base_ed            PDC_auint64_ed;

typedef PDC_base_ed            PDC_string_ed;

typedef struct PDC_string_s {
  size_t    len;
  char      *str;
  RBuf_t    *rbuf;
} PDC_string;

/* ================================================================================ */
/* USEFUL CONSTANTS */

#define PDC_MIN_INT8                        -128
#define PDC_MAX_INT8                         127
#define PDC_MAX_UINT8                        255U

#define PDC_MIN_INT16                     -32768
#define PDC_MAX_INT16                      32767
#define PDC_MAX_UINT16                     65535U

#define PDC_MIN_INT32                -2147483647L
#define PDC_MAX_INT32                 2147483647L
#define PDC_MAX_UINT32                4294967295UL

#define PDC_MIN_INT64       -9223372036854775807LL
#define PDC_MAX_INT64        9223372036854775807LL
#define PDC_MAX_UINT64      18446744073709551615ULL

/* ================================================================================ */ 
/* DISC FUNCTION FOR ERROR REPORTING */

/* Prototypes:
 *
 * A PDC_error_f function is an output function that output a
 * formatted error message, where level should be one of:
 *      -K : negative # is used for debugging messages
 *       PDC_LEV_INFO  : informative, no prefixes appended to message
 *       PDC_LEV_WARN  : warning
 *       PDC_LEV_ERR   : soft error
 *       PDC_LEV_FATAL : fatal error, program should exit 
 * One can 'or' in the following flags (as in PDC_LEV_WARN|PDC_FLG_PROMPT):
 *       PDC_FLG_PROMPT  : do not emit a newline
 *       PDC_FLG_SYSERR  : add a description of errno (errno should be a system error)
 *       PDC_FLG_LIBRARY : error is from library
 * Give a level lev that may include flags, one can use:
 *   PDC_GET_LEV(lev) : just the level   example: PDC_GET_LEV(lev) == PDC_LEV_FATAL
 *   PDC_GET_FLG(lev) : just the flags   example: PDC_GET_FLG(lev) & PDC_FLG_PROMPT
 *
 * LIBRARY messages are forced if env variable ERROR_OPTIONS includes 'library'
 * SYSERR (errno) messages are forced if it includes 'system'
 * Debug messages at level >= -K enabled if it includes 'trace=K'.
 *
 * Thus, to enable debugging message >= level -4, library messages, and
 * system errno text:
 *
 *    export ERROR_OPTIONS="trace=4 library system"   -- for sh/ksh/bash/etc
 *    setenv ERROR_OPTIONS "trace=4 library system"   -- for csh/tcsh/etc
 *
 * Note: For convenience, if the first arg, library name libnm, is non-NULL,
 * then flag PDC_FLG_LIBRARY is automatically or'd into level.  In the normal
 * case, a null libnm should be used. 
 */

typedef int (*PDC_error_f)(const char *libnm, int level, ...);

/*
 * The default implementation:
 */

int PDC_errorf(const char *libnm, int level, ...);

/* ================================================================================ */
/* LIBRARY TYPES */

/* type PDC_base_em: */
enum PDC_base_em_e { PDC_CheckAndSet, PDC_Check, PDC_Ignore };

/* type PDC_errorRep: */
enum PDC_errorRep_e { PDC_errorRep_Max, PDC_errorRep_Med, PDC_errorRep_Min, PDC_errorRep_None };

/* type PDC_endian: */
enum PDC_endian_e { PDC_bigEndian, PDC_littleEndian };

/* helper for PDC_endian: */
const char *PDC_Endian2String(PDC_endian e);

/* A PDC_pos_t (IO position) has a byte position within the num'th read element
 * where unit describes the element kind (e.g., "line", "1K Block", etc.)
 *
 * A PDC_loc_t (IO location) has two positions, b and e, marking the
 * first byte and the last byte where something interesting
 * happened, e.g., a field with an invalid format.
 *
 * In cases where clearcut boundaries for an error are not known, the
 * parse position where the error was 'found' is used for both the
 * begin and end positions.  In this case, and in some other cases,
 * the end byte is set to one less than the start byte, indicating an
 * error that occurred just before the start byte (as opposed to an
 * error that spans the start byte). 
 */

/* type PDC_pos_t: */
struct PDC_pos_s {
  size_t       byte;
  size_t       num;
  const char  *unit;
};

/* type PDC_loc_t: */
struct PDC_loc_s {
  PDC_pos_t b;
  PDC_pos_t e;
};

/* type PDC_base_ed: */
struct PDC_base_ed_s {
  int            panic;
  PDC_errCode_t  errCode;
  PDC_loc_t      loc;
};

/* type PDC_disc_t: */
struct PDC_disc_s {
  PDC_flags_t           version;      /* interface version */
  PDC_flags_t           flags;        /* control flags */
  PDC_regexp_t          *stop_regexp; /* scan stop pattern, use 0 to disable */
  size_t                stop_maxlen;  /* max scan distance, use 0 to disable */
  PDC_error_f           errorf;       /* error function using  ... */
  PDC_errorRep          e_rep;        /* controls error reporting */
  PDC_endian            m_endian;     /* endian-ness of the machine */ 
  PDC_endian            d_endian;     /* endian-ness of the data */ 
  PDC_IO_disc_t         *io_disc;     /* sub-discipline for controlling IO */
};

/* ================================================================================ */
/* LIBRARY HANDLE OPEN/CLOSE FUNCTIONS */

PDC_error_t  PDC_open          (PDC_disc_t *disc, PDC_t **pdc_out);
PDC_error_t  PDC_close         (PDC_t *pdc, PDC_disc_t *disc); 

/* ================================================================================ */
/* RMM FUNCTIONS */

/* PDC_rmm_zero    : get rbuf memory manager that zeroes allocated memory
 * PDC_rmm_nozero  : get rbuf memory manager that does not zero allocated memory
 *
 * See rbuf.h for the RMM/Rbuf memory management API
 */

RMM_t* PDC_rmm_zero  (PDC_t *pdc, PDC_disc_t *disc);
RMM_t* PDC_rmm_nozero(PDC_t *pdc, PDC_disc_t *disc);

/* ================================================================================ */
/* TOP-LEVEL IO FUNCTIONS */

/* PDC_IO_fopen    : open a file for reading
 *                   uses disc->fopen_fn, if present, otherwise default PDC_fopen
 *                   returns PDC_OK on success, PDC_ERR on error
 *
 * PDC_IO_fclose   : close the previously opened file
 *                   uses disc->fclose_fn, if present, otherwise default PDC_fclose
 *                   returns PDC_OK on success, PDC_ERR on error
 *
 * PDC_IO_next_rec : advances current IO position to start of the next record, if any.
 *                   returns PDC_OK on success, PDC_ERR on failure. 
 *                   (failure includes hitting EOF before EOR)
 *                   For PDC_OK case, sets (*skipped_bytes_out) to the number of
 *                   data bytes that were passed over while searching for EOR.
 *
 * PDC_IO_at_EOR   : returns 1 if the current IO position is at EOR, otherwise 0
 *
 * PDC_IO_at_EOF   : returns 1 if current IO position is at EOF, otherwise 0
 *
 * PDC_IO_getPos   : fill in (*pos) with IO position
 * PDC_IO_getLocB  : fill in loc->b with IO position
 * PDC_IO_getLocE  : fill in loc->e with IO position
 * PDC_IO_getLoc   : fill in both loc->b and loc->e with IO position
 *   All of these above take an offset.  If offset is 0, the current IO position is
 *   used, otherwise the position used is K bytes from the current IO position
 *   (offset == K ... offset is an int, and can be positive or negative).
 *   Note the current IO position does not change.  PDC_ERR is returned if
 *   info about the specified position cannot be determined. 
 *   EOR marker bytes (if any) are ignored when moving forward or back
 *   based on offset -- offset only refers to data bytes.
 */

PDC_error_t  PDC_IO_fopen    (PDC_t *pdc, char *path, PDC_disc_t *disc);
PDC_error_t  PDC_IO_fclose   (PDC_t *pdc, PDC_disc_t *disc);
PDC_error_t  PDC_IO_next_rec (PDC_t *pdc, size_t *skipped_bytes_out, PDC_disc_t *disc);

int          PDC_IO_at_EOR   (PDC_t *pdc, PDC_disc_t *disc);
int          PDC_IO_at_EOF   (PDC_t *pdc, PDC_disc_t *disc);

PDC_error_t  PDC_IO_getPos   (PDC_t *pdc, PDC_pos_t *pos, int offset, PDC_disc_t *disc); 
PDC_error_t  PDC_IO_getLocB  (PDC_t *pdc, PDC_loc_t *loc, int offset, PDC_disc_t *disc); 
PDC_error_t  PDC_IO_getLocE  (PDC_t *pdc, PDC_loc_t *loc, int offset, PDC_disc_t *disc); 
PDC_error_t  PDC_IO_getLoc   (PDC_t *pdc, PDC_loc_t *loc, int offset, PDC_disc_t *disc); 

/* ================================================================================ */
/* LITERAL READ FUNCTIONS */

/* PDC_char_lit_read / str_lit_read:
 *
 * EFFECT: verify IO cursor points to specified char/string, move IO cursor just beyond
 *         N.B. If *em is PDC_Ignore, IO cursor is advanced by the specified length
 *              (1 char / length of string) without checking
 *
 * RETURNS: PDC_error_t
 *            OK    => IO cursor now points just beyond char / string
 *            ERROR => IO cursor did not point to char/string; unchanged
 *               (PDC_CHAR_LIT_NOT_FOUND / PDC_STR_LIT_NOT_FOUND)
 */

PDC_error_t PDC_char_lit_read(PDC_t *pdc, PDC_base_em *em,
			      PDC_base_ed *ed, unsigned char c, PDC_disc_t *disc);

PDC_error_t PDC_str_lit_read(PDC_t *pdc, PDC_base_em *em,
			     PDC_base_ed *ed, const PDC_string *s, PDC_disc_t *disc);

/* PDC_countXtoY: count occurrences of char x until char y
 * Uses disc->p_stop to determine how far to scan for y.
 * Does not modify the IO cursor position.  Cases:
 *   1. IO cursor is at EOF 
 *     => If !em || *em < PDC_Ignore:
 *           + ed->errCode set to PDC_AT_EOF
 *           + ed->loc begin/end set to EOF 'location'
 *             (last elt number, 1 past last char in elt)
 *     PDC_ERR returned   
 *   2. Char y is not found
 *     => If !em || *em < PDC_Ignore:
 *           + ed->errCode set to PDC_CHAR_LIT_NOT_FOUND
 *           + ed->loc begin/end set to current IO cursor location
 *     PDC_ERR returned   
 *   3. Char y found
 *     if res_out, *res_out is set to the number of occurrences of x
 *     PDC_OK returned
 */

PDC_error_t PDC_countXtoY(PDC_t *pdc, PDC_base_em *em, PDC_uint8 x, PDC_uint8 y,
		          PDC_base_ed *ed, PDC_int32 *res_out, PDC_disc_t *disc);

/* ================================================================================ */
/* DATE/TIME READ FUNCTIONS */

/* PDC_adate_read : attempts to read an ascii date string and convert it to
 *                  seconds since the epoch.  For the different formats supported,
 *                  see the libast tmdate documentation.
 *
 * If the current IO cursor position points to a valid ascii date string:
 *   + if res_out, sets *res_out to the resulting date in seconds since the epoch
 *   + advances the IO cursor position to just after the last legal character
 *     in the date string
 *   + returns PDC_OK
 * Otherwise:
 *   + does not advance the IO cursor pos
 *   + returns PDC_ERR
 */

PDC_error_t PDC_adate_read(PDC_t *pdc, PDC_base_em *em, PDC_base_ed *ed, 
			   PDC_uint32 *res_out, PDC_disc_t *disc);

/* ================================================================================ */
/* STRING READ FUNCTIONS */

/* Related helper functions:
 *
 *    PDC_string_init : initialize to valid empty string (no dynamic memory allocated yet)
 *    PDC_string_cleanup : free up the rbuf and any allocated space for the string
 *    PDC_string_copy : Copy len chars from string src into the PDC_string targ;
 *                      allocates RBuf and/or space for the copy, as necessary.
 *                      Although not strictly necessary, null-terminates targ->str.
 *              string_copy returns PDC_ERR on bad arguments or on failure to alloc space,
 *              otherwise it returns PDC_OK
 */

PDC_error_t PDC_string_init(PDC_t *pdc, PDC_string *s, PDC_disc_t *disc);
PDC_error_t PDC_string_cleanup(PDC_t *pdc, PDC_string *s, PDC_disc_t *disc);
PDC_error_t PDC_string_copy(PDC_t *pdc, PDC_string *targ, const char *src, size_t len, PDC_disc_t *disc);

/*
 * Type T with T_init/T_cleanup also must have T_ed_init/T_ed_cleanup.
 * For PDC_string_ed, these turn out to just be no-ops
 *    PDC_string_ed_init    : a no-op
 *    PDC_string_ed_cleanup : a no-op  
 */

PDC_error_t PDC_string_ed_init(PDC_t *pdc, PDC_string_ed *ed, PDC_disc_t *disc);
PDC_error_t PDC_string_ed_cleanup(PDC_t *pdc, PDC_string_ed *ed, PDC_disc_t *disc);

/* The string read functions each has a different way of specifying
 * how much to read: string_fw_read specifies a fixed width,
 * string_stopChar_read specifies a single stop character (can be 0 to
 * specify eof as the stop character), and string_stopRegexp_read
 * specifies a compiled regular expression (see PDC_regexp_compile)
 * where a match of this 'stop expression' stops the string.  The stop
 * char(s) are not included in the resulting string.
 *
 * If an expected stop char/pattern/width is found, PDC_OK is returned.
 * If !em || *em == PDC_CheckAndSet, then:
 *   + if s_out is non-null, a null-terminated form the string is placed in the
 *     memory buffer managed by *s_out; *s_out should have been initialized
 *     at some point prior using PDC_string_init (it can be initialized once
 *     and re-used in string read calls many times).  The memory allocated by *s_out
 *     should ultimately be freed using PDC_string_cleanup.
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

PDC_error_t PDC_string_fw_read(PDC_t *pdc, PDC_base_em *em, size_t width,
			       PDC_base_ed *ed, PDC_string *s_out, PDC_disc_t *disc);

PDC_error_t PDC_string_stopChar_read(PDC_t *pdc, PDC_base_em *em, unsigned char stopChar,
				     PDC_base_ed *ed, PDC_string *s_out, PDC_disc_t *disc);

PDC_error_t PDC_string_stopRegexp_read(PDC_t *pdc, PDC_base_em *em, PDC_regexp_t *stopRegexp,
				       PDC_base_ed *ed, PDC_string *s_out, PDC_disc_t *disc);

/* ================================================================================ */
/* REGULAR EXPRESSION SUPPORT */

/* PDC_regexp_compile: if regexp is a valid regular expression, this function
 * allocates a new compiled regular expression obj, assigns a handle to the obj
 * to (*regexp_out), and returns PDC_OK.  If regexp_out is NULL or regexp is
 * not valid, it returns PDC_ERR.
 *
 * PDC_regexp_free takes a handle to a compiled regexp obj and frees the obj.
 *
 * ** At the moment, only regular expressions of the form
 *               [<chars>]
 *     are supported, i.e., a set of chars can be specified.
 */

PDC_error_t PDC_regexp_compile(PDC_t *pdc, const char *regexp, PDC_regexp_t **regexp_out, PDC_disc_t *disc);
PDC_error_t PDC_regexp_free(PDC_t *pdc, PDC_regexp_t *regexp, PDC_disc_t *disc);

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
 * PDC_ERR is returned on error.
 * Cursor advancement/err settings for different error cases:
 *
 * (1) If IO cursor is at EOF
 *     => IO cursor remains at EOF
 *     => If !em || *em < PDC_Ignore:
 *           + ed->errCode set to PDC_AT_EOF
 *           + ed->loc begin/end set to EOF 'location'
 *             (last elt number, 1 past last char in elt)
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
 *          + ed->loc begin/end set to elt/char position of start and end of the ascii integer
 */

PDC_error_t PDC_aint8_read (PDC_t *pdc, PDC_base_em *em,
			    PDC_base_ed *ed, PDC_int8 *res_out, PDC_disc_t *disc);

PDC_error_t PDC_aint16_read(PDC_t *pdc, PDC_base_em *em,
			    PDC_base_ed *ed, PDC_int16 *res_out, PDC_disc_t *disc);

PDC_error_t PDC_aint32_read(PDC_t *pdc, PDC_base_em *em,
			    PDC_base_ed *ed, PDC_int32 *res_out, PDC_disc_t *disc);

PDC_error_t PDC_aint64_read(PDC_t *pdc, PDC_base_em *em,
			    PDC_base_ed *ed, PDC_int64 *res_out, PDC_disc_t *disc);


PDC_error_t PDC_auint8_read (PDC_t *pdc, PDC_base_em *em,
			     PDC_base_ed *ed, PDC_uint8 *res_out, PDC_disc_t *disc);

PDC_error_t PDC_auint16_read(PDC_t *pdc, PDC_base_em *em,
			     PDC_base_ed *ed, PDC_uint16 *res_out, PDC_disc_t *disc);

PDC_error_t PDC_auint32_read(PDC_t *pdc, PDC_base_em *em,
			     PDC_base_ed *ed, PDC_uint32 *res_out, PDC_disc_t *disc);

PDC_error_t PDC_auint64_read(PDC_t *pdc, PDC_base_em *em,
			     PDC_base_ed *ed, PDC_uint64 *res_out, PDC_disc_t *disc);

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
 *    In this case if !em || *em < PDC_Ignore:
 *       + ed->loc begin/end is set to the first/last char of the fixed-width field. 
 *
 *    If the specified width is *not* available (EOR/EOF hit), IO cursor is not advanced and
 *      if !em || *em < PDC_Ignore:
 *        + ed->errCode set to PDC_WIDTH_NOT_AVAILABLE
 *        + ed->loc begin/end set to elt/char position of start/end of the 'too small' field
 */

PDC_error_t PDC_aint8_fw_read (PDC_t *pdc, PDC_base_em *em, size_t width,
			       PDC_base_ed *ed, PDC_int8 *res_out, PDC_disc_t *disc);

PDC_error_t PDC_aint16_fw_read(PDC_t *pdc, PDC_base_em *em, size_t width,
			       PDC_base_ed *ed, PDC_int16 *res_out, PDC_disc_t *disc);

PDC_error_t PDC_aint32_fw_read(PDC_t *pdc, PDC_base_em *em, size_t width,
			       PDC_base_ed *ed, PDC_int32 *res_out, PDC_disc_t *disc);

PDC_error_t PDC_aint64_fw_read(PDC_t *pdc, PDC_base_em *em, size_t width,
			       PDC_base_ed *ed, PDC_int64 *res_out, PDC_disc_t *disc);


PDC_error_t PDC_auint8_fw_read (PDC_t *pdc, PDC_base_em *em, size_t width,
				PDC_base_ed *ed, PDC_uint8 *res_out, PDC_disc_t *disc);

PDC_error_t PDC_auint16_fw_read(PDC_t *pdc, PDC_base_em *em, size_t width,
				PDC_base_ed *ed, PDC_uint16 *res_out, PDC_disc_t *disc);

PDC_error_t PDC_auint32_fw_read(PDC_t *pdc, PDC_base_em *em, size_t width,
				PDC_base_ed *ed, PDC_uint32 *res_out, PDC_disc_t *disc);

PDC_error_t PDC_auint64_fw_read(PDC_t *pdc, PDC_base_em *em, size_t width,
				PDC_base_ed *ed, PDC_uint64 *res_out, PDC_disc_t *disc);

/* ================================================================================ */
/* BINARY INTEGER READ FUNCTIONS */

/* These functions parse signed or unsigned binary integers.
 * Whether bytes are reversed is controlled by two fields,
 * disc->m_endian and disc->d_endian. If they differ, then the byte
 * order is reversed in the in-memory representation, otherwise it is not.
 *
 * A good way to set the d_endian value in a machine-independent way is to
 * use PRAGMA CHECK_ENDIAN with the first multi-byte binary integer field that appears
 * in the data.  For example, this header definition:
 *
 *
 * pstruct header {
 *    buint16 version : version < 10; //- PRAGMA CHECK_ENDIAN
 *    ..etc..
 * };
 *
 * indicates the first value is a 2-byte unsigned binary integer, version,
 * whose value should be less than 10.   The pragma indicates that there
 * should be two attempts at reading the version field: once with the
 * current disc->d_endian setting, and (if the read fails) once with the
 * opposite disc->d_endian setting.  If the second read succeeds, then
 * the new disc->d_endian setting is retained, otherwise the original
 * disc->d_endian setting is retained.
 * 
 * N.B. The CHECK_ENDIAN pragma is only able to determine the correct endian
 * choice for a field that has an attached constraint, where the
 * wrong choice of endian setting will always cause the constraint to fail.
 * (In the above example, if a value < 10 is read with the wrong d_endian
 * setting, the result is a value that is much greater than 10.) 
 *
 * For all cases, if the specified number of bytes is available, it is always read.
 * If the width is not available, the IO cursor is not advanced, and
 *    if !em || *em < PDC_Ignore:
 *        + ed->errCode set to PDC_WIDTH_NOT_AVAILABLE
 *        + ed->loc begin/end set to elt/char position of start/end of the 'too small' field
 */

PDC_error_t PDC_bint8_read (PDC_t *pdc, PDC_base_em *em,
			    PDC_base_ed *ed, PDC_int8 *res_out, PDC_disc_t *disc);

PDC_error_t PDC_bint16_read(PDC_t *pdc, PDC_base_em *em,
			    PDC_base_ed *ed, PDC_int16 *res_out, PDC_disc_t *disc);

PDC_error_t PDC_bint32_read(PDC_t *pdc, PDC_base_em *em,
			    PDC_base_ed *ed, PDC_int32 *res_out, PDC_disc_t *disc);

PDC_error_t PDC_bint64_read(PDC_t *pdc, PDC_base_em *em,
			    PDC_base_ed *ed, PDC_int64 *res_out, PDC_disc_t *disc);

PDC_error_t PDC_buint8_read (PDC_t *pdc, PDC_base_em *em,
			     PDC_base_ed *ed, PDC_uint8 *res_out, PDC_disc_t *disc);

PDC_error_t PDC_buint16_read(PDC_t *pdc, PDC_base_em *em,
			     PDC_base_ed *ed, PDC_uint16 *res_out, PDC_disc_t *disc);

PDC_error_t PDC_buint32_read(PDC_t *pdc, PDC_base_em *em,
			     PDC_base_ed *ed, PDC_uint32 *res_out, PDC_disc_t *disc);

PDC_error_t PDC_buint64_read(PDC_t *pdc, PDC_base_em *em,
			     PDC_base_ed *ed, PDC_uint64 *res_out, PDC_disc_t *disc);

/* ================================================================================ */
/* BASE TYPE ACCUMULATORS */

/*
 * Each report function takes the following params (in addition to pdc/disc first/last args):
 *   prefix: a descriptive string, usually the field name
 *           if NULL or empty, the string "<top>" is used
 *   what:   string describing kind of data
 *           if NULL, a short form of the accumulator type is used as default,
 *           e.g., "int32" is the default for PDC_int32_acc.
 *   nst:    nesting level: level zero should be used for a top-level report call;
 *           reporting routines bump the nesting level for recursive report calls that
 *           describe sub-parts.  Nesting level -1 indicates a minimal prefix header
 *           should be output, i.e., just the prefix without any adornment.
 *   a:      the accumulator
 */

typedef struct PDC_int_acc_s {
  Dt_t        *dict;
  PDC_uint64  good;
  PDC_uint64  bad;
  PDC_uint64  fold;
  PDC_int64   psum;
  double      avg;
  PDC_int64   min;
  PDC_int64   max;
} PDC_int_acc;

typedef PDC_int_acc PDC_int8_acc;
typedef PDC_int_acc PDC_int16_acc;
typedef PDC_int_acc PDC_int32_acc;
typedef PDC_int_acc PDC_int64_acc;

typedef struct PDC_uint_acc_s {
  Dt_t        *dict;
  PDC_uint64  good;
  PDC_uint64  bad;
  PDC_uint64  fold;
  PDC_uint64  psum;
  double      avg;
  PDC_uint64  min;
  PDC_uint64  max;
} PDC_uint_acc;

typedef PDC_uint_acc PDC_uint8_acc;
typedef PDC_uint_acc PDC_uint16_acc;
typedef PDC_uint_acc PDC_uint32_acc;
typedef PDC_uint_acc PDC_uint64_acc;

typedef struct PDC_string_acc_s {
  Dt_t           *dict;
  PDC_uint32_acc len_accum; /* used for length distribution and good/bad accounting */
} PDC_string_acc;

PDC_error_t PDC_int8_acc_init    (PDC_t *pdc, PDC_int8_acc *a, PDC_disc_t *disc);
PDC_error_t PDC_int8_acc_reset   (PDC_t *pdc, PDC_int8_acc *a, PDC_disc_t *disc);
PDC_error_t PDC_int8_acc_cleanup (PDC_t *pdc, PDC_int8_acc *a, PDC_disc_t *disc);
PDC_error_t PDC_int8_acc_add     (PDC_t *pdc, PDC_int8_acc *a, PDC_base_ed *ed, PDC_int8 *val, PDC_disc_t *disc);
PDC_error_t PDC_int8_acc_report  (PDC_t *pdc, const char *prefix, const char *what, int nst,
				  PDC_int8_acc *a, PDC_disc_t *disc);

PDC_error_t PDC_int16_acc_init    (PDC_t *pdc, PDC_int16_acc *a, PDC_disc_t *disc);
PDC_error_t PDC_int16_acc_reset   (PDC_t *pdc, PDC_int16_acc *a, PDC_disc_t *disc);
PDC_error_t PDC_int16_acc_cleanup (PDC_t *pdc, PDC_int16_acc *a, PDC_disc_t *disc);
PDC_error_t PDC_int16_acc_add     (PDC_t *pdc, PDC_int16_acc *a, PDC_base_ed *ed, PDC_int16 *val, PDC_disc_t *disc);
PDC_error_t PDC_int16_acc_report  (PDC_t *pdc, const char *prefix, const char *what, int nst,
				   PDC_int16_acc *a, PDC_disc_t *disc);

PDC_error_t PDC_int32_acc_init    (PDC_t *pdc, PDC_int32_acc *a, PDC_disc_t *disc);
PDC_error_t PDC_int32_acc_reset   (PDC_t *pdc, PDC_int32_acc *a, PDC_disc_t *disc);
PDC_error_t PDC_int32_acc_cleanup (PDC_t *pdc, PDC_int32_acc *a, PDC_disc_t *disc);
PDC_error_t PDC_int32_acc_add     (PDC_t *pdc, PDC_int32_acc *a, PDC_base_ed *ed, PDC_int32 *val, PDC_disc_t *disc);
PDC_error_t PDC_int32_acc_report  (PDC_t *pdc, const char *prefix, const char *what, int nst,
				   PDC_int32_acc *a, PDC_disc_t *disc);

PDC_error_t PDC_int64_acc_init    (PDC_t *pdc, PDC_int64_acc *a, PDC_disc_t *disc);
PDC_error_t PDC_int64_acc_reset   (PDC_t *pdc, PDC_int64_acc *a, PDC_disc_t *disc);
PDC_error_t PDC_int64_acc_cleanup (PDC_t *pdc, PDC_int64_acc *a, PDC_disc_t *disc);
PDC_error_t PDC_int64_acc_add     (PDC_t *pdc, PDC_int64_acc *a, PDC_base_ed *ed, PDC_int64 *val, PDC_disc_t *disc);
PDC_error_t PDC_int64_acc_report  (PDC_t *pdc, const char *prefix, const char *what, int nst,
				   PDC_int64_acc *a, PDC_disc_t *disc);

PDC_error_t PDC_uint8_acc_init    (PDC_t *pdc, PDC_uint8_acc *a, PDC_disc_t *disc);
PDC_error_t PDC_uint8_acc_reset   (PDC_t *pdc, PDC_uint8_acc *a, PDC_disc_t *disc);
PDC_error_t PDC_uint8_acc_cleanup (PDC_t *pdc, PDC_uint8_acc *a, PDC_disc_t *disc);
PDC_error_t PDC_uint8_acc_add     (PDC_t *pdc, PDC_uint8_acc *a, PDC_base_ed *ed, PDC_uint8 *val, PDC_disc_t *disc);
PDC_error_t PDC_uint8_acc_report  (PDC_t *pdc, const char *prefix, const char *what, int nst,
				   PDC_uint8_acc *a, PDC_disc_t *disc);

PDC_error_t PDC_uint16_acc_init    (PDC_t *pdc, PDC_uint16_acc *a, PDC_disc_t *disc);
PDC_error_t PDC_uint16_acc_reset   (PDC_t *pdc, PDC_uint16_acc *a, PDC_disc_t *disc);
PDC_error_t PDC_uint16_acc_cleanup (PDC_t *pdc, PDC_uint16_acc *a, PDC_disc_t *disc);
PDC_error_t PDC_uint16_acc_add     (PDC_t *pdc, PDC_uint16_acc *a, PDC_base_ed *ed, PDC_uint16 *val, PDC_disc_t *disc);
PDC_error_t PDC_uint16_acc_report  (PDC_t *pdc, const char *prefix, const char *what, int nst,
				    PDC_uint16_acc *a, PDC_disc_t *disc);

PDC_error_t PDC_uint32_acc_init    (PDC_t *pdc, PDC_uint32_acc *a, PDC_disc_t *disc);
PDC_error_t PDC_uint32_acc_reset   (PDC_t *pdc, PDC_uint32_acc *a, PDC_disc_t *disc);
PDC_error_t PDC_uint32_acc_cleanup (PDC_t *pdc, PDC_uint32_acc *a, PDC_disc_t *disc);
PDC_error_t PDC_uint32_acc_add     (PDC_t *pdc, PDC_uint32_acc *a, PDC_base_ed *ed, PDC_uint32 *val, PDC_disc_t *disc);
PDC_error_t PDC_uint32_acc_report  (PDC_t *pdc, const char *prefix, const char *what, int nst,
				    PDC_uint32_acc *a, PDC_disc_t *disc);

PDC_error_t PDC_uint64_acc_init    (PDC_t *pdc, PDC_uint64_acc *a, PDC_disc_t *disc);
PDC_error_t PDC_uint64_acc_reset   (PDC_t *pdc, PDC_uint64_acc *a, PDC_disc_t *disc);
PDC_error_t PDC_uint64_acc_cleanup (PDC_t *pdc, PDC_uint64_acc *a, PDC_disc_t *disc);
PDC_error_t PDC_uint64_acc_add     (PDC_t *pdc, PDC_uint64_acc *a, PDC_base_ed *ed, PDC_uint64 *val, PDC_disc_t *disc);
PDC_error_t PDC_uint64_acc_report  (PDC_t *pdc, const char *prefix, const char *what, int nst,
				    PDC_uint64_acc *a, PDC_disc_t *disc);

PDC_error_t PDC_string_acc_init    (PDC_t *pdc, PDC_string_acc *a, PDC_disc_t *disc);
PDC_error_t PDC_string_acc_reset   (PDC_t *pdc, PDC_string_acc *a, PDC_disc_t *disc);
PDC_error_t PDC_string_acc_cleanup (PDC_t *pdc, PDC_string_acc *a, PDC_disc_t *disc);
PDC_error_t PDC_string_acc_add     (PDC_t *pdc, PDC_string_acc *a, PDC_base_ed *ed, PDC_string* val, PDC_disc_t *disc);
PDC_error_t PDC_string_acc_report  (PDC_t *pdc, const char *prefix, const char *what, int nst,
				    PDC_string_acc *a, PDC_disc_t *disc);

/*
 * char_acc is just like uint8_acc except a different report is generated
 */
typedef PDC_uint8_acc PDC_char_acc;

PDC_error_t PDC_char_acc_init      (PDC_t *pdc, PDC_char_acc *a, PDC_disc_t *disc);
PDC_error_t PDC_char_acc_reset     (PDC_t *pdc, PDC_char_acc *a, PDC_disc_t *disc);
PDC_error_t PDC_char_acc_cleanup   (PDC_t *pdc, PDC_char_acc *a, PDC_disc_t *disc);
PDC_error_t PDC_char_acc_add       (PDC_t *pdc, PDC_char_acc *a, PDC_base_ed *ed, PDC_uint8 *val, PDC_disc_t *disc);
PDC_error_t PDC_char_acc_report    (PDC_t *pdc, const char *prefix, const char *what, int nst,
				    PDC_char_acc *a, PDC_disc_t *disc);

/* A map_<int_type> function maps a given integer type to a string */
typedef const char * (*PDC_int8_map_fn)  (PDC_int8   i);
typedef const char * (*PDC_int16_map_fn) (PDC_int16  i);
typedef const char * (*PDC_int32_map_fn) (PDC_int32  i);
typedef const char * (*PDC_int64_map_fn) (PDC_int64  i);
typedef const char * (*PDC_uint8_map_fn) (PDC_uint8  u);
typedef const char * (*PDC_uint16_map_fn)(PDC_uint16 u);
typedef const char * (*PDC_uint32_map_fn)(PDC_uint32 u);
typedef const char * (*PDC_uint64_map_fn)(PDC_uint64 u);

/*
 * Mapped versions of the integer acc_report functions:
 * these functions are used when integers have associated
 * string values.  
 */
PDC_error_t PDC_int32_acc_report_map(PDC_t *pdc, const char *prefix, const char *what, int nst,
				     PDC_int32_map_fn  fn, PDC_int32_acc *a, PDC_disc_t *disc);

/* ================================================================================ */
/* MISC ROUTINES */

/*
 * PDC_swap_bytes: in-place memory byte order swap
 *    num_bytes should be oneof: 1, 2, 4, 8
 */
PDC_error_t PDC_swap_bytes(PDC_t *pdc, char *bytes, size_t num_bytes, PDC_disc_t *disc);

#endif  /* __LIBPADSC_H__ */
