#ifdef _USE_PROTO
#pragma prototyped
#endif
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
#include <swap.h>
#include <tm.h>
#include <vmalloc.h>
#include <sfio.h>
#include <sfstr.h>
#include <ctype.h>
#include <dt.h>
#include <error.h>
#include <math.h>
#include "rbuf.h"
#include "libpadsc-private.h"

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
 *   copy_strings : if non-zero, the string read functions copy the strings found, otherwise they do not
 *                  (instead the target PDC_string points to memory managed by the current IO discipline).
 *                  copy_strings should only be set to zero for record-based IO disciplines where
 *                  strings from record K are not used after PDC_IO_next_rec has been called to move
 *                  the IO cursor to record K+1.  Note: PDC_string_preserve can be used to
 *                  force a string that is using sharing to make a copy so that the string is 'preserved'
 *                  (remains valid) across calls to PDC_IO_next_rec.
 *
 *   d_endian  : data endian-ness    (PDC_bigEndian or PDC_littleEndian)
 *                 If d_endian != the endian-ness of the machine running the parsing code,
 *                 byte order of binary integers is swapped 
 *                 by the binary integer read functions.  See comments below about
 *                 the CHECK_ENDIAN pragma.
 *
 *   io_disc  : This field contains a pointer to a sub-discipline obj of type
 *              PDC_IO_dist_t which is used to enable reading different kinds
 *              of data files.  See pdc_io_disc.h for details.
 *              Also see 'Changing The IO Discipline' below.
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
 *    copy_strings: 0
 *    stop_regexp:  0
 *    stop_maxlen:  0
 *    errorf:       PDC_errorf
 *    e_rep:        PDC_errorRep_Max
 *    d_endian:     PDC_littleEndian
 *    io_disc:      NULL -- a default IO discipline (newline-terminated records)
 *                  is installed on PDC_open if one is not installed beforehand
 *
 *
 * Initializing a PDC handle
 * -------------------------
 *   XXX_TODOC
 *
 * Here is an example initialization that modifies the constructs a discipline
 * object, my_disc, and allocates an instance of the 'norec' IO discpline
 * to be the IO discipline:
 *
 *     PDC_t *pdc;
 *     PDC_IO_disc_t* norec;
 *     PDC_disc_t my_disc = PDC_default_disc;
 *     my_disc.flags |= (PDC_flags_t)PDC_WSPACE_OK;
 *     norec = PDC_norec_make(0);
 *     if (PDC_ERR == PDC_open(&pdc, &my_disc, norec)) {
 *       fprintf(stderr, "Failed to open PDC library handle\n");
 *       exit(-1);
 *     }
 *     -- start using pdc
 *
 * If we are willing to use the default IO discipline we could have used:
 *        
 *     PDC_t *pdc;
 *     PDC_disc_t my_disc = PDC_default_disc;
 *     my_disc.flags |= (PDC_flags_t)PDC_WSPACE_OK;
 *     if (PDC_ERR == PDC_open(&pdc, &my_disc, 0)) {
 *       fprintf(stderr, "Failed to open PDC library handle\n");
 *       exit(-1);
 *     }
 *     -- start using pdc
 *
 * Similarly, if we do not need to modify the default discipline:
 *
 *     PDC_t *pdc;
 *     if (PDC_ERR == PDC_open(&pdc, 0, 0)) {
 *       fprintf(stderr, "Failed to open PDC library handle\n");
 *       exit(-1);
 *     }
 *     -- start using pdc
 *
 * Changing The Main Discipline
 * -----------------------------
 *   XXX_TODOC
 *     PDC_disc_t my_disc = PDC_default_disc;
 *     my_disc.flags |= (PDC_flags_t)PDC_WSPACE_OK;
 *     PDC_set_disc(pdc, &my_disc, 1);
 *
 * The third arg value of 1 indicates that the IO discipline
 * installed in the old main discipline should be moved to
 * be installed instead in the new main discipline.
 *
 * Changing The IO Discipline
 * --------------------------
 *   XXX_TODOC
 * For example, suppose in the middle of parsing we need to change
 * to a version of the fixed-width IO discipline for records that have
 * 0 leader bytes, 30 data byte records, and  2 trailer bytes:
 *
 *       PDC_IO_disc_t* fwrec;
 *       ..
 *       fwrec = PDC_fwrec_make(0, 30, 2);
 *       PDC_set_IO_disc(pdc, fwrec, 1);
 *
 *  The third arg value of 1 indicates the current sfio stream
 *  should be transferred to the new IO discipline.  If this is not done,
 *  XXX_TODOC.
 */

/* ================================================================================
 * CONSTANTS
 */

#define PDC_VERSION                  20020815L

/* flags are unsigned long values */
typedef unsigned long          PDC_flags_t;
#define PDC_NULL_CTL_FLAG      0UL
#define PDC_WSPACE_OK          1UL

typedef enum PDC_error_t_e {
  PDC_OK                            =    0,
  PDC_ERR                           =   -1
} PDC_error_t;

typedef enum PDC_errCode_t_e {
  PDC_NO_ERR                        =    0,

  PDC_UNEXPECTED_ERR                =    1,

  PDC_BAD_PARAM                     =    2,
  PDC_SYS_ERR                       =    3,
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
  PDC_STRUCT_EXTRA_BEFORE_SEP       =  121,
  PDC_UNION_MATCH_ERR               =  130,
  PDC_ENUM_MATCH_ERR                =  140,
  PDC_TYPEDEF_CONSTRAINT_ERR        =  150,

  PDC_AT_EOF                        =  160,
  PDC_AT_EOR                        =  161,
  PDC_EXTRA_BEFORE_EOR              =  162,
  PDC_EOF_BEFORE_EOR                =  163,
  PDC_RANGE                         =  170,

  PDC_INVALID_A_NUM                 =  180,
  PDC_INVALID_E_NUM                 =  181,
  PDC_INVALID_EBC_NUM               =  182,
  PDC_INVALID_BCD_NUM               =  183,

  PDC_CHAR_LIT_NOT_FOUND            =  200,
  PDC_STR_LIT_NOT_FOUND             =  210,
  PDC_REGEXP_NOT_FOUND              =  220,
  PDC_INVALID_REGEXP                =  230,
  PDC_WIDTH_NOT_AVAILABLE           =  240,
  PDC_INVALID_DATE                  =  250
} PDC_errCode_t;

/* ================================================================================
 * INTERFACE LIBRARY TYPES: FORWARD DECLS
 *
 *
 * The struct and enum decls for these types are in this file:
 *     PDC_t*        : runtime library handle (opaque)
 *                      initialized with PDC_open, passed as first arg to most library routines
 *     PDC_disc_t*   : handle to discipline
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
 *     PDC_IO_elt_t  : element of a linked list managed by the IO discipline 
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

/* ================================================================================
 * BASIC LIBRARY TYPES
 */

typedef unsigned char          PDC_byte;

typedef signed _ast_int1_t     PDC_int8;
typedef signed _ast_int2_t     PDC_int16;
typedef signed _ast_int4_t     PDC_int32; 
typedef signed _ast_int8_t     PDC_int64; 

typedef unsigned _ast_int1_t   PDC_uint8;
typedef unsigned _ast_int2_t   PDC_uint16;
typedef unsigned _ast_int4_t   PDC_uint32;
typedef unsigned _ast_int8_t   PDC_uint64;

typedef	struct { PDC_int8   num; PDC_uint8  denom;} PDC_fpoint8;
typedef	struct { PDC_int16  num; PDC_uint16 denom;} PDC_fpoint16;
typedef	struct { PDC_int32  num; PDC_uint32 denom;} PDC_fpoint32;
typedef	struct { PDC_int64  num; PDC_uint64 denom;} PDC_fpoint64;

typedef	struct { PDC_uint8  num; PDC_uint8  denom;} PDC_ufpoint8;
typedef	struct { PDC_uint16 num; PDC_uint16 denom;} PDC_ufpoint16;
typedef	struct { PDC_uint32 num; PDC_uint32 denom;} PDC_ufpoint32;
typedef	struct { PDC_uint64 num; PDC_uint64 denom;} PDC_ufpoint64;

/* HELPERS: 
 *    PDC_FPOINT2FLT calculates num/denom as a float
 *    PDC_FPOINT2DBL calculates num/denom as a double
 */
#define PDC_FPOINT2FLT(fp) ((fp).num/(float)(fp).denom)
#define PDC_FPOINT2DBL(fp) ((fp).num/(double)(fp).denom)

/* ================================================================================
 * PDC_string: PADS strings have a ptr and length;
 *             required since they need not be null-terminated.
 *             They also have some private state, which should
 *             be ignored by users of the library.
 */

typedef struct PDC_string_s PDC_string;

/* type PDC_string: */
struct PDC_string_s {
  char             *str;
  size_t           len;
  PDC_STRING_PRIVATE_STATE;
};

/* ================================================================================
 * USEFUL CONSTANTS
 */

#define PDC_MIN_INT8                         -128
#define PDC_MAX_INT8                          127
#define PDC_MAX_UINT8                         255U

#define PDC_MIN_INT16                      -32768
#define PDC_MAX_INT16                       32767
#define PDC_MAX_UINT16                      65535U

#define PDC_MIN_INT24                    -8388608
#define PDC_MAX_INT24                     8388607
#define PDC_MAX_UINT24                   16777215U

#define PDC_MIN_INT32                 -2147483647L   /* should end in 8 but gcc does not like that */
#define PDC_MAX_INT32                  2147483647L
#define PDC_MAX_UINT32                 4294967295UL

#define PDC_MIN_INT40               -549755813888LL
#define PDC_MAX_INT40                549755813887LL
#define PDC_MAX_UINT40              1099511627775ULL

#define PDC_MIN_INT48            -140737488355328LL
#define PDC_MAX_INT48             140737488355327LL
#define PDC_MAX_UINT48            281474976710655ULL

#define PDC_MIN_INT56          -36028797018963968LL
#define PDC_MAX_INT56           36028797018963967LL
#define PDC_MAX_UINT56          72057594037927935ULL

#define PDC_MIN_INT64        -9223372036854775807LL  /* should end in 8 but gcc does not like that */
#define PDC_MAX_INT64         9223372036854775807LL
#define PDC_MAX_UINT64       18446744073709551615ULL

/* USEFUL ASCII AND EBCDIC CHAR CONSTANTS */

#define PDC_ASCII_NEWLINE '\n'
#define PDC_EBCDIC_NEWLINE 0x25
/* N.B. EBCDIC 0x15 is used on some systems for LF, 0x25 on others */

#define PDC_ASCII_SPACE ' '
#define PDC_EBCDIC_SPACE 0x40

#define PDC_ASCII_PLUS '+'
#define PDC_EBCDIC_PLUS 0x4e

#define PDC_ASCII_MINUS '-'
#define PDC_EBCDIC_MINUS 0x60

/* ================================================================================
 * DISC FUNCTION FOR ERROR REPORTING
 *
 * Prototypes:
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

/* ================================================================================
 * LIBRARY TYPES
 */

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

/* HELPER: PDC_POS_EQ tests whether pos1 is the same IO position as pos2 */
#define PDC_POS_EQ(pos1, pos2) ((pos1).num == (pos2).num && (pos1).byte == (pos2).byte)

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
  int                   copy_strings; /* if non-zero, a_string read functions copy the strings found, otherwise not */
  PDC_regexp_t          *stop_regexp; /* scan stop pattern, use 0 to disable */
  size_t                stop_maxlen;  /* max scan distance, use 0 to disable */
  PDC_error_f           errorf;       /* error function using  ... */
  PDC_errorRep          e_rep;        /* controls error reporting */
  PDC_endian            d_endian;     /* endian-ness of the data */ 
  PDC_IO_disc_t         *io_disc;     /* sub-discipline for controlling IO */
};

extern PDC_disc_t PDC_default_disc;

/* PARTIAL descriptionof type PDC_t:
 * It is OK to get the id and disc from a PDC_t* handle,
 * but other elements of the struct should only manipulated
 * by the internal library routines.
 *
 */

PDC_PRIVATE_DECLS;

struct PDC_s {
  const char        *id;       /* interface id */
  PDC_disc_t        *disc;     /* discipline handle */
  PDC_PRIVATE_STATE;
};

/* ================================================================================
 * LIBRARY HANDLE OPEN/CLOSE FUNCTIONS
 */

PDC_error_t  PDC_open          (PDC_t **pdc_out, PDC_disc_t *disc, PDC_IO_disc_t *io_disc);
PDC_error_t  PDC_close         (PDC_t *pdc); 

/* ================================================================================
 * TOP-LEVEL GET/SET FUNCTIONS
 *
 * PDC_get_disc    : returns NULL on error, otherwise returns pointer to
 *                   the installed discipline
 *
 * PDC_set_disc    : install a different discipline handle.  If param xfer_io
 *                   is non-zero, then the IO discipline from the old handle is
 *                   moved to the new handle.  In other words, the call
 *                      PDC_set_disc(pdc, new_handle, 1)
 *                   is equivalent to
 *                      old_handle = PDC_get_disc(pdc);
 *                      new_handle->io_disc = old_handle->io_disc;
 *                      old_handle->io_disc = 0;
 *                      PDC_set_disc(pdc, new_handle, 0);
 *
 * PDC_set_IO_disc : install a different IO discipline into the
 *                   main discipline.  if there is an open sfio stream,
 *                   it is transferred to the
 *                   new IO discipline after closing the old IO
 *                   discipline in a way that returns
 *                   all bytes beyond the current IO cursor to 
 *                   the stream.  The old IO discipline is unmade.
 */

PDC_disc_t * PDC_get_disc   (PDC_t *pdc);
PDC_error_t  PDC_set_disc   (PDC_t *pdc, PDC_disc_t *new_disc, int xfer_io);
PDC_error_t  PDC_set_IO_disc(PDC_t* pdc, PDC_IO_disc_t* new_io_disc);

/* PDC_rmm_zero    : get rbuf memory manager that zeroes allocated memory
 * PDC_rmm_nozero  : get rbuf memory manager that does not zero allocated memory
 *
 * See rbuf.h for the RMM/Rbuf memory management API
 */

RMM_t * PDC_rmm_zero  (PDC_t *pdc);
RMM_t * PDC_rmm_nozero(PDC_t *pdc);

/* ================================================================================
 * TOP-LEVEL IO FUNCTIONS
 * 
 * PDC_IO_set      : Initialize or change the current sfio stream used for input.
 *
 * PDC_IO_fopen    : Open a file for reading (a higher-level alternative to io_set).
 *                   Uses disc->fopen_fn, if present, otherwise default PDC_fopen.
 *                   Returns PDC_OK on success, PDC_ERR on error
 *
 * PDC_IO_close    : Close the current sfio stream.
 *                   Uses disc->fclose_fn, if present, otherwise default PDC_fclose.
 *                   Returns PDC_OK on success, PDC_ERR on error.
 *
 * PDC_IO_next_rec : Advances current IO position to start of the next record, if any.
 *                   Returns PDC_OK on success, PDC_ERR on failure 
 *                   (failure includes hitting EOF before EOR).
 *                   For PDC_OK case, sets (*skipped_bytes_out) to the number of
 *                   data bytes that were passed over while searching for EOR.
 *
 * PDC_IO_at_EOR   : Returns 1 if the current IO position is at EOR, otherwise 0.
 *
 * PDC_IO_at_EOF   : Returns 1 if current IO position is at EOF, otherwise 0.
 *
 * PDC_IO_getPos   : Fill in (*pos) with IO position.
 * PDC_IO_getLocB  : Fill in loc->b with IO position.
 * PDC_IO_getLocE  : Fill in loc->e with IO position.
 * PDC_IO_getLoc   : Fill in both loc->b and loc->e with IO position.
 *
 *   All of these above take an offset.  If offset is 0, the current IO position is
 *   used, otherwise the position used is K bytes from the current IO position
 *   (offset == K ... offset is an int, and can be positive or negative).
 *   Note the current IO position does not change.  PDC_ERR is returned if
 *   info about the specified position cannot be determined. 
 *   EOR marker bytes (if any) are ignored when moving forward or back
 *   based on offset -- offset only refers to data bytes.
 */

PDC_error_t  PDC_IO_set      (PDC_t *pdc, Sfio_t *io);
PDC_error_t  PDC_IO_fopen    (PDC_t *pdc, char *path);
PDC_error_t  PDC_IO_close    (PDC_t *pdc);
PDC_error_t  PDC_IO_next_rec (PDC_t *pdc, size_t *skipped_bytes_out);

int          PDC_IO_at_EOR   (PDC_t *pdc);
int          PDC_IO_at_EOF   (PDC_t *pdc);

PDC_error_t  PDC_IO_getPos   (PDC_t *pdc, PDC_pos_t *pos, int offset); 
PDC_error_t  PDC_IO_getLocB  (PDC_t *pdc, PDC_loc_t *loc, int offset); 
PDC_error_t  PDC_IO_getLocE  (PDC_t *pdc, PDC_loc_t *loc, int offset); 
PDC_error_t  PDC_IO_getLoc   (PDC_t *pdc, PDC_loc_t *loc, int offset); 

/* ================================================================================
 * LITERAL READ FUNCTIONS
 *
 * PDC_a_char_lit_read / a_str_lit_read:
 *
 * EFFECT: verify IO cursor points to specified char/string, move IO cursor just beyond
 *   N.B. The error mask has the following meaning for these two functions.  If *em is:
 *
 *        PDC_CheckAndSet : IO cursor only advanced if literal matches,
 *                          warning message is issued on error
 *        PDC_Check       : like PDC_CheckAndSet, but no warning on error
 *        PDC_Ignore      : IO cursor is advanced by the length
 *                          (1 char / length of string) of the literal
 *                          without checking for a match
 *
 * RETURNS: PDC_error_t
 *            OK    => IO cursor now points just beyond char / string
 *            ERROR => IO cursor did not point to char/string; unchanged
 *               (errCode PDC_CHAR_LIT_NOT_FOUND / PDC_STR_LIT_NOT_FOUND)
 */

PDC_error_t PDC_a_char_lit_read(PDC_t *pdc, PDC_base_em *em,
				PDC_base_ed *ed, PDC_byte c);

PDC_error_t PDC_a_str_lit_read(PDC_t *pdc, PDC_base_em *em,
			       PDC_base_ed *ed, const PDC_string *s);

/* PDC_countX : count occurrences of char x between the
 * current IO cursor and the first EOR or EOF.  If param
 * eor_required is non-zero, then encountering EOF
 * before EOR produces an error.
 * Does not modify the IO cursor position.  Cases:
 *   1. IO cursor is already at EOF and eor_required is non-zero
 *     => If !em || *em < PDC_Ignore:
 *           + ed->errCode set to PDC_AT_EOF
 *           + ed->loc begin/end set to EOF 'location'
 *     PDC_ERR returned   
 *   2. EOF is encountered before EOR and eor_required is non-zero
 *     => If !em || *em < PDC_Ignore:
 *           + ed->errCode set to PDC_EOF_BEFORE_EOR
 *           + ed->loc begin/end set to current IO cursor location
 *     PDC_ERR returned   
 *   3. EOR is encountered, or EOF is encounterd and eor_required is zero
 *     if res_out, *res_out is set to the number of occurrences of x
 *     from the IO cursor to EOR/EOF.
 *     PDC_OK returned
 *
 * PDC_countXtoY: count occurrences of char x between the
 * current IO cursor and the first occurrence of char y.
 * Does not modify the IO cursor position.  Cases:
 *   1. IO cursor is already at EOF
 *     => If !em || *em < PDC_Ignore:
 *           + ed->errCode set to PDC_AT_EOF
 *           + ed->loc begin/end set to EOF 'location'
 *     PDC_ERR returned   
 *   2. y is not found
 *     => If !em || *em < PDC_Ignore:
 *           + ed->errCode set to PDC_CHAR_LIT_NOT_FOUND
 *           + ed->loc begin/end set to current IO cursor location
 *     PDC_ERR returned   
 *   3. Char y is found
 *     if res_out, *res_out is set to the number of occurrences of x
 *     from the IO cursor to first y.
 *     PDC_OK returned
 *
 */

PDC_error_t PDC_countX(PDC_t *pdc, PDC_base_em *em, PDC_uint8 x, int eor_required,
		       PDC_base_ed *ed, PDC_int32 *res_out);

PDC_error_t PDC_countXtoY(PDC_t *pdc, PDC_base_em *em, PDC_uint8 x, PDC_uint8 y,
		          PDC_base_ed *ed, PDC_int32 *res_out);

/* ================================================================================
 * DATE/TIME READ FUNCTIONS
 *
 * PDC_a_date_read : attempts to read an ascii date string and convert it to
 *                   seconds since the epoch.  For the different formats supported,
 *                   see the libast tmdate documentation.
 *                   a_date_read takes the same stop character arg as a_string_read
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

PDC_error_t PDC_a_date_read(PDC_t *pdc, PDC_base_em *em, PDC_byte stopChar,
			    PDC_base_ed *ed, PDC_uint32 *res_out);

/* ================================================================================
 * STRING HELPER FUNCTIONS
 *
 *    PDC_string_init     : initialize to valid empty string (no dynamic memory allocated yet)
 *    PDC_string_cleanup  : free up the rbuf and any allocated space for the string
 *    PDC_string_mk_share : makes the PDC_string targ refer to the string specified by src/len,
 *                           sharing the space with the original owner.
 *    PDC_string_mk_copy  : copy len chars from string src into the PDC_string targ;
 *                           allocates RBuf and/or space for the copy, as necessary.
 *                           Although not strictly necessary, null-terminates targ->str.
 *                           string_mk_copy returns PDC_ERR on bad arguments or on failure to
 *                           alloc space, otherwise it returns PDC_OK
 *    PDC_string_preserve : If the string is using space-sharing, force it use a private copy 
 *                          instead, so that the (formerly) shared space can be discarded.
 *                          It is safe to call preserve on any PDC_string.
 *    PDC_string_copy     : Copy src PDC_string into targ PDC_string; sharing is not used.
 */

PDC_error_t PDC_string_init(PDC_t *pdc, PDC_string *s);
PDC_error_t PDC_string_cleanup(PDC_t *pdc, PDC_string *s);
PDC_error_t PDC_string_mk_share(PDC_t *pdc, PDC_string *targ, const char *src, size_t len);
PDC_error_t PDC_string_mk_copy(PDC_t *pdc, PDC_string *targ, const char *src, size_t len);
PDC_error_t PDC_string_preserve(PDC_t *pdc, PDC_string *s);
PDC_error_t PDC_string_copy(PDC_t *pdc, PDC_string *targ, const PDC_string *src);

/*
 * Type T with T_init/T_cleanup also must have T_ed_init/T_ed_cleanup.
 * For PDC_string_ed, these turn out to just be no-ops
 *    PDC_string_ed_init    : a no-op
 *    PDC_string_ed_cleanup : a no-op  
 */

PDC_error_t PDC_string_ed_init(PDC_t *pdc, PDC_base_ed *ed);
PDC_error_t PDC_string_ed_cleanup(PDC_t *pdc, PDC_base_ed *ed);

/* ================================================================================
 * ASCII STRING READ FUNCTIONS
 *
 * The string read functions each has a different way of specifying
 * the extent of the string:
 *   + a_string_FW_read specifies a fixed width
 *   + a_string_read specifies a single stop character
 *       (can be 0 to specify eof as the stop character)
 *   + a_string_ME and a_string_CME specify a Match Extpression
 *       (string includes all chars that match)
 *   + a_string_SE_read and a_string_CSE specify a Stop Expression
 *       (string terminated by encountering 'stop chars' that match)
 *
 * ME/SE take a string containing a regular expression, while CME/CSE take a
 * compiled form of regular expression (see PDC_regexp_compile).
 *
 * For all stop cases, the stop char/chars are not included in the
 * resulting string.  Note that if the IO cursor is already at a stop
 * condition, then a string of length zero results.
 *
 * If an expected stop char/pattern/width is found, PDC_OK is returned.
 * If !em || *em == PDC_CheckAndSet, then:
 *   + if s_out is non-null, PDC_string_set is used with (*s_out) to copy/set
 *     the string that is found, where pdc->disc->copy_strings controlls the copy arg.
 *     *s_out should have been initialized
 *     at some point prior using PDC_string_init (it can be initialized once
 *     and re-used in string read calls many times).  If pdc->disc->copy_strings is
 *     non-zero, the memory allocated by *s_out
 *     should ultimately be freed using PDC_string_cleanup.
 *   + if l_out is non-null, *l_out is set to the length of the string
 *     (not including the null terminator).
 * 
 * If an expected stop condition is not encountered, the
 * IO cursor position is unchanged.  Error codes used:
 *     PDC_WIDTH_NOT_AVAILABLE
 *     PDC_STOPCHAR_NOT_FOUND
 *     PDC_STOPREGEXP_NOT_FOUND
 *     PDC_INVALID_REGEXP
 */

PDC_error_t PDC_a_string_read(PDC_t *pdc, PDC_base_em *em, PDC_byte stopChar,
			      PDC_base_ed *ed, PDC_string *s_out);

PDC_error_t PDC_a_string_FW_read(PDC_t *pdc, PDC_base_em *em, size_t width,
				 PDC_base_ed *ed, PDC_string *s_out);

PDC_error_t PDC_a_string_ME_read(PDC_t *pdc, PDC_base_em *em, const char *matchRegexp,
				 PDC_base_ed *ed, PDC_string *s_out);

PDC_error_t PDC_a_string_CME_read(PDC_t *pdc, PDC_base_em *em, PDC_regexp_t *matchRegexp,
				  PDC_base_ed *ed, PDC_string *s_out);

PDC_error_t PDC_a_string_SE_read(PDC_t *pdc, PDC_base_em *em, const char *stopRegexp,
				 PDC_base_ed *ed, PDC_string *s_out);

PDC_error_t PDC_a_string_CSE_read(PDC_t *pdc, PDC_base_em *em, PDC_regexp_t *stopRegexp,
				  PDC_base_ed *ed, PDC_string *s_out);

/* ================================================================================
 * EBCDIC STRING READ FUNCTIONS
 *
 * These functions behave exactly like the corresponding ASCII string read
 * functions, except the data being read is EBCDIC STRING encoded.
 *
 * N.B. ** Two important things to remember:
 *       1. The EBCDIC string data is converted to ASCII, thus the resulting
 *          string is an ASCII string.
 *       2. The stop char (stop regular expression) are specified
 *          using an ASCII character (ASCII string).
 *
 * Example: passing '|' (vertical bar, which is code 124 in ASCII) as the stop
 *          char will result in a search for the EBCDIC encoding
 *          of vertical bar (code 79 in EBCDIC), and all EBCDIC chars
 *          between the IO cursor and the EBCDIC vertical bar will
 *          be converted to ASCII chars. 
 *
 * ** N.B. If pdc->disc->copy_strings is zero, then sharing is used and the
 *         string is modified 'in place' (replacing EBCDIC chars with ASCII chars).
 *         If copy_strings is non-zero, the original EBCDIC chars are unmodified,
 *         while the string copy will contain ASCII chars.  This distinction only
 *         matters if the special output version of PDC_IO_commit is used.
 */

PDC_error_t PDC_e_string_read(PDC_t *pdc, PDC_base_em *em, PDC_byte stopChar,
			      PDC_base_ed *ed, PDC_string *s_out);

PDC_error_t PDC_e_string_FW_read(PDC_t *pdc, PDC_base_em *em, size_t width,
				 PDC_base_ed *ed, PDC_string *s_out);

PDC_error_t PDC_e_string_ME_read(PDC_t *pdc, PDC_base_em *em, const char *matchRegexp,
				 PDC_base_ed *ed, PDC_string *s_out);

PDC_error_t PDC_e_string_CME_read(PDC_t *pdc, PDC_base_em *em, PDC_regexp_t *matchRegexp,
				  PDC_base_ed *ed, PDC_string *s_out);

PDC_error_t PDC_e_string_SE_read(PDC_t *pdc, PDC_base_em *em, const char *stopRegexp,
				 PDC_base_ed *ed, PDC_string *s_out);

PDC_error_t PDC_e_string_CSE_read(PDC_t *pdc, PDC_base_em *em, PDC_regexp_t *stopRegexp,
				  PDC_base_ed *ed, PDC_string *s_out);

PDC_error_t PDC_e_date_read(PDC_t *pdc, PDC_base_em *em, PDC_byte stopChar,
			    PDC_base_ed *ed, PDC_uint32 *res_out);

/* ================================================================================
 * ASCII STRING TO INTEGER READ FUNCTIONS
 *
 * Documentation for variable-width ascii string to integer read functions:
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
 * (2a) There is leading white space and not (disc flags & PDC_WSPACE_OK)
 * (2b) The target is unsigned and the first char is a -
 * (2c) The first character is not a +, -, or in [0-9]
 * (2d) First character is allowable + or -, following by a char that is not a digit
 * For the above 4 cases:
 *     => IO cursor is not advanced
 *     => If !em || *em < PDC_Ignore:
 *          + ed->errCode set to PDC_INVALID_A_NUM
 *          + ed->loc begin/end set to the IO cursor position.
 * (3) A valid ascii integer string is found, but it describes
 *     an integer that does not fit in the specified target type
 *     => IO cursor is advanced just beyond the last digit
 *     => If !em || *em < PDC_Ignore:
 *          + ed->errCode set to PDC_RANGE
 *          + ed->loc begin/end set to elt/char position of start and end of the ascii integer
 */

PDC_error_t PDC_a_int8_read (PDC_t *pdc, PDC_base_em *em,
			     PDC_base_ed *ed, PDC_int8 *res_out);

PDC_error_t PDC_a_int16_read(PDC_t *pdc, PDC_base_em *em,
			     PDC_base_ed *ed, PDC_int16 *res_out);

PDC_error_t PDC_a_int32_read(PDC_t *pdc, PDC_base_em *em,
			     PDC_base_ed *ed, PDC_int32 *res_out);

PDC_error_t PDC_a_int64_read(PDC_t *pdc, PDC_base_em *em,
			     PDC_base_ed *ed, PDC_int64 *res_out);


PDC_error_t PDC_a_uint8_read (PDC_t *pdc, PDC_base_em *em,
			      PDC_base_ed *ed, PDC_uint8 *res_out);

PDC_error_t PDC_a_uint16_read(PDC_t *pdc, PDC_base_em *em,
			      PDC_base_ed *ed, PDC_uint16 *res_out);

PDC_error_t PDC_a_uint32_read(PDC_t *pdc, PDC_base_em *em,
			      PDC_base_ed *ed, PDC_uint32 *res_out);

PDC_error_t PDC_a_uint64_read(PDC_t *pdc, PDC_base_em *em,
			      PDC_base_ed *ed, PDC_uint64 *res_out);

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

PDC_error_t PDC_a_int8_FW_read (PDC_t *pdc, PDC_base_em *em, size_t width,
				PDC_base_ed *ed, PDC_int8 *res_out);

PDC_error_t PDC_a_int16_FW_read(PDC_t *pdc, PDC_base_em *em, size_t width,
				PDC_base_ed *ed, PDC_int16 *res_out);

PDC_error_t PDC_a_int32_FW_read(PDC_t *pdc, PDC_base_em *em, size_t width,
				PDC_base_ed *ed, PDC_int32 *res_out);

PDC_error_t PDC_a_int64_FW_read(PDC_t *pdc, PDC_base_em *em, size_t width,
				PDC_base_ed *ed, PDC_int64 *res_out);


PDC_error_t PDC_a_uint8_FW_read (PDC_t *pdc, PDC_base_em *em, size_t width,
				 PDC_base_ed *ed, PDC_uint8 *res_out);

PDC_error_t PDC_a_uint16_FW_read(PDC_t *pdc, PDC_base_em *em, size_t width,
				 PDC_base_ed *ed, PDC_uint16 *res_out);

PDC_error_t PDC_a_uint32_FW_read(PDC_t *pdc, PDC_base_em *em, size_t width,
				 PDC_base_ed *ed, PDC_uint32 *res_out);

PDC_error_t PDC_a_uint64_FW_read(PDC_t *pdc, PDC_base_em *em, size_t width,
				 PDC_base_ed *ed, PDC_uint64 *res_out);

/* ================================================================================
 * EBCDIC STRING TO INTEGER READ FUNCTIONS
 *
 * These functions are just like their ASCII counterparts; the only
 * difference is the integers are encoding using EBCDIC string data.
 * The error codes used are also the same,
 * except that error code PDC_INVALID_E_NUM is used rather 
 * than PDC_INVALID_A_NUM
 */

PDC_error_t PDC_e_int8_read (PDC_t *pdc, PDC_base_em *em,
			     PDC_base_ed *ed, PDC_int8 *res_out);

PDC_error_t PDC_e_int16_read(PDC_t *pdc, PDC_base_em *em,
			     PDC_base_ed *ed, PDC_int16 *res_out);

PDC_error_t PDC_e_int32_read(PDC_t *pdc, PDC_base_em *em,
			     PDC_base_ed *ed, PDC_int32 *res_out);

PDC_error_t PDC_e_int64_read(PDC_t *pdc, PDC_base_em *em,
			     PDC_base_ed *ed, PDC_int64 *res_out);

PDC_error_t PDC_e_uint8_read (PDC_t *pdc, PDC_base_em *em,
			      PDC_base_ed *ed, PDC_uint8 *res_out);

PDC_error_t PDC_e_uint16_read(PDC_t *pdc, PDC_base_em *em,
			      PDC_base_ed *ed, PDC_uint16 *res_out);

PDC_error_t PDC_e_uint32_read(PDC_t *pdc, PDC_base_em *em,
			      PDC_base_ed *ed, PDC_uint32 *res_out);

PDC_error_t PDC_e_uint64_read(PDC_t *pdc, PDC_base_em *em,
			      PDC_base_ed *ed, PDC_uint64 *res_out);

PDC_error_t PDC_e_int8_FW_read (PDC_t *pdc, PDC_base_em *em, size_t width,
				PDC_base_ed *ed, PDC_int8 *res_out);

PDC_error_t PDC_e_int16_FW_read(PDC_t *pdc, PDC_base_em *em, size_t width,
				PDC_base_ed *ed, PDC_int16 *res_out);

PDC_error_t PDC_e_int32_FW_read(PDC_t *pdc, PDC_base_em *em, size_t width,
				PDC_base_ed *ed, PDC_int32 *res_out);

PDC_error_t PDC_e_int64_FW_read(PDC_t *pdc, PDC_base_em *em, size_t width,
				PDC_base_ed *ed, PDC_int64 *res_out);

PDC_error_t PDC_e_uint8_FW_read (PDC_t *pdc, PDC_base_em *em, size_t width,
				 PDC_base_ed *ed, PDC_uint8 *res_out);

PDC_error_t PDC_e_uint16_FW_read(PDC_t *pdc, PDC_base_em *em, size_t width,
				 PDC_base_ed *ed, PDC_uint16 *res_out);

PDC_error_t PDC_e_uint32_FW_read(PDC_t *pdc, PDC_base_em *em, size_t width,
				 PDC_base_ed *ed, PDC_uint32 *res_out);

PDC_error_t PDC_e_uint64_FW_read(PDC_t *pdc, PDC_base_em *em, size_t width,
				 PDC_base_ed *ed, PDC_uint64 *res_out);

/* ================================================================================
 * COMMON WIDTH BINARY INTEGER READ FUNCTIONS
 *
 * These functions parse signed or unsigned binary integers
 * of common bit widths (8, 16, 32, and 64 bit widths).
 * Whether bytes are reversed is controlled by the endian-ness of
 * the machine (determined automatically) and disc->d_endian. If they differ,
 * byte order is reversed in the in-memory representation, otherwise it is not.
 *
 * A good way to set the d_endian value in a machine-independent way is to
 * use PRAGMA CHECK_ENDIAN with the first multi-byte binary integer field that appears
 * in the data.  For example, this header definition:
 *
 *
 * pstruct header {
 *    b_uint16 version : version < 10; //- PRAGMA CHECK_ENDIAN
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

PDC_error_t PDC_b_int8_read (PDC_t *pdc, PDC_base_em *em,
			     PDC_base_ed *ed, PDC_int8 *res_out);

PDC_error_t PDC_b_int16_read(PDC_t *pdc, PDC_base_em *em,
			     PDC_base_ed *ed, PDC_int16 *res_out);

PDC_error_t PDC_b_int32_read(PDC_t *pdc, PDC_base_em *em,
			     PDC_base_ed *ed, PDC_int32 *res_out);

PDC_error_t PDC_b_int64_read(PDC_t *pdc, PDC_base_em *em,
			     PDC_base_ed *ed, PDC_int64 *res_out);

PDC_error_t PDC_b_uint8_read (PDC_t *pdc, PDC_base_em *em,
			      PDC_base_ed *ed, PDC_uint8 *res_out);

PDC_error_t PDC_b_uint16_read(PDC_t *pdc, PDC_base_em *em,
			      PDC_base_ed *ed, PDC_uint16 *res_out);

PDC_error_t PDC_b_uint32_read(PDC_t *pdc, PDC_base_em *em,
			      PDC_base_ed *ed, PDC_uint32 *res_out);

PDC_error_t PDC_b_uint64_read(PDC_t *pdc, PDC_base_em *em,
			      PDC_base_ed *ed, PDC_uint64 *res_out);

/* ================================================================================
 * EBC, BCD, and SBL, and SBH ENCODINGS OF INTEGERS
 *   (VARIABLE NUMBER OF DIGITS/BYTES)
 *
 * These functions parse signed or unsigned EBCDIC numeric (ebc_), BCD (bcd_),
 * SBL (sbl_) or SBH (sbh_) encoded integers with a specified number of digits
 * (for ebc_ and bcd_) or number of bytes (for sbl_ and sbh_).
 *
 * EBC INTEGER ENCODING (PDC_ebc_int64_read / PDC_ebc_uint64_read):
 *
 *   Each byte on disk encodes one digit (in low 4 bits).  For signed
 *   values, the final byte encodes the sign (high 4 bits == 0xD for negative).
 *   A signed or unsigned 5 digit value is encoded in 5 bytes.
 *
 * BCD INTEGER ENCODING (PDC_bcd_int_read / PDC_bcd_uint_read):
 *
 *   Each byte on disk encodes two digits, 4 bits per digit.  For signed
 *   values, a negative number is encoded by having number of digits be odd
 *   so that the remaining low 4 bits in the last byte are available for the sign.
 *   (low 4 bits == 0xD for negative).
 *   A signed or unsigned 5 digit value is encoded in 3 bytes, where the unsigned
 *   value ignores the final 4 bits and the signed value uses them to get the sign.
 *
 * SBL (Serialized Binary, Low-Order Byte First) INTEGER ENCODING
 *   (PDC_sbl_int_read / PDC_sbl_uint_read):
 *
 *   For a K-byte SBL encoding, the first byte on disk is treated 
 *   as the low order byte of a K byte value.
 *
 * SBH (Serialized Binary, High-Order Byte First) INTEGER ENCODING
 *   (PDC_sbh_int_read / PDC_sbh_uint_read):
 *
 *   For a K-byte SBH encoding, the first byte on disk is treated 
 *   as the high order byte of a K byte value.
 * 
 * For SBL and SBH, each byte is moved to the in-memory target integer unchanged.
 * Whether the result is treated as a signed or unsigned number depends on the target type.
 *
 * Note that SBL and SBH differ from the COMMON WIDTH BINARY (B) read functions above
 * in 3 ways: (1) SBL and SBH support any number of bytes between 1 and 8,
 * while B only supports 1, 2, 4, and 8; (2) with SBL and SBH you specify the target
 * type independently of the num_bytes; (3) SBL and SBH explicitly state the
 * byte ordering, while B uses the disc->d_endian setting to determine the
 * byte ordering of the data.
 *
 * FOR ALL TYPES
 * =============
 *
 * The legal range of values for num_digits (for EBC/BCD) or num_bytes (for SB)
 * depends on target type:
 *    
 * Type        num_digits    num_bytes Min/Max values
 * ----------- ----------    --------- ----------------------------------------------------
 * PDC_int8    1-3           1-1       PDC_MIN_INT8  / PDC_MAX_INT8
 * PDC_uint8   1-3           1-1       0             / PDC_MAX_UINT8
 * PDC_int16   1-5           1-2       PDC_MIN_INT16 / PDC_MAX_INT16
 * PDC_uint16  1-5           1-2       0             / PDC_MAX_UINT16
 * PDC_int32   1-10/11**     1-4       PDC_MIN_INT32 / PDC_MAX_INT32
 * PDC_uint32  1-10          1-4       0             / PDC_MAX_UINT32
 * PDC_int64   1-19          1-8       PDC_MIN_INT64 / PDC_MAX_INT64
 * PDC_uint64  1-20          1-8       0             / PDC_MAX_UINT64
 * 
 * NB: num_digits must be odd if the value on disk can be negative.
 *
 * ** For PDC_bcd_int32_read only, even though the min and max int32 have 10 digits, we allow
 * num_digits == 11 due to the fact that 11 is required for a 10 digit negative value
 * (an actual 11 digit number would cause a range error, so the leading digit must be 0).
 * 
 * For all cases, if the specified number of bytes is NOT available,
 * the IO cursor is not advanced, and:
 *    if !em || *em < PDC_Ignore:
 *        + ed->errCode set to PDC_WIDTH_NOT_AVAILABLE
 *        + ed->loc begin/end set to elt/char position of start/end of the 'too small' field
 *
 * Otherwise, the IO cursor is always advanced.  There are 3 error cases that
 * can occur even though the IO cursor advances:
 *
 * If num_digits or num_bytes is not a legal choice for the target type and
 * sign of the value:
 *    if !em || *em < PDC_Ignore:
 *          + ed->errCode set to PDC_BAD_PARAM
 *
 * If the specified bytes make up an integer that does not fit in the target type,
 * or if the actual value is not in the min/max range, then:
 *    if !em || *em < PDC_Ignore:
 *          + ed->errCode set to PDC_RANGE
 *
 * If the specified bytes are not legal EBC/BCD integer bytes, then 
 *    if !em || *em < PDC_Ignore:
 *          + ed->errCode set to one of:
 *                PDC_INVALID_EBC_NUM
 *                PDC_INVALID_BCD_NUM
 */

PDC_error_t PDC_ebc_int8_read   (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_digits,
				 PDC_base_ed *ed, PDC_int8 *res_out);
PDC_error_t PDC_ebc_int16_read  (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_digits,
				 PDC_base_ed *ed, PDC_int16 *res_out);
PDC_error_t PDC_ebc_int32_read  (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_digits,
				 PDC_base_ed *ed, PDC_int32 *res_out);
PDC_error_t PDC_ebc_int64_read  (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_digits,
				 PDC_base_ed *ed, PDC_int64 *res_out);

PDC_error_t PDC_ebc_uint8_read  (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_digits,
				 PDC_base_ed *ed, PDC_uint8 *res_out);
PDC_error_t PDC_ebc_uint16_read (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_digits,
				 PDC_base_ed *ed, PDC_uint16 *res_out);
PDC_error_t PDC_ebc_uint32_read (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_digits,
				 PDC_base_ed *ed, PDC_uint32 *res_out);
PDC_error_t PDC_ebc_uint64_read (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_digits,
				 PDC_base_ed *ed, PDC_uint64 *res_out);

PDC_error_t PDC_bcd_int8_read   (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_digits,
				 PDC_base_ed *ed, PDC_int8 *res_out);
PDC_error_t PDC_bcd_int16_read  (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_digits,
				 PDC_base_ed *ed, PDC_int16 *res_out);
PDC_error_t PDC_bcd_int32_read  (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_digits,
				 PDC_base_ed *ed, PDC_int32 *res_out);
PDC_error_t PDC_bcd_int64_read  (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_digits,
				 PDC_base_ed *ed, PDC_int64 *res_out);

PDC_error_t PDC_bcd_uint8_read  (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_digits,
				 PDC_base_ed *ed, PDC_uint8 *res_out);
PDC_error_t PDC_bcd_uint16_read (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_digits,
				 PDC_base_ed *ed, PDC_uint16 *res_out);
PDC_error_t PDC_bcd_uint32_read (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_digits,
				 PDC_base_ed *ed, PDC_uint32 *res_out);
PDC_error_t PDC_bcd_uint64_read (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_digits,
				 PDC_base_ed *ed, PDC_uint64 *res_out);

PDC_error_t PDC_sbl_int8_read    (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_bytes,
			         PDC_base_ed *ed, PDC_int8 *res_out);
PDC_error_t PDC_sbl_int16_read   (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_bytes,
			         PDC_base_ed *ed, PDC_int16 *res_out);
PDC_error_t PDC_sbl_int32_read   (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_bytes,
			         PDC_base_ed *ed, PDC_int32 *res_out);
PDC_error_t PDC_sbl_int64_read   (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_bytes,
			         PDC_base_ed *ed, PDC_int64 *res_out);

PDC_error_t PDC_sbl_uint8_read   (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_bytes,
			 	 PDC_base_ed *ed, PDC_uint8 *res_out);
PDC_error_t PDC_sbl_uint16_read  (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_bytes,
			 	 PDC_base_ed *ed, PDC_uint16 *res_out);
PDC_error_t PDC_sbl_uint32_read  (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_bytes,
			 	 PDC_base_ed *ed, PDC_uint32 *res_out);
PDC_error_t PDC_sbl_uint64_read  (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_bytes,
			 	 PDC_base_ed *ed, PDC_uint64 *res_out);

PDC_error_t PDC_sbh_int8_read    (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_bytes,
			         PDC_base_ed *ed, PDC_int8 *res_out);
PDC_error_t PDC_sbh_int16_read   (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_bytes,
			         PDC_base_ed *ed, PDC_int16 *res_out);
PDC_error_t PDC_sbh_int32_read   (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_bytes,
			         PDC_base_ed *ed, PDC_int32 *res_out);
PDC_error_t PDC_sbh_int64_read   (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_bytes,
			         PDC_base_ed *ed, PDC_int64 *res_out);

PDC_error_t PDC_sbh_uint8_read   (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_bytes,
			 	 PDC_base_ed *ed, PDC_uint8 *res_out);
PDC_error_t PDC_sbh_uint16_read  (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_bytes,
			 	 PDC_base_ed *ed, PDC_uint16 *res_out);
PDC_error_t PDC_sbh_uint32_read  (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_bytes,
			 	 PDC_base_ed *ed, PDC_uint32 *res_out);
PDC_error_t PDC_sbh_uint64_read  (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_bytes,
			 	 PDC_base_ed *ed, PDC_uint64 *res_out);

/* ================================================================================
 * FIXED POINT READ FUNCTIONS
 * FOR EBC (ebc_), BCD (bcd_), SBL (sbl_), and SBH (sbh_) ENCODINGS
 *
 * An fpoint or ufpoint number is a signed or unsigned fixed-point
 * rational number with a numerator and denominator that both have the
 * same size.  For signed fpoint types, the numerator carries the sign, while
 * the denominator is always unsigned.  For example, type PDC_fpoint16
 * has a signed PDC_int16 numerator and an unsigned PDC_uint16 denominator.
 *
 * For the EBC and BCD fpoint read functions, num_digits is the
 * number of digits used to encode the numerator (on disk). The number
 * of bytes implied by num_digits is the same as specified above for the
 * EBC/BCD integer read functions.
 *
 * For the SBL and SBH fpoint read functions, num_bytes is the number of bytes on
 * disk used to encode the numerator, the encoding being the same as
 * for the SBL and SBH integer read functions, respectively.
 *
 * For all fpoint types, d_exp determines the denominator value,
 * which is implicitly 10^d_exp and is not encoded on disk.
 * The legal range of values for d_exp depends on the type:
 *
 * Type                     d_exp     Max denominator (min is 1)
 * -----------------------  --------  --------------------------
 * PDC_fpoint8  /  ufpoint8  0-2                             100
 * PDC_fpoint16 / ufpoint16  0-4                          10,000
 * PDC_fpoint32 / ufpoint32  0-9                   1,000,000,000
 * PDC_fpoint64 / ufpoint64  0-19     10,000,000,000,000,000,000
 *
 * The legal range of values for num_digits (for EBC/BCD) or num_bytes (for SBL/SBH)
 * depends on target type, and is the same as specified above for the
 * EBC/BCD/SBL/SBH integer read functions.
 *    
 * For all cases, if the specified number of bytes are NOT available,
 * the IO cursor is not advanced, and:
 *    if !em || *em < PDC_Ignore:
 *        + ed->errCode set to PDC_WIDTH_NOT_AVAILABLE
 *        + ed->loc begin/end set to elt/char position of start/end of the 'too small' field
 *
 * Otherwise, the IO cursor is always advanced.  There are 3 error cases that
 * can occur even though the IO cursor advances:
 *
 * If num_digits, num_bytes, or d_exp is not in a legal choice for the target type
 * and sign of the value:
 *    if !em || *em < PDC_Ignore:
 *          + ed->errCode set to PDC_BAD_PARAM
 *
 * If the actual numerator is not in the min/max numerator range, then:
 *    if !em || *em < PDC_Ignore:
 *          + ed->errCode set to PDC_RANGE
 *
 * If the specified bytes are not legal EBC/BCD integer bytes, then 
 *    if !em || *em < PDC_Ignore:
 *          + ed->errCode set to one of:
 *                PDC_INVALID_EBC_NUM
 *                PDC_INVALID_BCD_NUM
 *
 */

PDC_error_t PDC_ebc_fpoint8_read   (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_digits, PDC_uint32 d_exp,
				    PDC_base_ed *ed, PDC_fpoint8 *res_out);
PDC_error_t PDC_ebc_fpoint16_read  (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_digits, PDC_uint32 d_exp,
				    PDC_base_ed *ed, PDC_fpoint16 *res_out);
PDC_error_t PDC_ebc_fpoint32_read  (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_digits, PDC_uint32 d_exp,
				    PDC_base_ed *ed, PDC_fpoint32 *res_out);
PDC_error_t PDC_ebc_fpoint64_read  (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_digits, PDC_uint32 d_exp,
				    PDC_base_ed *ed, PDC_fpoint64 *res_out);

PDC_error_t PDC_ebc_ufpoint8_read  (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_digits, PDC_uint32 d_exp,
				    PDC_base_ed *ed, PDC_ufpoint8 *res_out);
PDC_error_t PDC_ebc_ufpoint16_read (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_digits, PDC_uint32 d_exp,
				    PDC_base_ed *ed, PDC_ufpoint16 *res_out);
PDC_error_t PDC_ebc_ufpoint32_read (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_digits, PDC_uint32 d_exp,
				    PDC_base_ed *ed, PDC_ufpoint32 *res_out);
PDC_error_t PDC_ebc_ufpoint64_read (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_digits, PDC_uint32 d_exp,
				    PDC_base_ed *ed, PDC_ufpoint64 *res_out);

PDC_error_t PDC_bcd_fpoint8_read   (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_digits, PDC_uint32 d_exp,
				    PDC_base_ed *ed, PDC_fpoint8 *res_out);
PDC_error_t PDC_bcd_fpoint16_read  (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_digits, PDC_uint32 d_exp,
				    PDC_base_ed *ed, PDC_fpoint16 *res_out);
PDC_error_t PDC_bcd_fpoint32_read  (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_digits, PDC_uint32 d_exp,
				    PDC_base_ed *ed, PDC_fpoint32 *res_out);
PDC_error_t PDC_bcd_fpoint64_read  (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_digits, PDC_uint32 d_exp,
				    PDC_base_ed *ed, PDC_fpoint64 *res_out);

PDC_error_t PDC_bcd_ufpoint8_read  (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_digits, PDC_uint32 d_exp,
				    PDC_base_ed *ed, PDC_ufpoint8 *res_out);
PDC_error_t PDC_bcd_ufpoint16_read (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_digits, PDC_uint32 d_exp,
				    PDC_base_ed *ed, PDC_ufpoint16 *res_out);
PDC_error_t PDC_bcd_ufpoint32_read (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_digits, PDC_uint32 d_exp,
				    PDC_base_ed *ed, PDC_ufpoint32 *res_out);
PDC_error_t PDC_bcd_ufpoint64_read (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_digits, PDC_uint32 d_exp,
				    PDC_base_ed *ed, PDC_ufpoint64 *res_out);

PDC_error_t PDC_sbl_fpoint8_read    (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_bytes, PDC_uint32 d_exp,
				    PDC_base_ed *ed, PDC_fpoint8 *res_out);
PDC_error_t PDC_sbl_fpoint16_read   (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_bytes, PDC_uint32 d_exp,
				    PDC_base_ed *ed, PDC_fpoint16 *res_out);
PDC_error_t PDC_sbl_fpoint32_read   (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_bytes, PDC_uint32 d_exp,
				    PDC_base_ed *ed, PDC_fpoint32 *res_out);
PDC_error_t PDC_sbl_fpoint64_read   (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_bytes, PDC_uint32 d_exp,
				    PDC_base_ed *ed, PDC_fpoint64 *res_out);

PDC_error_t PDC_sbl_ufpoint8_read   (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_bytes, PDC_uint32 d_exp,
				    PDC_base_ed *ed, PDC_ufpoint8 *res_out);
PDC_error_t PDC_sbl_ufpoint16_read  (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_bytes, PDC_uint32 d_exp,
				    PDC_base_ed *ed, PDC_ufpoint16 *res_out);
PDC_error_t PDC_sbl_ufpoint32_read  (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_bytes, PDC_uint32 d_exp,
				    PDC_base_ed *ed, PDC_ufpoint32 *res_out);
PDC_error_t PDC_sbl_ufpoint64_read  (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_bytes, PDC_uint32 d_exp,
				    PDC_base_ed *ed, PDC_ufpoint64 *res_out);

PDC_error_t PDC_sbh_fpoint8_read    (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_bytes, PDC_uint32 d_exp,
				    PDC_base_ed *ed, PDC_fpoint8 *res_out);
PDC_error_t PDC_sbh_fpoint16_read   (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_bytes, PDC_uint32 d_exp,
				    PDC_base_ed *ed, PDC_fpoint16 *res_out);
PDC_error_t PDC_sbh_fpoint32_read   (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_bytes, PDC_uint32 d_exp,
				    PDC_base_ed *ed, PDC_fpoint32 *res_out);
PDC_error_t PDC_sbh_fpoint64_read   (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_bytes, PDC_uint32 d_exp,
				    PDC_base_ed *ed, PDC_fpoint64 *res_out);

PDC_error_t PDC_sbh_ufpoint8_read   (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_bytes, PDC_uint32 d_exp,
				    PDC_base_ed *ed, PDC_ufpoint8 *res_out);
PDC_error_t PDC_sbh_ufpoint16_read  (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_bytes, PDC_uint32 d_exp,
				    PDC_base_ed *ed, PDC_ufpoint16 *res_out);
PDC_error_t PDC_sbh_ufpoint32_read  (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_bytes, PDC_uint32 d_exp,
				    PDC_base_ed *ed, PDC_ufpoint32 *res_out);
PDC_error_t PDC_sbh_ufpoint64_read  (PDC_t *pdc, PDC_base_em *em, PDC_uint32 num_bytes, PDC_uint32 d_exp,
				    PDC_base_ed *ed, PDC_ufpoint64 *res_out);

/* ================================================================================
 * BASE TYPE ACCUMULATORS
 *
 * For integer type T, accumulator functions PDC_T_acc_avg returns the running average
 * as a double, while PDC_T_acc_ravg returns the average as a T value by roudning the
 * double to the nearest T.
 *
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
  PDC_uint64  tracked;
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
  PDC_uint64  tracked;
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
  PDC_uint64     tracked;
  PDC_uint32_acc len_accum; /* used for length distribution and good/bad accounting */
} PDC_string_acc;

PDC_error_t PDC_int8_acc_init    (PDC_t *pdc, PDC_int8_acc *a);
PDC_error_t PDC_int8_acc_reset   (PDC_t *pdc, PDC_int8_acc *a);
PDC_error_t PDC_int8_acc_cleanup (PDC_t *pdc, PDC_int8_acc *a);
PDC_error_t PDC_int8_acc_add     (PDC_t *pdc, PDC_int8_acc *a, PDC_base_ed *ed, PDC_int8 *val);
PDC_error_t PDC_int8_acc_report  (PDC_t *pdc, const char *prefix, const char *what, int nst,
				  PDC_int8_acc *a);
double      PDC_int8_acc_avg     (PDC_t *pdc, PDC_int8_acc *a);
PDC_int8    PDC_int8_acc_ravg    (PDC_t *pdc, PDC_int8_acc *a);

PDC_error_t PDC_int16_acc_init    (PDC_t *pdc, PDC_int16_acc *a);
PDC_error_t PDC_int16_acc_reset   (PDC_t *pdc, PDC_int16_acc *a);
PDC_error_t PDC_int16_acc_cleanup (PDC_t *pdc, PDC_int16_acc *a);
PDC_error_t PDC_int16_acc_add     (PDC_t *pdc, PDC_int16_acc *a, PDC_base_ed *ed, PDC_int16 *val);
PDC_error_t PDC_int16_acc_report  (PDC_t *pdc, const char *prefix, const char *what, int nst,
				   PDC_int16_acc *a);
double      PDC_int16_acc_avg     (PDC_t *pdc, PDC_int16_acc *a);
PDC_int16   PDC_int16_acc_ravg    (PDC_t *pdc, PDC_int16_acc *a);

PDC_error_t PDC_int32_acc_init    (PDC_t *pdc, PDC_int32_acc *a);
PDC_error_t PDC_int32_acc_reset   (PDC_t *pdc, PDC_int32_acc *a);
PDC_error_t PDC_int32_acc_cleanup (PDC_t *pdc, PDC_int32_acc *a);
PDC_error_t PDC_int32_acc_add     (PDC_t *pdc, PDC_int32_acc *a, PDC_base_ed *ed, PDC_int32 *val);
PDC_error_t PDC_int32_acc_report  (PDC_t *pdc, const char *prefix, const char *what, int nst,
				   PDC_int32_acc *a);
double      PDC_int32_acc_avg     (PDC_t *pdc, PDC_int32_acc *a);
PDC_int32   PDC_int32_acc_ravg    (PDC_t *pdc, PDC_int32_acc *a);

PDC_error_t PDC_int64_acc_init    (PDC_t *pdc, PDC_int64_acc *a);
PDC_error_t PDC_int64_acc_reset   (PDC_t *pdc, PDC_int64_acc *a);
PDC_error_t PDC_int64_acc_cleanup (PDC_t *pdc, PDC_int64_acc *a);
PDC_error_t PDC_int64_acc_add     (PDC_t *pdc, PDC_int64_acc *a, PDC_base_ed *ed, PDC_int64 *val);
PDC_error_t PDC_int64_acc_report  (PDC_t *pdc, const char *prefix, const char *what, int nst,
				   PDC_int64_acc *a);
double      PDC_int64_acc_avg     (PDC_t *pdc, PDC_int64_acc *a);
PDC_int64   PDC_int64_acc_ravg    (PDC_t *pdc, PDC_int64_acc *a);

PDC_error_t PDC_uint8_acc_init    (PDC_t *pdc, PDC_uint8_acc *a);
PDC_error_t PDC_uint8_acc_reset   (PDC_t *pdc, PDC_uint8_acc *a);
PDC_error_t PDC_uint8_acc_cleanup (PDC_t *pdc, PDC_uint8_acc *a);
PDC_error_t PDC_uint8_acc_add     (PDC_t *pdc, PDC_uint8_acc *a, PDC_base_ed *ed, PDC_uint8 *val);
PDC_error_t PDC_uint8_acc_report  (PDC_t *pdc, const char *prefix, const char *what, int nst,
				   PDC_uint8_acc *a);
double      PDC_uint8_acc_avg     (PDC_t *pdc, PDC_uint8_acc *a);
PDC_uint8   PDC_uint8_acc_ravg    (PDC_t *pdc, PDC_uint8_acc *a);

PDC_error_t PDC_uint16_acc_init    (PDC_t *pdc, PDC_uint16_acc *a);
PDC_error_t PDC_uint16_acc_reset   (PDC_t *pdc, PDC_uint16_acc *a);
PDC_error_t PDC_uint16_acc_cleanup (PDC_t *pdc, PDC_uint16_acc *a);
PDC_error_t PDC_uint16_acc_add     (PDC_t *pdc, PDC_uint16_acc *a, PDC_base_ed *ed, PDC_uint16 *val);
PDC_error_t PDC_uint16_acc_report  (PDC_t *pdc, const char *prefix, const char *what, int nst,
				    PDC_uint16_acc *a);
double      PDC_uint16_acc_avg     (PDC_t *pdc, PDC_uint16_acc *a);
PDC_uint16  PDC_uint16_acc_ravg    (PDC_t *pdc, PDC_uint16_acc *a);

PDC_error_t PDC_uint32_acc_init    (PDC_t *pdc, PDC_uint32_acc *a);
PDC_error_t PDC_uint32_acc_reset   (PDC_t *pdc, PDC_uint32_acc *a);
PDC_error_t PDC_uint32_acc_cleanup (PDC_t *pdc, PDC_uint32_acc *a);
PDC_error_t PDC_uint32_acc_add     (PDC_t *pdc, PDC_uint32_acc *a, PDC_base_ed *ed, PDC_uint32 *val);
PDC_error_t PDC_uint32_acc_report  (PDC_t *pdc, const char *prefix, const char *what, int nst,
				    PDC_uint32_acc *a);
double      PDC_uint32_acc_avg     (PDC_t *pdc, PDC_uint32_acc *a);
PDC_uint32  PDC_uint32_acc_ravg    (PDC_t *pdc, PDC_uint32_acc *a);

PDC_error_t PDC_uint64_acc_init    (PDC_t *pdc, PDC_uint64_acc *a);
PDC_error_t PDC_uint64_acc_reset   (PDC_t *pdc, PDC_uint64_acc *a);
PDC_error_t PDC_uint64_acc_cleanup (PDC_t *pdc, PDC_uint64_acc *a);
PDC_error_t PDC_uint64_acc_add     (PDC_t *pdc, PDC_uint64_acc *a, PDC_base_ed *ed, PDC_uint64 *val);
PDC_error_t PDC_uint64_acc_report  (PDC_t *pdc, const char *prefix, const char *what, int nst,
				    PDC_uint64_acc *a);
double      PDC_uint64_acc_avg     (PDC_t *pdc, PDC_uint64_acc *a);
PDC_uint64  PDC_uint64_acc_ravg    (PDC_t *pdc, PDC_uint64_acc *a);

PDC_error_t PDC_string_acc_init    (PDC_t *pdc, PDC_string_acc *a);
PDC_error_t PDC_string_acc_reset   (PDC_t *pdc, PDC_string_acc *a);
PDC_error_t PDC_string_acc_cleanup (PDC_t *pdc, PDC_string_acc *a);
PDC_error_t PDC_string_acc_add     (PDC_t *pdc, PDC_string_acc *a, PDC_base_ed *ed, PDC_string* val);
PDC_error_t PDC_string_acc_report  (PDC_t *pdc, const char *prefix, const char *what, int nst,
				    PDC_string_acc *a);

/*
 * char_acc is just like uint8_acc except a different report is generated
 */
typedef PDC_uint8_acc PDC_char_acc;

PDC_error_t PDC_char_acc_init      (PDC_t *pdc, PDC_char_acc *a);
PDC_error_t PDC_char_acc_reset     (PDC_t *pdc, PDC_char_acc *a);
PDC_error_t PDC_char_acc_cleanup   (PDC_t *pdc, PDC_char_acc *a);
PDC_error_t PDC_char_acc_add       (PDC_t *pdc, PDC_char_acc *a, PDC_base_ed *ed, PDC_uint8 *val);
PDC_error_t PDC_char_acc_report    (PDC_t *pdc, const char *prefix, const char *what, int nst,
				    PDC_char_acc *a);

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
				     PDC_int32_map_fn  fn, PDC_int32_acc *a);

/*
 * fpoint/ufpoint accumulator types
 *
 *    Note that double-based arithmetic is used for the fpoint64/ufpoint64 accumulators,
 *    while float-based arithmetic is used for all other fpoint/ufpoint accumulators.
 */

typedef struct PDC_fpoint_acc_flt_s {
  Dt_t        *dict;
  PDC_uint64  good;
  PDC_uint64  bad;
  PDC_uint64  fold;
  PDC_uint64  tracked;
  double      psum;
  double      avg;
  double      min;
  double      max;
} PDC_fpoint_acc_flt;

typedef struct PDC_fpoint_acc_dbl_s {
  Dt_t        *dict;
  PDC_uint64  good;
  PDC_uint64  bad;
  PDC_uint64  fold;
  PDC_uint64  tracked;
  double      psum;
  double      avg;
  double      min;
  double      max;
} PDC_fpoint_acc_dbl;

typedef PDC_fpoint_acc_flt PDC_fpoint8_acc;
typedef PDC_fpoint_acc_flt PDC_fpoint16_acc;
typedef PDC_fpoint_acc_flt PDC_fpoint32_acc;
typedef PDC_fpoint_acc_dbl PDC_fpoint64_acc;

typedef PDC_fpoint_acc_flt PDC_ufpoint8_acc;
typedef PDC_fpoint_acc_flt PDC_ufpoint16_acc;
typedef PDC_fpoint_acc_flt PDC_ufpoint32_acc;
typedef PDC_fpoint_acc_dbl PDC_ufpoint64_acc;

PDC_error_t PDC_fpoint8_acc_init    (PDC_t *pdc, PDC_fpoint8_acc *a);
PDC_error_t PDC_fpoint8_acc_reset   (PDC_t *pdc, PDC_fpoint8_acc *a);
PDC_error_t PDC_fpoint8_acc_cleanup (PDC_t *pdc, PDC_fpoint8_acc *a);
PDC_error_t PDC_fpoint8_acc_add     (PDC_t *pdc, PDC_fpoint8_acc *a, PDC_base_ed *ed, PDC_fpoint8 *val);
PDC_error_t PDC_fpoint8_acc_report  (PDC_t *pdc, const char *prefix, const char *what, int nst,
				     PDC_fpoint8_acc *a);
float       PDC_fpoint8_acc_avg     (PDC_t *pdc, PDC_fpoint8_acc *a);

PDC_error_t PDC_fpoint16_acc_init   (PDC_t *pdc, PDC_fpoint16_acc *a);
PDC_error_t PDC_fpoint16_acc_reset  (PDC_t *pdc, PDC_fpoint16_acc *a);
PDC_error_t PDC_fpoint16_acc_cleanup(PDC_t *pdc, PDC_fpoint16_acc *a);
PDC_error_t PDC_fpoint16_acc_add    (PDC_t *pdc, PDC_fpoint16_acc *a, PDC_base_ed *ed, PDC_fpoint16 *val);
PDC_error_t PDC_fpoint16_acc_report (PDC_t *pdc, const char *prefix, const char *what, int nst,
				     PDC_fpoint16_acc *a);
float       PDC_fpoint16_acc_avg    (PDC_t *pdc, PDC_fpoint16_acc *a);

PDC_error_t PDC_fpoint32_acc_init   (PDC_t *pdc, PDC_fpoint32_acc *a);
PDC_error_t PDC_fpoint32_acc_reset  (PDC_t *pdc, PDC_fpoint32_acc *a);
PDC_error_t PDC_fpoint32_acc_cleanup(PDC_t *pdc, PDC_fpoint32_acc *a);
PDC_error_t PDC_fpoint32_acc_add    (PDC_t *pdc, PDC_fpoint32_acc *a, PDC_base_ed *ed, PDC_fpoint32 *val);
PDC_error_t PDC_fpoint32_acc_report (PDC_t *pdc, const char *prefix, const char *what, int nst,
				     PDC_fpoint32_acc *a);
float       PDC_fpoint32_acc_avg    (PDC_t *pdc, PDC_fpoint32_acc *a);

PDC_error_t PDC_fpoint64_acc_init   (PDC_t *pdc, PDC_fpoint64_acc *a);
PDC_error_t PDC_fpoint64_acc_reset  (PDC_t *pdc, PDC_fpoint64_acc *a);
PDC_error_t PDC_fpoint64_acc_cleanup(PDC_t *pdc, PDC_fpoint64_acc *a);
PDC_error_t PDC_fpoint64_acc_add    (PDC_t *pdc, PDC_fpoint64_acc *a, PDC_base_ed *ed, PDC_fpoint64 *val);
PDC_error_t PDC_fpoint64_acc_report (PDC_t *pdc, const char *prefix, const char *what, int nst,
				     PDC_fpoint64_acc *a);
double      PDC_fpoint64_acc_avg    (PDC_t *pdc, PDC_fpoint64_acc *a);

PDC_error_t PDC_ufpoint8_acc_init    (PDC_t *pdc, PDC_ufpoint8_acc *a);
PDC_error_t PDC_ufpoint8_acc_reset   (PDC_t *pdc, PDC_ufpoint8_acc *a);
PDC_error_t PDC_ufpoint8_acc_cleanup (PDC_t *pdc, PDC_ufpoint8_acc *a);
PDC_error_t PDC_ufpoint8_acc_add     (PDC_t *pdc, PDC_ufpoint8_acc *a, PDC_base_ed *ed, PDC_ufpoint8 *val);
PDC_error_t PDC_ufpoint8_acc_report  (PDC_t *pdc, const char *prefix, const char *what, int nst,
				     PDC_ufpoint8_acc *a);
float       PDC_ufpoint8_acc_avg     (PDC_t *pdc, PDC_ufpoint8_acc *a);

PDC_error_t PDC_ufpoint16_acc_init   (PDC_t *pdc, PDC_ufpoint16_acc *a);
PDC_error_t PDC_ufpoint16_acc_reset  (PDC_t *pdc, PDC_ufpoint16_acc *a);
PDC_error_t PDC_ufpoint16_acc_cleanup(PDC_t *pdc, PDC_ufpoint16_acc *a);
PDC_error_t PDC_ufpoint16_acc_add    (PDC_t *pdc, PDC_ufpoint16_acc *a, PDC_base_ed *ed, PDC_ufpoint16 *val);
PDC_error_t PDC_ufpoint16_acc_report (PDC_t *pdc, const char *prefix, const char *what, int nst,
				     PDC_ufpoint16_acc *a);
float       PDC_ufpoint16_acc_avg    (PDC_t *pdc, PDC_ufpoint16_acc *a);

PDC_error_t PDC_ufpoint32_acc_init   (PDC_t *pdc, PDC_ufpoint32_acc *a);
PDC_error_t PDC_ufpoint32_acc_reset  (PDC_t *pdc, PDC_ufpoint32_acc *a);
PDC_error_t PDC_ufpoint32_acc_cleanup(PDC_t *pdc, PDC_ufpoint32_acc *a);
PDC_error_t PDC_ufpoint32_acc_add    (PDC_t *pdc, PDC_ufpoint32_acc *a, PDC_base_ed *ed, PDC_ufpoint32 *val);
PDC_error_t PDC_ufpoint32_acc_report (PDC_t *pdc, const char *prefix, const char *what, int nst,
				     PDC_ufpoint32_acc *a);
float       PDC_ufpoint32_acc_avg    (PDC_t *pdc, PDC_ufpoint32_acc *a);

PDC_error_t PDC_ufpoint64_acc_init   (PDC_t *pdc, PDC_ufpoint64_acc *a);
PDC_error_t PDC_ufpoint64_acc_reset  (PDC_t *pdc, PDC_ufpoint64_acc *a);
PDC_error_t PDC_ufpoint64_acc_cleanup(PDC_t *pdc, PDC_ufpoint64_acc *a);
PDC_error_t PDC_ufpoint64_acc_add    (PDC_t *pdc, PDC_ufpoint64_acc *a, PDC_base_ed *ed, PDC_ufpoint64 *val);
PDC_error_t PDC_ufpoint64_acc_report (PDC_t *pdc, const char *prefix, const char *what, int nst,
				     PDC_ufpoint64_acc *a);
double      PDC_ufpoint64_acc_avg    (PDC_t *pdc, PDC_ufpoint64_acc *a);

/* ================================================================================
 * SCAN FUNCTIONS
 *
 * Scan functions are used to 'find' a location that is forward of the
 * current IO position.  They are normally used for error recovery purposes,
 * but are exposed here because they are generally useful.
 * N.B. Use PDC_a_char_lit_read / PDC_a_string_lit_read for cases where
 * a literal is known to be at the current IO position.
 */

/* PDC_a_char_lit_scan:
 *
 * EFFECT: 
 *  Scans for either goal character c or stop character s.  If a gloal
 *  char is found, then if eat_lit is non-zero the IO points to just
 *  beyond the char, otherwise it points to the char.  disc controls
 *  maximum scan distance.  Hitting eor or eof considered to be an
 *  error.  N.B. If there is mixed binary and ascii data, scanning can
 *  'find' an ascii char in a binary field.  Be careful!  Do not use 0
 *  to mean EOR/EOF.  If there is no stop char, use the same char for both
 *  the c and s params.
 *
 * RETURNS: PDC_error_t
 *         PDC_OK    => goal/stop char found, IO cursor now points to just beyond char
 *                      (eat_lit non-zero) or to the char (eat_lit zero).
 *                      if c_out, *c_out set to the char that was found
 *                      if offset_out, *offset_out set to the distance scanned to find that char
 *                      (0 means the IO cursor was already pointing at the found char)
 *         PDC_ERR   => char not found, IO cursor unchanged
 * 
 * PDC_a_str_lit_scan: same as a_char_lit_scan execpt a goal string
 * and stop string are given.  In this case, if there is no stop
 * string, a NULL stop string should be used.  On PDC_OK, *str_out
 * points to either findStr or stopStr (depending on which was found),
 * *offset_out is the distance scanned to find the string (0 means
 * the IO cursor was already pointing at the string).  If eat_lit
 * is non-zero, the IO cursor points just beyond the string literal
 * that was found, otherwise it points to the start of the string that
 * was found.  On PDC_ERR, the IO cursor is unchanged. 
 */

PDC_error_t PDC_a_char_lit_scan(PDC_t *pdc, PDC_byte c, PDC_byte s, int eat_lit,
				PDC_byte *c_out, size_t *offset_out);

PDC_error_t PDC_a_str_lit_scan(PDC_t *pdc, const PDC_string *findStr, const PDC_string *stopStr, int eat_lit,
			       PDC_string **str_out, size_t *offset_out);

/* ================================================================================
 * EBCDIC LITERAL READ and SCAN FUNCTIONS 
 *
 * Just like the ASCII versions, except the data read is EBCDIC data.
 *
 * ** N.B. The char or string to be read or scanned is specified in ASCII
 *    and converted to EBCDIC by the read or scan routine.  
 */

PDC_error_t PDC_e_char_lit_read(PDC_t *pdc, PDC_base_em *em,
				PDC_base_ed *ed, PDC_byte c);

PDC_error_t PDC_e_str_lit_read(PDC_t *pdc, PDC_base_em *em,
			       PDC_base_ed *ed, const PDC_string *s);

PDC_error_t PDC_e_char_lit_scan(PDC_t *pdc, PDC_byte c, PDC_byte s, int eat_lit,
				PDC_byte *c_out, size_t *offset_out);

PDC_error_t PDC_e_str_lit_scan(PDC_t *pdc, const PDC_string *findStr, const PDC_string *stopStr, int eat_lit,
			       PDC_string **str_out, size_t *offset_out);



/* ================================================================================
 * STRING COMPARISON
 */

#define PDC_string_eq(str1, str2) \
  ((str1)->len == (str2)->len && strncmp((str1)->str, (str2)->str, (str1)->len) == 0)

#define PDC_string_eq_Cstr(PDCstr, Cstr) \
  ((PDCstr)->len == strlen(Cstr) && strncmp((PDCstr)->str, (Cstr), (PDCstr)->len) == 0)

/* ================================================================================
 * IO CHECKPOINT API
 *
 * The checkpoint API: if any of these return PDC_ERR, it is due to a space
 * allocation problem or a non-balanced use of checkpoint/commit/restore.
 * These are normally fatal errors -- the calling code should probably exit the program.
 *
 * If a non-zero speculative flag is passed to checkpoint, then the
 * speculative nesting level  is incremented by one.  Once the checkpoint
 * is removed by either commit or restore, the nesting level is
 * decremented by one.  PDC_spec_level gives the current nesting level.
 */
PDC_error_t  PDC_IO_checkpoint (PDC_t *pdc, int speculative);
PDC_error_t  PDC_IO_commit     (PDC_t *pdc);
PDC_error_t  PDC_IO_restore    (PDC_t *pdc);
unsigned int PDC_spec_level    (PDC_t *pdc);

/* ================================================================================
 * REGULAR EXPRESSION SUPPORT
 *
 * PDC_regexp_compile: if regexp is a valid regular expression, this function
 * allocates a new compiled regular expression obj, assigns a handle to the obj
 * to (*regexp_out), and returns PDC_OK.  If regexp_out is NULL or regexp is
 * not valid, it returns PDC_ERR.
 *
 * PDC_regexp_free takes a handle to a compiled regexp obj and frees the obj.
 *
 * ** At the moment, the following regular expression forms are supported:
 *               EOR                  -- matches EOR
 *               [<chars>]            -- matches one of the chars in <chars>
 *               [<chars>]|EOR        -- matches either EOR or one of the chars in <chars>
 */

PDC_error_t PDC_regexp_compile(PDC_t *pdc, const char *regexp, PDC_regexp_t **regexp_out);
PDC_error_t PDC_regexp_free(PDC_t *pdc, PDC_regexp_t *regexp);

/* ================================================================================
 * MISC ROUTINES
 *
 *    PDC_fmt_char: produce a ptr to a string that is a pretty-print (escaped) formated for char c
 *        N.B. Resulting string should be printed immediately then not used again, e.g.,
 *        PDC_errorf(0, 0, "Missing separator: %s", PDC_fmt_Char(c)); 
 * 
 *    PDC_fmt_str   : same thing for a PDC_string
 *    PDC_fmt_Cstr  : same thing for a C string (must specify a char * ptr and a length)
 *
 *    PDC_qfmt_char/PDC_qfmt_str/PDC_qfmt_Cstr : same as above, but quote marks are added
 */
char *PDC_fmt_char(char c);
char *PDC_fmt_str(const PDC_string *s);
char *PDC_fmt_Cstr(const char *s, size_t len);
char *PDC_qfmt_char(char c);
char *PDC_qfmt_str(const PDC_string *s);
char *PDC_qfmt_Cstr(const char *s, size_t len);

/*
 * PDC_swap_bytes: in-place memory byte order swap
 *    num_bytes should be oneof: 1, 2, 4, 8
 */
PDC_error_t PDC_swap_bytes(PDC_byte *bytes, size_t num_bytes);

/*
 * Going away eventually
 */
PDC_error_t PDC_dummy_read(PDC_t *pdc, PDC_base_em *em, PDC_int32 dummy_val, PDC_base_ed *ed, PDC_int32 *res_out);

/* ================================================================================
 * INCLUDE THE IO DISCIPLINE DECLS
 */
#include "pdc_io_disc.h"

/* ================================================================================ */

#endif  /* __LIBPADSC_H__ */
