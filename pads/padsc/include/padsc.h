#ifdef _USE_PROTO
#pragma prototyped
#endif
/*
 * padsc library interface
 * 
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#ifndef __PADSC_H__
#define __PADSC_H__

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
#include "padsc-config.h"
#include "padsc-private.h"

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
 *   def_charset : default character set, one of:
 *                PDC_charset_ASCII
 *                PDC_charset_EBCDIC
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
 *   acc_max2track : default maximum distinct values for accumulators to track.
 *                 Use value PDC_MAX_UINT64 to indicate no limit.
 *                 Upon calling an acc_init function on some base-type accumulator a,
 *                 a.max2track is set to pdc->disc->acc_max2track, the default
 *                 limit on number of distinct to keep track of.
 *                 a.max2track can be modified directly after this call to force
 *                 accumulator a to use a non-default value.
 *
 *   acc_max2rep : default number of tracked values for accumulator to describe in detail in report.
 *                 Use value PDC_MAX_UINT64 to indicate no limit.
 *                 Upon calling an acc_init function on some base-type accumulator a,
 *                 a.max2rep is set to pdc->disc->acc_max2rep, the default
 *                 number of tracked values to describe in detail.
 *                 a.max2rep can be modified directly after this call to force
 *                 accumulator a to use a non-default value.
 *
 *   acc_pcnt2rep : default percent of values for accumulator to describe in detail in report.
 *                 Use value 100.0 to indicate no limit.
 *                 Upon calling an acc_init function on some base-type accumulator a,
 *                 a.pcnt2rep is set to pdc->disc->acc_pcnt2rep, the default
 *                 percent of values to describe in detail.
 *                 a.pcnt2rep can be modified directly after this call to force
 *                 accumulator a to use a non-default value.
 *
 *      [Note that the limit on reported values is hit when either the
 *       max2rep or pcnt2rep limit occurs.]
 *
 *      [Note that generated accumulators have components that are base-type
 *       accumlators.  Thus, after initializing some generated accumulator a,
 *       one could modify a.foo.bar.max2track or a.foo.bar.max2rep to change
 *       the tracking or reporting of the foo.bar component a.]
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
 *  file read discipline is used, scanning is limited to the scope of a single record.
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
 * Specifying what value to write during write calls when an invalid value is present:
 *
 * Write functions take a parse descriptor and a value.  The value is valid if the
 * parse descriptor's errCode is set PDC_NO_ERR.  The value has been filled in if the
 * errCode is PDC_USER_CONSTRAINT_VIOLATION.  For other errCodes, the value should be
 * assumed to contain garbage.  For invalid values, the write function must still
 * write SOME value.  For every type, one can specify an inv_val helper function
 * that produces an invalid value for the type, to be used by the type's write
 * functions.  If no function is specified, then a default invalid value is used,
 * where there are two cases: if the errorCode is PDC_USER_CONSTRAINT_VIOLATION, then
 * the current invalid value is used; for any other errorCode, a default invalid
 * value is used.
 *
 * The map from write functions to inv_val functions is found in the discipline:
 *
 *    inv_valfn_map: map from const char* (string form of the type name)
 *                        to PDC_inv_valfn function
 *                   can be NULL, in which case no mapping are used
 *
 * An invalid val function that handles type T values always takes 4 arguments:
 *       1. The PDC_t* handle
 *       2. A pointer to a type T parse descriptor
 *       3. A pointer to the invalid type T rep
 *       4. A void ** arg which is a pointer to a list of pointers to type parameters,
 *          where the list is terminated by a null pointer.  For example,
 *          type a_int32_FW(:<width>:) has a single type parameter (width) of type PDC_uint32.
 *   Args 2-4 use void* types to enable the table to be used with arbitrary types,
 *   including user-defined types.  One must cast these void* args to the appropriate
 *   error pointer types before use -- see the example below.  The function should
 *   replace the invalid value with a new 'invalid val' value.  Return PDC_OK on
 *   success and PDC_ERR if a replacement value has not been set.
 *
 * Use PDC_set_inv_valfn to set a function ptr, PDC_get_inv_valfn to do a lookup.
 *
 * EXAMPLE: suppose an a_int32 field has an attached constraint that requires the
 * value must be >= -30.  If a value of -50 is read, errCode will be
 * PDC_USER_CONSTRAINT_VIOLATION, and if no inv_val function is provided then the
 * a_int32 write function will output -50.  If the read function fails to read even a
 * valid integer, the errCode will be PDC_INVALID_A_NUM, and the a_int32 write
 * function will output PDC_MIN_INT32 (the default invalid value for all int32 write
 * functions). If one wanted to correct all user contraint cases to use value -30, and
 * to use PDC_INT32_MAX for other invalid cases, one could provide an inv_val
 * helper function to do so:
 *
 *   PDC_error_t my_int32_inv_val(PDC_t *pdc, void *pd_void, void *val_void, void **type_args) {
 *     PDC_base_pd *pd  = (PDC_base_pd*)pd_void;
 *     PDC_int32   *val = (PDC_int32*)val_void;
 *     if (pd->errCode == PDC_USER_CONSTRAINT_VIOLATION) {
 *       (*val) = -30;
 *     } else {
 *       (*val) = PDC_INT32_MAX;
 *     }
 *     return PDC_OK;
 *   }
 *
 *   pdc->disc->inv_valfn_map = PDC_inv_valfn_map_create(pdc);   (only needed if no map installed yet)
 *   PDC_set_inv_valfn(pdc, pdc->disc->inv_valfn_map, "PDC_int32", my_int32_inv_val);
 *
 * N.B. Note that for a type T with three forms, PDC_T, PDC_a_T, and PDC_e_T, there
 * is only one entry in the inv_valfn_map, under string "PDC_T".  For example, use
 * "PDC_int32" to specify an invalid val function for all of these types: PDC_int32,
 * PDC_a_int32, PDC_e_int32.
 *
 * N.B. An inv_valfn for a string type should use PDC_string_copy, PDC_string_Cstr_copy,
 * PDC_string_share, or PDC_string_Cstr_share to fill in the value of the PDC_string* param.
 * 
 * The default disc is PDC_default_disc.  It provides the following defaults:
 *    version:       PDC_VERSION (above) 
 *    flags:         0
 *    def_charset:   PDC_charset_ASCII
 *    copy_strings:  0
 *    stop_regexp:   0
 *    stop_maxlen:   0
 *    errorf:        PDC_errorf
 *    e_rep:         PDC_errorRep_Max
 *    d_endian:      PDC_littleEndian
 *    acc_max2track  1000
 *    acc_max2rep    10
 *    inv_valfn_map  NULL -- user must created and install a map
 *                           if inv_val functions need to be provided
 *    io_disc:       NULL -- a default IO discipline (newline-terminated records)
 *                     is installed on PDC_open if one is not installed beforehand
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

typedef enum PDC_error_t_e {
  PDC_OK                            =    0,
  PDC_ERR                           =   -1
} PDC_error_t;

typedef enum PDC_errCode_t_e {
  /* First set of errors have no corresponding location  */
  PDC_NOT_PARSED                    =    0,
  PDC_NO_ERR                        =    1,
  PDC_SKIPPED                       =    2, 

  PDC_UNEXPECTED_ERR                =    3,

  PDC_BAD_PARAM                     =    4,
  PDC_SYS_ERR                       =    5,
  PDC_IO_ERR                        =    6,

  PDC_CHKPOINT_ERR                  =   11,
  PDC_COMMIT_ERR                    =   12,
  PDC_RESTORE_ERR                   =   13,
  PDC_ALLOC_ERR                     =   14,
  PDC_FORWARD_ERR                   =   15,
  PDC_PANIC_SKIPPED                 =   20,

  /* The following errors (code >= 100) DO have a corresponding location  */
  PDC_USER_CONSTRAINT_VIOLATION     =  100,
  PDC_MISSING_LITERAL               =  101,
  PDC_ARRAY_ELEM_ERR                =  110,
  PDC_ARRAY_SEP_ERR                 =  111,
  PDC_ARRAY_TERM_ERR                =  112,
  PDC_ARRAY_SIZE_ERR                =  113,
  PDC_ARRAY_SEP_TERM_SAME_ERR       =  114,      
  PDC_ARRAY_USER_CONSTRAINT_ERR     =  115,
  PDC_ARRAY_MIN_BIGGER_THAN_MAX_ERR =  116,
  PDC_ARRAY_MIN_NEGATIVE            =  117,
  PDC_ARRAY_MAX_NEGATIVE            =  118,
  PDC_ARRAY_EXTRA_BEFORE_SEP        =  119,
  PDC_ARRAY_EXTRA_BEFORE_TERM       =  120,

  PDC_STRUCT_FIELD_ERR              =  125,
  PDC_STRUCT_EXTRA_BEFORE_SEP       =  126,
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

  PDC_INVALID_CHARSET               =  190,
  PDC_INVALID_WIDTH                 =  191,

  PDC_CHAR_LIT_NOT_FOUND            =  200,
  PDC_STR_LIT_NOT_FOUND             =  210,
  PDC_REGEXP_NOT_FOUND              =  220,
  PDC_INVALID_REGEXP                =  230,
  PDC_WIDTH_NOT_AVAILABLE           =  240,
  PDC_INVALID_DATE                  =  250
} PDC_errCode_t;

/* parse state flags */
#define PDC_Panic               0x0001
/* more flags will be added later to support partial-read functionality */ 
 
/*
 * Other useful constants
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

/* DEFAULT 'invalid value' VALUES */

#define PDC_CHAR_DEF_INV_VAL     PDC_MAX_UINT8

#define PDC_INT8_DEF_INV_VAL     PDC_MIN_INT8
#define PDC_UINT8_DEF_INV_VAL    PDC_MAX_UINT8
#define PDC_INT16_DEF_INV_VAL    PDC_MIN_INT16
#define PDC_UINT16_DEF_INV_VAL   PDC_MAX_UINT16
#define PDC_INT32_DEF_INV_VAL    PDC_MIN_INT32
#define PDC_UINT32_DEF_INV_VAL   PDC_MAX_UINT32
#define PDC_INT64_DEF_INV_VAL    PDC_MIN_INT64
#define PDC_UINT64_DEF_INV_VAL   PDC_MAX_UINT64


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
 *     PDC_base_pd   : base parse descriptor
 *     PDC_base_m    : base mask
 *     PDC_errorRep  : enum for specifying error reporting level
 *     PDC_endian    : enum for specifying endian-ness
 *     PDC_charset   : enum for specifying character set
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
typedef struct PDC_base_pd_s       PDC_base_pd;
typedef enum   PDC_errorRep_e      PDC_errorRep;
typedef enum   PDC_endian_e        PDC_endian;
typedef enum   PDC_charset_e       PDC_charset;

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

typedef PDC_uint8 PDC_char;

/* HELPERS: 
 *    PDC_FPOINT2FLT calculates num/denom as a float
 *    PDC_FPOINT2DBL calculates num/denom as a double
 */
#define PDC_FPOINT2FLT(fp) ((fp).num/(float)(fp).denom)
#define PDC_FPOINT2DBL(fp) ((fp).num/(double)(fp).denom)

/* flags are PDC_uint32 values */
typedef PDC_uint32          PDC_flags_t;

#ifdef FOR_CKIT
extern PDC_uint32 PDC_NULL_CTL_FLAG;
extern PDC_uint32 PDC_WSPACE_OK;
#else
#define PDC_NULL_CTL_FLAG      0UL
#define PDC_WSPACE_OK          1UL
#endif /* FOR_CKIT */


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
 * STRING HELPER FUNCTIONS
 *
 *    PDC_string_init       : initialize to valid empty string (no dynamic memory allocated yet)
 *
 *    PDC_string_cleanup    : free up the rbuf and any allocated space for the string
 *
 *    PDC_string_share      : makes the PDC_string targ refer to the string in PDC_string src,
 *                            sharing the space with the original owner.
 *
 *    PDC_string_Cstr_share : makes the PDC_string targ refer len chars in the C-style string src.
 *
 *       Note on sharing: the original space for the string (src) must not be 'cleaned up' while
 *                        the targ PDC_string continues to be used.  One can use PDC_string_preserve
 *                        on targ if it becomes necessary to copy the string into targ at a later point.
 *
 *    PDC_string_copy      : Copy src PDC_string into targ PDC_string; sharing is not used.
 *
 *    PDC_string_Cstr_copy : copy len chars from C-style string src into the PDC_string targ;
 *                           sharing is not used.
 *
 *       Both copy functions allocate an RBuf and/or space for the copy, as necessary.
 *       Although not strictly necessary, they also null-terminate targ->str.
 *       They return PDC_ERR on bad arguments or on failure to alloc space, otherwise PDC_OK.
 *
 *    PDC_string_preserve  : If the string is using space-sharing, force it use a private copy 
 *                            instead, so that the (formerly) shared space can be discarded.
 *                            It is safe to call preserve on any PDC_string.
 *
 * String comparison:
 *
 *    PDC_string_eq        : compares two PDC_string, str1 and str2.
 *                            returns 0 if str1 equals str2, a negative # if str1 < str2,
 *                            and a positive # if str1 > str2.
 *
 *    PDC_string_eq_Cstr   : compare PDC_string str to a C-style string Cstr.
 *                            returns 0 if str equals Cstr, a negative # if str < Cstr,
 *                            and a positive # if str > Cstr.
 */

PDC_error_t PDC_string_init(PDC_t *pdc, PDC_string *s);
PDC_error_t PDC_string_cleanup(PDC_t *pdc, PDC_string *s);
PDC_error_t PDC_string_share(PDC_t *pdc, PDC_string *targ, const PDC_string *src);
PDC_error_t PDC_string_Cstr_share(PDC_t *pdc, PDC_string *targ, const char *src, size_t len);
PDC_error_t PDC_string_copy(PDC_t *pdc, PDC_string *targ, const PDC_string *src);
PDC_error_t PDC_string_Cstr_copy(PDC_t *pdc, PDC_string *targ, const char *src, size_t len);
PDC_error_t PDC_string_preserve(PDC_t *pdc, PDC_string *s);
#ifdef FOR_CKIT
int PDC_string_eq(const PDC_string *str1, const PDC_string *str2);
int PDC_string_eq_Cstr(const PDC_string *str, const char *Cstr);
#endif /* FOR_CKIT */


/*
 * A base type T with T_init/T_cleanup must also have T_pd_init/T_pd_cleanup.
 * Similarly, if T has T_copy, it must also have T_pd_copy.
 *
 * For PDC_string_ed, which is just a PDC_base_pd, init and cleanup are no-ops,
 * while copy has a trivial implementation (struct assignment).
 */

PDC_error_t PDC_string_pd_init(PDC_t *pdc, PDC_base_pd *pd);
PDC_error_t PDC_string_pd_cleanup(PDC_t *pdc, PDC_base_pd *pd);
PDC_error_t PDC_string_pd_copy(PDC_t *pdc, PDC_base_pd *targ, const PDC_base_pd *src);

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

/* type PDC_base_m: */
typedef PDC_uint32 PDC_base_m;

#ifdef FOR_CKIT
/* Declarations for CKIT */
extern PDC_uint32 PDC_Set;
extern PDC_uint32 PDC_SynCheck;
extern PDC_uint32 PDC_SemCheck;
extern PDC_uint32 PDC_Write;

extern PDC_uint32 PDC_CheckAndSet;
extern PDC_uint32 PDC_BothCheck;
extern PDC_uint32 PDC_Ignore;

PDC_uint32 PDC_Test_Set(PDC_uint32 m);
PDC_uint32 PDC_Test_SynCheck(PDC_uint32 m);
PDC_uint32 PDC_Test_SemCheck(PDC_uint32 m);
PDC_uint32 PDC_Test_Write(PDC_uint32 m);

PDC_uint32 PDC_Test_NotSet(PDC_uint32 m);
PDC_uint32 PDC_Test_NotSynCheck(PDC_uint32 m);
PDC_uint32 PDC_Test_NotSemCheck(PDC_uint32 m);
PDC_uint32 PDC_Test_NotWrite(PDC_uint32 m);

PDC_uint32 PDC_Test_CheckAndSet(PDC_uint32 m);
PDC_uint32 PDC_Test_BothCheck(PDC_uint32 m);
PDC_uint32 PDC_Test_Ignore(PDC_uint32 m);

PDC_uint32 PDC_Test_NotCheckAndSet(PDC_uint32 m);
PDC_uint32 PDC_Test_NotBothCheck(PDC_uint32 m);
PDC_uint32 PDC_Test_NotIgnore(PDC_uint32 m);

void       PDC_Do_Set(PDC_uint32 m);
void       PDC_Do_SynCheck(PDC_uint32 m);
void       PDC_Do_SemCheck(PDC_uint32 m);
void       PDC_Do_Write(PDC_uint32 m);

void       PDC_Dont_Set(PDC_uint32 m);
void       PDC_Dont_SynCheck(PDC_uint32 m);
void       PDC_Dont_SemCheck(PDC_uint32 m);
void       PDC_Dont_Write(PDC_uint32 m);

#else
/* The actual declarations */

/* Mask flags used with read functions */
#define PDC_Set                 0x0001
#define PDC_SynCheck            0x0002
#define PDC_SemCheck            0x0004

/* Mask flags used with write functions */
#define PDC_Write               0x0008

/* Useful Combinations of Mask Flags */
#define PDC_CheckAndSet         0x0007     /* PDC_Set|PDC_SynCheck|PDC_SemCheck */
#define PDC_BothCheck           0x0006     /* PDC_SynCheck|PDC_SemCheck */
#define PDC_Ignore              0x0000     /* none of the checks, no set */

/* Useful macros for testing or modifying mask bits */

#define PDC_Test_Set(m)            (m & PDC_Set)
#define PDC_Test_SynCheck(m)       (m & PDC_SynCheck)
#define PDC_Test_SemCheck(m)       (m & PDC_SemCheck)
#define PDC_Test_Write(m)          (m & PDC_Write)

#define PDC_Test_NotSet(m)         (!PDC_Test_Set(m))
#define PDC_Test_NotSynCheck(m)    (!PDC_Test_SynCheck(m))
#define PDC_Test_NotSemCheck(m)    (!PDC_Test_SemCheck(m))
#define PDC_Test_NotWrite(m)       (!PDC_Test_Write(m))

#define PDC_Test_CheckAndSet(m)    ((m & PDC_CheckAndSet) == PDC_CheckAndSet)
#define PDC_Test_BothCheck(m)      ((m & PDC_CheckAndSet) == PDC_BothCheck)
#define PDC_Test_Ignore(m)         ((m & PDC_CheckAndSet) == PDC_Ignore)

#define PDC_Test_NotCheckAndSet(m) ((m & PDC_CheckAndSet) != PDC_CheckAndSet)
#define PDC_Test_NotBothCheck(m)   ((m & PDC_CheckAndSet) != PDC_BothCheck)
#define PDC_Test_NotIgnore(m)      ((m & PDC_CheckAndSet) != PDC_Ignore)

#define PDC_Do_Set(m)              (m |= PDC_Set)
#define PDC_Do_SynCheck(m)         (m |= PDC_SynCheck)
#define PDC_Do_SemCheck(m)         (m |= PDC_SemCheck)
#define PDC_Do_Write(m)            (m |= PDC_Write)

#define PDC_Dont_Set(m)            (m &= (~PDC_Set))
#define PDC_Dont_SynCheck(m)       (m &= (~PDC_SynCheck))
#define PDC_Dont_SemCheck(m)       (m &= (~PDC_SemCheck))
#define PDC_Dont_Write(m)          (m &= (~PDC_Write))

#endif  /*  FOR_CKIT  */

/* type PDC_errorRep: */
enum PDC_errorRep_e { PDC_errorRep_Max, PDC_errorRep_Med, PDC_errorRep_Min, PDC_errorRep_None };

/* type PDC_endian: */
enum PDC_endian_e { PDC_bigEndian, PDC_littleEndian };

/* type PDC_charset: */
enum PDC_charset_e { PDC_charset_INVALID = 0, PDC_charset_ASCII = 1, PDC_charset_EBCDIC = 2 };

/* helper functions for the above enumerated types: */
const char *PDC_base_m2str   (PDC_t *pdc, PDC_base_m  m);
const char *PDC_errorRep2str (PDC_errorRep  e);
const char *PDC_endian2str   (PDC_endian    e);
const char *PDC_charset2str  (PDC_charset e); 
/* Note: For PDC_base_m2str, result should be used/copied prior to further library calls */

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

/* type PDC_base_pd: */
struct PDC_base_pd_s {
  PDC_flags_t    pstate; /* parse state */
  PDC_errCode_t  errCode;
  PDC_loc_t      loc;
};

/* Functions (macros actually) for setting or testing parse state (PS) pd->pstate */
#ifdef FOR_CKIT
void PDC_PS_init(void *pd);         /* init pd->pstate */
void PDC_PS_setPanic(void *pd);     /* set PDC_Panic in pd->pstate */
void PDC_PS_unsetPanic(void *pd);   /* unset PDC_Panic in pd->pstate */
int  PDC_PS_isPanic(void *pd);      /* test whether PDC_Panic is set in pd->pstate */
#endif

/* Function (macro actually) for initalizing a PDC_base_pd: */
#ifdef FOR_CKIT
void PDC_base_pd_init(PDC_base_pd *pd); /* init pstate to 'not panic' state; errCode to PDC_NO_ERR */
#endif

/* PDC_inv_valfn: type of a pointer to an invalid val function */
typedef PDC_error_t (*PDC_inv_valfn)(PDC_t *pdc, void *pd_void, void *val_void, void **type_args);

/* PDC_inv_valfn_map_t: type of an invalid val function map */
typedef struct PDC_inv_valfn_map_s PDC_inv_valfn_map_t;

/* type PDC_disc_t: */
struct PDC_disc_s {
  PDC_flags_t           version;       /* interface version */
  PDC_flags_t           flags;         /* control flags */
  PDC_charset           def_charset;   /* default char set */ 
  int                   copy_strings;  /* if non-zero,  ASCII string read functions copy the strings found, otherwise not */
  PDC_regexp_t          *stop_regexp;  /* scan stop pattern, use 0 to disable */
  size_t                stop_maxlen;   /* max scan distance, use 0 to disable */
  PDC_error_f           errorf;        /* error function using  ... */
  PDC_errorRep          e_rep;         /* controls error reporting */
  PDC_endian            d_endian;      /* endian-ness of the data */ 
  PDC_uint64            acc_max2track; /* default maximum distinct values for accumulators to track */
  PDC_uint64            acc_max2rep;   /* default maximum number of tracked values to describe in detail in report */
  double                acc_pcnt2rep;  /* default maximum percent of values to describe in detail in report */
  PDC_inv_valfn_map_t  *inv_valfn_map; /* map types to inv_valfn for write functions */
  PDC_IO_disc_t        *io_disc;       /* sub-discipline for controlling IO */
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
 * TOP-LEVEL invalid_valfn FUNCTIONS
 *
 * Getting and setting invalid val functions in a map:
 *   PDC_get_inv_valfn returns the currently installed function for type_name, or NULL if none is installed
 *
 *   PDC_set_inv_valfn returns the previously installed function for type_name, or NULL if none was installed.
 *   If the fn argument is NULL, any current mapping for type_name is removed.
 *
 * Creating and destroying invalid val function maps: 
 *
 * PDC_inv_valfn_map_create: create a new, empty map
 * PDC_inv_valfn_map_destroy: destroy a map
 *
 */
PDC_inv_valfn PDC_get_inv_valfn(PDC_t* pdc, PDC_inv_valfn_map_t *map, const char *type_name); 
PDC_inv_valfn PDC_set_inv_valfn(PDC_t* pdc, PDC_inv_valfn_map_t *map, const char *type_name, PDC_inv_valfn fn);

PDC_inv_valfn_map_t* PDC_inv_valfn_map_create(PDC_t *pdc);
PDC_error_t          PDC_inv_valfn_map_destroy(PDC_t *pdc, PDC_inv_valfn_map_t *map);

/* ================================================================================
 * TOP-LEVEL IO FUNCTIONS
 * 
 * PDC_IO_set      : Initialize or change the current sfio stream used for input.
 *
 * PDC_IO_fopen    : Open a file for reading (a higher-level alternative to io_set).
 *                   Uses disc->fopen_fn, if present, otherwise default PDC_fopen.
 *                   Returns PDC_OK on success, PDC_ERR on error
 *
 * PDC_IO_close    : Clean up the io discipline state.
 *                   If a file was opened via PDC_IO_fopen, close it.
 *                   If an io was installed via PDC_IO_set, it is not closed.
 *                   When an IO is installed via PDC_IO_set, it is up to the
 *                   the program that opened to installed io to close it
 *                   (*after* calling PDC_IO_close).
 *
 * PDC_IO_next_rec : Advances current IO position to start of the next record, if any.
 *                   Returns PDC_OK on success, PDC_ERR on failure 
 *                   (failure includes hitting EOF before EOR).
 *                   For PDC_OK case, sets (*skipped_bytes_out) to the number of
 *                   data bytes that were passed over while searching for EOR.
 *
 * PDC_IO_at_EOR   : Returns 1 if the current IO position is at EOR, otherwise 0.
 * PDC_IO_at_EOF   : Returns 1 if current IO position is at EOF, otherwise 0.
 * PDC_IO_at_EOR_or_EOF : Returns 1 if current IO position is at EOR or EOF, otherwise 0.
 *
 * PDC_IO_getPos   : Fill in (*pos) with IO position.
 * PDC_IO_getLocB  : Fill in loc->b with IO position.
 * PDC_IO_getLocE  : Fill in loc->e with IO position.
 * PDC_IO_getLoc   : Fill in both loc->b and loc->e with IO position.
 *
 *   All of the above take an offset.  If offset is 0, the current IO position is
 *   used, otherwise the position used is K bytes from the current IO position
 *   (offset == K ... offset is an int, and can be positive or negative).
 *   Note the current IO position does not change.  PDC_ERR is returned if
 *   info about the specified position cannot be determined. 
 *   EOR marker bytes (if any) are ignored when moving forward or back
 *   based on offset -- offset only refers to data bytes.
 *
 *
 * PDC_IO_write_start:   Alloc a buffer buf associated with an output Sfio stream io
 *                       that can be filled in using the write2buf functions.
 *                       Must be paired with either commit_write or abort_write. 
 *                       Param buf_len specifies how many bytes will be required, and
 *                       can be modified to a greater value if an existing buffer of
 *                       larger size is available.  Param set_buf is set to indicate whether the
 *                       stream's buffer was set to an internal PDC buffer.  buf, io, and set_buf
 *                       must be passed to the paired commit_write or abort_write.
 *                       Returns NULL on failure, buf on success.
 *
 * PDC_IO_write_commit:  Write num_bytes bytes from buf to io, undo write_start effects.
 *                       Returns -1 on error, otherwise number of bytes written.
 * 
 * PDC_IO_write_abort:   Undo write_start effects; do not write anything.
 *
 * Record and Block-of-Records write functions:
 *
 *   Note: pdc->disc->io_disc must be set to a valid PADS I/O discipline when using the
 *         following functions.  Further, the I/O discipline must support records
 *         to use the record write functions, and it must support blocks of records to
 *         use the block-of-record write functions.
 *
 * PDC_IO_rec_write2io: write a record to io, an Sfio stream.  buf must contain the data bytes
 *                      (of length rec_data_len) for the record.  Record start/end markers are
 *                      written around the data bytes according to the current I/O discipline.
 *                      On success, the total number of bytes added to io is returned.  On failure,
 *                      no bytes are added to io and -1 is returned.
 *
 * PDC_IO_rec_open_write2buf: append an open record marker (if used) to a buffer buf that has at least buf_len
 *                      available bytes.  If the open record marker would require more than buf_len
 *                      bytes, (*buf_full) is set to 1 and -1 is returned.  For all other cases,
 *                      (*buf_full) is unmodified.  Returns -1 on failure, otherwise the number of
 *                      bytes appeneded.  Note: PDC_IO_rec_close_write2buf *must* be called with
 *                      param rec_start set to the same location that was passed as buf to this call,
 *                      to allow the record open marker to be updated with appropriate length info.
 *
 * PDC_IO_rec_close_write2buf: append a record close marker (if used) to a buffer buf that has at least buf_len
 *                      available bytes.  If the close record marker would require more than buf_len bytes,
 *                      (*buf_full) is set to 1 and -1 is returned.  For all other cases, (*buf_full) is
 *                      unmodified.  Returns -1 on failure, otherwise the number of bytes
 *                      appended.  Note that param rec_start must be used to specify the location of the
 *                      record open marker / start of record, and num_bytes must specify the number of
 *                      bytes used for both the open marker and the data bytes of the record.  Thus,
 *                      num_bytes will equal (buf - rec_start) if the full record is in a contiguous
 *                      region of memory.
 *
 * PDC_IO_rblk_write2io: write a block of records to io, an Sfio stream.  buf must contain the data bytes
 *                      for all of the records (of length blk_data_len).  Block start/end markers are
 *                      written around the data bytes according to the current I/O discipline, where
 *                      num_recs or the appropriate length will be written as appropriate.  (Some disciplines
 *                      require a length for the block, others require the number of records in the block, so
 *                      both must be provided.)  
 *                      On success, the total number of bytes added to io is returned.  On failure,
 *                      no bytes are added to io and -1 is returned.
 *
 * PDC_IO_rblk_open_write2buf: append an open block marker (if used) to a buffer buf that has at least buf_len
 *                      available bytes.  If the open marker would require more than buf_len
 *                      bytes, (*buf_full) is set to 1 and -1 is returned.  For all other cases,
 *                      (*buf_full) is unmodified.  Returns -1 on failure, otherwise the number of
 *                      bytes appeneded.  Note: PDC_IO_rblk_close_write2buf *must* be called with
 *                      param blk_start set to the same location that was passed as buf to this call,
 *                      to allow the block open marker to be updated with appropriate info.
 *
 * PDC_IO_rblk_close_write2buf: append a block close marker (if used) to a buffer buf that has at least buf_len
 *                      available bytes.  If the close marker would require more than buf_len bytes,
 *                      (*buf_full) is set to 1 and -1 is returned.  For all other cases, (*buf_full) is
 *                      unmodified.  Returns -1 on failure, otherwise the number of bytes
 *                      appended.  Note that param blk_start must be used to specify the location of the
 *                      block open marker / start of block, and num_bytes must specify the number of
 *                      bytes used for both the block open marker and the data bytes of all records.  Thus,
 *                      num_bytes will equal (buf - blk_start) if the full block is in a contiguous
 *                      region of memory.  As with rblk_write2io, num_recs must specify the number of records
 *                      in the block.
 */

PDC_error_t  PDC_IO_set      (PDC_t *pdc, Sfio_t *io);
PDC_error_t  PDC_IO_fopen    (PDC_t *pdc, const char *path);
PDC_error_t  PDC_IO_close    (PDC_t *pdc);
PDC_error_t  PDC_IO_next_rec (PDC_t *pdc, size_t *skipped_bytes_out);

int          PDC_IO_at_EOR        (PDC_t *pdc);
int          PDC_IO_at_EOF        (PDC_t *pdc);
int          PDC_IO_at_EOR_or_EOF (PDC_t *pdc);

PDC_error_t  PDC_IO_getPos   (PDC_t *pdc, PDC_pos_t *pos, int offset); 
PDC_error_t  PDC_IO_getLocB  (PDC_t *pdc, PDC_loc_t *loc, int offset); 
PDC_error_t  PDC_IO_getLocE  (PDC_t *pdc, PDC_loc_t *loc, int offset); 
PDC_error_t  PDC_IO_getLoc   (PDC_t *pdc, PDC_loc_t *loc, int offset); 

#if PDC_CONFIG_WRITE_FUNCTIONS > 0
PDC_byte*    PDC_IO_write_start (PDC_t *pdc, Sfio_t *io, size_t *buf_len, int *set_buf);
ssize_t      PDC_IO_write_commit(PDC_t *pdc, Sfio_t *io, PDC_byte *buf, int set_buf, size_t num_bytes);
void         PDC_IO_write_abort (PDC_t *pdc, Sfio_t *io, PDC_byte *buf, int set_buf);

ssize_t      PDC_IO_rec_write2io(PDC_t *pdc, Sfio_t *io, PDC_byte *buf, size_t rec_data_len);
ssize_t      PDC_IO_rec_open_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full);
ssize_t      PDC_IO_rec_close_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full,
					PDC_byte *rec_start, size_t num_bytes);

ssize_t      PDC_IO_rblk_write2io(PDC_t *pdc, Sfio_t *io, PDC_byte *buf, size_t blk_data_len, PDC_uint32 num_recs);
ssize_t      PDC_IO_rblk_open_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full);
ssize_t      PDC_IO_rblk_close_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full,
					 PDC_byte *blk_start, size_t num_bytes, PDC_uint32 num_recs);
#endif

/* ================================================================================
 * LITERAL SCAN FUNCTIONS
 *
 * Scan functions are used to 'find' a location that is forward of the
 * current IO position.  They are normally used for error recovery purposes,
 * but are exposed here because they are generally useful.
 * N.B. Use the char_lit_read functions for cases where
 * a literal is known to be at the current IO position.
 *
 * DEFAULT                        ASCII                          EBCDIC
 * -----------------------------  -----------------------------  -----------------------------
 * PDC_char_lit_scan              PDC_a_char_lit_scan            PDC_e_char_lit_scan
 *
 * EFFECT: 
 *
 *  Scans for either goal character c or stop character s.  The chars are
 *  specified as ASCII characters, and are converted to EBCDIC if the EBCDIC
 *  form is used or if the DEFAULT form is used and pdc->disc->def_charset is
 *  PDC_charset_EBCDIC.
 *

 *  If a goal char is found, then if the corresponding 'eat' param
 *  (eat_c if c is found, eat_s if s is found) is non-zero the IO
 *  points to just beyond the char, otherwise it points to the char.
 *  pdc->disc controls maximum scan distance.  Hitting eor or eof
 *  considered to be an error.  N.B. If there is mixed binary and
 *  ascii data, scanning can 'find' an ascii char in a binary field.
 *  Be careful!  Do not use 0 to mean EOR/EOF.  If there is no stop
 *  char, use the same char for both the c and s params.

 *
 * RETURNS: PDC_error_t
 *         PDC_OK    => goal/stop char found, IO cursor now points to just beyond char
 *                      (corresponding eat param non-zero) or to the char (eat param zero).
 *                      if c_out, *c_out set to the ASCII version of the char that was found
 *                      if offset_out, *offset_out set to the distance scanned to find that char
 *                      (0 means the IO cursor was already pointing at the found char)
 *         PDC_ERR   => char not found, IO cursor unchanged
 * 
 * DEFAULT                        ASCII                          EBCDIC
 * -----------------------------  -----------------------------  -----------------------------
 * PDC_str_lit_scan               PDC_a_str_lit_scan             PDC_e_str_lit_scan
 * PDC_Cstr_lit_scan              PDC_a_Cstr_lit_scan            PDC_e_Cstr_lit_scan
 *
 * These functions are similar to the character scan functions, except ASCII goal
 * and stop strings are given.  These strings are converted to EBCDIC if an EBCDIC form
 * is used or if a DEFAULT form is used and pdc->disc->def_charset is PDC_charset_EBCDIC.
 *
 * If there is no stop string, a NULL stop string should be used.  On
 * PDC_OK, if str_out is set then (*str_out) points to the original
 * ASCII version of either findStr or stopStr, depending on which was
 * found, and if offset is set then (*offset_out) is set to the
 * distance scanned to find the string (0 means the IO cursor was
 * already pointing at the string). If the corresponding eat param is
 * non-zero (eat_findStr for findStr, eat_stopStr for stopStr), the IO
 * cursor points just beyond the string literal that was found,
 * otherwise it points to the start of the string that was found.  On
 * PDC_ERR, the IO cursor is unchanged.
 */

#ifdef FOR_CKIT
#if PDC_CONFIG_READ_FUNCTIONS > 0

#if PDC_CONFIG_A_CHAR_STRING > 0
PDC_error_t PDC_a_char_lit_scan(PDC_t *pdc, PDC_char c, PDC_char s, int eat_c, int eat_s,
				PDC_char *c_out, size_t *offset_out);
PDC_error_t PDC_a_str_lit_scan (PDC_t *pdc, const PDC_string *findStr, const PDC_string *stopStr,
				int eat_findStr, int eat_stopStr,
			        PDC_string **str_out, size_t *offset_out);
PDC_error_t PDC_a_Cstr_lit_scan(PDC_t *pdc, const char *findStr, const char *stopStr,
				int eat_findStr, int eat_stopStr,
				const char **str_out, size_t *offset_out);
#endif

#if PDC_CONFIG_E_CHAR_STRING > 0
PDC_error_t PDC_e_char_lit_scan(PDC_t *pdc, PDC_char c, PDC_char s, int eat_c, int eat_s,
				PDC_char *c_out, size_t *offset_out);
PDC_error_t PDC_e_str_lit_scan (PDC_t *pdc, const PDC_string *findStr, const PDC_string *stopStr,
				int eat_findStr, int eat_stopStr,
			        PDC_string **str_out, size_t *offset_out);
PDC_error_t PDC_e_Cstr_lit_scan(PDC_t *pdc, const char *findStr, const char *stopStr,
				int eat_findStr, int eat_stopStr,
				const char **str_out, size_t *offset_out);
#endif

#if PDC_CONFIG_A_CHAR_STRING > 0 && PDC_CONFIG_E_CHAR_STRING > 0
PDC_error_t PDC_char_lit_scan  (PDC_t *pdc, PDC_char c, PDC_char s, int eat_c, int eat_s,
			        PDC_char *c_out, size_t *offset_out);
PDC_error_t PDC_str_lit_scan   (PDC_t *pdc, const PDC_string *findStr, const PDC_string *stopStr,
				int eat_findStr, int eat_stopStr,
			        PDC_string **str_out, size_t *offset_out);
PDC_error_t PDC_Cstr_lit_scan  (PDC_t *pdc, const char *findStr, const char *stopStr,
				int eat_findStr, int eat_stopStr,
				const char **str_out, size_t *offset_out);
#endif

#endif /* PDC_CONFIG_READ_FUNCTIONS */
#endif /* FOR_CKIT */

/* ================================================================================
 * LITERAL READ FUNCTIONS
 *
 * DEFAULT                        ASCII                          EBCDIC
 * -----------------------------  -----------------------------  -----------------------------
 * PDC_char_lit_read              PDC_a_char_lit_read            PDC_e_char_lit_read
 * PDC_str_lit_read               PDC_a_str_lit_read             PDC_e_str_lit_read
 * PDC_Cstr_lit_read              PDC_a_Cstr_lit_read            PDC_e_Cstr_lit_read
 * 
 * These char and string literal read functions all take an char or string to be
 * read specified in ASCII.  The char or string is converted to EBCDIC if one of
 * the EBCDIC forms is used or if one of the DEFAULT forms is used and
 * pdc->disc->def_charset is PDC_charset_EBCDIC.
 *
 * Mask flags control the behavior, as follows:
 *
 *
 * PDC_Test_SynCheck(*m)              PDC_Test_NoSynCheck(*m)
 * ---------------------------------   ------------------------------
 * If IO cursor points to specified    Always advance cursor by length
 * literal, advance cursor by length   of literal, regardless of what
 * of the literal and return PDC_OK,   cursor points to, and return
 * otherwise report error, do not      PDC_OK.
 * advance cursor, return PDC_ERR.
 * 
 * The error code used is either PDC_CHAR_LIT_NOT_FOUND or PDC_STR_LIT_NOT_FOUND.
 */

#ifdef FOR_CKIT
#if PDC_CONFIG_READ_FUNCTIONS > 0

#if PDC_CONFIG_A_CHAR_STRING > 0
PDC_error_t PDC_a_char_lit_read(PDC_t *pdc, const PDC_base_m *m,
				PDC_base_pd *pd, PDC_char c);
PDC_error_t PDC_a_str_lit_read (PDC_t *pdc, const PDC_base_m *m,
			        PDC_base_pd *pd, const PDC_string *s);
PDC_error_t PDC_a_Cstr_lit_read(PDC_t *pdc, const PDC_base_m *m,
				PDC_base_pd *pd, const char *s);
#endif

#if PDC_CONFIG_E_CHAR_STRING > 0
PDC_error_t PDC_e_char_lit_read(PDC_t *pdc, const PDC_base_m *m,
				PDC_base_pd *pd, PDC_char c);
PDC_error_t PDC_e_str_lit_read (PDC_t *pdc, const PDC_base_m *m,
			        PDC_base_pd *pd, const PDC_string *s);
PDC_error_t PDC_e_Cstr_lit_read(PDC_t *pdc, const PDC_base_m *m,
				PDC_base_pd *pd, const char *s);
#endif

#if PDC_CONFIG_A_CHAR_STRING > 0 && PDC_CONFIG_E_CHAR_STRING > 0
PDC_error_t PDC_char_lit_read  (PDC_t *pdc, const PDC_base_m *m,
			        PDC_base_pd *pd, PDC_char c);
PDC_error_t PDC_str_lit_read   (PDC_t *pdc, const PDC_base_m *m,
			        PDC_base_pd *pd, const PDC_string *s);
PDC_error_t PDC_Cstr_lit_read  (PDC_t *pdc, const PDC_base_m *m,
			        PDC_base_pd *pd, const char *s);
#endif

#endif /* PDC_CONFIG_READ_FUNCTIONS */
#endif /* FOR_CKIT */

/* ================================================================================
 * CHAR COUNTING FUNCTIONS
 *
 * DEFAULT                        ASCII                          EBCDIC
 * -----------------------------  -----------------------------  -----------------------------
 * PDC_countX                     PDC_a_countX                   PDC_e_countX
 * PDC_countXtoY                  PDC_a_countXtoY                PDC_e_countXtoY
 *
 * countX counts occurrences of char x between the current IO cursor and the
 * first EOR or EOF, while countXtoY counts occurrences of x between the current
 * IO cursor and the first occurrence of char y.  x and y are always specified
 * as ASCII chars.  They are converted to EBCDIC if the EBCDIC form is used or
 * if the default form is used and pdc->disc->def->charset is
 * PDC_charset_EBCDIC.
 *
 * For countX, if param eor_required is non-zero, then encountering EOF
 * before EOR produces an error.
 *
 * These functions do not change the IO cursor position.
 *
 * countX outcomes:
 *   1. IO cursor is already at EOF and eor_required is non-zero:
 *     => If !m || *m < PDC_Ignore:
 *           + pd->errCode set to PDC_AT_EOF
 *           + pd->loc begin/end set to EOF 'location'
 *     PDC_ERR returned   
 *   2. EOF is encountered before EOR and eor_required is non-zero
 *     => If !m || *m < PDC_Ignore:
 *           + pd->errCode set to PDC_EOF_BEFORE_EOR
 *           + pd->loc begin/end set to current IO cursor location
 *     PDC_ERR returned   
 *   3. EOR is encountered, or EOF is encounterd and eor_required is zero
 *     if res_out, *res_out is set to the number of occurrences of x
 *     from the IO cursor to EOR/EOF.
 *     PDC_OK returned
 *
 * countXtoY outcomes:
 *   1. IO cursor is already at EOF
 *     => If !m || *m < PDC_Ignore:
 *           + pd->errCode set to PDC_AT_EOF
 *           + pd->loc begin/end set to EOF 'location'
 *     PDC_ERR returned   
 *   2. y is not found
 *     => If !m || *m < PDC_Ignore:
 *           + pd->errCode set to PDC_CHAR_LIT_NOT_FOUND
 *           + pd->loc begin/end set to current IO cursor location
 *     PDC_ERR returned   
 *   3. Char y is found
 *     if res_out, *res_out is set to the number of occurrences of x
 *     from the IO cursor to first y.
 *     PDC_OK returned
 *
 */

#ifdef FOR_CKIT
#if PDC_CONFIG_READ_FUNCTIONS > 0

#if PDC_CONFIG_A_CHAR_STRING > 0
PDC_error_t PDC_a_countX   (PDC_t *pdc, const PDC_base_m *m, PDC_uint8 x, int eor_required,
			    PDC_base_pd *pd, PDC_int32 *res_out);
PDC_error_t PDC_a_countXtoY(PDC_t *pdc, const PDC_base_m *m, PDC_uint8 x, PDC_uint8 y,
			    PDC_base_pd *pd, PDC_int32 *res_out);
#endif

#if PDC_CONFIG_E_CHAR_STRING > 0
PDC_error_t PDC_e_countX   (PDC_t *pdc, const PDC_base_m *m, PDC_uint8 x, int eor_required,
			    PDC_base_pd *pd, PDC_int32 *res_out);
PDC_error_t PDC_e_countXtoY(PDC_t *pdc, const PDC_base_m *m, PDC_uint8 x, PDC_uint8 y,
			    PDC_base_pd *pd, PDC_int32 *res_out);
#endif

#if PDC_CONFIG_A_CHAR_STRING > 0 && PDC_CONFIG_E_CHAR_STRING > 0
PDC_error_t PDC_countX     (PDC_t *pdc, const PDC_base_m *m, PDC_uint8 x, int eor_required,
		            PDC_base_pd *pd, PDC_int32 *res_out);
PDC_error_t PDC_countXtoY  (PDC_t *pdc, const PDC_base_m *m, PDC_uint8 x, PDC_uint8 y,
		            PDC_base_pd *pd, PDC_int32 *res_out);
#endif

#endif /* PDC_CONFIG_READ_FUNCTIONS */
#endif /* FOR_CKIT */

/* ================================================================================
 * CHAR READ FUNCTIONS
 * 
 * DEFAULT                        ASCII                          EBCDIC
 * -----------------------------  -----------------------------  -----------------------------
 * PDC_char_read                  PDC_a_char_read                PDC_e_char_read
 *
 * Read a single character.  The in-memory result is always an ASCII character.
 * A conversion fom EBCDIC to ASCII occurs if the EBCDIC form is used or if the DEFAULT
 * form is used and pdc->disc->def_charset is PDC_charset_EBCDIC.
 *
 *   If *m is PDC_Ignore or PDC_Check, simply skips one byte and returns PDC_OK.
 *   If *m is PDC_CheckAndSet, sets (*c_out) to the byte at the current IO position
 *   and advances one byte.
 *
 *   If a char is not available, the IO cursor is not advanced, and
 *    if !m || *m < PDC_Ignore:
 *        + pd->errCode set to PDC_WIDTH_NOT_AVAILABLE
 *        + pd->loc begin/end set to the current IO position
 */

#ifdef FOR_CKIT
#if PDC_CONFIG_READ_FUNCTIONS > 0

#if PDC_CONFIG_A_CHAR_STRING > 0
PDC_error_t PDC_a_char_read (PDC_t *pdc, const PDC_base_m *m, PDC_base_pd *pd, PDC_char *c_out);
#endif

#if PDC_CONFIG_E_CHAR_STRING > 0
PDC_error_t PDC_e_char_read (PDC_t *pdc, const PDC_base_m *m, PDC_base_pd *pd, PDC_char *c_out);
#endif

#if PDC_CONFIG_A_CHAR_STRING > 0 && PDC_CONFIG_E_CHAR_STRING > 0
PDC_error_t PDC_char_read   (PDC_t *pdc, const PDC_base_m *m, PDC_base_pd *pd, PDC_char *c_out);
#endif

#endif /* PDC_CONFIG_READ_FUNCTIONS */
#endif /* FOR_CKIT */

/* ================================================================================
 * STRING READ FUNCTIONS
 *
 * DEFAULT                        ASCII                          EBCDIC
 * -----------------------------  -----------------------------  -----------------------------
 * PDC_string_FW_read             PDC_a_string_FW_read           PDC_e_string_FW_read
 * PDC_string_read                PDC_a_string_read              PDC_e_string_read
 * PDC_string_ME_read             PDC_a_string_ME_read           PDC_e_string_ME_read
 * PDC_string_CME_read            PDC_a_string_CME_read          PDC_e_string_CME_read
 * PDC_string_SE_read             PDC_a_string_SE_read           PDC_e_string_SE_read
 * PDC_string_CSE_read            PDC_a_string_CSE_read          PDC_e_string_CSE_read
 *
 * The string read functions each has a different way of specifying
 * the extent of the string:
 *   + all string_FW_read functions specify a fixed width
 *   + all string_read functions specify a single stop character
 *       (can be 0 to specify eof as the stop character)
 *   + all string_ME_read and string_CME_read functions specify a Match Extpression
 *       (string includes all chars that match)
 *   + all string_SE_read and string_CSE_read specify a Stop Expression
 *       (string terminated by encountering 'stop chars' that match)
 *
 * The ME/SE functions take a string containing a regular expression, while the CME/CSE
 * functions take a compiled form of regular expression (see PDC_regexp_compile).
 *
 * stop chars and regular expressions are specified using ASCII, but reading/matching occurs
 * using converted EBCDIC forms if an EBCDIC form is used or if a DEFAULT form is used
 * and pdc->disc->def_charset is PDC_charset_EBCDIC.
 * 
 * For all stop cases, the stop char/chars are not included in the
 * resulting string.  Note that if the IO cursor is already at a stop
 * condition, then a string of length zero results.
 *
 * If an expected stop char/pattern/width is found, PDC_OK is returned.
 * If !m || *m == PDC_CheckAndSet, then:
 *   + if s_out is non-null it is set to contain an in-memory string.
 *     If the original data is ASCII, then s_out will either share the string or contain a
 *     copy of the string, depending on pdc->disc->copy_strings.  If the original data is
 *     EBCDIC, s_out always contains a copy of the string that has been converted to ASCII.
 *     N.B. : (*s_out) should have been initialized
 *     at some point prior using PDC_string_init (it can be initialized once
 *     and re-used in string read calls many times).
 * 
 * Cleanup note: If copy_strings is non-zero, the memory allocated by *s_out should
 *               ultimately be freed using PDC_string_cleanup.
 *
 * If an expected stop condition is not encountered, the
 * IO cursor position is unchanged.  Error codes used:
 *     PDC_WIDTH_NOT_AVAILABLE
 *     PDC_STOPCHAR_NOT_FOUND
 *     PDC_STOPREGEXP_NOT_FOUND
 *     PDC_INVALID_REGEXP
 * 
 * EBCDIC Example: passing '|' (vertical bar, which is code 124 in ASCII) to
 * PDC_e_string_read as the stop char will result in a search for the EBCDIC
 * encoding of vertical bar (code 79 in EBCDIC), and (*s_out) will be a string
 * containing all EBCDIC chars between the IO cursor and the EBCDIC vertical
 * bar, with each cahr converted to ASCII. 
 */

#ifdef FOR_CKIT
#if PDC_CONFIG_READ_FUNCTIONS > 0

#if PDC_CONFIG_A_CHAR_STRING > 0
PDC_error_t PDC_a_string_FW_read (PDC_t *pdc, const PDC_base_m *m, size_t width,
				  PDC_base_pd *pd, PDC_string *s_out);
PDC_error_t PDC_a_string_read    (PDC_t *pdc, const PDC_base_m *m, PDC_char stopChar,
			          PDC_base_pd *pd, PDC_string *s_out);
PDC_error_t PDC_a_string_ME_read (PDC_t *pdc, const PDC_base_m *m, const char *matchRegexp,
				  PDC_base_pd *pd, PDC_string *s_out);
PDC_error_t PDC_a_string_CME_read(PDC_t *pdc, const PDC_base_m *m, PDC_regexp_t *matchRegexp,
				  PDC_base_pd *pd, PDC_string *s_out);
PDC_error_t PDC_a_string_SE_read (PDC_t *pdc, const PDC_base_m *m, const char *stopRegexp,
				  PDC_base_pd *pd, PDC_string *s_out);
PDC_error_t PDC_a_string_CSE_read(PDC_t *pdc, const PDC_base_m *m, PDC_regexp_t *stopRegexp,
				  PDC_base_pd *pd, PDC_string *s_out);
#endif

#if PDC_CONFIG_E_CHAR_STRING > 0
PDC_error_t PDC_e_string_FW_read (PDC_t *pdc, const PDC_base_m *m, size_t width,
				  PDC_base_pd *pd, PDC_string *s_out);
PDC_error_t PDC_e_string_read    (PDC_t *pdc, const PDC_base_m *m, PDC_char stopChar,
			          PDC_base_pd *pd, PDC_string *s_out);
PDC_error_t PDC_e_string_ME_read (PDC_t *pdc, const PDC_base_m *m, const char *matchRegexp,
				  PDC_base_pd *pd, PDC_string *s_out);
PDC_error_t PDC_e_string_CME_read(PDC_t *pdc, const PDC_base_m *m, PDC_regexp_t *matchRegexp,
				  PDC_base_pd *pd, PDC_string *s_out);
PDC_error_t PDC_e_string_SE_read (PDC_t *pdc, const PDC_base_m *m, const char *stopRegexp,
				  PDC_base_pd *pd, PDC_string *s_out);
PDC_error_t PDC_e_string_CSE_read(PDC_t *pdc, const PDC_base_m *m, PDC_regexp_t *stopRegexp,
				  PDC_base_pd *pd, PDC_string *s_out);
#endif

#if PDC_CONFIG_A_CHAR_STRING > 0 && PDC_CONFIG_E_CHAR_STRING > 0
PDC_error_t PDC_string_FW_read   (PDC_t *pdc, const PDC_base_m *m, size_t width,
				  PDC_base_pd *pd, PDC_string *s_out);
PDC_error_t PDC_string_read      (PDC_t *pdc, const PDC_base_m *m, PDC_char stopChar,
			          PDC_base_pd *pd, PDC_string *s_out);
PDC_error_t PDC_string_ME_read   (PDC_t *pdc, const PDC_base_m *m, const char *matchRegexp,
				  PDC_base_pd *pd, PDC_string *s_out);
PDC_error_t PDC_string_CME_read  (PDC_t *pdc, const PDC_base_m *m, PDC_regexp_t *matchRegexp,
				  PDC_base_pd *pd, PDC_string *s_out);
PDC_error_t PDC_string_SE_read   (PDC_t *pdc, const PDC_base_m *m, const char *stopRegexp,
				  PDC_base_pd *pd, PDC_string *s_out);
PDC_error_t PDC_string_CSE_read  (PDC_t *pdc, const PDC_base_m *m, PDC_regexp_t *stopRegexp,
				  PDC_base_pd *pd, PDC_string *s_out);
#endif

#endif /* PDC_CONFIG_READ_FUNCTIONS */
#endif /* FOR_CKIT */


/* ================================================================================
 * DATE/TIME READ FUNCTIONS
 *
 * DEFAULT                        ASCII                          EBCDIC
 * -----------------------------  -----------------------------  -----------------------------
 * PDC_date_read                  PDC_a_date_read                PDC_e_date_read
 *
 * Attempts to read a date string and convert it to seconds since the epoch.
 * For the different date formats supported, see the libast tmdate
 * documentation.  These read functions take a stop character, which is always
 * specified in ASCII.  It is converted to EBCDIC and the data is read as
 * EBCDIC chars if the EBCDIC form is used or if the DEFAULT form is used and
 * pdc->disc->def_charset is PDC_charset_EBCDIC.  Otherwise the data is read as
 * ASCII chars.
 *
 * If the current IO cursor position points to a valid date string:
 *   + if res_out, sets *res_out to the resulting date in seconds since the epoch
 *   + advances the IO cursor position to just after the last legal character
 *     in the date string
 *   + returns PDC_OK
 * Otherwise:
 *   + does not advance the IO cursor pos
 *   + returns PDC_ERR */

#ifdef FOR_CKIT
#if PDC_CONFIG_READ_FUNCTIONS > 0

#if PDC_CONFIG_A_CHAR_STRING > 0
PDC_error_t PDC_a_date_read(PDC_t *pdc, const PDC_base_m *m, PDC_char stopChar,
			    PDC_base_pd *pd, PDC_uint32 *res_out);
#endif

#if PDC_CONFIG_E_CHAR_STRING > 0
PDC_error_t PDC_e_date_read(PDC_t *pdc, const PDC_base_m *m, PDC_char stopChar,
			    PDC_base_pd *pd, PDC_uint32 *res_out);
#endif

#if PDC_CONFIG_A_CHAR_STRING > 0 && PDC_CONFIG_E_CHAR_STRING > 0
PDC_error_t PDC_date_read  (PDC_t *pdc, const PDC_base_m *m, PDC_char stopChar,
			    PDC_base_pd *pd, PDC_uint32 *res_out);
#endif

#endif /* PDC_CONFIG_READ_FUNCTIONS */
#endif /* FOR_CKIT */

/* ================================================================================
 * ASCII STRING TO INTEGER READ FUNCTIONS
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
 *   + if !m || *m == PDC_CheckAndSet, the out param is assigned a value
 *
 * PDC_ERR is returned on error.
 * Cursor advancement/err settings for different error cases:
 *
 * (1) If IO cursor is at EOF
 *     => IO cursor remains at EOF
 *     => If !m || *m < PDC_Ignore:
 *           + pd->errCode set to PDC_AT_EOF
 *           + pd->loc begin/end set to EOF 'location'
 * (2a) There is leading white space and not (disc flags & PDC_WSPACE_OK)
 * (2b) The target is unsigned and the first char is a -
 * (2c) The first character is not a +, -, or in [0-9]
 * (2d) First character is allowable + or -, following by a char that is not a digit
 * For the above 4 cases:
 *     => IO cursor is not advanced
 *     => If !m || *m < PDC_Ignore:
 *          + pd->errCode set to PDC_INVALID_A_NUM
 *          + pd->loc begin/end set to the IO cursor position.
 * (3) A valid ascii integer string is found, but it describes
 *     an integer that does not fit in the specified target type
 *     => IO cursor is advanced just beyond the last digit
 *     => If !m || *m < PDC_Ignore:
 *          + pd->errCode set to PDC_RANGE
 *          + pd->loc begin/end set to elt/char position of start and end of the ascii integer
 */

#if PDC_CONFIG_READ_FUNCTIONS > 0

#if PDC_CONFIG_A_INT > 0
PDC_error_t PDC_a_int8_read (PDC_t *pdc, const PDC_base_m *m,
			     PDC_base_pd *pd, PDC_int8 *res_out);

PDC_error_t PDC_a_int16_read(PDC_t *pdc, const PDC_base_m *m,
			     PDC_base_pd *pd, PDC_int16 *res_out);

PDC_error_t PDC_a_int32_read(PDC_t *pdc, const PDC_base_m *m,
			     PDC_base_pd *pd, PDC_int32 *res_out);

PDC_error_t PDC_a_int64_read(PDC_t *pdc, const PDC_base_m *m,
			     PDC_base_pd *pd, PDC_int64 *res_out);


PDC_error_t PDC_a_uint8_read (PDC_t *pdc, const PDC_base_m *m,
			      PDC_base_pd *pd, PDC_uint8 *res_out);

PDC_error_t PDC_a_uint16_read(PDC_t *pdc, const PDC_base_m *m,
			      PDC_base_pd *pd, PDC_uint16 *res_out);

PDC_error_t PDC_a_uint32_read(PDC_t *pdc, const PDC_base_m *m,
			      PDC_base_pd *pd, PDC_uint32 *res_out);

PDC_error_t PDC_a_uint64_read(PDC_t *pdc, const PDC_base_m *m,
			      PDC_base_pd *pd, PDC_uint64 *res_out);
#endif

#endif /* PDC_CONFIG_READ_FUNCTIONS */

/*
 * Fixed-width ascii integer read functions:
 *    Like the above, only a fixed width in input characters is specified, and
 *    only those characters are examined.  E.g., input '11112222' could be used
 *    to read two fixed-width ascii integers of width 4.
 *
 * N.B. The APIs require width > 0.  If width <= 0 is given, an immediate error 
 * return occurs, without setting pd's location or error code.
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
 *    In this case if !m || *m < PDC_Ignore:
 *       + pd->loc begin/end is set to the first/last char of the fixed-width field. 
 *
 *    If the specified width is *not* available (EOR/EOF hit), IO cursor is not advanced and
 *      if !m || *m < PDC_Ignore:
 *        + pd->errCode set to PDC_WIDTH_NOT_AVAILABLE
 *        + pd->loc begin/end set to elt/char position of start/end of the 'too small' field
 */

#if PDC_CONFIG_READ_FUNCTIONS > 0

#if PDC_CONFIG_A_INT_FW > 0
PDC_error_t PDC_a_int8_FW_read (PDC_t *pdc, const PDC_base_m *m, size_t width,
				PDC_base_pd *pd, PDC_int8 *res_out);

PDC_error_t PDC_a_int16_FW_read(PDC_t *pdc, const PDC_base_m *m, size_t width,
				PDC_base_pd *pd, PDC_int16 *res_out);

PDC_error_t PDC_a_int32_FW_read(PDC_t *pdc, const PDC_base_m *m, size_t width,
				PDC_base_pd *pd, PDC_int32 *res_out);

PDC_error_t PDC_a_int64_FW_read(PDC_t *pdc, const PDC_base_m *m, size_t width,
				PDC_base_pd *pd, PDC_int64 *res_out);


PDC_error_t PDC_a_uint8_FW_read (PDC_t *pdc, const PDC_base_m *m, size_t width,
				 PDC_base_pd *pd, PDC_uint8 *res_out);

PDC_error_t PDC_a_uint16_FW_read(PDC_t *pdc, const PDC_base_m *m, size_t width,
				 PDC_base_pd *pd, PDC_uint16 *res_out);

PDC_error_t PDC_a_uint32_FW_read(PDC_t *pdc, const PDC_base_m *m, size_t width,
				 PDC_base_pd *pd, PDC_uint32 *res_out);

PDC_error_t PDC_a_uint64_FW_read(PDC_t *pdc, const PDC_base_m *m, size_t width,
				 PDC_base_pd *pd, PDC_uint64 *res_out);
#endif

#endif /* PDC_CONFIG_READ_FUNCTIONS */

/* ================================================================================
 * EBCDIC STRING TO INTEGER READ FUNCTIONS
 *
 * These functions are just like their ASCII counterparts; the only
 * difference is the integers are encoding using EBCDIC string data.
 * The error codes used are also the same,
 * except that error code PDC_INVALID_E_NUM is used rather 
 * than PDC_INVALID_A_NUM
 */

#if PDC_CONFIG_READ_FUNCTIONS > 0

#if PDC_CONFIG_E_INT > 0
PDC_error_t PDC_e_int8_read (PDC_t *pdc, const PDC_base_m *m,
			     PDC_base_pd *pd, PDC_int8 *res_out);

PDC_error_t PDC_e_int16_read(PDC_t *pdc, const PDC_base_m *m,
			     PDC_base_pd *pd, PDC_int16 *res_out);

PDC_error_t PDC_e_int32_read(PDC_t *pdc, const PDC_base_m *m,
			     PDC_base_pd *pd, PDC_int32 *res_out);

PDC_error_t PDC_e_int64_read(PDC_t *pdc, const PDC_base_m *m,
			     PDC_base_pd *pd, PDC_int64 *res_out);

PDC_error_t PDC_e_uint8_read (PDC_t *pdc, const PDC_base_m *m,
			      PDC_base_pd *pd, PDC_uint8 *res_out);

PDC_error_t PDC_e_uint16_read(PDC_t *pdc, const PDC_base_m *m,
			      PDC_base_pd *pd, PDC_uint16 *res_out);

PDC_error_t PDC_e_uint32_read(PDC_t *pdc, const PDC_base_m *m,
			      PDC_base_pd *pd, PDC_uint32 *res_out);

PDC_error_t PDC_e_uint64_read(PDC_t *pdc, const PDC_base_m *m,
			      PDC_base_pd *pd, PDC_uint64 *res_out);
#endif

#if PDC_CONFIG_E_INT_FW > 0
PDC_error_t PDC_e_int8_FW_read (PDC_t *pdc, const PDC_base_m *m, size_t width,
				PDC_base_pd *pd, PDC_int8 *res_out);

PDC_error_t PDC_e_int16_FW_read(PDC_t *pdc, const PDC_base_m *m, size_t width,
				PDC_base_pd *pd, PDC_int16 *res_out);

PDC_error_t PDC_e_int32_FW_read(PDC_t *pdc, const PDC_base_m *m, size_t width,
				PDC_base_pd *pd, PDC_int32 *res_out);

PDC_error_t PDC_e_int64_FW_read(PDC_t *pdc, const PDC_base_m *m, size_t width,
				PDC_base_pd *pd, PDC_int64 *res_out);

PDC_error_t PDC_e_uint8_FW_read (PDC_t *pdc, const PDC_base_m *m, size_t width,
				 PDC_base_pd *pd, PDC_uint8 *res_out);

PDC_error_t PDC_e_uint16_FW_read(PDC_t *pdc, const PDC_base_m *m, size_t width,
				 PDC_base_pd *pd, PDC_uint16 *res_out);

PDC_error_t PDC_e_uint32_FW_read(PDC_t *pdc, const PDC_base_m *m, size_t width,
				 PDC_base_pd *pd, PDC_uint32 *res_out);

PDC_error_t PDC_e_uint64_FW_read(PDC_t *pdc, const PDC_base_m *m, size_t width,
				 PDC_base_pd *pd, PDC_uint64 *res_out);
#endif

#endif /* PDC_CONFIG_READ_FUNCTIONS */

/* ================================================================================
 * DEFAULT STRING TO INTEGER READ FUNCTIONS
 *
 * These functions select the appropriate ASCII or EBCDIC string to integer
 * function based on pdc->disc->def_charset.
 *
 * Example: the call 
 *
 *     PDC_int8_read(pdc, &m, &ed, *res)
 *
 * is converted to one of these forms:
 *
 *     PDC_a_int8_read(pdc, &m, &ed, *res)
 *     PDC_e_int8_read(pdc, &m, &ed, *res)
 *     etc.
 */

#ifdef FOR_CKIT
#if PDC_CONFIG_READ_FUNCTIONS > 0

#if PDC_CONFIG_A_INT > 0 && PDC_CONFIG_E_INT > 0
PDC_error_t PDC_int8_read (PDC_t *pdc, const PDC_base_m *m,
			    PDC_base_pd *pd, PDC_int8 *res_out);
PDC_error_t PDC_int16_read(PDC_t *pdc, const PDC_base_m *m,
			   PDC_base_pd *pd, PDC_int16 *res_out);
PDC_error_t PDC_int32_read(PDC_t *pdc, const PDC_base_m *m,
			   PDC_base_pd *pd, PDC_int32 *res_out);
PDC_error_t PDC_int64_read(PDC_t *pdc, const PDC_base_m *m,
			   PDC_base_pd *pd, PDC_int64 *res_out);
PDC_error_t PDC_uint8_read (PDC_t *pdc, const PDC_base_m *m,
			    PDC_base_pd *pd, PDC_uint8 *res_out);
PDC_error_t PDC_uint16_read(PDC_t *pdc, const PDC_base_m *m,
			    PDC_base_pd *pd, PDC_uint16 *res_out);
PDC_error_t PDC_uint32_read(PDC_t *pdc, const PDC_base_m *m,
			    PDC_base_pd *pd, PDC_uint32 *res_out);
PDC_error_t PDC_uint64_read(PDC_t *pdc, const PDC_base_m *m,
			    PDC_base_pd *pd, PDC_uint64 *res_out);
#endif

#if PDC_CONFIG_A_INT_FW > 0 && PDC_CONFIG_E_INT_FW > 0
PDC_error_t PDC_int8_FW_read (PDC_t *pdc, const PDC_base_m *m, size_t width,
			      PDC_base_pd *pd, PDC_int8 *res_out);
PDC_error_t PDC_int16_FW_read(PDC_t *pdc, const PDC_base_m *m, size_t width,
			      PDC_base_pd *pd, PDC_int16 *res_out);
PDC_error_t PDC_int32_FW_read(PDC_t *pdc, const PDC_base_m *m, size_t width,
			      PDC_base_pd *pd, PDC_int32 *res_out);
PDC_error_t PDC_int64_FW_read(PDC_t *pdc, const PDC_base_m *m, size_t width,
			      PDC_base_pd *pd, PDC_int64 *res_out);
PDC_error_t PDC_uint8_FW_read (PDC_t *pdc, const PDC_base_m *m, size_t width,
			       PDC_base_pd *pd, PDC_uint8 *res_out);
PDC_error_t PDC_uint16_FW_read(PDC_t *pdc, const PDC_base_m *m, size_t width,
			       PDC_base_pd *pd, PDC_uint16 *res_out);
PDC_error_t PDC_uint32_FW_read(PDC_t *pdc, const PDC_base_m *m, size_t width,
			       PDC_base_pd *pd, PDC_uint32 *res_out);
PDC_error_t PDC_uint64_FW_read(PDC_t *pdc, const PDC_base_m *m, size_t width,
#endif
			       PDC_base_pd *pd, PDC_uint64 *res_out);
#endif /* PDC_CONFIG_READ_FUNCTIONS */
#endif /* FOR_CKIT */

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
 *    if !m || *m < PDC_Ignore:
 *        + pd->errCode set to PDC_WIDTH_NOT_AVAILABLE
 *        + pd->loc begin/end set to elt/char position of start/end of the 'too small' field
 */

#if PDC_CONFIG_READ_FUNCTIONS > 0

#if PDC_CONFIG_B_INT > 0
PDC_error_t PDC_b_int8_read (PDC_t *pdc, const PDC_base_m *m,
			     PDC_base_pd *pd, PDC_int8 *res_out);

PDC_error_t PDC_b_int16_read(PDC_t *pdc, const PDC_base_m *m,
			     PDC_base_pd *pd, PDC_int16 *res_out);

PDC_error_t PDC_b_int32_read(PDC_t *pdc, const PDC_base_m *m,
			     PDC_base_pd *pd, PDC_int32 *res_out);

PDC_error_t PDC_b_int64_read(PDC_t *pdc, const PDC_base_m *m,
			     PDC_base_pd *pd, PDC_int64 *res_out);

PDC_error_t PDC_b_uint8_read (PDC_t *pdc, const PDC_base_m *m,
			      PDC_base_pd *pd, PDC_uint8 *res_out);

PDC_error_t PDC_b_uint16_read(PDC_t *pdc, const PDC_base_m *m,
			      PDC_base_pd *pd, PDC_uint16 *res_out);

PDC_error_t PDC_b_uint32_read(PDC_t *pdc, const PDC_base_m *m,
			      PDC_base_pd *pd, PDC_uint32 *res_out);

PDC_error_t PDC_b_uint64_read(PDC_t *pdc, const PDC_base_m *m,
			      PDC_base_pd *pd, PDC_uint64 *res_out);
#endif

#endif /* PDC_CONFIG_READ_FUNCTIONS */

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
 *    if !m || *m < PDC_Ignore:
 *        + pd->errCode set to PDC_WIDTH_NOT_AVAILABLE
 *        + pd->loc begin/end set to elt/char position of start/end of the 'too small' field
 *
 * Otherwise, the IO cursor is always advanced.  There are 3 error cases that
 * can occur even though the IO cursor advances:
 *
 * If num_digits or num_bytes is not a legal choice for the target type and
 * sign of the value:
 *    if !m || *m < PDC_Ignore:
 *          + pd->errCode set to PDC_BAD_PARAM
 *
 * If the specified bytes make up an integer that does not fit in the target type,
 * or if the actual value is not in the min/max range, then:
 *    if !m || *m < PDC_Ignore:
 *          + pd->errCode set to PDC_RANGE
 *
 * If the specified bytes are not legal EBC/BCD integer bytes, then 
 *    if !m || *m < PDC_Ignore:
 *          + pd->errCode set to one of:
 *                PDC_INVALID_EBC_NUM
 *                PDC_INVALID_BCD_NUM
 */

#if PDC_CONFIG_READ_FUNCTIONS > 0

#if PDC_CONFIG_EBC_INT > 0  || PDC_CONFIG_EBC_FPOINT > 0
PDC_error_t PDC_ebc_int8_read   (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_digits,
				 PDC_base_pd *pd, PDC_int8 *res_out);
PDC_error_t PDC_ebc_int16_read  (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_digits,
				 PDC_base_pd *pd, PDC_int16 *res_out);
PDC_error_t PDC_ebc_int32_read  (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_digits,
				 PDC_base_pd *pd, PDC_int32 *res_out);
PDC_error_t PDC_ebc_int64_read  (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_digits,
				 PDC_base_pd *pd, PDC_int64 *res_out);

PDC_error_t PDC_ebc_uint8_read  (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_digits,
				 PDC_base_pd *pd, PDC_uint8 *res_out);
PDC_error_t PDC_ebc_uint16_read (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_digits,
				 PDC_base_pd *pd, PDC_uint16 *res_out);
PDC_error_t PDC_ebc_uint32_read (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_digits,
				 PDC_base_pd *pd, PDC_uint32 *res_out);
PDC_error_t PDC_ebc_uint64_read (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_digits,
				 PDC_base_pd *pd, PDC_uint64 *res_out);
#endif

#if PDC_CONFIG_BCD_INT > 0 || PDC_CONFIG_BCD_FPOINT > 0
PDC_error_t PDC_bcd_int8_read   (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_digits,
				 PDC_base_pd *pd, PDC_int8 *res_out);
PDC_error_t PDC_bcd_int16_read  (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_digits,
				 PDC_base_pd *pd, PDC_int16 *res_out);
PDC_error_t PDC_bcd_int32_read  (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_digits,
				 PDC_base_pd *pd, PDC_int32 *res_out);
PDC_error_t PDC_bcd_int64_read  (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_digits,
				 PDC_base_pd *pd, PDC_int64 *res_out);

PDC_error_t PDC_bcd_uint8_read  (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_digits,
				 PDC_base_pd *pd, PDC_uint8 *res_out);
PDC_error_t PDC_bcd_uint16_read (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_digits,
				 PDC_base_pd *pd, PDC_uint16 *res_out);
PDC_error_t PDC_bcd_uint32_read (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_digits,
				 PDC_base_pd *pd, PDC_uint32 *res_out);
PDC_error_t PDC_bcd_uint64_read (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_digits,
				 PDC_base_pd *pd, PDC_uint64 *res_out);
#endif

#if PDC_CONFIG_SBL_INT > 0 || PDC_CONFIG_SBL_FPOINT > 0
PDC_error_t PDC_sbl_int8_read    (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_bytes,
			         PDC_base_pd *pd, PDC_int8 *res_out);
PDC_error_t PDC_sbl_int16_read   (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_bytes,
			         PDC_base_pd *pd, PDC_int16 *res_out);
PDC_error_t PDC_sbl_int32_read   (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_bytes,
			         PDC_base_pd *pd, PDC_int32 *res_out);
PDC_error_t PDC_sbl_int64_read   (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_bytes,
			         PDC_base_pd *pd, PDC_int64 *res_out);

PDC_error_t PDC_sbl_uint8_read   (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_bytes,
			 	 PDC_base_pd *pd, PDC_uint8 *res_out);
PDC_error_t PDC_sbl_uint16_read  (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_bytes,
			 	 PDC_base_pd *pd, PDC_uint16 *res_out);
PDC_error_t PDC_sbl_uint32_read  (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_bytes,
			 	 PDC_base_pd *pd, PDC_uint32 *res_out);
PDC_error_t PDC_sbl_uint64_read  (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_bytes,
			 	 PDC_base_pd *pd, PDC_uint64 *res_out);
#endif

#if PDC_CONFIG_SBH_INT > 0 || PDC_CONFIG_SBH_FPOINT > 0
PDC_error_t PDC_sbh_int8_read    (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_bytes,
			         PDC_base_pd *pd, PDC_int8 *res_out);
PDC_error_t PDC_sbh_int16_read   (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_bytes,
			         PDC_base_pd *pd, PDC_int16 *res_out);
PDC_error_t PDC_sbh_int32_read   (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_bytes,
			         PDC_base_pd *pd, PDC_int32 *res_out);
PDC_error_t PDC_sbh_int64_read   (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_bytes,
			         PDC_base_pd *pd, PDC_int64 *res_out);

PDC_error_t PDC_sbh_uint8_read   (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_bytes,
			 	 PDC_base_pd *pd, PDC_uint8 *res_out);
PDC_error_t PDC_sbh_uint16_read  (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_bytes,
			 	 PDC_base_pd *pd, PDC_uint16 *res_out);
PDC_error_t PDC_sbh_uint32_read  (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_bytes,
			 	 PDC_base_pd *pd, PDC_uint32 *res_out);
PDC_error_t PDC_sbh_uint64_read  (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_bytes,
			 	 PDC_base_pd *pd, PDC_uint64 *res_out);
#endif

#endif /* PDC_CONFIG_READ_FUNCTIONS */

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
 *    if !m || *m < PDC_Ignore:
 *        + pd->errCode set to PDC_WIDTH_NOT_AVAILABLE
 *        + pd->loc begin/end set to elt/char position of start/end of the 'too small' field
 *
 * Otherwise, the IO cursor is always advanced.  There are 3 error cases that
 * can occur even though the IO cursor advances:
 *
 * If num_digits, num_bytes, or d_exp is not in a legal choice for the target type
 * and sign of the value:
 *    if !m || *m < PDC_Ignore:
 *          + pd->errCode set to PDC_BAD_PARAM
 *
 * If the actual numerator is not in the min/max numerator range, then:
 *    if !m || *m < PDC_Ignore:
 *          + pd->errCode set to PDC_RANGE
 *
 * If the specified bytes are not legal EBC/BCD integer bytes, then 
 *    if !m || *m < PDC_Ignore:
 *          + pd->errCode set to one of:
 *                PDC_INVALID_EBC_NUM
 *                PDC_INVALID_BCD_NUM
 *
 */

#if PDC_CONFIG_READ_FUNCTIONS > 0

#if PDC_CONFIG_EBC_FPOINT > 0
PDC_error_t PDC_ebc_fpoint8_read   (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_digits, PDC_uint32 d_exp,
				    PDC_base_pd *pd, PDC_fpoint8 *res_out);
PDC_error_t PDC_ebc_fpoint16_read  (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_digits, PDC_uint32 d_exp,
				    PDC_base_pd *pd, PDC_fpoint16 *res_out);
PDC_error_t PDC_ebc_fpoint32_read  (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_digits, PDC_uint32 d_exp,
				    PDC_base_pd *pd, PDC_fpoint32 *res_out);
PDC_error_t PDC_ebc_fpoint64_read  (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_digits, PDC_uint32 d_exp,
				    PDC_base_pd *pd, PDC_fpoint64 *res_out);

PDC_error_t PDC_ebc_ufpoint8_read  (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_digits, PDC_uint32 d_exp,
				    PDC_base_pd *pd, PDC_ufpoint8 *res_out);
PDC_error_t PDC_ebc_ufpoint16_read (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_digits, PDC_uint32 d_exp,
				    PDC_base_pd *pd, PDC_ufpoint16 *res_out);
PDC_error_t PDC_ebc_ufpoint32_read (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_digits, PDC_uint32 d_exp,
				    PDC_base_pd *pd, PDC_ufpoint32 *res_out);
PDC_error_t PDC_ebc_ufpoint64_read (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_digits, PDC_uint32 d_exp,
				    PDC_base_pd *pd, PDC_ufpoint64 *res_out);
#endif

#if PDC_CONFIG_BCD_FPOINT > 0
PDC_error_t PDC_bcd_fpoint8_read   (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_digits, PDC_uint32 d_exp,
				    PDC_base_pd *pd, PDC_fpoint8 *res_out);
PDC_error_t PDC_bcd_fpoint16_read  (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_digits, PDC_uint32 d_exp,
				    PDC_base_pd *pd, PDC_fpoint16 *res_out);
PDC_error_t PDC_bcd_fpoint32_read  (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_digits, PDC_uint32 d_exp,
				    PDC_base_pd *pd, PDC_fpoint32 *res_out);
PDC_error_t PDC_bcd_fpoint64_read  (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_digits, PDC_uint32 d_exp,
				    PDC_base_pd *pd, PDC_fpoint64 *res_out);

PDC_error_t PDC_bcd_ufpoint8_read  (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_digits, PDC_uint32 d_exp,
				    PDC_base_pd *pd, PDC_ufpoint8 *res_out);
PDC_error_t PDC_bcd_ufpoint16_read (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_digits, PDC_uint32 d_exp,
				    PDC_base_pd *pd, PDC_ufpoint16 *res_out);
PDC_error_t PDC_bcd_ufpoint32_read (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_digits, PDC_uint32 d_exp,
				    PDC_base_pd *pd, PDC_ufpoint32 *res_out);
PDC_error_t PDC_bcd_ufpoint64_read (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_digits, PDC_uint32 d_exp,
				    PDC_base_pd *pd, PDC_ufpoint64 *res_out);
#endif

#if PDC_CONFIG_SBL_FPOINT > 0
PDC_error_t PDC_sbl_fpoint8_read    (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_bytes, PDC_uint32 d_exp,
				    PDC_base_pd *pd, PDC_fpoint8 *res_out);
PDC_error_t PDC_sbl_fpoint16_read   (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_bytes, PDC_uint32 d_exp,
				    PDC_base_pd *pd, PDC_fpoint16 *res_out);
PDC_error_t PDC_sbl_fpoint32_read   (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_bytes, PDC_uint32 d_exp,
				    PDC_base_pd *pd, PDC_fpoint32 *res_out);
PDC_error_t PDC_sbl_fpoint64_read   (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_bytes, PDC_uint32 d_exp,
				    PDC_base_pd *pd, PDC_fpoint64 *res_out);

PDC_error_t PDC_sbl_ufpoint8_read   (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_bytes, PDC_uint32 d_exp,
				    PDC_base_pd *pd, PDC_ufpoint8 *res_out);
PDC_error_t PDC_sbl_ufpoint16_read  (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_bytes, PDC_uint32 d_exp,
				    PDC_base_pd *pd, PDC_ufpoint16 *res_out);
PDC_error_t PDC_sbl_ufpoint32_read  (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_bytes, PDC_uint32 d_exp,
				    PDC_base_pd *pd, PDC_ufpoint32 *res_out);
PDC_error_t PDC_sbl_ufpoint64_read  (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_bytes, PDC_uint32 d_exp,
				    PDC_base_pd *pd, PDC_ufpoint64 *res_out);
#endif

#if PDC_CONFIG_SBH_FPOINT > 0
PDC_error_t PDC_sbh_fpoint8_read    (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_bytes, PDC_uint32 d_exp,
				    PDC_base_pd *pd, PDC_fpoint8 *res_out);
PDC_error_t PDC_sbh_fpoint16_read   (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_bytes, PDC_uint32 d_exp,
				    PDC_base_pd *pd, PDC_fpoint16 *res_out);
PDC_error_t PDC_sbh_fpoint32_read   (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_bytes, PDC_uint32 d_exp,
				    PDC_base_pd *pd, PDC_fpoint32 *res_out);
PDC_error_t PDC_sbh_fpoint64_read   (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_bytes, PDC_uint32 d_exp,
				    PDC_base_pd *pd, PDC_fpoint64 *res_out);

PDC_error_t PDC_sbh_ufpoint8_read   (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_bytes, PDC_uint32 d_exp,
				    PDC_base_pd *pd, PDC_ufpoint8 *res_out);
PDC_error_t PDC_sbh_ufpoint16_read  (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_bytes, PDC_uint32 d_exp,
				    PDC_base_pd *pd, PDC_ufpoint16 *res_out);
PDC_error_t PDC_sbh_ufpoint32_read  (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_bytes, PDC_uint32 d_exp,
				    PDC_base_pd *pd, PDC_ufpoint32 *res_out);
PDC_error_t PDC_sbh_ufpoint64_read  (PDC_t *pdc, const PDC_base_m *m, PDC_uint32 num_bytes, PDC_uint32 d_exp,
				    PDC_base_pd *pd, PDC_ufpoint64 *res_out);
#endif

#endif /* PDC_CONFIG_READ_FUNCTIONS */

/* ********************************************************************************
 * WRITE FUNCTIONS: GENERAL NOTES
 * ********************************************************************************
 * XXX_TODOC : discuss general issues writing to stream, to buffer
 */

/* ================================================================================
 * LITERAL WRITE FUNCTIONS
 *   Literal write functions: write a char or string to an Sfio stream or buffer.
 *   Typically used with a literal argument, as in 
 *      PDC_a_Cstr_lit_write(pdc, io, "hello");
 *   Note that these are similar to PDC_a_char and PDC_a_string write functions
 *   except there is no PDC_base_pd argument since literals have no errors.
 *
 * XXX_TODOC
 */

#ifdef FOR_CKIT
#if PDC_CONFIG_WRITE_FUNCTIONS > 0

#if PDC_CONFIG_A_CHAR_STRING > 0
ssize_t PDC_a_char_lit_write2io(PDC_t *pdc, Sfio_t *io, PDC_char c);
ssize_t PDC_a_str_lit_write2io (PDC_t *pdc, Sfio_t *io, const PDC_string *s);
ssize_t PDC_a_Cstr_lit_write2io(PDC_t *pdc, Sfio_t *io, const char *s);

ssize_t PDC_a_char_lit_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_char c);
ssize_t PDC_a_str_lit_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, const PDC_string *s);
ssize_t PDC_a_Cstr_lit_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, const char *s);
#endif

#if PDC_CONFIG_E_CHAR_STRING > 0
ssize_t PDC_e_char_lit_write2io(PDC_t *pdc, Sfio_t *io, PDC_char c);
ssize_t PDC_e_str_lit_write2io (PDC_t *pdc, Sfio_t *io, const PDC_string *s);
ssize_t PDC_e_Cstr_lit_write2io(PDC_t *pdc, Sfio_t *io, const char *s);

ssize_t PDC_e_char_lit_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_char c);
ssize_t PDC_e_str_lit_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, const PDC_string *s);
ssize_t PDC_e_Cstr_lit_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, const char *s);
#endif

#if PDC_CONFIG_A_CHAR_STRING > 0 && PDC_CONFIG_E_CHAR_STRING > 0
ssize_t PDC_char_lit_write2io(PDC_t *pdc, Sfio_t *io, PDC_char c);
ssize_t PDC_str_lit_write2io (PDC_t *pdc, Sfio_t *io, const PDC_string *s);
ssize_t PDC_Cstr_lit_write2io(PDC_t *pdc, Sfio_t *io, const char *s);

ssize_t PDC_char_lit_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_char c);
ssize_t PDC_str_lit_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, const PDC_string *s);
ssize_t PDC_Cstr_lit_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, const char *s);
#endif

#endif /* PDC_CONFIG_WRITE_FUNCTIONS */
#endif /* FOR_CKIT */

/* ================================================================================
 * CHAR WRITE FUNCTIONS
 * DEFAULT                        ASCII                          EBCDIC
 * -----------------------------  -----------------------------  -----------------------------
 * PDC_char_write2io              PDC_a_char_write2io            PDC_e_char_write2io
 *
 * PDC_char_write2buf             PDC_a_char_write2buf           PDC_e_char_write2buf
 */

#ifdef FOR_CKIT
#if PDC_CONFIG_WRITE_FUNCTIONS > 0

#if PDC_CONFIG_A_CHAR_STRING > 0
ssize_t PDC_a_char_write2io   (PDC_t *pdc, Sfio_t *io, PDC_base_pd *pd, PDC_char *c);
ssize_t PDC_a_char_write2buf  (PDC_t *pdc, PDC_byte *buf, size_t buf_len, ssize_t *buf_full, PDC_base_pd *pd, PDC_char *c);
#endif

#if PDC_CONFIG_E_CHAR_STRING > 0
ssize_t PDC_e_char_write2io   (PDC_t *pdc, Sfio_t *io, PDC_base_pd *pd, PDC_char *c);
ssize_t PDC_e_char_write2buf  (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_pd *pd, PDC_char *c);
#endif

#if PDC_CONFIG_A_CHAR_STRING > 0 && PDC_CONFIG_E_CHAR_STRING > 0
ssize_t PDC_char_write2io     (PDC_t *pdc, Sfio_t *io, PDC_base_pd *pd, PDC_char *c);
ssize_t PDC_char_write2buf    (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_pd *pd, PDC_char *c);
#endif

#endif /* PDC_CONFIG_WRITE_FUNCTIONS */
#endif /* FOR_CKIT */

/* ================================================================================
 * STRING WRITE FUNCTIONS
 * DEFAULT                        ASCII                          EBCDIC
 * -----------------------------  -----------------------------  -----------------------------
 * PDC_string_FW_write2io         PDC_a_string_FW_write2io       PDC_e_string_FW_write2io
 * PDC_string_write2io            PDC_a_string_write2io          PDC_e_string_write2io
 * PDC_string_ME_write2io         PDC_a_string_ME_write2io       PDC_e_string_ME_write2io
 * PDC_string_CME_write2io        PDC_a_string_CME_write2io      PDC_e_string_CME_write2io
 * PDC_string_SE_write2io         PDC_a_string_SE_write2io       PDC_e_string_SE_write2io
 * PDC_string_CSE_write2io        PDC_a_string_CSE_write2io      PDC_e_string_CSE_write2io
 *
 * PDC_string_FW_write2buf        PDC_a_string_FW_write2buf      PDC_e_string_FW_write2buf
 * PDC_string_write2buf           PDC_a_string_write2buf         PDC_e_string_write2buf
 * PDC_string_ME_write2buf        PDC_a_string_ME_write2buf      PDC_e_string_ME_write2buf
 * PDC_string_CME_write2buf       PDC_a_string_CME_write2buf     PDC_e_string_CME_write2buf
 * PDC_string_SE_write2buf        PDC_a_string_SE_write2buf      PDC_e_string_SE_write2buf
 * PDC_string_CSE_write2buf       PDC_a_string_CSE_write2buf     PDC_e_string_CSE_write2buf
 */

#ifdef FOR_CKIT
#if PDC_CONFIG_WRITE_FUNCTIONS > 0

#if PDC_CONFIG_A_CHAR_STRING > 0
ssize_t PDC_a_string_FW_write2io  (PDC_t *pdc, Sfio_t *io, size_t width, PDC_base_pd *pd, PDC_string *s);
ssize_t PDC_a_string_FW_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full,
				   size_t width, PDC_base_pd *pd, PDC_string *s);
ssize_t PDC_a_string_write2io     (PDC_t *pdc, Sfio_t *io, PDC_char stopChar, PDC_base_pd *pd, PDC_string *s);
ssize_t PDC_a_string_write2buf    (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_char stopChar, PDC_base_pd *pd, PDC_string *s);
ssize_t PDC_a_string_ME_write2io  (PDC_t *pdc, Sfio_t *io, const char *matchRegexp, PDC_base_pd *pd, PDC_string *s);
ssize_t PDC_a_string_ME_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full,
				   const char *matchRegexp, PDC_base_pd *pd, PDC_string *s);
ssize_t PDC_a_string_CME_write2io (PDC_t *pdc, Sfio_t *io, PDC_regexp_t *matchRegexp, PDC_base_pd *pd, PDC_string *s);
ssize_t PDC_a_string_CME_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full,
				   PDC_regexp_t *matchRegexp, PDC_base_pd *pd, PDC_string *s);
ssize_t PDC_a_string_SE_write2io  (PDC_t *pdc, Sfio_t *io, const char *stopRegexp, PDC_base_pd *pd, PDC_string *s);
ssize_t PDC_a_string_SE_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full,
				   const char *stopRegexp, PDC_base_pd *pd, PDC_string *s);
ssize_t PDC_a_string_CSE_write2io (PDC_t *pdc, Sfio_t *io, PDC_regexp_t *stopRegexp, PDC_base_pd *pd, PDC_string *s);
ssize_t PDC_a_string_CSE_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full,
				   PDC_regexp_t *stopRegexp, PDC_base_pd *pd, PDC_string *s);
#endif

#if PDC_CONFIG_E_CHAR_STRING > 0
ssize_t PDC_e_string_FW_write2io  (PDC_t *pdc, Sfio_t *io, size_t width, PDC_base_pd *pd, PDC_string *s);
ssize_t PDC_e_string_FW_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full,
				   size_t width, PDC_base_pd *pd, PDC_string *s);
ssize_t PDC_e_string_write2io     (PDC_t *pdc, Sfio_t *io, PDC_char stopChar, PDC_base_pd *pd, PDC_string *s);
ssize_t PDC_e_string_write2buf    (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_char stopChar, PDC_base_pd *pd, PDC_string *s);
ssize_t PDC_e_string_ME_write2io  (PDC_t *pdc, Sfio_t *io, const char *matchRegexp, PDC_base_pd *pd, PDC_string *s);
ssize_t PDC_e_string_ME_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full,
				   const char *matchRegexp, PDC_base_pd *pd, PDC_string *s);
ssize_t PDC_e_string_CME_write2io (PDC_t *pdc, Sfio_t *io, PDC_regexp_t *matchRegexp, PDC_base_pd *pd, PDC_string *s);
ssize_t PDC_e_string_CME_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full,
				   PDC_regexp_t *matchRegexp, PDC_base_pd *pd, PDC_string *s);
ssize_t PDC_e_string_SE_write2io  (PDC_t *pdc, Sfio_t *io, const char *stopRegexp, PDC_base_pd *pd, PDC_string *s);
ssize_t PDC_e_string_SE_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full,
				   const char *stopRegexp, PDC_base_pd *pd, PDC_string *s);
ssize_t PDC_e_string_CSE_write2io (PDC_t *pdc, Sfio_t *io, PDC_regexp_t *stopRegexp, PDC_base_pd *pd, PDC_string *s);
ssize_t PDC_e_string_CSE_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full,
				   PDC_regexp_t *stopRegexp, PDC_base_pd *pd, PDC_string *s);
#endif

#if PDC_CONFIG_A_CHAR_STRING > 0 && PDC_CONFIG_E_CHAR_STRING > 0
ssize_t PDC_string_FW_write2io    (PDC_t *pdc, Sfio_t *io, size_t width, PDC_base_pd *pd, PDC_string *s);
ssize_t PDC_string_FW_write2buf   (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full,
				   size_t width, PDC_base_pd *pd, PDC_string *s);
ssize_t PDC_string_write2io       (PDC_t *pdc, Sfio_t *io, PDC_char stopChar, PDC_base_pd *pd, PDC_string *s);
ssize_t PDC_string_write2buf      (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_char stopChar, PDC_base_pd *pd, PDC_string *s);
ssize_t PDC_string_ME_write2io    (PDC_t *pdc, Sfio_t *io, const char *matchRegexp, PDC_base_pd *pd, PDC_string *s);
ssize_t PDC_string_ME_write2buf   (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full,
				   const char *matchRegexp, PDC_base_pd *pd, PDC_string *s);
ssize_t PDC_string_CME_write2io   (PDC_t *pdc, Sfio_t *io, PDC_regexp_t *matchRegexp, PDC_base_pd *pd, PDC_string *s);
ssize_t PDC_string_CME_write2buf  (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full,
				   PDC_regexp_t *matchRegexp, PDC_base_pd *pd, PDC_string *s);
ssize_t PDC_string_SE_write2io    (PDC_t *pdc, Sfio_t *io, const char *stopRegexp, PDC_base_pd *pd, PDC_string *s);
ssize_t PDC_string_SE_write2buf   (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full,
				   const char *stopRegexp, PDC_base_pd *pd, PDC_string *s);
ssize_t PDC_string_CSE_write2io   (PDC_t *pdc, Sfio_t *io, PDC_regexp_t *stopRegexp, PDC_base_pd *pd, PDC_string *s);
ssize_t PDC_string_CSE_write2buf  (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full,
				   PDC_regexp_t *stopRegexp, PDC_base_pd *pd, PDC_string *s);
#endif

#endif /* PDC_CONFIG_WRITE_FUNCTIONS */
#endif /* FOR_CKIT */

/* ================================================================================
 * DATE WRITE FUNCTIONS
 * DEFAULT                        ASCII                          EBCDIC
 * -----------------------------  -----------------------------  -----------------------------
 * PDC_date_write2io              PDC_a_date_write2io            PDC_e_date_write2io 
 *
 * PDC_date_write2buf             PDC_a_date_write2buf           PDC_e_date_write2buf
 */

#ifdef FOR_CKIT
#if PDC_CONFIG_WRITE_FUNCTIONS > 0

#if PDC_CONFIG_A_CHAR_STRING > 0
ssize_t PDC_a_date_write2io (PDC_t *pdc, Sfio_t *io, PDC_char stopChar, PDC_base_pd *pd, PDC_uint32 *d);
ssize_t PDC_a_date_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_char stopChar, PDC_base_pd *pd, PDC_uint32 *d);
#endif

#if PDC_CONFIG_E_CHAR_STRING > 0
ssize_t PDC_e_date_write2io (PDC_t *pdc, Sfio_t *io, PDC_char stopChar, PDC_base_pd *pd, PDC_uint32 *d);
ssize_t PDC_e_date_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_char stopChar, PDC_base_pd *pd, PDC_uint32 *d);
#endif

#if PDC_CONFIG_A_CHAR_STRING > 0 && PDC_CONFIG_E_CHAR_STRING > 0
ssize_t PDC_date_write2io   (PDC_t *pdc, Sfio_t *io, PDC_char stopChar, PDC_base_pd *pd, PDC_uint32 *d);
ssize_t PDC_date_write2buf  (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_char stopChar, PDC_base_pd *pd, PDC_uint32 *d);
#endif

#endif /* PDC_CONFIG_WRITE_FUNCTIONS */
#endif /* FOR_CKIT */

/* ================================================================================
 * INTEGER/FPOINT WRITE FUNCTIONS
 * 
 * For each integer or fpoint read function there is a corresponding write2io
 * function and a corresponding write2buf function which output the specified
 * value in a format that will allow the corresponding read function to 
 * successfully read the value.
 *
 * For example, if a PDC_int8 is written using PDC_e_int8_write2io, the bytes
 * that were output can be read back into a PDC_int8 using PDC_e_int8_read.
 *
 * All write functions take an Sfio_t* stream pointer (the stream to write to),
 * a parse descriptor pd, and a pointer to the value to be written.  Some also take
 * additional arguments, such as num_digits.  All return an integer.
 *
 * If pd->errCode is either PDC_NO_ERR or PDC_USER_CONSTRAINT_VIOLATIONS then
 * the value is assumed to have been filled in, and it is the value written.
 * For other error codes, the value is assumed to *not* have been filled in,
 * and an error value is written.  See the Default Error Value discussion above
 * for the set of default error values and details on how to override them.
 *
 * If the write succeeds, the return value is the number of bytes written.
 * If it fails, -1 is returned, and no bytes are written to the stream.
 */

/* write2io functions */

#if PDC_CONFIG_WRITE_FUNCTIONS > 0

#if PDC_CONFIG_A_INT > 0
ssize_t PDC_a_int8_write2io  (PDC_t *pdc, Sfio_t *io, PDC_base_pd *pd, PDC_int8   *val);
ssize_t PDC_a_int16_write2io (PDC_t *pdc, Sfio_t *io, PDC_base_pd *pd, PDC_int16  *val);
ssize_t PDC_a_int32_write2io (PDC_t *pdc, Sfio_t *io, PDC_base_pd *pd, PDC_int32  *val);
ssize_t PDC_a_int64_write2io (PDC_t *pdc, Sfio_t *io, PDC_base_pd *pd, PDC_int64  *val);

ssize_t PDC_a_uint8_write2io (PDC_t *pdc, Sfio_t *io, PDC_base_pd *pd, PDC_uint8  *val);
ssize_t PDC_a_uint16_write2io(PDC_t *pdc, Sfio_t *io, PDC_base_pd *pd, PDC_uint16 *val);
ssize_t PDC_a_uint32_write2io(PDC_t *pdc, Sfio_t *io, PDC_base_pd *pd, PDC_uint32 *val);
ssize_t PDC_a_uint64_write2io(PDC_t *pdc, Sfio_t *io, PDC_base_pd *pd, PDC_uint64 *val);
#endif

#if PDC_CONFIG_E_INT > 0
ssize_t PDC_e_int8_write2io  (PDC_t *pdc, Sfio_t *io, PDC_base_pd *pd, PDC_int8   *val);
ssize_t PDC_e_int16_write2io (PDC_t *pdc, Sfio_t *io, PDC_base_pd *pd, PDC_int16  *val);
ssize_t PDC_e_int32_write2io (PDC_t *pdc, Sfio_t *io, PDC_base_pd *pd, PDC_int32  *val);
ssize_t PDC_e_int64_write2io (PDC_t *pdc, Sfio_t *io, PDC_base_pd *pd, PDC_int64  *val);

ssize_t PDC_e_uint8_write2io (PDC_t *pdc, Sfio_t *io, PDC_base_pd *pd, PDC_uint8  *val);
ssize_t PDC_e_uint16_write2io(PDC_t *pdc, Sfio_t *io, PDC_base_pd *pd, PDC_uint16 *val);
ssize_t PDC_e_uint32_write2io(PDC_t *pdc, Sfio_t *io, PDC_base_pd *pd, PDC_uint32 *val);
ssize_t PDC_e_uint64_write2io(PDC_t *pdc, Sfio_t *io, PDC_base_pd *pd, PDC_uint64 *val);
#endif

#if PDC_CONFIG_B_INT > 0
ssize_t PDC_b_int8_write2io  (PDC_t *pdc, Sfio_t *io, PDC_base_pd *pd, PDC_int8   *val);
ssize_t PDC_b_int16_write2io (PDC_t *pdc, Sfio_t *io, PDC_base_pd *pd, PDC_int16  *val);
ssize_t PDC_b_int32_write2io (PDC_t *pdc, Sfio_t *io, PDC_base_pd *pd, PDC_int32  *val);
ssize_t PDC_b_int64_write2io (PDC_t *pdc, Sfio_t *io, PDC_base_pd *pd, PDC_int64  *val);

ssize_t PDC_b_uint8_write2io (PDC_t *pdc, Sfio_t *io, PDC_base_pd *pd, PDC_uint8  *val);
ssize_t PDC_b_uint16_write2io(PDC_t *pdc, Sfio_t *io, PDC_base_pd *pd, PDC_uint16 *val);
ssize_t PDC_b_uint32_write2io(PDC_t *pdc, Sfio_t *io, PDC_base_pd *pd, PDC_uint32 *val);
ssize_t PDC_b_uint64_write2io(PDC_t *pdc, Sfio_t *io, PDC_base_pd *pd, PDC_uint64 *val);
#endif

#if PDC_CONFIG_A_INT_FW > 0
ssize_t PDC_a_int8_FW_write2io  (PDC_t *pdc, Sfio_t *io, size_t width, PDC_base_pd *pd, PDC_int8   *val);
ssize_t PDC_a_int16_FW_write2io (PDC_t *pdc, Sfio_t *io, size_t width, PDC_base_pd *pd, PDC_int16  *val);
ssize_t PDC_a_int32_FW_write2io (PDC_t *pdc, Sfio_t *io, size_t width, PDC_base_pd *pd, PDC_int32  *val);
ssize_t PDC_a_int64_FW_write2io (PDC_t *pdc, Sfio_t *io, size_t width, PDC_base_pd *pd, PDC_int64  *val);

ssize_t PDC_a_uint8_FW_write2io (PDC_t *pdc, Sfio_t *io, size_t width, PDC_base_pd *pd, PDC_uint8  *val);
ssize_t PDC_a_uint16_FW_write2io(PDC_t *pdc, Sfio_t *io, size_t width, PDC_base_pd *pd, PDC_uint16 *val);
ssize_t PDC_a_uint32_FW_write2io(PDC_t *pdc, Sfio_t *io, size_t width, PDC_base_pd *pd, PDC_uint32 *val);
ssize_t PDC_a_uint64_FW_write2io(PDC_t *pdc, Sfio_t *io, size_t width, PDC_base_pd *pd, PDC_uint64 *val);
#endif

#if PDC_CONFIG_E_INT_FW > 0
ssize_t PDC_e_int8_FW_write2io  (PDC_t *pdc, Sfio_t *io, size_t width, PDC_base_pd *pd, PDC_int8   *val);
ssize_t PDC_e_int16_FW_write2io (PDC_t *pdc, Sfio_t *io, size_t width, PDC_base_pd *pd, PDC_int16  *val);
ssize_t PDC_e_int32_FW_write2io (PDC_t *pdc, Sfio_t *io, size_t width, PDC_base_pd *pd, PDC_int32  *val);
ssize_t PDC_e_int64_FW_write2io (PDC_t *pdc, Sfio_t *io, size_t width, PDC_base_pd *pd, PDC_int64  *val);

ssize_t PDC_e_uint8_FW_write2io (PDC_t *pdc, Sfio_t *io, size_t width, PDC_base_pd *pd, PDC_uint8  *val);
ssize_t PDC_e_uint16_FW_write2io(PDC_t *pdc, Sfio_t *io, size_t width, PDC_base_pd *pd, PDC_uint16 *val);
ssize_t PDC_e_uint32_FW_write2io(PDC_t *pdc, Sfio_t *io, size_t width, PDC_base_pd *pd, PDC_uint32 *val);
ssize_t PDC_e_uint64_FW_write2io(PDC_t *pdc, Sfio_t *io, size_t width, PDC_base_pd *pd, PDC_uint64 *val);
#endif

#if PDC_CONFIG_EBC_INT > 0 || PDC_CONFIG_EBC_FPOINT > 0
ssize_t PDC_ebc_int8_write2io  (PDC_t *pdc, Sfio_t *io, PDC_uint32 num_digits, PDC_base_pd *pd, PDC_int8   *val);
ssize_t PDC_ebc_int16_write2io (PDC_t *pdc, Sfio_t *io, PDC_uint32 num_digits, PDC_base_pd *pd, PDC_int16  *val);
ssize_t PDC_ebc_int32_write2io (PDC_t *pdc, Sfio_t *io, PDC_uint32 num_digits, PDC_base_pd *pd, PDC_int32  *val);
ssize_t PDC_ebc_int64_write2io (PDC_t *pdc, Sfio_t *io, PDC_uint32 num_digits, PDC_base_pd *pd, PDC_int64  *val);

ssize_t PDC_ebc_uint8_write2io (PDC_t *pdc, Sfio_t *io, PDC_uint32 num_digits, PDC_base_pd *pd, PDC_uint8  *val);
ssize_t PDC_ebc_uint16_write2io(PDC_t *pdc, Sfio_t *io, PDC_uint32 num_digits, PDC_base_pd *pd, PDC_uint16 *val);
ssize_t PDC_ebc_uint32_write2io(PDC_t *pdc, Sfio_t *io, PDC_uint32 num_digits, PDC_base_pd *pd, PDC_uint32 *val);
ssize_t PDC_ebc_uint64_write2io(PDC_t *pdc, Sfio_t *io, PDC_uint32 num_digits, PDC_base_pd *pd, PDC_uint64 *val);
#endif

#if PDC_CONFIG_BCD_INT > 0 || PDC_CONFIG_BCD_FPOINT > 0
ssize_t PDC_bcd_int8_write2io  (PDC_t *pdc, Sfio_t *io, PDC_uint32 num_digits, PDC_base_pd *pd, PDC_int8   *val);
ssize_t PDC_bcd_int16_write2io (PDC_t *pdc, Sfio_t *io, PDC_uint32 num_digits, PDC_base_pd *pd, PDC_int16  *val);
ssize_t PDC_bcd_int32_write2io (PDC_t *pdc, Sfio_t *io, PDC_uint32 num_digits, PDC_base_pd *pd, PDC_int32  *val);
ssize_t PDC_bcd_int64_write2io (PDC_t *pdc, Sfio_t *io, PDC_uint32 num_digits, PDC_base_pd *pd, PDC_int64  *val);

ssize_t PDC_bcd_uint8_write2io (PDC_t *pdc, Sfio_t *io, PDC_uint32 num_digits, PDC_base_pd *pd, PDC_uint8  *val);
ssize_t PDC_bcd_uint16_write2io(PDC_t *pdc, Sfio_t *io, PDC_uint32 num_digits, PDC_base_pd *pd, PDC_uint16 *val);
ssize_t PDC_bcd_uint32_write2io(PDC_t *pdc, Sfio_t *io, PDC_uint32 num_digits, PDC_base_pd *pd, PDC_uint32 *val);
ssize_t PDC_bcd_uint64_write2io(PDC_t *pdc, Sfio_t *io, PDC_uint32 num_digits, PDC_base_pd *pd, PDC_uint64 *val);
#endif

#if PDC_CONFIG_SBL_INT > 0 || PDC_CONFIG_SBL_FPOINT > 0
ssize_t PDC_sbl_int8_write2io  (PDC_t *pdc, Sfio_t *io, PDC_uint32 num_bytes, PDC_base_pd *pd, PDC_int8   *val);
ssize_t PDC_sbl_int16_write2io (PDC_t *pdc, Sfio_t *io, PDC_uint32 num_bytes, PDC_base_pd *pd, PDC_int16  *val);
ssize_t PDC_sbl_int32_write2io (PDC_t *pdc, Sfio_t *io, PDC_uint32 num_bytes, PDC_base_pd *pd, PDC_int32  *val);
ssize_t PDC_sbl_int64_write2io (PDC_t *pdc, Sfio_t *io, PDC_uint32 num_bytes, PDC_base_pd *pd, PDC_int64  *val);

ssize_t PDC_sbl_uint8_write2io (PDC_t *pdc, Sfio_t *io, PDC_uint32 num_bytes, PDC_base_pd *pd, PDC_uint8  *val);
ssize_t PDC_sbl_uint16_write2io(PDC_t *pdc, Sfio_t *io, PDC_uint32 num_bytes, PDC_base_pd *pd, PDC_uint16 *val);
ssize_t PDC_sbl_uint32_write2io(PDC_t *pdc, Sfio_t *io, PDC_uint32 num_bytes, PDC_base_pd *pd, PDC_uint32 *val);
ssize_t PDC_sbl_uint64_write2io(PDC_t *pdc, Sfio_t *io, PDC_uint32 num_bytes, PDC_base_pd *pd, PDC_uint64 *val);
#endif

#if PDC_CONFIG_SBH_INT > 0 || PDC_CONFIG_SBH_FPOINT > 0
ssize_t PDC_sbh_int8_write2io  (PDC_t *pdc, Sfio_t *io, PDC_uint32 num_bytes, PDC_base_pd *pd, PDC_int8   *val);
ssize_t PDC_sbh_int16_write2io (PDC_t *pdc, Sfio_t *io, PDC_uint32 num_bytes, PDC_base_pd *pd, PDC_int16  *val);
ssize_t PDC_sbh_int32_write2io (PDC_t *pdc, Sfio_t *io, PDC_uint32 num_bytes, PDC_base_pd *pd, PDC_int32  *val);
ssize_t PDC_sbh_int64_write2io (PDC_t *pdc, Sfio_t *io, PDC_uint32 num_bytes, PDC_base_pd *pd, PDC_int64  *val);

ssize_t PDC_sbh_uint8_write2io (PDC_t *pdc, Sfio_t *io, PDC_uint32 num_bytes, PDC_base_pd *pd, PDC_uint8  *val);
ssize_t PDC_sbh_uint16_write2io(PDC_t *pdc, Sfio_t *io, PDC_uint32 num_bytes, PDC_base_pd *pd, PDC_uint16 *val);
ssize_t PDC_sbh_uint32_write2io(PDC_t *pdc, Sfio_t *io, PDC_uint32 num_bytes, PDC_base_pd *pd, PDC_uint32 *val);
ssize_t PDC_sbh_uint64_write2io(PDC_t *pdc, Sfio_t *io, PDC_uint32 num_bytes, PDC_base_pd *pd, PDC_uint64 *val);
#endif

#if PDC_CONFIG_EBC_FPOINT > 0
ssize_t PDC_ebc_fpoint8_write2io  (PDC_t *pdc, Sfio_t *io, PDC_uint32 num_digits, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_fpoint8   *val);
ssize_t PDC_ebc_fpoint16_write2io (PDC_t *pdc, Sfio_t *io, PDC_uint32 num_digits, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_fpoint16  *val);
ssize_t PDC_ebc_fpoint32_write2io (PDC_t *pdc, Sfio_t *io, PDC_uint32 num_digits, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_fpoint32  *val);
ssize_t PDC_ebc_fpoint64_write2io (PDC_t *pdc, Sfio_t *io, PDC_uint32 num_digits, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_fpoint64  *val);

ssize_t PDC_ebc_ufpoint8_write2io (PDC_t *pdc, Sfio_t *io, PDC_uint32 num_digits, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_ufpoint8  *val);
ssize_t PDC_ebc_ufpoint16_write2io(PDC_t *pdc, Sfio_t *io, PDC_uint32 num_digits, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_ufpoint16 *val);
ssize_t PDC_ebc_ufpoint32_write2io(PDC_t *pdc, Sfio_t *io, PDC_uint32 num_digits, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_ufpoint32 *val);
ssize_t PDC_ebc_ufpoint64_write2io(PDC_t *pdc, Sfio_t *io, PDC_uint32 num_digits, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_ufpoint64 *val);
#endif

#if PDC_CONFIG_BCD_FPOINT > 0
ssize_t PDC_bcd_fpoint8_write2io  (PDC_t *pdc, Sfio_t *io, PDC_uint32 num_digits, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_fpoint8   *val);
ssize_t PDC_bcd_fpoint16_write2io (PDC_t *pdc, Sfio_t *io, PDC_uint32 num_digits, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_fpoint16  *val);
ssize_t PDC_bcd_fpoint32_write2io (PDC_t *pdc, Sfio_t *io, PDC_uint32 num_digits, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_fpoint32  *val);
ssize_t PDC_bcd_fpoint64_write2io (PDC_t *pdc, Sfio_t *io, PDC_uint32 num_digits, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_fpoint64  *val);

ssize_t PDC_bcd_ufpoint8_write2io (PDC_t *pdc, Sfio_t *io, PDC_uint32 num_digits, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_ufpoint8  *val);
ssize_t PDC_bcd_ufpoint16_write2io(PDC_t *pdc, Sfio_t *io, PDC_uint32 num_digits, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_ufpoint16 *val);
ssize_t PDC_bcd_ufpoint32_write2io(PDC_t *pdc, Sfio_t *io, PDC_uint32 num_digits, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_ufpoint32 *val);
ssize_t PDC_bcd_ufpoint64_write2io(PDC_t *pdc, Sfio_t *io, PDC_uint32 num_digits, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_ufpoint64 *val);
#endif

#if PDC_CONFIG_SBL_FPOINT > 0
ssize_t PDC_sbl_fpoint8_write2io  (PDC_t *pdc, Sfio_t *io, PDC_uint32 num_bytes, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_fpoint8   *val);
ssize_t PDC_sbl_fpoint16_write2io (PDC_t *pdc, Sfio_t *io, PDC_uint32 num_bytes, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_fpoint16  *val);
ssize_t PDC_sbl_fpoint32_write2io (PDC_t *pdc, Sfio_t *io, PDC_uint32 num_bytes, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_fpoint32  *val);
ssize_t PDC_sbl_fpoint64_write2io (PDC_t *pdc, Sfio_t *io, PDC_uint32 num_bytes, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_fpoint64  *val);

ssize_t PDC_sbl_ufpoint8_write2io (PDC_t *pdc, Sfio_t *io, PDC_uint32 num_bytes, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_ufpoint8  *val);
ssize_t PDC_sbl_ufpoint16_write2io(PDC_t *pdc, Sfio_t *io, PDC_uint32 num_bytes, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_ufpoint16 *val);
ssize_t PDC_sbl_ufpoint32_write2io(PDC_t *pdc, Sfio_t *io, PDC_uint32 num_bytes, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_ufpoint32 *val);
ssize_t PDC_sbl_ufpoint64_write2io(PDC_t *pdc, Sfio_t *io, PDC_uint32 num_bytes, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_ufpoint64 *val);
#endif

#if PDC_CONFIG_SBH_FPOINT > 0
ssize_t PDC_sbh_fpoint8_write2io  (PDC_t *pdc, Sfio_t *io, PDC_uint32 num_bytes, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_fpoint8   *val);
ssize_t PDC_sbh_fpoint16_write2io (PDC_t *pdc, Sfio_t *io, PDC_uint32 num_bytes, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_fpoint16  *val);
ssize_t PDC_sbh_fpoint32_write2io (PDC_t *pdc, Sfio_t *io, PDC_uint32 num_bytes, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_fpoint32  *val);
ssize_t PDC_sbh_fpoint64_write2io (PDC_t *pdc, Sfio_t *io, PDC_uint32 num_bytes, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_fpoint64  *val);

ssize_t PDC_sbh_ufpoint8_write2io (PDC_t *pdc, Sfio_t *io, PDC_uint32 num_bytes, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_ufpoint8  *val);
ssize_t PDC_sbh_ufpoint16_write2io(PDC_t *pdc, Sfio_t *io, PDC_uint32 num_bytes, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_ufpoint16 *val);
ssize_t PDC_sbh_ufpoint32_write2io(PDC_t *pdc, Sfio_t *io, PDC_uint32 num_bytes, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_ufpoint32 *val);
ssize_t PDC_sbh_ufpoint64_write2io(PDC_t *pdc, Sfio_t *io, PDC_uint32 num_bytes, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_ufpoint64 *val);
#endif

/* write2buf functions */

#if PDC_CONFIG_A_INT > 0
ssize_t PDC_a_int8_write2buf  (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_pd *pd, PDC_int8   *val);
ssize_t PDC_a_int16_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_pd *pd, PDC_int16  *val);
ssize_t PDC_a_int32_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_pd *pd, PDC_int32  *val);
ssize_t PDC_a_int64_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_pd *pd, PDC_int64  *val);

ssize_t PDC_a_uint8_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_pd *pd, PDC_uint8  *val);
ssize_t PDC_a_uint16_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_pd *pd, PDC_uint16 *val);
ssize_t PDC_a_uint32_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_pd *pd, PDC_uint32 *val);
ssize_t PDC_a_uint64_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_pd *pd, PDC_uint64 *val);
#endif

#if PDC_CONFIG_E_INT > 0
ssize_t PDC_e_int8_write2buf  (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_pd *pd, PDC_int8   *val);
ssize_t PDC_e_int16_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_pd *pd, PDC_int16  *val);
ssize_t PDC_e_int32_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_pd *pd, PDC_int32  *val);
ssize_t PDC_e_int64_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_pd *pd, PDC_int64  *val);

ssize_t PDC_e_uint8_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_pd *pd, PDC_uint8  *val);
ssize_t PDC_e_uint16_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_pd *pd, PDC_uint16 *val);
ssize_t PDC_e_uint32_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_pd *pd, PDC_uint32 *val);
ssize_t PDC_e_uint64_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_pd *pd, PDC_uint64 *val);
#endif

#if PDC_CONFIG_B_INT > 0
ssize_t PDC_b_int8_write2buf  (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_pd *pd, PDC_int8   *val);
ssize_t PDC_b_int16_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_pd *pd, PDC_int16  *val);
ssize_t PDC_b_int32_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_pd *pd, PDC_int32  *val);
ssize_t PDC_b_int64_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_pd *pd, PDC_int64  *val);

ssize_t PDC_b_uint8_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_pd *pd, PDC_uint8  *val);
ssize_t PDC_b_uint16_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_pd *pd, PDC_uint16 *val);
ssize_t PDC_b_uint32_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_pd *pd, PDC_uint32 *val);
ssize_t PDC_b_uint64_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_pd *pd, PDC_uint64 *val);
#endif

#if PDC_CONFIG_A_INT_FW > 0
ssize_t PDC_a_int8_FW_write2buf  (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, size_t width, PDC_base_pd *pd, PDC_int8   *val);
ssize_t PDC_a_int16_FW_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, size_t width, PDC_base_pd *pd, PDC_int16  *val);
ssize_t PDC_a_int32_FW_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, size_t width, PDC_base_pd *pd, PDC_int32  *val);
ssize_t PDC_a_int64_FW_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, size_t width, PDC_base_pd *pd, PDC_int64  *val);

ssize_t PDC_a_uint8_FW_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, size_t width, PDC_base_pd *pd, PDC_uint8  *val);
ssize_t PDC_a_uint16_FW_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, size_t width, PDC_base_pd *pd, PDC_uint16 *val);
ssize_t PDC_a_uint32_FW_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, size_t width, PDC_base_pd *pd, PDC_uint32 *val);
ssize_t PDC_a_uint64_FW_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, size_t width, PDC_base_pd *pd, PDC_uint64 *val);
#endif

#if PDC_CONFIG_E_INT_FW > 0
ssize_t PDC_e_int8_FW_write2buf  (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, size_t width, PDC_base_pd *pd, PDC_int8   *val);
ssize_t PDC_e_int16_FW_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, size_t width, PDC_base_pd *pd, PDC_int16  *val);
ssize_t PDC_e_int32_FW_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, size_t width, PDC_base_pd *pd, PDC_int32  *val);
ssize_t PDC_e_int64_FW_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, size_t width, PDC_base_pd *pd, PDC_int64  *val);

ssize_t PDC_e_uint8_FW_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, size_t width, PDC_base_pd *pd, PDC_uint8  *val);
ssize_t PDC_e_uint16_FW_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, size_t width, PDC_base_pd *pd, PDC_uint16 *val);
ssize_t PDC_e_uint32_FW_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, size_t width, PDC_base_pd *pd, PDC_uint32 *val);
ssize_t PDC_e_uint64_FW_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, size_t width, PDC_base_pd *pd, PDC_uint64 *val);
#endif

#if PDC_CONFIG_EBC_INT > 0 || PDC_CONFIG_EBC_FPOINT > 0
ssize_t PDC_ebc_int8_write2buf  (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_digits, PDC_base_pd *pd, PDC_int8   *val);
ssize_t PDC_ebc_int16_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_digits, PDC_base_pd *pd, PDC_int16  *val);
ssize_t PDC_ebc_int32_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_digits, PDC_base_pd *pd, PDC_int32  *val);
ssize_t PDC_ebc_int64_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_digits, PDC_base_pd *pd, PDC_int64  *val);

ssize_t PDC_ebc_uint8_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_digits, PDC_base_pd *pd, PDC_uint8  *val);
ssize_t PDC_ebc_uint16_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_digits, PDC_base_pd *pd, PDC_uint16 *val);
ssize_t PDC_ebc_uint32_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_digits, PDC_base_pd *pd, PDC_uint32 *val);
ssize_t PDC_ebc_uint64_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_digits, PDC_base_pd *pd, PDC_uint64 *val);
#endif

#if PDC_CONFIG_BCD_INT > 0 || PDC_CONFIG_BCD_FPOINT > 0
ssize_t PDC_bcd_int8_write2buf  (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_digits, PDC_base_pd *pd, PDC_int8   *val);
ssize_t PDC_bcd_int16_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_digits, PDC_base_pd *pd, PDC_int16  *val);
ssize_t PDC_bcd_int32_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_digits, PDC_base_pd *pd, PDC_int32  *val);
ssize_t PDC_bcd_int64_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_digits, PDC_base_pd *pd, PDC_int64  *val);

ssize_t PDC_bcd_uint8_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_digits, PDC_base_pd *pd, PDC_uint8  *val);
ssize_t PDC_bcd_uint16_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_digits, PDC_base_pd *pd, PDC_uint16 *val);
ssize_t PDC_bcd_uint32_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_digits, PDC_base_pd *pd, PDC_uint32 *val);
ssize_t PDC_bcd_uint64_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_digits, PDC_base_pd *pd, PDC_uint64 *val);
#endif

#if PDC_CONFIG_SBL_INT > 0 || PDC_CONFIG_SBL_FPOINT > 0
ssize_t PDC_sbl_int8_write2buf  (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_bytes, PDC_base_pd *pd, PDC_int8   *val);
ssize_t PDC_sbl_int16_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_bytes, PDC_base_pd *pd, PDC_int16  *val);
ssize_t PDC_sbl_int32_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_bytes, PDC_base_pd *pd, PDC_int32  *val);
ssize_t PDC_sbl_int64_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_bytes, PDC_base_pd *pd, PDC_int64  *val);

ssize_t PDC_sbl_uint8_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_bytes, PDC_base_pd *pd, PDC_uint8  *val);
ssize_t PDC_sbl_uint16_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_bytes, PDC_base_pd *pd, PDC_uint16 *val);
ssize_t PDC_sbl_uint32_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_bytes, PDC_base_pd *pd, PDC_uint32 *val);
ssize_t PDC_sbl_uint64_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_bytes, PDC_base_pd *pd, PDC_uint64 *val);
#endif

#if PDC_CONFIG_SBH_INT > 0 || PDC_CONFIG_SBH_FPOINT > 0
ssize_t PDC_sbh_int8_write2buf  (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_bytes, PDC_base_pd *pd, PDC_int8   *val);
ssize_t PDC_sbh_int16_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_bytes, PDC_base_pd *pd, PDC_int16  *val);
ssize_t PDC_sbh_int32_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_bytes, PDC_base_pd *pd, PDC_int32  *val);
ssize_t PDC_sbh_int64_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_bytes, PDC_base_pd *pd, PDC_int64  *val);

ssize_t PDC_sbh_uint8_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_bytes, PDC_base_pd *pd, PDC_uint8  *val);
ssize_t PDC_sbh_uint16_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_bytes, PDC_base_pd *pd, PDC_uint16 *val);
ssize_t PDC_sbh_uint32_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_bytes, PDC_base_pd *pd, PDC_uint32 *val);
ssize_t PDC_sbh_uint64_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_bytes, PDC_base_pd *pd, PDC_uint64 *val);
#endif

#if PDC_CONFIG_EBC_FPOINT > 0
ssize_t PDC_ebc_fpoint8_write2buf  (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_digits, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_fpoint8   *val);
ssize_t PDC_ebc_fpoint16_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_digits, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_fpoint16  *val);
ssize_t PDC_ebc_fpoint32_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_digits, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_fpoint32  *val);
ssize_t PDC_ebc_fpoint64_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_digits, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_fpoint64  *val);

ssize_t PDC_ebc_ufpoint8_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_digits, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_ufpoint8  *val);
ssize_t PDC_ebc_ufpoint16_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_digits, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_ufpoint16 *val);
ssize_t PDC_ebc_ufpoint32_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_digits, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_ufpoint32 *val);
ssize_t PDC_ebc_ufpoint64_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_digits, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_ufpoint64 *val);
#endif

#if PDC_CONFIG_BCD_FPOINT > 0
ssize_t PDC_bcd_fpoint8_write2buf  (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_digits, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_fpoint8   *val);
ssize_t PDC_bcd_fpoint16_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_digits, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_fpoint16  *val);
ssize_t PDC_bcd_fpoint32_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_digits, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_fpoint32  *val);
ssize_t PDC_bcd_fpoint64_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_digits, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_fpoint64  *val);

ssize_t PDC_bcd_ufpoint8_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_digits, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_ufpoint8  *val);
ssize_t PDC_bcd_ufpoint16_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_digits, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_ufpoint16 *val);
ssize_t PDC_bcd_ufpoint32_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_digits, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_ufpoint32 *val);
ssize_t PDC_bcd_ufpoint64_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_digits, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_ufpoint64 *val);
#endif

#if PDC_CONFIG_SBL_FPOINT > 0
ssize_t PDC_sbl_fpoint8_write2buf  (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_bytes, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_fpoint8   *val);
ssize_t PDC_sbl_fpoint16_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_bytes, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_fpoint16  *val);
ssize_t PDC_sbl_fpoint32_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_bytes, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_fpoint32  *val);
ssize_t PDC_sbl_fpoint64_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_bytes, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_fpoint64  *val);

ssize_t PDC_sbl_ufpoint8_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_bytes, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_ufpoint8  *val);
ssize_t PDC_sbl_ufpoint16_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_bytes, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_ufpoint16 *val);
ssize_t PDC_sbl_ufpoint32_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_bytes, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_ufpoint32 *val);
ssize_t PDC_sbl_ufpoint64_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_bytes, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_ufpoint64 *val);
#endif

#if PDC_CONFIG_SBH_FPOINT > 0
ssize_t PDC_sbh_fpoint8_write2buf  (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_bytes, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_fpoint8   *val);
ssize_t PDC_sbh_fpoint16_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_bytes, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_fpoint16  *val);
ssize_t PDC_sbh_fpoint32_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_bytes, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_fpoint32  *val);
ssize_t PDC_sbh_fpoint64_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_bytes, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_fpoint64  *val);

ssize_t PDC_sbh_ufpoint8_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_bytes, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_ufpoint8  *val);
ssize_t PDC_sbh_ufpoint16_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_bytes, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_ufpoint16 *val);
ssize_t PDC_sbh_ufpoint32_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_bytes, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_ufpoint32 *val);
ssize_t PDC_sbh_ufpoint64_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_bytes, PDC_uint32 d_exp, PDC_base_pd *pd, PDC_ufpoint64 *val);
#endif

#endif /* PDC_CONFIG_WRITE_FUNCTIONS */

/*
 * The following default versions simply call the appropriate ASCII or EBCDIC version,
 * depending on pdc->disc->def_charset.
 */

#ifdef FOR_CKIT
#if PDC_CONFIG_WRITE_FUNCTIONS > 0

#if PDC_CONFIG_A_INT_FW > 0 && PDC_CONFIG_E_INT_FW > 0
ssize_t PDC_int8_FW_write2io  (PDC_t *pdc, Sfio_t *io, size_t width, PDC_base_pd *pd, PDC_int8   *val);
ssize_t PDC_int16_FW_write2io (PDC_t *pdc, Sfio_t *io, size_t width, PDC_base_pd *pd, PDC_int16  *val);
ssize_t PDC_int32_FW_write2io (PDC_t *pdc, Sfio_t *io, size_t width, PDC_base_pd *pd, PDC_int32  *val);
ssize_t PDC_int64_FW_write2io (PDC_t *pdc, Sfio_t *io, size_t width, PDC_base_pd *pd, PDC_int64  *val);

ssize_t PDC_uint8_FW_write2io (PDC_t *pdc, Sfio_t *io, size_t width, PDC_base_pd *pd, PDC_uint8  *val);
ssize_t PDC_uint16_FW_write2io(PDC_t *pdc, Sfio_t *io, size_t width, PDC_base_pd *pd, PDC_uint16 *val);
ssize_t PDC_uint32_FW_write2io(PDC_t *pdc, Sfio_t *io, size_t width, PDC_base_pd *pd, PDC_uint32 *val);
ssize_t PDC_uint64_FW_write2io(PDC_t *pdc, Sfio_t *io, size_t width, PDC_base_pd *pd, PDC_uint64 *val);

ssize_t PDC_int8_FW_write2buf  (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, size_t width, PDC_base_pd *pd, PDC_int8   *val);
ssize_t PDC_int16_FW_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, size_t width, PDC_base_pd *pd, PDC_int16  *val);
ssize_t PDC_int32_FW_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, size_t width, PDC_base_pd *pd, PDC_int32  *val);
ssize_t PDC_int64_FW_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, size_t width, PDC_base_pd *pd, PDC_int64  *val);

ssize_t PDC_uint8_FW_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, size_t width, PDC_base_pd *pd, PDC_uint8  *val);
ssize_t PDC_uint16_FW_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, size_t width, PDC_base_pd *pd, PDC_uint16 *val);
ssize_t PDC_uint32_FW_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, size_t width, PDC_base_pd *pd, PDC_uint32 *val);
ssize_t PDC_uint64_FW_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, size_t width, PDC_base_pd *pd, PDC_uint64 *val);
#endif

#if PDC_CONFIG_A_INT > 0 && PDC_CONFIG_E_INT > 0
ssize_t PDC_int8_write2io  (PDC_t *pdc, Sfio_t *io, PDC_base_pd *pd, PDC_int8   *val);
ssize_t PDC_int16_write2io (PDC_t *pdc, Sfio_t *io, PDC_base_pd *pd, PDC_int16  *val);
ssize_t PDC_int32_write2io (PDC_t *pdc, Sfio_t *io, PDC_base_pd *pd, PDC_int32  *val);
ssize_t PDC_int64_write2io (PDC_t *pdc, Sfio_t *io, PDC_base_pd *pd, PDC_int64  *val);

ssize_t PDC_uint8_write2io (PDC_t *pdc, Sfio_t *io, PDC_base_pd *pd, PDC_uint8  *val);
ssize_t PDC_uint16_write2io(PDC_t *pdc, Sfio_t *io, PDC_base_pd *pd, PDC_uint16 *val);
ssize_t PDC_uint32_write2io(PDC_t *pdc, Sfio_t *io, PDC_base_pd *pd, PDC_uint32 *val);
ssize_t PDC_uint64_write2io(PDC_t *pdc, Sfio_t *io, PDC_base_pd *pd, PDC_uint64 *val);

ssize_t PDC_int8_write2buf  (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_pd *pd, PDC_int8   *val);
ssize_t PDC_int16_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_pd *pd, PDC_int16  *val);
ssize_t PDC_int32_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_pd *pd, PDC_int32  *val);
ssize_t PDC_int64_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_pd *pd, PDC_int64  *val);

ssize_t PDC_uint8_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_pd *pd, PDC_uint8  *val);
ssize_t PDC_uint16_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_pd *pd, PDC_uint16 *val);
ssize_t PDC_uint32_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_pd *pd, PDC_uint32 *val);
ssize_t PDC_uint64_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_pd *pd, PDC_uint64 *val);
#endif

#endif /* PDC_CONFIG_WRITE_FUNCTIONS */
#endif /* FOR_CKIT */

/* ================================================================================
 * MISC WRITE FUNCTIONS
 *
 * The countX and countXtoY write functions do nothing and return length 0.
 * They exist for completeness.
 */

#if PDC_CONFIG_WRITE_FUNCTIONS > 0
ssize_t PDC_countX_write2io    (PDC_t *pdc, Sfio_t *io, PDC_uint8 x, int eor_required,
				PDC_base_pd *pd, PDC_int32  *val);
ssize_t PDC_countX_write2buf   (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint8 x, int eor_required,
				PDC_base_pd *pd, PDC_int32  *val);

ssize_t PDC_countXtoY_write2io (PDC_t *pdc, Sfio_t *io, PDC_uint8 x, PDC_uint8 y,
				PDC_base_pd *pd, PDC_int32  *val);
ssize_t PDC_countXtoY_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint8 x, PDC_uint8 y,
				PDC_base_pd *pd, PDC_int32  *val);
#endif /* PDC_CONFIG_WRITE_FUNCTIONS */

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
  PDC_uint64   max2track;
  PDC_uint64   max2rep;
  double       pcnt2rep;
  PDC_uint64   good;
  PDC_uint64   bad;
  PDC_uint64   fold;
  PDC_uint64   tracked;
  PDC_int64    psum;
  double       avg;
  PDC_int64    min;
  PDC_int64    max;
} PDC_int_acc;

typedef struct PDC_uint_acc_s {
  Dt_t        *dict;
  PDC_uint64   max2track;
  PDC_uint64   max2rep;
  double       pcnt2rep;
  PDC_uint64   good;
  PDC_uint64   bad;
  PDC_uint64   fold;
  PDC_uint64   tracked;
  PDC_uint64   psum;
  double       avg;
  PDC_uint64   min;
  PDC_uint64   max;
} PDC_uint_acc;

/* A map_<int_type> function maps a given integer type to a string */
typedef const char * (*PDC_int8_map_fn)  (PDC_int8   i);
typedef const char * (*PDC_int16_map_fn) (PDC_int16  i);
typedef const char * (*PDC_int32_map_fn) (PDC_int32  i);
typedef const char * (*PDC_int64_map_fn) (PDC_int64  i);
typedef const char * (*PDC_uint8_map_fn) (PDC_uint8  u);
typedef const char * (*PDC_uint16_map_fn)(PDC_uint16 u);
typedef const char * (*PDC_uint32_map_fn)(PDC_uint32 u);
typedef const char * (*PDC_uint64_map_fn)(PDC_uint64 u);

/* We always need type PDC_int32_acc, PDC_uint32_acc */

typedef PDC_int_acc PDC_int32_acc;
typedef PDC_uint_acc PDC_uint32_acc;

PDC_error_t PDC_int32_acc_init    (PDC_t *pdc, PDC_int32_acc *a);
PDC_error_t PDC_int32_acc_reset   (PDC_t *pdc, PDC_int32_acc *a);
PDC_error_t PDC_int32_acc_cleanup (PDC_t *pdc, PDC_int32_acc *a);
PDC_error_t PDC_int32_acc_add     (PDC_t *pdc, PDC_int32_acc *a, const PDC_base_pd *pd, const PDC_int32 *val);
PDC_error_t PDC_int32_acc_report  (PDC_t *pdc, const char *prefix, const char *what, int nst,
				   PDC_int32_acc *a);
double      PDC_int32_acc_avg     (PDC_t *pdc, PDC_int32_acc *a);
PDC_int32   PDC_int32_acc_ravg    (PDC_t *pdc, PDC_int32_acc *a);

PDC_error_t PDC_uint32_acc_init    (PDC_t *pdc, PDC_uint32_acc *a);
PDC_error_t PDC_uint32_acc_reset   (PDC_t *pdc, PDC_uint32_acc *a);
PDC_error_t PDC_uint32_acc_cleanup (PDC_t *pdc, PDC_uint32_acc *a);
PDC_error_t PDC_uint32_acc_add     (PDC_t *pdc, PDC_uint32_acc *a, const PDC_base_pd *pd, const PDC_uint32 *val);
PDC_error_t PDC_uint32_acc_report  (PDC_t *pdc, const char *prefix, const char *what, int nst,
				    PDC_uint32_acc *a);
double      PDC_uint32_acc_avg     (PDC_t *pdc, PDC_uint32_acc *a);
PDC_uint32  PDC_uint32_acc_ravg    (PDC_t *pdc, PDC_uint32_acc *a);

/*
 * Mapped versions of the integer acc_report functions:
 * these functions are used when integers have associated
 * string values.  
 */
PDC_error_t PDC_int32_acc_map_report(PDC_t *pdc, const char *prefix, const char *what, int nst,
				     PDC_int32_map_fn  fn, PDC_int32_acc *a);

/*
 * PDC_nerr_acc_report is used to report on the accumulation of the nerr field
 * of a struct, union, array, etc.  The accumulator used must be a PDC_uint32_acc.
 * This is very similar to calling PDC_uint32_acc_report, it just has slightly
 * different formatting since no bad values are expected.
 */
PDC_error_t PDC_nerr_acc_report(PDC_t *pdc, const char *prefix, const char *what, int nst,
				PDC_uint32_acc *a);

/* Remaining accumulator types: only if configured */ 
#if PDC_CONFIG_ACCUM_FUNCTIONS > 0

typedef PDC_int_acc PDC_int8_acc;
typedef PDC_int_acc PDC_int16_acc;
typedef PDC_int_acc PDC_int64_acc;

typedef PDC_uint_acc PDC_uint8_acc;
typedef PDC_uint_acc PDC_uint16_acc;
typedef PDC_uint_acc PDC_uint64_acc;

typedef struct PDC_string_acc_s {
  Dt_t           *dict;
  PDC_uint64      max2track;
  PDC_uint64      max2rep;
  double          pcnt2rep;
  PDC_uint64      tracked;
  PDC_uint32_acc  len_accum; /* used for length distribution and good/bad accounting */
} PDC_string_acc;

PDC_error_t PDC_int8_acc_init    (PDC_t *pdc, PDC_int8_acc *a);
PDC_error_t PDC_int8_acc_reset   (PDC_t *pdc, PDC_int8_acc *a);
PDC_error_t PDC_int8_acc_cleanup (PDC_t *pdc, PDC_int8_acc *a);
PDC_error_t PDC_int8_acc_add     (PDC_t *pdc, PDC_int8_acc *a, const PDC_base_pd *pd, const PDC_int8 *val);
PDC_error_t PDC_int8_acc_report  (PDC_t *pdc, const char *prefix, const char *what, int nst,
				  PDC_int8_acc *a);
double      PDC_int8_acc_avg     (PDC_t *pdc, PDC_int8_acc *a);
PDC_int8    PDC_int8_acc_ravg    (PDC_t *pdc, PDC_int8_acc *a);

PDC_error_t PDC_int16_acc_init    (PDC_t *pdc, PDC_int16_acc *a);
PDC_error_t PDC_int16_acc_reset   (PDC_t *pdc, PDC_int16_acc *a);
PDC_error_t PDC_int16_acc_cleanup (PDC_t *pdc, PDC_int16_acc *a);
PDC_error_t PDC_int16_acc_add     (PDC_t *pdc, PDC_int16_acc *a, const PDC_base_pd *pd, const PDC_int16 *val);
PDC_error_t PDC_int16_acc_report  (PDC_t *pdc, const char *prefix, const char *what, int nst,
				   PDC_int16_acc *a);
double      PDC_int16_acc_avg     (PDC_t *pdc, PDC_int16_acc *a);
PDC_int16   PDC_int16_acc_ravg    (PDC_t *pdc, PDC_int16_acc *a);

PDC_error_t PDC_int64_acc_init    (PDC_t *pdc, PDC_int64_acc *a);
PDC_error_t PDC_int64_acc_reset   (PDC_t *pdc, PDC_int64_acc *a);
PDC_error_t PDC_int64_acc_cleanup (PDC_t *pdc, PDC_int64_acc *a);
PDC_error_t PDC_int64_acc_add     (PDC_t *pdc, PDC_int64_acc *a, const PDC_base_pd *pd, const PDC_int64 *val);
PDC_error_t PDC_int64_acc_report  (PDC_t *pdc, const char *prefix, const char *what, int nst,
				   PDC_int64_acc *a);
double      PDC_int64_acc_avg     (PDC_t *pdc, PDC_int64_acc *a);
PDC_int64   PDC_int64_acc_ravg    (PDC_t *pdc, PDC_int64_acc *a);

PDC_error_t PDC_uint8_acc_init    (PDC_t *pdc, PDC_uint8_acc *a);
PDC_error_t PDC_uint8_acc_reset   (PDC_t *pdc, PDC_uint8_acc *a);
PDC_error_t PDC_uint8_acc_cleanup (PDC_t *pdc, PDC_uint8_acc *a);
PDC_error_t PDC_uint8_acc_add     (PDC_t *pdc, PDC_uint8_acc *a, const PDC_base_pd *pd, const PDC_uint8 *val);
PDC_error_t PDC_uint8_acc_report  (PDC_t *pdc, const char *prefix, const char *what, int nst,
				   PDC_uint8_acc *a);
double      PDC_uint8_acc_avg     (PDC_t *pdc, PDC_uint8_acc *a);
PDC_uint8   PDC_uint8_acc_ravg    (PDC_t *pdc, PDC_uint8_acc *a);

PDC_error_t PDC_uint16_acc_init    (PDC_t *pdc, PDC_uint16_acc *a);
PDC_error_t PDC_uint16_acc_reset   (PDC_t *pdc, PDC_uint16_acc *a);
PDC_error_t PDC_uint16_acc_cleanup (PDC_t *pdc, PDC_uint16_acc *a);
PDC_error_t PDC_uint16_acc_add     (PDC_t *pdc, PDC_uint16_acc *a, const PDC_base_pd *pd, const PDC_uint16 *val);
PDC_error_t PDC_uint16_acc_report  (PDC_t *pdc, const char *prefix, const char *what, int nst,
				    PDC_uint16_acc *a);
double      PDC_uint16_acc_avg     (PDC_t *pdc, PDC_uint16_acc *a);
PDC_uint16  PDC_uint16_acc_ravg    (PDC_t *pdc, PDC_uint16_acc *a);

PDC_error_t PDC_uint64_acc_init    (PDC_t *pdc, PDC_uint64_acc *a);
PDC_error_t PDC_uint64_acc_reset   (PDC_t *pdc, PDC_uint64_acc *a);
PDC_error_t PDC_uint64_acc_cleanup (PDC_t *pdc, PDC_uint64_acc *a);
PDC_error_t PDC_uint64_acc_add     (PDC_t *pdc, PDC_uint64_acc *a, const PDC_base_pd *pd, const PDC_uint64 *val);
PDC_error_t PDC_uint64_acc_report  (PDC_t *pdc, const char *prefix, const char *what, int nst,
				    PDC_uint64_acc *a);
double      PDC_uint64_acc_avg     (PDC_t *pdc, PDC_uint64_acc *a);
PDC_uint64  PDC_uint64_acc_ravg    (PDC_t *pdc, PDC_uint64_acc *a);

PDC_error_t PDC_string_acc_init    (PDC_t *pdc, PDC_string_acc *a);
PDC_error_t PDC_string_acc_reset   (PDC_t *pdc, PDC_string_acc *a);
PDC_error_t PDC_string_acc_cleanup (PDC_t *pdc, PDC_string_acc *a);
PDC_error_t PDC_string_acc_add     (PDC_t *pdc, PDC_string_acc *a, const PDC_base_pd *pd, const PDC_string* val);
PDC_error_t PDC_string_acc_report  (PDC_t *pdc, const char *prefix, const char *what, int nst,
				    PDC_string_acc *a);

/*
 * char_acc is just like uint8_acc except a different report is generated
 */
typedef PDC_uint8_acc PDC_char_acc;

PDC_error_t PDC_char_acc_init      (PDC_t *pdc, PDC_char_acc *a);
PDC_error_t PDC_char_acc_reset     (PDC_t *pdc, PDC_char_acc *a);
PDC_error_t PDC_char_acc_cleanup   (PDC_t *pdc, PDC_char_acc *a);
PDC_error_t PDC_char_acc_add       (PDC_t *pdc, PDC_char_acc *a, const PDC_base_pd *pd, const PDC_uint8 *val);
PDC_error_t PDC_char_acc_report    (PDC_t *pdc, const char *prefix, const char *what, int nst,
				    PDC_char_acc *a);

/*
 * fpoint/ufpoint accumulator types
 *
 *    Note that double-based arithmetic is used for the fpoint64/ufpoint64 accumulators,
 *    while float-based arithmetic is used for all other fpoint/ufpoint accumulators.
 */

typedef struct PDC_fpoint_acc_flt_s {
  Dt_t        *dict;
  PDC_uint64   max2track;
  PDC_uint64   max2rep;
  double       pcnt2rep;
  PDC_uint64   good;
  PDC_uint64   bad;
  PDC_uint64   fold;
  PDC_uint64   tracked;
  double       psum;
  double       avg;
  double       min;
  double       max;
} PDC_fpoint_acc_flt;

typedef struct PDC_fpoint_acc_dbl_s {
  Dt_t        *dict;
  PDC_uint64   max2track;
  PDC_uint64   max2rep;
  double       pcnt2rep;
  PDC_uint64   good;
  PDC_uint64   bad;
  PDC_uint64   fold;
  PDC_uint64   tracked;
  double       psum;
  double       avg;
  double       min;
  double       max;
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
PDC_error_t PDC_fpoint8_acc_add     (PDC_t *pdc, PDC_fpoint8_acc *a, const PDC_base_pd *pd, const PDC_fpoint8 *val);
PDC_error_t PDC_fpoint8_acc_report  (PDC_t *pdc, const char *prefix, const char *what, int nst,
				     PDC_fpoint8_acc *a);
float       PDC_fpoint8_acc_avg     (PDC_t *pdc, PDC_fpoint8_acc *a);

PDC_error_t PDC_fpoint16_acc_init   (PDC_t *pdc, PDC_fpoint16_acc *a);
PDC_error_t PDC_fpoint16_acc_reset  (PDC_t *pdc, PDC_fpoint16_acc *a);
PDC_error_t PDC_fpoint16_acc_cleanup(PDC_t *pdc, PDC_fpoint16_acc *a);
PDC_error_t PDC_fpoint16_acc_add    (PDC_t *pdc, PDC_fpoint16_acc *a, const PDC_base_pd *pd, const PDC_fpoint16 *val);
PDC_error_t PDC_fpoint16_acc_report (PDC_t *pdc, const char *prefix, const char *what, int nst,
				     PDC_fpoint16_acc *a);
float       PDC_fpoint16_acc_avg    (PDC_t *pdc, PDC_fpoint16_acc *a);

PDC_error_t PDC_fpoint32_acc_init   (PDC_t *pdc, PDC_fpoint32_acc *a);
PDC_error_t PDC_fpoint32_acc_reset  (PDC_t *pdc, PDC_fpoint32_acc *a);
PDC_error_t PDC_fpoint32_acc_cleanup(PDC_t *pdc, PDC_fpoint32_acc *a);
PDC_error_t PDC_fpoint32_acc_add    (PDC_t *pdc, PDC_fpoint32_acc *a, const PDC_base_pd *pd, const PDC_fpoint32 *val);
PDC_error_t PDC_fpoint32_acc_report (PDC_t *pdc, const char *prefix, const char *what, int nst,
				     PDC_fpoint32_acc *a);
float       PDC_fpoint32_acc_avg    (PDC_t *pdc, PDC_fpoint32_acc *a);

PDC_error_t PDC_fpoint64_acc_init   (PDC_t *pdc, PDC_fpoint64_acc *a);
PDC_error_t PDC_fpoint64_acc_reset  (PDC_t *pdc, PDC_fpoint64_acc *a);
PDC_error_t PDC_fpoint64_acc_cleanup(PDC_t *pdc, PDC_fpoint64_acc *a);
PDC_error_t PDC_fpoint64_acc_add    (PDC_t *pdc, PDC_fpoint64_acc *a, const PDC_base_pd *pd, const PDC_fpoint64 *val);
PDC_error_t PDC_fpoint64_acc_report (PDC_t *pdc, const char *prefix, const char *what, int nst,
				     PDC_fpoint64_acc *a);
double      PDC_fpoint64_acc_avg    (PDC_t *pdc, PDC_fpoint64_acc *a);

PDC_error_t PDC_ufpoint8_acc_init    (PDC_t *pdc, PDC_ufpoint8_acc *a);
PDC_error_t PDC_ufpoint8_acc_reset   (PDC_t *pdc, PDC_ufpoint8_acc *a);
PDC_error_t PDC_ufpoint8_acc_cleanup (PDC_t *pdc, PDC_ufpoint8_acc *a);
PDC_error_t PDC_ufpoint8_acc_add     (PDC_t *pdc, PDC_ufpoint8_acc *a, const PDC_base_pd *pd, const PDC_ufpoint8 *val);
PDC_error_t PDC_ufpoint8_acc_report  (PDC_t *pdc, const char *prefix, const char *what, int nst,
				     PDC_ufpoint8_acc *a);
float       PDC_ufpoint8_acc_avg     (PDC_t *pdc, PDC_ufpoint8_acc *a);

PDC_error_t PDC_ufpoint16_acc_init   (PDC_t *pdc, PDC_ufpoint16_acc *a);
PDC_error_t PDC_ufpoint16_acc_reset  (PDC_t *pdc, PDC_ufpoint16_acc *a);
PDC_error_t PDC_ufpoint16_acc_cleanup(PDC_t *pdc, PDC_ufpoint16_acc *a);
PDC_error_t PDC_ufpoint16_acc_add    (PDC_t *pdc, PDC_ufpoint16_acc *a, const PDC_base_pd *pd, const PDC_ufpoint16 *val);
PDC_error_t PDC_ufpoint16_acc_report (PDC_t *pdc, const char *prefix, const char *what, int nst,
				     PDC_ufpoint16_acc *a);
float       PDC_ufpoint16_acc_avg    (PDC_t *pdc, PDC_ufpoint16_acc *a);

PDC_error_t PDC_ufpoint32_acc_init   (PDC_t *pdc, PDC_ufpoint32_acc *a);
PDC_error_t PDC_ufpoint32_acc_reset  (PDC_t *pdc, PDC_ufpoint32_acc *a);
PDC_error_t PDC_ufpoint32_acc_cleanup(PDC_t *pdc, PDC_ufpoint32_acc *a);
PDC_error_t PDC_ufpoint32_acc_add    (PDC_t *pdc, PDC_ufpoint32_acc *a, const PDC_base_pd *pd, const PDC_ufpoint32 *val);
PDC_error_t PDC_ufpoint32_acc_report (PDC_t *pdc, const char *prefix, const char *what, int nst,
				     PDC_ufpoint32_acc *a);
float       PDC_ufpoint32_acc_avg    (PDC_t *pdc, PDC_ufpoint32_acc *a);

PDC_error_t PDC_ufpoint64_acc_init   (PDC_t *pdc, PDC_ufpoint64_acc *a);
PDC_error_t PDC_ufpoint64_acc_reset  (PDC_t *pdc, PDC_ufpoint64_acc *a);
PDC_error_t PDC_ufpoint64_acc_cleanup(PDC_t *pdc, PDC_ufpoint64_acc *a);
PDC_error_t PDC_ufpoint64_acc_add    (PDC_t *pdc, PDC_ufpoint64_acc *a, const PDC_base_pd *pd, const PDC_ufpoint64 *val);
PDC_error_t PDC_ufpoint64_acc_report (PDC_t *pdc, const char *prefix, const char *what, int nst,
				     PDC_ufpoint64_acc *a);
double      PDC_ufpoint64_acc_avg    (PDC_t *pdc, PDC_ufpoint64_acc *a);

#endif /* PDC_CONFIG_ACCUM_FUNCTIONS */

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
PDC_error_t PDC_dummy_read(PDC_t *pdc, const PDC_base_m *m, PDC_int32 dummy_val, PDC_base_pd *pd, PDC_int32 *res_out);

/* ================================================================================
 * INCLUDE MACRO IMPLS OF SOME OF THE FUNCTIONS DECLARED ABOVE
 */
#include "padsc-impl.h"

/* ================================================================================
 * INCLUDE THE IO DISCIPLINE DECLS
 */
#include "pdc_io_disc.h"

/* ================================================================================ */

#endif  /* __PADSC_H__ */
