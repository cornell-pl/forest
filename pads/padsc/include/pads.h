#ifdef _USE_PROTO
#pragma prototyped
#endif
/*
 * PADS library interface
 * 
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#ifndef __PADS_H__
#define __PADS_H__

#include <ast.h>
#include <ast_float.h>
//#include <ast_common.h>
#include <stdio.h>
#include <swap.h>
#include <tm.h>
#include <vmalloc.h>
#include <sfio.h>
#include <sfstr.h>
#include <ctype.h>
#include <dt.h>
#include <error.h>
#include <math.h>
#include <regex.h>
#include "rbuf.h"
#include "pads-config.h"
#include "pads-private.h"

/* ================================================================================
 * LIBRARY DISCIPLINE TYPES
 *
 * The Main Discipline Type
 * ------------------------
 *
 * Pdisc_t is the main discipline type.  This section gives an overview
 * of each field: 
 *
 *   version  : interface version
 *   flags    : control flags: some combination of the following
 *                 P_WSPACE_OK: for variable-width ASCII integers, indicates
 *                                leading white space is OK; for fixed-width ASCII
 *                                integers, indicates leading and/or trailing
 *                                white space is OK
 *
 *   fopen_fn : file open function used by P_io_fopen; if NULL, the default
 *              function P_fopen is used. See DISC FUNCTION FOR FILE OPEN below.
 *
 *   error_fn : error reporting function; if NULL, the default P_error is used
 *              See DISC FUNCTION FOR ERROR REPORTING below.
 *
 *   e_rep    : error reporting level, one of:
 *                PerrorRep_None : do not generate descriptive error reports
 *                PerrorRep_Min  : minimal reporting: report errCode, IO elt num/char position
 *                PerrorRep_Med  : medium reporting:  like Min, but adds descriptive string
 *                PerrorRep_Max  : maximum reporting, like Med, but adds offending IO elt up to error position
 *
 *   def_charset : default character set, one of:
 *                Pcharset_ASCII
 *                Pcharset_EBCDIC
 *
 *   copy_strings : if non-zero, the string read functions copy the strings found, otherwise they do not
 *                  (instead the target Pstring points to memory managed by the current IO discipline).
 *                  copy_strings should only be set to zero for record-based IO disciplines where
 *                  strings from record K are not used after P_io_next_rec has been called to move
 *                  the IO cursor to record K+1.  Note: Pstring_preserve can be used to
 *                  force a string that is using sharing to make a copy so that the string is 'preserved'
 *                  (remains valid) across calls to P_io_next_rec.
 *
 *   d_endian  : data endian-ness    (PbigEndian or PlittleEndian)
 *                 If d_endian != the endian-ness of the machine running the parsing code,
 *                 byte order of binary integers is swapped 
 *                 by the binary integer read functions.  See comments below about
 *                 the CHECK_ENDIAN pragma.
 *
 *   acc_max2track : default maximum distinct values for accumulators to track.
 *                 Use value P_MAX_UINT64 to indicate no limit.
 *                 Upon calling an acc_init function on some base-type accumulator a,
 *                 a.max2track is set to pads->disc->acc_max2track, the default
 *                 limit on number of distinct to keep track of.
 *                 a.max2track can be modified directly after this call to force
 *                 accumulator a to use a non-default value.
 *
 *   acc_max2rep : default number of tracked values for accumulator to describe in detail in report.
 *                 Use value P_MAX_UINT64 to indicate no limit.
 *                 Upon calling an acc_init function on some base-type accumulator a,
 *                 a.max2rep is set to pads->disc->acc_max2rep, the default
 *                 number of tracked values to describe in detail.
 *                 a.max2rep can be modified directly after this call to force
 *                 accumulator a to use a non-default value.
 *
 *   acc_pcnt2rep : default percent of values for accumulator to describe in detail in report.
 *                 Use value 100.0 to indicate no limit.
 *                 Upon calling an acc_init function on some base-type accumulator a,
 *                 a.pcnt2rep is set to pads->disc->acc_pcnt2rep, the default
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
 *              Pio_disc_t which is used to enable reading different kinds
 *              of data files.  See io_disc.h for details.
 *              Also see 'Changing The IO Discipline' below.
 *
 *   in_time_zone :
 *              The default time zone for string-based Pdate input, used for input date strings
 *              that do not have time zone information in them.     For example, the date
 *                        15/Oct/1997:18:46:51
 *              has no time zone information.  If in_time_zone is set  to "UTC",
 *              then this date/time would be assumed to be a UTC time.
 *
 *              In contrast, regardless of the in_time_zone setting, the date
 *                        15/Oct/1997:18:46:51 -0700
 *              will always be interpreted as being in a time zone seven hours
 *              earlier than UTC time.
 * 
 *              in_time_zone is passed to the tmzone function, so it must be a time
 *              zone description that tmzone understands.
 *
 *              N.B. disc->in_time_zone can be initialized directly before calling
 *              P_open.  After calling P_open, however, it must be changed by passing
 *              the pads handle and a time zone string to P_set_in_time_zone, e.g.,
 *
 *                         P_set_in_time_zone(pads, "PDT");
 *
 *              This will set pads->disc->in_time_zone, and will also update
 *              an internal representation of this time zone maintained as part of
 *              the pads state.
 *
 *   output_time_zone : The output time zone for formatted Pdate output.
 *              Regardless of the time zone used to read in a Pdate,
 *              disc->output_time_zone controls which time zone is used when
 *              formatting the date for output.  For example, a Pdate that is read as 6am UTC time
 *              would be formatted as 1am if the output_time_zone is "-0500".
 *              Note that in the normal case you should use the same time zone
 *              for both input and output, unless you are intentially translating
 *              times from one time zone to another one.
 *              
 *              N.B. disc->output_time_zone can be initialized directly before calling
 *              P_open.  After calling P_open, however, it must be changed by passing
 *              the pads handle and a time zone string to P_set_output_time_zone, e.g.,
 *
 *                         P_set_output_time_zone(pads, "PDT");
 *
 *              This will set pads->disc->output_time_zone, and will also update
 *              an internal representation of this time zone maintained as part of
 *              the pads state.
 *
 *  Default input formats
 *  ---------------------
 *  The in_formats field of the discipline allows one to specify
 *  default input formats for some special types where there is 
 *  in no 'obvious' default.  The only current entry is:
 *
 *     in_formats.date : a format string specifying the input date format;
 *                       alternatives can be given using %|, and the
 *                       special %& format can be used to indicate all
 *                       formats that can be parsed by the tmdate
 *                       fuction.  The default,
 *
 *                         "%m%d%y%|%m%d%Y%|%&"
 *
 *                       allows for dates of these two forms:
 *                               091172
 *                               09111972
 *                       and also allows for all dates that can be parsed
 *                       by tmdate.
 *
 *                   (See the documentation of the libast tmscan and tmdate functions
 *                    for a description of the legal values for in_formats.date)
 *
 *  Default output formats
 *  ----------------------
 *  The out_formats field of the discipline allows one to specify
 *  default output formats for some special types where there is 
 *  in no 'obvious' default.  The only current entry is:
 *
 *     out_formats.date : a format string specifying the date format
 *                    some examples:
 *                        "%Y-%m-%d"  (default)
 *                        "%m/%d/%Y"
 *                        "%K" (which is the same as "%Y-%m-%d+%H:%M:%S")
 *
 *                     (See the documentation of the libast fmttime function
 *                      for a description of the legal values for formats.date)
 *
 *  Limiting the scope of scanning and pattern matching
 *  ---------------------------------------------------
 *
 *  When scanning for a literal or regular expression match, how far
 *  should the scan/match go before giving up?    If a record-based IO discipline is
 *  used, scanning and matching is limited to the scope of a single record.  In
 *  addition, the following 3 Pdisc_t fields can be used to provide further
 *  constraints on scan/match scope.  
 *
 *  match_max:   Maximum # of bytes that will be included in an
 *               inclusive pattern match attempt (see, e.g., data type
 *               Pstring_ME).  If set to 0, no match_max constraint is imposed
 *               for a record-based IO discipline (other than finding
 *               end-of-record), whereas a built-in soft limit of
 *               P_BUILTIN_MATCH_MAX characters is imposed for non-record-based
 *               IO disciplnes.  (The built-in limit is soft because if the match
 *               happens to get more than P_BUILTIN_MATCH_MAX characters in a
 *               single IO discipline read call it will go ahead and consider all
 *               of them.  In contrast, if the discipline match_max is set
 *               explicitly to value K, then this is a hard limit: the match will
 *               only consider K characters even if more are available.)
 *
 *  numeric_max: Maximum # of bytes that will be included in an
 *               attempt to read a character-based representation of a number.  If
 *               non-zero, should be set large enough to cover any leading white
 *               space (if allowed by P_WSPACE_OK), an optional +/- sign, and the
 *               digits (dot etc. for floats) that make up the numeric value.  A
 *               numeric_max of 0 results in an end-of-record constraint for
 *               record-based IO disciplines and in a soft limit of
 *               P_BUILTIN_NUMERIC_MAX bytes for non-record-based IO disciplines.
 *
 *  scan_max :   Maximum # of bytes that will be considered by a normal
 *               scan that is looking for a terminating literal or a terminating
 *               regular expressin (see, e.g., data type Pstring_SE.).  Note
 *               that this includes both the bytes skipped plus the bytes used for
 *               the match.  A scan_max of 0 results in an end-of-record
 *               constraint for record-based IO disciplines and in a soft limit of
 *               P_BUILTIN_SCAN_MAX bytes for non-record-based IO disciplines.
 * 
 *  panic_max :  Maximum # of bytes that will be considered by when
 *               parsing hits a 'panic' state and is looking for a synchronizing
 *               literal or pattern.  See, for example, termination conditions for
 *               user-defined array types.  A panic_max of 0 results in an
 *               end-of-record constraint for record-based IO disciplines and in a
 *               soft limit of P_BUILTIN_PANIC_MAX bytes for non-record-based IO
 *               disciplines.
 *
 *   ** N.B.: For non-record-based IO disciplines, the default soft limits may
 *            be either too small or too large for a given input type.  It is
 *            important important to determine appropriate hard limit settings.
 *
 *  The built-in soft limits for use with non-record-based IO disciplines are
 *  as follows.  Although you can change them and recompile the PADS library,
 *  it is easier to simply set up the correct hard limits in the discipline.
 */

#define P_BUILTIN_MATCH_MAX       512
#define P_BUILTIN_SCAN_MAX        512
#define P_BUILTIN_NUMERIC_MAX     512
#define P_BUILTIN_PANIC_MAX      1024

/*
 *
 * Specifying what value to write during write calls when an invalid value is present
 * ----------------------------------------------------------------------------------
 *
 * Write functions take a parse descriptor and a value.  The value is valid if the
 * parse descriptor's errCode is set P_NO_ERR.  The value has been filled in if the
 * errCode is P_USER_CONSTRAINT_VIOLATION.  For other errCodes, the value should be
 * assumed to contain garbage.  For invalid values, the write function must still
 * write SOME value.  For every type, one can specify an inv_val helper function
 * that produces an invalid value for the type, to be used by the type's write
 * functions.  If no function is specified, then a default invalid value is used,
 * where there are two cases: if the errorCode is P_USER_CONSTRAINT_VIOLATION, then
 * the current invalid value is used; for any other errorCode, a default invalid
 * value is used.
 *
 * The map from write functions to inv_val functions is found in the discipline:
 *
 *    inv_val_fn_map: map from const char* (string form of the type name)
 *                        to Pinv_val_fn function
 *                   can be NULL, in which case no mapping are used
 *
 * An invalid val function that handles type T values always takes 4 arguments:
 *       1. The P_t* handle
 *       2. A pointer to a type T parse descriptor
 *       3. A pointer to the invalid type T rep
 *       4. va_list argument that encodes the extra type parameters (if any)
 *          associated with the type.  For example,
 *          type Pa_int32_FW(:<width>:) has a single type parameter (width) of type Puint32,
 *          so va_list would encode a single Puint32 argument.
 *   Args 2+3 use void* types to enable the table to be used with arbitrary types,
 *   including user-defined types.  One must cast these void* args to the appropriate
 *   pointer types before use -- see the example below.  The function should
 *   replace the invalid value with a new value, and return P_OK on
 *   success and P_ERR if a replacement value has not been set.
 *
 * Use P_set_inv_val_fn to set a function ptr, P_get_inv_val_fn to do a lookup.
 *
 * EXAMPLE: suppose an a_int32 field has an attached constraint that requires the
 * value must be >= -30.  If a value of -50 is read, errCode will be
 * P_USER_CONSTRAINT_VIOLATION, and if no inv_val function is provided then the
 * a_int32 write function will output -50.  If the read function fails to read even a
 * valid integer, the errCode will be P_INVALID_A_NUM, and the a_int32 write
 * function will output P_MIN_INT32 (the default invalid value for all int32 write
 * functions). If one wanted to correct all user contraint cases to use value -30, and
 * to use P_MAX_INT32 for other invalid cases, one could provide an inv_val
 * helper function to do so:
 *
 *   Perror_t my_int32_inv_val(P_t *pads, void *pd_void, void *val_void, va_list type_args) {
 *     Pbase_pd *pd  = (Pbase_pd*)pd_void;
 *     Pint32   *val = (Pint32*)val_void;
 *     if (pd->errCode == P_USER_CONSTRAINT_VIOLATION) {
 *       (*val) = -30;
 *     } else {
 *       (*val) = P_MAX_INT32;
 *     }
 *     return P_OK;
 *   }
 *
 *   pads->disc->inv_val_fn_map = Pinv_val_fn_map_create(pads);   (only needed if no map installed yet)
 *   P_set_inv_val_fn(pads, pads->disc->inv_val_fn_map, "Pint32", my_int32_inv_val);
 *
 * N.B. Note that for a type T with three forms, P_T, Pa_T, and Pe_T, there
 * is only one entry in the inv_val_fn_map, under string "P_T".  For example, use
 * "Pint32" to specify an invalid val function for all of these types: Pint32,
 * Pa_int32, Pe_int32.
 *
 * N.B. An inv_val_fn for a string type should use Pstring_copy, Pstring_cstr_copy,
 * Pstring_share, or Pstring_cstr_share to fill in the value of the Pstring* param.
 *
 * Replacing the default behavior of fmt2io functions
 * --------------------------------------------------
 * XXX_TODOC
 *
 * The default discipline
 * ----------------------
 * 
 * The default disc is Pdefault_disc.  It provides the following defaults:
 *    version:            P_VERSION (above) 
 *    flags:              0
 *    def_charset:        Pcharset_ASCII
 *    copy_strings:       0
 *    match_max:          0
 *    scan_max:           0
 *    panic_max:          0
 *    fopen_fn:           P_fopen
 *    error_fn:           P_error
 *    e_rep:              PerrorRep_Max
 *    d_endian:           PlittleEndian
 *    acc_max2track:      1000
 *    acc_max2rep:        10
 *    in_time_zone:       "UTC"
 *    out_time_zone:      "UTC"
 *    in_formats.date:    "%m%d%y%|%m%d%Y%|%&" 
 *    out_formats.date:   "%Y-%m-%d"
 *    inv_val_fn_map:     NULL -- user must created and install a map
 *                         if inv_val functions need to be provided
 *    fmt_fn_map:         NULL -- user must created and install a map
 *                         if fmt functions need to be provided
 *    io_disc:            NULL -- a default IO discipline (newline-terminated records)
 *                         is installed on P_open if one is not installed beforehand
 *
 *
 * Initializing a PADS handle
 * --------------------------
 *   XXX_TODOC
 *
 * Here is an example initialization that modifies the constructs a discipline
 * object, my_disc, and allocates an instance of the 'norec' IO discpline
 * to be the IO discipline:
 *
 *     P_t *pads;
 *     Pio_disc_t* norec;
 *     Pdisc_t my_disc = Pdefault_disc;
 *     my_disc.flags |= (Pflags_t)P_WSPACE_OK;
 *     norec = P_norec_make(0);
 *     if (P_ERR == P_open(&pads, &my_disc, norec)) {
 *       fprintf(stderr, "Failed to open PADS library handle\n");
 *       exit(-1);
 *     }
 *     -- start using pads
 *
 * If we are willing to use the default IO discipline we could have used:
 *        
 *     P_t *pads;
 *     Pdisc_t my_disc = Pdefault_disc;
 *     my_disc.flags |= (Pflags_t)P_WSPACE_OK;
 *     if (P_ERR == P_open(&pads, &my_disc, 0)) {
 *       fprintf(stderr, "Failed to open PADS library handle\n");
 *       exit(-1);
 *     }
 *     -- start using pads
 *
 * Similarly, if we do not need to modify the default discipline:
 *
 *     P_t *pads;
 *     if (P_ERR == P_open(&pads, 0, 0)) {
 *       fprintf(stderr, "Failed to open PADS library handle\n");
 *       exit(-1);
 *     }
 *     -- start using pads
 *
 * Changing The Main Discipline
 * -----------------------------
 *   XXX_TODOC
 *     Pdisc_t my_disc = Pdefault_disc;
 *     my_disc.flags |= (Pflags_t)P_WSPACE_OK;
 *     P_set_disc(pads, &my_disc, 1);
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
 *       Pio_disc_t* fwrec;
 *       ..
 *       fwrec = P_fwrec_make(0, 30, 2);
 *       P_set_io_disc(pads, fwrec, 1);
 *
 *  The third arg value of 1 indicates the current sfio stream
 *  should be transferred to the new IO discipline.  If this is not done,
 *  XXX_TODOC.  */

/* ================================================================================
 * CONSTANTS
 */

#define P_VERSION                  20020815L

typedef enum Perror_t_e {
  P_OK                            =    0,
  P_ERR                           =   -1
} Perror_t;

typedef enum PerrCode_t_e {
  /* First set of errors have no corresponding location  */
  P_NOT_PARSED                    =    0,
  P_NO_ERR                        =    1,
  P_SKIPPED                       =    2, 

  P_UNEXPECTED_ERR                =    3,

  P_BAD_PARAM                     =    4,
  P_SYS_ERR                       =    5,
  P_IO_ERR                        =    6,

  P_CHKPOINT_ERR                  =   11,
  P_COMMIT_ERR                    =   12,
  P_RESTORE_ERR                   =   13,
  P_ALLOC_ERR                     =   14,
  P_FORWARD_ERR                   =   15,
  P_PANIC_SKIPPED                 =   20,

  P_FMT_EMPTY_DELIM_ERR           =   30,

  P_INVALID_FUNCTION_CALL         =   50,

  /* A general error relating to smart nodes*/
  P_SMART_NODE_ERR                =   60,

  /* 
   * A general error that requires that an ocaml 
   * ocaml exception be thrown.
   */
  P_FAILWITH_ERR                  =   61,

  /* The following errors (code >= 100) DO have a corresponding location  */
  P_USER_CONSTRAINT_VIOLATION     =  100,
  P_MISSING_LITERAL               =  101,
  P_ARRAY_ELEM_ERR                =  110,
  P_ARRAY_SEP_ERR                 =  111,
  P_ARRAY_TERM_ERR                =  112,
  P_ARRAY_SIZE_ERR                =  113,
  P_ARRAY_SEP_TERM_SAME_ERR       =  114,      
  P_ARRAY_USER_CONSTRAINT_ERR     =  115,
  P_ARRAY_MIN_BIGGER_THAN_MAX_ERR =  116,
  P_ARRAY_MIN_NEGATIVE            =  117,
  P_ARRAY_MAX_NEGATIVE            =  118,
  P_ARRAY_EXTRA_BEFORE_SEP        =  119,
  P_ARRAY_EXTRA_BEFORE_TERM       =  120,

  P_STRUCT_FIELD_ERR              =  125,
  P_STRUCT_EXTRA_BEFORE_SEP       =  126,
  P_UNION_MATCH_ERR               =  130,
  P_OPTION_MATCH_ERR              =  130,
  P_ENUM_MATCH_ERR                =  140,
  P_TYPEDEF_CONSTRAINT_ERR        =  150,

  P_AT_EOF                        =  160,
  P_AT_EOR                        =  161,
  P_EXTRA_BEFORE_EOR              =  162,
  P_EOF_BEFORE_EOR                =  163,
  P_COUNT_MAX_LIMIT               =  164,
  P_RANGE                         =  170,

  P_INVALID_A_NUM                 =  180,
  P_INVALID_E_NUM                 =  181,
  P_INVALID_EBC_NUM               =  182,
  P_INVALID_BCD_NUM               =  183,

  P_INVALID_CHARSET               =  190,
  P_INVALID_WIDTH                 =  191,

  P_CHAR_LIT_NOT_FOUND            =  200,
  P_STR_LIT_NOT_FOUND             =  210,
  P_REGEXP_NOT_FOUND              =  220,
  P_INVALID_REGEXP                =  230,
  P_WIDTH_NOT_AVAILABLE           =  240,
  P_INVALID_DATE                  =  250,
  P_INVALID_DATE_WIDTH            =  251,
  P_INVALID_IPADDR                =  260
} PerrCode_t;

typedef enum Pread_res_t_e{
  
  /*
   * The read call placed data in 
   * the "out" parameters.
   */
  P_READ_OK_DATA,

  /*
   * The read call did not place data in the "out" parameters.
   * Does not imply that there was an error. For example, satisfaction
   * of Pomit or consume (in Pended) can both cause P_READ_OK_NO_DATA to
   * be returned.
   */
  P_READ_OK_NO_DATA,

  /*
   *  All error codes imply that there was no data.
   */
  P_READ_ERR                  = P_ERR,
  P_READ_ALREADY_DONE_ERR,
  P_READ_BAD_INDEX_ERR,

  /*
   * A hard error occurred.
   */
  P_READ_FAILURE
} Pread_res_t;

/* PerrCode_t as string */
const char *P_errCode2str(PerrCode_t code);

/* parse state flags */
#define P_Panic               0x0001
/* more flags will be added later to support partial-read functionality */ 
#define P_Partial             0x0002  /* final state not reached, more reading required */
 
/*
 * Other useful constants
 */

#define P_MIN_INT8                         -128
#define P_MAX_INT8                          127
#define P_MAX_UINT8                         255U

#define P_MIN_INT16                      -32768
#define P_MAX_INT16                       32767
#define P_MAX_UINT16                      65535U

#define P_MIN_INT24                    -8388608
#define P_MAX_INT24                     8388607
#define P_MAX_UINT24                   16777215U

#define P_MIN_INT32                 -2147483647L   /* should end in 8 but gcc does not like that */
#define P_MAX_INT32                  2147483647L
#define P_MAX_UINT32                 4294967295UL

#define P_MIN_INT40               -549755813888LL
#define P_MAX_INT40                549755813887LL
#define P_MAX_UINT40              1099511627775ULL

#define P_MIN_INT48            -140737488355328LL
#define P_MAX_INT48             140737488355327LL
#define P_MAX_UINT48            281474976710655ULL

#define P_MIN_INT56          -36028797018963968LL
#define P_MAX_INT56           36028797018963967LL
#define P_MAX_UINT56          72057594037927935ULL

#define P_MIN_INT64        -9223372036854775807LL  /* should end in 8 but gcc does not like that */
#define P_MAX_INT64         9223372036854775807LL
#define P_MAX_UINT64       18446744073709551615ULL

#define P_MIN_FLOAT32      ((float)-3.40282346638528860e+38)
#define P_MAX_FLOAT32      ((float)3.40282346638528860e+38)

#define P_MIN_FLOAT64      ((double)-1.79769313486231470e+308)
#define P_MAX_FLOAT64      ((double)1.79769313486231470e+308)

/* USEFUL ASCII AND EBCDIC CHAR CONSTANTS */

#define P_ASCII_NEWLINE '\n'
#define P_EBCDIC_NEWLINE 0x25
/* N.B. EBCDIC 0x15 is used on some systems for LF, 0x25 on others */

#define P_ASCII_SPACE ' '
#define P_EBCDIC_SPACE 0x40

#define P_ASCII_PLUS '+'
#define P_EBCDIC_PLUS 0x4e

#define P_ASCII_MINUS '-'
#define P_EBCDIC_MINUS 0x60

#define P_ASCII_DOT '.'
#define P_EBCDIC_DOT 0x4b

/* DEFAULT 'invalid value' VALUES */

#define P_CHAR_DEF_INV_VAL     P_MAX_UINT8

#define P_INT8_DEF_INV_VAL     P_MIN_INT8
#define P_UINT8_DEF_INV_VAL    P_MAX_UINT8
#define P_INT16_DEF_INV_VAL    P_MIN_INT16
#define P_UINT16_DEF_INV_VAL   P_MAX_UINT16
#define P_INT32_DEF_INV_VAL    P_MIN_INT32
#define P_UINT32_DEF_INV_VAL   P_MAX_UINT32
#define P_INT64_DEF_INV_VAL    P_MIN_INT64
#define P_UINT64_DEF_INV_VAL   P_MAX_UINT64
#define P_FLOAT32_DEF_INV_VAL  P_MIN_FLOAT32
#define P_FLOAT64_DEF_INV_VAL  P_MIN_FLOAT64

/* ================================================================================
 * INTERFACE LIBRARY TYPES: FORWARD DECLS
 *
 *
 * The struct and enum decls for these types are in this file:
 *     P_t*        : runtime library handle (opaque)
 *                      initialized with P_open, passed as first arg to most library routines
 *     Pdisc_t*   : handle to discipline
 *     Pin_formats_t  : default input format descriptions, a component of Pdisc_t
 *     Pout_formats_t : default output format descriptions, a component of Pdisc_t
 *     Pregexp_t* : handle to a compiled regular expression
 *
 *     Ppos_t     : IO position
 *     Ploc_t     : IO location / range
 *     Pbase_pd   : base parse descriptor
 *     Pbase_m    : base mask
 *     PerrorRep  : enum for specifying error reporting level
 *     Pendian_t  : enum for specifying endian-ness
 *     Pcharset   : enum for specifying character set
 * 
 * The struct type decls for these types are in io_disc.h:
 *     Pio_disc_t : sub-discipline type for controlling IO
 *     Pio_elt_t  : element of a linked list managed by the IO discipline 
 */

typedef struct P_s              P_t;
typedef struct Pdisc_s          Pdisc_t;
typedef struct Pin_formats_s    Pin_formats_t;
typedef struct Pout_formats_s   Pout_formats_t;
typedef struct Pregexp_s        Pregexp_t;

typedef struct Ppos_s           Ppos_t;
typedef struct Ploc_s           Ploc_t;
typedef struct Pbase_pd_s       Pbase_pd;
typedef enum   PerrorRep_e      PerrorRep;
typedef enum   Pendian_e        Pendian_t;
typedef enum   Pcharset_e       Pcharset;

typedef struct Pio_elt_s        Pio_elt_t;
typedef struct Pio_disc_s       Pio_disc_t;

/* ================================================================================
 * BASIC LIBRARY TYPES
 */

typedef unsigned char          Pbyte;

typedef signed _ast_int1_t     Pint8;
typedef signed _ast_int2_t     Pint16;
typedef signed _ast_int4_t     Pint32; 
typedef signed _ast_int8_t     Pint64; 

typedef unsigned _ast_int1_t   Puint8;
typedef unsigned _ast_int2_t   Puint16;
typedef unsigned _ast_int4_t   Puint32;
typedef unsigned _ast_int8_t   Puint64;

typedef	struct { Pint8   num; Puint8  denom;} Pfpoint8;
typedef	struct { Pint16  num; Puint16 denom;} Pfpoint16;
typedef	struct { Pint32  num; Puint32 denom;} Pfpoint32;
typedef	struct { Pint64  num; Puint64 denom;} Pfpoint64;

typedef	struct { Puint8  num; Puint8  denom;} Pufpoint8;
typedef	struct { Puint16 num; Puint16 denom;} Pufpoint16;
typedef	struct { Puint32 num; Puint32 denom;} Pufpoint32;
typedef	struct { Puint64 num; Puint64 denom;} Pufpoint64;

typedef Puint8 Pchar;

typedef _ast_flt4_t   Pfloat32;
typedef _ast_flt8_t   Pfloat64;

/* HELPERS: 
 *    P_FPOINT2FLOAT32 calculates num/denom as a Pfloat32
 *    P_FPOINT2FLOAT64 calculates num/denom as a Pfloat64
 */
#define P_FPOINT2FLOAT32(fp) ((fp).num/(Pfloat32)(fp).denom)
#define P_FPOINT2FLOAT64(fp) ((fp).num/(Pfloat64)(fp).denom)

/* flags are Puint32 values */
typedef Puint32 Pflags_t;

#ifdef FOR_CKIT
extern Puint32 P_NULL_CTL_FLAG;
extern Puint32 P_WSPACE_OK;
#else
#define P_NULL_CTL_FLAG      0UL
#define P_WSPACE_OK          1UL
#endif /* FOR_CKIT */

/* PDCI_id_t: type of the internal identifier generated for use with 
   systems needing unique identifiers for each node in a pads data structure
   (for example, Galax). 
 */
typedef Puint64 PDCI_id_t;

/* ================================================================================
 * Pstring: PADS strings have a ptr and length;
 *             required since they need not be null-terminated.
 *             They also have some private state, which should
 *             be ignored by users of the library.
 */

typedef struct Pstring_s Pstring;

/* type Pstring: */
struct Pstring_s {
  char             *str;
  size_t            len;
  P_STRING_PRIVATE_STATE;
};

/* ================================================================================
 * STRING HELPER FUNCTIONS
 *
 *    Pstring_init       : initialize to valid empty string (no dynamic memory allocated yet)
 *
 *    Pstring_cleanup    : free up the rbuf and any allocated space for the string
 *
 *    Pstring_share      : makes the Pstring targ refer to the string in Pstring src,
 *                            sharing the space with the original owner.
 *
 *    Pstring_cstr_share : makes the Pstring targ refer len chars in the C-style string src.
 *
 *       Note on sharing: the original space for the string (src) must not be 'cleaned up' while
 *                        the targ Pstring continues to be used.  One can use Pstring_preserve
 *                        on targ if it becomes necessary to copy the string into targ at a later point.
 *
 *    Pstring_copy      : Copy src Pstring into targ Pstring; sharing is not used.
 *
 *    Pstring_cstr_copy : copy len chars from C-style string src into the Pstring targ;
 *                           sharing is not used.
 *
 *       Both copy functions allocate an RBuf and/or space for the copy, as necessary.
 *       Although not strictly necessary, they also null-terminate targ->str.
 *       They return P_ERR on bad arguments or on failure to alloc space, otherwise P_OK.
 *
 *    Pstring_preserve  : If the string is using space-sharing, force it use a private copy 
 *                            instead, so that the (formerly) shared space can be discarded.
 *                            It is safe to call preserve on any Pstring.
 *
 * String comparison:
 *
 *    Pstring_eq        : compares two Pstring, str1 and str2.
 *                            returns 0 if str1 equals str2, a negative # if str1 < str2,
 *                            and a positive # if str1 > str2.
 *
 *    Pstring_eq_cstr   : compare Pstring str to a C-style string cstr.
 *                            returns 0 if str equals cstr, a negative # if str < cstr,
 *                            and a positive # if str > cstr.
 *
 * ----------------------------
 * HELPER MACROS for Pstring
 * ----------------------------
 * Pstring helper macros have 2 forms.  The INIT forms are used to
 * initialize a a Pstring that has already been declared but has not been
 * initialized.  The DECL forms are used to both declare a new Pstring and
 * to initialize it.  The DECL forms produce C variabled declarations, and
 * must appear at the beginning of a C scope with other variable declarations
 * (before any normal code).  The arguments to the DECL forms must be
 * valid for use in a C struct initializing declaration.
 *
 * P_STRING_INIT_NULL(my_str);
 *
 * ==> Initializes my_str to a valid null state, where my_str is assumed to have
 * been declared but not yet initialized.
 *
 * P_STRING_INIT_LIT(my_str, "foo");
 *
 * ==> Initializes Pstring my_str to refer to a string literal.  It uses
 * string-sharing mode so that the string will not attempt to free the string
 * literal on cleanup.
 *
 * P_STRING_INIT_CSTR(my_str, char_ptr_expr);
 *
 * ==> Initializes my_str to contain the C string produced by char_ptr_expr.
 * String-sharing mode is used so that my_str will not attempt free the string
 * referred to by char_ptr_expr.
 *
 * P_STRING_INIT_CSTR_LEN(my_str, char_ptr_expr, length_expr);
 *
 * ==> Like the previous macro except that my_str.len is set to the value of
 * length_expr instead of using strlen(char_ptr_expr) to obtain the length.  The
 * _LEN form is useful when the character(s) to be shared are not
 * null-terminated.
 *
 * The corresponding DECL forms are:
 *
 * P_STRING_DECL_NULL(my_str);
 * P_STRING_DECL_LIT(my_str, "foo");
 * P_STRING_DECL_CSTR(my_str, char_ptr_expr);
 * P_STRING_DECL_CSTR_LEN(my_str, char_ptr_expr, length_expr);
 */

Perror_t Pstring_init(P_t *pads, Pstring *s);
Perror_t Pstring_cleanup(P_t *pads, Pstring *s);
Perror_t Pstring_share(P_t *pads, Pstring *targ, const Pstring *src);
Perror_t Pstring_cstr_share(P_t *pads, Pstring *targ, const char *src, size_t len);
Perror_t Pstring_copy(P_t *pads, Pstring *targ, const Pstring *src);
Perror_t Pstring_cstr_copy(P_t *pads, Pstring *targ, const char *src, size_t len);
Perror_t Pstring_preserve(P_t *pads, Pstring *s);

Pint8    Pstring2int8  (const Pstring *str);  /* returns P_MIN_INT8 on error */ 
Pint16   Pstring2int16 (const Pstring *str);  /* returns P_MIN_INT16 on error */ 
Pint32   Pstring2int32 (const Pstring *str);  /* returns P_MIN_INT32 on error */ 
Pint64   Pstring2int64 (const Pstring *str);  /* returns P_MIN_INT64 on error */ 

Puint8   Pstring2uint8 (const Pstring *str);  /* returns P_MAX_UINT8 on error */ 
Puint16  Pstring2uint16(const Pstring *str);  /* returns P_MAX_UINT16 on error */ 
Puint32  Pstring2uint32(const Pstring *str);  /* returns P_MAX_UINT32 on error */ 
Puint64  Pstring2uint64(const Pstring *str);  /* returns P_MAX_UINT64 on error */ 

Pfloat32 Pstring2float32(const Pstring *str); /* returns P_MIN_FLOAT32 on error */
Pfloat64 Pstring2float64(const Pstring *str); /* returns P_MIN_FLOAT64 on error */

#ifdef FOR_CKIT
int Pstring_eq(const Pstring *str1, const Pstring *str2);
int Pstring_eq_cstr(const Pstring *str, const char *cstr);

void P_STRING_INIT_NULL(Pstring my_str);
void P_STRING_INIT_LIT(Pstring my_str, const char *lit);
void P_STRING_INIT_CSTR(Pstring my_str, const char *expr);
void P_STRING_INIT_CSTR_LEN(Pstring my_str, char *expr, size_t length_expr);
#endif /* FOR_CKIT */

/*
 * A base type T with T_init/T_cleanup must also have T_pd_init/T_pd_cleanup.
 * Similarly, if T has T_copy, it must also have T_pd_copy.
 *
 * For Pstring_ed, which is just a Pbase_pd, init and cleanup are no-ops,
 * while copy has a trivial implementation (struct assignment).
 */

Perror_t Pstring_pd_init(P_t *pads, Pbase_pd *pd);
Perror_t Pstring_pd_cleanup(P_t *pads, Pbase_pd *pd);
Perror_t Pstring_pd_copy(P_t *pads, Pbase_pd *targ, const Pbase_pd *src);

/* ================================================================================
 * DISC FUNCTION FOR FILE OPEN:
 *
 * A Pfopen_fn takes arguments (source, mode) and 
 * opens file source in the specified mode and returns 
 * an sfio stream on success or NULL on failure.
 *
 * It should normally have the the same semantics as the call
 * sfopen(NiL, string, mode), except that for 3 string constants
 * it should return an existing sfio stream:
 *
 *        "/dev/stdin"    -->    sfstdin
 *        "/dev/stdout"   -->    sfstdout
 *        "/dev/stderr"   -->    sfstderr
 *
 * For /dev/stdin, mode "r" (read) is expected.
 * For /dev/stdout or /dev/stderr, mode "a" (append-only) is expected.
 * If you use some other mode for these 3 cases, the function 
 * should attempt to apply mode to the specified sfio stream;
 * it should return NULL if this fails, otherwise the specified stream.
 */

typedef Sfio_t* (*Pfopen_fn)(const char *source, const char *mode);

/*
 * The default implementation:
 */
Sfio_t *P_fopen(const char* source, const char* mode);

/* ================================================================================
 * DISC FUNCTION FOR ERROR REPORTING
 *
 * A Perror_fn function is an output function that output a
 * formatted error message, where level should be one of:
 *      -K : negative # is used for debugging messages
 *       P_LEV_INFO  : informative, no prefixes appended to message
 *       P_LEV_WARN  : warning
 *       P_LEV_ERR   : soft error
 *       P_LEV_FATAL : fatal error, program should exit 
 * One can 'or' in the following flags (as in P_LEV_WARN|P_FLG_PROMPT):
 *       P_FLG_PROMPT  : do not emit a newline
 *       P_FLG_SYSERR  : add a description of errno (errno should be a system error)
 *       P_FLG_LIBRARY : error is from library
 * Given a level lev that may include flags, one can use:
 *   P_GET_LEV(lev) : just the level   example: P_GET_LEV(lev) == P_LEV_FATAL
 *   P_GET_FLG(lev) : just the flags   example: P_GET_FLG(lev) & P_FLG_PROMPT
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
 * then flag P_FLG_LIBRARY is automatically or'd into level.  In the normal
 * case, a null libnm should be used. 
 */

typedef int (*Perror_fn)(const char *libnm, int level, ...);

/*
 * The default implementation:
 */

int P_error(const char *libnm, int level, ...);

/* ================================================================================
 * LIBRARY TYPES
 */

/* type Pbase_m: */
typedef Puint32 Pbase_m;

#ifdef FOR_CKIT
/* Declarations for CKIT */
extern Puint32 P_Set;
extern Puint32 P_SynCheck;
extern Puint32 P_SemCheck;
extern Puint32 P_DbgRead;
extern Puint32 P_Write;
extern Puint32 P_WriteVoid;
extern Puint32 P_WriteMeta;

extern Puint32 P_CheckAndSet;
extern Puint32 P_BothCheck;
extern Puint32 P_Ignore;

Puint32 P_Test_Set(Puint32 m);
Puint32 P_Test_SynCheck(Puint32 m);
Puint32 P_Test_SemCheck(Puint32 m);
Puint32 P_Test_DbgRead(Puint32 m);
Puint32 P_Test_Write(Puint32 m);
Puint32 P_Test_WriteVoid(Puint32 m);
Puint32 P_Test_WriteMeta(Puint32 m);

Puint32 P_Test_NotSet(Puint32 m);
Puint32 P_Test_NotSynCheck(Puint32 m);
Puint32 P_Test_NotSemCheck(Puint32 m);
Puint32 P_Test_NotDbgRead(Puint32 m);
Puint32 P_Test_NotWrite(Puint32 m);
Puint32 P_Test_NotWriteVoid(Puint32 m);
Puint32 P_Test_NotWriteMeta(Puint32 m);

Puint32 P_Test_CheckAndSet(Puint32 m);
Puint32 P_Test_BothCheck(Puint32 m);
Puint32 P_Test_Ignore(Puint32 m);

Puint32 P_Test_NotCheckAndSet(Puint32 m);
Puint32 P_Test_NotBothCheck(Puint32 m);
Puint32 P_Test_NotIgnore(Puint32 m);

Puint32 P_Test_WriteOrWriteVoid(Puint32 m);
Puint32 P_Test_NotWriteOrWriteVoid(Puint32 m);

void    P_Do_Set(Puint32 m);
void    P_Do_SynCheck(Puint32 m);
void    P_Do_SemCheck(Puint32 m);
void    P_Do_DbgRead(Puint32 m);
void    P_Do_Write(Puint32 m);
void    P_Do_WriteVoid(Puint32 m);
void    P_Do_WriteMeta(Puint32 m);

void    P_Dont_Set(Puint32 m);
void    P_Dont_SynCheck(Puint32 m);
void    P_Dont_SemCheck(Puint32 m);
void    P_Dont_DbgRead(Puint32 m);
void    P_Dont_Write(Puint32 m);
void    P_Dont_WriteVoid(Puint32 m);
void    P_Dont_WriteMeta(Puint32 m);

#else
/* The actual declarations */

/* Mask flags used with read functions */
#define P_Set                 0x0001
#define P_SynCheck            0x0002
#define P_SemCheck            0x0004
#define P_DbgRead             0x0008

/* Mask flags used with write functions */
#define P_Write               0x0010
#define P_WriteVoid           0x0020
#define P_WriteMeta           0x0040

/* Useful Combinations of Mask Flags */
#define P_CheckAndSet         0x0007     /* P_Set|P_SynCheck|P_SemCheck */
#define P_BothCheck           0x0006     /* P_SynCheck|P_SemCheck */
#define P_Ignore              0x0000     /* none of the checks, no set */

/* Useful macros for testing or modifying mask bits */

#define P_Test_Set(m)              ((m) & P_Set)
#define P_Test_SynCheck(m)         ((m) & P_SynCheck)
#define P_Test_SemCheck(m)         ((m) & P_SemCheck)
#define P_Test_DbgRead(m)          ((m) & P_DbgRead)
#define P_Test_Write(m)            ((m) & P_Write)
#define P_Test_WriteVoid(m)        ((m) & P_WriteVoid)
#define P_Test_WriteMeta(m)        ((m) & P_WriteMeta)

#define P_Test_NotSet(m)           (!P_Test_Set(m))
#define P_Test_NotSynCheck(m)      (!P_Test_SynCheck(m))
#define P_Test_NotSemCheck(m)      (!P_Test_SemCheck(m))
#define P_Test_NotDbgRead(m)       (!P_Test_DbgRead(m))
#define P_Test_NotWrite(m)         (!P_Test_Write(m))
#define P_Test_NotWriteVoid(m)     (!P_Test_WriteVoid(m))
#define P_Test_NotWriteMeta(m)     (!P_Test_WriteMeta(m))

#define P_Test_CheckAndSet(m)      (((m) & P_CheckAndSet) == P_CheckAndSet)
#define P_Test_BothCheck(m)        (((m) & P_CheckAndSet) == P_BothCheck)
#define P_Test_Ignore(m)           (((m) & P_CheckAndSet) == P_Ignore)

#define P_Test_NotCheckAndSet(m)   (((m) & P_CheckAndSet) != P_CheckAndSet)
#define P_Test_NotBothCheck(m)     (((m) & P_CheckAndSet) != P_BothCheck)
#define P_Test_NotIgnore(m)        (((m) & P_CheckAndSet) != P_Ignore)

#define P_Test_WriteOrWriteVoid(m)    ((m) & (P_Write|P_WriteVoid))
#define P_Test_NotWriteOrWriteVoid(m) (!P_Test_WriteOrWriteVoid(m))

#define P_Do_Set(m)                ((m) |= P_Set)
#define P_Do_SynCheck(m)           ((m) |= P_SynCheck)
#define P_Do_SemCheck(m)           ((m) |= P_SemCheck)
#define P_Do_DbgRead(m)            ((m) |= P_DbgRead)
#define P_Do_Write(m)              ((m) |= P_Write)
#define P_Do_WriteVoid(m)          ((m) |= P_WriteVoid)
#define P_Do_WriteMeta(m)          ((m) |= P_WriteMeta)

#define P_Dont_Set(m)              ((m) &= (~P_Set))
#define P_Dont_SynCheck(m)         ((m) &= (~P_SynCheck))
#define P_Dont_SemCheck(m)         ((m) &= (~P_SemCheck))
#define P_Dont_DbgRead(m)          ((m) &= (~P_DbgRead))
#define P_Dont_Write(m)            ((m) &= (~P_Write))
#define P_Dont_WriteVoid(m)        ((m) &= (~P_WriteVoid))
#define P_Dont_WriteMeta(m)        ((m) &= (~P_WriteMeta))

#endif  /*  FOR_CKIT  */

/* type PerrorRep: */
enum PerrorRep_e { PerrorRep_Max, PerrorRep_Med, PerrorRep_Min, PerrorRep_None };

/* type Pendian_t: */
enum Pendian_e { PbigEndian, PlittleEndian };

/* type Pcharset: */
enum Pcharset_e { Pcharset_INVALID = 0, Pcharset_ASCII = 1, Pcharset_EBCDIC = 2 };

/* helper functions for the above enumerated types: */
const char *Pbase_m2str   (P_t *pads, Pbase_m  m);
const char *PerrorRep2str (PerrorRep  e);
const char *Pendian2str   (Pendian_t  e);
const char *Pcharset2str  (Pcharset   e); 
/* Note: For Pbase_m2str, result should be used/copied prior to further library calls */

/* A Ppos_t (IO position) has a byte position within the num'th read unit,
 * where the read unit is determined by the IO discipline.  A description
 * of the read unit (e.g., "record", "1K Block", etc.) can be obtained
 * using P_io_read_unit.  There is also an offset field which gives the 
 * absolute offset of the location within the currently installed IO stream.
 *
 * A Ploc_t (IO location) has two positions, b and e, marking the
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

/* type Ppos_t: */
struct Ppos_s {
  size_t       byte;
  size_t       num;
  Sfoff_t      offset;
};

/* HELPER: P_POS_EQ tests whether pos1 is the same IO position as pos2 */
/* #define P_POS_EQ(pos1, pos2) ((pos1).num == (pos2).num && (pos1).byte == (pos2).byte) */

#ifdef FOR_CKIT
int P_POS_EQ(Ppos_t first, Ppos_t second);
int P_POS_GT(Ppos_t first, Ppos_t second);
#endif

/* type Ploc_t: */
struct Ploc_s {
  Ppos_t b;
  Ppos_t e;
};



/* The following four fields are always the first four fields in every PADS
 * parse descriptor type.  (They are the only fields in type Pbase_pd.)
 */

#define PD_COMMON_FIELDS_BASE \
  Pflags_t    pstate; /* parse state */ \
  Puint32     nerr; \
  PerrCode_t  errCode; \
  Ploc_t      loc

#ifndef USE_GALAX
#  define PD_COMMON_FIELDS PD_COMMON_FIELDS_BASE
#else
#  define PD_COMMON_FIELDS PD_COMMON_FIELDS_BASE; PDCI_id_t _id_
#endif

/* PD_COMMON_INIT: Function (macro actually) that initializes the first four fields of
 * any parse descriptor type: initializes pstate to 'not panic',
 * errCode to P_NOT_PARSED, nerr to 0, loc to all zeros.
 *
 * PD_COMMON_INIT_NO_ERR: like PD_COMMON_INIT, but initializes errCode to P_NO_ERR.
 *
 * PD_COMMON_READ_INIT: Macros that initializes the loc.b field to the current io 
 * position.
 */
#ifdef FOR_CKIT
void PD_COMMON_INIT(void *pd);
void PD_COMMON_INIT_NO_ERR(void *pd);
void PD_COMMON_READ_INIT(P_t *pads,void *pd);
#endif

/* type Pbase_pd: */
struct Pbase_pd_s {
  PD_COMMON_FIELDS;
};

/* These functions (macros actually) invoke PD_COMMON_INIT/PD_COMMON_INIT_NO_ERR on pd */
#ifdef FOR_CKIT
void Pbase_pd_init(Pbase_pd *pd);
void Pbase_pd_init_no_err(Pbase_pd *pd);
#endif

/* string description of parse state */
const char * P_pstate2str(Pflags_t pstate);

/* Functions (macros actually) for setting or testing parse state (PS) pd->pstate */
#ifdef FOR_CKIT
void P_PS_init(void *pd);         /* init pd->pstate */
void P_PS_setPanic(void *pd);     /* set P_Panic in pd->pstate */
void P_PS_unsetPanic(void *pd);   /* unset P_Panic in pd->pstate */
int  P_PS_isPanic(void *pd);      /* test whether P_Panic is set in pd->pstate */

void P_PS_setPartial(void *pd);   /* set P_Partial in pd->pstate */
void P_PS_unsetPartial(void *pd); /* unset P_Partial in pd->pstate */
int  P_PS_isPartial(void *pd);    /* test whether P_Partial is set in pd->pstate */
#endif

/* Pinv_val_fn: type of a pointer to an invalid val function */
typedef Perror_t (*Pinv_val_fn)(P_t *pads, void *pd_void, void *val_void, va_list type_args);

/* A helper function that invokes inv_val function fn with
   the specified arguments, passing the ... arguments in a va_list */
Perror_t P_invoke_inv_val_fn(Pinv_val_fn fn, P_t *pads, void *pd_void, void *val_void, ...);

/* Pinv_val_fn_map_t: type of an invalid val function map */
typedef struct Pinv_val_fn_map_s Pinv_val_fn_map_t;

/* Pfmt_fn: type of a pointer to a fmt function */
typedef ssize_t (*Pfmt_fn)(P_t *pads, Pbyte *buf, size_t buf_len,
			   int *buf_full, int *requestedOut, char const *delims,
			   void *m, void  *pd, void *rep, va_list type_args);

/* A helper function that invokes fmt function fn with
   the specified arguments, passing the ... arguments in a va_list */
ssize_t P_invoke_fmt_fn(Pfmt_fn fn, P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full,
			int *requested_out, const char *delims,
			void *m, void *pd, void *rep, ...);

/* If delims is a pointer to a delimiter within a string of
 * delimiters, P_ADVANCE_DELIMS produces a pointer to the
 * next delimiter, or to the same delimeter if there are no
 * more available delimiters.  Typically used as follows:
 *
 *      delims = P_ADVANCE_DELIMS(delims);
 */
#define P_ADVANCE_DELIMS(delims) ((*(delims + 1)) ? delims + 1 : delims)

/* Pfmt_fn_map_t: type of a fmt function map */
typedef struct Pfmt_fn_map_s Pfmt_fn_map_t;

/* type Pin_formats_t: */
struct Pin_formats_s {
  const char        *date;          /* must be a non-NULL date input format string */
};
/* type Pout_formats_t: */
struct Pout_formats_s {
  const char        *date;          /* must be a non-NULL date output format string */
};

/* type Pdisc_t: */
struct Pdisc_s {
  Pflags_t            version;        /* interface version */
  Pflags_t            flags;          /* control flags */
  Pcharset            def_charset;    /* default char set */ 
  int                 copy_strings;   /* if non-zero,  ASCII string read functions copy the strings found, otherwise not */
  /* For the next four values, 0 means end-of-record / soft limit for non-record-based IO disciplines */
  size_t              match_max;      /* max match distance */ 
  size_t              numeric_max;    /* max numeric value distance */
  size_t              scan_max;       /* max normal scan distance */
  size_t              panic_max;      /* max panic scan distance */
  Pfopen_fn           fopen_fn;       /* file open function (default P_fopen) */
  Perror_fn           error_fn;       /* error function using  ... */
  PerrorRep           e_rep;          /* controls error reporting */
  Pendian_t           d_endian;       /* endian-ness of the data */ 
  Puint64             acc_max2track;  /* default maximum distinct values for accumulators to track */
  Puint64             acc_max2rep;    /* default maximum number of tracked values to describe in detail in report */
  Pfloat64            acc_pcnt2rep;   /* default maximum percent of values to describe in detail in report */
  const char         *in_time_zone;   /* default time zone for Pdate input, specified as a string */ 
  const char         *out_time_zone;  /* default time zone for Pdate formatted output, specified as a string */ 
  Pin_formats_t       in_formats;     /* default input formats */
  Pout_formats_t      out_formats;    /* default output formats */
  Pinv_val_fn_map_t  *inv_val_fn_map; /* map types to inv_val_fn for write functions */
  Pfmt_fn_map_t      *fmt_fn_map;     /* map types to fmt functions */
  Pio_disc_t         *io_disc;        /* sub-discipline for controlling IO */
};

extern Pdisc_t Pdefault_disc;

/* PARTIAL descriptionof type P_t:
 * It is OK to get the id and disc from a P_t* handle,
 * but other elements of the struct should only manipulated
 * by the internal library routines.
 *
 */

P_PRIVATE_DECLS;

struct P_s {
  const char     *id;       /* interface id */
  Pdisc_t        *disc;     /* discipline handle */
  P_PRIVATE_STATE;
};

/* ================================================================================
 * LIBRARY HANDLE OPEN/CLOSE FUNCTIONS
 *
 * P_open:
 *         XXX_TODOC
 * P_libopen:
 *         XXX_TODOC
 *
 * P_close:
 *         XXX_TODOC
 *
 *         If there is an installed IO discipline,
 *         it is unmade; after this point it should NOT be used any more.
 *         (See P_close_keep_io_disc below.)

 * P_close_keep_io_disc:
 *         Like P_close, except takes an extra argument, keep_io_disc, which
 *         if non-zero indicates the installed IO discipline (if any) should not be unmade;
 *         in this case it CAN be used again, e.g., in a future P_open call.
 */

Perror_t  P_open  (P_t **pads_out, Pdisc_t *disc, Pio_disc_t *io_disc);
Perror_t  P_libopen  (P_t **pads_out, Pdisc_t *disc, Pio_disc_t *io_disc, int io_disc_required);
Perror_t  P_close (P_t *pads); 
Perror_t  P_close_keep_io_disc(P_t *pads, int keep_io_disc);

/*
 * The following is normally generated by the PADS compiler, but it can also
 * be provided by other means.
 */
extern void P_lib_init(void);

/*
 * If you want to use the PADS library without linking against generated code, you need
 * to provide an implementation of P_lib_init.  Here is a macro that you can use
 * in your main.c file to provide a trivial implementation (it does nothing):
 */
#define P_NOGEN void P_lib_init() { }

/* ================================================================================
 * TOP-LEVEL GET/SET FUNCTIONS
 *
 * P_get_disc    : returns NULL on error, otherwise returns pointer to
 *                   the installed discipline
 *
 * P_set_disc    : install a different discipline handle.  If param xfer_io
 *                   is non-zero, then the IO discipline from the old handle is
 *                   moved to the new handle.  In other words, the call
 *                      P_set_disc(pads, new_handle, 1)
 *                   is equivalent to
 *                      old_handle = pads->disc;
 *                      new_handle->io_disc = old_handle->io_disc;
 *                      old_handle->io_disc = 0;
 *                      P_set_disc(pads, new_handle, 0);
 *
 * P_set_io_disc : install a different IO discipline into the
 *                   main discipline.  if there is an open sfio stream,
 *                   it is transferred to the
 *                   new IO discipline after closing the old IO
 *                   discipline in a way that returns
 *                   all bytes beyond the current IO cursor to 
 *                   the stream.  The old IO discipline (if any) is
 *                   unmade.   After this point the old IO discipine should NOT
 *                   be re-used.  (See P_set_io_disc_keep_old below.)
 *
 * P_set_io_disc_keep_old:
 *                 Like P_set_io_disc, except takes an extra argument, keep_old_io_disc,
 *                 which is non-zero indicates that the old IO discipline
 *                 should not be unmade; in this case it CAN be used again, e.g., in a future
 *                 P_set_io_disc call. 
 *
 * P_set_in_time_zone:
 * P_set_out_time_zone:
 *                 See the comments above under the descriptions of the discipline
 *                 fields in_time_zone and out_time_zone. 
 */

Pdisc_t * P_get_disc   (P_t *pads);
Perror_t  P_set_disc   (P_t *pads, Pdisc_t *new_disc, int xfer_io);
Perror_t  P_set_io_disc(P_t* pads, Pio_disc_t* new_io_disc);
Perror_t  P_set_io_disc_keep_old(P_t* pads, Pio_disc_t* new_io_disc, int keep_old_io_disc);

Perror_t  P_set_in_time_zone(P_t *pads, const char *new_in_time_zone);
Perror_t  P_set_out_time_zone(P_t *pads, const char *new_out_time_zone);

/* P_rmm_zero    : get rbuf memory manager that zeroes allocated memory
 * P_rmm_nozero  : get rbuf memory manager that does not zero allocated memory
 *
 * See rbuf.h for the RMM/Rbuf memory management API
 */

RMM_t * P_rmm_zero  (P_t *pads);
RMM_t * P_rmm_nozero(P_t *pads);

/* ================================================================================
 * TOP-LEVEL invalid_val_fn FUNCTIONS
 *
 * Getting and setting invalid val functions in a map:
 *   P_get_inv_val_fn returns the currently installed function for type_name, or NULL if none is installed
 *
 *   P_set_inv_val_fn returns the previously installed function for type_name, or NULL if none was installed.
 *   If the fn argument is NULL, any current mapping for type_name is removed.
 *
 * Creating and destroying invalid val function maps: 
 *
 * Pinv_val_fn_map_create: create a new, empty map
 * Pinv_val_fn_map_destroy: destroy a map
 *
 */
Pinv_val_fn P_get_inv_val_fn(P_t* pads, Pinv_val_fn_map_t *map, const char *type_name); 
Pinv_val_fn P_set_inv_val_fn(P_t* pads, Pinv_val_fn_map_t *map, const char *type_name, Pinv_val_fn fn);

Pinv_val_fn_map_t* Pinv_val_fn_map_create(P_t *pads);
Perror_t          Pinv_val_fn_map_destroy(P_t *pads, Pinv_val_fn_map_t *map);

/* ================================================================================
 * TOP-LEVEL fmt_fn FUNCTIONS
 *
 * Getting and setting a fmt function in a map:
 *   P_get_fmt_fn returns the currently installed function for type_name, or NULL if none is installed
 *
 *   P_set_fmt_fn returns the previously installed function for type_name, or NULL if none was installed.
 *   If the fn argument is NULL, any current mapping for type_name is removed.
 *
 * Creating and destroying fmt function maps: 
 *
 * Pfmt_fn_map_create: create a new, empty map
 * Pfmt_fn_map_destroy: destroy a map
 *
 */
Pfmt_fn P_get_fmt_fn(P_t* pads, Pfmt_fn_map_t *map, const char *type_name); 
Pfmt_fn P_set_fmt_fn(P_t* pads, Pfmt_fn_map_t *map, const char *type_name, Pfmt_fn fn);

Pfmt_fn_map_t* Pfmt_fn_map_create(P_t *pads);
Perror_t       Pfmt_fn_map_destroy(P_t *pads, Pfmt_fn_map_t *map);

/* ================================================================================
 * TOP-LEVEL IO FUNCTIONS
 * 
 * P_io_set      : Initialize or change the current sfio stream used for input.
 *                 If there is already an installed sfio stream, P_io_close is
 *                 implicitly called first.
 *
 * P_io_fopen    : Open a file for reading (a higher-level alternative to io_set).
 *                 Uses pads->disc->fopen_fn, if non-null, otherwise uses P_fopen;
 *                 always uses mode "r".  Returns P_OK on success, P_ERR on error
 *
 * P_io_close    : Clean up the io discipline state; attempts to return bytes that were
 *                 read from the underlying sfio stream but not consumed by the parse back
 *                 to the stream.
 * 
 *                 If the underlying sfio stream is due to a file open via P_io_fopen,
 *                 the file is closed.  If the underlying Sfio_stream is installed via
 *                 P_io_set, it is not closed; it is up to the program that opened the
 *                 installed sfio stream to close it  (*after* calling P_io_close).
 * 
 * P_io_next_rec : Advances current IO position to start of the next record, if any.
 *                   Returns P_OK on success, P_ERR on failure 
 *                   (failure includes hitting EOF before EOR).
 *                   For P_OK case, sets (*skipped_bytes_out) to the number of
 *                   data bytes that were passed over while searching for EOR.
 *
 * P_io_skip_bytes : Advances current IO position by specified number of bytes, or if that many
 *                   bytes cannot be skipped, then by as many bytes as available.
 *                   Sets (*bytes_skipped_out) to the number of bytes skipped.
 *                   Returns P_OK if the requested bytes were skipped, P_ERR if fewer
 *                   than the requested bytes were skipped.  NOTE: for record-based
 *                   disciplines, does NOT advance the IO position beyond the current
 *                   record.
 *
 * P_io_at_eor   : Returns 1 if the current IO position is at EOR, otherwise 0.
 * P_io_at_eof   : Returns 1 if current IO position is at EOF, otherwise 0.
 * P_io_at_eor_or_eof : Returns 1 if current IO position is at EOR or EOF, otherwise 0.
 *
 * P_io_getPos   : Fill in (*pos) with IO position.
 * P_io_getLocB  : Fill in loc->b with IO position.
 * P_io_getLocE  : Fill in loc->e with IO position.
 * P_io_getLoc   : Fill in both loc->b and loc->e with IO position.
 *
 *   All of the above take an offset.  If offset is 0, the current IO position is
 *   used, otherwise the position used is K bytes from the current IO position
 *   (offset == K ... offset is an int, and can be positive or negative).
 *   Note the current IO position does not change.  P_ERR is returned if
 *   info about the specified position cannot be determined. 
 *   EOR marker bytes (if any) are ignored when moving forward or back
 *   based on offset -- offset only refers to data bytes.
 *
 * P_io_read_unit : Provides a description of the read unit used in Ppos_t
 *                  (e.g., "line", "1K block", etc.). Returns NULL on error
 *                  (if there is no installed IO discipline).
 *
 * P_io_write_start:   Alloc a buffer buf associated with an output sfio stream io
 *                       that can be filled in using the write2buf functions.
 *                       Must be paired with either commit_write or abort_write. 
 *                       Param buf_len specifies how many bytes will be required, and
 *                       can be modified to a greater value if an existing buffer of
 *                       larger size is available.  Param set_buf is set to indicate whether the
 *                       stream's buffer was set to an internal PADS buffer.  buf, io, and set_buf
 *                       must be passed to the paired commit_write or abort_write.
 *                       Returns NULL on failure, buf on success.
 *
 * P_io_write_commit:  Write num_bytes bytes from buf to io, undo write_start effects.
 *                       Returns -1 on error, otherwise number of bytes written.
 * 
 * P_io_write_abort:   Undo write_start effects; do not write anything.
 *
 * Record and Block-of-Records write functions:
 *
 *   Note: pads->disc->io_disc must be set to a valid PADS I/O discipline when using the
 *         following functions.  Further, the I/O discipline must support records
 *         to use the record write functions, and it must support blocks of records to
 *         use the block-of-record write functions.
 *
 * P_io_rec_write2io: write a record to io, an sfio stream.  buf must contain the data bytes
 *                      (of length rec_data_len) for the record.  Record start/end markers are
 *                      written around the data bytes according to the current I/O discipline.
 *                      On success, the total number of bytes added to io is returned.  On failure,
 *                      no bytes are added to io and -1 is returned.
 *
 * P_io_rec_open_write2buf: append an open record marker (if used) to a buffer buf that has at least buf_len
 *                      available bytes.  If the open record marker would require more than buf_len
 *                      bytes, (*buf_full) is set to 1 and -1 is returned.  For all other cases,
 *                      (*buf_full) is unmodified.  Returns -1 on failure, otherwise the number of
 *                      bytes appeneded.  Note: P_io_rec_close_write2buf *must* be called with
 *                      param rec_start set to the same location that was passed as buf to this call,
 *                      to allow the record open marker to be updated with appropriate length info.
 *
 * P_io_rec_close_write2buf: append a record close marker (if used) to a buffer buf that has at least buf_len
 *                      available bytes.  If the close record marker would require more than buf_len bytes,
 *                      (*buf_full) is set to 1 and -1 is returned.  For all other cases, (*buf_full) is
 *                      unmodified.  Returns -1 on failure, otherwise the number of bytes
 *                      appended.  Note that param rec_start must be used to specify the location of the
 *                      record open marker / start of record, and num_bytes must specify the number of
 *                      bytes used for both the open marker and the data bytes of the record.  Thus,
 *                      num_bytes will equal (buf - rec_start) if the full record is in a contiguous
 *                      region of memory.
 *
 * P_io_rblk_write2io: write a block of records to io, an sfio stream.  buf must contain the data bytes
 *                      for all of the records (of length blk_data_len).  Block start/end markers are
 *                      written around the data bytes according to the current I/O discipline, where
 *                      num_recs or the appropriate length will be written as appropriate.  (Some disciplines
 *                      require a length for the block, others require the number of records in the block, so
 *                      both must be provided.)  
 *                      On success, the total number of bytes added to io is returned.  On failure,
 *                      no bytes are added to io and -1 is returned.
 *
 * P_io_rblk_open_write2buf: append an open block marker (if used) to a buffer buf that has at least buf_len
 *                      available bytes.  If the open marker would require more than buf_len
 *                      bytes, (*buf_full) is set to 1 and -1 is returned.  For all other cases,
 *                      (*buf_full) is unmodified.  Returns -1 on failure, otherwise the number of
 *                      bytes appeneded.  Note: P_io_rblk_close_write2buf *must* be called with
 *                      param blk_start set to the same location that was passed as buf to this call,
 *                      to allow the block open marker to be updated with appropriate info.
 *
 * P_io_rblk_close_write2buf: append a block close marker (if used) to a buffer buf that has at least buf_len
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

Perror_t  P_io_set       (P_t *pads, Sfio_t *io);
Perror_t  P_io_fopen     (P_t *pads, const char *path);
Perror_t  P_io_close     (P_t *pads);
Perror_t  P_io_next_rec  (P_t *pads, size_t *skipped_bytes_out);
Perror_t  P_io_skip_bytes(P_t *pads, size_t width, size_t *skipped_bytes_out);

int       P_io_at_eor        (P_t *pads);
int       P_io_at_eof        (P_t *pads);
int       P_io_at_eor_or_eof (P_t *pads);

Perror_t  P_io_getPos   (P_t *pads, Ppos_t *pos, int offset); 
Perror_t  P_io_getLocB  (P_t *pads, Ploc_t *loc, int offset); 
Perror_t  P_io_getLocE  (P_t *pads, Ploc_t *loc, int offset); 
Perror_t  P_io_getLoc   (P_t *pads, Ploc_t *loc, int offset); 

const char * P_io_read_unit(P_t *pads);

#if P_CONFIG_WRITE_FUNCTIONS > 0
Pbyte*    P_io_write_start (P_t *pads, Sfio_t *io, size_t *buf_len, int *set_buf);
ssize_t   P_io_write_commit(P_t *pads, Sfio_t *io, Pbyte *buf, int set_buf, size_t num_bytes);
void      P_io_write_abort (P_t *pads, Sfio_t *io, Pbyte *buf, int set_buf);

ssize_t   P_io_rec_write2io(P_t *pads, Sfio_t *io, Pbyte *buf, size_t rec_data_len);
ssize_t   P_io_rec_open_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full);
ssize_t   P_io_rec_close_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full,
				   Pbyte *rec_start, size_t num_bytes);

ssize_t   P_io_rblk_write2io(P_t *pads, Sfio_t *io, Pbyte *buf, size_t blk_data_len, Puint32 num_recs);
ssize_t   P_io_rblk_open_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full);
ssize_t   P_io_rblk_close_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full,
				    Pbyte *blk_start, size_t num_bytes, Puint32 num_recs);

ssize_t   P_io_rec_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full);
#endif

/* ================================================================================
 * SCAN FUNCTIONS
 *
 * Scan functions are used to 'find' a location that is forward of the current
 * IO position.  They are normally used by library routines or by generated
 * code, but are exposed here because they are generally useful.
 *
 * ================================
 * CHARACTER LITERAL SCAN FUNCTIONS
 * ================================
 *
 * DEFAULT                        ASCII                          EBCDIC
 * -----------------------------  -----------------------------  -----------------------------
 * Pchar_lit_scan1                Pa_char_lit_scan1              Pe_char_lit_scan1
 * Pchar_lit_scan2                Pa_char_lit_scan2              Pe_char_lit_scan2
 *
 * EFFECT: 
 *
 * The scan1 functions:
 *
 *  Scans for find character f.  The char is specified as an ASCII
 *  character, and is converted to EBCDIC if the EBCDIC form is used or if the
 *  DEFAULT form is used and pads->disc->def_charset is Pcharset_EBCDIC.
 *
 *  If f is found, then if eat_f is non-zero the IO points to just beyond the
 *  char, otherwise it points to the char.  If panic is set,
 *  pads->disc->panic_max controls the scope of the scan, otherwise
 *  pads->disc->scan_max controls the scope of the scan.  Hitting eor or eof
 *  considered to be an error.  N.B. If there is mixed binary and ASCII data,
 *  scanning can 'find' an ASCII char in a binary field.  Be careful!
 *
 * RETURNS: Perror_t
 *         P_OK    => f found, IO cursor now points to just beyond char
 *                      (eat_f param non-zero) or to the char (eat_f zero).
 *                      Sets (*offset_out) to the distance scanned to find f
 *                      (0 means the IO cursor was already pointing at f)
 *         P_ERR   => f not found, IO cursor unchanged
 * 
 * The scan2 functions:
 *
 *  Scans for either find character f or stop character s.  The chars are
 *  specified as ASCII characters, and are converted to EBCDIC if the EBCDIC
 *  form is used or if the DEFAULT form is used and pads->disc->def_charset is
 *  Pcharset_EBCDIC.
 *
 *  If f or s is found, then if the corresponding 'eat' param (eat_f if f
 *  is found, eat_s if s is found) is non-zero the IO points to just beyond the
 *  char, otherwise it points to the char.  If panic is set,
 *  pads->disc->panic_max controls the scope of the scan, otherwise
 *  pads->disc->scan_max controls the scope of the scan.  Hitting eor or eof
 *  considered to be an error.  N.B. If there is mixed binary and ASCII data,
 *  scanning can 'find' an ASCII char in a binary field.  Be careful!
 *
 * RETURNS: Perror_t
 *         P_OK    => f/s found, IO cursor now points to just beyond char
 *                      (corresponding eat param non-zero) or to the char (eat param zero).
 *                      Sets (*f_found_out) to 1 if f was found, 0 if s was found.
 *                      Sets (*offset_out) to the distance scanned to find the char
 *                      (0 means the IO cursor was already pointing at the char).
 *         P_ERR   => char not found, IO cursor unchanged
 * 
 * =============================
 * STRING LITERAL SCAN FUNCTIONS
 * =============================
 *
 * DEFAULT                        ASCII                          EBCDIC
 * -----------------------------  -----------------------------  -----------------------------
 * Pstr_lit_scan1                 Pa_str_lit_scan1               Pe_str_lit_scan1
 * Pcstr_lit_scan1                Pa_cstr_lit_scan1              Pe_cstr_lit_scan1
 *
 * Pstr_lit_scan2                 Pa_str_lit_scan2               Pe_str_lit_scan2
 * Pcstr_lit_scan2                Pa_cstr_lit_scan2              Pe_cstr_lit_scan2
 *
 * These functions are similar to the character scan functions, except ASCII find
 * and stop strings f and s are given.  String literals are passed as arguments in one of
 * two ways:
 *    + The str_lit  scan functions take type Pstring*
 *    + The cstr_lit scan functions take type const char*
 *
 * The input strings are converted internally to EBCDIC if an EBCDIC form
 * is used or if a DEFAULT form is used and pads->disc->def_charset is Pcharset_EBCDIC.
 * (The input args are unchanged.)
 *
 * If there is no stop string, a scan1 function should be used.  For the scan2
 * functions, on P_OK, sets (*f_found_out) to 1 if f was found, to 0 is s was
 * found.  For both scan1 and scan2 functions, sets (*offset_out) to the
 * distance scanned to find the string (0 means the IO cursor was already
 * pointing at the string). If the corresponding eat param is non-zero (eat_f
 * for f, eat_s for s), the IO cursor points just beyond the string literal that
 * was found, otherwise it points to the start of the string that was found.  On
 * P_ERR, the IO cursor is unchanged.
 *
 * =================================
 * REGULAR EXPRESSION SCAN FUNCTIONS
 * =================================
 *
 * DEFAULT                        ASCII                          EBCDIC
 * -----------------------------  -----------------------------  -----------------------------
 * Pre_scan1                      Pa_re_scan1                    Pe_re_scan1
 * Pre_scan2                      Pa_re_scan2                    Pe_re_scan2
 *
 * These functions are similar to the string literal scan functions except they
 * take a find regular expresssion f and (for scan2) a stop regular expression s.
 * The RE scan functions all have Pregexp_t* regular expression arguments.
 * See the section 'REGULAR EXPRESSION MACROS' below for convenient ways to
 * initialize Pregexp_t values.
 * 
 * If there is no stop case, a scan1 function should be used.  For the scan2
 * functions, on P_OK, sets (*f_found_out) to 1 if f was found, to 0 if s was
 * found.  For both scan1 and scan2 functions, sets (*offset_out) to the
 * distance scanned to find the start of the match (0 means the matching
 * characters begin at the current IO cursor position). If the corresponding eat
 * param is non-zero (eat_f for f, eat_s for s), the IO cursor points just
 * beyond the set of matching characters, otherwise it points to the first
 * matching character.  On P_ERR, the IO cursor is unchanged.
 */

#ifdef FOR_CKIT
#if P_CONFIG_READ_FUNCTIONS > 0

#if P_CONFIG_A_CHAR_STRING > 0
Perror_t Pa_char_lit_scan1 (P_t *pads, Pchar f,          int eat_f, int panic, size_t *offset_out);
Perror_t Pa_str_lit_scan1  (P_t *pads, const Pstring *f, int eat_f, int panic, size_t *offset_out);
Perror_t Pa_cstr_lit_scan1 (P_t *pads, const char *f,    int eat_f, int panic, size_t *offset_out);
Perror_t Pa_re_scan1       (P_t *pads, Pregexp_t *f,     int eat_f, int panic, size_t *offset_out);

Perror_t Pa_char_lit_scan2 (P_t *pads, Pchar f, Pchar s,
			    int eat_f, int eat_s, int panic,
			    int *f_found_out, size_t *offset_out);
Perror_t Pa_str_lit_scan2  (P_t *pads, const Pstring *f, const Pstring *s,
			    int eat_f, int eat_s, int panic,
			    int *f_found_out, size_t *offset_out);
Perror_t Pa_cstr_lit_scan2 (P_t *pads, const char *f, const char *s,
			    int eat_f, int eat_s, int panic,
			    int *f_found_out, size_t *offset_out);
Perror_t Pa_re_scan2       (P_t *pads, Pregexp_t *f, Pregexp_t *s,
			    int eat_f, int eat_s, int panic,
			    int *f_found_out, size_t *offset_out);
#endif

#if P_CONFIG_E_CHAR_STRING > 0
Perror_t Pe_char_lit_scan1 (P_t *pads, Pchar f,          int eat_f, int panic, size_t *offset_out);
Perror_t Pe_str_lit_scan1  (P_t *pads, const Pstring *f, int eat_f, int panic, size_t *offset_out);
Perror_t Pe_cstr_lit_scan1 (P_t *pads, const char *f,    int eat_f, int panic, size_t *offset_out);
Perror_t Pe_re_scan1       (P_t *pads, Pregexp_t *f,     int eat_f, int panic, size_t *offset_out);

Perror_t Pe_char_lit_scan2 (P_t *pads, Pchar f, Pchar s,
			    int eat_f, int eat_s, int panic,
			    int *f_found_out, size_t *offset_out);
Perror_t Pe_str_lit_scan2  (P_t *pads, const Pstring *f, const Pstring *s,
			    int eat_f, int eat_s, int panic,
			    int *f_found_out, size_t *offset_out);
Perror_t Pe_cstr_lit_scan2 (P_t *pads, const char *f, const char *s,
			    int eat_f, int eat_s, int panic,
			    int *f_found_out, size_t *offset_out);
Perror_t Pe_re_scan2       (P_t *pads, Pregexp_t *f, Pregexp_t *s,
			    int eat_f, int eat_s, int panic,
			    int *f_found_out, size_t *offset_out);
#endif

#if P_CONFIG_A_CHAR_STRING > 0 && P_CONFIG_E_CHAR_STRING > 0
Perror_t Pchar_lit_scan1   (P_t *pads, Pchar f,          int eat_f, int panic, size_t *offset_out);
Perror_t Pstr_lit_scan1    (P_t *pads, const Pstring *f, int eat_f, int panic, size_t *offset_out);
Perror_t Pcstr_lit_scan1   (P_t *pads, const char *f,    int eat_f, int panic, size_t *offset_out);
Perror_t Pre_scan1         (P_t *pads, Pregexp_t *f,     int eat_f, int panic, size_t *offset_out);

Perror_t Pchar_lit_scan2   (P_t *pads, Pchar f, Pchar s,
			    int eat_f, int eat_s, int panic,
			    int *f_found_out, size_t *offset_out);
Perror_t Pstr_lit_scan2    (P_t *pads, const Pstring *f, const Pstring *s,
			    int eat_f, int eat_s, int panic,
			    int *f_found_out, size_t *offset_out);
Perror_t Pcstr_lit_scan2   (P_t *pads, const char *f, const char *s,
			    int eat_f, int eat_s, int panic,
			    int *f_found_out, size_t *offset_out);
Perror_t Pre_scan2         (P_t *pads, Pregexp_t *f, Pregexp_t *s,
			    int eat_f, int eat_s, int panic,
			    int *f_found_out, size_t *offset_out);
#endif

#endif /* P_CONFIG_READ_FUNCTIONS */
#endif /* FOR_CKIT */

/* ================================================================================
 * MATCH FUNCTIONS
 *
 * Match functions are used to check whether a character literal, string literal,
 * or regular expression matches the data at the current IO position.
 * They are normally used by library routines or by generated
 * code, but are exposed here because they are generally useful.
 *
 * =================================
 * CHARACTER LITERAL MATCH FUNCTIONS
 * =================================
 *
 * DEFAULT                        ASCII                          EBCDIC
 * -----------------------------  -----------------------------  -----------------------------
 * Pchar_lit_match                Pa_char_lit_match              Pe_char_lit_match
 *
 * XXX_TODOC
 *
 * ==============================
 * STRING LITERAL MATCH FUNCTIONS
 * ==============================
 *
 * DEFAULT                        ASCII                          EBCDIC
 * -----------------------------  -----------------------------  -----------------------------
 * Pstr_lit_match                 Pa_str_lit_match               Pe_str_lit_match
 * Pcstr_lit_match                Pa_cstr_lit_match              Pe_cstr_lit_match
 *
 * XXX_TODOC
 *
 * =================================
 * REGULAR EXPRESSION SCAN FUNCTIONS
 * =================================
 *
 * DEFAULT                        ASCII                          EBCDIC
 * -----------------------------  -----------------------------  -----------------------------
 * Pre_match                      Pa_re_match                    Pe_re_match
 * Pcstr_re_match                 Pa_cstr_re_match               Pe_cstr_re_match
 *
 * XXX_TODOC
 */

#ifdef FOR_CKIT
#if P_CONFIG_READ_FUNCTIONS > 0

#if P_CONFIG_A_CHAR_STRING > 0
Perror_t Pa_char_lit_match (P_t *pads, Pchar f,          int eat_f);
Perror_t Pa_str_lit_match  (P_t *pads, const Pstring *f, int eat_f);
Perror_t Pa_cstr_lit_match (P_t *pads, const char *f,    int eat_f);
Perror_t Pa_re_match       (P_t *pads, Pregexp_t *f,     int eat_f);
Perror_t Pa_cstr_re_match  (P_t *pads, const char *f,    int eat_f);
#endif

#if P_CONFIG_E_CHAR_STRING > 0
Perror_t Pe_char_lit_match (P_t *pads, Pchar f,          int eat_f);
Perror_t Pe_str_lit_match  (P_t *pads, const Pstring *f, int eat_f);
Perror_t Pe_cstr_lit_match (P_t *pads, const char *f,    int eat_f);
Perror_t Pe_re_match       (P_t *pads, Pregexp_t *f,     int eat_f);
Perror_t Pe_cstr_re_match  (P_t *pads, const char *f,    int eat_f);
#endif

#if P_CONFIG_A_CHAR_STRING > 0 && P_CONFIG_E_CHAR_STRING > 0
Perror_t Pchar_lit_match   (P_t *pads, Pchar f,          int eat_f);
Perror_t Pstr_lit_match    (P_t *pads, const Pstring *f, int eat_f);
Perror_t Pcstr_lit_match   (P_t *pads, const char *f,    int eat_f);
Perror_t Pre_match         (P_t *pads, Pregexp_t *f,     int eat_f);
Perror_t Pcstr_re_match    (P_t *pads, const char *f,    int eat_f);
#endif

#endif /* P_CONFIG_READ_FUNCTIONS */
#endif /* FOR_CKIT */

/* ================================================================================
 * LITERAL READ FUNCTIONS
 *
 * DEFAULT                        ASCII                          EBCDIC
 * -----------------------------  -----------------------------  -----------------------------
 * Pchar_lit_read                 Pa_char_lit_read               Pe_char_lit_read
 * Pstr_lit_read                  Pa_str_lit_read                Pe_str_lit_read
 * Pcstr_lit_read                 Pa_cstr_lit_read               Pe_cstr_lit_read
 * 
 * These char and string literal read functions all take an char or string to be
 * read specified in ASCII.  The char or string is converted to EBCDIC if one of
 * the EBCDIC forms is used or if one of the DEFAULT forms is used and
 * pads->disc->def_charset is Pcharset_EBCDIC.
 *
 * Mask flags control the behavior, as follows:
 *
 *
 * P_Test_SynCheck(*m)                 P_Test_NoSynCheck(*m)
 * ---------------------------------   ------------------------------
 * If IO cursor points to specified    Always advance cursor by length
 * literal, advance cursor by length   of literal, regardless of what
 * of the literal and return P_OK,     cursor points to, and return
 * otherwise report error, do not      P_OK.
 * advance cursor, return P_ERR.
 * 
 * The error code used is either P_CHAR_LIT_NOT_FOUND or P_STR_LIT_NOT_FOUND.
 */

#ifdef FOR_CKIT
#if P_CONFIG_READ_FUNCTIONS > 0

#if P_CONFIG_A_CHAR_STRING > 0
Perror_t Pa_char_lit_read(P_t *pads, const Pbase_m *m, Pchar c, Pbase_pd *pd, Pchar *c_out);
Perror_t Pa_str_lit_read (P_t *pads, const Pbase_m *m, const Pstring *s, Pbase_pd *pd, Pstring *s_out);
Perror_t Pa_cstr_lit_read(P_t *pads, const Pbase_m *m, const char *s, Pbase_pd *pd, Pstring *s_out);
#endif

#if P_CONFIG_E_CHAR_STRING > 0
Perror_t Pe_char_lit_read(P_t *pads, const Pbase_m *m, Pchar c, Pbase_pd *pd, Pchar *c_out);
Perror_t Pe_str_lit_read (P_t *pads, const Pbase_m *m, const Pstring *s, Pbase_pd *pd, Pstring *s_out);
Perror_t Pe_cstr_lit_read(P_t *pads, const Pbase_m *m, const char *s, Pbase_pd *pd, Pstring *s_out);
#endif

#if P_CONFIG_A_CHAR_STRING > 0 && P_CONFIG_E_CHAR_STRING > 0
Perror_t Pchar_lit_read(P_t *pads, const Pbase_m *m, Pchar c, Pbase_pd *pd, Pchar *c_out);
Perror_t Pstr_lit_read (P_t *pads, const Pbase_m *m, const Pstring *s, Pbase_pd *pd, Pstring *s_out);
Perror_t Pcstr_lit_read(P_t *pads, const Pbase_m *m, const char *s, Pbase_pd *pd, Pstring *s_out);
#endif

#endif /* P_CONFIG_READ_FUNCTIONS */
#endif /* FOR_CKIT */

/* ================================================================================
 * CHAR COUNTING FUNCTIONS
 *
 * DEFAULT                        ASCII                          EBCDIC
 * -----------------------------  -----------------------------  -----------------------------
 * PcountX_read                   Pa_countX_read                 Pe_countX_read
 * PcountXtoY                     Pa_countXtoY_read              Pe_countXtoY_read
 *
 * countX counts occurrences of char x between the current IO cursor and the
 * first EOR or EOF, while countXtoY counts occurrences of x between the current
 * IO cursor and the first occurrence of char y.  x and y are always specified
 * as ASCII chars.  They are converted to EBCDIC if the EBCDIC form is used or
 * if the DEFAULT form is used and pads->disc->def->charset is Pcharset_EBCDIC.
 *
 * If parameter count_max is non-zero, then the count functions also stop counting
 * after scanning count_max characters, in which case an error is returned.
 * If the IO discipline is not record-based and count_max is zero, an error is
 * returned immediately:  you *must* specify a count_max > 0 when using an IO discipline
 * that has no records.
 *
 * For countX, if param eor_required is non-zero, then encountering EOF
 * before EOR produces an error.
 *
 * These functions do not change the IO cursor position.
 *
 * countX outcomes:
 *   1. IO cursor is already at EOF and eor_required is non-zero:
 *     + pd->loc.b/e set to EOF 'location'
 *     + if P_Test_NotIgnore(*m), pd->errCode set to P_AT_EOF,
 *         pd->nerr set to 1, and an error is reported
 *     + P_ERR returned   
 *   2. EOF is encountered before EOR and eor_required is non-zero:
 *     + pd->loc.b/e set to current IO cursor location
 *     + if P_Test_NotIgnore(*m), pd->errCode set to P_EOF_BEFORE_EOR,
 *         pd->nerr set to 1, and an error is reported
 *     + P_ERR returned
 *   3. count_max is > 0 and count_max limit is reached before x or EOR or EOF:
 *     + pd->loc.b/e set to current IO cursor location
 *     + if P_Test_NotIgnore(*m), pd->errCode set to P_COUNT_MAX_LIMIT,
 *         pd->nerr set to 1, and an error is reported
 *     + P_ERR returned
 *   4. EOR is encountered, or EOF is encounterd and eor_required is zero:
 *     + (*res_out) is set to the number of occurrences of x from the IO cursor to EOR/EOF
 *     + P_OK returned
 *
 * countXtoY outcomes:
 *   1. IO cursor is already at EOF
 *     + pd->loc.b/e set to EOF 'location'
 *     + if P_Test_NotIgnore(*m), pd->errCode set to P_AT_EOF,
 *         pd->nerr set to 1, and an error is reported
 *     + P_ERR returned   
 *   2. y is not found before EOR or EOF is hit
 *     + pd->loc.b/e set to current IO cursor location
 *     + if P_Test_NotIgnore(*m), pd->errCode set to P_CHAR_LIT_NOT_FOUND,
 *         pd->nerr set to 1, and an error is reported
 *     + P_ERR returned
 *   3. y is not found and count_max > 0 and count_max limit is hit 
 *     + pd->loc.b/e set to current IO cursor location
 *     + if P_Test_NotIgnore(*), pd->errCode set to P_COUNT_MAX_LIMIT,
 *         pd->nerr set to 1, and an error is reported
 *     P_ERR returned
 *   4. Char y is found
 *     + (*res_out) is set to the number of occurrences of x
 *       from the IO cursor to first y
 *     + P_OK returned
 */

#ifdef FOR_CKIT
#if P_CONFIG_READ_FUNCTIONS > 0

#if P_CONFIG_A_CHAR_STRING > 0
Perror_t Pa_countX_read   (P_t *pads, const Pbase_m *m,
			   Pbase_pd *pd, Pint32 *res_out, Puint8 x, int eor_required, size_t count_max);
Perror_t Pa_countXtoY_read(P_t *pads, const Pbase_m *m,
			   Pbase_pd *pd, Pint32 *res_out, Puint8 x, Puint8 y, size_t count_max);
#endif

#if P_CONFIG_E_CHAR_STRING > 0
Perror_t Pe_countX_read   (P_t *pads, const Pbase_m *m,
			   Pbase_pd *pd, Pint32 *res_out, Puint8 x, int eor_required, size_t count_max);
Perror_t Pe_countXtoY_read(P_t *pads, const Pbase_m *m,
			   Pbase_pd *pd, Pint32 *res_out, Puint8 x, Puint8 y, size_t count_max);
#endif

#if P_CONFIG_A_CHAR_STRING > 0 && P_CONFIG_E_CHAR_STRING > 0
Perror_t PcountX_read     (P_t *pads, const Pbase_m *m,
			   Pbase_pd *pd, Pint32 *res_out, Puint8 x, int eor_required, size_t count_max);
Perror_t PcountXtoY_read (P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Pint32 *res_out, Puint8 x, Puint8 y, size_t count_max);
#endif

#endif /* P_CONFIG_READ_FUNCTIONS */
#endif /* FOR_CKIT */

/* ================================================================================
 * CHAR READ FUNCTIONS
 * 
 * DEFAULT                        ASCII                          EBCDIC
 * -----------------------------  -----------------------------  -----------------------------
 * Pchar_read                     Pa_char_read                   Pe_char_read
 *
 * Cases:
 *  (1) A character is not available:
 *        + pd->loc set to the current IO position
 *        + if P_Test_NotIgnore(*m), pd->errCode is set to P_WIDTH_NOT_AVAILABLE,
 *             pd->nerr is set to 1, and an error is reported
 *        + the IO cursor is not advanced
 *        + P_ERR is returned
 *  (2) A character is available and P_Test_NotSet(*m)
 *        + the IO cursor is advanced by 1 byte
 *        + P_OK is returned
 *  (3) A character is available and P_Test_Set(*m)
 *        + (*c_out) is set to the ASCII equivalent of the character, where
 *             a conversion fom EBCDIC to ASCII occurs if the EBCDIC form is used or
 *             if the DEFAULT form is used and pads->disc->def_charset is Pcharset_EBCDIC.
 *        + the IO cursor is advanced by 1 byte
 *        + P_OK is returned
 */

#ifdef FOR_CKIT
#if P_CONFIG_READ_FUNCTIONS > 0

#if P_CONFIG_A_CHAR_STRING > 0
Perror_t Pa_char_read (P_t *pads, const Pbase_m *m, Pbase_pd *pd, Pchar *c_out);
#endif

#if P_CONFIG_E_CHAR_STRING > 0
Perror_t Pe_char_read (P_t *pads, const Pbase_m *m, Pbase_pd *pd, Pchar *c_out);
#endif

#if P_CONFIG_A_CHAR_STRING > 0 && P_CONFIG_E_CHAR_STRING > 0
Perror_t Pchar_read   (P_t *pads, const Pbase_m *m, Pbase_pd *pd, Pchar *c_out);
#endif

#endif /* P_CONFIG_READ_FUNCTIONS */
#endif /* FOR_CKIT */

/* ================================================================================
 * STRING READ FUNCTIONS
 *
 * DEFAULT                        ASCII                          EBCDIC
 * -----------------------------  -----------------------------  -----------------------------
 * Pstring_FW_read                Pa_string_FW_read              Pe_string_FW_read
 * Pstring_read                   Pa_string_read                 Pe_string_read
 * Pstring_ME_read                Pa_string_ME_read              Pe_string_ME_read
 * Pstring_CME_read               Pa_string_CME_read             Pe_string_CME_read
 * Pstring_SE_read                Pa_string_SE_read              Pe_string_SE_read
 * Pstring_CSE_read               Pa_string_CSE_read             Pe_string_CSE_read
 *
 * The string read functions each has a different way of specifying
 * the extent of the string:
 *   + all string_FW_read functions specify a fixed width.
 *     N.B.: width zero is allowed: the result is an empty string
 *       (and the IO cursor does not move)
 *   + all string_read functions specify a single stop character.
 *       if 0 (NULL) is used, then this will match a NULL in the data,
 *       and eor/eof will ALSO successfully terminate the string 
 *   + all string_ME_read and string_CME_read functions specify a Match Expression
 *       (string includes all chars that match)
 *   + all string_SE_read and string_CSE_read specify a Stop Expression
 *       (string terminated by encountering 'stop chars' that match)
 *
 * The ME/SE functions take a string containing a regular expression, while the CME/CSE
 * functions take a compiled form of regular expression (see Pregexp_compile).
 *
 * stop chars and regular expressions are specified using ASCII, but reading/matching occurs
 * using converted EBCDIC forms if an EBCDIC form is used or if a DEFAULT form is used
 * and pads->disc->def_charset is Pcharset_EBCDIC.
 * 
 * For all stop cases, the stop char/chars are not included in the
 * resulting string.  Note that if the IO cursor is already at a stop
 * condition, then a string of length zero results.
 *
 * If an expected stop char/pattern/width is found, P_OK is returned.
 * If P_TestSet(*m) then:
 *   + (*s_out) is set to contain an in-memory string.
 *     If the original data is ASCII, then s_out will either share the string or contain a
 *     copy of the string, depending on pads->disc->copy_strings.  If the original data is
 *     EBCDIC, s_out always contains a copy of the string that has been converted to ASCII.
 *     N.B. : (*s_out) should have been initialized
 *     at some point prior using Pstring_init or one of the initializing P_STRING macros.
 *     (It can be initialized once and re-used in string read calls many times.)
 * 
 * Cleanup note: If pads->disc->copy_strings is non-zero, the memory allocated in
 *               (*s_out) should ultimately be freed using Pstring_cleanup.
 *
 * If an expected stop condition is not encountered, the
 * IO cursor position is unchanged.  Error codes used:
 *     P_WIDTH_NOT_AVAILABLE
 *     P_STOPCHAR_NOT_FOUND
 *     P_STOPREGEXP_NOT_FOUND
 *     P_INVALID_REGEXP
 * 
 * EBCDIC Example: passing '|' (vertical bar, which is code 124 in ASCII) to
 * Pe_string_read as the stop char will result in a search for the EBCDIC
 * encoding of vertical bar (code 79 in EBCDIC), and (*s_out) will be a string
 * containing all chars between the IO cursor and the EBCDIC vertical
 * bar, with each EBCDIC char converted to ASCII. 
 */

#ifdef FOR_CKIT
#if P_CONFIG_READ_FUNCTIONS > 0

#if P_CONFIG_A_CHAR_STRING > 0
Perror_t Pa_string_FW_read (P_t *pads, const Pbase_m *m,
			    Pbase_pd *pd, Pstring *s_out, size_t width);
Perror_t Pa_string_read    (P_t *pads, const Pbase_m *m,
			    Pbase_pd *pd, Pstring *s_out, Pchar stopChar);
Perror_t Pa_string_ME_read (P_t *pads, const Pbase_m *m,
			    Pbase_pd *pd, Pstring *s_out, const char *matchRegexp);
Perror_t Pa_string_CME_read(P_t *pads, const Pbase_m *m,
			    Pbase_pd *pd, Pstring *s_out, Pregexp_t *matchRegexp);
Perror_t Pa_string_SE_read (P_t *pads, const Pbase_m *m,
			    Pbase_pd *pd, Pstring *s_out, const char *stopRegexp);
Perror_t Pa_string_CSE_read(P_t *pads, const Pbase_m *m,
			    Pbase_pd *pd, Pstring *s_out, Pregexp_t *stopRegexp);
#endif

#if P_CONFIG_E_CHAR_STRING > 0
Perror_t Pe_string_FW_read (P_t *pads, const Pbase_m *m,
			    Pbase_pd *pd, Pstring *s_out, size_t width);
Perror_t Pe_string_read    (P_t *pads, const Pbase_m *m,
			    Pbase_pd *pd, Pstring *s_out, Pchar stopChar);
Perror_t Pe_string_ME_read (P_t *pads, const Pbase_m *m,
			    Pbase_pd *pd, Pstring *s_out, const char *matchRegexp);
Perror_t Pe_string_CME_read(P_t *pads, const Pbase_m *m,
			    Pbase_pd *pd, Pstring *s_out, Pregexp_t *matchRegexp);
Perror_t Pe_string_SE_read (P_t *pads, const Pbase_m *m,
			    Pbase_pd *pd, Pstring *s_out, const char *stopRegexp);
Perror_t Pe_string_CSE_read(P_t *pads, const Pbase_m *m,
			    Pbase_pd *pd, Pstring *s_out, Pregexp_t *stopRegexp);
#endif

#if P_CONFIG_A_CHAR_STRING > 0 && P_CONFIG_E_CHAR_STRING > 0
Perror_t Pstring_FW_read   (P_t *pads, const Pbase_m *m,
			    Pbase_pd *pd, Pstring *s_out, size_t width);
Perror_t Pstring_read      (P_t *pads, const Pbase_m *m,
			    Pbase_pd *pd, Pstring *s_out, Pchar stopChar);
Perror_t Pstring_ME_read   (P_t *pads, const Pbase_m *m,
			    Pbase_pd *pd, Pstring *s_out, const char *matchRegexp);
Perror_t Pstring_CME_read  (P_t *pads, const Pbase_m *m,
			    Pbase_pd *pd, Pstring *s_out, Pregexp_t *matchRegexp);
Perror_t Pstring_SE_read   (P_t *pads, const Pbase_m *m,
			    Pbase_pd *pd, Pstring *s_out, const char *stopRegexp);
Perror_t Pstring_CSE_read  (P_t *pads, const Pbase_m *m,
			    Pbase_pd *pd, Pstring *s_out, Pregexp_t *stopRegexp);
#endif

#endif /* P_CONFIG_READ_FUNCTIONS */
#endif /* FOR_CKIT */


/* ================================================================================
 * DATE/TIME READ FUNCTIONS
 *
 * DEFAULT                        ASCII                          EBCDIC
 * -----------------------------  -----------------------------  -----------------------------
 * Pdate_FW_read                  Pa_date_FW_read                Pe_date_FW_read
 * Pdate_read                     Pa_date_read                   Pe_date_read
 * Pdate_ME_read                  Pa_date_ME_read                Pe_date_ME_read
 * Pdate_CME_read                 Pa_date_CME_read               Pe_date_CME_read
 * Pdate_SE_read                  Pa_date_SE_read                Pe_date_SE_read
 * Pdate_CSE_read                 Pa_date_CSE_read               Pe_date_CSE_read
 *
 * Each of the date types corresponds to one of the above string types w.r.t. specifying
 * the extent of the date field, which occurs as a 'string' in the input.  Once the
 * string is read, it is converted to a Puint32 representing the date in
 * seconds since the epoch.  For the different date formats that are supported,
 * see the discussion of disc->in_formats.date above.
 *
 * If the current IO cursor position points to a valid date string:
 *   + Sets (*res_out) to the resulting date in seconds since the epoch
 *   + advances the IO cursor position to just after the last legal character
 *     in the date string
 *   + returns P_OK
 * Otherwise:
 *   + does not advance the IO cursor pos
 *   + returns P_ERR
 */

#ifdef FOR_CKIT
#if P_CONFIG_READ_FUNCTIONS > 0

#if P_CONFIG_A_CHAR_STRING > 0
Perror_t Pa_date_FW_read (P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Puint32 *res_out, size_t width);
Perror_t Pa_date_read    (P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Puint32 *res_out, Pchar stopChar);
Perror_t Pa_date_ME_read (P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Puint32 *res_out, const char *matchRegexp);
Perror_t Pa_date_CME_read(P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Puint32 *res_out, Pregexp_t *matchRegexp);
Perror_t Pa_date_SE_read (P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Puint32 *res_out, const char *stopRegexp);
Perror_t Pa_date_CSE_read(P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Puint32 *res_out, Pregexp_t *stopRegexp);
#endif

#if P_CONFIG_E_CHAR_STRING > 0
Perror_t Pe_date_FW_read (P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Puint32 *res_out, size_t width);
Perror_t Pe_date_read    (P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Puint32 *res_out, Pchar stopChar);
Perror_t Pe_date_ME_read (P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Puint32 *res_out, const char *matchRegexp);
Perror_t Pe_date_CME_read(P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Puint32 *res_out, Pregexp_t *matchRegexp);
Perror_t Pe_date_SE_read (P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Puint32 *res_out, const char *stopRegexp);
Perror_t Pe_date_CSE_read(P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Puint32 *res_out, Pregexp_t *stopRegexp);
#endif

#if P_CONFIG_A_CHAR_STRING > 0 && P_CONFIG_E_CHAR_STRING > 0
Perror_t Pdate_FW_read   (P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Puint32 *res_out, size_t width);
Perror_t Pdate_read      (P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Puint32 *res_out, Pchar stopChar);
Perror_t Pdate_ME_read   (P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Puint32 *res_out, const char *matchRegexp);
Perror_t Pdate_CME_read  (P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Puint32 *res_out, Pregexp_t *matchRegexp);
Perror_t Pdate_SE_read   (P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Puint32 *res_out, const char *stopRegexp);
Perror_t Pdate_CSE_read  (P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Puint32 *res_out, Pregexp_t *stopRegexp);
#endif

#endif /* P_CONFIG_READ_FUNCTIONS */
#endif /* FOR_CKIT */


/* ================================================================================
 * IP ADDRESS READ FUNCTIONS
 *
 * DEFAULT                        ASCII                          EBCDIC
 * -----------------------------  -----------------------------  -----------------------------
 * Pipaddr_read                     Pa_ipaddr_read                   Pe_ipaddr_read
 *
 * Attempts to read an ipaddr string (an IP address in numeric dotted form); stores
 * a Puint32.  These read functions take a stop character, which is always
 * specified in ASCII.  It is converted to EBCDIC and the data is read as
 * EBCDIC chars if the EBCDIC form is used or if the DEFAULT form is used and
 * pads->disc->def_charset is Pcharset_EBCDIC.  Otherwise the data is read as
 * ASCII chars.
 *
 * If the current IO cursor position points to a valid ipaddr string:
 *   + Sets (*res_out) to the resulting Puint32
 *   + advances the IO cursor position to just after the last legal character
 *     in the ipaddr string
 *   + returns P_OK
 * Otherwise:
 *   + does not advance the IO cursor pos
 *   + returns P_ERR
 */

#ifdef FOR_CKIT
#if P_CONFIG_READ_FUNCTIONS > 0

#if P_CONFIG_A_CHAR_STRING > 0
Perror_t Pa_ipaddr_read(P_t *pads, const Pbase_m *m,
			Pbase_pd *pd, Puint32 *res_out, Pchar stopChar);
#endif

#if P_CONFIG_E_CHAR_STRING > 0
Perror_t Pe_ipaddr_read(P_t *pads, const Pbase_m *m,
			Pbase_pd *pd, Puint32 *res_out, Pchar stopChar);
#endif

#if P_CONFIG_A_CHAR_STRING > 0 && P_CONFIG_E_CHAR_STRING > 0
Perror_t Pipaddr_read  (P_t *pads, const Pbase_m *m,
			Pbase_pd *pd, Puint32 *res_out, Pchar stopChar);
#endif

#endif /* P_CONFIG_READ_FUNCTIONS */
#endif /* FOR_CKIT */

/* ================================================================================
 * ASCII STRING TO INTEGER READ FUNCTIONS
 *
 * An ASCII representation of an integer value (a string of digits in [0-9])
 * is assumed to be at the current cursor position, where
 * if the target type is a signed type a leading - or + is allowed and
 * if unsigned a leading + is allowed.  If (pads->disc->flags & P_WSPACE_OK), leading
 * white space is skipped, otherwise leading white space causes an error.
 * Thus, the string to be converted consists of: optional white space,
 * optional +/-, and all consecutive digits (first nondigit marks end).
 *
 * RETURN VALUE: Perror_t
 *
 * Upon success, P_OK returned: 
 *   + the IO cursor is advanced to just beyond the last digit
 *   + if P_Test_NotIngore(*m), the out param is assigned a value
 *
 * P_ERR is returned on error.
 * Cursor advancement/err settings for different error cases:
 *
 * (1) If IO cursor is at EOF
 *     + pd->loc.b/e set to EOF 'location'
 *     + IO cursor remains at EOF
 *     + if P_Test_NotIgnore(*), pd->errCode set to P_AT_EOF,
 *         pd->nerr set to 1, and an error is reported
 * (2a) There is leading white space and not (pads->disc->flags & P_WSPACE_OK)
 * (2b) The target is unsigned and the first char is a -
 * (2c) The first character is not a +, -, or in [0-9]
 * (2d) First character is allowable + or -, following by a char that is not a digit
 * For the above 4 cases:
 *     + pd->loc.b/e set to the IO cursor position.
 *     + IO cursor is not advanced
 *     + if P_Test_NotIgnore(*m), pd->errCode set to P_INVALID_A_NUM,
 *         pd->nerr set to 1, and an error is reported
 * (3) A valid ASCII integer string is found, but it describes
 *     an integer that does not fit in the specified target type
 *     + pd->loc.b/e set to elt/char position of start and end of the ASCII integer
 *     + IO cursor is advanced just beyond the last digit
 *     + if P_Test_NotIgnore(*m), pd->errCode set to P_RANGE,
 *         pd->nerr set to 1, and an error is reported
 */

#if P_CONFIG_READ_FUNCTIONS > 0

#if P_CONFIG_A_INT > 0
Perror_t Pa_int8_read (P_t *pads, const Pbase_m *m,
		       Pbase_pd *pd, Pint8 *res_out);

Perror_t Pa_int16_read(P_t *pads, const Pbase_m *m,
		       Pbase_pd *pd, Pint16 *res_out);

Perror_t Pa_int32_read(P_t *pads, const Pbase_m *m,
		       Pbase_pd *pd, Pint32 *res_out);

Perror_t Pa_int64_read(P_t *pads, const Pbase_m *m,
		       Pbase_pd *pd, Pint64 *res_out);


Perror_t Pa_uint8_read (P_t *pads, const Pbase_m *m,
			Pbase_pd *pd, Puint8 *res_out);

Perror_t Pa_uint16_read(P_t *pads, const Pbase_m *m,
			Pbase_pd *pd, Puint16 *res_out);

Perror_t Pa_uint32_read(P_t *pads, const Pbase_m *m,
			Pbase_pd *pd, Puint32 *res_out);

Perror_t Pa_uint64_read(P_t *pads, const Pbase_m *m,
			Pbase_pd *pd, Puint64 *res_out);
#endif

#endif /* P_CONFIG_READ_FUNCTIONS */

/*
 * Fixed-width ASCII integer read functions:
 *    Like the above, only a fixed width in input characters is specified, and
 *    only those characters are examined.  E.g., input '11112222' could be used
 *    to read two fixed-width ASCII integers of width 4.
 *
 * N.B. The APIs require width > 0.  If width <= 0 is given, an immediate error 
 * return occurs, without setting pd's location or error code.
 *
 * Other differences from the variable-width read functions:
 *
 * 1. It is an error if the entire specified width is not an integer, e.g.,
 *    for fixed width 4, input '111|' is an error
 *
 * 2. (pads->disc->flags & P_WSPACE_OK) indicates whether leading OR trailing spaces are OK, e.g.,
 *    for fixed width 4, input ' 1  ' is not an error is wpace_ok is 1
 *    (trailing white space is not an issue for variable-width routines)
 *
 * 3. If the specified width is available, it is always consumed, even if there is an error.
 *    In this case
 *      + pd->loc.b/e is set to the first/last char of the fixed-width field
 *      + if P_Test_NotIgnore(*m), an error code is set,
 *         pd->nerr set to 1, and an error is reported
 *
 *    If the specified width is *not* available (EOR/EOF hit):
 *      + pd->loc.b/e set to elt/char position of start/end of the 'too small' field
 *      + IO cursor is not advanced
 *      + if P_Test_NotIgnore(*m), pd->errCode set to P_WIDTH_NOT_AVAILABLE,
 *         pd->nerr set to 1, and an error is reported
 */

#if P_CONFIG_READ_FUNCTIONS > 0

#if P_CONFIG_A_INT_FW > 0
Perror_t Pa_int8_FW_read (P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Pint8 *res_out, size_t width);

Perror_t Pa_int16_FW_read(P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Pint16 *res_out, size_t width);

Perror_t Pa_int32_FW_read(P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Pint32 *res_out, size_t width);

Perror_t Pa_int64_FW_read(P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Pint64 *res_out, size_t width);


Perror_t Pa_uint8_FW_read (P_t *pads, const Pbase_m *m,
			   Pbase_pd *pd, Puint8 *res_out, size_t width);

Perror_t Pa_uint16_FW_read(P_t *pads, const Pbase_m *m,
			   Pbase_pd *pd, Puint16 *res_out, size_t width);

Perror_t Pa_uint32_FW_read(P_t *pads, const Pbase_m *m,
			   Pbase_pd *pd, Puint32 *res_out, size_t width);

Perror_t Pa_uint64_FW_read(P_t *pads, const Pbase_m *m,
			   Pbase_pd *pd, Puint64 *res_out, size_t width);
#endif

#endif /* P_CONFIG_READ_FUNCTIONS */

/* ================================================================================
 * EBCDIC STRING TO INTEGER READ FUNCTIONS
 *
 * These functions are just like their ASCII counterparts; the only
 * difference is the integers are encoding using EBCDIC string data.
 * The error codes used are also the same,
 * except that error code P_INVALID_E_NUM is used rather 
 * than P_INVALID_A_NUM
 */

#if P_CONFIG_READ_FUNCTIONS > 0

#if P_CONFIG_E_INT > 0
Perror_t Pe_int8_read (P_t *pads, const Pbase_m *m,
		       Pbase_pd *pd, Pint8 *res_out);

Perror_t Pe_int16_read(P_t *pads, const Pbase_m *m,
		       Pbase_pd *pd, Pint16 *res_out);

Perror_t Pe_int32_read(P_t *pads, const Pbase_m *m,
		       Pbase_pd *pd, Pint32 *res_out);

Perror_t Pe_int64_read(P_t *pads, const Pbase_m *m,
		       Pbase_pd *pd, Pint64 *res_out);

Perror_t Pe_uint8_read (P_t *pads, const Pbase_m *m,
			Pbase_pd *pd, Puint8 *res_out);

Perror_t Pe_uint16_read(P_t *pads, const Pbase_m *m,
			Pbase_pd *pd, Puint16 *res_out);

Perror_t Pe_uint32_read(P_t *pads, const Pbase_m *m,
			Pbase_pd *pd, Puint32 *res_out);

Perror_t Pe_uint64_read(P_t *pads, const Pbase_m *m,
			Pbase_pd *pd, Puint64 *res_out);
#endif

#if P_CONFIG_E_INT_FW > 0
Perror_t Pe_int8_FW_read (P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Pint8 *res_out, size_t width);

Perror_t Pe_int16_FW_read(P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Pint16 *res_out, size_t width);

Perror_t Pe_int32_FW_read(P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Pint32 *res_out, size_t width);

Perror_t Pe_int64_FW_read(P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Pint64 *res_out, size_t width);

Perror_t Pe_uint8_FW_read (P_t *pads, const Pbase_m *m,
			   Pbase_pd *pd, Puint8 *res_out, size_t width);

Perror_t Pe_uint16_FW_read(P_t *pads, const Pbase_m *m,
			   Pbase_pd *pd, Puint16 *res_out, size_t width);

Perror_t Pe_uint32_FW_read(P_t *pads, const Pbase_m *m,
			   Pbase_pd *pd, Puint32 *res_out, size_t width);

Perror_t Pe_uint64_FW_read(P_t *pads, const Pbase_m *m,
			   Pbase_pd *pd, Puint64 *res_out, size_t width);
#endif

#endif /* P_CONFIG_READ_FUNCTIONS */

/* ================================================================================
 * DEFAULT STRING TO INTEGER READ FUNCTIONS
 *
 * These functions select the appropriate ASCII or EBCDIC string to integer
 * function based on pads->disc->def_charset.
 *
 * Example: the call 
 *
 *     Pint8_read(pads, &m, &ed, *res)
 *
 * is converted to one of these forms:
 *
 *     Pa_int8_read(pads, &m, &ed, *res)
 *     Pe_int8_read(pads, &m, &ed, *res)
 *     etc.
 */

#ifdef FOR_CKIT
#if P_CONFIG_READ_FUNCTIONS > 0

#if P_CONFIG_A_INT > 0 && P_CONFIG_E_INT > 0
Perror_t Pint8_read (P_t *pads, const Pbase_m *m,
		     Pbase_pd *pd, Pint8 *res_out);
Perror_t Pint16_read(P_t *pads, const Pbase_m *m,
		     Pbase_pd *pd, Pint16 *res_out);
Perror_t Pint32_read(P_t *pads, const Pbase_m *m,
		     Pbase_pd *pd, Pint32 *res_out);
Perror_t Pint64_read(P_t *pads, const Pbase_m *m,
		     Pbase_pd *pd, Pint64 *res_out);
Perror_t Puint8_read (P_t *pads, const Pbase_m *m,
		      Pbase_pd *pd, Puint8 *res_out);
Perror_t Puint16_read(P_t *pads, const Pbase_m *m,
		      Pbase_pd *pd, Puint16 *res_out);
Perror_t Puint32_read(P_t *pads, const Pbase_m *m,
		      Pbase_pd *pd, Puint32 *res_out);
Perror_t Puint64_read(P_t *pads, const Pbase_m *m,
		      Pbase_pd *pd, Puint64 *res_out);
#endif

#if P_CONFIG_A_INT_FW > 0 && P_CONFIG_E_INT_FW > 0
Perror_t Pint8_FW_read (P_t *pads, const Pbase_m *m,
			Pbase_pd *pd, Pint8 *res_out, size_t width);
Perror_t Pint16_FW_read(P_t *pads, const Pbase_m *m,
			Pbase_pd *pd, Pint16 *res_out, size_t width);
Perror_t Pint32_FW_read(P_t *pads, const Pbase_m *m,
			Pbase_pd *pd, Pint32 *res_out, size_t width);
Perror_t Pint64_FW_read(P_t *pads, const Pbase_m *m,
			Pbase_pd *pd, Pint64 *res_out, size_t width);
Perror_t Puint8_FW_read (P_t *pads, const Pbase_m *m,
			 Pbase_pd *pd, Puint8 *res_out, size_t width);
Perror_t Puint16_FW_read(P_t *pads, const Pbase_m *m,
			 Pbase_pd *pd, Puint16 *res_out, size_t width);
Perror_t Puint32_FW_read(P_t *pads, const Pbase_m *m,
			 Pbase_pd *pd, Puint32 *res_out, size_t width);
Perror_t Puint64_FW_read(P_t *pads, const Pbase_m *m,
			 Pbase_pd *pd, Puint64 *res_out, size_t width);
#endif

#endif /* P_CONFIG_READ_FUNCTIONS */
#endif /* FOR_CKIT */

/* ================================================================================
 * COMMON WIDTH BINARY INTEGER READ FUNCTIONS
 *
 * These functions parse signed or unsigned binary integers
 * of common bit widths (8, 16, 32, and 64 bit widths).
 * Whether bytes are reversed is controlled by the endian-ness of
 * the machine (determined automatically) and pads->disc->d_endian. If they differ,
 * byte order is reversed in the in-memory representation, otherwise it is not.
 *
 * A good way to set the d_endian value in a machine-independent way is to
 * use PRAGMA CHECK_ENDIAN with the first multi-byte binary integer field that appears
 * in the data.  For example, this header definition:
 *
 * Pstruct header {
 *    Pb_uint16 version : version < 10; //- PRAGMA CHECK_ENDIAN
 *    ..etc..
 * };
 *
 * indicates the first value is a 2-byte unsigned binary integer, version,
 * whose value should be less than 10.   The pragma indicates that there
 * should be two attempts at reading the version field: once with the
 * current pads->disc->d_endian setting, and (if the read fails) once with the
 * opposite pads->disc->d_endian setting.  If the second read succeeds, then
 * the new pads->disc->d_endian setting is retained, otherwise the original
 * pads->disc->d_endian setting is retained.
 * 
 * N.B. The CHECK_ENDIAN pragma is only able to determine the correct endian
 * choice for a field that has an attached constraint, where the
 * wrong choice of endian setting will always cause the constraint to fail.
 * (In the above example, if a value < 10 is read with the wrong d_endian
 * setting, the result is a value that is much greater than 10.) 
 *
 * For all cases, if the specified number of bytes is available, it is always read.
 * If the width is not available:
 *    + pd->loc.b/e set to elt/char position of start/end of the 'too small' field
 *    + IO cursor is not advanced
 *    + if P_Test_NotIgnore(*m), pd->errCode set to P_WIDTH_NOT_AVAILABLE,
 *         pd->nerr set to 1, and an error is reported
 */

#if P_CONFIG_READ_FUNCTIONS > 0

#if P_CONFIG_B_INT > 0
Perror_t Pb_int8_read (P_t *pads, const Pbase_m *m,
		       Pbase_pd *pd, Pint8 *res_out);

Perror_t Pb_int16_read(P_t *pads, const Pbase_m *m,
		       Pbase_pd *pd, Pint16 *res_out);

Perror_t Pb_int32_read(P_t *pads, const Pbase_m *m,
		       Pbase_pd *pd, Pint32 *res_out);

Perror_t Pb_int64_read(P_t *pads, const Pbase_m *m,
		       Pbase_pd *pd, Pint64 *res_out);

Perror_t Pb_uint8_read (P_t *pads, const Pbase_m *m,
			Pbase_pd *pd, Puint8 *res_out);

Perror_t Pb_uint16_read(P_t *pads, const Pbase_m *m,
			Pbase_pd *pd, Puint16 *res_out);

Perror_t Pb_uint32_read(P_t *pads, const Pbase_m *m,
			Pbase_pd *pd, Puint32 *res_out);

Perror_t Pb_uint64_read(P_t *pads, const Pbase_m *m,
			Pbase_pd *pd, Puint64 *res_out);
#endif

#endif /* P_CONFIG_READ_FUNCTIONS */

/* ================================================================================
 * EBC, BCD, and SBL, and SBH ENCODINGS OF INTEGERS
 *   (VARIABLE NUMBER OF DIGITS/BYTES)
 *
 * These functions parse signed or unsigned EBCDIC numeric (ebc_), BCD (bcd_),
 * SBL (sbl_) or SBH (sbh_) encoded integers with a specified number of digits
 * (for ebc_ and bcd_) or number of bytes (for sbl_ and sbh_).
 *
 * EBC INTEGER ENCODING (Pebc_int64_read / Pebc_uint64_read):
 *
 *   Each byte on disk encodes one digit (in low 4 bits).  For signed
 *   values, the final byte encodes the sign (high 4 bits == 0xD for negative).
 *   A signed or unsigned 5 digit value is encoded in 5 bytes.
 *
 * BCD INTEGER ENCODING (Pbcd_int_read / Pbcd_uint_read):
 *
 *   Each byte on disk encodes two digits, 4 bits per digit.  For signed
 *   values, a negative number is encoded by having number of digits be odd
 *   so that the remaining low 4 bits in the last byte are available for the sign.
 *   (low 4 bits == 0xD for negative).
 *   A signed or unsigned 5 digit value is encoded in 3 bytes, where the unsigned
 *   value ignores the final 4 bits and the signed value uses them to get the sign.
 *
 * SBL (Serialized Binary, Low-Order Byte First) INTEGER ENCODING
 *   (Psbl_int_read / Psbl_uint_read):
 *
 *   For a K-byte SBL encoding, the first byte on disk is treated 
 *   as the low order byte of a K byte value.
 *
 * SBH (Serialized Binary, High-Order Byte First) INTEGER ENCODING
 *   (Psbh_int_read / Psbh_uint_read):
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
 * byte ordering, while B uses the pads->disc->d_endian setting to determine the
 * byte ordering of the data.
 *
 * FOR ALL TYPES
 * =============
 *
 * The legal range of values for num_digits (for EBC/BCD) or num_bytes (for SB)
 * depends on target type:
 *    
 * Type        num_digits    num_bytes Min/Max values
 * ----------- ----------    --------- ----------------------------
 * Pint8       1-3           1-1       P_MIN_INT8  / P_MAX_INT8
 * Puint8      1-3           1-1       0           / P_MAX_UINT8
 * Pint16      1-5           1-2       P_MIN_INT16 / P_MAX_INT16
 * Puint16     1-5           1-2       0           / P_MAX_UINT16
 * Pint32      1-10/11**     1-4       P_MIN_INT32 / P_MAX_INT32
 * Puint32     1-10          1-4       0           / P_MAX_UINT32
 * Pint64      1-19          1-8       P_MIN_INT64 / P_MAX_INT64
 * Puint64     1-20          1-8       0           / P_MAX_UINT64
 * 
 * N.B.: num_digits must be odd if the value on disk can be negative.
 *
 * ** For Pbcd_int32_read only, even though the min and max int32 have 10 digits, we allow
 * num_digits == 11 due to the fact that 11 is required for a 10 digit negative value
 * (an actual 11 digit number would cause a range error, so the leading digit must be 0).
 * 
 * For all cases, if the specified number of bytes is NOT available:
 *    + pd->loc.b/e set to elt/char position of start/end of the 'too small' field
 *    + IO cursor is not advanced
 *    + if P_Test_NotIgnore(*m), pd->errCode set to P_WIDTH_NOT_AVAILABLE,
 *         pd->nerr set to 1, and an error is reported
 *
 * Otherwise, the IO cursor is always advanced.  There are 3 error cases that
 * can occur even though the IO cursor advances:
 *
 * If num_digits or num_bytes is not a legal choice for the target type and
 * sign of the value:
 *    + pd->loc.b/e set to elt/char position at the start/end of the field
 *    + if P_Test_NotIgnore(*m), pd->errCode set to P_BAD_PARAM,
 *         pd->nerr set to 1, and an error is reported
 *
 * If the specified bytes make up an integer that does not fit in the target type,
 * or if the actual value is not in the min/max range, then:
 *    + pd->loc.b/e set to elt/char position at the start/end of the field
 *    + if P_Test_NotIgnore(*m), pd->errCode set to P_RANGE,
 *         pd->nerr set to 1, and an error is reported
 *
 * If the specified bytes are not legal EBC/BCD integer bytes, then 
 *    + pd->loc.b/e set to elt/char position at the start/end of the field
 *    + if P_Test_NotIgnore(*m), pd->errCode set to P_INVALID_EBC_NUM or P_INVALID_BCD_NUM,
 *         pd->nerr set to 1, and an error is reported
 */

#if P_CONFIG_READ_FUNCTIONS > 0

#if P_CONFIG_EBC_INT > 0  || P_CONFIG_EBC_FPOINT > 0
Perror_t Pebc_int8_read   (P_t *pads, const Pbase_m *m,
			   Pbase_pd *pd, Pint8 *res_out, Puint32 num_digits);
Perror_t Pebc_int16_read  (P_t *pads, const Pbase_m *m,
			   Pbase_pd *pd, Pint16 *res_out, Puint32 num_digits);
Perror_t Pebc_int32_read  (P_t *pads, const Pbase_m *m,
			   Pbase_pd *pd, Pint32 *res_out, Puint32 num_digits);
Perror_t Pebc_int64_read  (P_t *pads, const Pbase_m *m,
			   Pbase_pd *pd, Pint64 *res_out, Puint32 num_digits);

Perror_t Pebc_uint8_read  (P_t *pads, const Pbase_m *m,
			   Pbase_pd *pd, Puint8 *res_out, Puint32 num_digits);
Perror_t Pebc_uint16_read (P_t *pads, const Pbase_m *m,
			   Pbase_pd *pd, Puint16 *res_out, Puint32 num_digits);
Perror_t Pebc_uint32_read (P_t *pads, const Pbase_m *m,
			   Pbase_pd *pd, Puint32 *res_out, Puint32 num_digits);
Perror_t Pebc_uint64_read (P_t *pads, const Pbase_m *m,
			   Pbase_pd *pd, Puint64 *res_out, Puint32 num_digits);
#endif

#if P_CONFIG_BCD_INT > 0 || P_CONFIG_BCD_FPOINT > 0
Perror_t Pbcd_int8_read   (P_t *pads, const Pbase_m *m,
			   Pbase_pd *pd, Pint8 *res_out, Puint32 num_digits);
Perror_t Pbcd_int16_read  (P_t *pads, const Pbase_m *m,
			   Pbase_pd *pd, Pint16 *res_out, Puint32 num_digits);
Perror_t Pbcd_int32_read  (P_t *pads, const Pbase_m *m,
			   Pbase_pd *pd, Pint32 *res_out, Puint32 num_digits);
Perror_t Pbcd_int64_read  (P_t *pads, const Pbase_m *m,
			   Pbase_pd *pd, Pint64 *res_out, Puint32 num_digits);

Perror_t Pbcd_uint8_read  (P_t *pads, const Pbase_m *m,
			   Pbase_pd *pd, Puint8 *res_out, Puint32 num_digits);
Perror_t Pbcd_uint16_read (P_t *pads, const Pbase_m *m,
			   Pbase_pd *pd, Puint16 *res_out, Puint32 num_digits);
Perror_t Pbcd_uint32_read (P_t *pads, const Pbase_m *m,
			   Pbase_pd *pd, Puint32 *res_out, Puint32 num_digits);
Perror_t Pbcd_uint64_read (P_t *pads, const Pbase_m *m,
			   Pbase_pd *pd, Puint64 *res_out, Puint32 num_digits);
#endif

#if P_CONFIG_SBL_INT > 0 || P_CONFIG_SBL_FPOINT > 0
Perror_t Psbl_int8_read    (P_t *pads, const Pbase_m *m,
			    Pbase_pd *pd, Pint8 *res_out, Puint32 num_bytes);
Perror_t Psbl_int16_read   (P_t *pads, const Pbase_m *m,
			    Pbase_pd *pd, Pint16 *res_out, Puint32 num_bytes);
Perror_t Psbl_int32_read   (P_t *pads, const Pbase_m *m,
			    Pbase_pd *pd, Pint32 *res_out, Puint32 num_bytes);
Perror_t Psbl_int64_read   (P_t *pads, const Pbase_m *m,
			    Pbase_pd *pd, Pint64 *res_out, Puint32 num_bytes);

Perror_t Psbl_uint8_read   (P_t *pads, const Pbase_m *m,
			    Pbase_pd *pd, Puint8 *res_out, Puint32 num_bytes);
Perror_t Psbl_uint16_read  (P_t *pads, const Pbase_m *m,
			    Pbase_pd *pd, Puint16 *res_out, Puint32 num_bytes);
Perror_t Psbl_uint32_read  (P_t *pads, const Pbase_m *m,
			    Pbase_pd *pd, Puint32 *res_out, Puint32 num_bytes);
Perror_t Psbl_uint64_read  (P_t *pads, const Pbase_m *m,
			    Pbase_pd *pd, Puint64 *res_out, Puint32 num_bytes);
#endif

#if P_CONFIG_SBH_INT > 0 || P_CONFIG_SBH_FPOINT > 0
Perror_t Psbh_int8_read    (P_t *pads, const Pbase_m *m,
			    Pbase_pd *pd, Pint8 *res_out, Puint32 num_bytes);
Perror_t Psbh_int16_read   (P_t *pads, const Pbase_m *m,
			    Pbase_pd *pd, Pint16 *res_out, Puint32 num_bytes);
Perror_t Psbh_int32_read   (P_t *pads, const Pbase_m *m,
			    Pbase_pd *pd, Pint32 *res_out, Puint32 num_bytes);
Perror_t Psbh_int64_read   (P_t *pads, const Pbase_m *m,
			    Pbase_pd *pd, Pint64 *res_out, Puint32 num_bytes);

Perror_t Psbh_uint8_read   (P_t *pads, const Pbase_m *m,
			    Pbase_pd *pd, Puint8 *res_out, Puint32 num_bytes);
Perror_t Psbh_uint16_read  (P_t *pads, const Pbase_m *m,
			    Pbase_pd *pd, Puint16 *res_out, Puint32 num_bytes);
Perror_t Psbh_uint32_read  (P_t *pads, const Pbase_m *m,
			    Pbase_pd *pd, Puint32 *res_out, Puint32 num_bytes);
Perror_t Psbh_uint64_read  (P_t *pads, const Pbase_m *m,
			    Pbase_pd *pd, Puint64 *res_out, Puint32 num_bytes);
#endif

#endif /* P_CONFIG_READ_FUNCTIONS */

/* ================================================================================
 * FIXED POINT READ FUNCTIONS
 * FOR EBC (ebc_), BCD (bcd_), SBL (sbl_), and SBH (sbh_) ENCODINGS
 *
 * An fpoint or ufpoint number is a signed or unsigned fixed-point
 * rational number with a numerator and denominator that both have the
 * same size.  For signed fpoint types, the numerator carries the sign, while
 * the denominator is always unsigned.  For example, type Pfpoint16
 * has a signed Pint16 numerator and an unsigned Puint16 denominator.
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
 * Pfpoint8  /  ufpoint8  0-2                             100
 * Pfpoint16 / ufpoint16  0-4                          10,000
 * Pfpoint32 / ufpoint32  0-9                   1,000,000,000
 * Pfpoint64 / ufpoint64  0-19     10,000,000,000,000,000,000
 *
 * The legal range of values for num_digits (for EBC/BCD) or num_bytes (for SBL/SBH)
 * depends on target type, and is the same as specified above for the
 * EBC/BCD/SBL/SBH integer read functions.
 *    
 * For all cases, if the specified number of bytes are NOT available:
 *    + pd->loc.b/e set to elt/char position of start/end of the 'too small' field
 *    + IO cursor not advanced
 *    + if PD_Test_NotIgnore(*m), pd->errCode set to P_WIDTH_NOT_AVAILABLE,
 *         pd->nerr set to 1, and an error is reported
 *
 * Otherwise, the IO cursor is always advanced.  There are 3 error cases that
 * can occur even though the IO cursor advances:
 *
 * If num_digits, num_bytes, or d_exp is not in a legal choice for the target type
 * and sign of the value:
 *    + pd->loc.b/e set to elt/char position at the start/end of the field
 *    + if PD_Test_NotIgnore(*m), pd->errCode set to P_BAD_PARAM,
 *         pd->nerr set to 1, and an error is reported
 *
 * If the actual numerator is not in the min/max numerator range, then:
 *    + pd->loc.b/e set to elt/char position at the start/end of the field
 *    + if PD_Test_NotIgnore(*m), pd->errCode set to P_RANGE,
 *         pd->nerr set to 1, and an error is reported
 *
 * If the specified bytes are not legal EBC/BCD integer bytes, then 
 *    + pd->loc.b/e set to elt/char position at the start/end of the field
 *    + if PD_Test_NotIgnore(*m), pd->errCode set to P_INVALID_EBC_NUM or P_INVALID_BCD_NUM,
 *         pd->nerr set to 1, and an error is reported
 *
 */

#if P_CONFIG_READ_FUNCTIONS > 0

#if P_CONFIG_EBC_FPOINT > 0
Perror_t Pebc_fpoint8_read   (P_t *pads, const Pbase_m *m,
			      Pbase_pd *pd, Pfpoint8 *res_out, Puint32 num_digits, Puint32 d_exp);
Perror_t Pebc_fpoint16_read  (P_t *pads, const Pbase_m *m,
			      Pbase_pd *pd, Pfpoint16 *res_out, Puint32 num_digits, Puint32 d_exp);
Perror_t Pebc_fpoint32_read  (P_t *pads, const Pbase_m *m,
			      Pbase_pd *pd, Pfpoint32 *res_out, Puint32 num_digits, Puint32 d_exp);
Perror_t Pebc_fpoint64_read  (P_t *pads, const Pbase_m *m,
			      Pbase_pd *pd, Pfpoint64 *res_out, Puint32 num_digits, Puint32 d_exp);

Perror_t Pebc_ufpoint8_read  (P_t *pads, const Pbase_m *m,
			      Pbase_pd *pd, Pufpoint8 *res_out, Puint32 num_digits, Puint32 d_exp);
Perror_t Pebc_ufpoint16_read (P_t *pads, const Pbase_m *m,
			      Pbase_pd *pd, Pufpoint16 *res_out, Puint32 num_digits, Puint32 d_exp);
Perror_t Pebc_ufpoint32_read (P_t *pads, const Pbase_m *m,
			      Pbase_pd *pd, Pufpoint32 *res_out, Puint32 num_digits, Puint32 d_exp);
Perror_t Pebc_ufpoint64_read (P_t *pads, const Pbase_m *m,
			      Pbase_pd *pd, Pufpoint64 *res_out, Puint32 num_digits, Puint32 d_exp);
#endif

#if P_CONFIG_BCD_FPOINT > 0
Perror_t Pbcd_fpoint8_read   (P_t *pads, const Pbase_m *m,
			      Pbase_pd *pd, Pfpoint8 *res_out, Puint32 num_digits, Puint32 d_exp);
Perror_t Pbcd_fpoint16_read  (P_t *pads, const Pbase_m *m,
			      Pbase_pd *pd, Pfpoint16 *res_out, Puint32 num_digits, Puint32 d_exp);
Perror_t Pbcd_fpoint32_read  (P_t *pads, const Pbase_m *m,
			      Pbase_pd *pd, Pfpoint32 *res_out, Puint32 num_digits, Puint32 d_exp);
Perror_t Pbcd_fpoint64_read  (P_t *pads, const Pbase_m *m,
			      Pbase_pd *pd, Pfpoint64 *res_out, Puint32 num_digits, Puint32 d_exp);

Perror_t Pbcd_ufpoint8_read  (P_t *pads, const Pbase_m *m,
			      Pbase_pd *pd, Pufpoint8 *res_out, Puint32 num_digits, Puint32 d_exp);
Perror_t Pbcd_ufpoint16_read (P_t *pads, const Pbase_m *m,
			      Pbase_pd *pd, Pufpoint16 *res_out, Puint32 num_digits, Puint32 d_exp);
Perror_t Pbcd_ufpoint32_read (P_t *pads, const Pbase_m *m,
			      Pbase_pd *pd, Pufpoint32 *res_out, Puint32 num_digits, Puint32 d_exp);
Perror_t Pbcd_ufpoint64_read (P_t *pads, const Pbase_m *m,
			      Pbase_pd *pd, Pufpoint64 *res_out, Puint32 num_digits, Puint32 d_exp);
#endif

#if P_CONFIG_SBL_FPOINT > 0
Perror_t Psbl_fpoint8_read    (P_t *pads, const Pbase_m *m,
			       Pbase_pd *pd, Pfpoint8 *res_out, Puint32 num_bytes, Puint32 d_exp);
Perror_t Psbl_fpoint16_read   (P_t *pads, const Pbase_m *m,
			       Pbase_pd *pd, Pfpoint16 *res_out, Puint32 num_bytes, Puint32 d_exp);
Perror_t Psbl_fpoint32_read   (P_t *pads, const Pbase_m *m,
			       Pbase_pd *pd, Pfpoint32 *res_out, Puint32 num_bytes, Puint32 d_exp);
Perror_t Psbl_fpoint64_read   (P_t *pads, const Pbase_m *m,
			       Pbase_pd *pd, Pfpoint64 *res_out, Puint32 num_bytes, Puint32 d_exp);

Perror_t Psbl_ufpoint8_read   (P_t *pads, const Pbase_m *m,
			       Pbase_pd *pd, Pufpoint8 *res_out, Puint32 num_bytes, Puint32 d_exp);
Perror_t Psbl_ufpoint16_read  (P_t *pads, const Pbase_m *m,
			       Pbase_pd *pd, Pufpoint16 *res_out, Puint32 num_bytes, Puint32 d_exp);
Perror_t Psbl_ufpoint32_read  (P_t *pads, const Pbase_m *m,
			       Pbase_pd *pd, Pufpoint32 *res_out, Puint32 num_bytes, Puint32 d_exp);
Perror_t Psbl_ufpoint64_read  (P_t *pads, const Pbase_m *m,
			       Pbase_pd *pd, Pufpoint64 *res_out, Puint32 num_bytes, Puint32 d_exp);
#endif

#if P_CONFIG_SBH_FPOINT > 0
Perror_t Psbh_fpoint8_read    (P_t *pads, const Pbase_m *m,
			       Pbase_pd *pd, Pfpoint8 *res_out, Puint32 num_bytes, Puint32 d_exp);
Perror_t Psbh_fpoint16_read   (P_t *pads, const Pbase_m *m,
			       Pbase_pd *pd, Pfpoint16 *res_out, Puint32 num_bytes, Puint32 d_exp);
Perror_t Psbh_fpoint32_read   (P_t *pads, const Pbase_m *m,
			       Pbase_pd *pd, Pfpoint32 *res_out, Puint32 num_bytes, Puint32 d_exp);
Perror_t Psbh_fpoint64_read   (P_t *pads, const Pbase_m *m,
			       Pbase_pd *pd, Pfpoint64 *res_out, Puint32 num_bytes, Puint32 d_exp);

Perror_t Psbh_ufpoint8_read   (P_t *pads, const Pbase_m *m,
			       Pbase_pd *pd, Pufpoint8 *res_out, Puint32 num_bytes, Puint32 d_exp);
Perror_t Psbh_ufpoint16_read  (P_t *pads, const Pbase_m *m,
			       Pbase_pd *pd, Pufpoint16 *res_out, Puint32 num_bytes, Puint32 d_exp);
Perror_t Psbh_ufpoint32_read  (P_t *pads, const Pbase_m *m,
			       Pbase_pd *pd, Pufpoint32 *res_out, Puint32 num_bytes, Puint32 d_exp);
Perror_t Psbh_ufpoint64_read  (P_t *pads, const Pbase_m *m,
			       Pbase_pd *pd, Pufpoint64 *res_out, Puint32 num_bytes, Puint32 d_exp);
#endif

#endif /* P_CONFIG_READ_FUNCTIONS */

/* ================================================================================
 * CHARACTER-BASED FLOAT READ FUNCTIONS
 * 
 * DEFAULT                        ASCII                          EBCDIC
 * -----------------------------  -----------------------------  -----------------------------
 * Pfloat32_read                  Pa_float32_read                Pe_float32_read
 * Pfloat64_read                  Pa_float64_read                Pe_float64_read
 */

#if P_CONFIG_READ_FUNCTIONS > 0

#if P_CONFIG_A_FLOAT > 0
Perror_t Pa_float32_read(P_t *pads, const Pbase_m *m,
			 Pbase_pd *pd, Pfloat32 *res_out);
Perror_t Pa_float64_read(P_t *pads, const Pbase_m *m,
			 Pbase_pd *pd, Pfloat64 *res_out);
#endif

#if P_CONFIG_E_FLOAT > 0
Perror_t Pe_float32_read(P_t *pads, const Pbase_m *m,
			 Pbase_pd *pd, Pfloat32 *res_out);
Perror_t Pe_float64_read(P_t *pads, const Pbase_m *m,
			 Pbase_pd *pd, Pfloat64 *res_out);
#endif

#ifdef FOR_CKIT
#if P_CONFIG_A_FLOAT > 0 && P_CONFIG_E_FLOAT > 0
Perror_t Pfloat32_read(P_t *pads, const Pbase_m *m,
		       Pbase_pd *pd, Pfloat32 *res_out);
Perror_t Pfloat64_read(P_t *pads, const Pbase_m *m,
		       Pbase_pd *pd, Pfloat64 *res_out);
#endif
#endif

#endif /* P_CONFIG_READ_FUNCTIONS */


/* ********************************************************************************
 * WRITE FUNCTIONS: GENERAL NOTES
 * ********************************************************************************
 * XXX_TODOC : discuss general issues writing to stream, to buffer
 */

/* ================================================================================
 * LITERAL WRITE FUNCTIONS
 *   Literal write functions: write a char or string to an sfio stream or buffer.
 *   Typically used with a literal argument, as in 
 *      Pa_cstr_lit_write(pads, io, "hello");
 *   Note that these are similar to Pa_char and Pa_string write functions
 *   except there is no Pbase_pd argument since literals have no errors.
 *
 * XXX_TODOC
 */

#ifdef FOR_CKIT
#if P_CONFIG_WRITE_FUNCTIONS > 0

#if P_CONFIG_A_CHAR_STRING > 0
ssize_t Pa_char_lit_write2io(P_t *pads, Sfio_t *io, Pchar c);
ssize_t Pa_str_lit_write2io (P_t *pads, Sfio_t *io, const Pstring *s);
ssize_t Pa_cstr_lit_write2io(P_t *pads, Sfio_t *io, const char *s);

ssize_t Pa_char_lit_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pchar c);
ssize_t Pa_str_lit_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, const Pstring *s);
ssize_t Pa_cstr_lit_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, const char *s);

ssize_t Pa_char_lit_write_xml_2io(P_t *pads, Sfio_t *io, Pchar c, const char *tag, int indent);
ssize_t Pa_str_lit_write_xml_2io (P_t *pads, Sfio_t *io, const Pstring *s, const char *tag, int indent);
ssize_t Pa_cstr_lit_write_xml_2io(P_t *pads, Sfio_t *io, const char *s, const char *tag, int indent);

ssize_t Pa_char_lit_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pchar c, const char *tag, int indent);
ssize_t Pa_str_lit_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, const Pstring *s, const char *tag, int indent);
ssize_t Pa_cstr_lit_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, const char *s, const char *tag, int indent);
#endif

#if P_CONFIG_E_CHAR_STRING > 0
ssize_t Pe_char_lit_write2io(P_t *pads, Sfio_t *io, Pchar c);
ssize_t Pe_str_lit_write2io (P_t *pads, Sfio_t *io, const Pstring *s);
ssize_t Pe_cstr_lit_write2io(P_t *pads, Sfio_t *io, const char *s);

ssize_t Pe_char_lit_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pchar c);
ssize_t Pe_str_lit_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, const Pstring *s);
ssize_t Pe_cstr_lit_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, const char *s);

ssize_t Pe_char_lit_write_xml_2io(P_t *pads, Sfio_t *io, Pchar c, const char *tag, int indent);
ssize_t Pe_str_lit_write_xml_2io (P_t *pads, Sfio_t *io, const Pstring *s, const char *tag, int indent);
ssize_t Pe_cstr_lit_write_xml_2io(P_t *pads, Sfio_t *io, const char *s, const char *tag, int indent);

ssize_t Pe_char_lit_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pchar c, const char *tag, int indent);
ssize_t Pe_str_lit_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, const Pstring *s, const char *tag, int indent);
ssize_t Pe_cstr_lit_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, const char *s, const char *tag, int indent);
#endif

#if P_CONFIG_A_CHAR_STRING > 0 && P_CONFIG_E_CHAR_STRING > 0
ssize_t Pchar_lit_write2io(P_t *pads, Sfio_t *io, Pchar c);
ssize_t Pstr_lit_write2io (P_t *pads, Sfio_t *io, const Pstring *s);
ssize_t Pcstr_lit_write2io(P_t *pads, Sfio_t *io, const char *s);

ssize_t Pchar_lit_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pchar c);
ssize_t Pstr_lit_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, const Pstring *s);
ssize_t Pcstr_lit_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, const char *s);

ssize_t Pchar_lit_write_xml_2io(P_t *pads, Sfio_t *io, Pchar c, const char *tag, int indent);
ssize_t Pstr_lit_write_xml_2io (P_t *pads, Sfio_t *io, const Pstring *s, const char *tag, int indent);
ssize_t Pcstr_lit_write_xml_2io(P_t *pads, Sfio_t *io, const char *s, const char *tag, int indent);

ssize_t Pchar_lit_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pchar c, const char *tag, int indent);
ssize_t Pstr_lit_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, const Pstring *s, const char *tag, int indent);
ssize_t Pcstr_lit_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, const char *s, const char *tag, int indent);
#endif

#endif /* P_CONFIG_WRITE_FUNCTIONS */
#endif /* FOR_CKIT */

/* ================================================================================
 * CHAR WRITE FUNCTIONS
 * DEFAULT                        ASCII                          EBCDIC
 * -----------------------------  -----------------------------  -----------------------------
 * Pchar_write2io                 Pa_char_write2io               Pe_char_write2io
 *
 * Pchar_write2buf                Pa_char_write2buf              Pe_char_write2buf
 */

#ifdef FOR_CKIT
#if P_CONFIG_WRITE_FUNCTIONS > 0

#if P_CONFIG_A_CHAR_STRING > 0
ssize_t Pa_char_write2io   (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pchar *c);
ssize_t Pa_char_write2buf  (P_t *pads, Pbyte *buf, size_t buf_len, ssize_t *buf_full, Pbase_pd *pd, Pchar *c);

ssize_t Pa_char_write_xml_2io   (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pchar *c, const char *tag, int indent);
ssize_t Pa_char_write_xml_2buf  (P_t *pads, Pbyte *buf, size_t buf_len, ssize_t *buf_full, Pbase_pd *pd, Pchar *c, const char *tag, int indent);
#endif

#if P_CONFIG_E_CHAR_STRING > 0
ssize_t Pe_char_write2io   (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pchar *c);
ssize_t Pe_char_write2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pchar *c);

ssize_t Pe_char_write_xml_2io   (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pchar *c, const char *tag, int indent);
ssize_t Pe_char_write_xml_2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pchar *c, const char *tag, int indent);
#endif

#if P_CONFIG_A_CHAR_STRING > 0 && P_CONFIG_E_CHAR_STRING > 0
ssize_t Pchar_write2io     (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pchar *c);
ssize_t Pchar_write2buf    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pchar *c);

ssize_t Pchar_write_xml_2io     (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pchar *c, const char *tag, int indent);
ssize_t Pchar_write_xml_2buf    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pchar *c, const char *tag, int indent);
#endif

#endif /* P_CONFIG_WRITE_FUNCTIONS */
#endif /* FOR_CKIT */

/* ================================================================================
 * STRING WRITE FUNCTIONS
 * DEFAULT                        ASCII                          EBCDIC
 * -----------------------------  -----------------------------  -----------------------------
 * Pstring_FW_write2io            Pa_string_FW_write2io          Pe_string_FW_write2io
 * Pstring_write2io               Pa_string_write2io             Pe_string_write2io
 * Pstring_ME_write2io            Pa_string_ME_write2io          Pe_string_ME_write2io
 * Pstring_CME_write2io           Pa_string_CME_write2io         Pe_string_CME_write2io
 * Pstring_SE_write2io            Pa_string_SE_write2io          Pe_string_SE_write2io
 * Pstring_CSE_write2io           Pa_string_CSE_write2io         Pe_string_CSE_write2io
 *
 * Pstring_FW_write2buf           Pa_string_FW_write2buf         Pe_string_FW_write2buf
 * Pstring_write2buf              Pa_string_write2buf            Pe_string_write2buf
 * Pstring_ME_write2buf           Pa_string_ME_write2buf         Pe_string_ME_write2buf
 * Pstring_CME_write2buf          Pa_string_CME_write2buf        Pe_string_CME_write2buf
 * Pstring_SE_write2buf           Pa_string_SE_write2buf         Pe_string_SE_write2buf
 * Pstring_CSE_write2buf          Pa_string_CSE_write2buf        Pe_string_CSE_write2buf
 */

#ifdef FOR_CKIT
#if P_CONFIG_WRITE_FUNCTIONS > 0

#if P_CONFIG_A_CHAR_STRING > 0
ssize_t Pa_string_FW_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pstring *s, size_t width);
ssize_t Pa_string_FW_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				Pstring *s, size_t width);
ssize_t Pa_string_write2io     (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pstring *s, Pchar stopChar);
ssize_t Pa_string_write2buf    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pstring *s, Pchar stopChar);
ssize_t Pa_string_ME_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pstring *s, const char *matchRegexp);
ssize_t Pa_string_ME_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pstring *s, const char *matchRegexp);
ssize_t Pa_string_CME_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pstring *s, Pregexp_t *matchRegexp);
ssize_t Pa_string_CME_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pstring *s, Pregexp_t *matchRegexp);
ssize_t Pa_string_SE_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pstring *s, const char *stopRegexp);
ssize_t Pa_string_SE_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				Pstring *s, const char *stopRegexp);
ssize_t Pa_string_CSE_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pstring *s, Pregexp_t *stopRegexp);
ssize_t Pa_string_CSE_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				Pstring *s, Pregexp_t *stopRegexp);


ssize_t Pa_string_FW_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pstring *s, const char *tag, int indent, size_t width);
ssize_t Pa_string_FW_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				     Pstring *s, const char *tag, int indent, size_t width);
ssize_t Pa_string_write_xml_2io     (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pstring *s, const char *tag, int indent, Pchar stopChar);
ssize_t Pa_string_write_xml_2buf    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pstring *s, const char *tag, int indent, Pchar stopChar);
ssize_t Pa_string_ME_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pstring *s, const char *tag, int indent, const char *matchRegexp);
ssize_t Pa_string_ME_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				     Pstring *s, const char *tag, int indent, const char *matchRegexp);
ssize_t Pa_string_CME_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pstring *s, const char *tag, int indent, Pregexp_t *matchRegexp);
ssize_t Pa_string_CME_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				     Pstring *s, const char *tag, int indent, Pregexp_t *matchRegexp);
ssize_t Pa_string_SE_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pstring *s, const char *tag, int indent, const char *stopRegexp);
ssize_t Pa_string_SE_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				     Pstring *s, const char *tag, int indent, const char *stopRegexp);
ssize_t Pa_string_CSE_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pstring *s, const char *tag, int indent, Pregexp_t *stopRegexp);
ssize_t Pa_string_CSE_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				     Pstring *s, const char *tag, int indent, Pregexp_t *stopRegexp);
#endif

#if P_CONFIG_E_CHAR_STRING > 0
ssize_t Pe_string_FW_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pstring *s, size_t width);
ssize_t Pe_string_FW_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				Pstring *s, size_t width);
ssize_t Pe_string_write2io     (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pstring *s, Pchar stopChar);
ssize_t Pe_string_write2buf    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pstring *s, Pchar stopChar);
ssize_t Pe_string_ME_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pstring *s, const char *matchRegexp);
ssize_t Pe_string_ME_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				Pstring *s, const char *matchRegexp);
ssize_t Pe_string_CME_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pstring *s, Pregexp_t *matchRegexp);
ssize_t Pe_string_CME_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				Pstring *s, Pregexp_t *matchRegexp);
ssize_t Pe_string_SE_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pstring *s, const char *stopRegexp);
ssize_t Pe_string_SE_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				Pstring *s, const char *stopRegexp);
ssize_t Pe_string_CSE_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pstring *s, Pregexp_t *stopRegexp);
ssize_t Pe_string_CSE_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				Pstring *s, Pregexp_t *stopRegexp);

ssize_t Pe_string_FW_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pstring *s, const char *tag, int indent, size_t width);
ssize_t Pe_string_FW_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				     Pstring *s, const char *tag, int indent, size_t width);
ssize_t Pe_string_write_xml_2io     (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pstring *s, const char *tag, int indent, Pchar stopChar);
ssize_t Pe_string_write_xml_2buf    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pstring *s, const char *tag, int indent, Pchar stopChar);
ssize_t Pe_string_ME_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pstring *s, const char *tag, int indent, const char *matchRegexp);
ssize_t Pe_string_ME_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				     Pstring *s, const char *tag, int indent, const char *matchRegexp);
ssize_t Pe_string_CME_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pstring *s, const char *tag, int indent, Pregexp_t *matchRegexp);
ssize_t Pe_string_CME_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				     Pstring *s, const char *tag, int indent, Pregexp_t *matchRegexp);
ssize_t Pe_string_SE_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pstring *s, const char *tag, int indent, const char *stopRegexp);
ssize_t Pe_string_SE_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				     Pstring *s, const char *tag, int indent, const char *stopRegexp);
ssize_t Pe_string_CSE_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pstring *s, const char *tag, int indent, Pregexp_t *stopRegexp);
ssize_t Pe_string_CSE_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				     Pstring *s, const char *tag, int indent, Pregexp_t *stopRegexp);
#endif

#if P_CONFIG_A_CHAR_STRING > 0 && P_CONFIG_E_CHAR_STRING > 0
ssize_t Pstring_FW_write2io    (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pstring *s, size_t width);
ssize_t Pstring_FW_write2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				Pstring *s, size_t width);
ssize_t Pstring_write2io       (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pstring *s, Pchar stopChar);
ssize_t Pstring_write2buf      (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pstring *s, Pchar stopChar);
ssize_t Pstring_ME_write2io    (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pstring *s, const char *matchRegexp);
ssize_t Pstring_ME_write2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full,
				Pbase_pd *pd, Pstring *s, const char *matchRegexp);
ssize_t Pstring_CME_write2io   (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pstring *s, Pregexp_t *matchRegexp);
ssize_t Pstring_CME_write2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				Pstring *s, Pregexp_t *matchRegexp);
ssize_t Pstring_SE_write2io    (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pstring *s, const char *stopRegexp);
ssize_t Pstring_SE_write2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pstring *s, const char *stopRegexp);
ssize_t Pstring_CSE_write2io   (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pstring *s, Pregexp_t *stopRegexp);
ssize_t Pstring_CSE_write2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pstring *s, Pregexp_t *stopRegexp);

ssize_t Pstring_FW_write_xml_2io    (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pstring *s, const char *tag, int indent, size_t width);
ssize_t Pstring_FW_write_xml_2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				     Pstring *s, const char *tag, int indent, size_t width);
ssize_t Pstring_write_xml_2io       (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pstring *s, const char *tag, int indent, Pchar stopChar);
ssize_t Pstring_write_xml_2buf      (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pstring *s, const char *tag, int indent, Pchar stopChar);
ssize_t Pstring_ME_write_xml_2io    (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pstring *s, const char *tag, int indent, const char *matchRegexp);
ssize_t Pstring_ME_write_xml_2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				     Pstring *s, const char *tag, int indent, const char *matchRegexp);
ssize_t Pstring_CME_write_xml_2io   (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pstring *s, const char *tag, int indent, Pregexp_t *matchRegexp);
ssize_t Pstring_CME_write_xml_2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				     Pstring *s, const char *tag, int indent, Pregexp_t *matchRegexp);
ssize_t Pstring_SE_write_xml_2io    (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pstring *s, const char *tag, int indent, const char *stopRegexp);
ssize_t Pstring_SE_write_xml_2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				     Pstring *s, const char *tag, int indent, const char *stopRegexp);
ssize_t Pstring_CSE_write_xml_2io   (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pstring *s, const char *tag, int indent, Pregexp_t *stopRegexp);
ssize_t Pstring_CSE_write_xml_2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				     Pstring *s, const char *tag, int indent, Pregexp_t *stopRegexp);
#endif

#endif /* P_CONFIG_WRITE_FUNCTIONS */
#endif /* FOR_CKIT */

/* ================================================================================
 * DATE WRITE FUNCTIONS
 * DEFAULT                        ASCII                          EBCDIC
 * -----------------------------  -----------------------------  -----------------------------
 * Pdate_FW_write2io              Pa_date_FW_write2io            Pe_date_FW_write2io
 * Pdate_write2io                 Pa_date_write2io               Pe_date_write2io
 * Pdate_ME_write2io              Pa_date_ME_write2io            Pe_date_ME_write2io
 * Pdate_CME_write2io             Pa_date_CME_write2io           Pe_date_CME_write2io
 * Pdate_SE_write2io              Pa_date_SE_write2io            Pe_date_SE_write2io
 * Pdate_CSE_write2io             Pa_date_CSE_write2io           Pe_date_CSE_write2io
 *
 * Pdate_FW_write2buf             Pa_date_FW_write2buf           Pe_date_FW_write2buf
 * Pdate_write2buf                Pa_date_write2buf              Pe_date_write2buf
 * Pdate_ME_write2buf             Pa_date_ME_write2buf           Pe_date_ME_write2buf
 * Pdate_CME_write2buf            Pa_date_CME_write2buf          Pe_date_CME_write2buf
 * Pdate_SE_write2buf             Pa_date_SE_write2buf           Pe_date_SE_write2buf
 * Pdate_CSE_write2buf            Pa_date_CSE_write2buf          Pe_date_CSE_write2buf
 */

#ifdef FOR_CKIT
#if P_CONFIG_WRITE_FUNCTIONS > 0

#if P_CONFIG_A_CHAR_STRING > 0
ssize_t Pa_date_FW_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, size_t width);
ssize_t Pa_date_FW_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *d, size_t width);
ssize_t Pa_date_write2io     (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, Pchar stopChar);
ssize_t Pa_date_write2buf    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *d, Pchar stopChar);
ssize_t Pa_date_ME_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *matchRegexp);
ssize_t Pa_date_ME_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, const char *matchRegexp);
ssize_t Pa_date_CME_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, Pregexp_t *matchRegexp);
ssize_t Pa_date_CME_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, Pregexp_t *matchRegexp);
ssize_t Pa_date_SE_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *stopRegexp);
ssize_t Pa_date_SE_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, const char *stopRegexp);
ssize_t Pa_date_CSE_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, Pregexp_t *stopRegexp);
ssize_t Pa_date_CSE_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, Pregexp_t *stopRegexp);

ssize_t Pa_date_FW_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, size_t width);
ssize_t Pa_date_FW_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, size_t width);

ssize_t Pa_date_write_xml_2io     (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pchar stopChar);
ssize_t Pa_date_write_xml_2buf    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pchar stopChar);
ssize_t Pa_date_ME_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, const char *matchRegexp);
ssize_t Pa_date_ME_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, const char *matchRegexp);
ssize_t Pa_date_CME_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pregexp_t *matchRegexp);
ssize_t Pa_date_CME_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, Pregexp_t *matchRegexp);
ssize_t Pa_date_SE_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, const char *stopRegexp);
ssize_t Pa_date_SE_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, const char *stopRegexp);
ssize_t Pa_date_CSE_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pregexp_t *stopRegexp);
ssize_t Pa_date_CSE_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, Pregexp_t *stopRegexp);
#endif

#if P_CONFIG_E_CHAR_STRING > 0
ssize_t Pe_date_FW_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, size_t width);
ssize_t Pe_date_FW_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, size_t width);
ssize_t Pe_date_write2io     (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, Pchar stopChar);
ssize_t Pe_date_write2buf    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *d, Pchar stopChar);
ssize_t Pe_date_ME_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *matchRegexp);
ssize_t Pe_date_ME_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, const char *matchRegexp);
ssize_t Pe_date_CME_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, Pregexp_t *matchRegexp);
ssize_t Pe_date_CME_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, Pregexp_t *matchRegexp);
ssize_t Pe_date_SE_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *stopRegexp);
ssize_t Pe_date_SE_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, const char *stopRegexp);
ssize_t Pe_date_CSE_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, Pregexp_t *stopRegexp);
ssize_t Pe_date_CSE_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, Pregexp_t *stopRegexp);

ssize_t Pe_date_FW_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, size_t width);
ssize_t Pe_date_FW_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, size_t width);
ssize_t Pe_date_write_xml_2io     (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pchar stopChar);
ssize_t Pe_date_write_xml_2buf    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pchar stopChar);
ssize_t Pe_date_ME_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, const char *matchRegexp);
ssize_t Pe_date_ME_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, const char *matchRegexp);
ssize_t Pe_date_CME_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pregexp_t *matchRegexp);
ssize_t Pe_date_CME_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, Pregexp_t *matchRegexp);
ssize_t Pe_date_SE_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, const char *stopRegexp);
ssize_t Pe_date_SE_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, const char *stopRegexp);
ssize_t Pe_date_CSE_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pregexp_t *stopRegexp);
ssize_t Pe_date_CSE_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, Pregexp_t *stopRegexp);
#endif

#if P_CONFIG_A_CHAR_STRING > 0 && P_CONFIG_E_CHAR_STRING > 0
ssize_t Pdate_FW_write2io    (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, size_t width);
ssize_t Pdate_FW_write2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, size_t width);
ssize_t Pdate_write2io       (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, Pchar stopChar);
ssize_t Pdate_write2buf      (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *d, Pchar stopChar);
ssize_t Pdate_ME_write2io    (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *matchRegexp);
ssize_t Pdate_ME_write2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, const char *matchRegexp);
ssize_t Pdate_CME_write2io   (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, Pregexp_t *matchRegexp);
ssize_t Pdate_CME_write2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, Pregexp_t *matchRegexp);
ssize_t Pdate_SE_write2io    (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *stopRegexp);
ssize_t Pdate_SE_write2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, const char *stopRegexp);
ssize_t Pdate_CSE_write2io   (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, Pregexp_t *stopRegexp);
ssize_t Pdate_CSE_write2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, Pregexp_t *stopRegexp);

ssize_t Pdate_FW_write_xml_2io    (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, size_t width);
ssize_t Pdate_FW_write_xml_2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, size_t width);
ssize_t Pdate_write_xml_2io       (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pchar stopChar);
ssize_t Pdate_write_xml_2buf      (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pchar stopChar);
ssize_t Pdate_ME_write_xml_2io    (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, const char *matchRegexp);
ssize_t Pdate_ME_write_xml_2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, const char *matchRegexp);
ssize_t Pdate_CME_write_xml_2io   (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pregexp_t *matchRegexp);
ssize_t Pdate_CME_write_xml_2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, Pregexp_t *matchRegexp);
ssize_t Pdate_SE_write_xml_2io    (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, const char *stopRegexp);
ssize_t Pdate_SE_write_xml_2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, const char *stopRegexp);
ssize_t Pdate_CSE_write_xml_2io   (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pregexp_t *stopRegexp);
ssize_t Pdate_CSE_write_xml_2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, Pregexp_t *stopRegexp);
#endif

#endif /* P_CONFIG_WRITE_FUNCTIONS */
#endif /* FOR_CKIT */

/* ================================================================================
 * IPADDR WRITE FUNCTIONS
 * DEFAULT                        ASCII                          EBCDIC
 * -----------------------------  -----------------------------  -----------------------------
 * Pipaddr_write2io               Pa_ipaddr_write2io             Pe_ipaddr_write2io 
 *
 * Pipaddr_write2buf              Pa_ipaddr_write2buf            Pe_ipaddr_write2buf
 */

#ifdef FOR_CKIT
#if P_CONFIG_WRITE_FUNCTIONS > 0

#if P_CONFIG_A_CHAR_STRING > 0
ssize_t Pa_ipaddr_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, Pchar stopChar);
ssize_t Pa_ipaddr_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *d, Pchar stopChar);

ssize_t Pa_ipaddr_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pchar stopChar);
ssize_t Pa_ipaddr_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pchar stopChar);
#endif

#if P_CONFIG_E_CHAR_STRING > 0
ssize_t Pe_ipaddr_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, Pchar stopChar);
ssize_t Pe_ipaddr_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *d, Pchar stopChar);

ssize_t Pe_ipaddr_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pchar stopChar);
ssize_t Pe_ipaddr_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pchar stopChar);
#endif

#if P_CONFIG_A_CHAR_STRING > 0 && P_CONFIG_E_CHAR_STRING > 0
ssize_t Pipaddr_write2io   (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, Pchar stopChar);
ssize_t Pipaddr_write2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *d, Pchar stopChar);

ssize_t Pipaddr_write_xml_2io   (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pchar stopChar);
ssize_t Pipaddr_write_xml_2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pchar stopChar);
#endif

#endif /* P_CONFIG_WRITE_FUNCTIONS */
#endif /* FOR_CKIT */

/* ================================================================================
 * INTEGER/FPOINT WRITE FUNCTIONS
 * 
 * For each integer or fpoint read function there is a corresponding write2io
 * function and a corresponding write2buf function which output the specified
 * value in a format that will allow the corresponding read function to 
 * successfully read the value.
 *
 * For example, if a Pint8 is written using Pe_int8_write2io, the bytes
 * that were output can be read back into a Pint8 using Pe_int8_read.
 *
 * All write functions take an Sfio_t* stream pointer (the stream to write to),
 * a parse descriptor pd, and a pointer to the value to be written.  Some also take
 * additional arguments, such as num_digits.  All return an integer.
 *
 * If pd->errCode is either P_NO_ERR or P_USER_CONSTRAINT_VIOLATIONS then
 * the value is assumed to have been filled in, and it is the value written.
 * For other error codes, the value is assumed to *not* have been filled in,
 * and an error value is written.  See the Default Error Value discussion above
 * for the set of default error values and details on how to override them.
 *
 * If the write succeeds, the return value is the number of bytes written.
 * If it fails, -1 is returned, and no bytes are written to the stream.
 */

/* write2io functions */

#if P_CONFIG_WRITE_FUNCTIONS > 0

#if P_CONFIG_A_INT > 0
ssize_t Pa_int8_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint8   *val);
ssize_t Pa_int16_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint16  *val);
ssize_t Pa_int32_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint32  *val);
ssize_t Pa_int64_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint64  *val);

ssize_t Pa_uint8_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint8  *val);
ssize_t Pa_uint16_write2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint16 *val);
ssize_t Pa_uint32_write2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *val);
ssize_t Pa_uint64_write2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint64 *val);

ssize_t Pa_int8_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint8   *val, const char *tag, int indent);
ssize_t Pa_int16_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint16  *val, const char *tag, int indent);
ssize_t Pa_int32_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint32  *val, const char *tag, int indent);
ssize_t Pa_int64_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint64  *val, const char *tag, int indent);

ssize_t Pa_uint8_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint8  *val, const char *tag, int indent);
ssize_t Pa_uint16_write_xml_2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint16 *val, const char *tag, int indent);
ssize_t Pa_uint32_write_xml_2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *val, const char *tag, int indent);
ssize_t Pa_uint64_write_xml_2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint64 *val, const char *tag, int indent);
#endif

#if P_CONFIG_E_INT > 0
ssize_t Pe_int8_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint8   *val);
ssize_t Pe_int16_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint16  *val);
ssize_t Pe_int32_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint32  *val);
ssize_t Pe_int64_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint64  *val);

ssize_t Pe_uint8_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint8  *val);
ssize_t Pe_uint16_write2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint16 *val);
ssize_t Pe_uint32_write2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *val);
ssize_t Pe_uint64_write2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint64 *val);

ssize_t Pe_int8_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint8   *val, const char *tag, int indent);
ssize_t Pe_int16_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint16  *val, const char *tag, int indent);
ssize_t Pe_int32_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint32  *val, const char *tag, int indent);
ssize_t Pe_int64_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint64  *val, const char *tag, int indent);

ssize_t Pe_uint8_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint8  *val, const char *tag, int indent);
ssize_t Pe_uint16_write_xml_2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint16 *val, const char *tag, int indent);
ssize_t Pe_uint32_write_xml_2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *val, const char *tag, int indent);
ssize_t Pe_uint64_write_xml_2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint64 *val, const char *tag, int indent);
#endif

#if P_CONFIG_B_INT > 0
ssize_t Pb_int8_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint8   *val);
ssize_t Pb_int16_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint16  *val);
ssize_t Pb_int32_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint32  *val);
ssize_t Pb_int64_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint64  *val);

ssize_t Pb_uint8_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint8  *val);
ssize_t Pb_uint16_write2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint16 *val);
ssize_t Pb_uint32_write2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *val);
ssize_t Pb_uint64_write2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint64 *val);

ssize_t Pb_int8_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint8   *val, const char *tag, int indent);
ssize_t Pb_int16_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint16  *val, const char *tag, int indent);
ssize_t Pb_int32_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint32  *val, const char *tag, int indent);
ssize_t Pb_int64_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint64  *val, const char *tag, int indent);

ssize_t Pb_uint8_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint8  *val, const char *tag, int indent);
ssize_t Pb_uint16_write_xml_2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint16 *val, const char *tag, int indent);
ssize_t Pb_uint32_write_xml_2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *val, const char *tag, int indent);
ssize_t Pb_uint64_write_xml_2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint64 *val, const char *tag, int indent);
#endif

#if P_CONFIG_A_INT_FW > 0
ssize_t Pa_int8_FW_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint8   *val, size_t width);
ssize_t Pa_int16_FW_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint16  *val, size_t width);
ssize_t Pa_int32_FW_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint32  *val, size_t width);
ssize_t Pa_int64_FW_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint64  *val, size_t width);

ssize_t Pa_uint8_FW_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint8  *val, size_t width);
ssize_t Pa_uint16_FW_write2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint16 *val, size_t width);
ssize_t Pa_uint32_FW_write2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *val, size_t width);
ssize_t Pa_uint64_FW_write2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint64 *val, size_t width);

ssize_t Pa_int8_FW_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint8   *val, const char *tag, int indent, size_t width);
ssize_t Pa_int16_FW_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint16  *val, const char *tag, int indent, size_t width);
ssize_t Pa_int32_FW_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint32  *val, const char *tag, int indent, size_t width);
ssize_t Pa_int64_FW_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint64  *val, const char *tag, int indent, size_t width);

ssize_t Pa_uint8_FW_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint8  *val, const char *tag, int indent, size_t width);
ssize_t Pa_uint16_FW_write_xml_2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint16 *val, const char *tag, int indent, size_t width);
ssize_t Pa_uint32_FW_write_xml_2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *val, const char *tag, int indent, size_t width);
ssize_t Pa_uint64_FW_write_xml_2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint64 *val, const char *tag, int indent, size_t width);
#endif

#if P_CONFIG_E_INT_FW > 0
ssize_t Pe_int8_FW_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint8   *val, size_t width);
ssize_t Pe_int16_FW_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint16  *val, size_t width);
ssize_t Pe_int32_FW_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint32  *val, size_t width);
ssize_t Pe_int64_FW_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint64  *val, size_t width);

ssize_t Pe_uint8_FW_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint8  *val, size_t width);
ssize_t Pe_uint16_FW_write2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint16 *val, size_t width);
ssize_t Pe_uint32_FW_write2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *val, size_t width);
ssize_t Pe_uint64_FW_write2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint64 *val, size_t width);

ssize_t Pe_int8_FW_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint8   *val, const char *tag, int indent, size_t width);
ssize_t Pe_int16_FW_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint16  *val, const char *tag, int indent, size_t width);
ssize_t Pe_int32_FW_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint32  *val, const char *tag, int indent, size_t width);
ssize_t Pe_int64_FW_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint64  *val, const char *tag, int indent, size_t width);

ssize_t Pe_uint8_FW_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint8  *val, const char *tag, int indent, size_t width);
ssize_t Pe_uint16_FW_write_xml_2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint16 *val, const char *tag, int indent, size_t width);
ssize_t Pe_uint32_FW_write_xml_2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *val, const char *tag, int indent, size_t width);
ssize_t Pe_uint64_FW_write_xml_2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint64 *val, const char *tag, int indent, size_t width);
#endif

#if P_CONFIG_EBC_INT > 0 || P_CONFIG_EBC_FPOINT > 0
ssize_t Pebc_int8_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint8   *val, Puint32 num_digits);
ssize_t Pebc_int16_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint16  *val, Puint32 num_digits);
ssize_t Pebc_int32_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint32  *val, Puint32 num_digits);
ssize_t Pebc_int64_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint64  *val, Puint32 num_digits);
						  			                          
ssize_t Pebc_uint8_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint8  *val, Puint32 num_digits);
ssize_t Pebc_uint16_write2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint16 *val, Puint32 num_digits);
ssize_t Pebc_uint32_write2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *val, Puint32 num_digits);
ssize_t Pebc_uint64_write2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint64 *val, Puint32 num_digits);

ssize_t Pebc_int8_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint8   *val, const char *tag, int indent, Puint32 num_digits);
ssize_t Pebc_int16_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint16  *val, const char *tag, int indent, Puint32 num_digits);
ssize_t Pebc_int32_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint32  *val, const char *tag, int indent, Puint32 num_digits);
ssize_t Pebc_int64_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint64  *val, const char *tag, int indent, Puint32 num_digits);
						       								                    
ssize_t Pebc_uint8_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint8  *val, const char *tag, int indent, Puint32 num_digits);
ssize_t Pebc_uint16_write_xml_2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint16 *val, const char *tag, int indent, Puint32 num_digits);
ssize_t Pebc_uint32_write_xml_2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *val, const char *tag, int indent, Puint32 num_digits);
ssize_t Pebc_uint64_write_xml_2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint64 *val, const char *tag, int indent, Puint32 num_digits);
#endif

#if P_CONFIG_BCD_INT > 0 || P_CONFIG_BCD_FPOINT > 0
ssize_t Pbcd_int8_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint8   *val, Puint32 num_digits);
ssize_t Pbcd_int16_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint16  *val, Puint32 num_digits);
ssize_t Pbcd_int32_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint32  *val, Puint32 num_digits);
ssize_t Pbcd_int64_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint64  *val, Puint32 num_digits);
						  			                          
ssize_t Pbcd_uint8_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint8  *val, Puint32 num_digits);
ssize_t Pbcd_uint16_write2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint16 *val, Puint32 num_digits);
ssize_t Pbcd_uint32_write2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *val, Puint32 num_digits);
ssize_t Pbcd_uint64_write2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint64 *val, Puint32 num_digits);

ssize_t Pbcd_int8_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint8   *val, const char *tag, int indent, Puint32 num_digits);
ssize_t Pbcd_int16_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint16  *val, const char *tag, int indent, Puint32 num_digits);
ssize_t Pbcd_int32_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint32  *val, const char *tag, int indent, Puint32 num_digits);
ssize_t Pbcd_int64_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint64  *val, const char *tag, int indent, Puint32 num_digits);
						       								                    
ssize_t Pbcd_uint8_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint8  *val, const char *tag, int indent, Puint32 num_digits);
ssize_t Pbcd_uint16_write_xml_2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint16 *val, const char *tag, int indent, Puint32 num_digits);
ssize_t Pbcd_uint32_write_xml_2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *val, const char *tag, int indent, Puint32 num_digits);
ssize_t Pbcd_uint64_write_xml_2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint64 *val, const char *tag, int indent, Puint32 num_digits);
#endif

#if P_CONFIG_SBL_INT > 0 || P_CONFIG_SBL_FPOINT > 0
ssize_t Psbl_int8_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint8   *val, Puint32 num_bytes);
ssize_t Psbl_int16_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint16  *val, Puint32 num_bytes);
ssize_t Psbl_int32_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint32  *val, Puint32 num_bytes);
ssize_t Psbl_int64_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint64  *val, Puint32 num_bytes);

ssize_t Psbl_uint8_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint8  *val, Puint32 num_bytes);
ssize_t Psbl_uint16_write2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint16 *val, Puint32 num_bytes);
ssize_t Psbl_uint32_write2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *val, Puint32 num_bytes);
ssize_t Psbl_uint64_write2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint64 *val, Puint32 num_bytes);

ssize_t Psbl_int8_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint8   *val, const char *tag, int indent, Puint32 num_bytes);
ssize_t Psbl_int16_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint16  *val, const char *tag, int indent, Puint32 num_bytes);
ssize_t Psbl_int32_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint32  *val, const char *tag, int indent, Puint32 num_bytes);
ssize_t Psbl_int64_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint64  *val, const char *tag, int indent, Puint32 num_bytes);

ssize_t Psbl_uint8_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint8  *val, const char *tag, int indent, Puint32 num_bytes);
ssize_t Psbl_uint16_write_xml_2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint16 *val, const char *tag, int indent, Puint32 num_bytes);
ssize_t Psbl_uint32_write_xml_2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *val, const char *tag, int indent, Puint32 num_bytes);
ssize_t Psbl_uint64_write_xml_2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint64 *val, const char *tag, int indent, Puint32 num_bytes);
#endif

#if P_CONFIG_SBH_INT > 0 || P_CONFIG_SBH_FPOINT > 0
ssize_t Psbh_int8_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint8   *val, Puint32 num_bytes);
ssize_t Psbh_int16_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint16  *val, Puint32 num_bytes);
ssize_t Psbh_int32_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint32  *val, Puint32 num_bytes);
ssize_t Psbh_int64_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint64  *val, Puint32 num_bytes);

ssize_t Psbh_uint8_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint8  *val, Puint32 num_bytes);
ssize_t Psbh_uint16_write2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint16 *val, Puint32 num_bytes);
ssize_t Psbh_uint32_write2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *val, Puint32 num_bytes);
ssize_t Psbh_uint64_write2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint64 *val, Puint32 num_bytes);

ssize_t Psbh_int8_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint8   *val, const char *tag, int indent, Puint32 num_bytes);
ssize_t Psbh_int16_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint16  *val, const char *tag, int indent, Puint32 num_bytes);
ssize_t Psbh_int32_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint32  *val, const char *tag, int indent, Puint32 num_bytes);
ssize_t Psbh_int64_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint64  *val, const char *tag, int indent, Puint32 num_bytes);

ssize_t Psbh_uint8_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint8  *val, const char *tag, int indent, Puint32 num_bytes);
ssize_t Psbh_uint16_write_xml_2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint16 *val, const char *tag, int indent, Puint32 num_bytes);
ssize_t Psbh_uint32_write_xml_2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *val, const char *tag, int indent, Puint32 num_bytes);
ssize_t Psbh_uint64_write_xml_2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint64 *val, const char *tag, int indent, Puint32 num_bytes);
#endif

#if P_CONFIG_EBC_FPOINT > 0
ssize_t Pebc_fpoint8_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pfpoint8   *val, Puint32 num_digits, Puint32 d_exp);
ssize_t Pebc_fpoint16_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pfpoint16  *val, Puint32 num_digits, Puint32 d_exp);
ssize_t Pebc_fpoint32_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pfpoint32  *val, Puint32 num_digits, Puint32 d_exp);
ssize_t Pebc_fpoint64_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pfpoint64  *val, Puint32 num_digits, Puint32 d_exp);
						     				                                       
ssize_t Pebc_ufpoint8_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pufpoint8  *val, Puint32 num_digits, Puint32 d_exp);
ssize_t Pebc_ufpoint16_write2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Pufpoint16 *val, Puint32 num_digits, Puint32 d_exp);
ssize_t Pebc_ufpoint32_write2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Pufpoint32 *val, Puint32 num_digits, Puint32 d_exp);
ssize_t Pebc_ufpoint64_write2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Pufpoint64 *val, Puint32 num_digits, Puint32 d_exp);

ssize_t Pebc_fpoint8_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pfpoint8   *val, const char *tag, int indent, Puint32 num_digits, Puint32 d_exp);
ssize_t Pebc_fpoint16_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pfpoint16  *val, const char *tag, int indent, Puint32 num_digits, Puint32 d_exp);
ssize_t Pebc_fpoint32_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pfpoint32  *val, const char *tag, int indent, Puint32 num_digits, Puint32 d_exp);
ssize_t Pebc_fpoint64_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pfpoint64  *val, const char *tag, int indent, Puint32 num_digits, Puint32 d_exp);

ssize_t Pebc_ufpoint8_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pufpoint8  *val, const char *tag, int indent, Puint32 num_digits, Puint32 d_exp);
ssize_t Pebc_ufpoint16_write_xml_2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Pufpoint16 *val, const char *tag, int indent, Puint32 num_digits, Puint32 d_exp);
ssize_t Pebc_ufpoint32_write_xml_2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Pufpoint32 *val, const char *tag, int indent, Puint32 num_digits, Puint32 d_exp);
ssize_t Pebc_ufpoint64_write_xml_2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Pufpoint64 *val, const char *tag, int indent, Puint32 num_digits, Puint32 d_exp);
#endif

#if P_CONFIG_BCD_FPOINT > 0
ssize_t Pbcd_fpoint8_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pfpoint8   *val, Puint32 num_digits, Puint32 d_exp);
ssize_t Pbcd_fpoint16_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pfpoint16  *val, Puint32 num_digits, Puint32 d_exp);
ssize_t Pbcd_fpoint32_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pfpoint32  *val, Puint32 num_digits, Puint32 d_exp);
ssize_t Pbcd_fpoint64_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pfpoint64  *val, Puint32 num_digits, Puint32 d_exp);

ssize_t Pbcd_ufpoint8_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pufpoint8  *val, Puint32 num_digits, Puint32 d_exp);
ssize_t Pbcd_ufpoint16_write2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Pufpoint16 *val, Puint32 num_digits, Puint32 d_exp);
ssize_t Pbcd_ufpoint32_write2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Pufpoint32 *val, Puint32 num_digits, Puint32 d_exp);
ssize_t Pbcd_ufpoint64_write2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Pufpoint64 *val, Puint32 num_digits, Puint32 d_exp);

ssize_t Pbcd_fpoint8_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pfpoint8   *val, const char *tag, int indent, Puint32 num_digits, Puint32 d_exp);
ssize_t Pbcd_fpoint16_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pfpoint16  *val, const char *tag, int indent, Puint32 num_digits, Puint32 d_exp);
ssize_t Pbcd_fpoint32_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pfpoint32  *val, const char *tag, int indent, Puint32 num_digits, Puint32 d_exp);
ssize_t Pbcd_fpoint64_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pfpoint64  *val, const char *tag, int indent, Puint32 num_digits, Puint32 d_exp);
							  							                                         
ssize_t Pbcd_ufpoint8_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pufpoint8  *val, const char *tag, int indent, Puint32 num_digits, Puint32 d_exp);
ssize_t Pbcd_ufpoint16_write_xml_2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Pufpoint16 *val, const char *tag, int indent, Puint32 num_digits, Puint32 d_exp);
ssize_t Pbcd_ufpoint32_write_xml_2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Pufpoint32 *val, const char *tag, int indent, Puint32 num_digits, Puint32 d_exp);
ssize_t Pbcd_ufpoint64_write_xml_2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Pufpoint64 *val, const char *tag, int indent, Puint32 num_digits, Puint32 d_exp);
#endif

#if P_CONFIG_SBL_FPOINT > 0
ssize_t Psbl_fpoint8_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pfpoint8   *val, Puint32 num_bytes, Puint32 d_exp);
ssize_t Psbl_fpoint16_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pfpoint16  *val, Puint32 num_bytes, Puint32 d_exp);
ssize_t Psbl_fpoint32_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pfpoint32  *val, Puint32 num_bytes, Puint32 d_exp);
ssize_t Psbl_fpoint64_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pfpoint64  *val, Puint32 num_bytes, Puint32 d_exp);
						     				                                      
ssize_t Psbl_ufpoint8_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pufpoint8  *val, Puint32 num_bytes, Puint32 d_exp);
ssize_t Psbl_ufpoint16_write2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Pufpoint16 *val, Puint32 num_bytes, Puint32 d_exp);
ssize_t Psbl_ufpoint32_write2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Pufpoint32 *val, Puint32 num_bytes, Puint32 d_exp);
ssize_t Psbl_ufpoint64_write2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Pufpoint64 *val, Puint32 num_bytes, Puint32 d_exp);

ssize_t Psbl_fpoint8_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pfpoint8   *val, const char *tag, int indent, Puint32 num_bytes, Puint32 d_exp);
ssize_t Psbl_fpoint16_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pfpoint16  *val, const char *tag, int indent, Puint32 num_bytes, Puint32 d_exp);
ssize_t Psbl_fpoint32_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pfpoint32  *val, const char *tag, int indent, Puint32 num_bytes, Puint32 d_exp);
ssize_t Psbl_fpoint64_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pfpoint64  *val, const char *tag, int indent, Puint32 num_bytes, Puint32 d_exp);
							  							                                        
ssize_t Psbl_ufpoint8_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pufpoint8  *val, const char *tag, int indent, Puint32 num_bytes, Puint32 d_exp);
ssize_t Psbl_ufpoint16_write_xml_2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Pufpoint16 *val, const char *tag, int indent, Puint32 num_bytes, Puint32 d_exp);
ssize_t Psbl_ufpoint32_write_xml_2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Pufpoint32 *val, const char *tag, int indent, Puint32 num_bytes, Puint32 d_exp);
ssize_t Psbl_ufpoint64_write_xml_2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Pufpoint64 *val, const char *tag, int indent, Puint32 num_bytes, Puint32 d_exp);
#endif

#if P_CONFIG_SBH_FPOINT > 0
ssize_t Psbh_fpoint8_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pfpoint8   *val, Puint32 num_bytes, Puint32 d_exp);
ssize_t Psbh_fpoint16_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pfpoint16  *val, Puint32 num_bytes, Puint32 d_exp);
ssize_t Psbh_fpoint32_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pfpoint32  *val, Puint32 num_bytes, Puint32 d_exp);
ssize_t Psbh_fpoint64_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pfpoint64  *val, Puint32 num_bytes, Puint32 d_exp);
						     				                                      
ssize_t Psbh_ufpoint8_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pufpoint8  *val, Puint32 num_bytes, Puint32 d_exp);
ssize_t Psbh_ufpoint16_write2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Pufpoint16 *val, Puint32 num_bytes, Puint32 d_exp);
ssize_t Psbh_ufpoint32_write2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Pufpoint32 *val, Puint32 num_bytes, Puint32 d_exp);
ssize_t Psbh_ufpoint64_write2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Pufpoint64 *val, Puint32 num_bytes, Puint32 d_exp);

ssize_t Psbh_fpoint8_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pfpoint8   *val, const char *tag, int indent, Puint32 num_bytes, Puint32 d_exp);
ssize_t Psbh_fpoint16_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pfpoint16  *val, const char *tag, int indent, Puint32 num_bytes, Puint32 d_exp);
ssize_t Psbh_fpoint32_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pfpoint32  *val, const char *tag, int indent, Puint32 num_bytes, Puint32 d_exp);
ssize_t Psbh_fpoint64_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pfpoint64  *val, const char *tag, int indent, Puint32 num_bytes, Puint32 d_exp);
							  							                                        
ssize_t Psbh_ufpoint8_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pufpoint8  *val, const char *tag, int indent, Puint32 num_bytes, Puint32 d_exp);
ssize_t Psbh_ufpoint16_write_xml_2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Pufpoint16 *val, const char *tag, int indent, Puint32 num_bytes, Puint32 d_exp);
ssize_t Psbh_ufpoint32_write_xml_2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Pufpoint32 *val, const char *tag, int indent, Puint32 num_bytes, Puint32 d_exp);
ssize_t Psbh_ufpoint64_write_xml_2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Pufpoint64 *val, const char *tag, int indent, Puint32 num_bytes, Puint32 d_exp);
#endif

/* write2buf functions */

#if P_CONFIG_A_INT > 0
ssize_t Pa_int8_write2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint8   *val);
ssize_t Pa_int16_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint16  *val);
ssize_t Pa_int32_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint32  *val);
ssize_t Pa_int64_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint64  *val);

ssize_t Pa_uint8_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint8  *val);
ssize_t Pa_uint16_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint16 *val);
ssize_t Pa_uint32_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *val);
ssize_t Pa_uint64_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint64 *val);

ssize_t Pa_int8_write_xml_2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint8   *val, const char *tag, int indent);
ssize_t Pa_int16_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint16  *val, const char *tag, int indent);
ssize_t Pa_int32_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint32  *val, const char *tag, int indent);
ssize_t Pa_int64_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint64  *val, const char *tag, int indent);

ssize_t Pa_uint8_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint8  *val, const char *tag, int indent);
ssize_t Pa_uint16_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint16 *val, const char *tag, int indent);
ssize_t Pa_uint32_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *val, const char *tag, int indent);
ssize_t Pa_uint64_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint64 *val, const char *tag, int indent);
#endif

#if P_CONFIG_E_INT > 0
ssize_t Pe_int8_write2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint8   *val);
ssize_t Pe_int16_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint16  *val);
ssize_t Pe_int32_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint32  *val);
ssize_t Pe_int64_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint64  *val);

ssize_t Pe_uint8_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint8  *val);
ssize_t Pe_uint16_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint16 *val);
ssize_t Pe_uint32_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *val);
ssize_t Pe_uint64_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint64 *val);

ssize_t Pe_int8_write_xml_2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint8   *val, const char *tag, int indent);
ssize_t Pe_int16_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint16  *val, const char *tag, int indent);
ssize_t Pe_int32_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint32  *val, const char *tag, int indent);
ssize_t Pe_int64_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint64  *val, const char *tag, int indent);

ssize_t Pe_uint8_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint8  *val, const char *tag, int indent);
ssize_t Pe_uint16_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint16 *val, const char *tag, int indent);
ssize_t Pe_uint32_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *val, const char *tag, int indent);
ssize_t Pe_uint64_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint64 *val, const char *tag, int indent);
#endif

#if P_CONFIG_B_INT > 0
ssize_t Pb_int8_write2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint8   *val);
ssize_t Pb_int16_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint16  *val);
ssize_t Pb_int32_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint32  *val);
ssize_t Pb_int64_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint64  *val);

ssize_t Pb_uint8_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint8  *val);
ssize_t Pb_uint16_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint16 *val);
ssize_t Pb_uint32_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *val);
ssize_t Pb_uint64_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint64 *val);

ssize_t Pb_int8_write_xml_2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint8   *val, const char *tag, int indent);
ssize_t Pb_int16_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint16  *val, const char *tag, int indent);
ssize_t Pb_int32_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint32  *val, const char *tag, int indent);
ssize_t Pb_int64_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint64  *val, const char *tag, int indent);

ssize_t Pb_uint8_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint8  *val, const char *tag, int indent);
ssize_t Pb_uint16_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint16 *val, const char *tag, int indent);
ssize_t Pb_uint32_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *val, const char *tag, int indent);
ssize_t Pb_uint64_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint64 *val, const char *tag, int indent);
#endif

#if P_CONFIG_A_INT_FW > 0
ssize_t Pa_int8_FW_write2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint8   *val, size_t width);
ssize_t Pa_int16_FW_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint16  *val, size_t width);
ssize_t Pa_int32_FW_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint32  *val, size_t width);
ssize_t Pa_int64_FW_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint64  *val, size_t width);

ssize_t Pa_uint8_FW_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint8  *val, size_t width);
ssize_t Pa_uint16_FW_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint16 *val, size_t width);
ssize_t Pa_uint32_FW_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *val, size_t width);
ssize_t Pa_uint64_FW_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint64 *val, size_t width);

ssize_t Pa_int8_FW_write_xml_2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint8   *val, const char *tag, int indent, size_t width);
ssize_t Pa_int16_FW_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint16  *val, const char *tag, int indent, size_t width);
ssize_t Pa_int32_FW_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint32  *val, const char *tag, int indent, size_t width);
ssize_t Pa_int64_FW_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint64  *val, const char *tag, int indent, size_t width);

ssize_t Pa_uint8_FW_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint8  *val, const char *tag, int indent, size_t width);
ssize_t Pa_uint16_FW_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint16 *val, const char *tag, int indent, size_t width);
ssize_t Pa_uint32_FW_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *val, const char *tag, int indent, size_t width);
ssize_t Pa_uint64_FW_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint64 *val, const char *tag, int indent, size_t width);
#endif

#if P_CONFIG_E_INT_FW > 0
ssize_t Pe_int8_FW_write2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint8   *val, size_t width);
ssize_t Pe_int16_FW_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint16  *val, size_t width);
ssize_t Pe_int32_FW_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint32  *val, size_t width);
ssize_t Pe_int64_FW_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint64  *val, size_t width);

ssize_t Pe_uint8_FW_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint8  *val, size_t width);
ssize_t Pe_uint16_FW_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint16 *val, size_t width);
ssize_t Pe_uint32_FW_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *val, size_t width);
ssize_t Pe_uint64_FW_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint64 *val, size_t width);

ssize_t Pe_int8_FW_write_xml_2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint8   *val, const char *tag, int indent, size_t width);
ssize_t Pe_int16_FW_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint16  *val, const char *tag, int indent, size_t width);
ssize_t Pe_int32_FW_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint32  *val, const char *tag, int indent, size_t width);
ssize_t Pe_int64_FW_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint64  *val, const char *tag, int indent, size_t width);

ssize_t Pe_uint8_FW_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint8  *val, const char *tag, int indent, size_t width);
ssize_t Pe_uint16_FW_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint16 *val, const char *tag, int indent, size_t width);
ssize_t Pe_uint32_FW_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *val, const char *tag, int indent, size_t width);
ssize_t Pe_uint64_FW_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint64 *val, const char *tag, int indent, size_t width);
#endif

#if P_CONFIG_EBC_INT > 0 || P_CONFIG_EBC_FPOINT > 0
ssize_t Pebc_int8_write2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint8   *val, Puint32 num_digits);
ssize_t Pebc_int16_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint16  *val, Puint32 num_digits);
ssize_t Pebc_int32_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint32  *val, Puint32 num_digits);
ssize_t Pebc_int64_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint64  *val, Puint32 num_digits);

ssize_t Pebc_uint8_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint8  *val, Puint32 num_digits);
ssize_t Pebc_uint16_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint16 *val, Puint32 num_digits);
ssize_t Pebc_uint32_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *val, Puint32 num_digits);
ssize_t Pebc_uint64_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint64 *val, Puint32 num_digits);

ssize_t Pebc_int8_write_xml_2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint8   *val, const char *tag, int indent, Puint32 num_digits);
ssize_t Pebc_int16_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint16  *val, const char *tag, int indent, Puint32 num_digits);
ssize_t Pebc_int32_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint32  *val, const char *tag, int indent, Puint32 num_digits);
ssize_t Pebc_int64_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint64  *val, const char *tag, int indent, Puint32 num_digits);
										          									  
ssize_t Pebc_uint8_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint8  *val, const char *tag, int indent, Puint32 num_digits);
ssize_t Pebc_uint16_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint16 *val, const char *tag, int indent, Puint32 num_digits);
ssize_t Pebc_uint32_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *val, const char *tag, int indent, Puint32 num_digits);
ssize_t Pebc_uint64_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint64 *val, const char *tag, int indent, Puint32 num_digits);
#endif

#if P_CONFIG_BCD_INT > 0 || P_CONFIG_BCD_FPOINT > 0
ssize_t Pbcd_int8_write2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint8   *val, Puint32 num_digits);
ssize_t Pbcd_int16_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint16  *val, Puint32 num_digits);
ssize_t Pbcd_int32_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint32  *val, Puint32 num_digits);
ssize_t Pbcd_int64_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint64  *val, Puint32 num_digits);

ssize_t Pbcd_uint8_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint8  *val, Puint32 num_digits);
ssize_t Pbcd_uint16_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint16 *val, Puint32 num_digits);
ssize_t Pbcd_uint32_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *val, Puint32 num_digits);
ssize_t Pbcd_uint64_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint64 *val, Puint32 num_digits);

ssize_t Pbcd_int8_write_xml_2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint8   *val, const char *tag, int indent, Puint32 num_digits);
ssize_t Pbcd_int16_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint16  *val, const char *tag, int indent, Puint32 num_digits);
ssize_t Pbcd_int32_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint32  *val, const char *tag, int indent, Puint32 num_digits);
ssize_t Pbcd_int64_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint64  *val, const char *tag, int indent, Puint32 num_digits);

ssize_t Pbcd_uint8_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint8  *val, const char *tag, int indent, Puint32 num_digits);
ssize_t Pbcd_uint16_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint16 *val, const char *tag, int indent, Puint32 num_digits);
ssize_t Pbcd_uint32_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *val, const char *tag, int indent, Puint32 num_digits);
ssize_t Pbcd_uint64_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint64 *val, const char *tag, int indent, Puint32 num_digits);
#endif

#if P_CONFIG_SBL_INT > 0 || P_CONFIG_SBL_FPOINT > 0
ssize_t Psbl_int8_write2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint8   *val, Puint32 num_bytes);
ssize_t Psbl_int16_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint16  *val, Puint32 num_bytes);
ssize_t Psbl_int32_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint32  *val, Puint32 num_bytes);
ssize_t Psbl_int64_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint64  *val, Puint32 num_bytes);

ssize_t Psbl_uint8_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint8  *val, Puint32 num_bytes);
ssize_t Psbl_uint16_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint16 *val, Puint32 num_bytes);
ssize_t Psbl_uint32_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *val, Puint32 num_bytes);
ssize_t Psbl_uint64_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint64 *val, Puint32 num_bytes);

ssize_t Psbl_int8_write_xml_2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint8   *val, const char *tag, int indent, Puint32 num_bytes);
ssize_t Psbl_int16_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint16  *val, const char *tag, int indent, Puint32 num_bytes);
ssize_t Psbl_int32_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint32  *val, const char *tag, int indent, Puint32 num_bytes);
ssize_t Psbl_int64_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint64  *val, const char *tag, int indent, Puint32 num_bytes);

ssize_t Psbl_uint8_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint8  *val, const char *tag, int indent, Puint32 num_bytes);
ssize_t Psbl_uint16_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint16 *val, const char *tag, int indent, Puint32 num_bytes);
ssize_t Psbl_uint32_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *val, const char *tag, int indent, Puint32 num_bytes);
ssize_t Psbl_uint64_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint64 *val, const char *tag, int indent, Puint32 num_bytes);
#endif

#if P_CONFIG_SBH_INT > 0 || P_CONFIG_SBH_FPOINT > 0
ssize_t Psbh_int8_write2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint8   *val, Puint32 num_bytes);
ssize_t Psbh_int16_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint16  *val, Puint32 num_bytes);
ssize_t Psbh_int32_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint32  *val, Puint32 num_bytes);
ssize_t Psbh_int64_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint64  *val, Puint32 num_bytes);

ssize_t Psbh_uint8_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint8  *val, Puint32 num_bytes);
ssize_t Psbh_uint16_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint16 *val, Puint32 num_bytes);
ssize_t Psbh_uint32_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *val, Puint32 num_bytes);
ssize_t Psbh_uint64_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint64 *val, Puint32 num_bytes);

ssize_t Psbh_int8_write_xml_2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint8   *val, const char *tag, int indent, Puint32 num_bytes);
ssize_t Psbh_int16_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint16  *val, const char *tag, int indent, Puint32 num_bytes);
ssize_t Psbh_int32_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint32  *val, const char *tag, int indent, Puint32 num_bytes);
ssize_t Psbh_int64_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint64  *val, const char *tag, int indent, Puint32 num_bytes);

ssize_t Psbh_uint8_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint8  *val, const char *tag, int indent, Puint32 num_bytes);
ssize_t Psbh_uint16_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint16 *val, const char *tag, int indent, Puint32 num_bytes);
ssize_t Psbh_uint32_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *val, const char *tag, int indent, Puint32 num_bytes);
ssize_t Psbh_uint64_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint64 *val, const char *tag, int indent, Puint32 num_bytes);
#endif

#if P_CONFIG_EBC_FPOINT > 0
ssize_t Pebc_fpoint8_write2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfpoint8   *val, Puint32 num_digits, Puint32 d_exp);
ssize_t Pebc_fpoint16_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfpoint16  *val, Puint32 num_digits, Puint32 d_exp);
ssize_t Pebc_fpoint32_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfpoint32  *val, Puint32 num_digits, Puint32 d_exp);
ssize_t Pebc_fpoint64_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfpoint64  *val, Puint32 num_digits, Puint32 d_exp);
										     				                                       
ssize_t Pebc_ufpoint8_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pufpoint8  *val, Puint32 num_digits, Puint32 d_exp);
ssize_t Pebc_ufpoint16_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pufpoint16 *val, Puint32 num_digits, Puint32 d_exp);
ssize_t Pebc_ufpoint32_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pufpoint32 *val, Puint32 num_digits, Puint32 d_exp);
ssize_t Pebc_ufpoint64_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pufpoint64 *val, Puint32 num_digits, Puint32 d_exp);

ssize_t Pebc_fpoint8_write_xml_2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfpoint8   *val, const char *tag, int indent, Puint32 num_digits, Puint32 d_exp);
ssize_t Pebc_fpoint16_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfpoint16  *val, const char *tag, int indent, Puint32 num_digits, Puint32 d_exp);
ssize_t Pebc_fpoint32_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfpoint32  *val, const char *tag, int indent, Puint32 num_digits, Puint32 d_exp);
ssize_t Pebc_fpoint64_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfpoint64  *val, const char *tag, int indent, Puint32 num_digits, Puint32 d_exp);
											  							                                         
ssize_t Pebc_ufpoint8_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pufpoint8  *val, const char *tag, int indent, Puint32 num_digits, Puint32 d_exp);
ssize_t Pebc_ufpoint16_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pufpoint16 *val, const char *tag, int indent, Puint32 num_digits, Puint32 d_exp);
ssize_t Pebc_ufpoint32_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pufpoint32 *val, const char *tag, int indent, Puint32 num_digits, Puint32 d_exp);
ssize_t Pebc_ufpoint64_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pufpoint64 *val, const char *tag, int indent, Puint32 num_digits, Puint32 d_exp);
#endif

#if P_CONFIG_BCD_FPOINT > 0
ssize_t Pbcd_fpoint8_write2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfpoint8   *val, Puint32 num_digits, Puint32 d_exp);
ssize_t Pbcd_fpoint16_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfpoint16  *val, Puint32 num_digits, Puint32 d_exp);
ssize_t Pbcd_fpoint32_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfpoint32  *val, Puint32 num_digits, Puint32 d_exp);
ssize_t Pbcd_fpoint64_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfpoint64  *val, Puint32 num_digits, Puint32 d_exp);
										     				                                       
ssize_t Pbcd_ufpoint8_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pufpoint8  *val, Puint32 num_digits, Puint32 d_exp);
ssize_t Pbcd_ufpoint16_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pufpoint16 *val, Puint32 num_digits, Puint32 d_exp);
ssize_t Pbcd_ufpoint32_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pufpoint32 *val, Puint32 num_digits, Puint32 d_exp);
ssize_t Pbcd_ufpoint64_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pufpoint64 *val, Puint32 num_digits, Puint32 d_exp);

ssize_t Pbcd_fpoint8_write_xml_2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfpoint8   *val, const char *tag, int indent, Puint32 num_digits, Puint32 d_exp);
ssize_t Pbcd_fpoint16_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfpoint16  *val, const char *tag, int indent, Puint32 num_digits, Puint32 d_exp);
ssize_t Pbcd_fpoint32_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfpoint32  *val, const char *tag, int indent, Puint32 num_digits, Puint32 d_exp);
ssize_t Pbcd_fpoint64_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfpoint64  *val, const char *tag, int indent, Puint32 num_digits, Puint32 d_exp);
											  							                                         
ssize_t Pbcd_ufpoint8_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pufpoint8  *val, const char *tag, int indent, Puint32 num_digits, Puint32 d_exp);
ssize_t Pbcd_ufpoint16_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pufpoint16 *val, const char *tag, int indent, Puint32 num_digits, Puint32 d_exp);
ssize_t Pbcd_ufpoint32_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pufpoint32 *val, const char *tag, int indent, Puint32 num_digits, Puint32 d_exp);
ssize_t Pbcd_ufpoint64_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pufpoint64 *val, const char *tag, int indent, Puint32 num_digits, Puint32 d_exp);
#endif

#if P_CONFIG_SBL_FPOINT > 0
ssize_t Psbl_fpoint8_write2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfpoint8   *val, Puint32 num_bytes, Puint32 d_exp);
ssize_t Psbl_fpoint16_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfpoint16  *val, Puint32 num_bytes, Puint32 d_exp);
ssize_t Psbl_fpoint32_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfpoint32  *val, Puint32 num_bytes, Puint32 d_exp);
ssize_t Psbl_fpoint64_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfpoint64  *val, Puint32 num_bytes, Puint32 d_exp);
										     						                      
ssize_t Psbl_ufpoint8_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pufpoint8  *val, Puint32 num_bytes, Puint32 d_exp);
ssize_t Psbl_ufpoint16_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pufpoint16 *val, Puint32 num_bytes, Puint32 d_exp);
ssize_t Psbl_ufpoint32_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pufpoint32 *val, Puint32 num_bytes, Puint32 d_exp);
ssize_t Psbl_ufpoint64_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pufpoint64 *val, Puint32 num_bytes, Puint32 d_exp);

ssize_t Psbl_fpoint8_write_xml_2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfpoint8   *val, const char *tag, int indent, Puint32 num_bytes, Puint32 d_exp);
ssize_t Psbl_fpoint16_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfpoint16  *val, const char *tag, int indent, Puint32 num_bytes, Puint32 d_exp);
ssize_t Psbl_fpoint32_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfpoint32  *val, const char *tag, int indent, Puint32 num_bytes, Puint32 d_exp);
ssize_t Psbl_fpoint64_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfpoint64  *val, const char *tag, int indent, Puint32 num_bytes, Puint32 d_exp);
											  										                
ssize_t Psbl_ufpoint8_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pufpoint8  *val, const char *tag, int indent, Puint32 num_bytes, Puint32 d_exp);
ssize_t Psbl_ufpoint16_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pufpoint16 *val, const char *tag, int indent, Puint32 num_bytes, Puint32 d_exp);
ssize_t Psbl_ufpoint32_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pufpoint32 *val, const char *tag, int indent, Puint32 num_bytes, Puint32 d_exp);
ssize_t Psbl_ufpoint64_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pufpoint64 *val, const char *tag, int indent, Puint32 num_bytes, Puint32 d_exp);
#endif

#if P_CONFIG_SBH_FPOINT > 0
ssize_t Psbh_fpoint8_write2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfpoint8   *val, Puint32 num_bytes, Puint32 d_exp);
ssize_t Psbh_fpoint16_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfpoint16  *val, Puint32 num_bytes, Puint32 d_exp);
ssize_t Psbh_fpoint32_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfpoint32  *val, Puint32 num_bytes, Puint32 d_exp);
ssize_t Psbh_fpoint64_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfpoint64  *val, Puint32 num_bytes, Puint32 d_exp);
										     						                      
ssize_t Psbh_ufpoint8_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pufpoint8  *val, Puint32 num_bytes, Puint32 d_exp);
ssize_t Psbh_ufpoint16_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pufpoint16 *val, Puint32 num_bytes, Puint32 d_exp);
ssize_t Psbh_ufpoint32_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pufpoint32 *val, Puint32 num_bytes, Puint32 d_exp);
ssize_t Psbh_ufpoint64_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pufpoint64 *val, Puint32 num_bytes, Puint32 d_exp);

ssize_t Psbh_fpoint8_write_xml_2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfpoint8   *val, const char *tag, int indent, Puint32 num_bytes, Puint32 d_exp);
ssize_t Psbh_fpoint16_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfpoint16  *val, const char *tag, int indent, Puint32 num_bytes, Puint32 d_exp);
ssize_t Psbh_fpoint32_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfpoint32  *val, const char *tag, int indent, Puint32 num_bytes, Puint32 d_exp);
ssize_t Psbh_fpoint64_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfpoint64  *val, const char *tag, int indent, Puint32 num_bytes, Puint32 d_exp);
											  										                
ssize_t Psbh_ufpoint8_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pufpoint8  *val, const char *tag, int indent, Puint32 num_bytes, Puint32 d_exp);
ssize_t Psbh_ufpoint16_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pufpoint16 *val, const char *tag, int indent, Puint32 num_bytes, Puint32 d_exp);
ssize_t Psbh_ufpoint32_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pufpoint32 *val, const char *tag, int indent, Puint32 num_bytes, Puint32 d_exp);
ssize_t Psbh_ufpoint64_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pufpoint64 *val, const char *tag, int indent, Puint32 num_bytes, Puint32 d_exp);
#endif

#if P_CONFIG_A_FLOAT > 0
ssize_t Pa_float32_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfloat32  *val);
ssize_t Pa_float64_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfloat64  *val);

ssize_t Pa_float32_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfloat32  *val, const char *tag, int indent);
ssize_t Pa_float64_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfloat64  *val, const char *tag, int indent);
#endif

#if P_CONFIG_E_FLOAT > 0
ssize_t Pe_float32_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfloat32  *val);
ssize_t Pe_float64_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfloat64  *val);

ssize_t Pe_float32_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfloat32  *val, const char *tag, int indent);
ssize_t Pe_float64_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfloat64  *val, const char *tag, int indent);
#endif

/* fmt2buf/fmt2io functions */

#if P_CONFIG_A_CHAR_STRING > 0
ssize_t Pa_char_fmt2buf      (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Pchar *rep);
ssize_t Pa_char_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Pchar *rep);
ssize_t Pa_char_fmt2io       (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Pchar *rep);

ssize_t Pa_string_FW_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Pstring *rep, size_t width);
ssize_t Pa_string_FW_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				    Pbase_m *m, Pbase_pd *pd, Pstring *rep, size_t width);
ssize_t Pa_string_fmt2buf    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Pstring *rep, Pchar stopChar);
ssize_t Pa_string_fmt2buf_final    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				    Pbase_m *m, Pbase_pd *pd, Pstring *rep, Pchar stopChar);
ssize_t Pa_string_ME_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Pstring *rep, const char *matchRegexp);
ssize_t Pa_string_ME_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				    Pbase_m *m, Pbase_pd *pd, Pstring *rep, const char *matchRegexp);
ssize_t Pa_string_CME_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Pstring *rep, Pregexp_t *matchRegexp);
ssize_t Pa_string_CME_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				    Pbase_m *m, Pbase_pd *pd, Pstring *rep, Pregexp_t *matchRegexp);
ssize_t Pa_string_SE_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Pstring *rep, const char *stopRegexp);
ssize_t Pa_string_SE_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				    Pbase_m *m, Pbase_pd *pd, Pstring *rep, const char *stopRegexp);
ssize_t Pa_string_CSE_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Pstring *rep, Pregexp_t *stopRegexp);
ssize_t Pa_string_CSE_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				    Pbase_m *m, Pbase_pd *pd, Pstring *rep, Pregexp_t *stopRegexp);

ssize_t Pa_string_FW_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Pstring *rep, size_t width);
ssize_t Pa_string_fmt2io     (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Pstring *rep, Pchar stopChar);
ssize_t Pa_string_ME_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Pstring *rep, const char *matchRegexp);
ssize_t Pa_string_CME_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Pstring *rep, Pregexp_t *matchRegexp);
ssize_t Pa_string_SE_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Pstring *rep, const char *stopRegexp);
ssize_t Pa_string_CSE_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Pstring *rep, Pregexp_t *stopRegexp);
#endif

#if P_CONFIG_E_CHAR_STRING > 0
ssize_t Pe_char_fmt2buf      (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Pchar *rep);
ssize_t Pe_char_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Pchar *rep);
ssize_t Pe_char_fmt2io       (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Pchar *rep);

ssize_t Pe_string_FW_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Pstring *rep, size_t width);
ssize_t Pe_string_FW_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				    Pbase_m *m, Pbase_pd *pd, Pstring *rep, size_t width);
ssize_t Pe_string_fmt2buf    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Pstring *rep, Pchar stopChar);
ssize_t Pe_string_fmt2buf_final    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				    Pbase_m *m, Pbase_pd *pd, Pstring *rep, Pchar stopChar);
ssize_t Pe_string_ME_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Pstring *rep, const char *matchRegexp);
ssize_t Pe_string_ME_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				    Pbase_m *m, Pbase_pd *pd, Pstring *rep, const char *matchRegexp);
ssize_t Pe_string_CME_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Pstring *rep, Pregexp_t *matchRegexp);
ssize_t Pe_string_CME_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				    Pbase_m *m, Pbase_pd *pd, Pstring *rep, Pregexp_t *matchRegexp);
ssize_t Pe_string_SE_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Pstring *rep, const char *stopRegexp);
ssize_t Pe_string_SE_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				    Pbase_m *m, Pbase_pd *pd, Pstring *rep, const char *stopRegexp);
ssize_t Pe_string_CSE_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Pstring *rep, Pregexp_t *stopRegexp);
ssize_t Pe_string_CSE_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				    Pbase_m *m, Pbase_pd *pd, Pstring *rep, Pregexp_t *stopRegexp);

ssize_t Pe_string_FW_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Pstring *rep, size_t width);
ssize_t Pe_string_fmt2io     (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Pstring *rep, Pchar stopChar);
ssize_t Pe_string_ME_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Pstring *rep, const char *matchRegexp);
ssize_t Pe_string_CME_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Pstring *rep, Pregexp_t *matchRegexp);
ssize_t Pe_string_SE_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Pstring *rep, const char *stopRegexp);
ssize_t Pe_string_CSE_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Pstring *rep, Pregexp_t *stopRegexp);
#endif

#if P_CONFIG_A_CHAR_STRING > 0 || P_CONFIG_E_CHAR_STRING > 0
ssize_t Pchar_fmt2buf      (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Pchar *rep);
ssize_t Pchar_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Pchar *rep);
ssize_t Pchar_fmt2io       (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Pchar *rep);

ssize_t Pstring_FW_fmt2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Pstring *rep, size_t width);
ssize_t Pstring_FW_fmt2buf_final   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				    Pbase_m *m, Pbase_pd *pd, Pstring *rep, size_t width);
ssize_t Pstring_fmt2buf      (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Pstring *rep, Pchar stopChar);
ssize_t Pstring_fmt2buf_final      (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				    Pbase_m *m, Pbase_pd *pd, Pstring *rep, Pchar stopChar);
ssize_t Pstring_ME_fmt2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Pstring *rep, const char *matchRegexp);
ssize_t Pstring_ME_fmt2buf_final   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				    Pbase_m *m, Pbase_pd *pd, Pstring *rep, const char *matchRegexp);
ssize_t Pstring_CME_fmt2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Pstring *rep, Pregexp_t *matchRegexp);
ssize_t Pstring_CME_fmt2buf_final  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				    Pbase_m *m, Pbase_pd *pd, Pstring *rep, Pregexp_t *matchRegexp);
ssize_t Pstring_SE_fmt2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Pstring *rep, const char *stopRegexp);
ssize_t Pstring_SE_fmt2buf_final   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				    Pbase_m *m, Pbase_pd *pd, Pstring *rep, const char *stopRegexp);
ssize_t Pstring_CSE_fmt2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Pstring *rep, Pregexp_t *stopRegexp);
ssize_t Pstring_CSE_fmt2buf_final  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				    Pbase_m *m, Pbase_pd *pd, Pstring *rep, Pregexp_t *stopRegexp);

ssize_t Pstring_FW_fmt2io    (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Pstring *rep, size_t width);
ssize_t Pstring_fmt2io       (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Pstring *rep, Pchar stopChar);
ssize_t Pstring_ME_fmt2io    (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Pstring *rep, const char *matchRegexp);
ssize_t Pstring_CME_fmt2io   (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Pstring *rep, Pregexp_t *matchRegexp);
ssize_t Pstring_SE_fmt2io    (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Pstring *rep, const char *stopRegexp);
ssize_t Pstring_CSE_fmt2io   (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Pstring *rep, Pregexp_t *stopRegexp);
#endif

#if P_CONFIG_A_CHAR_STRING > 0
ssize_t Pa_date_FW_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width);
ssize_t Pa_date_FW_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width);
ssize_t Pa_date_fmt2buf    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar);
ssize_t Pa_date_fmt2buf_final    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar);
ssize_t Pa_date_ME_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *matchRegexp);
ssize_t Pa_date_ME_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *matchRegexp);
ssize_t Pa_date_CME_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *matchRegexp);
ssize_t Pa_date_CME_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *matchRegexp);
ssize_t Pa_date_SE_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *stopRegexp);
ssize_t Pa_date_SE_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *stopRegexp);
ssize_t Pa_date_CSE_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *stopRegexp);
ssize_t Pa_date_CSE_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *stopRegexp);

ssize_t Pa_date_FW_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width);
ssize_t Pa_date_fmt2io     (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar);
ssize_t Pa_date_ME_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *matchRegexp);
ssize_t Pa_date_CME_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *matchRegexp);
ssize_t Pa_date_SE_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *stopRegexp);
ssize_t Pa_date_CSE_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *stopRegexp);
#endif

#if P_CONFIG_E_CHAR_STRING > 0
ssize_t Pe_date_FW_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width);
ssize_t Pe_date_FW_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width);
ssize_t Pe_date_fmt2buf    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar);
ssize_t Pe_date_fmt2buf_final    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar);
ssize_t Pe_date_ME_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *matchRegexp);
ssize_t Pe_date_ME_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *matchRegexp);
ssize_t Pe_date_CME_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *matchRegexp);
ssize_t Pe_date_CME_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *matchRegexp);
ssize_t Pe_date_SE_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *stopRegexp);
ssize_t Pe_date_SE_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *stopRegexp);
ssize_t Pe_date_CSE_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *stopRegexp);
ssize_t Pe_date_CSE_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *stopRegexp);

ssize_t Pe_date_FW_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width);
ssize_t Pe_date_fmt2io     (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar);
ssize_t Pe_date_ME_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *matchRegexp);
ssize_t Pe_date_CME_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *matchRegexp);
ssize_t Pe_date_SE_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *stopRegexp);
ssize_t Pe_date_CSE_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *stopRegexp);
#endif

#if P_CONFIG_A_CHAR_STRING > 0 || P_CONFIG_E_CHAR_STRING > 0
ssize_t Pdate_FW_fmt2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width);
ssize_t Pdate_FW_fmt2buf_final   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width);
ssize_t Pdate_fmt2buf      (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar);
ssize_t Pdate_fmt2buf_final      (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar);
ssize_t Pdate_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar);
ssize_t Pdate_fmt2buf_final_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar);
ssize_t Pdate_ME_fmt2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *matchRegexp);
ssize_t Pdate_ME_fmt2buf_final   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *matchRegexp);
ssize_t Pdate_CME_fmt2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *matchRegexp);
ssize_t Pdate_CME_fmt2buf_final  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *matchRegexp);
ssize_t Pdate_SE_fmt2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *stopRegexp);
ssize_t Pdate_SE_fmt2buf_final   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *stopRegexp);
ssize_t Pdate_CSE_fmt2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *stopRegexp);
ssize_t Pdate_CSE_fmt2buf_final  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *stopRegexp);

ssize_t Pdate_FW_fmt2io    (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width);
ssize_t Pdate_fmt2io       (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar);
ssize_t Pdate_ME_fmt2io    (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *matchRegexp);
ssize_t Pdate_CME_fmt2io   (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *matchRegexp);
ssize_t Pdate_SE_fmt2io    (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *stopRegexp);
ssize_t Pdate_CSE_fmt2io   (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *stopRegexp);
#endif

#if P_CONFIG_A_INT > 0
ssize_t Pa_int8_fmt2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			  Pbase_m *m, Pbase_pd *pd, Pint8  *rep);
ssize_t Pa_int8_fmt2buf_final  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				Pbase_m *m, Pbase_pd *pd, Pint8  *rep);
ssize_t Pa_int16_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			  Pbase_m *m, Pbase_pd *pd, Pint16 *rep);
ssize_t Pa_int16_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				Pbase_m *m, Pbase_pd *pd, Pint16 *rep);
ssize_t Pa_int32_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			  Pbase_m *m, Pbase_pd *pd, Pint32 *rep);
ssize_t Pa_int32_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				Pbase_m *m, Pbase_pd *pd, Pint32 *rep);
ssize_t Pa_int64_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			  Pbase_m *m, Pbase_pd *pd, Pint64 *rep);
ssize_t Pa_int64_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				Pbase_m *m, Pbase_pd *pd, Pint64 *rep);

ssize_t Pa_uint8_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			  Pbase_m *m, Pbase_pd *pd, Puint8  *rep);
ssize_t Pa_uint8_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				Pbase_m *m, Pbase_pd *pd, Puint8  *rep);
ssize_t Pa_uint16_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			  Pbase_m *m, Pbase_pd *pd, Puint16 *rep);
ssize_t Pa_uint16_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				Pbase_m *m, Pbase_pd *pd, Puint16 *rep);
ssize_t Pa_uint32_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			  Pbase_m *m, Pbase_pd *pd, Puint32 *rep);
ssize_t Pa_uint32_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				Pbase_m *m, Pbase_pd *pd, Puint32 *rep);
ssize_t Pa_uint64_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			  Pbase_m *m, Pbase_pd *pd, Puint64 *rep);
ssize_t Pa_uint64_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				Pbase_m *m, Pbase_pd *pd, Puint64 *rep);

ssize_t Pa_int8_fmt2io   (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			  Pbase_m *m, Pbase_pd *pd, Pint8  *rep);
ssize_t Pa_int16_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			  Pbase_m *m, Pbase_pd *pd, Pint16 *rep);
ssize_t Pa_int32_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			  Pbase_m *m, Pbase_pd *pd, Pint32 *rep);
ssize_t Pa_int64_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			  Pbase_m *m, Pbase_pd *pd, Pint64 *rep);

ssize_t Pa_uint8_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			  Pbase_m *m, Pbase_pd *pd, Puint8  *rep);
ssize_t Pa_uint16_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			  Pbase_m *m, Pbase_pd *pd, Puint16 *rep);
ssize_t Pa_uint32_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			  Pbase_m *m, Pbase_pd *pd, Puint32 *rep);
ssize_t Pa_uint64_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			  Pbase_m *m, Pbase_pd *pd, Puint64 *rep);
#endif

#if P_CONFIG_A_INT_FW > 0
ssize_t Pa_int8_FW_fmt2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Pint8  *rep, size_t width);
ssize_t Pa_int8_FW_fmt2buf_final  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				   Pbase_m *m, Pbase_pd *pd, Pint8  *rep, size_t width);
ssize_t Pa_int16_FW_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Pint16 *rep, size_t width);
ssize_t Pa_int16_FW_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				   Pbase_m *m, Pbase_pd *pd, Pint16 *rep, size_t width);
ssize_t Pa_int32_FW_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Pint32 *rep, size_t width);
ssize_t Pa_int32_FW_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				   Pbase_m *m, Pbase_pd *pd, Pint32 *rep, size_t width);
ssize_t Pa_int64_FW_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Pint64 *rep, size_t width);
ssize_t Pa_int64_FW_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				   Pbase_m *m, Pbase_pd *pd, Pint64 *rep, size_t width);

ssize_t Pa_uint8_FW_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Puint8  *rep, size_t width);
ssize_t Pa_uint8_FW_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				   Pbase_m *m, Pbase_pd *pd, Puint8  *rep, size_t width);
ssize_t Pa_uint16_FW_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Puint16 *rep, size_t width);
ssize_t Pa_uint16_FW_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				   Pbase_m *m, Pbase_pd *pd, Puint16 *rep, size_t width);
ssize_t Pa_uint32_FW_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width);
ssize_t Pa_uint32_FW_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				   Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width);
ssize_t Pa_uint64_FW_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Puint64 *rep, size_t width);
ssize_t Pa_uint64_FW_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				   Pbase_m *m, Pbase_pd *pd, Puint64 *rep, size_t width);

ssize_t Pa_int8_FW_fmt2io   (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Pint8  *rep, size_t width);
ssize_t Pa_int16_FW_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Pint16 *rep, size_t width);
ssize_t Pa_int32_FW_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Pint32 *rep, size_t width);
ssize_t Pa_int64_FW_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Pint64 *rep, size_t width);

ssize_t Pa_uint8_FW_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Puint8  *rep, size_t width);
ssize_t Pa_uint16_FW_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Puint16 *rep, size_t width);
ssize_t Pa_uint32_FW_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width);
ssize_t Pa_uint64_FW_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Puint64 *rep, size_t width);
#endif

#if P_CONFIG_E_INT > 0
ssize_t Pe_int8_fmt2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			  Pbase_m *m, Pbase_pd *pd, Pint8  *rep);
ssize_t Pe_int8_fmt2buf_final  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				Pbase_m *m, Pbase_pd *pd, Pint8  *rep);
ssize_t Pe_int16_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			  Pbase_m *m, Pbase_pd *pd, Pint16 *rep);
ssize_t Pe_int16_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				Pbase_m *m, Pbase_pd *pd, Pint16 *rep);
ssize_t Pe_int32_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			  Pbase_m *m, Pbase_pd *pd, Pint32 *rep);
ssize_t Pe_int32_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				Pbase_m *m, Pbase_pd *pd, Pint32 *rep);
ssize_t Pe_int64_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			  Pbase_m *m, Pbase_pd *pd, Pint64 *rep);
ssize_t Pe_int64_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				Pbase_m *m, Pbase_pd *pd, Pint64 *rep);

ssize_t Pe_uint8_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			  Pbase_m *m, Pbase_pd *pd, Puint8  *rep);
ssize_t Pe_uint8_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				Pbase_m *m, Pbase_pd *pd, Puint8  *rep);
ssize_t Pe_uint16_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			  Pbase_m *m, Pbase_pd *pd, Puint16 *rep);
ssize_t Pe_uint16_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				Pbase_m *m, Pbase_pd *pd, Puint16 *rep);
ssize_t Pe_uint32_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			  Pbase_m *m, Pbase_pd *pd, Puint32 *rep);
ssize_t Pe_uint32_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				Pbase_m *m, Pbase_pd *pd, Puint32 *rep);
ssize_t Pe_uint64_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			  Pbase_m *m, Pbase_pd *pd, Puint64 *rep);
ssize_t Pe_uint64_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				Pbase_m *m, Pbase_pd *pd, Puint64 *rep);

ssize_t Pe_int8_fmt2io   (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			  Pbase_m *m, Pbase_pd *pd, Pint8  *rep);
ssize_t Pe_int16_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			  Pbase_m *m, Pbase_pd *pd, Pint16 *rep);
ssize_t Pe_int32_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			  Pbase_m *m, Pbase_pd *pd, Pint32 *rep);
ssize_t Pe_int64_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			  Pbase_m *m, Pbase_pd *pd, Pint64 *rep);

ssize_t Pe_uint8_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			  Pbase_m *m, Pbase_pd *pd, Puint8  *rep);
ssize_t Pe_uint16_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			  Pbase_m *m, Pbase_pd *pd, Puint16 *rep);
ssize_t Pe_uint32_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			  Pbase_m *m, Pbase_pd *pd, Puint32 *rep);
ssize_t Pe_uint64_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			  Pbase_m *m, Pbase_pd *pd, Puint64 *rep);
#endif

#if P_CONFIG_E_INT_FW > 0
ssize_t Pe_int8_FW_fmt2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Pint8  *rep, size_t width);
ssize_t Pe_int8_FW_fmt2buf_final  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				   Pbase_m *m, Pbase_pd *pd, Pint8  *rep, size_t width);
ssize_t Pe_int16_FW_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Pint16 *rep, size_t width);
ssize_t Pe_int16_FW_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				   Pbase_m *m, Pbase_pd *pd, Pint16 *rep, size_t width);
ssize_t Pe_int32_FW_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Pint32 *rep, size_t width);
ssize_t Pe_int32_FW_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				   Pbase_m *m, Pbase_pd *pd, Pint32 *rep, size_t width);
ssize_t Pe_int64_FW_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Pint64 *rep, size_t width);
ssize_t Pe_int64_FW_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				   Pbase_m *m, Pbase_pd *pd, Pint64 *rep, size_t width);

ssize_t Pe_uint8_FW_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Puint8  *rep, size_t width);
ssize_t Pe_uint8_FW_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				   Pbase_m *m, Pbase_pd *pd, Puint8  *rep, size_t width);
ssize_t Pe_uint16_FW_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Puint16 *rep, size_t width);
ssize_t Pe_uint16_FW_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				   Pbase_m *m, Pbase_pd *pd, Puint16 *rep, size_t width);
ssize_t Pe_uint32_FW_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width);
ssize_t Pe_uint32_FW_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				   Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width);
ssize_t Pe_uint64_FW_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Puint64 *rep, size_t width);
ssize_t Pe_uint64_FW_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				   Pbase_m *m, Pbase_pd *pd, Puint64 *rep, size_t width);

ssize_t Pe_int8_FW_fmt2io   (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Pint8  *rep, size_t width);
ssize_t Pe_int16_FW_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Pint16 *rep, size_t width);
ssize_t Pe_int32_FW_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Pint32 *rep, size_t width);
ssize_t Pe_int64_FW_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Pint64 *rep, size_t width);

ssize_t Pe_uint8_FW_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Puint8  *rep, size_t width);
ssize_t Pe_uint16_FW_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Puint16 *rep, size_t width);
ssize_t Pe_uint32_FW_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width);
ssize_t Pe_uint64_FW_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Puint64 *rep, size_t width);
#endif

#if P_CONFIG_A_INT > 0 || P_CONFIG_E_INT > 0
ssize_t Pint8_fmt2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			Pbase_m *m, Pbase_pd *pd, Pint8  *rep);
ssize_t Pint8_fmt2buf_final  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Pint8  *rep);
ssize_t Pint16_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			Pbase_m *m, Pbase_pd *pd, Pint16 *rep);
ssize_t Pint16_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Pint16 *rep);
ssize_t Pint32_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			Pbase_m *m, Pbase_pd *pd, Pint32 *rep);
ssize_t Pint32_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Pint32 *rep);
ssize_t Pint64_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			Pbase_m *m, Pbase_pd *pd, Pint64 *rep);
ssize_t Pint64_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Pint64 *rep);

ssize_t Puint8_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			Pbase_m *m, Pbase_pd *pd, Puint8  *rep);
ssize_t Puint8_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Puint8  *rep);
ssize_t Puint16_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			Pbase_m *m, Pbase_pd *pd, Puint16 *rep);
ssize_t Puint16_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Puint16 *rep);
ssize_t Puint32_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			Pbase_m *m, Pbase_pd *pd, Puint32 *rep);
ssize_t Puint32_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Puint32 *rep);
ssize_t Puint64_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			Pbase_m *m, Pbase_pd *pd, Puint64 *rep);
ssize_t Puint64_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			      Pbase_m *m, Pbase_pd *pd, Puint64 *rep);

ssize_t Pint8_fmt2io   (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			Pbase_m *m, Pbase_pd *pd, Pint8  *rep);
ssize_t Pint16_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			Pbase_m *m, Pbase_pd *pd, Pint16 *rep);
ssize_t Pint32_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			Pbase_m *m, Pbase_pd *pd, Pint32 *rep);
ssize_t Pint64_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			Pbase_m *m, Pbase_pd *pd, Pint64 *rep);

ssize_t Puint8_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			Pbase_m *m, Pbase_pd *pd, Puint8  *rep);
ssize_t Puint16_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			Pbase_m *m, Pbase_pd *pd, Puint16 *rep);
ssize_t Puint32_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			Pbase_m *m, Pbase_pd *pd, Puint32 *rep);
ssize_t Puint64_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			Pbase_m *m, Pbase_pd *pd, Puint64 *rep);
#endif

#if P_CONFIG_A_INT_FW > 0 || P_CONFIG_E_INT_FW > 0
ssize_t Pint8_FW_fmt2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			   Pbase_m *m, Pbase_pd *pd, Pint8  *rep, size_t width);
ssize_t Pint8_FW_fmt2buf_final  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				 Pbase_m *m, Pbase_pd *pd, Pint8  *rep, size_t width);
ssize_t Pint16_FW_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			   Pbase_m *m, Pbase_pd *pd, Pint16 *rep, size_t width);
ssize_t Pint16_FW_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				 Pbase_m *m, Pbase_pd *pd, Pint16 *rep, size_t width);
ssize_t Pint32_FW_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			   Pbase_m *m, Pbase_pd *pd, Pint32 *rep, size_t width);
ssize_t Pint32_FW_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				 Pbase_m *m, Pbase_pd *pd, Pint32 *rep, size_t width);
ssize_t Pint64_FW_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			   Pbase_m *m, Pbase_pd *pd, Pint64 *rep, size_t width);
ssize_t Pint64_FW_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				 Pbase_m *m, Pbase_pd *pd, Pint64 *rep, size_t width);

ssize_t Puint8_FW_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			   Pbase_m *m, Pbase_pd *pd, Puint8  *rep, size_t width);
ssize_t Puint8_FW_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				 Pbase_m *m, Pbase_pd *pd, Puint8  *rep, size_t width);
ssize_t Puint16_FW_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			   Pbase_m *m, Pbase_pd *pd, Puint16 *rep, size_t width);
ssize_t Puint16_FW_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				 Pbase_m *m, Pbase_pd *pd, Puint16 *rep, size_t width);
ssize_t Puint32_FW_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			   Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width);
ssize_t Puint32_FW_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				 Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width);
ssize_t Puint64_FW_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			   Pbase_m *m, Pbase_pd *pd, Puint64 *rep, size_t width);
ssize_t Puint64_FW_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				 Pbase_m *m, Pbase_pd *pd, Puint64 *rep, size_t width);

ssize_t Pint8_FW_fmt2io   (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			   Pbase_m *m, Pbase_pd *pd, Pint8  *rep, size_t width);
ssize_t Pint16_FW_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			   Pbase_m *m, Pbase_pd *pd, Pint16 *rep, size_t width);
ssize_t Pint32_FW_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			   Pbase_m *m, Pbase_pd *pd, Pint32 *rep, size_t width);
ssize_t Pint64_FW_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			   Pbase_m *m, Pbase_pd *pd, Pint64 *rep, size_t width);

ssize_t Puint8_FW_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			   Pbase_m *m, Pbase_pd *pd, Puint8  *rep, size_t width);
ssize_t Puint16_FW_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			   Pbase_m *m, Pbase_pd *pd, Puint16 *rep, size_t width);
ssize_t Puint32_FW_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			   Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width);
ssize_t Puint64_FW_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			   Pbase_m *m, Pbase_pd *pd, Puint64 *rep, size_t width);
#endif

#if P_CONFIG_B_INT > 0
ssize_t Pb_int8_fmt2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Pint8  *rep);
ssize_t Pb_int8_fmt2buf_final  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				Pbase_m *m, Pbase_pd *pd, Pint8  *rep);
ssize_t Pb_int16_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Pint16 *rep);
ssize_t Pb_int16_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				Pbase_m *m, Pbase_pd *pd, Pint16 *rep);
ssize_t Pb_int32_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Pint32 *rep);
ssize_t Pb_int32_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				Pbase_m *m, Pbase_pd *pd, Pint32 *rep);
ssize_t Pb_int64_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Pint64 *rep);
ssize_t Pb_int64_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				Pbase_m *m, Pbase_pd *pd, Pint64 *rep);

ssize_t Pb_uint8_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Puint8  *rep);
ssize_t Pb_uint8_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				Pbase_m *m, Pbase_pd *pd, Puint8  *rep);
ssize_t Pb_uint16_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Puint16 *rep);
ssize_t Pb_uint16_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				Pbase_m *m, Pbase_pd *pd, Puint16 *rep);
ssize_t Pb_uint32_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Puint32 *rep);
ssize_t Pb_uint32_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				Pbase_m *m, Pbase_pd *pd, Puint32 *rep);
ssize_t Pb_uint64_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Puint64 *rep);
ssize_t Pb_uint64_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				Pbase_m *m, Pbase_pd *pd, Puint64 *rep);

ssize_t Pb_int8_fmt2io   (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Pint8  *rep);
ssize_t Pb_int16_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Pint16 *rep);
ssize_t Pb_int32_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Pint32 *rep);
ssize_t Pb_int64_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Pint64 *rep);

ssize_t Pb_uint8_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Puint8  *rep);
ssize_t Pb_uint16_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Puint16 *rep);
ssize_t Pb_uint32_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Puint32 *rep);
ssize_t Pb_uint64_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Puint64 *rep);
#endif

#if P_CONFIG_EBC_INT > 0
ssize_t Pebc_int8_fmt2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Pint8  *rep, Puint32 num_digits);
ssize_t Pebc_int8_fmt2buf_final  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Pint8  *rep, Puint32 num_digits);
ssize_t Pebc_int16_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Pint16 *rep, Puint32 num_digits);
ssize_t Pebc_int16_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Pint16 *rep, Puint32 num_digits);
ssize_t Pebc_int32_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Pint32 *rep, Puint32 num_digits);
ssize_t Pebc_int32_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Pint32 *rep, Puint32 num_digits);
ssize_t Pebc_int64_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Pint64 *rep, Puint32 num_digits);
ssize_t Pebc_int64_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Pint64 *rep, Puint32 num_digits);

ssize_t Pebc_uint8_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Puint8  *rep, Puint32 num_digits);
ssize_t Pebc_uint8_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint8  *rep, Puint32 num_digits);
ssize_t Pebc_uint16_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Puint16 *rep, Puint32 num_digits);
ssize_t Pebc_uint16_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint16 *rep, Puint32 num_digits);
ssize_t Pebc_uint32_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Puint32 num_digits);
ssize_t Pebc_uint32_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Puint32 num_digits);
ssize_t Pebc_uint64_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Puint64 *rep, Puint32 num_digits);
ssize_t Pebc_uint64_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint64 *rep, Puint32 num_digits);

ssize_t Pebc_int8_fmt2io   (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Pint8  *rep, Puint32 num_digits);
ssize_t Pebc_int16_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Pint16 *rep, Puint32 num_digits);
ssize_t Pebc_int32_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Pint32 *rep, Puint32 num_digits);
ssize_t Pebc_int64_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Pint64 *rep, Puint32 num_digits);

ssize_t Pebc_uint8_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Puint8  *rep, Puint32 num_digits);
ssize_t Pebc_uint16_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Puint16 *rep, Puint32 num_digits);
ssize_t Pebc_uint32_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Puint32 num_digits);
ssize_t Pebc_uint64_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Puint64 *rep, Puint32 num_digits);
#endif

#if P_CONFIG_BCD_INT > 0
ssize_t Pbcd_int8_fmt2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Pint8  *rep, Puint32 num_digits);
ssize_t Pbcd_int8_fmt2buf_final  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Pint8  *rep, Puint32 num_digits);
ssize_t Pbcd_int16_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Pint16 *rep, Puint32 num_digits);
ssize_t Pbcd_int16_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Pint16 *rep, Puint32 num_digits);
ssize_t Pbcd_int32_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Pint32 *rep, Puint32 num_digits);
ssize_t Pbcd_int32_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Pint32 *rep, Puint32 num_digits);
ssize_t Pbcd_int64_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Pint64 *rep, Puint32 num_digits);
ssize_t Pbcd_int64_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Pint64 *rep, Puint32 num_digits);

ssize_t Pbcd_uint8_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Puint8  *rep, Puint32 num_digits);
ssize_t Pbcd_uint8_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint8  *rep, Puint32 num_digits);
ssize_t Pbcd_uint16_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Puint16 *rep, Puint32 num_digits);
ssize_t Pbcd_uint16_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint16 *rep, Puint32 num_digits);
ssize_t Pbcd_uint32_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Puint32 num_digits);
ssize_t Pbcd_uint32_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Puint32 num_digits);
ssize_t Pbcd_uint64_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Puint64 *rep, Puint32 num_digits);
ssize_t Pbcd_uint64_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint64 *rep, Puint32 num_digits);

ssize_t Pbcd_int8_fmt2io   (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Pint8  *rep, Puint32 num_digits);
ssize_t Pbcd_int16_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Pint16 *rep, Puint32 num_digits);
ssize_t Pbcd_int32_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Pint32 *rep, Puint32 num_digits);
ssize_t Pbcd_int64_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Pint64 *rep, Puint32 num_digits);

ssize_t Pbcd_uint8_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Puint8  *rep, Puint32 num_digits);
ssize_t Pbcd_uint16_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Puint16 *rep, Puint32 num_digits);
ssize_t Pbcd_uint32_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Puint32 num_digits);
ssize_t Pbcd_uint64_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Puint64 *rep, Puint32 num_digits);
#endif

#if P_CONFIG_SBL_INT > 0
ssize_t Psbl_int8_fmt2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Pint8  *rep, Puint32 num_bytes);
ssize_t Psbl_int8_fmt2buf_final  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Pint8  *rep, Puint32 num_bytes);
ssize_t Psbl_int16_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Pint16 *rep, Puint32 num_bytes);
ssize_t Psbl_int16_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Pint16 *rep, Puint32 num_bytes);
ssize_t Psbl_int32_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Pint32 *rep, Puint32 num_bytes);
ssize_t Psbl_int32_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Pint32 *rep, Puint32 num_bytes);
ssize_t Psbl_int64_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Pint64 *rep, Puint32 num_bytes);
ssize_t Psbl_int64_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Pint64 *rep, Puint32 num_bytes);

ssize_t Psbl_uint8_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Puint8  *rep, Puint32 num_bytes);
ssize_t Psbl_uint8_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint8  *rep, Puint32 num_bytes);
ssize_t Psbl_uint16_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Puint16 *rep, Puint32 num_bytes);
ssize_t Psbl_uint16_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint16 *rep, Puint32 num_bytes);
ssize_t Psbl_uint32_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Puint32 num_bytes);
ssize_t Psbl_uint32_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Puint32 num_bytes);
ssize_t Psbl_uint64_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Puint64 *rep, Puint32 num_bytes);
ssize_t Psbl_uint64_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint64 *rep, Puint32 num_bytes);

ssize_t Psbl_int8_fmt2io   (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Pint8  *rep, Puint32 num_bytes);
ssize_t Psbl_int16_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Pint16 *rep, Puint32 num_bytes);
ssize_t Psbl_int32_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Pint32 *rep, Puint32 num_bytes);
ssize_t Psbl_int64_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Pint64 *rep, Puint32 num_bytes);

ssize_t Psbl_uint8_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Puint8  *rep, Puint32 num_bytes);
ssize_t Psbl_uint16_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Puint16 *rep, Puint32 num_bytes);
ssize_t Psbl_uint32_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Puint32 num_bytes);
ssize_t Psbl_uint64_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Puint64 *rep, Puint32 num_bytes);
#endif

#if P_CONFIG_SBH_INT > 0
ssize_t Psbh_int8_fmt2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Pint8  *rep, Puint32 num_bytes);
ssize_t Psbh_int8_fmt2buf_final  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Pint8  *rep, Puint32 num_bytes);
ssize_t Psbh_int16_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Pint16 *rep, Puint32 num_bytes);
ssize_t Psbh_int16_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Pint16 *rep, Puint32 num_bytes);
ssize_t Psbh_int32_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Pint32 *rep, Puint32 num_bytes);
ssize_t Psbh_int32_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Pint32 *rep, Puint32 num_bytes);
ssize_t Psbh_int64_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Pint64 *rep, Puint32 num_bytes);
ssize_t Psbh_int64_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Pint64 *rep, Puint32 num_bytes);

ssize_t Psbh_uint8_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Puint8  *rep, Puint32 num_bytes);
ssize_t Psbh_uint8_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint8  *rep, Puint32 num_bytes);
ssize_t Psbh_uint16_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Puint16 *rep, Puint32 num_bytes);
ssize_t Psbh_uint16_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint16 *rep, Puint32 num_bytes);
ssize_t Psbh_uint32_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Puint32 num_bytes);
ssize_t Psbh_uint32_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Puint32 num_bytes);
ssize_t Psbh_uint64_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Puint64 *rep, Puint32 num_bytes);
ssize_t Psbh_uint64_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint64 *rep, Puint32 num_bytes);

ssize_t Psbh_int8_fmt2io   (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Pint8  *rep, Puint32 num_bytes);
ssize_t Psbh_int16_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Pint16 *rep, Puint32 num_bytes);
ssize_t Psbh_int32_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Pint32 *rep, Puint32 num_bytes);
ssize_t Psbh_int64_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Pint64 *rep, Puint32 num_bytes);

ssize_t Psbh_uint8_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Puint8  *rep, Puint32 num_bytes);
ssize_t Psbh_uint16_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Puint16 *rep, Puint32 num_bytes);
ssize_t Psbh_uint32_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Puint32 num_bytes);
ssize_t Psbh_uint64_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			       Pbase_m *m, Pbase_pd *pd, Puint64 *rep, Puint32 num_bytes);
#endif

#if P_CONFIG_A_FLOAT > 0
ssize_t Pa_float32_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Pfloat32 *rep);
ssize_t Pa_float32_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Pfloat32 *rep);
ssize_t Pa_float64_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Pfloat64 *rep);
ssize_t Pa_float64_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Pfloat64 *rep);

ssize_t Pa_float32_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Pfloat32 *rep);
ssize_t Pa_float64_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Pfloat64 *rep);
#endif

#if P_CONFIG_E_FLOAT > 0
ssize_t Pe_float32_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Pfloat32 *rep);
ssize_t Pe_float32_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Pfloat32 *rep);
ssize_t Pe_float64_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Pfloat64 *rep);
ssize_t Pe_float64_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Pfloat64 *rep);

ssize_t Pe_float32_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Pfloat32 *rep);
ssize_t Pe_float64_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Pfloat64 *rep);
#endif

#if P_CONFIG_A_FLOAT > 0 || P_CONFIG_E_FLOAT > 0
ssize_t Pfloat32_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			  Pbase_m *m, Pbase_pd *pd, Pfloat32 *rep);
ssize_t Pfloat32_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				Pbase_m *m, Pbase_pd *pd, Pfloat32 *rep);
ssize_t Pfloat64_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			  Pbase_m *m, Pbase_pd *pd, Pfloat64 *rep);
ssize_t Pfloat64_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				Pbase_m *m, Pbase_pd *pd, Pfloat64 *rep);

ssize_t Pfloat32_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			  Pbase_m *m, Pbase_pd *pd, Pfloat32 *rep);
ssize_t Pfloat64_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			  Pbase_m *m, Pbase_pd *pd, Pfloat64 *rep);
#endif

#endif /* P_CONFIG_WRITE_FUNCTIONS */

/*
 * The following default versions simply call the appropriate ASCII or EBCDIC version,
 * depending on pads->disc->def_charset.
 */

#ifdef FOR_CKIT
#if P_CONFIG_WRITE_FUNCTIONS > 0

#if P_CONFIG_A_INT_FW > 0 && P_CONFIG_E_INT_FW > 0
ssize_t Pint8_FW_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint8   *val, size_t width);
ssize_t Pint16_FW_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint16  *val, size_t width);
ssize_t Pint32_FW_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint32  *val, size_t width);
ssize_t Pint64_FW_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint64  *val, size_t width);

ssize_t Puint8_FW_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint8  *val, size_t width);
ssize_t Puint16_FW_write2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint16 *val, size_t width);
ssize_t Puint32_FW_write2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *val, size_t width);
ssize_t Puint64_FW_write2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint64 *val, size_t width);

ssize_t Pint8_FW_write2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint8   *val, size_t width);
ssize_t Pint16_FW_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint16  *val, size_t width);
ssize_t Pint32_FW_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint32  *val, size_t width);
ssize_t Pint64_FW_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint64  *val, size_t width);

ssize_t Puint8_FW_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint8  *val, size_t width);
ssize_t Puint16_FW_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint16 *val, size_t width);
ssize_t Puint32_FW_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *val, size_t width);
ssize_t Puint64_FW_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint64 *val, size_t width);

ssize_t Pint8_FW_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint8   *val, const char *tag, int indent, size_t width);
ssize_t Pint16_FW_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint16  *val, const char *tag, int indent, size_t width);
ssize_t Pint32_FW_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint32  *val, const char *tag, int indent, size_t width);
ssize_t Pint64_FW_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint64  *val, const char *tag, int indent, size_t width);

ssize_t Puint8_FW_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint8  *val, const char *tag, int indent, size_t width);
ssize_t Puint16_FW_write_xml_2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint16 *val, const char *tag, int indent, size_t width);
ssize_t Puint32_FW_write_xml_2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *val, const char *tag, int indent, size_t width);
ssize_t Puint64_FW_write_xml_2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint64 *val, const char *tag, int indent, size_t width);

ssize_t Pint8_FW_write_xml_2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint8   *val, const char *tag, int indent, size_t width);
ssize_t Pint16_FW_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint16  *val, const char *tag, int indent, size_t width);
ssize_t Pint32_FW_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint32  *val, const char *tag, int indent, size_t width);
ssize_t Pint64_FW_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint64  *val, const char *tag, int indent, size_t width);

ssize_t Puint8_FW_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint8  *val, const char *tag, int indent, size_t width);
ssize_t Puint16_FW_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint16 *val, const char *tag, int indent, size_t width);
ssize_t Puint32_FW_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *val, const char *tag, int indent, size_t width);
ssize_t Puint64_FW_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint64 *val, const char *tag, int indent, size_t width);
#endif

#if P_CONFIG_A_INT > 0 && P_CONFIG_E_INT > 0
ssize_t Pint8_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint8   *val);
ssize_t Pint16_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint16  *val);
ssize_t Pint32_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint32  *val);
ssize_t Pint64_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint64  *val);

ssize_t Puint8_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint8  *val);
ssize_t Puint16_write2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint16 *val);
ssize_t Puint32_write2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *val);
ssize_t Puint64_write2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint64 *val);

ssize_t Pint8_write2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint8   *val);
ssize_t Pint16_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint16  *val);
ssize_t Pint32_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint32  *val);
ssize_t Pint64_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint64  *val);

ssize_t Puint8_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint8  *val);
ssize_t Puint16_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint16 *val);
ssize_t Puint32_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *val);
ssize_t Puint64_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint64 *val);

ssize_t Pint8_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint8   *val, const char *tag, int indent);
ssize_t Pint16_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint16  *val, const char *tag, int indent);
ssize_t Pint32_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint32  *val, const char *tag, int indent);
ssize_t Pint64_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint64  *val, const char *tag, int indent);

ssize_t Puint8_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint8  *val, const char *tag, int indent);
ssize_t Puint16_write_xml_2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint16 *val, const char *tag, int indent);
ssize_t Puint32_write_xml_2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *val, const char *tag, int indent);
ssize_t Puint64_write_xml_2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint64 *val, const char *tag, int indent);

ssize_t Pint8_write_xml_2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint8   *val, const char *tag, int indent);
ssize_t Pint16_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint16  *val, const char *tag, int indent);
ssize_t Pint32_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint32  *val, const char *tag, int indent);
ssize_t Pint64_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint64  *val, const char *tag, int indent);

ssize_t Puint8_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint8  *val, const char *tag, int indent);
ssize_t Puint16_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint16 *val, const char *tag, int indent);
ssize_t Puint32_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *val, const char *tag, int indent);
ssize_t Puint64_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint64 *val, const char *tag, int indent);
#endif

#if P_CONFIG_A_FLOAT > 0 && P_CONFIG_E_FLOAT > 0
ssize_t Pfloat32_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pfloat32  *val);
ssize_t Pfloat64_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pfloat64  *val);

ssize_t Pfloat32_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfloat32  *val);
ssize_t Pfloat64_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfloat64  *val);

ssize_t Pfloat32_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pfloat32  *val, const char *tag, int indent);
ssize_t Pfloat64_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pfloat64  *val, const char *tag, int indent);

ssize_t Pfloat32_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfloat32  *val, const char *tag, int indent);
ssize_t Pfloat64_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfloat64  *val, const char *tag, int indent);
#endif

#endif /* P_CONFIG_WRITE_FUNCTIONS */
#endif /* FOR_CKIT */

/* ================================================================================
 * MISC WRITE FUNCTIONS
 *
 * The countX and countXtoY write functions do nothing and return length 0.
 * They exist for completeness.
 */

#if P_CONFIG_WRITE_FUNCTIONS > 0
ssize_t PcountX_write2io    (P_t *pads, Sfio_t *io,
			     Puint8 x, int eor_required, size_t count_max,
			     Pbase_pd *pd, Pint32  *val);
ssize_t PcountX_write2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full,
			     Puint8 x, int eor_required, size_t count_max,
			     Pbase_pd *pd, Pint32  *val);

ssize_t PcountXtoY_write2io (P_t *pads, Sfio_t *io,
			     Puint8 x, Puint8 y, size_t count_max,
			     Pbase_pd *pd, Pint32  *val);
ssize_t PcountXtoY_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full,
			     Puint8 x, Puint8 y, size_t count_max,
			     Pbase_pd *pd, Pint32  *val);

ssize_t PcountX_write_xml_2io    (P_t *pads, Sfio_t *io,
				  Puint8 x, int eor_required, size_t count_max,
				  Pbase_pd *pd, Pint32  *val, const char *tag, int indent);
ssize_t PcountX_write_xml_2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full,
				  Puint8 x, int eor_required, size_t count_max,
				  Pbase_pd *pd, Pint32  *val, const char *tag, int indent);

ssize_t PcountXtoY_write_xml_2io (P_t *pads, Sfio_t *io,
				  Puint8 x, Puint8 y, size_t count_max,
				  Pbase_pd *pd, Pint32  *val, const char *tag, int indent);
ssize_t PcountXtoY_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full,
				  Puint8 x, Puint8 y, size_t count_max,
				  Pbase_pd *pd, Pint32  *val, const char *tag, int indent);
#endif /* P_CONFIG_WRITE_FUNCTIONS */

/* ================================================================================
 * BASE TYPE ACCUMULATORS
 *
 * For integer type T, accumulator functions P_T_acc_avg returns the running average
 * as a Pfloat64, while P_T_acc_ravg returns the average as a T value by rounding the
 * Pfloat64 to the nearest T.
 *
 * Each report function takes the following params (in addition to pads/disc first/last args):
 *   prefix: a descriptive string, usually the field name
 *           if NULL or empty, the string "<top>" is used
 *   what:   string describing kind of data
 *           if NULL, a short form of the accumulator type is used as default,
 *           e.g., "int32" is the default for Pint32_acc.
 *   nst:    nesting level: level zero should be used for a top-level report call;
 *           reporting routines bump the nesting level for recursive report calls that
 *           describe sub-parts.  Nesting level -1 indicates a minimal prefix header
 *           should be output, i.e., just the prefix without any adornment.
 *   a:      the accumulator
 */

typedef struct Pint_acc_s {
  Dt_t     *dict;
  Puint64   max2track;
  Puint64   max2rep;
  Pfloat64  pcnt2rep;
  Puint64   good;
  Puint64   bad;
  Puint64   tracked;
  Puint64   fold;
  Pint64    psum;
  Pfloat64  avg;
  Pint64    min;
  Pint64    max;
} Pint_acc;

typedef struct Puint_acc_s {
  Dt_t     *dict;
  Puint64   max2track;
  Puint64   max2rep;
  Pfloat64  pcnt2rep;
  Puint64   good;
  Puint64   bad;
  Puint64   tracked;
  Puint64   fold;
  Puint64   psum;
  Pfloat64  avg;
  Puint64   min;
  Puint64   max;
} Puint_acc;

/* A map_<int_type> function maps a given integer type to a string */
typedef const char * (*Pint8_map_fn)  (Pint8   i);
typedef const char * (*Pint16_map_fn) (Pint16  i);
typedef const char * (*Pint32_map_fn) (Pint32  i);
typedef const char * (*Pint64_map_fn) (Pint64  i);
typedef const char * (*Puint8_map_fn) (Puint8  u);
typedef const char * (*Puint16_map_fn)(Puint16 u);
typedef const char * (*Puint32_map_fn)(Puint32 u);
typedef const char * (*Puint64_map_fn)(Puint64 u);

/* We always need type Pint32_acc, Puint32_acc */

typedef Pint_acc Pint32_acc;
typedef Puint_acc Puint32_acc;

Perror_t Pint32_acc_init    (P_t *pads, Pint32_acc *a);
Perror_t Pint32_acc_reset   (P_t *pads, Pint32_acc *a);
Perror_t Pint32_acc_cleanup (P_t *pads, Pint32_acc *a);
Perror_t Pint32_acc_add     (P_t *pads, Pint32_acc *a, const Pbase_pd *pd, const Pint32 *val);
Perror_t Pint32_acc_report  (P_t *pads, const char *prefix, const char *what, int nst, Pint32_acc *a);
Pfloat64 Pint32_acc_avg     (P_t *pads, Pint32_acc *a);
Pint32   Pint32_acc_ravg    (P_t *pads, Pint32_acc *a);

Perror_t Puint32_acc_init    (P_t *pads, Puint32_acc *a);
Perror_t Puint32_acc_reset   (P_t *pads, Puint32_acc *a);
Perror_t Puint32_acc_cleanup (P_t *pads, Puint32_acc *a);
Perror_t Puint32_acc_add     (P_t *pads, Puint32_acc *a, const Pbase_pd *pd, const Puint32 *val);
Perror_t Puint32_acc_report  (P_t *pads, const char *prefix, const char *what, int nst, Puint32_acc *a);
Pfloat64 Puint32_acc_avg     (P_t *pads, Puint32_acc *a);
Puint32  Puint32_acc_ravg    (P_t *pads, Puint32_acc *a);

/*
 * Mapped versions of the integer acc_report functions:
 * these functions are used when integers have associated
 * string values.  
 */
Perror_t Pint32_acc_map_report(P_t *pads, const char *prefix, const char *what, int nst,
			       Pint32_map_fn  fn, Pint32_acc *a);

/*
 * P_nerr_acc_report is used to report on the accumulation of the nerr field
 * of a struct, union, array, etc.  The accumulator used must be a Puint32_acc.
 * This is very similar to calling Puint32_acc_report, it just has slightly
 * different formatting since no bad values are expected.
 */
Perror_t P_nerr_acc_report(P_t *pads, const char *prefix, const char *what, int nst,
			   Puint32_acc *a);

/* Remaining accumulator types: only if configured */ 
#if P_CONFIG_ACCUM_FUNCTIONS > 0

typedef Pint_acc Pint8_acc;
typedef Pint_acc Pint16_acc;
typedef Pint_acc Pint64_acc;

typedef Puint_acc Puint8_acc;
typedef Puint_acc Puint16_acc;
typedef Puint_acc Puint64_acc;

/* date accumulation uses Puint32 */
typedef Puint_acc Pdate_acc;

typedef struct Pstring_acc_s {
  Dt_t        *dict;
  Puint64      max2track;
  Puint64      max2rep;
  Pfloat64     pcnt2rep;
  Puint64      tracked;
  Puint32_acc  len_accum; /* used for length distribution and good/bad accounting */
} Pstring_acc;

Perror_t Pint8_acc_init    (P_t *pads, Pint8_acc *a);
Perror_t Pint8_acc_reset   (P_t *pads, Pint8_acc *a);
Perror_t Pint8_acc_cleanup (P_t *pads, Pint8_acc *a);
Perror_t Pint8_acc_add     (P_t *pads, Pint8_acc *a, const Pbase_pd *pd, const Pint8 *val);
Perror_t Pint8_acc_report  (P_t *pads, const char *prefix, const char *what, int nst, Pint8_acc *a);
Pfloat64 Pint8_acc_avg     (P_t *pads, Pint8_acc *a);
Pint8    Pint8_acc_ravg    (P_t *pads, Pint8_acc *a);

Perror_t Pint16_acc_init    (P_t *pads, Pint16_acc *a);
Perror_t Pint16_acc_reset   (P_t *pads, Pint16_acc *a);
Perror_t Pint16_acc_cleanup (P_t *pads, Pint16_acc *a);
Perror_t Pint16_acc_add     (P_t *pads, Pint16_acc *a, const Pbase_pd *pd, const Pint16 *val);
Perror_t Pint16_acc_report  (P_t *pads, const char *prefix, const char *what, int nst, Pint16_acc *a);
Pfloat64 Pint16_acc_avg     (P_t *pads, Pint16_acc *a);
Pint16   Pint16_acc_ravg    (P_t *pads, Pint16_acc *a);

Perror_t Pint64_acc_init    (P_t *pads, Pint64_acc *a);
Perror_t Pint64_acc_reset   (P_t *pads, Pint64_acc *a);
Perror_t Pint64_acc_cleanup (P_t *pads, Pint64_acc *a);
Perror_t Pint64_acc_add     (P_t *pads, Pint64_acc *a, const Pbase_pd *pd, const Pint64 *val);
Perror_t Pint64_acc_report  (P_t *pads, const char *prefix, const char *what, int nst, Pint64_acc *a);
Pfloat64 Pint64_acc_avg     (P_t *pads, Pint64_acc *a);
Pint64   Pint64_acc_ravg    (P_t *pads, Pint64_acc *a);

Perror_t Puint8_acc_init    (P_t *pads, Puint8_acc *a);
Perror_t Puint8_acc_reset   (P_t *pads, Puint8_acc *a);
Perror_t Puint8_acc_cleanup (P_t *pads, Puint8_acc *a);
Perror_t Puint8_acc_add     (P_t *pads, Puint8_acc *a, const Pbase_pd *pd, const Puint8 *val);
Perror_t Puint8_acc_report  (P_t *pads, const char *prefix, const char *what, int nst, Puint8_acc *a);
Pfloat64 Puint8_acc_avg     (P_t *pads, Puint8_acc *a);
Puint8   Puint8_acc_ravg    (P_t *pads, Puint8_acc *a);

Perror_t Puint16_acc_init    (P_t *pads, Puint16_acc *a);
Perror_t Puint16_acc_reset   (P_t *pads, Puint16_acc *a);
Perror_t Puint16_acc_cleanup (P_t *pads, Puint16_acc *a);
Perror_t Puint16_acc_add     (P_t *pads, Puint16_acc *a, const Pbase_pd *pd, const Puint16 *val);
Perror_t Puint16_acc_report  (P_t *pads, const char *prefix, const char *what, int nst, Puint16_acc *a);
Pfloat64 Puint16_acc_avg     (P_t *pads, Puint16_acc *a);
Puint16  Puint16_acc_ravg    (P_t *pads, Puint16_acc *a);

Perror_t Puint64_acc_init    (P_t *pads, Puint64_acc *a);
Perror_t Puint64_acc_reset   (P_t *pads, Puint64_acc *a);
Perror_t Puint64_acc_cleanup (P_t *pads, Puint64_acc *a);
Perror_t Puint64_acc_add     (P_t *pads, Puint64_acc *a, const Pbase_pd *pd, const Puint64 *val);
Perror_t Puint64_acc_report  (P_t *pads, const char *prefix, const char *what, int nst, Puint64_acc *a);
Pfloat64 Puint64_acc_avg     (P_t *pads, Puint64_acc *a);
Puint64  Puint64_acc_ravg    (P_t *pads, Puint64_acc *a);

Perror_t Pstring_acc_init    (P_t *pads, Pstring_acc *a);
Perror_t Pstring_acc_reset   (P_t *pads, Pstring_acc *a);
Perror_t Pstring_acc_cleanup (P_t *pads, Pstring_acc *a);
Perror_t Pstring_acc_add     (P_t *pads, Pstring_acc *a, const Pbase_pd *pd, const Pstring *val);
Perror_t Pstring_acc_report  (P_t *pads, const char *prefix, const char *what, int nst, Pstring_acc *a);

#ifdef FOR_CKIT
Perror_t Pdate_acc_init    (P_t *pads, Pdate_acc *a);
Perror_t Pdate_acc_reset   (P_t *pads, Pdate_acc *a);
Perror_t Pdate_acc_cleanup (P_t *pads, Pdate_acc *a);
Perror_t Pdate_acc_add     (P_t *pads, Pdate_acc *a, const Pbase_pd *pd, Puint32 *val);
#endif
Perror_t Pdate_acc_report  (P_t *pads, const char *prefix, const char *what, int nst, Pdate_acc *a);

/*
 * char_acc is just like uint8_acc except a different report is generated
 */
typedef Puint8_acc Pchar_acc;

Perror_t Pchar_acc_init      (P_t *pads, Pchar_acc *a);
Perror_t Pchar_acc_reset     (P_t *pads, Pchar_acc *a);
Perror_t Pchar_acc_cleanup   (P_t *pads, Pchar_acc *a);
Perror_t Pchar_acc_add       (P_t *pads, Pchar_acc *a, const Pbase_pd *pd, const Puint8 *val);
Perror_t Pchar_acc_report    (P_t *pads, const char *prefix, const char *what, int nst, Pchar_acc *a);

/*
 * fpoint/ufpoint accumulator types
 */

typedef struct Pfpoint_acc_s {
  Dt_t     *dict;
  Puint64   max2track;
  Puint64   max2rep;
  Pfloat64  pcnt2rep;
  Puint64   good;
  Puint64   bad;
  Puint64   tracked;
  Puint64   fold;
  Pfloat64  psum;
  Pfloat64  avg;
  Pfloat64  min;
  Pfloat64  max;
} Pfpoint_acc;

typedef Pfpoint_acc Pfpoint8_acc;
typedef Pfpoint_acc Pfpoint16_acc;
typedef Pfpoint_acc Pfpoint32_acc;
typedef Pfpoint_acc Pfpoint64_acc;

typedef Pfpoint_acc Pufpoint8_acc;
typedef Pfpoint_acc Pufpoint16_acc;
typedef Pfpoint_acc Pufpoint32_acc;
typedef Pfpoint_acc Pufpoint64_acc;

Perror_t Pfpoint8_acc_init    (P_t *pads, Pfpoint8_acc *a);
Perror_t Pfpoint8_acc_reset   (P_t *pads, Pfpoint8_acc *a);
Perror_t Pfpoint8_acc_cleanup (P_t *pads, Pfpoint8_acc *a);
Perror_t Pfpoint8_acc_add     (P_t *pads, Pfpoint8_acc *a, const Pbase_pd *pd, const Pfpoint8 *val);
Perror_t Pfpoint8_acc_report  (P_t *pads, const char *prefix, const char *what, int nst, Pfpoint8_acc *a);
Pfloat64 Pfpoint8_acc_avg     (P_t *pads, Pfpoint8_acc *a);

Perror_t Pfpoint16_acc_init   (P_t *pads, Pfpoint16_acc *a);
Perror_t Pfpoint16_acc_reset  (P_t *pads, Pfpoint16_acc *a);
Perror_t Pfpoint16_acc_cleanup(P_t *pads, Pfpoint16_acc *a);
Perror_t Pfpoint16_acc_add    (P_t *pads, Pfpoint16_acc *a, const Pbase_pd *pd, const Pfpoint16 *val);
Perror_t Pfpoint16_acc_report (P_t *pads, const char *prefix, const char *what, int nst, Pfpoint16_acc *a);
Pfloat64 Pfpoint16_acc_avg    (P_t *pads, Pfpoint16_acc *a);

Perror_t Pfpoint32_acc_init   (P_t *pads, Pfpoint32_acc *a);
Perror_t Pfpoint32_acc_reset  (P_t *pads, Pfpoint32_acc *a);
Perror_t Pfpoint32_acc_cleanup(P_t *pads, Pfpoint32_acc *a);
Perror_t Pfpoint32_acc_add    (P_t *pads, Pfpoint32_acc *a, const Pbase_pd *pd, const Pfpoint32 *val);
Perror_t Pfpoint32_acc_report (P_t *pads, const char *prefix, const char *what, int nst, Pfpoint32_acc *a);
Pfloat64 Pfpoint32_acc_avg    (P_t *pads, Pfpoint32_acc *a);

Perror_t Pfpoint64_acc_init   (P_t *pads, Pfpoint64_acc *a);
Perror_t Pfpoint64_acc_reset  (P_t *pads, Pfpoint64_acc *a);
Perror_t Pfpoint64_acc_cleanup(P_t *pads, Pfpoint64_acc *a);
Perror_t Pfpoint64_acc_add    (P_t *pads, Pfpoint64_acc *a, const Pbase_pd *pd, const Pfpoint64 *val);
Perror_t Pfpoint64_acc_report (P_t *pads, const char *prefix, const char *what, int nst, Pfpoint64_acc *a);
Pfloat64 Pfpoint64_acc_avg    (P_t *pads, Pfpoint64_acc *a);

Perror_t Pufpoint8_acc_init    (P_t *pads, Pufpoint8_acc *a);
Perror_t Pufpoint8_acc_reset   (P_t *pads, Pufpoint8_acc *a);
Perror_t Pufpoint8_acc_cleanup (P_t *pads, Pufpoint8_acc *a);
Perror_t Pufpoint8_acc_add     (P_t *pads, Pufpoint8_acc *a, const Pbase_pd *pd, const Pufpoint8 *val);
Perror_t Pufpoint8_acc_report  (P_t *pads, const char *prefix, const char *what, int nst, Pufpoint8_acc *a);
Pfloat64 Pufpoint8_acc_avg     (P_t *pads, Pufpoint8_acc *a);

Perror_t Pufpoint16_acc_init   (P_t *pads, Pufpoint16_acc *a);
Perror_t Pufpoint16_acc_reset  (P_t *pads, Pufpoint16_acc *a);
Perror_t Pufpoint16_acc_cleanup(P_t *pads, Pufpoint16_acc *a);
Perror_t Pufpoint16_acc_add    (P_t *pads, Pufpoint16_acc *a, const Pbase_pd *pd, const Pufpoint16 *val);
Perror_t Pufpoint16_acc_report (P_t *pads, const char *prefix, const char *what, int nst, Pufpoint16_acc *a);
Pfloat64 Pufpoint16_acc_avg    (P_t *pads, Pufpoint16_acc *a);

Perror_t Pufpoint32_acc_init   (P_t *pads, Pufpoint32_acc *a);
Perror_t Pufpoint32_acc_reset  (P_t *pads, Pufpoint32_acc *a);
Perror_t Pufpoint32_acc_cleanup(P_t *pads, Pufpoint32_acc *a);
Perror_t Pufpoint32_acc_add    (P_t *pads, Pufpoint32_acc *a, const Pbase_pd *pd, const Pufpoint32 *val);
Perror_t Pufpoint32_acc_report (P_t *pads, const char *prefix, const char *what, int nst, Pufpoint32_acc *a);
Pfloat64 Pufpoint32_acc_avg    (P_t *pads, Pufpoint32_acc *a);

Perror_t Pufpoint64_acc_init   (P_t *pads, Pufpoint64_acc *a);
Perror_t Pufpoint64_acc_reset  (P_t *pads, Pufpoint64_acc *a);
Perror_t Pufpoint64_acc_cleanup(P_t *pads, Pufpoint64_acc *a);
Perror_t Pufpoint64_acc_add    (P_t *pads, Pufpoint64_acc *a, const Pbase_pd *pd, const Pufpoint64 *val);
Perror_t Pufpoint64_acc_report (P_t *pads, const char *prefix, const char *what, int nst, Pufpoint64_acc *a);
Pfloat64 Pufpoint64_acc_avg    (P_t *pads, Pufpoint64_acc *a);

typedef struct Pfloat_acc_s {
  Dt_t     *dict;
  Puint64   max2track;
  Puint64   max2rep;
  Pfloat64  pcnt2rep;
  Puint64   good;
  Puint64   bad;
  Puint64   tracked;
  Puint64   fold;
  Pfloat64  psum;
  Pfloat64  avg;
  Pfloat64  min;
  Pfloat64  max;
} Pfloat_acc;

typedef Pfloat_acc Pfloat32_acc;
typedef Pfloat_acc Pfloat64_acc;

Perror_t Pfloat32_acc_init    (P_t *pads, Pfloat32_acc *a);
Perror_t Pfloat32_acc_reset   (P_t *pads, Pfloat32_acc *a);
Perror_t Pfloat32_acc_cleanup (P_t *pads, Pfloat32_acc *a);
Perror_t Pfloat32_acc_add     (P_t *pads, Pfloat32_acc *a, const Pbase_pd *pd, const Pfloat32 *val);
Perror_t Pfloat32_acc_report  (P_t *pads, const char *prefix, const char *what, int nst, Pfloat32_acc *a);
Pfloat64 Pfloat32_acc_avg     (P_t *pads, Pfloat32_acc *a);

Perror_t Pfloat64_acc_init    (P_t *pads, Pfloat64_acc *a);
Perror_t Pfloat64_acc_reset   (P_t *pads, Pfloat64_acc *a);
Perror_t Pfloat64_acc_cleanup (P_t *pads, Pfloat64_acc *a);
Perror_t Pfloat64_acc_add     (P_t *pads, Pfloat64_acc *a, const Pbase_pd *pd, const Pfloat64 *val);
Perror_t Pfloat64_acc_report  (P_t *pads, const char *prefix, const char *what, int nst, Pfloat64_acc *a);
Pfloat64 Pfloat64_acc_avg     (P_t *pads, Pfloat64_acc *a);

#endif /* P_CONFIG_ACCUM_FUNCTIONS */

/* ================================================================================
 * IO CHECKPOINT API
 *
 * The checkpoint API: if any of these return P_ERR, it is due to a space
 * allocation problem or a non-balanced use of checkpoint/commit/restore.
 * These are normally fatal errors -- the calling code should probably exit the program.
 *
 * If a non-zero speculative flag is passed to checkpoint, then the
 * speculative nesting level  is incremented by one.  Once the checkpoint
 * is removed by either commit or restore, the nesting level is
 * decremented by one.  P_spec_level gives the current nesting level.
 */
Perror_t  P_io_checkpoint  (P_t *pads, int speculative);
Perror_t  P_io_commit      (P_t *pads);
Perror_t  P_io_commit_pos  (P_t *pads, Ppos_t pos);
          /* commit at pos rather than using the current IO cursor position */
          /* pos should be >= the checkpoint position and <= the current IO cursor position */
Perror_t  P_io_restore     (P_t *pads);
unsigned int P_spec_level  (P_t *pads);

/* ================================================================================
 * REGULAR EXPRESSION SUPPORT
 *
 * PADS regular expressions support the full posix regex specification,
 * and also support many of the Perl extensions.  For the complete details,
 * see the PADS manual (not yet!).   If you have Perl installed, you can use
 *
 *    > man perlre
 *
 * to see Perl's regular expression man page.
 *
 * Here we just give some important features.
 *
 * [A] An uncompiled regular expression is specified as a string
 *     (a const char*).  The first character in the string is the
 *     expression delimeter: the next (non-espaced) occurence of
 *     this delimeter marks the end of the regular expression.
 *     We typically write our examples using slash (/) as the
 *     delimeter, but any delimeter can be used.  After the closing
 *     delimeter, one can add one or more single-character
 *     modifiers which change the normal matching behavior.  The
 *     modifies are based on those supported by Perl, and
 *     currently include:
 *
 *     l  : Treat the pattern as a literal.  All characters in the pattern are
 *          literal characters to be found in the input... there are no operators
 *          or special characters.
 *
 *     i  : Do case-insensitve pattern matching
 *
 *     x  : Extend your pattern's legibility by permitting whitespace
 *          and comments.
 *
 *          Tells the regular expression parser to ignore whitespace that
 *          is neither backslashed nor within a character class You can
 *          use this to break up your regular expression into (slightly)
 *          more readable parts.  The "#" character is also treated as a
 *          metacharacter introducing a comment.  This also means that if
 *          you want real whitespace or "#" characters in the pattern
 *          (outside a character class, where they are unaffected by
 *          "/x"), you'll either have to escape them or encode them using
 *          octal or hex escapes.  Be careful not to include the pattern
 *          delimiter in the comment -- there is no way of knowing you
 *          did not intend to close the pattern early. 
 *
 *     ?  : Minimal match.  Change from the normal maximal left-most match
 *          semantics to a minimal left-most match semantics.
 *
 *     f  : First match.  Change from the normal maximal left-most match
 *          semantics to accepting the first match found.  This may be
 *          useful for terminating regular expressions where any match
 *          is sufficient to trigger termination.  For termination, the matched
 *          characters are not included in the resulting value, so getting
 *          the best set of matching characters may not be necessary.
 *
 * It is important to note that in normal posix regexps, the '$' and '^'
 * special characters match 'beginning of line' and 'end of line' respectively,
 * where newline is the line separator character.  In contrast, in PADS regexps
 * the '$' and '^' special characters match 'beginning of record' and 'end of record'
 * respectively (and thus they only have meaning with the record-based IO 
 * disciplines).  For this reason, newlines that occur within records or within
 * input data for non-record-based input are treated as normal characters
 * with no special semantics. This means, for example, that the '.' special character
 * will match newlines.  (In Perl one would use the "/s" modifier to get similar
 * behavior.)
 *
 * ** If newlines in your input data mark record boundaries, you
 *    should be using one of the nlrec IO disciplines, in which case the newlines
 *    do not appear in your normal input, so there is no issue of '.'
 *    matching newlines, and $ and ^ will have their normal posix
 *    behavior.
 *
 * [B] Regular expressions are used for two purposes in PADS,
 * and the matching semantics with respect the current IO position
 * are different for these two cases, as follows.
 *
 *   1. A regexp can be used as the inclusive scope of a data field,
 *      i.e., it defines the set of characters that will be included
 *      in a resulting value (see Pstring_ME / Pstring_CME).
 *
 *      In this case, the regexp is implicitly left-bounded at the
 *      current IO position: if a match cannot be found that includes
 *      the character at the current IO position, then matching fails.
 *
 *      The default is that the longest such match will be used
 *      that is within the scope determined by pads->disc->match_max.
 * 
 *   2. A regexp can be used to terminate a data field
 *      (see Pstring_SE / Pstring_CSE).
 *
 *      In this case, the regexp is not 'left bounded': the
 *      matcher finds the longest match whose first->last characters
 *      occur anywhere in the scope determined by pads->disc->scan_max.
 *
 *      The resulting value consists of all characters from the current
 *      IO position up to (but not including) the left-most character
 *      in the match.  I.e., none of the characters in the match are
 *      included in the value; the match simply 'terminates' the value.
 *
 *      Example: suppose a string is either terminated by a comma
 *      or by end-of-record.  This would by specified in a PADSL description as:
 *
 *      Pstring_SE(:"/[,]|$/":)    my_string;
 * 
 * [C] Within regular expressions, one can write in brackets [] a set of
 *     characters to be matched against, or the inverse of such a set:
 *
 *         [abc]          matches an 'a', 'b', or 'c'
 *
 *         [^abc]         matches any character EXCEPT an 'a', 'b', or 'c'
 *
 *     INSIDE of one of these bracket expressions one can include a character
 *     class using the syntax [:<classname>:].  For example, the following
 *     matches either a letter ('A' through 'Z' or 'a' through 'z') or a '0' or '1':
 *
 *         [0[:alpha:]1]
 *
 *     Using character classes is preferable to writing something like this:
 *
 *         [0A-Za-z1]
 *
 *     because the letters A-Z may not occur contiguosly in all character set
 *     encodings.  Note that when you just specify a character class within 
 *     brackets, you end up with a double set of brackets, as in this pattern:
 *
 *        /[[:alpha:]]+/   : one or more alpha characters
 *    
 *     The following are all built-in character classes:
 *
 *        [:alnum:]           - alpha or digit
 *        [:alpha:]           - upper or lower alphabet character
 *        [:blank:]           - space (' ') or tab ('\t')
 *        [:cntrl:]           - control character
 *        [:digit:]           - digit (0 through 9)
 *        [:graph:]           - any printable character except space
 *        [:lower:]           - lower-case letter
 *        [:print:]           - any printable character including space
 *        [:punct:]           - any printable character which is not
 *                                a space or an alphanumeric character
 *        [:space:]           - a white-space character. Normally this
 *                                includes: space, form-feed ('\f'),
 *                                newline ('\n'), carriage return ('\r'),
 *                                horizontal tab ('\t'), and vertical tab ('\v')
 *        [:upper:]           - an upper-case letter
 *        [:word:]            - an alphanumeric character or an underscore ('_') 
 *        [:xdigit:]          - a hexadecimal digit (normal digits and A through F)
 *
 * It is possible to define your own character class in a PADSL file and then
 * use that class in regular expressions that occur later in the file.  See
 * the PADS manual for details.
 */

/* Pregexp_t: COMPILED REGULAR EXPRESSIONS
 *
 * The scan and read functions that take regular expressions as arguments
 * require pointers to compiled regular expressions, type Pregexp_t*.
 *
 * A Pregexp_t contains two things:
 *    1. a boolean, valid, which indicates whether the Pregexp_t
 *       contains a valid compiled regular expression.
 *    2. some private state (an internal represention of the compiled regular expression)
 *       which should be ignored by the users of the library.
 *
 * Here is the type decl:
 */

/* type Pregexp_t: */
struct Pregexp_s {
  int                  valid;
  P_REGEXP_T_PRIVATE_STATE;
};

/* If my_regexp.valid is non-zero, then my_regexp requires cleanup when no longer needed.
 *
 * Upon declaring a Pregexp_t, one should set valid to 0.
 * You can do this directly, as in:
 *
 *     Pregexp_t my_regexp = { 0 };
 *
 * or you can use the preferred method, which is to use the following macro:
 *
 *     P_REGEXP_DECL_NULL(my_regexp);
 *
 * When through with a Pregexp_t, one should call Pregexp_cleanup, as in:
 *
 *      Pregexp_cleanup(pads, &my_regexp);
 * 
 * to clean up any private state that may have been allocated.
 *
 * The following functions are used to compile a string into a Pregexp_t
 * and to cleanup a Pregexp_t when it is no longer needed.  They should
 * passed a pointer to a properly initialized (null or valid) Pregexp_t.
 *
 * Pregexp_compile: if regexp_str is a string containing a valid regular
 * expression, this function fills in (*regexp) and returns P_OK.
 * If the string is not a valid regular expression, it returns P_ERR.
 *
 * Pregexp_compile_cstr: like Pregexp_compile, but takes a
 * const char* argument rather than a const Pstring* argument.
 *
 * Both compile functions will perform a cleanup action if regexp->valid is
 * non-zero prior to doing the compilation, and they both set regexp->valid
 * to 0 if the compilation fails and to 1 if it succeeds.  Thus, if a 
 * only
 *
 * Note that if you use a Pregexp_t to hold more than one compiled
 * regular expression over time, you only need to call Pregexp_cleanup
 * after the final use.   Here is an example of correct usage:
 *
 *     P_REGEXP_DECL_NULL(my_regexp);
 *     ...
 *     // use my_regexp to hold regular expression /aaa/ :
 *     Pregexp_compile_cstr(pads, "/aaa/", &my_regexp);
 *     Pstring_ME_read(pads, ..., &my_regexp, ...);
 *     // done using my_regexp for /aaa/
 *
 *     // use my_regexp to hold regular expression /bbb/ :
 *     Pregexp_compile_cstr(pads, "/bbb/", &my_regexp);
 *     Pstring_ME_read(pads, ..., &my_regexp, ...);
 *     // done using my_regexp for /bbb/
 *
 *     // done using my_regexp, do a final cleanup step:
 *     Pregexp_cleanup(pads, &my_regexp);
 */

#ifdef FOR_CKIT
Perror_t Pregexp_compile(P_t *pads, const Pstring *regexp_str, Pregexp_t *regexp);
Perror_t Pregexp_compile_cstr(P_t *pads, const char *regexp_str, Pregexp_t *regexp);
Perror_t Pregexp_cleanup(P_t *pads, Pregexp_t *regexp);
#endif

/* REGULAR EXPRESSION MACROS
 * -------------------------
 * The P_RE_STRING_FROM macros convert their char or string args into
 * strings containing regular expressions that match exactly the
 * specified character or string. * The string result is in temporary
 * storage, so it should be used immediately (e.g., in a
 * Pregexp_compile_cstr call).
 *
 * P_RE_STRING_FROM_CHAR(pads, char_expr);
 *   ==> Produces a regular expression string that matches a single character.
 *       Example:  P_RE_STRING_FROM_CHAR(pads, 'a') returns string "/[a]/"
 *
 * P_RE_STRING_FROM_CSTR(pads, cstr_expr);
 *   ==> Produces a regular expression string that matches a string.
 *       Example:  P_RE_STRING_FROM_CSTR(pads, "abc") returns string "/abc/l"
 *
 * P_RE_STRING_FROM_STR(pads, pstr_expr);
 *   ==> Same as above, but takes a Pstring* rather than a const char*.
 *
 * The P_REGEXP_FROM macros do the above conversions, and then do the added step
 * of compiling the result into Pregexp my_regexp.  In each case below,
 * one can check my_regexp.valid after the macro call to check whether the result
 * is a valid compiled regular expression.
 *
 * P_REGEXP_FROM_CHAR(pads, my_regexp, char_expr);
 * P_REGEXP_FROM_CSTR(pads, my_regexp, cstr_expr);
 * P_REGEXP_FROM_STR(pads, my_regexp, pstr_expr);
 */

#ifdef FOR_CKIT
const char* P_RE_STRING_FROM_CHAR(P_t *pads, Pchar char_expr);
const char* P_RE_STRING_FROM_CSTR(P_t *pads, const char *str_expr);
const char* P_RE_STRING_FROM_STR(P_t *pads, Pstring *str_expr);

void P_REGEXP_FROM_CHAR(P_t *pads, Pregexp_t my_regexp, Pchar char_expr);
void P_REGEXP_FROM_CSTR(P_t *pads, Pregexp_t my_regexp, const char *str_expr);
void P_REGEXP_FROM_STR(P_t *pads, Pregexp_t my_regexp, Pstring *str_expr);

void P_REGEXP_DECL_NULL(Pregexp_t);
#endif

/* ================================================================================
 * MISC ROUTINES
 *
 *    P_fmt_char: produce a ptr to a string that is a pretty-print (escaped) formated for char c
 *        N.B. Resulting string should be printed immediately then not used again, e.g.,
 *        P_error(0, 0, "Missing separator: %s", P_fmt_Char(c)); 
 * 
 *    P_fmt_str    : same thing for a Pstring
 *    P_fmt_cstr   : same thing for a C string (specify a char * ptr)
 *    P_fmt_cstr_n : same thing for a C string (specify a char * ptr and a length)
 *
 *    P_qfmt_char/P_qfmt_str/P_qfmt_cstr/P_qfmt_cstr_n : same as above, but quote marks are added
 */
char *P_fmt_char(char c);
char *P_fmt_str(const Pstring *s);
char *P_fmt_cstr(const char *s);
char *P_fmt_cstr_n(const char *s, size_t len);
char *P_qfmt_char(char c);
char *P_qfmt_str(const Pstring *s);
char *P_qfmt_cstr(const char *s);
char *P_qfmt_cstr_n(const char *s, size_t len);

/*
 * P_swap_bytes: in-place memory byte order swap
 *    num_bytes should be oneof: 1, 2, 4, 8
 */
Perror_t P_swap_bytes(Pbyte *bytes, size_t num_bytes);

/*
 * Going away eventually
 */
Perror_t Pdummy_read(P_t *pads, const Pbase_m *m, Pint32 dummy_val, Pbase_pd *pd, Pint32 *res_out);

/* ================================================================================
 * USEFUL 'COMBO' MACROS
 *
 * Suppose your T.p file declares a type T, which gives you a generated T.h file
 * with types T, T_m, and T_pd.  You can write a main.c that includes:
 *
 *  #include "pads.h"
 *  #include "T.h"
 *  P_t *pads;
 *  T      t;
 *  T_m    t_m;
 *  T_pd   t_pd;
 *
 *  (first open pads handle)
 *  P_INIT_ALL(pads, T, t, t_m, t_pd, P_CheckAndSet);
 *
 * The P_INIT_ALL macro call is equivalent to writing:
 *
 *    T_init(pads, &t);
 *    T_m_init(pads, &t_m, P_CheckAndSet); 
 *    T_pd_init(pads, &t_pd);
 *
 * Similarly, the macro call P_CLEANUP_ALL(pads, T, t, t_pd) is equivalent to:
 *
 *    T_cleanup(pads, &t);
 *    T_pd_cleanup(pads, &t_pd);
 */

#define P_INIT_ALL(pads, T, t, t_m, t_pd, mask) \
do { \
  T ## _init (pads, &t); \
  T ## _m_init (pads, &t_m, mask); \
  T ## _pd_init (pads, &t_pd); \
} while (0)

#define P_CLEANUP_ALL(pads, T, t, t_pd) \
do { \
  T ## _cleanup (pads, &t); \
  T ## _pd_cleanup (pads, &t_pd); \
} while (0)

/* ================================================================================
 * INCLUDE MACRO IMPLS OF SOME OF THE FUNCTIONS DECLARED ABOVE
 */
#include "pads-impl.h"

/* ================================================================================
 * INCLUDE THE IO DISCIPLINE DECLS
 */
#include "io_disc.h"

/* ================================================================================ */

#endif  /* __PADS_H__ */
