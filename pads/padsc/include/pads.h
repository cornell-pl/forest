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

#define PDC_OK                              0
#define PDC_ERROR                          -1

#define PDC_PANIC_SKIPPED                  10

#define PDC_USER_CONSTRAINT_VIOLATION     100
#define PDC_MISSING_LITERAL               101
#define PDC_ARRAY_ELEM_ERR                110
#define PDC_ARRAY_SEP_ERR                 111
#define PDC_ARRAY_TERM_ERR                112
#define PDC_ARRAY_SIZE_ERR                113
#define PDC_ARRAY_USER_CONSTRAINT_ERR     114
#define PDC_STRUCT_FIELD_ERR              120
#define PDC_UNION_MATCH_FAILURE           130
#define PDC_ENUM_MATCH_FAILURE            140

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
 *   p_stop: panic stop 
 *             When searching for a character or string literal (or for some other
 *             target that allows resynching the input stream after a parse error),
 *             how far should the search progress?  Values currently supported:
 *                PDC_Line_Stop : stop at the first newline encountered (or EOF)
 *                PDC_EOF_Stop  : stop at EOF (no more input)
 *
 */

typedef int                        PDC_error_t;
typedef struct PDC_s               PDC_t;
typedef struct PDC_disc_s          PDC_disc_t;
typedef struct PDC_loc_s           PDC_loc_t;
typedef struct PDC_base_ed_s       PDC_base_ed;
typedef enum   PDC_panicStop_em_e  PDC_panicStop_em;
typedef enum   PDC_base_em_e       PDC_base_em;

/* ================================================================================ */
/* BASIC LIBRARY TYPES */

typedef _ast_int1_t            PDC_int8;
typedef _ast_int2_t            PDC_int16;
typedef _ast_int4_t            PDC_int32; 

typedef unsigned _ast_int1_t   PDC_uint8;
typedef unsigned _ast_int2_t   PDC_uint16;
typedef unsigned _ast_int4_t   PDC_uint32;

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

/* 
 * An error function outputs a formatted error message, where severity indicates:
 *     positive #    : warning/error (the larger the number, the greater the severity)
 *     negative # -K : debug msg at debug level K
 *
 * format is a printf-formatted string, and is followed by optional args as in printf.
 * 
 */
typedef void (*PDC_error_f)(PDC_t*, PDC_disc_t*, int severity, const char* format, ...);

/* ================================================================================ */
/* LIBRARY TYPES */

enum PDC_base_em_e { PDC_CheckAndSet, PDC_Check, PDC_Ignore };

enum PDC_panicStop_em_e { PDC_Line_Stop /* , PDC_EOF_Stop */ };    /* At the moment, only PDC_Line_Stop is supported */

struct PDC_loc_s {
  int          lineNum;
  int          posNum;
  char*        begin;
  char*        end;
};

struct PDC_base_ed_s {
  int          errCode;
  PDC_loc_t    loc;
  int          panic;
};

struct PDC_disc_s {
  PDC_error_f           errorf;
  PDC_panicStop_em      p_stop;
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
/* ASCII INTEGER READ FUNCTIONS */

PDC_error_t PDC_aint8_read(PDC_t* pdc, PDC_base_em* em,
			   PDC_base_ed* err, PDC_int8* res, PDC_disc_t* disc);

PDC_error_t PDC_aint32_read(PDC_t* pdc, PDC_base_em* em,
			    PDC_base_ed* err, PDC_int32* res, PDC_disc_t* disc);


PDC_error_t PDC_auint8_read(PDC_t* pdc, PDC_base_em* em,
			    PDC_base_ed* err, PDC_uint8* res, PDC_disc_t* disc);

PDC_error_t PDC_auint32_read(PDC_t* pdc, PDC_base_em* em,
			     PDC_base_ed* err, PDC_uint32* res, PDC_disc_t* disc);


/* ================================================================================ */

#endif  /* __LIBPADSC_H__ */
