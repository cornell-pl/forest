#ifdef _USE_PROTO
#pragma prototyped
#endif
/*
 * padc library interface -- internal functions
 * 
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#ifndef __LIBPADSC_INTERNAL__
#define __LIBPADSC_INTERNAL__

/* ================================================================================ */

#include "libpadsc.h"
#include "pdc_out_macros.h"

/* ================================================================================ */
/* NOT SURE WHERE THESE MACROS BELONG */

/* vmalloc macros:
 *   vmnewof copies existing bytes and zeroes extension
 *   vmoldof does not copy existing bytes and does not zero extension
 *   vmcpyoldof copies existing bytes but does not zero extension
 *
 * The first 2 are provided, the last one is here:
 */

#define vmcpyoldof(v,p,t,n,x) \
  (t*)vmresize((v), (p), sizeof(t)*(n)+(x), (VM_RSMOVE|VM_RSCOPY) )

#define PDCI_INTERNAL_CHARSET_SWITCH(pdc,fn_pre,fn_post,args) \
 ((pdc->disc->def_charset == PDC_charset_ASCII) \
    ?  fn_pre ## _a_ ## fn_post ## _internal args \
    :  fn_pre ## _e_ ## fn_post ## _internal args)

/* ================================================================================
 * MACROS used by generated code as well as library code
 */

#ifndef PDCI_MacroArg2String
#define PDCI_MacroArg2String(s) #s
#endif

#ifdef FOR_CKIT
/* Prototypes for CKIT */

void PDCI_DISC_INIT_CHECKS(char * prefix);
void PDCI_DISC_INIT_CHECKS_RET_0(char * prefix);
void PDCI_DISC_INIT_CHECKS_RET_VOID(char * prefix);
void PDCI_DISC_INIT_CHECKS_RET_SSIZE(char * prefix);

void PDCI_IODISC_INIT_CHECKS(char * prefix);
void PDCI_IODISC_INIT_CHECKS_RET_0(char * prefix);
void PDCI_IODISC_INIT_CHECKS_RET_VOID(char * prefix);
void PDCI_IODISC_INIT_CHECKS_RET_SSIZE(char * prefix);

void PDCI_NULLPARAM_CHECK(char *, void *);
void PDCI_NULLPARAM_CHECK_RET_0(char *, void *);
void PDCI_NULLPARAM_CHECK_RET_VOID(char *, void *);
void PDCI_NULLPARAM_CHECK_RET_SSIZE(char *, void *);

PDC_inv_valfn PDCI_GET_INV_VALFN(PDC_t *, const char *);

#else
/* The actual impls */

#define PDCI_DISC_INIT_CHECKS_RET(prefix, ret) \
  do { \
    if (!pdc)  { \
      PDC_WARN1(&PDC_default_disc, "%s: null pdc param", prefix); \
      ret; \
    } \
    if (!pdc->disc) { \
      PDC_WARN1(&PDC_default_disc, "%s: null pdc->disc", prefix); \
      ret; \
    } \
    PDC_TRACE1(pdc->disc, "%s called", prefix); \
  } while (0)

#define PDCI_DISC_INIT_CHECKS(prefix) \
     PDCI_DISC_INIT_CHECKS_RET(prefix, return PDC_ERR)

#define PDCI_DISC_INIT_CHECKS_RET_0(prefix) \
     PDCI_DISC_INIT_CHECKS_RET(prefix, return 0)

#define PDCI_DISC_INIT_CHECKS_RET_VOID(prefix) \
     PDCI_DISC_INIT_CHECKS_RET(prefix, return)

#define PDCI_DISC_INIT_CHECKS_RET_SSIZE(prefix) \
     PDCI_DISC_INIT_CHECKS_RET(prefix, return -1)

#define PDCI_IODISC_INIT_CHECKS_RET(prefix, ret) \
  do { \
    if (!pdc)  { \
      PDC_WARN1(&PDC_default_disc, "%s: null pdc param", prefix); \
      ret; \
    } \
    if (!pdc->disc) { \
      PDC_WARN1(&PDC_default_disc, "%s: null pdc->disc", prefix); \
      ret; \
    } \
    PDC_TRACE1(pdc->disc, "%s called", prefix); \
    if (!pdc->disc->io_disc) { \
      PDC_WARN1(pdc->disc, "%s: IO discipline not installed", prefix); \
      ret; \
    } \
  } while (0)

#define PDCI_IODISC_INIT_CHECKS(prefix) \
     PDCI_IODISC_INIT_CHECKS_RET(prefix, return PDC_ERR)

#define PDCI_IODISC_INIT_CHECKS_RET_0(prefix) \
     PDCI_IODISC_INIT_CHECKS_RET(prefix, return 0)

#define PDCI_IODISC_INIT_CHECKS_RET_VOID(prefix) \
     PDCI_IODISC_INIT_CHECKS_RET(prefix, return)

#define PDCI_IODISC_INIT_CHECKS_RET_SSIZE(prefix) \
     PDCI_IODISC_INIT_CHECKS_RET(prefix, return -1)

/* Assumes pdc and disc already checked */
#define PDCI_NULLPARAM_CHECK_RET(prefix, param, ret) \
  do { \
    if (!(param))  { \
      PDC_WARN1(pdc->disc, "%s: param " PDCI_MacroArg2String(param) " must not be NULL", prefix); \
      ret; \
    } \
  } while (0)

#define PDCI_NULLPARAM_CHECK(prefix, param) \
     PDCI_NULLPARAM_CHECK_RET(prefix, param, return PDC_ERR)

#define PDCI_NULLPARAM_CHECK_RET_0(prefix, param) \
     PDCI_NULLPARAM_CHECK_RET(prefix, param, return 0)

#define PDCI_NULLPARAM_CHECK_RET_VOID(prefix, param) \
     PDCI_NULLPARAM_CHECK_RET(prefix, param, return)

#define PDCI_NULLPARAM_CHECK_RET_SSIZE(prefix, param) \
     PDCI_NULLPARAM_CHECK_RET(prefix, param, return -1)

#define PDCI_GET_INV_VALFN(pdc,type_name) \
  (pdc->disc->inv_valfn_map ? PDC_get_inv_valfn_internal(pdc, pdc->disc->inv_valfn_map, type_name) : 0)

#endif /* FOR_CKIT */

/* ================================================================================ */
/* INTERNAL TYPE DEFINITIONS */

/* PDCI_stkElt_t: A stack elt has a cursor position cur, which is a
 * pointer to a PDC_IO_elt plus the number of byte remaining.  We also
 * record the spec flag passed to PDC_IO_checkpoint, to enable proper
 * de-bumping of pdc->speclev.
 */

/* type PDCI_stkElt_t: */
struct PDCI_stkElt_s {
  PDC_IO_elt_t  *elt;
  size_t        remain;  /* bytes remaining in current IO elt; determines cursor position */
  int           spec;    /* the spec flag passed to checkpoint */
};

/* ================================================================================ */
/* INTERNAL VERSIONS OF invalid_valfn FUNCTIONS */

#ifdef FOR_CKIT
/* Prototypes for CKIT */
PDC_inv_valfn PDC_get_inv_valfn_internal(PDC_t* pdc, PDC_inv_valfn_map_t *map, const char *type_name); 
PDC_inv_valfn PDC_set_inv_valfn_internal(PDC_t* pdc, PDC_inv_valfn_map_t *map, const char *type_name, PDC_inv_valfn fn);

PDC_inv_valfn_map_t* PDC_inv_valfn_map_create_internal(PDC_t *pdc);
PDC_error_t          PDC_inv_valfn_map_destroy_internal(PDC_t *pdc, PDC_inv_valfn_map_t *map);
#else
/* The actual impls */

#define PDC_get_inv_valfn_internal(pdc, map, type_name) \
  PDCI_get_inv_valfn(pdc, map, type_name, 1)

#define PDC_set_inv_valfn_internal(pdc, map, type_name, fn) \
  PDCI_set_inv_valfn(pdc, map, type_name, fn, 0)

#define PDC_inv_valfn_map_create_internal(pdc) \
  PDCI_inv_valfn_map_create(pdc, 1)

#define PDC_inv_valfn_map_destroy_internal(pdc, map) \
  PDCI_inv_valfn_map_destroy(pdc, map, 1)

#endif

/* ================================================================================ */
/* INTERNAL VERSIONS OF EXTERNAL IO FUNCTIONS */

PDC_error_t  PDC_IO_set_internal          (PDC_t *pdc, Sfio_t *io);
PDC_error_t  PDC_IO_fopen_internal        (PDC_t *pdc, const char *path);
PDC_error_t  PDC_IO_close_internal        (PDC_t *pdc);
PDC_error_t  PDC_IO_next_rec_internal     (PDC_t *pdc, size_t *skipped_bytes_out);

int          PDC_IO_at_EOR_internal       (PDC_t *pdc);
int          PDC_IO_at_EOF_internal       (PDC_t *pdc);

PDC_error_t  PDC_IO_getPos_internal       (PDC_t *pdc, PDC_pos_t *pos, int offset); 

/* These do not have the same API as external versions; they take a whatfn arg */

PDC_byte*    PDCI_IO_write_start  (PDC_t *pdc, Sfio_t *io, size_t *buf_len, int *set_buf, const char *whatfn);
ssize_t      PDCI_IO_write_commit (PDC_t *pdc, Sfio_t *io, PDC_byte *buf, int set_buf, size_t num_bytes, const char *whatfn);
void         PDCI_IO_write_abort  (PDC_t *pdc, Sfio_t *io, PDC_byte *buf, int set_buf, const char *whatfn);

ssize_t      PDCI_IO_rec_write2io        (PDC_t *pdc, Sfio_t *io, PDC_byte *buf, size_t rec_data_len, const char *whatfn);
ssize_t      PDCI_IO_rec_open_write2buf  (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, const char *whatfn);
ssize_t      PDCI_IO_rec_close_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full,
					  PDC_byte *rec_start, size_t num_bytes, const char *whatfn);

ssize_t      PDCI_IO_rblk_write2io       (PDC_t *pdc, Sfio_t *io, PDC_byte *buf, size_t blk_data_len, PDC_uint32 num_recs,
					  const char *whatfn);
ssize_t      PDCI_IO_rblk_open_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, const char *whatfn);
ssize_t      PDCI_IO_rblk_close_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full,
					  PDC_byte *blk_start, size_t num_bytes, PDC_uint32 num_recs, const char *whatfn);

/* ================================================================================ */ 
/* INTERNAL VERSIONS OF SCAN FUNCTIONS */

#ifdef FOR_CKIT
/* Prototypes for CKIT */

PDC_error_t PDC_char_lit_scan_internal(PDC_t *pdc, PDC_char c, PDC_char s, int eat_lit,
				       PDC_char *c_out, size_t *offset_out);
PDC_error_t PDC_a_char_lit_scan_internal(PDC_t *pdc, PDC_char c, PDC_char s, int eat_lit,
					 PDC_char *c_out, size_t *offset_out);
PDC_error_t PDC_e_char_lit_scan_internal(PDC_t *pdc, PDC_char c, PDC_char s, int eat_lit,
					 PDC_char *c_out, size_t *offset_out);
PDC_error_t PDC_str_lit_scan_internal(PDC_t *pdc, const PDC_string *findStr, const PDC_string *stopStr, int eat_lit,
				      PDC_string **str_out, size_t *offset_out);
PDC_error_t PDC_a_str_lit_scan_internal(PDC_t *pdc, const PDC_string *findStr, const PDC_string *stopStr, int eat_lit,
					PDC_string **str_out, size_t *offset_out);
PDC_error_t PDC_e_str_lit_scan_internal(PDC_t *pdc, const PDC_string *findStr, const PDC_string *stopStr, int eat_lit,
					PDC_string **str_out, size_t *offset_out);
PDC_error_t PDC_Cstr_lit_scan_internal(PDC_t *pdc, const char *findStr, const char *stopStr, int eat_lit,
				       const char **str_out, size_t *offset_out);
PDC_error_t PDC_a_Cstr_lit_scan_internal(PDC_t *pdc, const char *findStr, const char *stopStr, int eat_lit,
					 const char **str_out, size_t *offset_out);
PDC_error_t PDC_e_Cstr_lit_scan_internal(PDC_t *pdc, const char *findStr, const char *stopStr, int eat_lit,
					 const char **str_out, size_t *offset_out);
#else
/* The actual impls */

#define PDC_char_lit_scan_internal(pdc, c, s, eat_lit, c_out, offset_out) \
          PDCI_char_lit_scan(pdc, c, s, eat_lit, c_out, offset_out, pdc->disc->def_charset, ,"PDC_char_lit_scan", 0)

#define PDC_a_char_lit_scan_internal(pdc, c, s, eat_lit, c_out, offset_out) \
          PDCI_char_lit_scan(pdc, c, s, eat_lit, c_out, offset_out, PDC_charset_ASCII, "PDC_a_char_lit_scan", 0)

#define PDC_e_char_lit_scan_internal(pdc, c, s, eat_lit, c_out, offset_out) \
          PDCI_char_lit_scan(pdc, c, s, eat_lit, c_out, offset_out, PDC_charset_EBCDIC, "PDC_e_char_lit_scan", 0)

#define PDC_str_lit_scan_internal(pdc, findStr, stopStr, eat_lit, str_out, offset_out) \
          PDCI_str_lit_scan(pdc, findStr, stopStr, eat_lit, str_out, offset_out, pdc->disc->def_charset, "PDC_str_lit_scan", 0)

#define PDC_a_str_lit_scan_internal(pdc, findStr, stopStr, eat_lit, str_out, offset_out) \
          PDCI_str_lit_scan(pdc, findStr, stopStr, eat_lit, str_out, offset_out, PDC_charset_ASCII, "PDC_a_str_lit_scan", 0)

#define PDC_e_str_lit_scan_internal(pdc, findStr, stopStr, eat_lit, str_out, offset_out) \
          PDCI_str_lit_scan(pdc, findStr, stopStr, eat_lit, str_out, offset_out, PDC_charset_EBCDIC, "PDC_e_str_lit_scan", 0)

#define PDC_Cstr_lit_scan_internal(pdc, findStr, stopStr, eat_lit, str_out, offset_out) \
          PDCI_Cstr_lit_scan(pdc, findStr, stopStr, eat_lit, str_out, offset_out, pdc->disc->def_charset, "PDC_Cstr_lit_scan", 0)


#define PDC_a_Cstr_lit_scan_internal(pdc, findStr, stopStr, eat_lit, str_out, offset_out) \
          (pdc, findStr, stopStr, eat_lit, str_out, offset_out, PDC_charset_ASCII, "PDC_a_Cstr_lit_scan", 0)

#define PDC_e_Cstr_lit_scan_internal(pdc, findStr, stopStr, eat_lit, str_out, offset_out) \
          PDCI_Cstr_lit_scan(pdc, findStr, stopStr, eat_lit, str_out, offset_out, PDC_charset_EBCDIC, "PDC_e_Cstr_lit_scan", 0)

#endif /* FOR_CKIT */

/* ================================================================================ */ 
/* INTERNAL VERSIONS OF ALL BASE TYPE READ FUNCTIONS */

#ifdef FOR_CKIT
/* Prototypes for CKIT */

PDC_error_t PDC_char_lit_read_internal(PDC_t *pdc, const PDC_base_csm *csm,
				       PDC_base_ed *ed, PDC_char c);
PDC_error_t PDC_a_char_lit_read_internal(PDC_t *pdc, const PDC_base_csm *csm,
					 PDC_base_ed *ed, PDC_char c);
PDC_error_t PDC_e_char_lit_read_internal(PDC_t *pdc, const PDC_base_csm *csm,
					 PDC_base_ed *ed, PDC_char c);
PDC_error_t PDC_str_lit_read_internal(PDC_t *pdc, const PDC_base_csm *csm,
				      PDC_base_ed *ed, const PDC_string *s);
PDC_error_t PDC_a_str_lit_read_internal(PDC_t *pdc, const PDC_base_csm *csm,
					PDC_base_ed *ed, const PDC_string *s);
PDC_error_t PDC_e_str_lit_read_internal(PDC_t *pdc, const PDC_base_csm *csm,
					PDC_base_ed *ed, const PDC_string *s);
PDC_error_t PDC_Cstr_lit_read_internal(PDC_t *pdc, const PDC_base_csm *csm,
				      PDC_base_ed *ed, const char *s);
PDC_error_t PDC_a_Cstr_lit_read_internal(PDC_t *pdc, const PDC_base_csm *csm,
					PDC_base_ed *ed, const char *s);
PDC_error_t PDC_e_Cstr_lit_read_internal(PDC_t *pdc, const PDC_base_csm *csm,
					PDC_base_ed *ed, const char *s);
PDC_error_t PDC_countX_internal(PDC_t *pdc, const PDC_base_csm *csm, PDC_uint8 x, int eor_required,
				PDC_base_ed *ed, PDC_int32 *res_out);
PDC_error_t PDC_a_countX_internal(PDC_t *pdc, const PDC_base_csm *csm, PDC_uint8 x, int eor_required,
				  PDC_base_ed *ed, PDC_int32 *res_out);
PDC_error_t PDC_e_countX_internal(PDC_t *pdc, const PDC_base_csm *csm, PDC_uint8 x, int eor_required,
				  PDC_base_ed *ed, PDC_int32 *res_out);
PDC_error_t PDC_countXtoY_internal(PDC_t *pdc, const PDC_base_csm *csm, PDC_uint8 x, PDC_uint8 y,
				   PDC_base_ed *ed, PDC_int32 *res_out);
PDC_error_t PDC_a_countXtoY_internal(PDC_t *pdc, const PDC_base_csm *csm, PDC_uint8 x, PDC_uint8 y,
				     PDC_base_ed *ed, PDC_int32 *res_out);
PDC_error_t PDC_e_countXtoY_internal(PDC_t *pdc, const PDC_base_csm *csm, PDC_uint8 x, PDC_uint8 y,
				     PDC_base_ed *ed, PDC_int32 *res_out);
PDC_error_t PDC_date_read_internal(PDC_t *pdc, const PDC_base_csm *csm, PDC_char stopChar,
				   PDC_base_ed *ed, PDC_uint32 *res_out);
PDC_error_t PDC_a_date_read_internal(PDC_t *pdc, const PDC_base_csm *csm, PDC_char stopChar,
				     PDC_base_ed *ed, PDC_uint32 *res_out);
PDC_error_t PDC_e_date_read_internal(PDC_t *pdc, const PDC_base_csm *csm, PDC_char stopChar,
				     PDC_base_ed *ed, PDC_uint32 *res_out);
PDC_error_t PDC_char_read_internal (PDC_t *pdc, const PDC_base_csm *csm, PDC_base_ed *ed, PDC_char *c_out);
PDC_error_t PDC_a_char_read_internal (PDC_t *pdc, const PDC_base_csm *csm, PDC_base_ed *ed, PDC_char *c_out);
PDC_error_t PDC_e_char_read_internal (PDC_t *pdc, const PDC_base_csm *csm, PDC_base_ed *ed, PDC_char *c_out);
PDC_error_t PDC_string_FW_read_internal(PDC_t *pdc, const PDC_base_csm *csm, size_t width,
					PDC_base_ed *ed, PDC_string *s_out);
PDC_error_t PDC_a_string_FW_read_internal(PDC_t *pdc, const PDC_base_csm *csm, size_t width,
					  PDC_base_ed *ed, PDC_string *s_out);
PDC_error_t PDC_e_string_FW_read_internal(PDC_t *pdc, const PDC_base_csm *csm, size_t width,
					  PDC_base_ed *ed, PDC_string *s_out);
PDC_error_t PDC_string_read_internal(PDC_t *pdc, const PDC_base_csm *csm, PDC_char stopChar,
				     PDC_base_ed *ed, PDC_string *s_out);
PDC_error_t PDC_a_string_read_internal(PDC_t *pdc, const PDC_base_csm *csm, PDC_char stopChar,
				       PDC_base_ed *ed, PDC_string *s_out);
PDC_error_t PDC_e_string_read_internal(PDC_t *pdc, const PDC_base_csm *csm, PDC_char stopChar,
				       PDC_base_ed *ed, PDC_string *s_out);
PDC_error_t PDC_string_ME_read_internal(PDC_t *pdc, const PDC_base_csm *csm, const char *matchRegexp,
					PDC_base_ed *ed, PDC_string *s_out);
PDC_error_t PDC_a_string_ME_read_internal(PDC_t *pdc, const PDC_base_csm *csm, const char *matchRegexp,
					  PDC_base_ed *ed, PDC_string *s_out);
PDC_error_t PDC_e_string_ME_read_internal(PDC_t *pdc, const PDC_base_csm *csm, const char *matchRegexp,
					  PDC_base_ed *ed, PDC_string *s_out);
PDC_error_t PDC_string_CME_read_internal(PDC_t *pdc, const PDC_base_csm *csm, PDC_regexp_t *matchRegexp,
					 PDC_base_ed *ed, PDC_string *s_out);
PDC_error_t PDC_a_string_CME_read_internal(PDC_t *pdc, const PDC_base_csm *csm, PDC_regexp_t *matchRegexp,
					   PDC_base_ed *ed, PDC_string *s_out);
PDC_error_t PDC_e_string_CME_read_internal(PDC_t *pdc, const PDC_base_csm *csm, PDC_regexp_t *matchRegexp,
					   PDC_base_ed *ed, PDC_string *s_out);
PDC_error_t PDC_string_SE_read_internal(PDC_t *pdc, const PDC_base_csm *csm, const char *stopRegexp,
					PDC_base_ed *ed, PDC_string *s_out);
PDC_error_t PDC_a_string_SE_read_internal(PDC_t *pdc, const PDC_base_csm *csm, const char *stopRegexp,
					  PDC_base_ed *ed, PDC_string *s_out);
PDC_error_t PDC_e_string_SE_read_internal(PDC_t *pdc, const PDC_base_csm *csm, const char *stopRegexp,
					  PDC_base_ed *ed, PDC_string *s_out);
PDC_error_t PDC_string_CSE_read_internal(PDC_t *pdc, const PDC_base_csm *csm, PDC_regexp_t *stopRegexp,
					 PDC_base_ed *ed, PDC_string *s_out);
PDC_error_t PDC_a_string_CSE_read_internal(PDC_t *pdc, const PDC_base_csm *csm, PDC_regexp_t *stopRegexp,
					   PDC_base_ed *ed, PDC_string *s_out);
PDC_error_t PDC_e_string_CSE_read_internal(PDC_t *pdc, const PDC_base_csm *csm, PDC_regexp_t *stopRegexp,
					   PDC_base_ed *ed, PDC_string *s_out);
PDC_error_t PDC_int8_read_internal (PDC_t *pdc, const PDC_base_csm *csm,
				    PDC_base_ed *ed, PDC_int8 *res_out);
PDC_error_t PDC_int16_read_internal(PDC_t *pdc, const PDC_base_csm *csm,
				    PDC_base_ed *ed, PDC_int16 *res_out);
PDC_error_t PDC_int32_read_internal(PDC_t *pdc, const PDC_base_csm *csm,
				    PDC_base_ed *ed, PDC_int32 *res_out);
PDC_error_t PDC_int64_read_internal(PDC_t *pdc, const PDC_base_csm *csm,
				    PDC_base_ed *ed, PDC_int64 *res_out);
PDC_error_t PDC_uint8_read_internal (PDC_t *pdc, const PDC_base_csm *csm,
				     PDC_base_ed *ed, PDC_uint8 *res_out);
PDC_error_t PDC_uint16_read_internal(PDC_t *pdc, const PDC_base_csm *csm,
				     PDC_base_ed *ed, PDC_uint16 *res_out);
PDC_error_t PDC_uint32_read_internal(PDC_t *pdc, const PDC_base_csm *csm,
				     PDC_base_ed *ed, PDC_uint32 *res_out);
PDC_error_t PDC_uint64_read_internal(PDC_t *pdc, const PDC_base_csm *csm,
				     PDC_base_ed *ed, PDC_uint64 *res_out);
PDC_error_t PDC_int8_FW_read_internal (PDC_t *pdc, const PDC_base_csm *csm, size_t width,
				       PDC_base_ed *ed, PDC_int8 *res_out);
PDC_error_t PDC_int16_FW_read_internal(PDC_t *pdc, const PDC_base_csm *csm, size_t width,
				       PDC_base_ed *ed, PDC_int16 *res_out);
PDC_error_t PDC_int32_FW_read_internal(PDC_t *pdc, const PDC_base_csm *csm, size_t width,
				       PDC_base_ed *ed, PDC_int32 *res_out);
PDC_error_t PDC_int64_FW_read_internal(PDC_t *pdc, const PDC_base_csm *csm, size_t width,
				       PDC_base_ed *ed, PDC_int64 *res_out);
PDC_error_t PDC_uint8_FW_read_internal (PDC_t *pdc, const PDC_base_csm *csm, size_t width,
					PDC_base_ed *ed, PDC_uint8 *res_out);
PDC_error_t PDC_uint16_FW_read_internal(PDC_t *pdc, const PDC_base_csm *csm, size_t width,
					PDC_base_ed *ed, PDC_uint16 *res_out);
PDC_error_t PDC_uint32_FW_read_internal(PDC_t *pdc, const PDC_base_csm *csm, size_t width,
					PDC_base_ed *ed, PDC_uint32 *res_out);
PDC_error_t PDC_uint64_FW_read_internal(PDC_t *pdc, const PDC_base_csm *csm, size_t width,
					PDC_base_ed *ed, PDC_uint64 *res_out);
#else
/* The actual impls */

#define PDC_char_lit_read_internal(pdc, csm, ed, c) \
          PDCI_char_lit_read(pdc, csm, ed, c, pdc->disc->def_charset, "PDC_char_lit_read", 0)

#define PDC_a_char_lit_read_internal(pdc, csm, ed, c) \
          PDCI_char_lit_read(pdc, csm, ed, c, PDC_charset_ASCII, "PDC_a_char_lit_read", 0)

#define PDC_e_char_lit_read_internal(pdc, csm, ed, c) \
          PDCI_char_lit_read(pdc, csm, ed, c, PDC_charset_EBCDIC, "PDC_e_char_lit_read", 0)

#define PDC_str_lit_read_internal(pdc, csm, ed, s) \
          PDCI_str_lit_read(pdc, csm, ed, s, pdc->disc->def_charset, "PDC_str_lit_read", 0)

#define PDC_a_str_lit_read_internal(pdc, csm, ed, s) \
          PDCI_str_lit_read(pdc, csm, ed, s, PDC_charset_ASCII, "PDC_a_str_lit_read", 0)

#define PDC_e_str_lit_read_internal(pdc, csm, ed, s) \
          PDCI_str_lit_read(pdc, csm, ed, s, PDC_charset_EBCDIC, "PDC_e_str_lit_read", 0)

#define PDC_Cstr_lit_read_internal(pdc, csm, ed, s) \
          PDCI_Cstr_lit_read(pdc, csm, ed, s, pdc->disc->def_charset, "PDC_Cstr_lit_read", 0)

#define PDC_a_Cstr_lit_read_internal(pdc, csm, ed, s) \
          PDCI_Cstr_lit_read(pdc, csm, ed, s, PDC_charset_ASCII, "PDC_a_Cstr_lit_read", 0)

#define PDC_e_Cstr_lit_read_internal(pdc, csm, ed, s) \
          PDCI_Cstr_lit_read(pdc, csm, ed, s, PDC_charset_EBCDIC, "PDC_e_Cstr_lit_read", 0)

#define PDC_countX_internal(pdc, csm, x, eor_required, ed, res_out) \
          PDCI_countX(pdc, csm, x, eor_required, ed, res_out, pdc->disc->def_charset, "PDC_countX", 0)

#define PDC_a_countX_internal(pdc, csm, x, eor_required, ed, res_out) \
          PDCI_countX(pdc, csm, x, eor_required, ed, res_out, PDC_charset_ASCII, "PDC_a_countX", 0)

#define PDC_e_countX_internal(pdc, csm, x, eor_required, ed, res_out) \
          PDCI_countX(pdc, csm, x, eor_required, ed, res_out, PDC_charset_EBCDIC, "PDC_e_countX", 0)


#define PDC_countXtoY_internal(pdc, csm, x, y, ed, res_out) \
          PDCI_countXtoY(pdc, csm, x, y, ed, res_out, pdc->disc->def_charset, "PDC_countXtoY", 0)

#define PDC_a_countXtoY_internal(pdc, csm, x, y, ed, res_out) \
          PDCI_countXtoY(pdc, csm, x, y, ed, res_out, PDC_charset_ASCII, "PDC_a_countXtoY", 0)

#define PDC_e_countXtoY_internal(pdc, csm, x, y, ed, res_out) \
          PDCI_countXtoY(pdc, csm, x, y, ed, res_out, PDC_charset_EBCDIC, "PDC_e_countXtoY", 0)


#define PDC_date_read_internal(pdc, csm, stopChar, ed, res_out) \
          PDCI_date_read(pdc, csm, stopChar, ed, res_out, pdc->disc->def_charset, "PDC_date_read", 0)

#define PDC_a_date_read_internal(pdc, csm, stopChar, ed, res_out) \
          PDCI_date_read(pdc, csm, stopChar, ed, res_out, PDC_charset_ASCII, "PDC_a_date_read", 0)

#define PDC_e_date_read_internal(pdc, csm, stopChar, ed, res_out) \
          PDCI_date_read(pdc, csm, stopChar, ed, res_out, PDC_charset_EBCDIC, "PDC_e_date_read", 0)


#define PDC_char_read_internal(pdc, csm, ed, c_out) \
          PDCI_char_read(pdc, csm, ed, c_out, pdc->disc->def_charset, "PDC_char_read", 0)

#define PDC_a_char_read_internal(pdc, csm, ed, c_out) \
          PDCI_char_read(pdc, csm, ed, c_out, PDC_charset_ASCII, "PDC_a_char_read", 0)

#define PDC_e_char_read_internal(pdc, csm, ed, c_out) \
          PDCI_char_read(pdc, csm, ed, c_out, PDC_charset_EBCDIC, "PDC_e_char_read", 0)


#define PDC_string_FW_read_internal(pdc, csm, width, ed, s_out) \
          PDCI_string_FW_read(pdc, csm, width, ed, s_out, pdc->disc->def_charset, "PDC_string_FW_read", 0)

#define PDC_a_string_FW_read_internal(pdc, csm, width, ed, s_out) \
          PDCI_string_FW_read(pdc, csm, width, ed, s_out, PDC_charset_ASCII, "PDC_a_string_FW_read", 0)

#define PDC_e_string_FW_read_internal(pdc, csm, width, ed, s_out) \
          PDCI_string_FW_read(pdc, csm, width, ed, s_out, PDC_charset_EBCDIC, "PDC_e_string_FW_read", 0)


#define PDC_string_read_internal(pdc, csm, stopChar, ed, s_out) \
          PDCI_string_read(pdc, csm, stopChar, ed, s_out, pdc->disc->def_charset, "PDC_string_read", 0)

#define PDC_a_string_read_internal(pdc, csm, stopChar, ed, s_out) \
          PDCI_string_read(pdc, csm, stopChar, ed, s_out, PDC_charset_ASCII, "PDC_a_string_read", 0)

#define PDC_e_string_read_internal(pdc, csm, stopChar, ed, s_out) \
          PDCI_string_read(pdc, csm, stopChar, ed, s_out, PDC_charset_EBCDIC, "PDC_e_string_read", 0)


#define PDC_string_ME_read_internal(pdc, csm, matchRegexp, ed, s_out) \
          PDCI_string_ME_read(pdc, csm, matchRegexp, ed, s_out, pdc->disc->def_charset, "PDC_string_ME_read", 0)

#define PDC_a_string_ME_read_internal(pdc, csm, matchRegexp, ed, s_out) \
          PDCI_string_ME_read(pdc, csm, matchRegexp, ed, s_out, PDC_charset_ASCII, "PDC_a_string_ME_read", 0)

#define PDC_e_string_ME_read_internal(pdc, csm, matchRegexp, ed, s_out) \
          PDCI_string_ME_read(pdc, csm, matchRegexp, ed, s_out, PDC_charset_EBCDIC, "PDC_e_string_ME_read", 0)


#define PDC_string_CME_read_internal(pdc, csm, matchRegexp, ed, s_out) \
          PDCI_string_CME_read(pdc, csm, matchRegexp, ed, s_out, pdc->disc->def_charset, "PDC_string_CME_read", 0)

#define PDC_a_string_CME_read_internal(pdc, csm, matchRegexp, ed, s_out) \
          PDCI_string_CME_read(pdc, csm, matchRegexp, ed, s_out, PDC_charset_ASCII, "PDC_a_string_CME_read", 0)

#define PDC_e_string_CME_read_internal(pdc, csm, matchRegexp, ed, s_out) \
          PDCI_string_CME_read(pdc, csm, matchRegexp, ed, s_out, PDC_charset_EBCDIC, "PDC_e_string_CME_read", 0)


#define PDC_string_SE_read_internal(pdc, csm, stopRegexp, ed, s_out) \
          PDCI_string_SE_read(pdc, csm, stopRegexp, ed, s_out, pdc->disc->def_charset, "PDC_string_SE_read", 0)

#define PDC_a_string_SE_read_internal(pdc, csm, stopRegexp, ed, s_out) \
          PDCI_string_SE_read(pdc, csm, stopRegexp, ed, s_out, PDC_charset_ASCII, "PDC_a_string_SE_read", 0)

#define PDC_e_string_SE_read_internal(pdc, csm, stopRegexp, ed, s_out) \
          PDCI_string_SE_read(pdc, csm, stopRegexp, ed, s_out, PDC_charset_EBCDIC, "PDC_e_string_SE_read", 0)


#define PDC_string_CSE_read_internal(pdc, csm, stopRegexp, ed, s_out) \
          PDCI_string_CSE_read(pdc, csm, stopRegexp, ed, s_out, pdc->disc->def_charset, "PDC_string_CSE_read", 0)

#define PDC_a_string_CSE_read_internal(pdc, csm, stopRegexp, ed, s_out) \
          PDCI_string_CSE_read(pdc, csm, stopRegexp, ed, s_out, PDC_charset_ASCII, "PDC_a_string_CSE_read", 0)

#define PDC_e_string_CSE_read_internal(pdc, csm, stopRegexp, ed, s_out) \
          PDCI_string_CSE_read(pdc, csm, stopRegexp, ed, s_out, PDC_charset_EBCDIC, "PDC_e_string_CSE_read", 0)

/* def_charset read functions */

#define PDC_int8_read_internal(pdc, csm, ed, res_out) \
  PDCI_INTERNAL_CHARSET_SWITCH(pdc, PDC, int8_read, (pdc, csm, ed, res_out))

#define PDC_int16_read_internal(pdc, csm, ed, res_out) \
  PDCI_INTERNAL_CHARSET_SWITCH(pdc, PDC, int16_read, (pdc, csm, ed, res_out))

#define PDC_int32_read_internal(pdc, csm, ed, res_out) \
  PDCI_INTERNAL_CHARSET_SWITCH(pdc, PDC, int32_read, (pdc, csm, ed, res_out))

#define PDC_int64_read_internal(pdc, csm, ed, res_out) \
  PDCI_INTERNAL_CHARSET_SWITCH(pdc, PDC, int64_read, (pdc, csm, ed, res_out))

#define PDC_uint8_read_internal(pdc, csm, ed, res_out) \
  PDCI_INTERNAL_CHARSET_SWITCH(pdc, PDC, uint8_read, (pdc, csm, ed, res_out))

#define PDC_uint16_read_internal(pdc, csm, ed, res_out) \
  PDCI_INTERNAL_CHARSET_SWITCH(pdc, PDC, uint16_read, (pdc, csm, ed, res_out))

#define PDC_uint32_read_internal(pdc, csm, ed, res_out) \
  PDCI_INTERNAL_CHARSET_SWITCH(pdc, PDC, uint32_read, (pdc, csm, ed, res_out))

#define PDC_uint64_read_internal(pdc, csm, ed, res_out) \
  PDCI_INTERNAL_CHARSET_SWITCH(pdc, PDC, uint64_read, (pdc, csm, ed, res_out))

#define PDC_int8_FW_read_internal(pdc, csm, width, ed, res_out) \
  PDCI_INTERNAL_CHARSET_SWITCH(pdc, PDC, int8_FW_read, (pdc, csm, width, ed, res_out))

#define PDC_int16_FW_read_internal(pdc, csm, width, ed, res_out) \
  PDCI_INTERNAL_CHARSET_SWITCH(pdc, PDC, int16_FW_read, (pdc, csm, width, ed, res_out))

#define PDC_int32_FW_read_internal(pdc, csm, width, ed, res_out) \
  PDCI_INTERNAL_CHARSET_SWITCH(pdc, PDC, int32_FW_read, (pdc, csm, width, ed, res_out))

#define PDC_int64_FW_read_internal(pdc, csm, width, ed, res_out) \
  PDCI_INTERNAL_CHARSET_SWITCH(pdc, PDC, int64_FW_read, (pdc, csm, width, ed, res_out))

#define PDC_uint8_FW_read_internal(pdc, csm, width, ed, res_out) \
  PDCI_INTERNAL_CHARSET_SWITCH(pdc, PDC, uint8_FW_read, (pdc, csm, width, ed, res_out))

#define PDC_uint16_FW_read_internal(pdc, csm, width, ed, res_out) \
  PDCI_INTERNAL_CHARSET_SWITCH(pdc, PDC, uint16_FW_read, (pdc, csm, width, ed, res_out))

#define PDC_uint32_FW_read_internal(pdc, csm, width, ed, res_out) \
  PDCI_INTERNAL_CHARSET_SWITCH(pdc, PDC, uint32_FW_read, (pdc, csm, width, ed, res_out))

#define PDC_uint64_FW_read_internal(pdc, csm, width, ed, res_out) \
  PDCI_INTERNAL_CHARSET_SWITCH(pdc, PDC, uint64_FW_read, (pdc, csm, width, ed, res_out))

#endif /* FOR_CKIT */


/* ================================================================================ */ 
/* HELPER MACRO TO DECLARE FAMILY OF FUNCTIONS */
/* N.B. First you must declare PDCI_FIRST_ARGS and PDCI_LAST_ARGS.  Follow macro with a semi */

#define PDCI_DECL_FAMILY(ret_type, fn_prefix, typ, fn_suffix, lastnm) \
ret_type fn_prefix ## typ ## 8 ## fn_suffix (PDCI_FIRST_ARGS, PDC_ ## typ ## 8 *lastnm PDCI_LAST_ARGS); \
ret_type fn_prefix ## typ ## 16 ## fn_suffix(PDCI_FIRST_ARGS, PDC_ ## typ ## 16 *lastnm PDCI_LAST_ARGS); \
ret_type fn_prefix ## typ ## 32 ## fn_suffix(PDCI_FIRST_ARGS, PDC_ ## typ ## 32 *lastnm PDCI_LAST_ARGS); \
ret_type fn_prefix ## typ ## 64 ## fn_suffix(PDCI_FIRST_ARGS, PDC_ ## typ  ## 64 *lastnm PDCI_LAST_ARGS); \
ret_type fn_prefix ## u ## typ ## 8 ## fn_suffix (PDCI_FIRST_ARGS, PDC_u ## typ ## 8 *lastnm PDCI_LAST_ARGS); \
ret_type fn_prefix ## u ## typ ## 16 ## fn_suffix(PDCI_FIRST_ARGS, PDC_u ## typ ## 16 *lastnm PDCI_LAST_ARGS); \
ret_type fn_prefix ## u ## typ ## 32 ## fn_suffix(PDCI_FIRST_ARGS, PDC_u ## typ ## 32 *lastnm PDCI_LAST_ARGS); \
ret_type fn_prefix ## u ## typ ## 64 ## fn_suffix(PDCI_FIRST_ARGS, PDC_u ## typ ## 64 *lastnm PDCI_LAST_ARGS) \

#define PDCI_DECL_FAMILY_LASTCONST(ret_type, fn_prefix, typ, fn_suffix, lastnm) \
ret_type fn_prefix ## typ ## 8 ## fn_suffix (PDCI_FIRST_ARGS, const PDC_ ## typ ## 8 *lastnm PDCI_LAST_ARGS); \
ret_type fn_prefix ## typ ## 16 ## fn_suffix(PDCI_FIRST_ARGS, const PDC_ ## typ ## 16 *lastnm PDCI_LAST_ARGS); \
ret_type fn_prefix ## typ ## 32 ## fn_suffix(PDCI_FIRST_ARGS, const PDC_ ## typ ## 32 *lastnm PDCI_LAST_ARGS); \
ret_type fn_prefix ## typ ## 64 ## fn_suffix(PDCI_FIRST_ARGS, const PDC_ ## typ  ## 64 *lastnm PDCI_LAST_ARGS); \
ret_type fn_prefix ## u ## typ ## 8 ## fn_suffix (PDCI_FIRST_ARGS, const PDC_u ## typ ## 8 *lastnm PDCI_LAST_ARGS); \
ret_type fn_prefix ## u ## typ ## 16 ## fn_suffix(PDCI_FIRST_ARGS, const PDC_u ## typ ## 16 *lastnm PDCI_LAST_ARGS); \
ret_type fn_prefix ## u ## typ ## 32 ## fn_suffix(PDCI_FIRST_ARGS, const PDC_u ## typ ## 32 *lastnm PDCI_LAST_ARGS); \
ret_type fn_prefix ## u ## typ ## 64 ## fn_suffix(PDCI_FIRST_ARGS, const PDC_u ## typ ## 64 *lastnm PDCI_LAST_ARGS) \

/* ================================================================================ */ 
/* Declarations of function families */

#undef PDCI_LAST_ARGS
#define PDCI_LAST_ARGS 

#undef PDCI_FIRST_ARGS
#define PDCI_FIRST_ARGS PDC_t *pdc, const PDC_base_csm *csm, PDC_base_ed *ed
PDCI_DECL_FAMILY(PDC_error_t, PDC_a_, int, _read_internal, res_out);
PDCI_DECL_FAMILY(PDC_error_t, PDC_e_, int, _read_internal, res_out);
PDCI_DECL_FAMILY(PDC_error_t, PDC_b_, int, _read_internal, res_out);

#undef PDCI_FIRST_ARGS
#define PDCI_FIRST_ARGS PDC_t *pdc, const PDC_base_csm *csm, size_t width, PDC_base_ed *ed
PDCI_DECL_FAMILY(PDC_error_t, PDC_a_, int, _FW_read_internal, res_out);
PDCI_DECL_FAMILY(PDC_error_t, PDC_e_, int, _FW_read_internal, res_out);

#undef PDCI_FIRST_ARGS
#define PDCI_FIRST_ARGS PDC_t *pdc, const PDC_base_csm *csm, PDC_uint32 num_digits, PDC_base_ed *ed
PDCI_DECL_FAMILY(PDC_error_t, PDC_ebc_, int, _read_internal, res_out);
PDCI_DECL_FAMILY(PDC_error_t, PDC_bcd_, int, _read_internal, res_out);

#undef PDCI_FIRST_ARGS
#define PDCI_FIRST_ARGS PDC_t *pdc, const PDC_base_csm *csm, PDC_uint32 num_bytes, PDC_base_ed *ed
PDCI_DECL_FAMILY(PDC_error_t, PDC_sbl_, int, _read_internal, res_out);
PDCI_DECL_FAMILY(PDC_error_t, PDC_sbh_, int, _read_internal, res_out);

#undef PDCI_FIRST_ARGS
#define PDCI_FIRST_ARGS PDC_t *pdc, const PDC_base_csm *csm, PDC_uint32 num_digits, PDC_uint32 d_exp, PDC_base_ed *ed
PDCI_DECL_FAMILY(PDC_error_t, PDC_ecb_, fpoint, _read_internal, res_out);
PDCI_DECL_FAMILY(PDC_error_t, PDC_bcd_, fpoint, _read_internal, res_out);

#undef PDCI_FIRST_ARGS
#define PDCI_FIRST_ARGS PDC_t *pdc, const PDC_base_csm *csm, PDC_uint32 num_bytes, PDC_uint32 d_exp, PDC_base_ed *ed
PDCI_DECL_FAMILY(PDC_error_t, PDC_sbl_, fpoint, _read_internal, res_out);
PDCI_DECL_FAMILY(PDC_error_t, PDC_sbh_, fpoint, _read_internal, res_out);

/* INTERNAL VERSIONS OF WRITE FUNCTIONS */

#ifdef FOR_CKIT
/* Prototypes for CKIT */
ssize_t PDC_a_char_lit_write2io_internal(PDC_t *pdc, Sfio_t *io, PDC_char c);
ssize_t PDC_a_str_lit_write2io_internal (PDC_t *pdc, Sfio_t *io, const PDC_string *s);
ssize_t PDC_a_Cstr_lit_write2io_internal(PDC_t *pdc, Sfio_t *io, const char *s);

ssize_t PDC_a_char_lit_write2buf_internal(PDC_t *pdc, PDC_byte* buf, size_t buf_len, int *buf_full, PDC_char c);
ssize_t PDC_a_str_lit_write2buf_internal (PDC_t *pdc, PDC_byte* buf, size_t buf_len, int *buf_full, const PDC_string *s);
ssize_t PDC_a_Cstr_lit_write2buf_internal(PDC_t *pdc, PDC_byte* buf, size_t buf_len, int *buf_full, const char *s);

ssize_t PDC_e_char_lit_write2io_internal(PDC_t *pdc, Sfio_t *io, PDC_char c);
ssize_t PDC_e_str_lit_write2io_internal (PDC_t *pdc, Sfio_t *io, const PDC_string *s);
ssize_t PDC_e_Cstr_lit_write2io_internal(PDC_t *pdc, Sfio_t *io, const char *s);

ssize_t PDC_e_char_lit_write2buf_internal(PDC_t *pdc, PDC_byte* buf, size_t buf_len, int *buf_full, PDC_char c);
ssize_t PDC_e_str_lit_write2buf_internal (PDC_t *pdc, PDC_byte* buf, size_t buf_len, int *buf_full, const PDC_string *s);
ssize_t PDC_e_Cstr_lit_write2buf_internal(PDC_t *pdc, PDC_byte* buf, size_t buf_len, int *buf_full, const char *s);

ssize_t PDC_char_lit_write2io_internal(PDC_t *pdc, Sfio_t *io, PDC_char c);
ssize_t PDC_str_lit_write2io_internal (PDC_t *pdc, Sfio_t *io, const PDC_string *s);
ssize_t PDC_Cstr_lit_write2io_internal(PDC_t *pdc, Sfio_t *io, const char *s);

ssize_t PDC_char_lit_write2buf_internal(PDC_t *pdc, PDC_byte* buf, size_t buf_len, int *buf_full, PDC_char c);
ssize_t PDC_str_lit_write2buf_internal (PDC_t *pdc, PDC_byte* buf, size_t buf_len, int *buf_full, const PDC_string *s);
ssize_t PDC_Cstr_lit_write2buf_internal(PDC_t *pdc, PDC_byte* buf, size_t buf_len, int *buf_full, const char *s);

ssize_t PDC_char_write2io_internal     (PDC_t *pdc, Sfio_t *io, const PDC_base_ed *ed, PDC_char *c);
ssize_t PDC_a_char_write2io_internal   (PDC_t *pdc, Sfio_t *io, const PDC_base_ed *ed, PDC_char *c);
ssize_t PDC_e_char_write2io_internal   (PDC_t *pdc, Sfio_t *io, const PDC_base_ed *ed, PDC_char *c);

ssize_t PDC_char_write2buf_internal    (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, const PDC_base_ed *ed, PDC_char *c);
ssize_t PDC_a_char_write2buf_internal  (PDC_t *pdc, PDC_byte *buf, size_t buf_len, ssize_t *buf_full, const PDC_base_ed *ed, PDC_char *c);
ssize_t PDC_e_char_write2buf_internal  (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, const PDC_base_ed *ed, PDC_char *c);

ssize_t PDC_string_FW_write2io_internal   (PDC_t *pdc, Sfio_t *io, size_t width, const PDC_base_ed *ed, PDC_string *s);
ssize_t PDC_a_string_FW_write2io_internal (PDC_t *pdc, Sfio_t *io, size_t width, const PDC_base_ed *ed, PDC_string *s);
ssize_t PDC_e_string_FW_write2io_internal (PDC_t *pdc, Sfio_t *io, size_t width, const PDC_base_ed *ed, PDC_string *s);

ssize_t PDC_string_FW_write2buf_internal  (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full,
					   size_t width, const PDC_base_ed *ed, PDC_string *s);
ssize_t PDC_a_string_FW_write2buf_internal(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full,
					   size_t width, const PDC_base_ed *ed, PDC_string *s);
ssize_t PDC_e_string_FW_write2buf_internal(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full,
					   size_t width, const PDC_base_ed *ed, PDC_string *s);

ssize_t PDC_string_write2io_internal   (PDC_t *pdc, Sfio_t *io, PDC_char stopChar, const PDC_base_ed *ed, PDC_string *s);
ssize_t PDC_a_string_write2io_internal (PDC_t *pdc, Sfio_t *io, PDC_char stopChar, const PDC_base_ed *ed, PDC_string *s);
ssize_t PDC_e_string_write2io_internal (PDC_t *pdc, Sfio_t *io, PDC_char stopChar, const PDC_base_ed *ed, PDC_string *s);

ssize_t PDC_string_write2buf_internal  (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_char stopChar, const PDC_base_ed *ed, PDC_string *s);
ssize_t PDC_a_string_write2buf_internal(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_char stopChar, const PDC_base_ed *ed, PDC_string *s);
ssize_t PDC_e_string_write2buf_internal(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_char stopChar, const PDC_base_ed *ed, PDC_string *s);

ssize_t PDC_string_ME_write2io_internal   (PDC_t *pdc, Sfio_t *io, const char *matchRegexp, const PDC_base_ed *ed, PDC_string *s);
ssize_t PDC_a_string_ME_write2io_internal (PDC_t *pdc, Sfio_t *io, const char *matchRegexp, const PDC_base_ed *ed, PDC_string *s);
ssize_t PDC_e_string_ME_write2io_internal (PDC_t *pdc, Sfio_t *io, const char *matchRegexp, const PDC_base_ed *ed, PDC_string *s);

ssize_t PDC_string_ME_write2buf_internal  (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full,
					   const char *matchRegexp, const PDC_base_ed *ed, PDC_string *s);
ssize_t PDC_a_string_ME_write2buf_internal(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full,
					   const char *matchRegexp, const PDC_base_ed *ed, PDC_string *s);
ssize_t PDC_e_string_ME_write2buf_internal(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full,
					   const char *matchRegexp, const PDC_base_ed *ed, PDC_string *s);

ssize_t PDC_string_CME_write2io_internal   (PDC_t *pdc, Sfio_t *io, PDC_regexp_t *matchRegexp, const PDC_base_ed *ed, PDC_string *s);
ssize_t PDC_a_string_CME_write2io_internal (PDC_t *pdc, Sfio_t *io, PDC_regexp_t *matchRegexp, const PDC_base_ed *ed, PDC_string *s);
ssize_t PDC_e_string_CME_write2io_internal (PDC_t *pdc, Sfio_t *io, PDC_regexp_t *matchRegexp, const PDC_base_ed *ed, PDC_string *s);

ssize_t PDC_string_CME_write2buf_internal  (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full,
					    PDC_regexp_t *matchRegexp, const PDC_base_ed *ed, PDC_string *s);
ssize_t PDC_a_string_CME_write2buf_internal(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full,
					    PDC_regexp_t *matchRegexp, const PDC_base_ed *ed, PDC_string *s);
ssize_t PDC_e_string_CME_write2buf_internal(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full,
					    PDC_regexp_t *matchRegexp, const PDC_base_ed *ed, PDC_string *s);

ssize_t PDC_string_SE_write2io_internal   (PDC_t *pdc, Sfio_t *io, const char *stopRegexp, const PDC_base_ed *ed, PDC_string *s);
ssize_t PDC_a_string_SE_write2io_internal (PDC_t *pdc, Sfio_t *io, const char *stopRegexp, const PDC_base_ed *ed, PDC_string *s);
ssize_t PDC_e_string_SE_write2io_internal (PDC_t *pdc, Sfio_t *io, const char *stopRegexp, const PDC_base_ed *ed, PDC_string *s);

ssize_t PDC_string_SE_write2buf_internal  (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full,
					   const char *stopRegexp, const PDC_base_ed *ed, PDC_string *s);
ssize_t PDC_a_string_SE_write2buf_internal(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full,
					   const char *stopRegexp, const PDC_base_ed *ed, PDC_string *s);
ssize_t PDC_e_string_SE_write2buf_internal(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full,
					   const char *stopRegexp, const PDC_base_ed *ed, PDC_string *s);

ssize_t PDC_string_CSE_write2io_internal   (PDC_t *pdc, Sfio_t *io, PDC_regexp_t *stopRegexp, const PDC_base_ed *ed, PDC_string *s);
ssize_t PDC_a_string_CSE_write2io_internal (PDC_t *pdc, Sfio_t *io, PDC_regexp_t *stopRegexp, const PDC_base_ed *ed, PDC_string *s);
ssize_t PDC_e_string_CSE_write2io_internal (PDC_t *pdc, Sfio_t *io, PDC_regexp_t *stopRegexp, const PDC_base_ed *ed, PDC_string *s);

ssize_t PDC_string_CSE_write2buf_internal  (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full,
					    PDC_regexp_t *stopRegexp, const PDC_base_ed *ed, PDC_string *s);
ssize_t PDC_a_string_CSE_write2buf_internal(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full,
					    PDC_regexp_t *stopRegexp, const PDC_base_ed *ed, PDC_string *s);
ssize_t PDC_e_string_CSE_write2buf_internal(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full,
					    PDC_regexp_t *stopRegexp, const PDC_base_ed *ed, PDC_string *s);

#else
/* Actual impls */
#define PDC_a_char_lit_write2io_internal(pdc, io, c) \
  PDCI_char_lit_write2io(pdc, io, c, PDC_charset_ASCII, "PDC_a_char_lit_write2io", 0)

#define PDC_e_char_lit_write2io_internal(pdc, io, c) \
  PDCI_char_lit_write2io(pdc, io, c, PDC_charset_EBCDIC, "PDC_e_char_lit_write2io", 0)

#define PDC_char_lit_write2io_internal(pdc, io, c) \
  PDCI_char_lit_write2io(pdc, io, c, pdc->disc->def_charset, "PDC_char_lit_write2io", 0)

#define PDC_a_str_lit_write2io_internal(pdc, io, s) \
  PDCI_str_lit_write2io(pdc, io, s, PDC_charset_ASCII, "PDC_a_str_lit_write2io", 0)

#define PDC_e_str_lit_write2io_internal(pdc, io, s) \
  PDCI_str_lit_write2io(pdc, io, s, PDC_charset_EBCDIC, "PDC_e_str_lit_write2io", 0)

#define PDC_str_lit_write2io_internal(pdc, io, s) \
  PDCI_str_lit_write2io(pdc, io, s, pdc->disc->def_charset, "PDC_str_lit_write2io", 0)

#define PDC_a_Cstr_lit_write2io_internal(pdc, io, s) \
  PDCI_Cstr_lit_write2io(pdc, io, s, PDC_charset_ASCII, "PDC_a_Cstr_lit_write2io", 0)

#define PDC_e_Cstr_lit_write2io_internal(pdc, io, s) \
  PDCI_Cstr_lit_write2io(pdc, io, s, PDC_charset_EBCDIC, "PDC_e_Cstr_lit_write2io", 0)

#define PDC_Cstr_lit_write2io_internal(pdc, io, s) \
  PDCI_Cstr_lit_write2io(pdc, io, s, pdc->disc->def_charset, "PDC_Cstr_lit_write2io", 0)

#define PDC_a_char_lit_write2buf_internal(pdc, buf, buf_len, buf_full, c) \
  PDCI_char_lit_write2buf(pdc, buf, buf_len, buf_full, c, PDC_charset_ASCII, "PDC_a_char_lit_write2buf", 0)

#define PDC_e_char_lit_write2buf_internal(pdc, buf, buf_len, buf_full, c) \
  PDCI_char_lit_write2buf(pdc, buf, buf_len, buf_full, c, PDC_charset_EBCDIC, "PDC_e_char_lit_write2buf", 0)

#define PDC_char_lit_write2buf_internal(pdc, buf, buf_len, buf_full, c) \
  PDCI_char_lit_write2buf(pdc, buf, buf_len, buf_full, c, pdc->disc->def_charset, "PDC_char_lit_write2buf", 0)

#define PDC_a_str_lit_write2buf_internal(pdc, buf, buf_len, buf_full, s) \
  PDCI_str_lit_write2buf(pdc, buf, buf_len, buf_full, s, PDC_charset_ASCII, "PDC_a_str_lit_write2buf", 0)

#define PDC_e_str_lit_write2buf_internal(pdc, buf, buf_len, buf_full, s) \
  PDCI_str_lit_write2buf(pdc, buf, buf_len, buf_full, s, PDC_charset_EBCDIC, "PDC_e_str_lit_write2buf", 0)

#define PDC_str_lit_write2buf_internal(pdc, buf, buf_len, buf_full, s) \
  PDCI_str_lit_write2buf(pdc, buf, buf_len, buf_full, s, pdc->disc->def_charset, "PDC_str_lit_write2buf", 0)

#define PDC_a_Cstr_lit_write2buf_internal(pdc, buf, buf_len, buf_full, s) \
  PDCI_Cstr_lit_write2buf(pdc, buf, buf_len, buf_full, s, PDC_charset_ASCII, "PDC_a_Cstr_lit_write2buf", 0)

#define PDC_e_Cstr_lit_write2buf_internal(pdc, buf, buf_len, buf_full, s) \
  PDCI_Cstr_lit_write2buf(pdc, buf, buf_len, buf_full, s, PDC_charset_EBCDIC, "PDC_e_Cstr_lit_write2buf", 0)

#define PDC_Cstr_lit_write2buf_internal(pdc, buf, buf_len, buf_full, s) \
  PDCI_Cstr_lit_write2buf(pdc, buf, buf_len, buf_full, s, pdc->disc->dif_charset, "PDC_Cstr_lit_write2buf", 0)

#define PDC_char_write2io_internal(pdc, io, ed, c) \
  PDCI_char_write2io(pdc, io, ed, c, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_char_write2io", 0)

#define PDC_a_char_write2io_internal(pdc, io, ed, c) \
  PDCI_char_write2io(pdc, io, ed, c, pdc_charset_ASCII, "PDC_a_char_write2io", 0)

#define PDC_e_char_write2io_internal(pdc, io, ed, c) \
  PDCI_char_write2io(pdc, io, ed, c, pdc_charset_EBCDIC, "PDC_e_char_write2io", 0)

#define PDC_char_write2buf_internal(pdc, buf, buf_len, buf_full, ed, c) \
  PDCI_char_write2buf(pdc, buf, buf_len, buf_full, ed, c, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_char_write2buf", 0)

#define PDC_a_char_write2buf_internal(pdc, buf, buf_len, buf_full, ed, c) \
  PDCI_char_write2buf(pdc, buf, buf_len, buf_full, ed, c, PDC_charset_ASCII, "PDC_a_char_write2buf", 0)

#define PDC_e_char_write2buf_internal(pdc, buf, buf_len, buf_full, ed, c) \
  PDCI_char_write2buf(pdc, buf, buf_len, buf_full, ed, c, PDC_charset_EBCDIC, "PDC_e_char_write2buf", 0)

#define PDC_string_FW_write2io_internal(pdc, io, width, ed, s) \
  PDCI_string_FW_write2io(pdc, io, width, ed, s, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_string_FW_write2io", 0)

#define PDC_a_string_FW_write2io_internal(pdc, io, width, ed, s) \
  PDCI_string_FW_write2io(pdc, io, width, ed, s, PDC_charset_ASCII, "PDC_a_string_FW_write2io", 0)

#define PDC_e_string_FW_write2io_internal(pdc, io, width, ed, s) \
  PDCI_string_FW_write2io(pdc, io, width, ed, s, PDC_charset_EBCDIC, "PDC_e_string_FW_write2io", 0)

#define PDC_string_FW_write2buf_internal(pdc, buf, buf_len, buf_full, width, ed, s) \
  PDCI_string_FW_write2buf(pdc, buf, buf_len, buf_full, width, ed, s, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_string_FW_write2buf", 0)

#define PDC_a_string_FW_write2buf_internal(pdc, buf, buf_len, buf_full, width, ed, s) \
  PDCI_string_FW_write2buf(pdc, buf, buf_len, buf_full, width, ed, s, PDC_charset_ASCII, "PDC_a_string_FW_write2buf", 0)

#define PDC_e_string_FW_write2buf_internal(pdc, buf, buf_len, buf_full, width, ed, s) \
  PDCI_string_FW_write2buf(pdc, buf, buf_len, buf_full, width, ed, s, PDC_charset_EBCDIC, "PDC_e_string_FW_write2buf", 0)

#define PDC_string_write2io_internal(pdc, io, stopChar, ed, s) \
  PDCI_string_write2io(pdc, io, stopChar, ed, s, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_string_write2io", 0)

#define PDC_a_string_write2io_internal(pdc, io, stopChar, ed, s) \
  PDCI_string_write2io(pdc, io, stopChar, ed, s, PDC_charset_ASCII, "PDC_a_string_write2io", 0)

#define PDC_e_string_write2io_internal(pdc, io, stopChar, ed, s) \
  PDCI_string_write2io(pdc, io, stopChar, ed, s, PDC_charset_EBCDIC, "PDC_e_string_write2io", 0)

#define PDC_string_write2buf_internal(pdc, buf, buf_len, buf_full, stopChar, ed, s) \
  PDCI_string_write2buf(pdc, buf, buf_len, buf_full, stopChar, ed, s, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_string_write2buf", 0)

#define PDC_a_string_write2buf_internal(pdc, buf, buf_len, buf_full, stopChar, ed, s) \
  PDCI_string_write2buf(pdc, buf, buf_len, buf_full, stopChar, ed, s, PDC_charset_ASCII, "PDC_a_string_write2buf", 0)

#define PDC_e_string_write2buf_internal(pdc, buf, buf_len, buf_full, stopChar, ed, s) \
  PDCI_string_write2buf(pdc, buf, buf_len, buf_full, stopChar, ed, s, PDC_charset_EBCDIC, "PDC_e_string_write2buf", 0)

#define PDC_string_ME_write2io_internal(pdc, io, matchRegexp, ed, s) \
  PDCI_string_write2io(pdc, io, ed, s, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_string_ME_write2io", 0)

#define PDC_a_string_ME_write2io_internal(pdc, io, matchRegexp, ed, s) \
  PDCI_string_write2io(pdc, io, ed, s, PDCI_charset_ASCII, "PDC_a_string_ME_write2io", 0)

#define PDC_e_string_ME_write2io_internal(pdc, io, matchRegexp, ed, s) \
  PDCI_string_write2io(pdc, io, ed, s, PDCI_charset_EBCDIC, "PDC_e_string_ME_write2io", 0)

#define PDC_string_ME_write2buf_internal(pdc, buf, buf_len, buf_full, matchRegexp, ed, s) \
  PDCI_string_write2buf(pdc, buf, buf_len, buf_full, ed, s, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_ME_string_write2buf", 0)

#define PDC_a_string_ME_write2buf_internal(pdc, buf, buf_len, buf_full, matchRegexp, ed, s) \
  PDCI_string_write2buf(pdc, buf, buf_len, buf_full, ed, s, PDC_charset_ASCII, "PDC_a_string_ME_write2buf", 0)

#define PDC_e_string_ME_write2buf_internal(pdc, buf, buf_len, buf_full, matchRegexp, ed, s) \
  PDCI_string_write2buf(pdc, buf, buf_len, buf_full, ed, s, PDC_charset_EBCDIC, "PDC_e_string_ME_write2buf", 0)

#define PDC_string_CME_write2io_internal(pdc, io, matchRegexp, ed, s) \
  PDCI_string_write2io(pdc, io, ed, s, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_string_CME_write2io", 0)

#define PDC_a_string_CME_write2io_internal(pdc, io, matchRegexp, ed, s) \
  PDCI_string_write2io(pdc, io, ed, s, PDCI_charset_ASCII, "PDC_a_string_CME_write2io", 0)

#define PDC_e_string_CME_write2io_internal(pdc, io, matchRegexp, ed, s) \
  PDCI_string_write2io(pdc, io, ed, s, PDCI_charset_EBCDIC, "PDC_e_string_CME_write2io", 0)

#define PDC_string_CME_write2buf_internal(pdc, buf, buf_len, buf_full, matchRegexp, ed, s) \
  PDCI_string_write2buf(pdc, buf, buf_len, buf_full, ed, s, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_CME_string_write2buf", 0)

#define PDC_a_string_CME_write2buf_internal(pdc, buf, buf_len, buf_full, matchRegexp, ed, s) \
  PDCI_string_write2buf(pdc, buf, buf_len, buf_full, ed, s, PDC_charset_ASCII, "PDC_a_string_CME_write2buf", 0)

#define PDC_e_string_CME_write2buf_internal(pdc, buf, buf_len, buf_full, matchRegexp, ed, s) \
  PDCI_string_write2buf(pdc, buf, buf_len, buf_full, ed, s, PDC_charset_EBCDIC, "PDC_e_string_CME_write2buf", 0)

#define PDC_string_SE_write2io_internal(pdc, io, stopRegexp, ed, s) \
  PDCI_string_write2io(pdc, io, ed, s, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_string_SE_write2io", 0)

#define PDC_a_string_SE_write2io_internal(pdc, io, stopRegexp, ed, s) \
  PDCI_string_write2io(pdc, io, ed, s, PDCI_charset_ASCII, "PDC_a_string_SE_write2io", 0)

#define PDC_e_string_SE_write2io_internal(pdc, io, stopRegexp, ed, s) \
  PDCI_string_write2io(pdc, io, ed, s, PDCI_charset_EBCDIC, "PDC_e_string_SE_write2io", 0)

#define PDC_string_SE_write2buf_internal(pdc, buf, buf_len, buf_full, stopRegexp, ed, s) \
  PDCI_string_write2buf(pdc, buf, buf_len, buf_full, ed, s, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_SE_string_write2buf", 0)

#define PDC_a_string_SE_write2buf_internal(pdc, buf, buf_len, buf_full, stopRegexp, ed, s) \
  PDCI_string_write2buf(pdc, buf, buf_len, buf_full, ed, s, PDC_charset_ASCII, "PDC_a_string_SE_write2buf", 0)

#define PDC_e_string_SE_write2buf_internal(pdc, buf, buf_len, buf_full, stopRegexp, ed, s) \
  PDCI_string_write2buf(pdc, buf, buf_len, buf_full, ed, s, PDC_charset_EBCDIC, "PDC_e_string_SE_write2buf", 0)

#define PDC_string_CSE_write2io_internal(pdc, io, stopRegexp, ed, s) \
  PDCI_string_write2io(pdc, io, ed, s, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_string_CSE_write2io", 0)

#define PDC_a_string_CSE_write2io_internal(pdc, io, stopRegexp, ed, s) \
  PDCI_string_write2io(pdc, io, ed, s, PDCI_charset_ASCII, "PDC_a_string_CSE_write2io", 0)

#define PDC_e_string_CSE_write2io_internal(pdc, io, stopRegexp, ed, s) \
  PDCI_string_write2io(pdc, io, ed, s, PDCI_charset_EBCDIC, "PDC_e_string_CSE_write2io", 0)

#define PDC_string_CSE_write2buf_internal(pdc, buf, buf_len, buf_full, stopRegexp, ed, s) \
  PDCI_string_write2buf(pdc, buf, buf_len, buf_full, ed, s, PDCI_SAFE_DEF_CHARSET(pdc), "PDC_CSE_string_write2buf", 0)

#define PDC_a_string_CSE_write2buf_internal(pdc, buf, buf_len, buf_full, stopRegexp, ed, s) \
  PDCI_string_write2buf(pdc, buf, buf_len, buf_full, ed, s, PDC_charset_ASCII, "PDC_a_string_CSE_write2buf", 0)

#define PDC_e_string_CSE_write2buf_internal(pdc, buf, buf_len, buf_full, stopRegexp, ed, s) \
  PDCI_string_write2buf(pdc, buf, buf_len, buf_full, ed, s, PDC_charset_EBCDIC, "PDC_e_string_CSE_write2buf", 0)

#define PDC_int8_write2io_internal(pdc, io, ed, val) \
  PDCI_INTERNAL_CHARSET_SWITCH(pdc, PDC, int8_write2io, (pdc, io, ed, val))

#define PDC_int16_write2io_internal(pdc, io, ed, val) \
  PDCI_INTERNAL_CHARSET_SWITCH(pdc, PDC, int16_write2io, (pdc, io, ed, val))

#define PDC_int32_write2io_internal(pdc, io, ed, val) \
  PDCI_INTERNAL_CHARSET_SWITCH(pdc, PDC, int32_write2io, (pdc, io, ed, val))

#define PDC_int64_write2io_internal(pdc, io, ed, val) \
  PDCI_INTERNAL_CHARSET_SWITCH(pdc, PDC, int64_write2io, (pdc, io, ed, val))

#define PDC_uint8_write2io_internal(pdc, io, ed, val) \
  PDCI_INTERNAL_CHARSET_SWITCH(pdc, PDC, uint8_write2io, (pdc, io, ed, val))

#define PDC_uint16_write2io_internal(pdc, io, ed, val) \
  PDCI_INTERNAL_CHARSET_SWITCH(pdc, PDC, uint16_write2io, (pdc, io, ed, val))

#define PDC_uint32_write2io_internal(pdc, io, ed, val) \
  PDCI_INTERNAL_CHARSET_SWITCH(pdc, PDC, uint32_write2io, (pdc, io, ed, val))

#define PDC_uint64_write2io_internal(pdc, io, ed, val) \
  PDCI_INTERNAL_CHARSET_SWITCH(pdc, PDC, uint64_write2io, (pdc, io, ed, val))

#define PDC_int8_write2buf_internal(pdc, buf, buf_len, buf_full, ed, val) \
  PDCI_INTERNAL_CHARSET_SWITCH(pdc, PDC, int8_write2buf, (pdc, buf, buf_len, buf_full, ed, val))

#define PDC_int16_write2buf_internal(pdc, buf, buf_len, buf_full, ed, val) \
  PDCI_INTERNAL_CHARSET_SWITCH(pdc, PDC, int16_write2buf, (pdc, buf, buf_len, buf_full, ed, val))

#define PDC_int32_write2buf_internal(pdc, buf, buf_len, buf_full, ed, val) \
  PDCI_INTERNAL_CHARSET_SWITCH(pdc, PDC, int32_write2buf, (pdc, buf, buf_len, buf_full, ed, val))

#define PDC_int64_write2buf_internal(pdc, buf, buf_len, buf_full, ed, val) \
  PDCI_INTERNAL_CHARSET_SWITCH(pdc, PDC, int64_write2buf, (pdc, buf, buf_len, buf_full, ed, val))

#define PDC_uint8_write2buf_internal(pdc, buf, buf_len, buf_full, ed, val) \
  PDCI_INTERNAL_CHARSET_SWITCH(pdc, PDC, uint8_write2buf, (pdc, buf, buf_len, buf_full, ed, val))

#define PDC_uint16_write2buf_internal(pdc, buf, buf_len, buf_full, ed, val) \
  PDCI_INTERNAL_CHARSET_SWITCH(pdc, PDC, uint16_write2buf, (pdc, buf, buf_len, buf_full, ed, val))

#define PDC_uint32_write2buf_internal(pdc, buf, buf_len, buf_full, ed, val) \
  PDCI_INTERNAL_CHARSET_SWITCH(pdc, PDC, uint32_write2buf, (pdc, buf, buf_len, buf_full, ed, val))

#define PDC_uint64_write2buf_internal(pdc, buf, buf_len, buf_full, ed, val) \
  PDCI_INTERNAL_CHARSET_SWITCH(pdc, PDC, uint64_write2buf, (pdc, buf, buf_len, buf_full, ed, val))

#endif /* FOR_CKIT */

#undef PDCI_FIRST_ARGS
#define PDCI_FIRST_ARGS PDC_t *pdc, Sfio_t *io, const PDC_base_ed *ed
PDCI_DECL_FAMILY_LASTCONST(int, PDC_a_, int, _write2io_internal, val);
PDCI_DECL_FAMILY_LASTCONST(int, PDC_e_, int, _write2io_internal, val);
PDCI_DECL_FAMILY_LASTCONST(int, PDC_b_, int, _write2io_internal, val);

#undef PDCI_FIRST_ARGS
#define PDCI_FIRST_ARGS PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, const PDC_base_ed *ed
PDCI_DECL_FAMILY_LASTCONST(int, PDC_a_, int, _write2buf_internal, val);
PDCI_DECL_FAMILY_LASTCONST(int, PDC_e_, int, _write2buf_internal, val);
PDCI_DECL_FAMILY_LASTCONST(int, PDC_b_, int, _write2buf_internal, val);

#undef PDCI_FIRST_ARGS
#define PDCI_FIRST_ARGS PDC_t *pdc, Sfio_t *io, size_t width, const PDC_base_ed *ed
PDCI_DECL_FAMILY_LASTCONST(int, PDC_a_, int, _FW_write2io_internal, val);
PDCI_DECL_FAMILY_LASTCONST(int, PDC_e_, int, _FW_write2io_internal, val);

#undef PDCI_FIRST_ARGS
#define PDCI_FIRST_ARGS PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, size_t width, const PDC_base_ed *ed
PDCI_DECL_FAMILY_LASTCONST(int, PDC_a_, int, _FW_write2buf_internal, val);
PDCI_DECL_FAMILY_LASTCONST(int, PDC_e_, int, _FW_write2buf_internal, val);

#undef PDCI_FIRST_ARGS
#define PDCI_FIRST_ARGS PDC_t *pdc, Sfio_t *io, PDC_uint32 num_digits, const PDC_base_ed *ed
PDCI_DECL_FAMILY_LASTCONST(int, PDC_ebc_, int, _write2io_internal, val);
PDCI_DECL_FAMILY_LASTCONST(int, PDC_bcd_, int, _write2io_internal, val);

#undef PDCI_FIRST_ARGS
#define PDCI_FIRST_ARGS PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_digits, const PDC_base_ed *ed
PDCI_DECL_FAMILY_LASTCONST(int, PDC_ebc_, int, _write2buf_internal, val);
PDCI_DECL_FAMILY_LASTCONST(int, PDC_bcd_, int, _write2buf_internal, val);

#undef PDCI_FIRST_ARGS
#define PDCI_FIRST_ARGS PDC_t *pdc, Sfio_t *io, PDC_uint32 num_bytes, const PDC_base_ed *ed
PDCI_DECL_FAMILY_LASTCONST(int, PDC_sbl_, int, _write2io_internal, val);
PDCI_DECL_FAMILY_LASTCONST(int, PDC_sbh_, int, _write2io_internal, val);

#undef PDCI_FIRST_ARGS
#define PDCI_FIRST_ARGS PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_bytes, const PDC_base_ed *ed
PDCI_DECL_FAMILY_LASTCONST(int, PDC_sbl_, int, _write2buf_internal, val);
PDCI_DECL_FAMILY_LASTCONST(int, PDC_sbh_, int, _write2buf_internal, val);

#undef PDCI_FIRST_ARGS
#define PDCI_FIRST_ARGS PDC_t *pdc, Sfio_t *io, PDC_uint32 num_digits, PDC_uint32 d_exp, const PDC_base_ed *ed
PDCI_DECL_FAMILY_LASTCONST(int, PDC_ebc_, fpoint, _write2io_internal, val);
PDCI_DECL_FAMILY_LASTCONST(int, PDC_bcd_, fpoint, _write2io_internal, val);

#undef PDCI_FIRST_ARGS
#define PDCI_FIRST_ARGS PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_digits, PDC_uint32 d_exp, const PDC_base_ed *ed
PDCI_DECL_FAMILY_LASTCONST(int, PDC_ebc_, fpoint, _write2buf_internal, val);
PDCI_DECL_FAMILY_LASTCONST(int, PDC_bcd_, fpoint, _write2buf_internal, val);

#undef PDCI_FIRST_ARGS
#define PDCI_FIRST_ARGS PDC_t *pdc, Sfio_t *io, PDC_uint32 num_bytes, PDC_uint32 d_exp, const PDC_base_ed *ed
PDCI_DECL_FAMILY_LASTCONST(int, PDC_sbl_, fpoint, _write2io_internal, val);
PDCI_DECL_FAMILY_LASTCONST(int, PDC_sbh_, fpoint, _write2io_internal, val);

#undef PDCI_FIRST_ARGS
#define PDCI_FIRST_ARGS PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_uint32 num_bytes, PDC_uint32 d_exp, const PDC_base_ed *ed
PDCI_DECL_FAMILY_LASTCONST(int, PDC_sbl_, fpoint, _write2buf_internal, val);
PDCI_DECL_FAMILY_LASTCONST(int, PDC_sbh_, fpoint, _write2buf_internal, val);

#ifdef FOR_CKIT
/* Prototypes for CKIT */

ssize_t PDC_int8_write2io_internal  (PDC_t *pdc, Sfio_t *io, const PDC_base_ed *ed, const PDC_int8   *val);
ssize_t PDC_int16_write2io_internal (PDC_t *pdc, Sfio_t *io, const PDC_base_ed *ed, const PDC_int16  *val);
ssize_t PDC_int32_write2io_internal (PDC_t *pdc, Sfio_t *io, const PDC_base_ed *ed, const PDC_int32  *val);
ssize_t PDC_int64_write2io_internal (PDC_t *pdc, Sfio_t *io, const PDC_base_ed *ed, const PDC_int64  *val);

ssize_t PDC_uint8_write2io_internal (PDC_t *pdc, Sfio_t *io, const PDC_base_ed *ed, const PDC_uint8  *val);
ssize_t PDC_uint16_write2io_internal(PDC_t *pdc, Sfio_t *io, const PDC_base_ed *ed, const PDC_uint16 *val);
ssize_t PDC_uint32_write2io_internal(PDC_t *pdc, Sfio_t *io, const PDC_base_ed *ed, const PDC_uint32 *val);
ssize_t PDC_uint64_write2io_internal(PDC_t *pdc, Sfio_t *io, const PDC_base_ed *ed, const PDC_uint64 *val);

ssize_t PDC_int8_write2buf_internal  (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, const PDC_base_ed *ed, const PDC_int8   *val);
ssize_t PDC_int16_write2buf_internal (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, const PDC_base_ed *ed, const PDC_int16  *val);
ssize_t PDC_int32_write2buf_internal (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, const PDC_base_ed *ed, const PDC_int32  *val);
ssize_t PDC_int64_write2buf_internal (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, const PDC_base_ed *ed, const PDC_int64  *val);

ssize_t PDC_uint8_write2buf_internal (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, const PDC_base_ed *ed, const PDC_uint8  *val);
ssize_t PDC_uint16_write2buf_internal(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, const PDC_base_ed *ed, const PDC_uint16 *val);
ssize_t PDC_uint32_write2buf_internal(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, const PDC_base_ed *ed, const PDC_uint32 *val);
ssize_t PDC_uint64_write2buf_internal(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, const PDC_base_ed *ed, const PDC_uint64 *val);

#else
/* The actual impls */

#define PDC_int8_write2io_internal(pdc, io, ed, val) \
  PDCI_INTERNAL_CHARSET_SWITCH(pdc, PDC, int8_write2io, (pdc, io, ed, val))

#define PDC_int16_write2io_internal(pdc, io, ed, val) \
  PDCI_INTERNAL_CHARSET_SWITCH(pdc, PDC, int16_write2io, (pdc, io, ed, val))

#define PDC_int32_write2io_internal(pdc, io, ed, val) \
  PDCI_INTERNAL_CHARSET_SWITCH(pdc, PDC, int32_write2io, (pdc, io, ed, val))

#define PDC_int64_write2io_internal(pdc, io, ed, val) \
  PDCI_INTERNAL_CHARSET_SWITCH(pdc, PDC, int64_write2io, (pdc, io, ed, val))

#define PDC_uint8_write2io_internal(pdc, io, ed, val) \
  PDCI_INTERNAL_CHARSET_SWITCH(pdc, PDC, uint8_write2io, (pdc, io, ed, val))

#define PDC_uint16_write2io_internal(pdc, io, ed, val) \
  PDCI_INTERNAL_CHARSET_SWITCH(pdc, PDC, uint16_write2io, (pdc, io, ed, val))

#define PDC_uint32_write2io_internal(pdc, io, ed, val) \
  PDCI_INTERNAL_CHARSET_SWITCH(pdc, PDC, uint32_write2io, (pdc, io, ed, val))

#define PDC_uint64_write2io_internal(pdc, io, ed, val) \
  PDCI_INTERNAL_CHARSET_SWITCH(pdc, PDC, uint64_write2io, (pdc, io, ed, val))

#define PDC_int8_write2buf_internal(pdc, buf, buf_len, buf_full, ed, val) \
  PDCI_INTERNAL_CHARSET_SWITCH(pdc, PDC, int8_write2buf, (pdc, buf, buf_len, buf_full, ed, val))

#define PDC_int16_write2buf_internal(pdc, buf, buf_len, buf_full, ed, val) \
  PDCI_INTERNAL_CHARSET_SWITCH(pdc, PDC, int16_write2buf, (pdc, buf, buf_len, buf_full, ed, val))

#define PDC_int32_write2buf_internal(pdc, buf, buf_len, buf_full, ed, val) \
  PDCI_INTERNAL_CHARSET_SWITCH(pdc, PDC, int32_write2buf, (pdc, buf, buf_len, buf_full, ed, val))

#define PDC_int64_write2buf_internal(pdc, buf, buf_len, buf_full, ed, val) \
  PDCI_INTERNAL_CHARSET_SWITCH(pdc, PDC, int64_write2buf, (pdc, buf, buf_len, buf_full, ed, val))

#define PDC_uint8_write2buf_internal(pdc, buf, buf_len, buf_full, ed, val) \
  PDCI_INTERNAL_CHARSET_SWITCH(pdc, PDC, uint8_write2buf, (pdc, buf, buf_len, buf_full, ed, val))

#define PDC_uint16_write2buf_internal(pdc, buf, buf_len, buf_full, ed, val) \
  PDCI_INTERNAL_CHARSET_SWITCH(pdc, PDC, uint16_write2buf, (pdc, buf, buf_len, buf_full, ed, val))

#define PDC_uint32_write2buf_internal(pdc, buf, buf_len, buf_full, ed, val) \
  PDCI_INTERNAL_CHARSET_SWITCH(pdc, PDC, uint32_write2buf, (pdc, buf, buf_len, buf_full, ed, val))

#define PDC_uint64_write2buf_internal(pdc, buf, buf_len, buf_full, ed, val) \
  PDCI_INTERNAL_CHARSET_SWITCH(pdc, PDC, uint64_write2buf, (pdc, buf, buf_len, buf_full, ed, val))

#endif /* FOR_CKIT */

/* ================================================================================ */ 
/* INTERNAL VERSIONS OF ACCUM REPORTING FUNCTIONS */

/* These functions take an argument, outstr, for 
 * the output target, and do not check
 * the pdc, prefix, or accumulator arguments for NULL values.
 */

#undef PDCI_FIRST_ARGS
#define PDCI_FIRST_ARGS PDC_t *pdc, Sfio_t *outstr, const char *prefix, const char *what, int nst

PDC_error_t PDC_int8_acc_report_internal   (PDCI_FIRST_ARGS, PDC_int8_acc *a);
PDC_error_t PDC_int16_acc_report_internal  (PDCI_FIRST_ARGS, PDC_int16_acc *a);
PDC_error_t PDC_int32_acc_report_internal  (PDCI_FIRST_ARGS, PDC_int32_acc *a);
PDC_error_t PDC_int64_acc_report_internal  (PDCI_FIRST_ARGS, PDC_int64_acc *a);
PDC_error_t PDC_uint8_acc_report_internal  (PDCI_FIRST_ARGS, PDC_uint8_acc *a);
PDC_error_t PDC_uint16_acc_report_internal (PDCI_FIRST_ARGS, PDC_uint16_acc *a);
PDC_error_t PDC_uint32_acc_report_internal (PDCI_FIRST_ARGS, PDC_uint32_acc *a);
PDC_error_t PDC_uint64_acc_report_internal (PDCI_FIRST_ARGS, PDC_uint64_acc *a);

PDC_error_t PDC_int32_acc_report_map_internal(PDCI_FIRST_ARGS, PDC_int32_map_fn  fn, PDC_int32_acc *a);

PDC_error_t PDC_nerr_acc_report_internal(PDC_t *pdc, Sfio_t *outstr, const char *prefix, const char *what, int nst,
					 PDC_int32_acc *a);

PDC_error_t PDC_string_acc_report_internal (PDCI_FIRST_ARGS, PDC_string_acc *a);
PDC_error_t PDC_char_acc_report_internal   (PDCI_FIRST_ARGS, PDC_char_acc *a);

PDC_error_t PDC_fpoint8_acc_report_internal   (PDCI_FIRST_ARGS, PDC_fpoint8_acc *a);
PDC_error_t PDC_fpoint16_acc_report_internal  (PDCI_FIRST_ARGS, PDC_fpoint16_acc *a);
PDC_error_t PDC_fpoint32_acc_report_internal  (PDCI_FIRST_ARGS, PDC_fpoint32_acc *a);
PDC_error_t PDC_fpoint64_acc_report_internal  (PDCI_FIRST_ARGS, PDC_fpoint64_acc *a);
PDC_error_t PDC_ufpoint8_acc_report_internal  (PDCI_FIRST_ARGS, PDC_ufpoint8_acc *a);
PDC_error_t PDC_ufpoint16_acc_report_internal (PDCI_FIRST_ARGS, PDC_ufpoint16_acc *a);
PDC_error_t PDC_ufpoint32_acc_report_internal (PDCI_FIRST_ARGS, PDC_ufpoint32_acc *a);
PDC_error_t PDC_ufpoint64_acc_report_internal (PDCI_FIRST_ARGS, PDC_ufpoint64_acc *a);

/* ********************************************************************************
 * Remainder of this file contains function decls for functions
 * purely internal to the library impl.  Note the use of the PDCI prefix
 * for these functions 
 * ********************************************************************************/

/* ================================================================================ */ 
/* INTERNAL ERROR REPORTING FUNCTIONS */

/*
 * PDCI_report_err: Report a parse error that occurred at location loc.
 *
 * Can also use for other errors that have error codes: loc can be NULL.
 *
 * See description of PDC_error_f for description of level
 *  
 *   XXX errCode's type should be an enum that describes the kind of error XXX ???
 *
 * The whatfn param is optional (can be NULL). If non-null, the report
 * include a prefix of the form "[in <whatfn>]"
 *
 * The <format, ...> args are for a printf-style description that augments
 * the default description based on errCode. 
 *
 * N.B. This call does nothing if either there is no disc error function
 *      or if the disc e_rep is PDC_errorRep_None
 */

PDC_error_t PDCI_report_err(PDC_t *pdc, int level, PDC_loc_t *loc,
			    PDC_errCode_t errCode, const char *whatfn, const char *format, ... );

/* ================================================================================ */
/* PURELY INTERNAL IO FUNCTIONS */

/* 
 * Note: all of the following act on the IO cursor of the top checkpoint
 *
 * PDCI_IO_install_io:    XXX_TODOC
 * PDCI_IO_needbytes:     XXX_TODOC
 * PDCI_IO_morebytes:     XXX_TODOC
 *
 * PDCI_IO_forward:
 *
 *   Move IO cursor forward num_bytes bytes, which should be <=
 *   (end-begin), where [begin,end] are from the last call to needbytes
 *   or morebytes.  This call can obliviate that [begin,end] data
 *   region so IO_forward should only be used after all relevant data
 *   bytes have been observed.  Causes fatal error if K would move
 *   beyond an EOR/EOF marker or beyond the last in-memory data byte.
 */

PDC_error_t  PDCI_IO_install_io(PDC_t *pdc, Sfio_t *io);

PDC_error_t  PDCI_IO_needbytes (PDC_t *pdc,
				PDC_byte **b_out, PDC_byte **p1_out, PDC_byte **p2_out, PDC_byte **e_out,
			        int *eor_out, int *eof_out, size_t *bytes_out);
PDC_error_t  PDCI_IO_morebytes (PDC_t *pdc, PDC_byte **b_out, PDC_byte **p1_out, PDC_byte **p2_out, PDC_byte **e_out,
				int *eor_out, int *eof_out, size_t *bytes_out);
PDC_error_t  PDCI_IO_forward   (PDC_t *pdc, size_t num_bytes);

/*
 * Other IO routines:
 *    PDCI_IO_getElt: if the specified elt is currently in an in-memory buffer,
 *                    sets (*elt_out) to point to elt and returns PDC_OK,
 *                    otherwise returns PDC_ERR.
 */

PDC_error_t PDCI_IO_getElt(PDC_t *pdc, size_t num, PDC_IO_elt_t **elt_out);

/* ================================================================================ */
/* INTERNAL CONVERSION ROUTINES */

/* Various tables */
extern int PDCI_ascii_digit[];
extern int PDCI_ascii_is_digit[];
extern int PDCI_ascii_is_space[];
#define PDCI_is_a_digit(c) PDCI_ascii_is_digit[c]
#define PDCI_is_a_space(c) PDCI_ascii_is_space[c]

extern int PDCI_ebcdic_digit[];
extern int PDCI_ebcdic_is_digit[];
extern int PDCI_ebcdic_is_space[];
#define PDCI_is_e_digit(c) PDCI_ebcdic_is_digit[c]
#define PDCI_is_e_space(c) PDCI_ebcdic_is_space[c]

extern PDC_byte PDC_ea_tab[];
extern PDC_byte PDC_ae_tab[];
extern PDC_byte PDC_mod_ae_tab[];

extern int PDCI_bcd_hilo_digits[];
extern int PDCI_bcd_hi_digit[];
extern PDC_uint64 PDCI_10toThe[];

PDC_int8   PDCI_a2int8  (PDC_t *pdc, const PDC_byte *bytes, PDC_byte **ptr_out);
PDC_int16  PDCI_a2int16 (PDC_t *pdc, const PDC_byte *bytes, PDC_byte **ptr_out);
PDC_int32  PDCI_a2int32 (PDC_t *pdc, const PDC_byte *bytes, PDC_byte **ptr_out);
PDC_int64  PDCI_a2int64 (PDC_t *pdc, const PDC_byte *bytes, PDC_byte **ptr_out);

ssize_t PDCI_int8_2a_buf (PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_int8 i);
ssize_t PDCI_int16_2a_buf(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_int16 i);
ssize_t PDCI_int32_2a_buf(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_int32 i);
ssize_t PDCI_int64_2a_buf(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_int64 i);

ssize_t PDCI_int8_2a_io (PDC_t *pdc, Sfio_t *io, PDC_int8 i);
ssize_t PDCI_int16_2a_io(PDC_t *pdc, Sfio_t *io, PDC_int16 i);
ssize_t PDCI_int32_2a_io(PDC_t *pdc, Sfio_t *io, PDC_int32 i);
ssize_t PDCI_int64_2a_io(PDC_t *pdc, Sfio_t *io, PDC_int64 i);

PDC_uint8  PDCI_a2uint8 (PDC_t *pdc, const PDC_byte *bytes, PDC_byte **ptr_out);
PDC_uint16 PDCI_a2uint16(PDC_t *pdc, const PDC_byte *bytes, PDC_byte **ptr_out);
PDC_uint32 PDCI_a2uint32(PDC_t *pdc, const PDC_byte *bytes, PDC_byte **ptr_out);
PDC_uint64 PDCI_a2uint64(PDC_t *pdc, const PDC_byte *bytes, PDC_byte **ptr_out);

ssize_t PDCI_uint8_2a_buf (PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_uint8 u);
ssize_t PDCI_uint16_2a_buf(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_uint16 u);
ssize_t PDCI_uint32_2a_buf(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_uint32 u);
ssize_t PDCI_uint64_2a_buf(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_uint64 u);

ssize_t PDCI_uint8_2a_io (PDC_t *pdc, Sfio_t *io, PDC_uint8 u);
ssize_t PDCI_uint16_2a_io(PDC_t *pdc, Sfio_t *io, PDC_uint16 u);
ssize_t PDCI_uint32_2a_io(PDC_t *pdc, Sfio_t *io, PDC_uint32 u);
ssize_t PDCI_uint64_2a_io(PDC_t *pdc, Sfio_t *io, PDC_uint64 u);

PDC_int8   PDCI_e2int8  (PDC_t *pdc, const PDC_byte *bytes, PDC_byte **ptr_out);
PDC_int16  PDCI_e2int16 (PDC_t *pdc, const PDC_byte *bytes, PDC_byte **ptr_out);
PDC_int32  PDCI_e2int32 (PDC_t *pdc, const PDC_byte *bytes, PDC_byte **ptr_out);
PDC_int64  PDCI_e2int64 (PDC_t *pdc, const PDC_byte *bytes, PDC_byte **ptr_out);

ssize_t PDCI_int8_2e_buf (PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_int8  i);
ssize_t PDCI_int16_2e_buf(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_int16 i);
ssize_t PDCI_int32_2e_buf(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_int32 i);
ssize_t PDCI_int64_2e_buf(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_int64 i);

ssize_t PDCI_int8_2e_io (PDC_t *pdc, Sfio_t *io, PDC_int8  i);
ssize_t PDCI_int16_2e_io(PDC_t *pdc, Sfio_t *io, PDC_int16 i);
ssize_t PDCI_int32_2e_io(PDC_t *pdc, Sfio_t *io, PDC_int32 i);
ssize_t PDCI_int64_2e_io(PDC_t *pdc, Sfio_t *io, PDC_int64 i);

PDC_uint8  PDCI_e2uint8 (PDC_t *pdc, const PDC_byte *bytes, PDC_byte **ptr_out);
PDC_uint16 PDCI_e2uint16(PDC_t *pdc, const PDC_byte *bytes, PDC_byte **ptr_out);
PDC_uint32 PDCI_e2uint32(PDC_t *pdc, const PDC_byte *bytes, PDC_byte **ptr_out);
PDC_uint64 PDCI_e2uint64(PDC_t *pdc, const PDC_byte *bytes, PDC_byte **ptr_out);

ssize_t PDCI_uint8_2e_buf (PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_uint8  u);
ssize_t PDCI_uint16_2e_buf(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_uint16 u);
ssize_t PDCI_uint32_2e_buf(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_uint32 u);
ssize_t PDCI_uint64_2e_buf(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_uint64 u);

ssize_t PDCI_uint8_2e_io (PDC_t *pdc, Sfio_t *io, PDC_uint8  u);
ssize_t PDCI_uint16_2e_io(PDC_t *pdc, Sfio_t *io, PDC_uint16 u);
ssize_t PDCI_uint32_2e_io(PDC_t *pdc, Sfio_t *io, PDC_uint32 u);
ssize_t PDCI_uint64_2e_io(PDC_t *pdc, Sfio_t *io, PDC_uint64 u);

PDC_int8   PDCI_ebc2int8 (PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out);
PDC_int16  PDCI_ebc2int16(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out);
PDC_int32  PDCI_ebc2int32(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out);
PDC_int64  PDCI_ebc2int64(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out);

ssize_t PDCI_int8_2ebc_buf (PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_int8  i, PDC_uint32 num_digits);
ssize_t PDCI_int16_2ebc_buf(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_int16 i, PDC_uint32 num_digits);
ssize_t PDCI_int32_2ebc_buf(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_int32 i, PDC_uint32 num_digits);
ssize_t PDCI_int64_2ebc_buf(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_int64 i, PDC_uint32 num_digits);

ssize_t PDCI_int8_2ebc_io (PDC_t *pdc, Sfio_t *io, PDC_int8  i, PDC_uint32 num_digits);
ssize_t PDCI_int16_2ebc_io(PDC_t *pdc, Sfio_t *io, PDC_int16 i, PDC_uint32 num_digits);
ssize_t PDCI_int32_2ebc_io(PDC_t *pdc, Sfio_t *io, PDC_int32 i, PDC_uint32 num_digits);
ssize_t PDCI_int64_2ebc_io(PDC_t *pdc, Sfio_t *io, PDC_int64 i, PDC_uint32 num_digits);

PDC_uint8   PDCI_ebc2uint8 (PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out);
PDC_uint16  PDCI_ebc2uint16(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out);
PDC_uint32  PDCI_ebc2uint32(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out);
PDC_uint64  PDCI_ebc2uint64(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out);

ssize_t PDCI_uint8_2ebc_buf (PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_uint8  u, PDC_uint32 num_digits);
ssize_t PDCI_uint16_2ebc_buf(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_uint16 u, PDC_uint32 num_digits);
ssize_t PDCI_uint32_2ebc_buf(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_uint32 u, PDC_uint32 num_digits);
ssize_t PDCI_uint64_2ebc_buf(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_uint64 u, PDC_uint32 num_digits);

ssize_t PDCI_uint8_2ebc_io (PDC_t *pdc, Sfio_t *io, PDC_uint8  u, PDC_uint32 num_digits);
ssize_t PDCI_uint16_2ebc_io(PDC_t *pdc, Sfio_t *io, PDC_uint16 u, PDC_uint32 num_digits);
ssize_t PDCI_uint32_2ebc_io(PDC_t *pdc, Sfio_t *io, PDC_uint32 u, PDC_uint32 num_digits);
ssize_t PDCI_uint64_2ebc_io(PDC_t *pdc, Sfio_t *io, PDC_uint64 u, PDC_uint32 num_digits);

PDC_int8   PDCI_bcd2int8 (PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out);
PDC_int16  PDCI_bcd2int16(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out);
PDC_int32  PDCI_bcd2int32(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out);
PDC_int64  PDCI_bcd2int64(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out);

ssize_t PDCI_int8_2bcd_buf (PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_int8  i, PDC_uint32 num_digits);
ssize_t PDCI_int16_2bcd_buf(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_int16 i, PDC_uint32 num_digits);
ssize_t PDCI_int32_2bcd_buf(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_int32 i, PDC_uint32 num_digits);
ssize_t PDCI_int64_2bcd_buf(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_int64 i, PDC_uint32 num_digits);

ssize_t PDCI_int8_2bcd_io (PDC_t *pdc, Sfio_t *io, PDC_int8  i, PDC_uint32 num_digits);
ssize_t PDCI_int16_2bcd_io(PDC_t *pdc, Sfio_t *io, PDC_int16 i, PDC_uint32 num_digits);
ssize_t PDCI_int32_2bcd_io(PDC_t *pdc, Sfio_t *io, PDC_int32 i, PDC_uint32 num_digits);
ssize_t PDCI_int64_2bcd_io(PDC_t *pdc, Sfio_t *io, PDC_int64 i, PDC_uint32 num_digits);

PDC_uint8   PDCI_bcd2uint8 (PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out);
PDC_uint16  PDCI_bcd2uint16(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out);
PDC_uint32  PDCI_bcd2uint32(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out);
PDC_uint64  PDCI_bcd2uint64(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_digits, PDC_byte **ptr_out);

ssize_t PDCI_uint8_2bcd_buf (PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_uint8  u, PDC_uint32 num_digits);
ssize_t PDCI_uint16_2bcd_buf(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_uint16 u, PDC_uint32 num_digits);
ssize_t PDCI_uint32_2bcd_buf(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_uint32 u, PDC_uint32 num_digits);
ssize_t PDCI_uint64_2bcd_buf(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_uint64 u, PDC_uint32 num_digits);

ssize_t PDCI_uint8_2bcd_io (PDC_t *pdc, Sfio_t *io, PDC_uint8  u, PDC_uint32 num_digits);
ssize_t PDCI_uint16_2bcd_io(PDC_t *pdc, Sfio_t *io, PDC_uint16 u, PDC_uint32 num_digits);
ssize_t PDCI_uint32_2bcd_io(PDC_t *pdc, Sfio_t *io, PDC_uint32 u, PDC_uint32 num_digits);
ssize_t PDCI_uint64_2bcd_io(PDC_t *pdc, Sfio_t *io, PDC_uint64 u, PDC_uint32 num_digits);

PDC_int8   PDCI_sbl2int8 (PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out);
PDC_int16  PDCI_sbl2int16(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out);
PDC_int32  PDCI_sbl2int32(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out);
PDC_int64  PDCI_sbl2int64(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out);

ssize_t PDCI_int8_2sbl_buf (PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_int8  i, PDC_uint32 num_bytes);
ssize_t PDCI_int16_2sbl_buf(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_int16 i, PDC_uint32 num_bytes);
ssize_t PDCI_int32_2sbl_buf(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_int32 i, PDC_uint32 num_bytes);
ssize_t PDCI_int64_2sbl_buf(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_int64 i, PDC_uint32 num_bytes);

ssize_t PDCI_int8_2sbl_io (PDC_t *pdc, Sfio_t *io, PDC_int8  i, PDC_uint32 num_bytes);
ssize_t PDCI_int16_2sbl_io(PDC_t *pdc, Sfio_t *io, PDC_int16 i, PDC_uint32 num_bytes);
ssize_t PDCI_int32_2sbl_io(PDC_t *pdc, Sfio_t *io, PDC_int32 i, PDC_uint32 num_bytes);
ssize_t PDCI_int64_2sbl_io(PDC_t *pdc, Sfio_t *io, PDC_int64 i, PDC_uint32 num_bytes);

PDC_uint8   PDCI_sbl2uint8 (PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out);
PDC_uint16  PDCI_sbl2uint16(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out);
PDC_uint32  PDCI_sbl2uint32(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out);
PDC_uint64  PDCI_sbl2uint64(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out);

ssize_t PDCI_uint8_2sbl_buf (PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_uint8  u, PDC_uint32 num_bytes);
ssize_t PDCI_uint16_2sbl_buf(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_uint16 u, PDC_uint32 num_bytes);
ssize_t PDCI_uint32_2sbl_buf(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_uint32 u, PDC_uint32 num_bytes);
ssize_t PDCI_uint64_2sbl_buf(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_uint64 u, PDC_uint32 num_bytes);

ssize_t PDCI_uint8_2sbl_io (PDC_t *pdc, Sfio_t *io, PDC_uint8  u, PDC_uint32 num_bytes);
ssize_t PDCI_uint16_2sbl_io(PDC_t *pdc, Sfio_t *io, PDC_uint16 u, PDC_uint32 num_bytes);
ssize_t PDCI_uint32_2sbl_io(PDC_t *pdc, Sfio_t *io, PDC_uint32 u, PDC_uint32 num_bytes);
ssize_t PDCI_uint64_2sbl_io(PDC_t *pdc, Sfio_t *io, PDC_uint64 u, PDC_uint32 num_bytes);

PDC_int8   PDCI_sbh2int8 (PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out);
PDC_int16  PDCI_sbh2int16(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out);
PDC_int32  PDCI_sbh2int32(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out);
PDC_int64  PDCI_sbh2int64(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out);

ssize_t PDCI_int8_2sbh_buf (PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_int8  i, PDC_uint32 num_bytes);
ssize_t PDCI_int16_2sbh_buf(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_int16 i, PDC_uint32 num_bytes);
ssize_t PDCI_int32_2sbh_buf(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_int32 i, PDC_uint32 num_bytes);
ssize_t PDCI_int64_2sbh_buf(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_int64 i, PDC_uint32 num_bytes);

ssize_t PDCI_int8_2sbh_io (PDC_t *pdc, Sfio_t *io, PDC_int8  i, PDC_uint32 num_bytes);
ssize_t PDCI_int16_2sbh_io(PDC_t *pdc, Sfio_t *io, PDC_int16 i, PDC_uint32 num_bytes);
ssize_t PDCI_int32_2sbh_io(PDC_t *pdc, Sfio_t *io, PDC_int32 i, PDC_uint32 num_bytes);
ssize_t PDCI_int64_2sbh_io(PDC_t *pdc, Sfio_t *io, PDC_int64 i, PDC_uint32 num_bytes);

PDC_uint8   PDCI_sbh2uint8 (PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out);
PDC_uint16  PDCI_sbh2uint16(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out);
PDC_uint32  PDCI_sbh2uint32(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out);
PDC_uint64  PDCI_sbh2uint64(PDC_t *pdc, const PDC_byte *bytes, PDC_uint32 num_bytes, PDC_byte **ptr_out);

ssize_t PDCI_uint8_2sbh_buf (PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_uint8  u, PDC_uint32 num_bytes);
ssize_t PDCI_uint16_2sbh_buf(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_uint16 u, PDC_uint32 num_bytes);
ssize_t PDCI_uint32_2sbh_buf(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_uint32 u, PDC_uint32 num_bytes);
ssize_t PDCI_uint64_2sbh_buf(PDC_t *pdc, PDC_byte *outbuf, size_t outbuf_len, int *outbuf_full, PDC_uint64 u, PDC_uint32 num_bytes);

ssize_t PDCI_uint8_2sbh_io (PDC_t *pdc, Sfio_t *io, PDC_uint8  u, PDC_uint32 num_bytes);
ssize_t PDCI_uint16_2sbh_io(PDC_t *pdc, Sfio_t *io, PDC_uint16 u, PDC_uint32 num_bytes);
ssize_t PDCI_uint32_2sbh_io(PDC_t *pdc, Sfio_t *io, PDC_uint32 u, PDC_uint32 num_bytes);
ssize_t PDCI_uint64_2sbh_io(PDC_t *pdc, Sfio_t *io, PDC_uint64 u, PDC_uint32 num_bytes);

/* ================================================================================ */
/* INTERNAL MISC ROUTINES */


/* Internal version of PDC_regexp_compile, takes whatfn */
PDC_error_t
PDCI_regexp_compile(PDC_t *pdc, const char *regexp, PDC_regexp_t **regexp_out, const char *whatfn);

/*  PDCI_regexpMatch returns the number of characters in str that match regexp
 *  (or 0 if str does not match the regular expression).
 */
size_t PDCI_regexpMatch(PDC_t *pdc, PDC_regexp_t *regexp, PDC_byte *begin, PDC_byte *end, PDC_charset char_set);

/* Accum impl helpers:
 *
 * PDCI_nst_prefix_what prints a heading to outstr 
 * based on *nst nesting level and
 * (unless *nst is -1) it increments the nesting level.
 */

void PDCI_nst_prefix_what(Sfio_t *outstr, int *nst, const char *prefix, const char *what);

/* 
 * PDCI_findfirst and PDCI_findlast are like strchr and strrchr except NULL does
 * not terminate the search: instead, begin/end bracket the search space, where
 * end is one byte beyond the last byte to check.
 */ 
PDC_byte *PDCI_findfirst(const PDC_byte *begin, const PDC_byte *end, PDC_byte b);
PDC_byte *PDCI_findlast(const PDC_byte *begin, const PDC_byte *end, PDC_byte b);


/* ================================================================================ */

#endif /*  __LIBPADSC_INTERNAL__  */
