#ifdef _USE_PROTO
#pragma prototyped
#endif
/*
 * PDC ckit prototypes for output macros
 * 
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#ifndef __LIBPADSC_CKIT_H__
#define __LIBPADSC_CKIT_H__

/* ================================================================================ */
/* XXX THESE ARE REQUIRED FOR CKIT ON CERTAIN ARCHITECTURES ??? */

#ifdef __PREPROCESSOR_FIXES
typedef void * __builtin_va_list;
#define __THROW
/* extern int ftruncate (int __fd, long int __length) ; */

#endif
/* ================================================================================ */
/* MISC MACROS THAT NEED PROTOTYPES */

#undef RBuf_CPY_SRC2DEST
void RBuf_CPY_SRC2DEST(RBuf_t*, RBuf_t*, void *, size_t, RMM_t*);

/* ================================================================================ */
/* SIMPLE CONSTANTS */

/* undef FOO, then declare FOO as an extern */

#undef PDC_NULL_CTL_FLAG
extern unsigned long PDC_NULL_CTL_FLAG;
#undef PDC_WSPACE_OK
extern unsigned long PDC_WSPACE_OK;

/* ================================================================================ */
/* MACROS IN libpadsc.h THAT NEED PROTOTYPES */

#undef PDC_int8_read
PDC_error_t PDC_int8_read (PDC_t *pdc, PDC_base_csm *csm,
			    PDC_base_ed *ed, PDC_int8 *res_out);

#undef PDC_int16_read
PDC_error_t PDC_int16_read(PDC_t *pdc, PDC_base_csm *csm,
			   PDC_base_ed *ed, PDC_int16 *res_out);

#undef PDC_int32_read
PDC_error_t PDC_int32_read(PDC_t *pdc, PDC_base_csm *csm,
			   PDC_base_ed *ed, PDC_int32 *res_out);

#undef PDC_int64_read
PDC_error_t PDC_int64_read(PDC_t *pdc, PDC_base_csm *csm,
			   PDC_base_ed *ed, PDC_int64 *res_out);

#undef PDC_uint8_read
PDC_error_t PDC_uint8_read (PDC_t *pdc, PDC_base_csm *csm,
			    PDC_base_ed *ed, PDC_uint8 *res_out);

#undef PDC_uint16_read
PDC_error_t PDC_uint16_read(PDC_t *pdc, PDC_base_csm *csm,
			    PDC_base_ed *ed, PDC_uint16 *res_out);

#undef PDC_uint32_read
PDC_error_t PDC_uint32_read(PDC_t *pdc, PDC_base_csm *csm,
			    PDC_base_ed *ed, PDC_uint32 *res_out);

#undef PDC_uint64_read
PDC_error_t PDC_uint64_read(PDC_t *pdc, PDC_base_csm *csm,
			    PDC_base_ed *ed, PDC_uint64 *res_out);

#undef PDC_int8_FW_read
PDC_error_t PDC_int8_FW_read (PDC_t *pdc, PDC_base_csm *csm, size_t width,
			      PDC_base_ed *ed, PDC_int8 *res_out);

#undef PDC_int16_FW_read
PDC_error_t PDC_int16_FW_read(PDC_t *pdc, PDC_base_csm *csm, size_t width,
			      PDC_base_ed *ed, PDC_int16 *res_out);

#undef PDC_int32_FW_read
PDC_error_t PDC_int32_FW_read(PDC_t *pdc, PDC_base_csm *csm, size_t width,
			      PDC_base_ed *ed, PDC_int32 *res_out);

#undef PDC_int64_FW_read
PDC_error_t PDC_int64_FW_read(PDC_t *pdc, PDC_base_csm *csm, size_t width,
			      PDC_base_ed *ed, PDC_int64 *res_out);

#undef PDC_uint8_FW_read
PDC_error_t PDC_uint8_FW_read (PDC_t *pdc, PDC_base_csm *csm, size_t width,
			       PDC_base_ed *ed, PDC_uint8 *res_out);

#undef PDC_uint16_FW_read
PDC_error_t PDC_uint16_FW_read(PDC_t *pdc, PDC_base_csm *csm, size_t width,
			       PDC_base_ed *ed, PDC_uint16 *res_out);

#undef PDC_uint32_FW_read
PDC_error_t PDC_uint32_FW_read(PDC_t *pdc, PDC_base_csm *csm, size_t width,
			       PDC_base_ed *ed, PDC_uint32 *res_out);

#undef PDC_uint64_FW_read
PDC_error_t PDC_uint64_FW_read(PDC_t *pdc, PDC_base_csm *csm, size_t width,
			       PDC_base_ed *ed, PDC_uint64 *res_out);

#undef PDC_int8_write2io
#undef PDC_int16_write2io
#undef PDC_int32_write2io
#undef PDC_int64_write2io
int PDC_int8_write2io  (PDC_t *pdc, Sfio_t *io, PDC_base_ed *ed, PDC_int8   *val);
int PDC_int16_write2io (PDC_t *pdc, Sfio_t *io, PDC_base_ed *ed, PDC_int16  *val);
int PDC_int32_write2io (PDC_t *pdc, Sfio_t *io, PDC_base_ed *ed, PDC_int32  *val);
int PDC_int64_write2io (PDC_t *pdc, Sfio_t *io, PDC_base_ed *ed, PDC_int64  *val);

#undef PDC_uint8_write2io
#undef PDC_uint16_write2io
#undef PDC_uint32_write2io
#undef PDC_uint64_write2io
int PDC_uint8_write2io (PDC_t *pdc, Sfio_t *io, PDC_base_ed *ed, PDC_uint8  *val);
int PDC_uint16_write2io(PDC_t *pdc, Sfio_t *io, PDC_base_ed *ed, PDC_uint16 *val);
int PDC_uint32_write2io(PDC_t *pdc, Sfio_t *io, PDC_base_ed *ed, PDC_uint32 *val);
int PDC_uint64_write2io(PDC_t *pdc, Sfio_t *io, PDC_base_ed *ed, PDC_uint64 *val);

#undef PDC_int8_write2buf
#undef PDC_int16_write2buf
#undef PDC_int32_write2buf
#undef PDC_int64_write2buf
int PDC_int8_write2buf  (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_ed *ed, PDC_int8   *val);
int PDC_int16_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_ed *ed, PDC_int16  *val);
int PDC_int32_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_ed *ed, PDC_int32  *val);
int PDC_int64_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_ed *ed, PDC_int64  *val);

#undef PDC_uint8_write2buf
#undef PDC_uint16_write2buf
#undef PDC_uint32_write2buf
#undef PDC_uint64_write2buf
int PDC_uint8_write2buf (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_ed *ed, PDC_uint8  *val);
int PDC_uint16_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_ed *ed, PDC_uint16 *val);
int PDC_uint32_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_ed *ed, PDC_uint32 *val);
int PDC_uint64_write2buf(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_ed *ed, PDC_uint64 *val);

/* ================================================================================ */
/* MACROS IN libpadsc-internal.h THAT NEED PROTOTYPES */

#undef PDC_char_lit_scan_internal
PDC_error_t PDC_char_lit_scan_internal(PDC_t *pdc, PDC_byte c, PDC_byte s, int eat_lit,
				       PDC_byte *c_out, size_t *offset_out);

#undef PDC_a_char_lit_scan_internal
PDC_error_t PDC_a_char_lit_scan_internal(PDC_t *pdc, PDC_byte c, PDC_byte s, int eat_lit,
					 PDC_byte *c_out, size_t *offset_out);

#undef PDC_e_char_lit_scan_internal
PDC_error_t PDC_e_char_lit_scan_internal(PDC_t *pdc, PDC_byte c, PDC_byte s, int eat_lit,
					 PDC_byte *c_out, size_t *offset_out);

#undef PDC_str_lit_scan_internal
PDC_error_t PDC_str_lit_scan_internal(PDC_t *pdc, const PDC_string *findStr, const PDC_string *stopStr, int eat_lit,
				      PDC_string **str_out, size_t *offset_out);

#undef PDC_a_str_lit_scan_internal
PDC_error_t PDC_a_str_lit_scan_internal(PDC_t *pdc, const PDC_string *findStr, const PDC_string *stopStr, int eat_lit,
					PDC_string **str_out, size_t *offset_out);

#undef PDC_e_str_lit_scan_internal
PDC_error_t PDC_e_str_lit_scan_internal(PDC_t *pdc, const PDC_string *findStr, const PDC_string *stopStr, int eat_lit,
					PDC_string **str_out, size_t *offset_out);

#undef PDC_Cstr_lit_scan_internal
PDC_error_t PDC_Cstr_lit_scan_internal(PDC_t *pdc, const char *findStr, const char *stopStr, int eat_lit,
				       const char **str_out, size_t *offset_out);

#undef PDC_a_Cstr_lit_scan_internal
PDC_error_t PDC_a_Cstr_lit_scan_internal(PDC_t *pdc, const char *findStr, const char *stopStr, int eat_lit,
					 const char **str_out, size_t *offset_out);

#undef PDC_e_Cstr_lit_scan_internal
PDC_error_t PDC_e_Cstr_lit_scan_internal(PDC_t *pdc, const char *findStr, const char *stopStr, int eat_lit,
					 const char **str_out, size_t *offset_out);

#undef PDC_char_lit_read_internal
PDC_error_t PDC_char_lit_read_internal(PDC_t *pdc, PDC_base_csm *csm,
				       PDC_base_ed *ed, PDC_byte c);

#undef PDC_a_char_lit_read_internal
PDC_error_t PDC_a_char_lit_read_internal(PDC_t *pdc, PDC_base_csm *csm,
					 PDC_base_ed *ed, PDC_byte c);

#undef PDC_e_char_lit_read_internal
PDC_error_t PDC_e_char_lit_read_internal(PDC_t *pdc, PDC_base_csm *csm,
					 PDC_base_ed *ed, PDC_byte c);

#undef PDC_str_lit_read_internal
PDC_error_t PDC_str_lit_read_internal(PDC_t *pdc, PDC_base_csm *csm,
				      PDC_base_ed *ed, const PDC_string *s);

#undef PDC_a_str_lit_read_internal
PDC_error_t PDC_a_str_lit_read_internal(PDC_t *pdc, PDC_base_csm *csm,
					PDC_base_ed *ed, const PDC_string *s);

#undef PDC_e_str_lit_read_internal
PDC_error_t PDC_e_str_lit_read_internal(PDC_t *pdc, PDC_base_csm *csm,
					PDC_base_ed *ed, const PDC_string *s);

#undef PDC_Cstr_lit_read_internal
PDC_error_t PDC_Cstr_lit_read_internal(PDC_t *pdc, PDC_base_csm *csm,
				      PDC_base_ed *ed, const char *s);

#undef PDC_a_Cstr_lit_read_internal
PDC_error_t PDC_a_Cstr_lit_read_internal(PDC_t *pdc, PDC_base_csm *csm,
					PDC_base_ed *ed, const char *s);

#undef PDC_e_Cstr_lit_read_internal
PDC_error_t PDC_e_Cstr_lit_read_internal(PDC_t *pdc, PDC_base_csm *csm,
					PDC_base_ed *ed, const char *s);

#undef PDC_countX_internal
PDC_error_t PDC_countX_internal(PDC_t *pdc, PDC_base_csm *csm, PDC_uint8 x, int eor_required,
				PDC_base_ed *ed, PDC_int32 *res_out);

#undef PDC_a_countX_internal
PDC_error_t PDC_a_countX_internal(PDC_t *pdc, PDC_base_csm *csm, PDC_uint8 x, int eor_required,
				  PDC_base_ed *ed, PDC_int32 *res_out);

#undef PDC_e_countX_internal
PDC_error_t PDC_e_countX_internal(PDC_t *pdc, PDC_base_csm *csm, PDC_uint8 x, int eor_required,
				  PDC_base_ed *ed, PDC_int32 *res_out);

#undef PDC_countXtoY_internal
PDC_error_t PDC_countXtoY_internal(PDC_t *pdc, PDC_base_csm *csm, PDC_uint8 x, PDC_uint8 y,
				   PDC_base_ed *ed, PDC_int32 *res_out);

#undef PDC_a_countXtoY_internal
PDC_error_t PDC_a_countXtoY_internal(PDC_t *pdc, PDC_base_csm *csm, PDC_uint8 x, PDC_uint8 y,
				     PDC_base_ed *ed, PDC_int32 *res_out);

#undef PDC_e_countXtoY_internal
PDC_error_t PDC_e_countXtoY_internal(PDC_t *pdc, PDC_base_csm *csm, PDC_uint8 x, PDC_uint8 y,
				     PDC_base_ed *ed, PDC_int32 *res_out);

#undef PDC_date_read_internal
PDC_error_t PDC_date_read_internal(PDC_t *pdc, PDC_base_csm *csm, PDC_byte stopChar,
				   PDC_base_ed *ed, PDC_uint32 *res_out);

#undef PDC_a_date_read_internal
PDC_error_t PDC_a_date_read_internal(PDC_t *pdc, PDC_base_csm *csm, PDC_byte stopChar,
				     PDC_base_ed *ed, PDC_uint32 *res_out);

#undef PDC_e_date_read_internal
PDC_error_t PDC_e_date_read_internal(PDC_t *pdc, PDC_base_csm *csm, PDC_byte stopChar,
				     PDC_base_ed *ed, PDC_uint32 *res_out);

#undef PDC_char_read_internal 
PDC_error_t PDC_char_read_internal (PDC_t *pdc, PDC_base_csm *csm, PDC_base_ed *ed, PDC_char *c_out);

#undef PDC_a_char_read_internal 
PDC_error_t PDC_a_char_read_internal (PDC_t *pdc, PDC_base_csm *csm, PDC_base_ed *ed, PDC_char *c_out);

#undef PDC_e_char_read_internal 
PDC_error_t PDC_e_char_read_internal (PDC_t *pdc, PDC_base_csm *csm, PDC_base_ed *ed, PDC_char *c_out);

#undef PDC_string_FW_read_internal
PDC_error_t PDC_string_FW_read_internal(PDC_t *pdc, PDC_base_csm *csm, size_t width,
					PDC_base_ed *ed, PDC_string *s_out);

#undef PDC_a_string_FW_read_internal
PDC_error_t PDC_a_string_FW_read_internal(PDC_t *pdc, PDC_base_csm *csm, size_t width,
					  PDC_base_ed *ed, PDC_string *s_out);

#undef PDC_e_string_FW_read_internal
PDC_error_t PDC_e_string_FW_read_internal(PDC_t *pdc, PDC_base_csm *csm, size_t width,
					  PDC_base_ed *ed, PDC_string *s_out);

#undef PDC_string_read_internal
PDC_error_t PDC_string_read_internal(PDC_t *pdc, PDC_base_csm *csm, PDC_byte stopChar,
				     PDC_base_ed *ed, PDC_string *s_out);

#undef PDC_a_string_read_internal
PDC_error_t PDC_a_string_read_internal(PDC_t *pdc, PDC_base_csm *csm, PDC_byte stopChar,
				       PDC_base_ed *ed, PDC_string *s_out);

#undef PDC_e_string_read_internal
PDC_error_t PDC_e_string_read_internal(PDC_t *pdc, PDC_base_csm *csm, PDC_byte stopChar,
				       PDC_base_ed *ed, PDC_string *s_out);

#undef PDC_string_ME_read_internal
PDC_error_t PDC_string_ME_read_internal(PDC_t *pdc, PDC_base_csm *csm, const char *matchRegexp,
					PDC_base_ed *ed, PDC_string *s_out);

#undef PDC_a_string_ME_read_internal
PDC_error_t PDC_a_string_ME_read_internal(PDC_t *pdc, PDC_base_csm *csm, const char *matchRegexp,
					  PDC_base_ed *ed, PDC_string *s_out);

#undef PDC_e_string_ME_read_internal
PDC_error_t PDC_e_string_ME_read_internal(PDC_t *pdc, PDC_base_csm *csm, const char *matchRegexp,
					  PDC_base_ed *ed, PDC_string *s_out);

#undef PDC_string_CME_read_internal
PDC_error_t PDC_string_CME_read_internal(PDC_t *pdc, PDC_base_csm *csm, PDC_regexp_t *matchRegexp,
					 PDC_base_ed *ed, PDC_string *s_out);

#undef PDC_a_string_CME_read_internal
PDC_error_t PDC_a_string_CME_read_internal(PDC_t *pdc, PDC_base_csm *csm, PDC_regexp_t *matchRegexp,
					   PDC_base_ed *ed, PDC_string *s_out);

#undef PDC_e_string_CME_read_internal
PDC_error_t PDC_e_string_CME_read_internal(PDC_t *pdc, PDC_base_csm *csm, PDC_regexp_t *matchRegexp,
					   PDC_base_ed *ed, PDC_string *s_out);

#undef PDC_string_SE_read_internal
PDC_error_t PDC_string_SE_read_internal(PDC_t *pdc, PDC_base_csm *csm, const char *stopRegexp,
					PDC_base_ed *ed, PDC_string *s_out);

#undef PDC_a_string_SE_read_internal
PDC_error_t PDC_a_string_SE_read_internal(PDC_t *pdc, PDC_base_csm *csm, const char *stopRegexp,
					  PDC_base_ed *ed, PDC_string *s_out);

#undef PDC_e_string_SE_read_internal
PDC_error_t PDC_e_string_SE_read_internal(PDC_t *pdc, PDC_base_csm *csm, const char *stopRegexp,
					  PDC_base_ed *ed, PDC_string *s_out);

#undef PDC_string_CSE_read_internal
PDC_error_t PDC_string_CSE_read_internal(PDC_t *pdc, PDC_base_csm *csm, PDC_regexp_t *stopRegexp,
					 PDC_base_ed *ed, PDC_string *s_out);

#undef PDC_a_string_CSE_read_internal
PDC_error_t PDC_a_string_CSE_read_internal(PDC_t *pdc, PDC_base_csm *csm, PDC_regexp_t *stopRegexp,
					   PDC_base_ed *ed, PDC_string *s_out);

#undef PDC_e_string_CSE_read_internal
PDC_error_t PDC_e_string_CSE_read_internal(PDC_t *pdc, PDC_base_csm *csm, PDC_regexp_t *stopRegexp,
					   PDC_base_ed *ed, PDC_string *s_out);

#undef PDC_int8_read_internal
PDC_error_t PDC_int8_read_internal (PDC_t *pdc, PDC_base_csm *csm,
				    PDC_base_ed *ed, PDC_int8 *res_out);

#undef PDC_int16_read_internal
PDC_error_t PDC_int16_read_internal(PDC_t *pdc, PDC_base_csm *csm,
				    PDC_base_ed *ed, PDC_int16 *res_out);

#undef PDC_int32_read_internal
PDC_error_t PDC_int32_read_internal(PDC_t *pdc, PDC_base_csm *csm,
				    PDC_base_ed *ed, PDC_int32 *res_out);

#undef PDC_int64_read_internal
PDC_error_t PDC_int64_read_internal(PDC_t *pdc, PDC_base_csm *csm,
				    PDC_base_ed *ed, PDC_int64 *res_out);

#undef PDC_uint8_read_internal
PDC_error_t PDC_uint8_read_internal (PDC_t *pdc, PDC_base_csm *csm,
				     PDC_base_ed *ed, PDC_uint8 *res_out);

#undef PDC_uint16_read_internal
PDC_error_t PDC_uint16_read_internal(PDC_t *pdc, PDC_base_csm *csm,
				     PDC_base_ed *ed, PDC_uint16 *res_out);

#undef PDC_uint32_read_internal
PDC_error_t PDC_uint32_read_internal(PDC_t *pdc, PDC_base_csm *csm,
				     PDC_base_ed *ed, PDC_uint32 *res_out);

#undef PDC_uint64_read_internal
PDC_error_t PDC_uint64_read_internal(PDC_t *pdc, PDC_base_csm *csm,
				     PDC_base_ed *ed, PDC_uint64 *res_out);

#undef PDC_int8_FW_read_internal
PDC_error_t PDC_int8_FW_read_internal (PDC_t *pdc, PDC_base_csm *csm, size_t width,
				       PDC_base_ed *ed, PDC_int8 *res_out);

#undef PDC_int16_FW_read_internal
PDC_error_t PDC_int16_FW_read_internal(PDC_t *pdc, PDC_base_csm *csm, size_t width,
				       PDC_base_ed *ed, PDC_int16 *res_out);

#undef PDC_int32_FW_read_internal
PDC_error_t PDC_int32_FW_read_internal(PDC_t *pdc, PDC_base_csm *csm, size_t width,
				       PDC_base_ed *ed, PDC_int32 *res_out);

#undef PDC_int64_FW_read_internal
PDC_error_t PDC_int64_FW_read_internal(PDC_t *pdc, PDC_base_csm *csm, size_t width,
				       PDC_base_ed *ed, PDC_int64 *res_out);

#undef PDC_uint8_FW_read_internal
PDC_error_t PDC_uint8_FW_read_internal (PDC_t *pdc, PDC_base_csm *csm, size_t width,
					PDC_base_ed *ed, PDC_uint8 *res_out);

#undef PDC_uint16_FW_read_internal
PDC_error_t PDC_uint16_FW_read_internal(PDC_t *pdc, PDC_base_csm *csm, size_t width,
					PDC_base_ed *ed, PDC_uint16 *res_out);

#undef PDC_uint32_FW_read_internal
PDC_error_t PDC_uint32_FW_read_internal(PDC_t *pdc, PDC_base_csm *csm, size_t width,
					PDC_base_ed *ed, PDC_uint32 *res_out);

#undef PDC_uint64_FW_read_internal
PDC_error_t PDC_uint64_FW_read_internal(PDC_t *pdc, PDC_base_csm *csm, size_t width,
					PDC_base_ed *ed, PDC_uint64 *res_out);

#undef PDC_int8_write2io_internal
#undef PDC_int16_write2io_internal
#undef PDC_int32_write2io_internal
#undef PDC_int64_write2io_internal
int PDC_int8_write2io_internal  (PDC_t *pdc, Sfio_t *io, PDC_base_ed *ed, PDC_int8   *val);
int PDC_int16_write2io_internal (PDC_t *pdc, Sfio_t *io, PDC_base_ed *ed, PDC_int16  *val);
int PDC_int32_write2io_internal (PDC_t *pdc, Sfio_t *io, PDC_base_ed *ed, PDC_int32  *val);
int PDC_int64_write2io_internal (PDC_t *pdc, Sfio_t *io, PDC_base_ed *ed, PDC_int64  *val);

#undef PDC_uint8_write2io_internal
#undef PDC_uint16_write2io_internal
#undef PDC_uint32_write2io_internal
#undef PDC_uint64_write2io_internal
int PDC_uint8_write2io_internal (PDC_t *pdc, Sfio_t *io, PDC_base_ed *ed, PDC_uint8  *val);
int PDC_uint16_write2io_internal(PDC_t *pdc, Sfio_t *io, PDC_base_ed *ed, PDC_uint16 *val);
int PDC_uint32_write2io_internal(PDC_t *pdc, Sfio_t *io, PDC_base_ed *ed, PDC_uint32 *val);
int PDC_uint64_write2io_internal(PDC_t *pdc, Sfio_t *io, PDC_base_ed *ed, PDC_uint64 *val);

#undef PDC_int8_write2buf_internal
#undef PDC_int16_write2buf_internal
#undef PDC_int32_write2buf_internal
#undef PDC_int64_write2buf_internal
int PDC_int8_write2buf_internal  (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_ed *ed, PDC_int8   *val);
int PDC_int16_write2buf_internal (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_ed *ed, PDC_int16  *val);
int PDC_int32_write2buf_internal (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_ed *ed, PDC_int32  *val);
int PDC_int64_write2buf_internal (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_ed *ed, PDC_int64  *val);

#undef PDC_uint8_write2buf_internal
#undef PDC_uint16_write2buf_internal
#undef PDC_uint32_write2buf_internal
#undef PDC_uint64_write2buf_internal
int PDC_uint8_write2buf_internal (PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_ed *ed, PDC_uint8  *val);
int PDC_uint16_write2buf_internal(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_ed *ed, PDC_uint16 *val);
int PDC_uint32_write2buf_internal(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_ed *ed, PDC_uint32 *val);
int PDC_uint64_write2buf_internal(PDC_t *pdc, PDC_byte *buf, size_t buf_len, int *buf_full, PDC_base_ed *ed, PDC_uint64 *val);

/* ================================================================================ */
/* OUTPUT MACROS AND RELATED CONSTS */

/* first undefine all the output macros/defines */

#undef PDC_LEV_INFO
#undef PDC_LEV_WARN
#undef PDC_LEV_ERR
#undef PDC_LEV_FATAL
#undef PDC_LEV_MASK

#undef PDC_FLG_PROMPT
#undef PDC_FLG_SYSERR
#undef PDC_FLG_LIBRARY


#undef PDC_GET_LEV
#undef PDC_GET_FLG

#undef PDC_DBG
#undef PDC_DBG1
#undef PDC_DBG2
#undef PDC_DBG3
#undef PDC_DBG4
#undef PDC_DBG5

#undef PDC_TRACE
#undef PDC_TRACE1
#undef PDC_TRACE2
#undef PDC_TRACE3
#undef PDC_TRACE4
#undef PDC_TRACE5

#undef PDC_WARN
#undef PDC_WARN1
#undef PDC_WARN2
#undef PDC_WARN3
#undef PDC_WARN4
#undef PDC_WARN5

#undef PDC_SYSERR
#undef PDC_SYSERR1
#undef PDC_SYSERR2
#undef PDC_SYSERR3
#undef PDC_SYSERR4
#undef PDC_SYSERR5

#undef PDC_FATAL
#undef PDC_FATAL1
#undef PDC_FATAL2
#undef PDC_FATAL3
#undef PDC_FATAL4
#undef PDC_FATAL5

/* Now redeclare them as constants and functions */

extern int PDC_LEV_INFO;
extern int PDC_LEV_WARN;
extern int PDC_LEV_ERR;
extern int PDC_LEV_FATAL;
extern int PDC_LEV_MASK;

extern int PDC_FLG_PROMPT;
extern int PDC_FLG_SYSERR;
extern int PDC_FLG_LIBRARY;

int PDC_GET_LEV(int flags);
int PDC_GET_FLG(int flags);

void PDC_DBG(PDC_disc_t *t, char * fmt);
void PDC_DBG1(PDC_disc_t *t, char * fmt,...);
void PDC_DBG2(PDC_disc_t *t, char * fmt,...);
void PDC_DBG3(PDC_disc_t *t, char * fmt,...);
void PDC_DBG4(PDC_disc_t *t, char * fmt,...);
void PDC_DBG5(PDC_disc_t *t, char * fmt,...);

void PDC_TRACE(PDC_disc_t *t, char * fmt);
void PDC_TRACE1(PDC_disc_t *t, char * fmt,...);
void PDC_TRACE2(PDC_disc_t *t, char * fmt,...);
void PDC_TRACE3(PDC_disc_t *t, char * fmt,...);
void PDC_TRACE4(PDC_disc_t *t, char * fmt,...);
void PDC_TRACE5(PDC_disc_t *t, char * fmt,...);

void PDC_WARN(PDC_disc_t *t, char * fmt);
void PDC_WARN1(PDC_disc_t *t, char * fmt,...);
void PDC_WARN2(PDC_disc_t *t, char * fmt,...);
void PDC_WARN3(PDC_disc_t *t, char * fmt,...);
void PDC_WARN4(PDC_disc_t *t, char * fmt,...);
void PDC_WARN5(PDC_disc_t *t, char * fmt,...);

void PDC_SYSERR(PDC_disc_t *t, char * fmt);
void PDC_SYSERR1(PDC_disc_t *t, char * fmt,...);
void PDC_SYSERR2(PDC_disc_t *t, char * fmt,...);
void PDC_SYSERR3(PDC_disc_t *t, char * fmt,...);
void PDC_SYSERR4(PDC_disc_t *t, char * fmt,...);
void PDC_SYSERR5(PDC_disc_t *t, char * fmt,...);

void PDC_FATAL(PDC_disc_t *t, char * fmt);
void PDC_FATAL1(PDC_disc_t *t, char * fmt,...);
void PDC_FATAL2(PDC_disc_t *t, char * fmt,...);
void PDC_FATAL3(PDC_disc_t *t, char * fmt,...);
void PDC_FATAL4(PDC_disc_t *t, char * fmt,...);
void PDC_FATAL5(PDC_disc_t *t, char * fmt,...);

/* ================================================================================ */

#endif /*  __LIBPADSC_CKIT__  */
