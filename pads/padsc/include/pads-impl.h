#ifdef _USE_PROTO
#pragma prototyped
#endif
/*
 * PADS library interface: macro implementations of certain function calls
 *   
 * NOTE: C prototypes for these functions are given in pads.h
 *
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#ifndef __PADS_IMPL_H__
#define __PADS_IMPL_H__

#ifndef FOR_CKIT

/* ================================================================================
 * Helper Macro
 * 
 * P_DEF_CHARSET(pads) : produces pads->disc->def_charset (or Pcharset_INVALID)
 *
 * PDCI_CHARSET_SWITCH(pads,fn_pre, fn_post, ,args) selects from a set of
 * functions based on pads->disc->def_charset.  It inserts an appropriate
 * charset marker ("a_", "e_", etc.) between fn_pre and fn_post to form a
 * function name, and calls that function with the specified args, which must be
 * given in parens.  If charset is not valid it will still choose one
 * of the possible functions -- that function should detect and report the problem.
 *
 * Example:
 *
 *    PDCI_CHARSET_SWITCH(pads, my, fun, (pads, x, y))
 *
 * Is converted to one of these forms:
 * 
 *    mya_fun(pads, x, y)
 *    mye_fun(pads, x, y)
 *    etc. 
 */

#ifndef NDEBUG
/* safe versions */

#define PDCI_DEF_CHARSET(pads) \
  ((pads && pads->disc) ? (pads->disc->def_charset) : Pcharset_INVALID)

#define PDCI_CHARSET_SWITCH(pads,fn_pre,fn_post,args) \
 ((pads && pads->disc && pads->disc->def_charset == Pcharset_ASCII) \
    ?  fn_pre ## a_ ## fn_post args \
    :  fn_pre ## e_ ## fn_post args)

#else
/* unsafe versions */

#define PDCI_DEF_CHARSET(pads) \
   (pads->disc->def_charset)

#define PDCI_CHARSET_SWITCH(pads,fn_pre,fn_post,args) \
 ((pads->disc->def_charset == Pcharset_ASCII) \
    ?  fn_pre ## a_ ## fn_post args \
    :  fn_pre ## e_ ## fn_post args)

#endif /* !NDEBUG */

/* Macros for setting or testing parse state (PS) pd->pstate */
/* These can be used with both Pbase_pd and with generated structured parse descriptors */

#define P_PS_init(pd)          do { (pd)->pstate = 0; } while (0)
#define P_PS_setPanic(pd)      do { (pd)->pstate |= P_Panic; } while (0)
#define P_PS_unsetPanic(pd)    do { (pd)->pstate &= ~P_Panic; } while (0)
#define P_PS_isPanic(pd)       ((pd)->pstate & P_Panic)

#define PD_COMMON_INIT(pd) do { \
  memset((void*)(pd), 0, sizeof(Pbase_pd)); \
} while (0)

#define PD_COMMON_INIT_NO_ERR(pd) do { \
  memset((void*)(pd), 0, sizeof(Pbase_pd)); \
  (pd)->errCode = P_NO_ERR; \
} while (0)

#define Pbase_pd_init(pd) PD_COMMON_INIT(pd)
#define Pbase_pd_init_no_err(pd) PD_COMMON_INIT_NO_ERR(pd)

/* ================================================================================ */
/* INTERNAL SCAN ROUTINES (helpers) */

#if P_CONFIG_READ_FUNCTIONS > 0

Perror_t PDCI_char_lit_scan1(P_t *pads, Pchar f, int eat_f, int panic,
			     size_t *offset_out, Pcharset char_set,
			     const char *whatfn);

Perror_t PDCI_char_lit_scan2(P_t *pads, Pchar f, Pchar s, int eat_f, int eat_s, int panic,
			     int *f_found_out, size_t *offset_out, Pcharset char_set,
			     const char *whatfn);

Perror_t PDCI_str_lit_scan1(P_t *pads, const Pstring *f,
			    int eat_f, int panic,
			    size_t *offset_out, Pcharset char_set,
			    const char *whatfn);

Perror_t PDCI_str_lit_scan2(P_t *pads, const Pstring *f, const Pstring *s,
			    int eat_f, int eat_s, int panic,
			    int *f_found_out, size_t *offset_out, Pcharset char_set,
			    const char *whatfn);

Perror_t PDCI_cstr_lit_scan1(P_t *pads, const char *f,
			     int eat_f, int panic,
			     size_t *offset_out, Pcharset char_set,
			     const char *whatfn);

Perror_t PDCI_cstr_lit_scan2(P_t *pads, const char *f, const char *s,
			     int eat_f, int eat_s, int panic,
			     int *f_found_out, size_t *offset_out, Pcharset char_set,
			     const char *whatfn);

Perror_t PDCI_re_scan1(P_t *pads, Pregexp_t *f,
		       int eat_f, int panic,
		       size_t *offset_out, Pcharset char_set,
		       const char *whatfn);

Perror_t PDCI_re_scan2(P_t *pads, Pregexp_t *f, Pregexp_t *s,
		       int eat_f, int eat_s, int panic,
		       int *f_found_out, size_t *offset_out, Pcharset char_set,
		       const char *whatfn);

#endif /* P_CONFIG_READ_FUNCTIONS */

/* ================================================================================ */
/* INTERNAL MATCH ROUTINES (helpers) */

#if P_CONFIG_READ_FUNCTIONS > 0

Perror_t PDCI_char_lit_match(P_t *pads, Pchar f, int eat_f, Pcharset char_set, const char *whatfn);

Perror_t PDCI_str_lit_match(P_t *pads, const Pstring *f, int eat_f, Pcharset char_set, const char *whatfn);

Perror_t PDCI_cstr_lit_match(P_t *pads, const char *f, int eat_f, Pcharset char_set, const char *whatfn);

Perror_t PDCI_re_match(P_t *pads, Pregexp_t *f, int eat_f, Pcharset char_set, const char *whatfn);

#endif /* P_CONFIG_READ_FUNCTIONS */

/* ================================================================================ */
/* INTERNAL READ ROUTINES (helpers) */

#if P_CONFIG_READ_FUNCTIONS > 0

Perror_t PDCI_char_lit_read(P_t *pads, const Pbase_m *m, Pchar c, Pbase_pd *pd, Pchar *c_out,
			    Pcharset char_set, const char* whatfn);

Perror_t PDCI_str_lit_read(P_t *pads, const Pbase_m *m, const Pstring *s, Pbase_pd *pd, Pstring *s_out,
			   Pcharset char_set, const char *whatfn);

Perror_t PDCI_cstr_lit_read(P_t *pads, const Pbase_m *m, const char *s, Pbase_pd *pd, Pstring *s_out,
			    Pcharset char_set, const char *whatfn);

Perror_t PDCI_countX_read(P_t *pads, const Pbase_m *m, Puint8 x, int eor_required, size_t count_max,
			  Pbase_pd *pd, Pint32 *res_out,
			  Pcharset char_set, const char *whatfn);


Perror_t PDCI_countXtoY_read(P_t *pads, const Pbase_m *m, Puint8 x, Puint8 y, size_t count_max,
			     Pbase_pd *pd, Pint32 *res_out,
			     Pcharset char_set, const char *whatfn);

Perror_t PDCI_date_read(P_t *pads, const Pbase_m *m, Pchar stopChar,
			Pbase_pd *pd, Puint32 *res_out,
			Pcharset char_set, const char *whatfn);

Perror_t PDCI_char_read(P_t *pads, const Pbase_m *m, Pbase_pd *pd, Pchar *c_out,
			Pcharset char_set, const char *whatfn);


Perror_t PDCI_string_FW_read(P_t *pads, const Pbase_m *m, size_t width,
			     Pbase_pd *pd, Pstring *s_out,
			     Pcharset char_set, const char *whatfn);

Perror_t PDCI_string_read(P_t *pads, const Pbase_m *m, Pchar stopChar,
			  Pbase_pd *pd, Pstring *s_out,
			  Pcharset char_set, const char *whatfn);

Perror_t PDCI_string_ME_read(P_t *pads, const Pbase_m *m, const char *matchRegexp,
			     Pbase_pd *pd, Pstring *s_out,
			     Pcharset char_set, const char *whatfn);

Perror_t PDCI_string_CME_read(P_t *pads, const Pbase_m *m, Pregexp_t *matchRegexp,
			      Pbase_pd *pd, Pstring *s_out,
			      Pcharset char_set, const char *whatfn);

Perror_t PDCI_string_SE_read(P_t *pads, const Pbase_m *m, const char *stopRegexp,
			     Pbase_pd *pd, Pstring *s_out,
			     Pcharset char_set, const char *whatfn);

Perror_t PDCI_string_CSE_read(P_t *pads, const Pbase_m *m, Pregexp_t *stopRegexp,
			      Pbase_pd *pd, Pstring *s_out,
			      Pcharset char_set, const char *whatfn);

#endif  /* P_CONFIG_READ_FUNCTIONS */

/* ================================================================================ */
/* INTERNAL WRITE ROUTINES (helpers) */


#if P_CONFIG_WRITE_FUNCTIONS > 0

ssize_t PDCI_char_lit_write2io(P_t *pads, Sfio_t *io, Pchar c,
			       Pcharset char_set, const char *whatfn);

ssize_t PDCI_char_lit_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pchar c,
				Pcharset char_set, const char *whatfn);

ssize_t PDCI_str_lit_write2io(P_t *pads, Sfio_t *io, const Pstring *s,
			      Pcharset char_set, const char *whatfn);

ssize_t PDCI_str_lit_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, const Pstring *s,
			       Pcharset char_set, const char *whatfn);

ssize_t PDCI_cstr_lit_write2io(P_t *pads, Sfio_t *io, const char *s,
			       Pcharset char_set, const char *whatfn);

ssize_t PDCI_cstr_lit_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, const char *s,
				Pcharset char_set, const char *whatfn);

ssize_t PDCI_char_lit_write_xml_2io(P_t *pads, Sfio_t *io, Pchar c,
				    const char *tag, int indent, const char *whatfn);

ssize_t PDCI_char_lit_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pchar c,
				     const char *tag, int indent, const char *whatfn);

ssize_t PDCI_str_lit_write_xml_2io(P_t *pads, Sfio_t *io, const Pstring *s,
				   const char *tag, int indent, const char *whatfn);

ssize_t PDCI_str_lit_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, const Pstring *s,
				    const char *tag, int indent, const char *whatfn);

ssize_t PDCI_cstr_lit_write_xml_2io(P_t *pads, Sfio_t *io, const char *s,
				    const char *tag, int indent, const char *whatfn);

ssize_t PDCI_cstr_lit_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, const char *s,
				     const char *tag, int indent, const char *whatfn);

#endif /* P_CONFIG_WRITE_FUNCTIONS */

/* ================================================================================ */
/* INTERNAL char/string write FUNCTIONS (helpers) */

#if P_CONFIG_WRITE_FUNCTIONS > 0

ssize_t PDCI_char_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd,
			    Pchar *c, Pcharset char_set, const char *whatfn);
ssize_t PDCI_char_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			    Pchar *c, Pcharset char_set, const char *whatfn);
ssize_t PDCI_string_FW_write2io(P_t *pads, Sfio_t *io, size_t width, Pbase_pd *pd,
				Pstring *s, Pcharset char_set, const char *whatfn);
ssize_t PDCI_string_FW_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full,
				 size_t width, Pbase_pd *pd,
				 Pstring *s, Pcharset char_set, const char *whatfn);

ssize_t PDCI_string_write2io(P_t *pads, Sfio_t *io, void* type_arg1, Pbase_pd *pd,
			     Pstring *s, Pcharset char_set, const char *inv_type, const char *whatfn);
ssize_t PDCI_string_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full,
			      void* type_arg1, Pbase_pd *pd, Pstring *s,
			      Pcharset char_set, const char *inv_type, const char *whatfn);

ssize_t PDCI_string_write2io_chararg(P_t *pads, Sfio_t *io, Pchar type_arg1, Pbase_pd *pd,
				     Pstring *s, Pcharset char_set, const char *inv_type, const char *whatfn);
ssize_t PDCI_string_write2buf_chararg(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full,
				      Pchar type_arg1, Pbase_pd *pd, Pstring *s,
				      Pcharset char_set, const char *inv_type, const char *whatfn);

ssize_t PDCI_date_write2io(P_t *pads, Sfio_t *io,  void *type_arg1, Pbase_pd *pd,
			   Puint32 *d, Pcharset char_set, const char *inv_type, const char *whatfn);
ssize_t PDCI_date_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full,
			    void *type_arg1, Pbase_pd *pd, Puint32 *d,
			    Pcharset char_set, const char *inv_type, const char *whatfn);

ssize_t PDCI_date_write2io_chararg(P_t *pads, Sfio_t *io,  Pchar type_arg1, Pbase_pd *pd,
				   Puint32 *d, Pcharset char_set, const char *inv_type, const char *whatfn);
ssize_t PDCI_date_write2buf_chararg(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full,
				    Pchar type_arg1, Pbase_pd *pd, Puint32 *d,
				    Pcharset char_set, const char *inv_type, const char *whatfn);

ssize_t PDCI_countX_write2io(P_t *pads, Sfio_t *io,
			     Puint8 x, int eor_required, size_t count_max,
			     Pbase_pd *pd, Pint32  *val, Pcharset char_set, const char *whatfn);
ssize_t PDCI_countX_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full,
			      Puint8 x, int eor_required, size_t countx,
			      Pbase_pd *pd, Pint32  *val, Pcharset char_set, const char *whatfn);
ssize_t PDCI_countXtoY_write2io(P_t *pads, Sfio_t *io,
				Puint8 x, Puint8 y, size_t count_max,
				Pbase_pd *pd, Pint32  *val, Pcharset char_set, const char *whatfn);
ssize_t PDCI_countXtoY_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full,
				 Puint8 x, Puint8 y, size_t count_max,
				 Pbase_pd *pd, Pint32  *val, Pcharset char_set, const char *whatfn);

ssize_t PDCI_char_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd,
				 Pchar *c, const char* tag, int indent, const char *whatfn);
ssize_t PDCI_char_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				 Pchar *c, const char* tag, int indent, const char *whatfn);
ssize_t PDCI_string_FW_write_xml_2io(P_t *pads, Sfio_t *io, size_t width, Pbase_pd *pd,
				     Pstring *s, const char* tag, int indent, const char *whatfn);
ssize_t PDCI_string_FW_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full,
				      size_t width, Pbase_pd *pd,
				      Pstring *s, const char* tag, int indent, const char *whatfn);

ssize_t PDCI_string_write_xml_2io(P_t *pads, Sfio_t *io, void* type_arg1, Pbase_pd *pd,
				  Pstring *s, const char* tag, int indent, const char *inv_type, const char *whatfn);
ssize_t PDCI_string_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full,
				   void* type_arg1, Pbase_pd *pd, Pstring *s,
				   const char* tag, int indent, const char *inv_type, const char *whatfn);

ssize_t PDCI_string_write_xml_2io_chararg(P_t *pads, Sfio_t *io, Pchar type_arg1, Pbase_pd *pd,
					  Pstring *s, const char* tag, int indent, const char *inv_type, const char *whatfn);
ssize_t PDCI_string_write_xml_2buf_chararg(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full,
					   Pchar type_arg1, Pbase_pd *pd, Pstring *s,
					   const char* tag, int indent, const char *inv_type, const char *whatfn);

ssize_t PDCI_date_write_xml_2io(P_t *pads, Sfio_t *io,  void *type_arg1, Pbase_pd *pd,
				Puint32 *d, const char* tag, int indent, const char *inv_type, const char *whatfn);
ssize_t PDCI_date_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full,
				 void *type_arg1, Pbase_pd *pd, Puint32 *d,
				 const char* tag, int indent, const char *inv_type, const char *whatfn);

ssize_t PDCI_date_write_xml_2io_chararg(P_t *pads, Sfio_t *io,  Pchar type_arg1, Pbase_pd *pd,
					Puint32 *d, const char* tag, int indent, const char *inv_type, const char *whatfn);
ssize_t PDCI_date_write_xml_2buf_chararg(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full,
					 Pchar type_arg1, Pbase_pd *pd, Puint32 *d,
					 const char* tag, int indent, const char *inv_type, const char *whatfn);

ssize_t PDCI_countX_write_xml_2io(P_t *pads, Sfio_t *io,
				  Puint8 x, int eor_required, size_t count_max,
				  Pbase_pd *pd, Pint32  *val, const char* tag, int indent, const char *whatfn);
ssize_t PDCI_countX_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full,
				   Puint8 x, int eor_required, size_t countx,
				   Pbase_pd *pd, Pint32  *val, const char* tag, int indent, const char *whatfn);
ssize_t PDCI_countXtoY_write_xml_2io(P_t *pads, Sfio_t *io,
				     Puint8 x, Puint8 y, size_t count_max,
				     Pbase_pd *pd, Pint32  *val, const char* tag, int indent, const char *whatfn);
ssize_t PDCI_countXtoY_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full,
				      Puint8 x, Puint8 y, size_t count_max,
				      Pbase_pd *pd, Pint32  *val, const char* tag, int indent, const char *whatfn);

#endif /* P_CONFIG_WRITE_FUNCTIONS */

/* ================================================================================
 * STRING COMPARISON
 */

#define Pstring_eq(str1, str2) \
  ((str1)->len == (str2)->len && strncmp((str1)->str, (str2)->str, (str1)->len) == 0)

#define Pstring_eq_cstr(PDcstr, cstr) \
  ((PDcstr)->len == strlen(cstr) && strncmp((PDcstr)->str, (cstr), (PDcstr)->len) == 0)

/* ================================================================================
 * P_STRING_FROM macros
 */

#define P_STRING_INIT_NULL(the_pads_string) do { \
  memset((void*)(&(the_pads_string)), 0, sizeof(the_pads_string)); \
} while (0)

#define P_STRING_INIT_LIT(the_pads_string, str_lit) do { \
  (the_pads_string).str     = (char*)(str_lit); \
  (the_pads_string).len     = sizeof(str_lit)-1; \
  (the_pads_string).rbuf    = 0; \
  (the_pads_string).sharing = 1; \
} while (0)

#define P_STRING_INIT_CSTR(the_pads_string, char_ptr_expr) do { \
  char *the_pads_string_tmp = (char*)(char_ptr_expr); \
  (the_pads_string).str     = the_pads_string_tmp; \
  (the_pads_string).len     = strlen(the_pads_string_tmp); \
  (the_pads_string).rbuf    = 0; \
  (the_pads_string).sharing = 1; \
} while (0)

#define P_STRING_INIT_CSTR_LEN(the_pads_string, char_ptr_expr, length_expr) do { \
  (the_pads_string).str     = (char*)(char_ptr_expr); \
  (the_pads_string).len     = (length_expr); \
  (the_pads_string).rbuf    = 0; \
  (the_pads_string).sharing = 1; \
} while (0)

/* ================================================================================
 * REGEXP functions
 */

#define Pregexp_compile(pads, regexp_str, regexp) \
  PDCI_regexp_compile(pads, regexp_str, regexp, "", "Pregexp_compile")
#define Pregexp_compile_cstr(pads, regexp_str, regexp) \
  PDCI_regexp_compile_cstr(pads, regexp_str, regexp, "", "Pregexp_compile_cstr")
#define Pregexp_cleanup(pads, regexp) \
  PDCI_regexp_cleanup(pads, regexp, "Pregexp_cleanup")

/* ================================================================================
 * P_REGEXP macros
 */

#define P_REGEXP_DECL_NULL(my_regexp) \
  Pregexp_t my_regexp = { 0 }

#define P_RE_STRING_FROM_CHAR(pads, char_expr) \
  ( sfstrset((pads)->tmp2, 0), \
    sfprintf((pads)->tmp2, "/[%c]/", (char_expr)), \
    sfstruse((pads)->tmp2) )

#define P_RE_STRING_FROM_STR(pads, str_expr) \
  ( (pads)->tmp2_pstr = (str_expr), \
    sfstrset((pads)->tmp2, 0), \
    sfprintf((pads)->tmp2, "/%.*s/l", (pads)->tmp2_pstr->len, (pads)->tmp2_pstr->str), \
    sfstruse((pads)->tmp2) )

#define P_RE_STRING_FROM_CSTR(pads, str_expr) \
  ( sfstrset((pads)->tmp2, 0), \
    sfprintf((pads)->tmp2, "/%s/l", (str_expr)), \
    sfstruse((pads)->tmp2) )

#define PDCI_REGEXP_FROM_CHAR(pads, my_regexp, char_expr, err_prefix, whatfn) \
  PDCI_regexp_compile_cstr(pads, P_RE_STRING_FROM_CHAR(pads, char_expr), &(my_regexp), err_prefix, whatfn)

#define PDCI_REGEXP_FROM_STR(pads, my_regexp, str_expr, err_prefix, whatfn) \
  PDCI_regexp_compile_cstr(pads, P_RE_STRING_FROM_STR(pads, str_expr), &(my_regexp), err_prefix, whatfn)

#define PDCI_REGEXP_FROM_CSTR(pads, my_regexp, str_expr, err_prefix, whatfn) \
  PDCI_regexp_compile_cstr(pads, P_RE_STRING_FROM_CSTR(pads, str_expr), &(my_regexp), err_prefix, whatfn)

#define P_REGEXP_FROM_CHAR(pads, my_regexp, char_expr) \
  PDCI_REGEXP_FROM_CHAR(pads, my_regexp, char_expr, "", "P_REGEXP_FROM_CHAR")

#define P_REGEXP_FROM_STR(pads, my_regexp, str_expr) \
  PDCI_REGEXP_FROM_STR(pads, my_regexp, str_expr, "", "P_REGEXP_FROM_STR")

#define P_REGEXP_FROM_CSTR(pads, my_regexp, str_expr) \
  PDCI_REGEXP_FROM_CSTR(pads, my_regexp, str_expr, "", "P_REGEXP_FROM_CSTR")

/* ================================================================================
 * CHAR/STRING/RE SCAN FUNCTIONS
 */

#if P_CONFIG_READ_FUNCTIONS > 0

#define Pchar_lit_scan1(pads, c, eat_c, panic, offset_out) \
  PDCI_char_lit_scan1(pads, c, eat_c, panic, offset_out, PDCI_DEF_CHARSET(pads), "Pchar_lit_scan1")

#define Pa_char_lit_scan1(pads, c, eat_c, panic, offset_out) \
  PDCI_char_lit_scan1(pads, c, eat_c, panic, offset_out, Pcharset_ASCII, "Pa_char_lit_scan1")

#define Pe_char_lit_scan1(pads, c, eat_c, panic, offset_out) \
  PDCI_char_lit_scan1(pads, c, eat_c, panic, offset_out, Pcharset_EBCDIC, "Pe_char_lit_scan1")

#define Pstr_lit_scan1(pads, f, eat_f, panic, offset_out) \
  PDCI_str_lit_scan1(pads, f, eat_f, panic, offset_out, PDCI_DEF_CHARSET(pads), "Pstr_lit_scan1")

#define Pa_str_lit_scan1(pads, f, eat_f, panic, offset_out) \
  PDCI_str_lit_scan1(pads, f, eat_f, panic, offset_out, Pcharset_ASCII, "Pa_str_lit_scan1")

#define Pe_str_lit_scan1(pads, f, eat_f, panic, offset_out) \
  PDCI_str_lit_scan1(pads, f, eat_f, panic, offset_out, Pcharset_EBCDIC, "Pe_str_lit_scan1")

#define Pcstr_lit_scan1(pads, f, eat_f, panic, offset_out) \
  PDCI_cstr_lit_scan1(pads, f, eat_f, panic, offset_out, PDCI_DEF_CHARSET(pads), "Pcstr_lit_scan1")

#define Pa_cstr_lit_scan1(pads, f, eat_f, panic, offset_out) \
  PDCI_cstr_lit_scan1(pads, f, eat_f, panic, offset_out, Pcharset_ASCII, "Pa_cstr_lit_scan1")

#define Pe_cstr_lit_scan1(pads, f, eat_f, panic, offset_out) \
  PDCI_cstr_lit_scan1(pads, f, eat_f, panic, offset_out, Pcharset_EBCDIC, "Pe_cstr_lit_scan1")

#define Pre_scan1(pads, f, eat_f, panic, offset_out) \
  PDCI_re_scan1(pads, f, eat_f, panic, offset_out, PDCI_DEF_CHARSET(pads), "Pre_scan1")

#define Pa_re_scan1(pads, f, eat_f, panic, offset_out) \
  PDCI_re_scan1(pads, f, eat_f, panic, offset_out, Pcharset_ASCII, "Pa_re_scan1")

#define Pe_re_scan1(pads, f, eat_f, panic, offset_out) \
  PDCI_re_scan1(pads, f, eat_f, panic, offset_out, Pcharset_EBCDIC, "Pe_re_scan1")


#define Pchar_lit_scan2(pads, c, s, eat_c, eat_s, panic, f_found_out, offset_out) \
  PDCI_char_lit_scan2(pads, c, s, eat_c, eat_s, panic, f_found_out, offset_out, PDCI_DEF_CHARSET(pads), "Pchar_lit_scan2")

#define Pa_char_lit_scan2(pads, c, s, eat_c, eat_s, panic, f_found_out, offset_out) \
  PDCI_char_lit_scan2(pads, c, s, eat_c, eat_s, panic, f_found_out, offset_out, Pcharset_ASCII, "Pa_char_lit_scan2")

#define Pe_char_lit_scan2(pads, c, s, eat_c, eat_s, panic, f_found_out, offset_out) \
  PDCI_char_lit_scan2(pads, c, s, eat_c, eat_s, panic, f_found_out, offset_out, Pcharset_EBCDIC, "Pe_char_lit_scan2")

#define Pstr_lit_scan2(pads, f, s, eat_f, eat_s, panic, f_found_out, offset_out) \
  PDCI_str_lit_scan2(pads, f, s, eat_f, eat_s, panic, f_found_out, offset_out, PDCI_DEF_CHARSET(pads), "Pstr_lit_scan2")

#define Pa_str_lit_scan2(pads, f, s, eat_f, eat_s, panic, f_found_out, offset_out) \
  PDCI_str_lit_scan2(pads, f, s, eat_f, eat_s, panic, f_found_out, offset_out, Pcharset_ASCII, "Pa_str_lit_scan2")

#define Pe_str_lit_scan2(pads, f, s, eat_f, eat_s, panic, f_found_out, offset_out) \
  PDCI_str_lit_scan2(pads, f, s, eat_f, eat_s, panic, f_found_out, offset_out, Pcharset_EBCDIC, "Pe_str_lit_scan2")

#define Pcstr_lit_scan2(pads, f, s, eat_f, eat_s, panic, f_found_out, offset_out) \
  PDCI_cstr_lit_scan2(pads, f, s, eat_f, eat_s, panic, f_found_out, offset_out, PDCI_DEF_CHARSET(pads), "Pcstr_lit_scan2")

#define Pa_cstr_lit_scan2(pads, f, s, eat_f, eat_s, panic, f_found_out, offset_out) \
  PDCI_cstr_lit_scan2(pads, f, s, eat_f, eat_s, panic, f_found_out, offset_out, Pcharset_ASCII, "Pa_cstr_lit_scan2")

#define Pe_cstr_lit_scan2(pads, f, s, eat_f, eat_s, panic, f_found_out, offset_out) \
  PDCI_cstr_lit_scan2(pads, f, s, eat_f, eat_s, panic, f_found_out, offset_out, Pcharset_EBCDIC, "Pe_cstr_lit_scan2")

#define Pre_scan2(pads, f, s, eat_f, eat_s, panic, f_found_out, offset_out) \
  PDCI_re_scan2(pads, f, s, eat_f, eat_s, panic, f_found_out, offset_out, PDCI_DEF_CHARSET(pads), "Pre_scan2")

#define Pa_re_scan2(pads, f, s, eat_f, eat_s, panic, f_found_out, offset_out) \
  PDCI_re_scan2(pads, f, s, eat_f, eat_s, panic, f_found_out, offset_out, Pcharset_ASCII, "Pa_re_scan2")

#define Pe_re_scan2(pads, f, s, eat_f, eat_s, panic, f_found_out, offset_out) \
  PDCI_re_scan2(pads, f, s, eat_f, eat_s, panic, f_found_out, offset_out, Pcharset_EBCDIC, "Pe_re_scan2")

#endif /* P_CONFIG_READ_FUNCTIONS */

/* ================================================================================
 * CHAR/STRING/RE SCAN FUNCTIONS
 */

#if P_CONFIG_READ_FUNCTIONS > 0

#define Pchar_lit_match(pads, c, eat_c) \
  PDCI_char_lit_match(pads, c, eat_c, PDCI_DEF_CHARSET(pads), "Pchar_lit_match")

#define Pa_char_lit_match(pads, c, eat_c) \
  PDCI_char_lit_match(pads, c, eat_c, Pcharset_ASCII, "Pa_char_lit_match")

#define Pe_char_lit_match(pads, c, eat_c) \
  PDCI_char_lit_match(pads, c, eat_c, Pcharset_EBCDIC, "Pe_char_lit_match")

#define Pstr_lit_match(pads, f, eat_f) \
  PDCI_str_lit_match(pads, f, eat_f, PDCI_DEF_CHARSET(pads), "Pstr_lit_match")

#define Pa_str_lit_match(pads, f, eat_f) \
  PDCI_str_lit_match(pads, f, eat_f, Pcharset_ASCII, "Pa_str_lit_match")

#define Pe_str_lit_match(pads, f, eat_f) \
  PDCI_str_lit_match(pads, f, eat_f, Pcharset_EBCDIC, "Pe_str_lit_match")

#define Pcstr_lit_match(pads, f, eat_f) \
  PDCI_cstr_lit_match(pads, f, eat_f, PDCI_DEF_CHARSET(pads), "Pcstr_lit_match")

#define Pa_cstr_lit_match(pads, f, eat_f) \
  PDCI_cstr_lit_match(pads, f, eat_f, Pcharset_ASCII, "Pa_cstr_lit_match")

#define Pe_cstr_lit_match(pads, f, eat_f) \
  PDCI_cstr_lit_match(pads, f, eat_f, Pcharset_EBCDIC, "Pe_cstr_lit_match")

#define Pre_match(pads, f, eat_f) \
  PDCI_re_match(pads, f, eat_f, PDCI_DEF_CHARSET(pads), "Pre_match")

#define Pa_re_match(pads, f, eat_f) \
  PDCI_re_match(pads, f, eat_f, Pcharset_ASCII, "Pa_re_match")

#define Pe_re_match(pads, f, eat_f) \
  PDCI_re_match(pads, f, eat_f, Pcharset_EBCDIC, "Pe_re_match")

#endif /* P_CONFIG_READ_FUNCTIONS */

/* ================================================================================
 * CHAR/STRING READ FUNCTIONS
 */

#if P_CONFIG_READ_FUNCTIONS > 0

#define Pchar_lit_read(pads, m, c, pd, c_out) \
  PDCI_char_lit_read(pads, m, c, pd, c_out, PDCI_DEF_CHARSET(pads), "Pchar_lit_read")

#define Pa_char_lit_read(pads, m, c, pd, c_out) \
  PDCI_char_lit_read(pads, m, pd, c, Pcharset_ASCII, "Pa_char_lit_read")

#define Pe_char_lit_read(pads, m, c, pd, c_out) \
  PDCI_char_lit_read(pads, m, c, pd, c_out, Pcharset_EBCDIC, "Pe_char_lit_read")

#define Pstr_lit_read(pads, m, s, pd, s_out) \
  PDCI_str_lit_read(pads, m, s, pd, s_out, PDCI_DEF_CHARSET(pads), "Pstr_lit_read")

#define Pa_str_lit_read(pads, m, s, pd, s_out) \
  PDCI_str_lit_read(pads, m, s, pd, s_out, Pcharset_ASCII, "Pa_str_lit_read")

#define Pe_str_lit_read(pads, m, s, pd, s_out) \
  PDCI_str_lit_read(pads, m, s, pd, s_out, Pcharset_EBCDIC, "Pe_str_lit_read")

#define Pcstr_lit_read(pads, m, s, pd, s_out) \
  PDCI_cstr_lit_read(pads, m, s, pd, s_out, PDCI_DEF_CHARSET(pads), "Pcstr_lit_read")

#define Pa_cstr_lit_read(pads, m, s, pd, s_out) \
  PDCI_cstr_lit_read(pads, m, s, pd, s_out, Pcharset_ASCII, "Pa_cstr_lit_read")

#define Pe_cstr_lit_read(pads, m, s, pd, s_out) \
  PDCI_cstr_lit_read(pads, m, s, pd, s_out, Pcharset_EBCDIC, "Pe_cstr_lit_read")

#define PcountX_read(pads, m, x, eor_required, count_max, pd, res_out) \
  PDCI_countX_read(pads, m, x, eor_required, count_max, pd, res_out, PDCI_DEF_CHARSET(pads), "PcountX_read")

#define Pa_countX_read(pads, m, x, eor_required, count_max, pd, res_out) \
  PDCI_countX_read(pads, m, x, eor_required, count_max, pd, res_out, Pcharset_ASCII, "Pa_countX_read")

#define Pe_countX_read(pads, m, x, eor_required, count_max, pd, res_out) \
  PDCI_countX_read(pads, m, x, eor_required, count_max, pd, res_out, Pcharset_EBCDIC, "Pe_countX_read")

#define PcountXtoY_read(pads, m, x, y, count_max, pd, res_out) \
  PDCI_countXtoY_read(pads, m, x, y, pd, count_max, res_out, PDCI_DEF_CHARSET(pads), "PcountXtoY_read")

#define Pa_countXtoY_read(pads, m, x, y, count_max, pd, res_out) \
  PDCI_countXtoY_read(pads, m, x, y, pd, count_max, res_out, Pcharset_ASCII, "Pa_countXtoY_read")

#define Pe_countXtoY_read(pads, m, x, y, count_max, pd, res_out) \
  PDCI_countXtoY_read(pads, m, x, y, pd, count_max, res_out, Pcharset_EBCDIC, "Pe_countXtoY_read")

#define Pdate_read(pads, m, stopChar, pd, res_out) \
  PDCI_date_read(pads, m, stopChar, pd, res_out, PDCI_DEF_CHARSET(pads), "Pdate_read")

#define Pa_date_read(pads, m, stopChar, pd, res_out) \
  PDCI_date_read(pads, m, stopChar, pd, res_out, Pcharset_ASCII, "Pa_date_read")

#define Pe_date_read(pads, m, stopChar, pd, res_out) \
  PDCI_date_read(pads, m, stopChar, pd, res_out, Pcharset_EBCDIC, "Pe_date_read")

#define Pchar_read(pads, m, pd, c_out) \
  PDCI_char_read(pads, m, pd, c_out, PDCI_DEF_CHARSET(pads), "Pchar_read")

#define Pa_char_read(pads, m, pd, c_out) \
  PDCI_char_read(pads, m, pd, c_out, Pcharset_ASCII, "Pa_char_read")

#define Pe_char_read(pads, m, pd, c_out) \
  PDCI_char_read(pads, m, pd, c_out, Pcharset_EBCDIC, "Pe_char_read")

#define Pstring_FW_read(pads, m, width, pd, s_out) \
  PDCI_string_FW_read(pads, m, width, pd, s_out, PDCI_DEF_CHARSET(pads), "Pstring_FW_read")

#define Pa_string_FW_read(pads, m, width, pd, s_out) \
  PDCI_string_FW_read(pads, m, width, pd, s_out, Pcharset_ASCII, "Pa_string_FW_read")

#define Pe_string_FW_read(pads, m, width, pd, s_out) \
  PDCI_string_FW_read(pads, m, width, pd, s_out, Pcharset_EBCDIC, "Pe_string_FW_read")


#define Pstring_read(pads, m, stopChar, pd, s_out) \
  PDCI_string_read(pads, m, stopChar, pd, s_out, PDCI_DEF_CHARSET(pads), "Pstring_read")

#define Pa_string_read(pads, m, stopChar, pd, s_out) \
  PDCI_string_read(pads, m, stopChar, pd, s_out, Pcharset_ASCII, "Pa_string_read")

#define Pe_string_read(pads, m, stopChar, pd, s_out) \
  PDCI_string_read(pads, m, stopChar, pd, s_out, Pcharset_EBCDIC, "Pe_string_read")


#define Pstring_ME_read(pads, m, matchRegexp, pd, s_out) \
  PDCI_string_ME_read(pads, m, matchRegexp, pd, s_out, PDCI_DEF_CHARSET(pads), "Pstring_ME_read")

#define Pa_string_ME_read(pads, m, matchRegexp, pd, s_out) \
  PDCI_string_ME_read(pads, m, matchRegexp, pd, s_out, Pcharset_ASCII, "Pa_string_ME_read")

#define Pe_string_ME_read(pads, m, matchRegexp, pd, s_out) \
  PDCI_string_ME_read(pads, m, matchRegexp, pd, s_out, Pcharset_EBCDIC, "Pe_string_ME_read")


#define Pstring_CME_read(pads, m, matchRegexp, pd, s_out) \
  PDCI_string_CME_read(pads, m, matchRegexp, pd, s_out, PDCI_DEF_CHARSET(pads), "Pstring_CME_read")

#define Pa_string_CME_read(pads, m, matchRegexp, pd, s_out) \
  PDCI_string_CME_read(pads, m, matchRegexp, pd, s_out, Pcharset_ASCII, "Pa_string_CME_read")

#define Pe_string_CME_read(pads, m, matchRegexp, pd, s_out) \
  PDCI_string_CME_read(pads, m, matchRegexp, pd, s_out, Pcharset_EBCDIC, "Pe_string_CME_read")


#define Pstring_SE_read(pads, m, stopRegexp, pd, s_out) \
  PDCI_string_SE_read(pads, m, stopRegexp, pd, s_out, PDCI_DEF_CHARSET(pads), "Pstring_SE_read")

#define Pa_string_SE_read(pads, m, stopRegexp, pd, s_out) \
  PDCI_string_SE_read(pads, m, stopRegexp, pd, s_out, Pcharset_ASCII, "Pa_string_SE_read")

#define Pe_string_SE_read(pads, m, stopRegexp, pd, s_out) \
  PDCI_string_SE_read(pads, m, stopRegexp, pd, s_out, Pcharset_EBCDIC, "Pe_string_SE_read")


#define Pstring_CSE_read(pads, m, stopRegexp, pd, s_out) \
  PDCI_string_CSE_read(pads, m, stopRegexp, pd, s_out, PDCI_DEF_CHARSET(pads), "Pstring_CSE_read")

#define Pa_string_CSE_read(pads, m, stopRegexp, pd, s_out) \
  PDCI_string_CSE_read(pads, m, stopRegexp, pd, s_out, Pcharset_ASCII, "Pa_string_CSE_read")

#define Pe_string_CSE_read(pads, m, stopRegexp, pd, s_out) \
  PDCI_string_CSE_read(pads, m, stopRegexp, pd, s_out, Pcharset_EBCDIC, "Pe_string_CSE_read")

#endif /* P_CONFIG_READ_FUNCTIONS */

/* ================================================================================
 * DEFAULT STRING TO INTEGER READ FUNCTIONS
 */

#if P_CONFIG_READ_FUNCTIONS > 0

#define Pint8_read(pads, m, pd, res_out) \
  PDCI_CHARSET_SWITCH(pads, P, int8_read, (pads, m, pd, res_out))

#define Pint16_read(pads, m, pd, res_out) \
  PDCI_CHARSET_SWITCH(pads, P, int16_read, (pads, m, pd, res_out))

#define Pint32_read(pads, m, pd, res_out) \
  PDCI_CHARSET_SWITCH(pads, P, int32_read, (pads, m, pd, res_out))

#define Pint64_read(pads, m, pd, res_out) \
  PDCI_CHARSET_SWITCH(pads, P, int64_read, (pads, m, pd, res_out))

#define Puint8_read(pads, m, pd, res_out) \
  PDCI_CHARSET_SWITCH(pads, P, uint8_read, (pads, m, pd, res_out))

#define Puint16_read(pads, m, pd, res_out) \
  PDCI_CHARSET_SWITCH(pads, P, uint16_read, (pads, m, pd, res_out))

#define Puint32_read(pads, m, pd, res_out) \
  PDCI_CHARSET_SWITCH(pads, P, uint32_read, (pads, m, pd, res_out))

#define Puint64_read(pads, m, pd, res_out) \
  PDCI_CHARSET_SWITCH(pads, P, uint64_read, (pads, m, pd, res_out))

#define Pint8_FW_read(pads, m, width, pd, res_out) \
  PDCI_CHARSET_SWITCH(pads, P, int8_FW_read, (pads, m, width, pd, res_out))

#define Pint16_FW_read(pads, m, width, pd, res_out) \
  PDCI_CHARSET_SWITCH(pads, P, int16_FW_read, (pads, m, width, pd, res_out))

#define Pint32_FW_read(pads, m, width, pd, res_out) \
  PDCI_CHARSET_SWITCH(pads, P, int32_FW_read, (pads, m, width, pd, res_out))

#define Pint64_FW_read(pads, m, width, pd, res_out) \
  PDCI_CHARSET_SWITCH(pads, P, int64_FW_read, (pads, m, width, pd, res_out))

#define Puint8_FW_read(pads, m, width, pd, res_out) \
  PDCI_CHARSET_SWITCH(pads, P, uint8_FW_read, (pads, m, width, pd, res_out))

#define Puint16_FW_read(pads, m, width, pd, res_out) \
  PDCI_CHARSET_SWITCH(pads, P, uint16_FW_read, (pads, m, width, pd, res_out))

#define Puint32_FW_read(pads, m, width, pd, res_out) \
  PDCI_CHARSET_SWITCH(pads, P, uint32_FW_read, (pads, m, width, pd, res_out))

#define Puint64_FW_read(pads, m, width, pd, res_out) \
  PDCI_CHARSET_SWITCH(pads, P, uint64_FW_read, (pads, m, width, pd, res_out))

#endif /* P_CONFIG_READ_FUNCTIONS */

/* ================================================================================
 * WRITE FUNCTIONS
 */

#if P_CONFIG_WRITE_FUNCTIONS > 0

#define Pa_char_lit_write2io(pads, io, c) \
  PDCI_char_lit_write2io(pads, io, c, Pcharset_ASCII, "Pa_char_lit_write2io")

#define Pe_char_lit_write2io(pads, io, c) \
  PDCI_char_lit_write2io(pads, io, c, Pcharset_EBCDIC, "Pe_char_lit_write2io")

#define Pchar_lit_write2io(pads, io, c) \
  PDCI_char_lit_write2io(pads, io, c, PDCI_DEF_CHARSET(pads), "Pchar_lit_write2io")

#define Pa_str_lit_write2io(pads, io, s) \
  PDCI_str_lit_write2io(pads, io, s, Pcharset_ASCII, "Pa_str_lit_write2io")

#define Pe_str_lit_write2io(pads, io, s) \
  PDCI_str_lit_write2io(pads, io, s, Pcharset_EBCDIC, "Pe_str_lit_write2io")

#define Pstr_lit_write2io(pads, io, s) \
  PDCI_str_lit_write2io(pads, io, s, PDCI_DEF_CHARSET(pads), "Pstr_lit_write2io")

#define Pa_cstr_lit_write2io(pads, io, s) \
  PDCI_cstr_lit_write2io(pads, io, s, Pcharset_ASCII, "Pa_cstr_lit_write2io")

#define Pe_cstr_lit_write2io(pads, io, s) \
  PDCI_cstr_lit_write2io(pads, io, s, Pcharset_EBCDIC, "Pe_cstr_lit_write2io")

#define Pcstr_lit_write2io(pads, io, s) \
  PDCI_cstr_lit_write2io(pads, io, s, PDCI_DEF_CHARSET(pads), "Pcstr_lit_write2io")

#define Pa_char_lit_write2buf(pads, buf, buf_len, buf_full, c) \
  PDCI_char_lit_write2buf(pads, buf, buf_len, buf_full, c, Pcharset_ASCII, "Pa_char_lit_write2buf")

#define Pe_char_lit_write2buf(pads, buf, buf_len, buf_full, c) \
  PDCI_char_lit_write2buf(pads, buf, buf_len, buf_full, c, Pcharset_EBCDIC, "Pe_char_lit_write2buf")

#define Pchar_lit_write2buf(pads, buf, buf_len, buf_full, c) \
  PDCI_char_lit_write2buf(pads, buf, buf_len, buf_full, c, PDCI_DEF_CHARSET(pads), "Pchar_lit_write2buf")

#define Pa_str_lit_write2buf(pads, buf, buf_len, buf_full, s) \
  PDCI_str_lit_write2buf(pads, buf, buf_len, buf_full, s, Pcharset_ASCII, "Pa_str_lit_write2buf")

#define Pe_str_lit_write2buf(pads, buf, buf_len, buf_full, s) \
  PDCI_str_lit_write2buf(pads, buf, buf_len, buf_full, s, Pcharset_EBCDIC, "Pe_str_lit_write2buf")

#define Pstr_lit_write2buf(pads, buf, buf_len, buf_full, s) \
  PDCI_str_lit_write2buf(pads, buf, buf_len, buf_full, s, PDCI_DEF_CHARSET(pads), "Pstr_lit_write2buf")

#define Pa_cstr_lit_write2buf(pads, buf, buf_len, buf_full, s) \
  PDCI_cstr_lit_write2buf(pads, buf, buf_len, buf_full, s, Pcharset_ASCII, "Pa_cstr_lit_write2buf")

#define Pe_cstr_lit_write2buf(pads, buf, buf_len, buf_full, s) \
  PDCI_cstr_lit_write2buf(pads, buf, buf_len, buf_full, s, Pcharset_EBCDIC, "Pe_cstr_lit_write2buf")

#define Pcstr_lit_write2buf(pads, buf, buf_len, buf_full, s) \
  PDCI_cstr_lit_write2buf(pads, buf, buf_len, buf_full, s, PDCI_DEF_CHARSET(pads), "Pcstr_lit_write2buf")

#define Pchar_write2io(pads, io, pd, c) \
  PDCI_char_write2io(pads, io, pd, c, PDCI_DEF_CHARSET(pads), "Pchar_write2io")

#define Pa_char_write2io(pads, io, pd, c) \
  PDCI_char_write2io(pads, io, pd, c, Pcharset_ASCII, "Pa_char_write2io")

#define Pe_char_write2io(pads, io, pd, c) \
  PDCI_char_write2io(pads, io, pd, c, Pcharset_EBCDIC, "Pe_char_write2io")

#define Pchar_write2buf(pads, buf, buf_len, buf_full, pd, c) \
  PDCI_char_write2buf(pads, buf, buf_len, buf_full, pd, c, PDCI_DEF_CHARSET(pads), "Pchar_write2buf")

#define Pa_char_write2buf(pads, buf, buf_len, buf_full, pd, c) \
  PDCI_char_write2buf(pads, buf, buf_len, buf_full, pd, c, Pcharset_ASCII, "Pa_char_write2buf")

#define Pe_char_write2buf(pads, buf, buf_len, buf_full, pd, c) \
  PDCI_char_write2buf(pads, buf, buf_len, buf_full, pd, c, Pcharset_EBCDIC, "Pe_char_write2buf")

#define Pstring_FW_write2io(pads, io, width, pd, s) \
  PDCI_string_FW_write2io(pads, io, width, pd, s, PDCI_DEF_CHARSET(pads), "Pstring_FW_write2io")

#define Pa_string_FW_write2io(pads, io, width, pd, s) \
  PDCI_string_FW_write2io(pads, io, width, pd, s, Pcharset_ASCII, "Pa_string_FW_write2io")

#define Pe_string_FW_write2io(pads, io, width, pd, s) \
  PDCI_string_FW_write2io(pads, io, width, pd, s, Pcharset_EBCDIC, "Pe_string_FW_write2io")

#define Pstring_FW_write2buf(pads, buf, buf_len, buf_full, width, pd, s) \
  PDCI_string_FW_write2buf(pads, buf, buf_len, buf_full, width, pd, s, PDCI_DEF_CHARSET(pads), "Pstring_FW_write2buf")

#define Pa_string_FW_write2buf(pads, buf, buf_len, buf_full, width, pd, s) \
  PDCI_string_FW_write2buf(pads, buf, buf_len, buf_full, width, pd, s, Pcharset_ASCII, "Pa_string_FW_write2buf")

#define Pe_string_FW_write2buf(pads, buf, buf_len, buf_full, width, pd, s) \
  PDCI_string_FW_write2buf(pads, buf, buf_len, buf_full, width, pd, s, Pcharset_EBCDIC, "Pe_string_FW_write2buf")

#define Pstring_write2io(pads, io, stopChar, pd, s) \
  PDCI_string_write2io_chararg(pads, io, stopChar, pd, s, PDCI_DEF_CHARSET(pads), "Pstring", "Pstring_write2io")

#define Pa_string_write2io(pads, io, stopChar, pd, s) \
  PDCI_string_write2io_chararg(pads, io, stopChar, pd, s, Pcharset_ASCII, "Pstring", "Pa_string_write2io")

#define Pe_string_write2io(pads, io, stopChar, pd, s) \
  PDCI_string_write2io_chararg(pads, io, stopChar, pd, s, Pcharset_EBCDIC, "Pstring", "Pe_string_write2io")

#define Pstring_write2buf(pads, buf, buf_len, buf_full, stopChar, pd, s) \
  PDCI_string_write2buf_chararg(pads, buf, buf_len, buf_full, stopChar, pd, s, PDCI_DEF_CHARSET(pads), "Pstring", "Pstring_write2buf")

#define Pa_string_write2buf(pads, buf, buf_len, buf_full, stopChar, pd, s) \
  PDCI_string_write2buf_chararg(pads, buf, buf_len, buf_full, stopChar, pd, s, Pcharset_ASCII, "Pstring", "Pa_string_write2buf")

#define Pe_string_write2buf(pads, buf, buf_len, buf_full, stopChar, pd, s) \
  PDCI_string_write2buf_chararg(pads, buf, buf_len, buf_full, stopChar, pd, s, Pcharset_EBCDIC, "Pstring", "Pe_string_write2buf")

#define Pstring_ME_write2io(pads, io, matchRegexp, pd, s) \
  PDCI_string_write2io(pads, io, ((void*)(matchRegexp)), pd, s, PDCI_DEF_CHARSET(pads), "Pstring_ME", "Pstring_ME_write2io")

#define Pa_string_ME_write2io(pads, io, matchRegexp, pd, s) \
  PDCI_string_write2io(pads, io, ((void*)(matchRegexp)), pd, s, PDCI_charset_ASCII, "Pstring_ME", "Pa_string_ME_write2io")

#define Pe_string_ME_write2io(pads, io, matchRegexp, pd, s) \
  PDCI_string_write2io(pads, io, ((void*)(matchRegexp)), pd, s, PDCI_charset_EBCDIC, "Pstring_ME", "Pe_string_ME_write2io")

#define Pstring_ME_write2buf(pads, buf, buf_len, buf_full, matchRegexp, pd, s) \
  PDCI_string_write2buf(pads, buf, buf_len, buf_full, ((void*)(matchRegexp)), pd, s, PDCI_DEF_CHARSET(pads), "Pstring_ME", "P_ME_string_write2buf")

#define Pa_string_ME_write2buf(pads, buf, buf_len, buf_full, matchRegexp, pd, s) \
  PDCI_string_write2buf(pads, buf, buf_len, buf_full, ((void*)(matchRegexp)), pd, s, Pcharset_ASCII, "Pstring_ME", "Pa_string_ME_write2buf")

#define Pe_string_ME_write2buf(pads, buf, buf_len, buf_full, matchRegexp, pd, s) \
  PDCI_string_write2buf(pads, buf, buf_len, buf_full, ((void*)(matchRegexp)), pd, s, Pcharset_EBCDIC, "Pstring_ME", "Pe_string_ME_write2buf")

#define Pstring_CME_write2io(pads, io, matchRegexp, pd, s) \
  PDCI_string_write2io(pads, io, ((void*)(matchRegexp)), pd, s, PDCI_DEF_CHARSET(pads), "Pstring_CME", "Pstring_CME_write2io")

#define Pa_string_CME_write2io(pads, io, matchRegexp, pd, s) \
  PDCI_string_write2io(pads, io, ((void*)(matchRegexp)), pd, s, PDCI_charset_ASCII, "Pstring_CME", "Pa_string_CME_write2io")

#define Pe_string_CME_write2io(pads, io, matchRegexp, pd, s) \
  PDCI_string_write2io(pads, io, ((void*)(matchRegexp)), pd, s, PDCI_charset_EBCDIC, "Pstring_CME", "Pe_string_CME_write2io")

#define Pstring_CME_write2buf(pads, buf, buf_len, buf_full, matchRegexp, pd, s) \
  PDCI_string_write2buf(pads, buf, buf_len, buf_full, ((void*)(matchRegexp)), pd, s, PDCI_DEF_CHARSET(pads), "Pstring_CME", "P_CME_string_write2buf")

#define Pa_string_CME_write2buf(pads, buf, buf_len, buf_full, matchRegexp, pd, s) \
  PDCI_string_write2buf(pads, buf, buf_len, buf_full, ((void*)(matchRegexp)), pd, s, Pcharset_ASCII, "Pstring_CME", "Pa_string_CME_write2buf")

#define Pe_string_CME_write2buf(pads, buf, buf_len, buf_full, matchRegexp, pd, s) \
  PDCI_string_write2buf(pads, buf, buf_len, buf_full, ((void*)(matchRegexp)), pd, s, Pcharset_EBCDIC, "Pstring_CME", "Pe_string_CME_write2buf")

#define Pstring_SE_write2io(pads, io, stopRegexp, pd, s) \
  PDCI_string_write2io(pads, io, ((void*)(stopRegexp)), pd, s, PDCI_DEF_CHARSET(pads), "Pstring_SE", "Pstring_SE_write2io")

#define Pa_string_SE_write2io(pads, io, stopRegexp, pd, s) \
  PDCI_string_write2io(pads, io, ((void*)(stopRegexp)), pd, s, PDCI_charset_ASCII, "Pstring_SE", "Pa_string_SE_write2io")

#define Pe_string_SE_write2io(pads, io, stopRegexp, pd, s) \
  PDCI_string_write2io(pads, io, ((void*)(stopRegexp)), pd, s, PDCI_charset_EBCDIC, "Pstring_SE", "Pe_string_SE_write2io")

#define Pstring_SE_write2buf(pads, buf, buf_len, buf_full, stopRegexp, pd, s) \
  PDCI_string_write2buf(pads, buf, buf_len, buf_full, ((void*)(stopRegexp)), pd, s, PDCI_DEF_CHARSET(pads), "Pstring_SE", "P_SE_string_write2buf")

#define Pa_string_SE_write2buf(pads, buf, buf_len, buf_full, stopRegexp, pd, s) \
  PDCI_string_write2buf(pads, buf, buf_len, buf_full, ((void*)(stopRegexp)), pd, s, Pcharset_ASCII, "Pstring_SE", "Pa_string_SE_write2buf")

#define Pe_string_SE_write2buf(pads, buf, buf_len, buf_full, stopRegexp, pd, s) \
  PDCI_string_write2buf(pads, buf, buf_len, buf_full, ((void*)(stopRegexp)), pd, s, Pcharset_EBCDIC, "Pstring_SE", "Pe_string_SE_write2buf")

#define Pstring_CSE_write2io(pads, io, stopRegexp, pd, s) \
  PDCI_string_write2io(pads, io, ((void*)(stopRegexp)), pd, s, PDCI_DEF_CHARSET(pads), "Pstring_CSE", "Pstring_CSE_write2io")

#define Pa_string_CSE_write2io(pads, io, stopRegexp, pd, s) \
  PDCI_string_write2io(pads, io, ((void*)(stopRegexp)), pd, s, PDCI_charset_ASCII, "Pstring_CSE", "Pa_string_CSE_write2io")

#define Pe_string_CSE_write2io(pads, io, stopRegexp, pd, s) \
  PDCI_string_write2io(pads, io, ((void*)(stopRegexp)), pd, s, PDCI_charset_EBCDIC, "Pstring_CSE", "Pe_string_CSE_write2io")

#define Pstring_CSE_write2buf(pads, buf, buf_len, buf_full, stopRegexp, pd, s) \
  PDCI_string_write2buf(pads, buf, buf_len, buf_full, ((void*)(stopRegexp)), pd, s, PDCI_DEF_CHARSET(pads), "Pstring_CSE", "P_CSE_string_write2buf")

#define Pa_string_CSE_write2buf(pads, buf, buf_len, buf_full, stopRegexp, pd, s) \
  PDCI_string_write2buf(pads, buf, buf_len, buf_full, ((void*)(stopRegexp)), pd, s, Pcharset_ASCII, "Pstring_CSE", "Pa_string_CSE_write2buf")

#define Pe_string_CSE_write2buf(pads, buf, buf_len, buf_full, stopRegexp, pd, s) \
  PDCI_string_write2buf(pads, buf, buf_len, buf_full, ((void*)(stopRegexp)), pd, s, Pcharset_EBCDIC, "Pstring_CSE", "Pe_string_CSE_write2buf")

#define Pdate_write2io(pads, io, stopChar, pd, d) \
  PDCI_date_write2io_chararg(pads, io, stopChar, pd, d, PDCI_DEF_CHARSET(pads), "Pdate", "Pdate_write2io")

#define Pa_date_write2io(pads, io, stopChar, pd, d) \
  PDCI_date_write2io_chararg(pads, io, stopChar, pd, d, Pcharset_ASCII, "Pdate", "Pa_date_write2io")

#define Pe_date_write2io(pads, io, stopChar, pd, d) \
  PDCI_date_write2io_chararg(pads, io, stopChar, pd, d, Pcharset_EBCDIC, "Pdate", "Pe_date_write2io")

#define Pdate_write2buf(pads, buf, buf_len, buf_full, stopChar, pd, d) \
  PDCI_date_write2buf_chararg(pads, buf, buf_len, buf_full, stopChar, pd, d, PDCI_DEF_CHARSET(pads), "Pdate", "Pdate_write2buf")

#define Pa_date_write2buf(pads, buf, buf_len, buf_full, stopChar, pd, d) \
  PDCI_date_write2buf_chararg(pads, buf, buf_len, buf_full, stopChar, pd, d, Pcharset_ASCII, "Pdate", "Pa_date_write2buf")

#define Pe_date_write2buf(pads, buf, buf_len, buf_full, stopChar, pd, d) \
  PDCI_date_write2buf_chararg(pads, buf, buf_len, buf_full, stopChar, pd, d, Pcharset_EBCDIC, "Pdate", "Pe_date_write2buf")

#define PcountX_write2io(pads, io, x, eor_required, count_max, pd, val) \
  PDCI_countX_write2io(pads, io, x, eor_required, count_max, pd, val, PDCI_DEF_CHARSET(pads), "PcountX_write2io")

#define Pa_countX_write2io(pads, io, x, eor_required, count_max, pd, val) \
  PDCI_countX_write2io(pads, io, x, eor_required, count_max, pd, val, Pcharset_ASCII, "Pa_countX_write2io")

#define Pe_countX_write2io(pads, io, x, eor_required, count_max, pd, val) \
  PDCI_countX_write2io(pads, io, x, eor_required, count_max, pd, val, Pcharset_EBCDIC, "Pe_countX_write2io")

#define PcountX_write2buf(pads, buf, len, buf_full, x, eor_required, count_max, pd, val) \
  PDCI_countX_write2buf(pads, buf, len, buf_full, x, eor_required, count_max, pd, val, PDCI_DEF_CHARSET(pads), "PcountX_write2buf")

#define Pa_countX_write2buf(pads, buf, len, buf_full, x, eor_required, count_max, pd, val) \
  PDCI_countX_write2buf(pads, buf, len, buf_full, x, eor_required, count_max, pd, val, Pcharset_ASCII, "Pa_countX_write2buf")

#define Pe_countX_write2buf(pads, buf, len, buf_full, x, eor_required, count_max, pd, val) \
  PDCI_countX_write2buf(pads, buf, len, buf_full, x, eor_required, count_max, pd, val, Pcharset_EBCDIC, "Pe_countX_write2buf")

#define PcountXtoY_write2io(pads, io, x, y, count_max, pd, val) \
  PDCI_countXtoY_write2io(pads, io, x, y, count_max, pd, val, PDCI_DEF_CHARSET(pads), "PcountXtoY_write2io")

#define Pa_countXtoY_write2io(pads, io, x, y, count_max, pd, val) \
  PDCI_countXtoY_write2io(pads, io, x, y, count_max, pd, val, Pcharset_ASCII, "Pa_countXtoY_write2io")

#define Pe_countXtoY_write2io(pads, io, x, y, count_max, pd, val) \
  PDCI_countXtoY_write2io(pads, io, x, y, count_max, pd, val, Pcharset_EBCDIC, "Pe_countXtoY_write2io")

#define PcountXtoY_write2buf(pads, buf, buf_len, buf_full, x, y, count_max, pd, val) \
  PDCI_countXtoY_write2buf(pads, buf, buf_len, buf_full, x, y, count_max, pd, val, PDCI_DEF_CHARSET(pads), "PcountXtoY_write2buf")

#define Pa_countXtoY_write2buf(pads, buf, buf_len, buf_full, x, y, count_max, pd, val) \
  PDCI_countXtoY_write2buf(pads, buf, buf_len, buf_full, x, y, count_max, pd, val, Pcharset_ASCII, "Pa_countXtoY_write2buf")

#define Pe_countXtoY_write2buf(pads, buf, buf_len, buf_full, x, y, count_max, pd, val) \
  PDCI_countXtoY_write2buf(pads, buf, buf_len, buf_full, x, y, count_max, pd, val, Pcharset_EBCDIC, "Pe_countXtoY_write2buf")

#define Pint8_FW_write2io(pads, io, width, pd, val) \
  PDCI_CHARSET_SWITCH(pads, P, int8_FW_write2io, (pads, io, width, pd, val))

#define Pint16_FW_write2io(pads, io, width, pd, val) \
  PDCI_CHARSET_SWITCH(pads, P, int16_FW_write2io, (pads, io, width, pd, val))

#define Pint32_FW_write2io(pads, io, width, pd, val) \
  PDCI_CHARSET_SWITCH(pads, P, int32_FW_write2io, (pads, io, width, pd, val))

#define Pint64_FW_write2io(pads, io, width, pd, val) \
  PDCI_CHARSET_SWITCH(pads, P, int64_FW_write2io, (pads, io, width, pd, val))

#define Puint8_FW_write2io(pads, io, width, pd, val) \
  PDCI_CHARSET_SWITCH(pads, P, uint8_FW_write2io, (pads, io, width, pd, val))

#define Puint16_FW_write2io(pads, io, width, pd, val) \
  PDCI_CHARSET_SWITCH(pads, P, uint16_FW_write2io, (pads, io, width, pd, val))

#define Puint32_FW_write2io(pads, io, width, pd, val) \
  PDCI_CHARSET_SWITCH(pads, P, uint32_FW_write2io, (pads, io, width, pd, val))

#define Puint64_FW_write2io(pads, io, width, pd, val) \
  PDCI_CHARSET_SWITCH(pads, P, uint64_FW_write2io, (pads, io, width, pd, val))

#define Pint8_write2io(pads, io, pd, val) \
  PDCI_CHARSET_SWITCH(pads, P, int8_write2io, (pads, io, pd, val))

#define Pint16_write2io(pads, io, pd, val) \
  PDCI_CHARSET_SWITCH(pads, P, int16_write2io, (pads, io, pd, val))

#define Pint32_write2io(pads, io, pd, val) \
  PDCI_CHARSET_SWITCH(pads, P, int32_write2io, (pads, io, pd, val))

#define Pint64_write2io(pads, io, pd, val) \
  PDCI_CHARSET_SWITCH(pads, P, int64_write2io, (pads, io, pd, val))

#define Puint8_write2io(pads, io, pd, val) \
  PDCI_CHARSET_SWITCH(pads, P, uint8_write2io, (pads, io, pd, val))

#define Puint16_write2io(pads, io, pd, val) \
  PDCI_CHARSET_SWITCH(pads, P, uint16_write2io, (pads, io, pd, val))

#define Puint32_write2io(pads, io, pd, val) \
  PDCI_CHARSET_SWITCH(pads, P, uint32_write2io, (pads, io, pd, val))

#define Puint64_write2io(pads, io, pd, val) \
  PDCI_CHARSET_SWITCH(pads, P, uint64_write2io, (pads, io, pd, val))

#define Pint8_FW_write2buf(pads, buf, buf_len, buf_full, width, pd, val) \
  PDCI_CHARSET_SWITCH(pads, P, int8_FW_write2buf, (pads, buf, buf_len, buf_full, width, pd, val))

#define Pint16_FW_write2buf(pads, buf, buf_len, buf_full, width, pd, val) \
  PDCI_CHARSET_SWITCH(pads, P, int16_FW_write2buf, (pads, buf, buf_len, buf_full, width, pd, val))

#define Pint32_FW_write2buf(pads, buf, buf_len, buf_full, width, pd, val) \
  PDCI_CHARSET_SWITCH(pads, P, int32_FW_write2buf, (pads, buf, buf_len, buf_full, width, pd, val))

#define Pint64_FW_write2buf(pads, buf, buf_len, buf_full, width, pd, val) \
  PDCI_CHARSET_SWITCH(pads, P, int64_FW_write2buf, (pads, buf, buf_len, buf_full, width, pd, val))

#define Puint8_FW_write2buf(pads, buf, buf_len, buf_full, width, pd, val) \
  PDCI_CHARSET_SWITCH(pads, P, uint8_FW_write2buf, (pads, buf, buf_len, buf_full, width, pd, val))

#define Puint16_FW_write2buf(pads, buf, buf_len, buf_full, width, pd, val) \
  PDCI_CHARSET_SWITCH(pads, P, uint16_FW_write2buf, (pads, buf, buf_len, buf_full, width, pd, val))

#define Puint32_FW_write2buf(pads, buf, buf_len, buf_full, width, pd, val) \
  PDCI_CHARSET_SWITCH(pads, P, uint32_FW_write2buf, (pads, buf, buf_len, buf_full, width, pd, val))

#define Puint64_FW_write2buf(pads, buf, buf_len, buf_full, width, pd, val) \
  PDCI_CHARSET_SWITCH(pads, P, uint64_FW_write2buf, (pads, buf, buf_len, buf_full, width, pd, val))

#define Pint8_write2buf(pads, buf, buf_len, buf_full, pd, val) \
  PDCI_CHARSET_SWITCH(pads, P, int8_write2buf, (pads, buf, buf_len, buf_full, pd, val))

#define Pint16_write2buf(pads, buf, buf_len, buf_full, pd, val) \
  PDCI_CHARSET_SWITCH(pads, P, int16_write2buf, (pads, buf, buf_len, buf_full, pd, val))

#define Pint32_write2buf(pads, buf, buf_len, buf_full, pd, val) \
  PDCI_CHARSET_SWITCH(pads, P, int32_write2buf, (pads, buf, buf_len, buf_full, pd, val))

#define Pint64_write2buf(pads, buf, buf_len, buf_full, pd, val) \
  PDCI_CHARSET_SWITCH(pads, P, int64_write2buf, (pads, buf, buf_len, buf_full, pd, val))

#define Puint8_write2buf(pads, buf, buf_len, buf_full, pd, val) \
  PDCI_CHARSET_SWITCH(pads, P, uint8_write2buf, (pads, buf, buf_len, buf_full, pd, val))

#define Puint16_write2buf(pads, buf, buf_len, buf_full, pd, val) \
  PDCI_CHARSET_SWITCH(pads, P, uint16_write2buf, (pads, buf, buf_len, buf_full, pd, val))

#define Puint32_write2buf(pads, buf, buf_len, buf_full, pd, val) \
  PDCI_CHARSET_SWITCH(pads, P, uint32_write2buf, (pads, buf, buf_len, buf_full, pd, val))

#define Puint64_write2buf(pads, buf, buf_len, buf_full, pd, val) \
  PDCI_CHARSET_SWITCH(pads, P, uint64_write2buf, (pads, buf, buf_len, buf_full, pd, val))

/* _xml_ */

#define Pa_char_lit_write_xml_2io(pads, io, c, tag, indent) \
  PDCI_char_lit_write_xml_2io(pads, io, c, tag, indent, "Pa_char_lit_write_xml_2io")

#define Pe_char_lit_write_xml_2io(pads, io, c, tag, indent) \
  PDCI_char_lit_write_xml_2io(pads, io, c, tag, indent, "Pe_char_lit_write_xml_2io")

#define Pchar_lit_write_xml_2io(pads, io, c, tag, indent) \
  PDCI_char_lit_write_xml_2io(pads, io, c, tag, indent, "Pchar_lit_write_xml_2io")

#define Pa_str_lit_write_xml_2io(pads, io, s, tag, indent) \
  PDCI_str_lit_write_xml_2io(pads, io, s, tag, indent, "Pa_str_lit_write_xml_2io")

#define Pe_str_lit_write_xml_2io(pads, io, s, tag, indent) \
  PDCI_str_lit_write_xml_2io(pads, io, s, tag, indent, "Pe_str_lit_write_xml_2io")

#define Pstr_lit_write_xml_2io(pads, io, s, tag, indent) \
  PDCI_str_lit_write_xml_2io(pads, io, s, tag, indent, "Pstr_lit_write_xml_2io")

#define Pa_cstr_lit_write_xml_2io(pads, io, s, tag, indent) \
  PDCI_cstr_lit_write_xml_2io(pads, io, s, tag, indent, "Pa_cstr_lit_write_xml_2io")

#define Pe_cstr_lit_write_xml_2io(pads, io, s, tag, indent) \
  PDCI_cstr_lit_write_xml_2io(pads, io, s, tag, indent, "Pe_cstr_lit_write_xml_2io")

#define Pcstr_lit_write_xml_2io(pads, io, s, tag, indent) \
  PDCI_cstr_lit_write_xml_2io(pads, io, s, tag, indent, "Pcstr_lit_write_xml_2io")

#define Pa_char_lit_write_xml_2buf(pads, buf, buf_len, buf_full, c, tag, indent) \
  PDCI_char_lit_write_xml_2buf(pads, buf, buf_len, buf_full, c, tag, indent, "Pa_char_lit_write_xml_2buf")

#define Pe_char_lit_write_xml_2buf(pads, buf, buf_len, buf_full, c, tag, indent) \
  PDCI_char_lit_write_xml_2buf(pads, buf, buf_len, buf_full, c, tag, indent, "Pe_char_lit_write_xml_2buf")

#define Pchar_lit_write_xml_2buf(pads, buf, buf_len, buf_full, c, tag, indent) \
  PDCI_char_lit_write_xml_2buf(pads, buf, buf_len, buf_full, c, tag, indent, "Pchar_lit_write_xml_2buf")

#define Pa_str_lit_write_xml_2buf(pads, buf, buf_len, buf_full, s, tag, indent) \
  PDCI_str_lit_write_xml_2buf(pads, buf, buf_len, buf_full, s, tag, indent, "Pa_str_lit_write_xml_2buf")

#define Pe_str_lit_write_xml_2buf(pads, buf, buf_len, buf_full, s, tag, indent) \
  PDCI_str_lit_write_xml_2buf(pads, buf, buf_len, buf_full, s, tag, indent, "Pe_str_lit_write_xml_2buf")

#define Pstr_lit_write_xml_2buf(pads, buf, buf_len, buf_full, s, tag, indent) \
  PDCI_str_lit_write_xml_2buf(pads, buf, buf_len, buf_full, s, tag, indent, "Pstr_lit_write_xml_2buf")

#define Pa_cstr_lit_write_xml_2buf(pads, buf, buf_len, buf_full, s, tag, indent) \
  PDCI_cstr_lit_write_xml_2buf(pads, buf, buf_len, buf_full, s, tag, indent, "Pa_cstr_lit_write_xml_2buf")

#define Pe_cstr_lit_write_xml_2buf(pads, buf, buf_len, buf_full, s, tag, indent) \
  PDCI_cstr_lit_write_xml_2buf(pads, buf, buf_len, buf_full, s, tag, indent, "Pe_cstr_lit_write_xml_2buf")

#define Pcstr_lit_write_xml_2buf(pads, buf, buf_len, buf_full, s, tag, indent) \
  PDCI_cstr_lit_write_xml_2buf(pads, buf, buf_len, buf_full, s, tag, indent, "Pcstr_lit_write_xml_2buf")

#define Pchar_write_xml_2io(pads, io, pd, c, tag, indent) \
  PDCI_char_write_xml_2io(pads, io, pd, c, tag, indent, "Pchar_write_xml_2io")

#define Pa_char_write_xml_2io(pads, io, pd, c, tag, indent) \
  PDCI_char_write_xml_2io(pads, io, pd, c, tag, indent, "Pa_char_write_xml_2io")

#define Pe_char_write_xml_2io(pads, io, pd, c, tag, indent) \
  PDCI_char_write_xml_2io(pads, io, pd, c, tag, indent, "Pe_char_write_xml_2io")

#define Pchar_write_xml_2buf(pads, buf, buf_len, buf_full, pd, c, tag, indent) \
  PDCI_char_write_xml_2buf(pads, buf, buf_len, buf_full, pd, c, tag, indent, "Pchar_write_xml_2buf")

#define Pa_char_write_xml_2buf(pads, buf, buf_len, buf_full, pd, c, tag, indent) \
  PDCI_char_write_xml_2buf(pads, buf, buf_len, buf_full, pd, c, tag, indent, "Pa_char_write_xml_2buf")

#define Pe_char_write_xml_2buf(pads, buf, buf_len, buf_full, pd, c, tag, indent) \
  PDCI_char_write_xml_2buf(pads, buf, buf_len, buf_full, pd, c, tag, indent, "Pe_char_write_xml_2buf")

#define Pstring_FW_write_xml_2io(pads, io, width, pd, s, tag, indent) \
  PDCI_string_FW_write_xml_2io(pads, io, width, pd, s, tag, indent, "Pstring_FW_write_xml_2io")

#define Pa_string_FW_write_xml_2io(pads, io, width, pd, s, tag, indent) \
  PDCI_string_FW_write_xml_2io(pads, io, width, pd, s, tag, indent, "Pa_string_FW_write_xml_2io")

#define Pe_string_FW_write_xml_2io(pads, io, width, pd, s, tag, indent) \
  PDCI_string_FW_write_xml_2io(pads, io, width, pd, s, tag, indent, "Pe_string_FW_write_xml_2io")

#define Pstring_FW_write_xml_2buf(pads, buf, buf_len, buf_full, width, pd, s, tag, indent) \
  PDCI_string_FW_write_xml_2buf(pads, buf, buf_len, buf_full, width, pd, s, tag, indent, "Pstring_FW_write_xml_2buf")

#define Pa_string_FW_write_xml_2buf(pads, buf, buf_len, buf_full, width, pd, s, tag, indent) \
  PDCI_string_FW_write_xml_2buf(pads, buf, buf_len, buf_full, width, pd, s, tag, indent, "Pa_string_FW_write_xml_2buf")

#define Pe_string_FW_write_xml_2buf(pads, buf, buf_len, buf_full, width, pd, s, tag, indent) \
  PDCI_string_FW_write_xml_2buf(pads, buf, buf_len, buf_full, width, pd, s, tag, indent, "Pe_string_FW_write_xml_2buf")

#define Pstring_write_xml_2io(pads, io, stopChar, pd, s, tag, indent) \
  PDCI_string_write_xml_2io_chararg(pads, io, stopChar, pd, s, tag, indent, "Pstring", "Pstring_write_xml_2io")

#define Pa_string_write_xml_2io(pads, io, stopChar, pd, s, tag, indent) \
  PDCI_string_write_xml_2io_chararg(pads, io, stopChar, pd, s, tag, indent, "Pstring", "Pa_string_write_xml_2io")

#define Pe_string_write_xml_2io(pads, io, stopChar, pd, s, tag, indent) \
  PDCI_string_write_xml_2io_chararg(pads, io, stopChar, pd, s, tag, indent, "Pstring", "Pe_string_write_xml_2io")

#define Pstring_write_xml_2buf(pads, buf, buf_len, buf_full, stopChar, pd, s, tag, indent) \
  PDCI_string_write_xml_2buf_chararg(pads, buf, buf_len, buf_full, stopChar, pd, s, tag, indent, "Pstring", "Pstring_write_xml_2buf")

#define Pa_string_write_xml_2buf(pads, buf, buf_len, buf_full, stopChar, pd, s, tag, indent) \
  PDCI_string_write_xml_2buf_chararg(pads, buf, buf_len, buf_full, stopChar, pd, s, tag, indent, "Pstring", "Pa_string_write_xml_2buf")

#define Pe_string_write_xml_2buf(pads, buf, buf_len, buf_full, stopChar, pd, s, tag, indent) \
  PDCI_string_write_xml_2buf_chararg(pads, buf, buf_len, buf_full, stopChar, pd, s, tag, indent, "Pstring", "Pe_string_write_xml_2buf")

#define Pstring_ME_write_xml_2io(pads, io, matchRegexp, pd, s, tag, indent) \
  PDCI_string_write_xml_2io(pads, io, ((void*)(matchRegexp)), pd, s, tag, indent, "Pstring_ME", "Pstring_ME_write_xml_2io")

#define Pa_string_ME_write_xml_2io(pads, io, matchRegexp, pd, s, tag, indent) \
  PDCI_string_write_xml_2io(pads, io, ((void*)(matchRegexp)), pd, s, PDCI_charset_ASCII, tag, indent, "Pstring_ME", "Pa_string_ME_write_xml_2io")

#define Pe_string_ME_write_xml_2io(pads, io, matchRegexp, pd, s, tag, indent) \
  PDCI_string_write_xml_2io(pads, io, ((void*)(matchRegexp)), pd, s, PDCI_charset_EBCDIC, tag, indent, "Pstring_ME", "Pe_string_ME_write_xml_2io")

#define Pstring_ME_write_xml_2buf(pads, buf, buf_len, buf_full, matchRegexp, pd, s, tag, indent) \
  PDCI_string_write_xml_2buf(pads, buf, buf_len, buf_full, ((void*)(matchRegexp)), pd, s, tag, indent, "Pstring_ME", "P_ME_string_write_xml_2buf")

#define Pa_string_ME_write_xml_2buf(pads, buf, buf_len, buf_full, matchRegexp, pd, s, tag, indent) \
  PDCI_string_write_xml_2buf(pads, buf, buf_len, buf_full, ((void*)(matchRegexp)), pd, s, tag, indent, "Pstring_ME", "Pa_string_ME_write_xml_2buf")

#define Pe_string_ME_write_xml_2buf(pads, buf, buf_len, buf_full, matchRegexp, pd, s, tag, indent) \
  PDCI_string_write_xml_2buf(pads, buf, buf_len, buf_full, ((void*)(matchRegexp)), pd, s, tag, indent, "Pstring_ME", "Pe_string_ME_write_xml_2buf")

#define Pstring_CME_write_xml_2io(pads, io, matchRegexp, pd, s, tag, indent) \
  PDCI_string_write_xml_2io(pads, io, ((void*)(matchRegexp)), pd, s, tag, indent, "Pstring_CME", "Pstring_CME_write_xml_2io")

#define Pa_string_CME_write_xml_2io(pads, io, matchRegexp, pd, s, tag, indent) \
  PDCI_string_write_xml_2io(pads, io, ((void*)(matchRegexp)), pd, s, PDCI_charset_ASCII, tag, indent, "Pstring_CME", "Pa_string_CME_write_xml_2io")

#define Pe_string_CME_write_xml_2io(pads, io, matchRegexp, pd, s, tag, indent) \
  PDCI_string_write_xml_2io(pads, io, ((void*)(matchRegexp)), pd, s, PDCI_charset_EBCDIC, tag, indent, "Pstring_CME", "Pe_string_CME_write_xml_2io")

#define Pstring_CME_write_xml_2buf(pads, buf, buf_len, buf_full, matchRegexp, pd, s, tag, indent) \
  PDCI_string_write_xml_2buf(pads, buf, buf_len, buf_full, ((void*)(matchRegexp)), pd, s, tag, indent, "Pstring_CME", "P_CME_string_write_xml_2buf")

#define Pa_string_CME_write_xml_2buf(pads, buf, buf_len, buf_full, matchRegexp, pd, s, tag, indent) \
  PDCI_string_write_xml_2buf(pads, buf, buf_len, buf_full, ((void*)(matchRegexp)), pd, s, tag, indent, "Pstring_CME", "Pa_string_CME_write_xml_2buf")

#define Pe_string_CME_write_xml_2buf(pads, buf, buf_len, buf_full, matchRegexp, pd, s, tag, indent) \
  PDCI_string_write_xml_2buf(pads, buf, buf_len, buf_full, ((void*)(matchRegexp)), pd, s, tag, indent, "Pstring_CME", "Pe_string_CME_write_xml_2buf")

#define Pstring_SE_write_xml_2io(pads, io, stopRegexp, pd, s, tag, indent) \
  PDCI_string_write_xml_2io(pads, io, ((void*)(stopRegexp)), pd, s, tag, indent, "Pstring_SE", "Pstring_SE_write_xml_2io")

#define Pa_string_SE_write_xml_2io(pads, io, stopRegexp, pd, s, tag, indent) \
  PDCI_string_write_xml_2io(pads, io, ((void*)(stopRegexp)), pd, s, PDCI_charset_ASCII, tag, indent, "Pstring_SE", "Pa_string_SE_write_xml_2io")

#define Pe_string_SE_write_xml_2io(pads, io, stopRegexp, pd, s, tag, indent) \
  PDCI_string_write_xml_2io(pads, io, ((void*)(stopRegexp)), pd, s, PDCI_charset_EBCDIC, tag, indent, "Pstring_SE", "Pe_string_SE_write_xml_2io")

#define Pstring_SE_write_xml_2buf(pads, buf, buf_len, buf_full, stopRegexp, pd, s, tag, indent) \
  PDCI_string_write_xml_2buf(pads, buf, buf_len, buf_full, ((void*)(stopRegexp)), pd, s, tag, indent, "Pstring_SE", "P_SE_string_write_xml_2buf")

#define Pa_string_SE_write_xml_2buf(pads, buf, buf_len, buf_full, stopRegexp, pd, s, tag, indent) \
  PDCI_string_write_xml_2buf(pads, buf, buf_len, buf_full, ((void*)(stopRegexp)), pd, s, tag, indent, "Pstring_SE", "Pa_string_SE_write_xml_2buf")

#define Pe_string_SE_write_xml_2buf(pads, buf, buf_len, buf_full, stopRegexp, pd, s, tag, indent) \
  PDCI_string_write_xml_2buf(pads, buf, buf_len, buf_full, ((void*)(stopRegexp)), pd, s, tag, indent, "Pstring_SE", "Pe_string_SE_write_xml_2buf")

#define Pstring_CSE_write_xml_2io(pads, io, stopRegexp, pd, s, tag, indent) \
  PDCI_string_write_xml_2io(pads, io, ((void*)(stopRegexp)), pd, s, tag, indent, "Pstring_CSE", "Pstring_CSE_write_xml_2io")

#define Pa_string_CSE_write_xml_2io(pads, io, stopRegexp, pd, s, tag, indent) \
  PDCI_string_write_xml_2io(pads, io, ((void*)(stopRegexp)), pd, s, PDCI_charset_ASCII, tag, indent, "Pstring_CSE", "Pa_string_CSE_write_xml_2io")

#define Pe_string_CSE_write_xml_2io(pads, io, stopRegexp, pd, s, tag, indent) \
  PDCI_string_write_xml_2io(pads, io, ((void*)(stopRegexp)), pd, s, PDCI_charset_EBCDIC, tag, indent, "Pstring_CSE", "Pe_string_CSE_write_xml_2io")

#define Pstring_CSE_write_xml_2buf(pads, buf, buf_len, buf_full, stopRegexp, pd, s, tag, indent) \
  PDCI_string_write_xml_2buf(pads, buf, buf_len, buf_full, ((void*)(stopRegexp)), pd, s, tag, indent, "Pstring_CSE", "P_CSE_string_write_xml_2buf")

#define Pa_string_CSE_write_xml_2buf(pads, buf, buf_len, buf_full, stopRegexp, pd, s, tag, indent) \
  PDCI_string_write_xml_2buf(pads, buf, buf_len, buf_full, ((void*)(stopRegexp)), pd, s, tag, indent, "Pstring_CSE", "Pa_string_CSE_write_xml_2buf")

#define Pe_string_CSE_write_xml_2buf(pads, buf, buf_len, buf_full, stopRegexp, pd, s, tag, indent) \
  PDCI_string_write_xml_2buf(pads, buf, buf_len, buf_full, ((void*)(stopRegexp)), pd, s, tag, indent, "Pstring_CSE", "Pe_string_CSE_write_xml_2buf")

#define Pdate_write_xml_2io(pads, io, stopChar, pd, d, tag, indent) \
  PDCI_date_write_xml_2io_chararg(pads, io, stopChar, pd, d, tag, indent, "Pdate", "Pdate_write_xml_2io")

#define Pa_date_write_xml_2io(pads, io, stopChar, pd, d, tag, indent) \
  PDCI_date_write_xml_2io_chararg(pads, io, stopChar, pd, d, tag, indent, "Pdate", "Pa_date_write_xml_2io")

#define Pe_date_write_xml_2io(pads, io, stopChar, pd, d, tag, indent) \
  PDCI_date_write_xml_2io_chararg(pads, io, stopChar, pd, d, tag, indent, "Pdate", "Pe_date_write_xml_2io")

#define Pdate_write_xml_2buf(pads, buf, buf_len, buf_full, stopChar, pd, d, tag, indent) \
  PDCI_date_write_xml_2buf_chararg(pads, buf, buf_len, buf_full, stopChar, pd, d, tag, indent, "Pdate", "Pdate_write_xml_2buf")

#define Pa_date_write_xml_2buf(pads, buf, buf_len, buf_full, stopChar, pd, d, tag, indent) \
  PDCI_date_write_xml_2buf_chararg(pads, buf, buf_len, buf_full, stopChar, pd, d, tag, indent, "Pdate", "Pa_date_write_xml_2buf")

#define Pe_date_write_xml_2buf(pads, buf, buf_len, buf_full, stopChar, pd, d, tag, indent) \
  PDCI_date_write_xml_2buf_chararg(pads, buf, buf_len, buf_full, stopChar, pd, d, tag, indent, "Pdate", "Pe_date_write_xml_2buf")

#define PcountX_write_xml_2io(pads, io, x, eor_required, count_max, pd, val, tag, indent) \
  PDCI_countX_write_xml_2io(pads, io, x, eor_required, count_max, pd, val, tag, indent, "PcountX_write_xml_2io")

#define Pa_countX_write_xml_2io(pads, io, x, eor_required, count_max, pd, val, tag, indent) \
  PDCI_countX_write_xml_2io(pads, io, x, eor_required, count_max, pd, val, tag, indent, "Pa_countX_write_xml_2io")

#define Pe_countX_write_xml_2io(pads, io, x, eor_required, count_max, pd, val, tag, indent) \
  PDCI_countX_write_xml_2io(pads, io, x, eor_required, count_max, pd, val, tag, indent, "Pe_countX_write_xml_2io")

#define PcountX_write_xml_2buf(pads, buf, len, buf_full, x, eor_required, count_max, pd, val, tag, indent) \
  PDCI_countX_write_xml_2buf(pads, buf, len, buf_full, x, eor_required, count_max, pd, val, tag, indent, "PcountX_write_xml_2buf")

#define Pa_countX_write_xml_2buf(pads, buf, len, buf_full, x, eor_required, count_max, pd, val, tag, indent) \
  PDCI_countX_write_xml_2buf(pads, buf, len, buf_full, x, eor_required, count_max, pd, val, tag, indent, "Pa_countX_write_xml_2buf")

#define Pe_countX_write_xml_2buf(pads, buf, len, buf_full, x, eor_required, count_max, pd, val, tag, indent) \
  PDCI_countX_write_xml_2buf(pads, buf, len, buf_full, x, eor_required, count_max, pd, val, tag, indent, "Pe_countX_write_xml_2buf")

#define PcountXtoY_write_xml_2io(pads, io, x, y, count_max, pd, val, tag, indent) \
  PDCI_countXtoY_write_xml_2io(pads, io, x, y, count_max, pd, val, tag, indent, "PcountXtoY_write_xml_2io")

#define Pa_countXtoY_write_xml_2io(pads, io, x, y, count_max, pd, val, tag, indent) \
  PDCI_countXtoY_write_xml_2io(pads, io, x, y, count_max, pd, val, tag, indent, "Pa_countXtoY_write_xml_2io")

#define Pe_countXtoY_write_xml_2io(pads, io, x, y, count_max, pd, val, tag, indent) \
  PDCI_countXtoY_write_xml_2io(pads, io, x, y, count_max, pd, val, tag, indent, "Pe_countXtoY_write_xml_2io")

#define PcountXtoY_write_xml_2buf(pads, buf, buf_len, buf_full, x, y, count_max, pd, val, tag, indent) \
  PDCI_countXtoY_write_xml_2buf(pads, buf, buf_len, buf_full, x, y, count_max, pd, val, tag, indent, "PcountXtoY_write_xml_2buf")

#define Pa_countXtoY_write_xml_2buf(pads, buf, buf_len, buf_full, x, y, count_max, pd, val, tag, indent) \
  PDCI_countXtoY_write_xml_2buf(pads, buf, buf_len, buf_full, x, y, count_max, pd, val, tag, indent, "Pa_countXtoY_write_xml_2buf")

#define Pe_countXtoY_write_xml_2buf(pads, buf, buf_len, buf_full, x, y, count_max, pd, val, tag, indent) \
  PDCI_countXtoY_write_xml_2buf(pads, buf, buf_len, buf_full, x, y, count_max, pd, val, tag, indent, "Pe_countXtoY_write_xml_2buf")

#define Pint8_FW_write_xml_2io(pads, io, width, pd, val, tag, indent) \
  PDCI_CHARSET_SWITCH(pads, P, int8_FW_write_xml_2io, (pads, io, width, pd, val, tag, indent))

#define Pint16_FW_write_xml_2io(pads, io, width, pd, val, tag, indent) \
  PDCI_CHARSET_SWITCH(pads, P, int16_FW_write_xml_2io, (pads, io, width, pd, val, tag, indent))

#define Pint32_FW_write_xml_2io(pads, io, width, pd, val, tag, indent) \
  PDCI_CHARSET_SWITCH(pads, P, int32_FW_write_xml_2io, (pads, io, width, pd, val, tag, indent))

#define Pint64_FW_write_xml_2io(pads, io, width, pd, val, tag, indent) \
  PDCI_CHARSET_SWITCH(pads, P, int64_FW_write_xml_2io, (pads, io, width, pd, val, tag, indent))

#define Puint8_FW_write_xml_2io(pads, io, width, pd, val, tag, indent) \
  PDCI_CHARSET_SWITCH(pads, P, uint8_FW_write_xml_2io, (pads, io, width, pd, val, tag, indent))

#define Puint16_FW_write_xml_2io(pads, io, width, pd, val, tag, indent) \
  PDCI_CHARSET_SWITCH(pads, P, uint16_FW_write_xml_2io, (pads, io, width, pd, val, tag, indent))

#define Puint32_FW_write_xml_2io(pads, io, width, pd, val, tag, indent) \
  PDCI_CHARSET_SWITCH(pads, P, uint32_FW_write_xml_2io, (pads, io, width, pd, val, tag, indent))

#define Puint64_FW_write_xml_2io(pads, io, width, pd, val, tag, indent) \
  PDCI_CHARSET_SWITCH(pads, P, uint64_FW_write_xml_2io, (pads, io, width, pd, val, tag, indent))

#define Pint8_write_xml_2io(pads, io, pd, val, tag, indent) \
  PDCI_CHARSET_SWITCH(pads, P, int8_write_xml_2io, (pads, io, pd, val, tag, indent))

#define Pint16_write_xml_2io(pads, io, pd, val, tag, indent) \
  PDCI_CHARSET_SWITCH(pads, P, int16_write_xml_2io, (pads, io, pd, val, tag, indent))

#define Pint32_write_xml_2io(pads, io, pd, val, tag, indent) \
  PDCI_CHARSET_SWITCH(pads, P, int32_write_xml_2io, (pads, io, pd, val, tag, indent))

#define Pint64_write_xml_2io(pads, io, pd, val, tag, indent) \
  PDCI_CHARSET_SWITCH(pads, P, int64_write_xml_2io, (pads, io, pd, val, tag, indent))

#define Puint8_write_xml_2io(pads, io, pd, val, tag, indent) \
  PDCI_CHARSET_SWITCH(pads, P, uint8_write_xml_2io, (pads, io, pd, val, tag, indent))

#define Puint16_write_xml_2io(pads, io, pd, val, tag, indent) \
  PDCI_CHARSET_SWITCH(pads, P, uint16_write_xml_2io, (pads, io, pd, val, tag, indent))

#define Puint32_write_xml_2io(pads, io, pd, val, tag, indent) \
  PDCI_CHARSET_SWITCH(pads, P, uint32_write_xml_2io, (pads, io, pd, val, tag, indent))

#define Puint64_write_xml_2io(pads, io, pd, val, tag, indent) \
  PDCI_CHARSET_SWITCH(pads, P, uint64_write_xml_2io, (pads, io, pd, val, tag, indent))

#define Pint8_FW_write_xml_2buf(pads, buf, buf_len, buf_full, width, pd, val, tag, indent) \
  PDCI_CHARSET_SWITCH(pads, P, int8_FW_write_xml_2buf, (pads, buf, buf_len, buf_full, width, pd, val, tag, indent))

#define Pint16_FW_write_xml_2buf(pads, buf, buf_len, buf_full, width, pd, val, tag, indent) \
  PDCI_CHARSET_SWITCH(pads, P, int16_FW_write_xml_2buf, (pads, buf, buf_len, buf_full, width, pd, val, tag, indent))

#define Pint32_FW_write_xml_2buf(pads, buf, buf_len, buf_full, width, pd, val, tag, indent) \
  PDCI_CHARSET_SWITCH(pads, P, int32_FW_write_xml_2buf, (pads, buf, buf_len, buf_full, width, pd, val, tag, indent))

#define Pint64_FW_write_xml_2buf(pads, buf, buf_len, buf_full, width, pd, val, tag, indent) \
  PDCI_CHARSET_SWITCH(pads, P, int64_FW_write_xml_2buf, (pads, buf, buf_len, buf_full, width, pd, val, tag, indent))

#define Puint8_FW_write_xml_2buf(pads, buf, buf_len, buf_full, width, pd, val, tag, indent) \
  PDCI_CHARSET_SWITCH(pads, P, uint8_FW_write_xml_2buf, (pads, buf, buf_len, buf_full, width, pd, val, tag, indent))

#define Puint16_FW_write_xml_2buf(pads, buf, buf_len, buf_full, width, pd, val, tag, indent) \
  PDCI_CHARSET_SWITCH(pads, P, uint16_FW_write_xml_2buf, (pads, buf, buf_len, buf_full, width, pd, val, tag, indent))

#define Puint32_FW_write_xml_2buf(pads, buf, buf_len, buf_full, width, pd, val, tag, indent) \
  PDCI_CHARSET_SWITCH(pads, P, uint32_FW_write_xml_2buf, (pads, buf, buf_len, buf_full, width, pd, val, tag, indent))

#define Puint64_FW_write_xml_2buf(pads, buf, buf_len, buf_full, width, pd, val, tag, indent) \
  PDCI_CHARSET_SWITCH(pads, P, uint64_FW_write_xml_2buf, (pads, buf, buf_len, buf_full, width, pd, val, tag, indent))

#define Pint8_write_xml_2buf(pads, buf, buf_len, buf_full, pd, val, tag, indent) \
  PDCI_CHARSET_SWITCH(pads, P, int8_write_xml_2buf, (pads, buf, buf_len, buf_full, pd, val, tag, indent))

#define Pint16_write_xml_2buf(pads, buf, buf_len, buf_full, pd, val, tag, indent) \
  PDCI_CHARSET_SWITCH(pads, P, int16_write_xml_2buf, (pads, buf, buf_len, buf_full, pd, val, tag, indent))

#define Pint32_write_xml_2buf(pads, buf, buf_len, buf_full, pd, val, tag, indent) \
  PDCI_CHARSET_SWITCH(pads, P, int32_write_xml_2buf, (pads, buf, buf_len, buf_full, pd, val, tag, indent))

#define Pint64_write_xml_2buf(pads, buf, buf_len, buf_full, pd, val, tag, indent) \
  PDCI_CHARSET_SWITCH(pads, P, int64_write_xml_2buf, (pads, buf, buf_len, buf_full, pd, val, tag, indent))

#define Puint8_write_xml_2buf(pads, buf, buf_len, buf_full, pd, val, tag, indent) \
  PDCI_CHARSET_SWITCH(pads, P, uint8_write_xml_2buf, (pads, buf, buf_len, buf_full, pd, val, tag, indent))

#define Puint16_write_xml_2buf(pads, buf, buf_len, buf_full, pd, val, tag, indent) \
  PDCI_CHARSET_SWITCH(pads, P, uint16_write_xml_2buf, (pads, buf, buf_len, buf_full, pd, val, tag, indent))

#define Puint32_write_xml_2buf(pads, buf, buf_len, buf_full, pd, val, tag, indent) \
  PDCI_CHARSET_SWITCH(pads, P, uint32_write_xml_2buf, (pads, buf, buf_len, buf_full, pd, val, tag, indent))

#define Puint64_write_xml_2buf(pads, buf, buf_len, buf_full, pd, val, tag, indent) \
  PDCI_CHARSET_SWITCH(pads, P, uint64_write_xml_2buf, (pads, buf, buf_len, buf_full, pd, val, tag, indent))

#endif /* P_CONFIG_WRITE_FUNCTIONS */

#endif   /*   ! FOR_CKIT             */
#endif   /*   ! __PADS_IMPL_H__  */
