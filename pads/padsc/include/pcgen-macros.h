## This source file is run through srcgen.pl to produce:
##
##    pcgen-macros-gen.h        : pads codegen macros
##
/* ********************* BEGIN_MACROS(pcgen-macros-gen.h) ********************** */

/*
 * PCGEN_*: pads codegen macros
 * 
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

/* Prototypes for CKIT */

#ifdef FOR_CKIT

void PCGEN_TYPEDEF_READ(const char *fn_nm, Perror_t base_read_call);
void PCGEN_TYPEDEF_READ_REC(const char *fn_nm, Perror_t base_read_call);
void PCGEN_TYPEDEF_READ_CHECK(const char *fn_nm, Perror_t base_read_call, int usercheck);
void PCGEN_TYPEDEF_READ_CHECK_REC(const char *fn_nm, Perror_t base_read_call, int usercheck);

void PCGEN_STRUCT_READ_PRE(const char *fn_nm, void *the_field);
void PCGEN_STRUCT_READ_POST_CHECK(const char *fn_nm, void *the_field, int usercheck);
void PCGEN_STRUCT_READ_POST_CHECK_ENDIAN(const char *fn_nm, void *the_field, int usercheck, Perror_t swap_call);

void PCGEN_STRUCT_READ_FIRST(const char *fn_nm, void *the_field, Perror_t read_call);
void PCGEN_STRUCT_READ_FIRST_CHECK(const char *fn_nm, void *the_field, Perror_t read_call, int usercheck);
void PCGEN_STRUCT_READ_FIRST_CHECK_ENDIAN(const char *fn_nm, void *the_field, Perror_t read_call, int usercheck, Perror_t swap_call);
void PCGEN_STRUCT_READ_NEXT(const char *fn_nm, void *the_field, Perror_t read_call);
void PCGEN_STRUCT_READ_NEXT_CHECK(const char *fn_nm, void *the_field, Perror_t read_call, int usercheck);
void PCGEN_STRUCT_READ_NEXT_CHECK_ENDIAN(const char *fn_nm, void *the_field, Perror_t read_call, int usercheck, Perror_t swap_call);

void PCGEN_STRUCT_READ_FIRST_CHAR_LIT(const char *fn_nm, Pchar char_lit);
void PCGEN_STRUCT_READ_NEXT_CHAR_LIT(const char *fn_nm, Pchar char_lit);
void PCGEN_STRUCT_READ_FIRST_STR_LIT(const char *fn_nm, const char *str_lit, size_t str_len_expr);
void PCGEN_STRUCT_READ_NEXT_STR_LIT(const char *fn_nm, const char *str_lit, size_t str_len_expr);
void PCGEN_STRUCT_READ_FIRST_REGEXP(const char *fn_nm, const char *regexp_str);
void PCGEN_STRUCT_READ_NEXT_REGEXP(const char *fn_nm, const char *regexp_str);

void PCGEN_ALT_READ_BEGIN(const char *fn_nm);
void PCGEN_ALT_READ_END(const char *fn_nm);

void PCGEN_ALT_READ_PRE(const char *fn_nm, void *the_field);
void PCGEN_ALT_READ_POST_CHECK(const char *fn_nm, void *the_field, int usercheck);
void PCGEN_ALT_READ_POST_CHECK_ENDIAN(const char *fn_nm, void *the_field, int usercheck, Perror_t swap_call);

void PCGEN_ALT_READ(const char *fn_nm, void *the_field, Perror_t read_call);
void PCGEN_ALT_READ_CHECK(const char *fn_nm, void *the_field, Perror_t read_call, int usercheck);
void PCGEN_ALT_READ_CHECK_ENDIAN(const char *fn_nm, void *the_field, Perror_t read_call, int usercheck, Perror_t swap_call);

void PCGEN_ALT_READ_CHAR_LIT(const char *fn_nm, Pchar char_lit);
void PCGEN_ALT_READ_STR_LIT(const char *fn_nm, const char *str_lit, size_t str_len_expr);
void PCGEN_ALT_READ_REGEXP(const char *fn_nm, const char *regexp_str);

void PCGEN_UNION_READ_SETUP_STAT(const char *fn_nm, int the_tag, void *rep_cleanup, void *rep_init, void *pd_cleanup, void *pd_init);
void PCGEN_UNION_READ_SETUP(const char *fn_nm, int the_tag, void *rep_cleanup, void *rep_init, void *pd_cleanup, void *pd_init);

#define PDCI_UNION_READ_ARGS const char *fn_nm, int the_tag, \
                             void *rep_cleanup, void *rep_init, \
                             void *pd_cleanup, void *pd_init, Perror_t read_call

void PCGEN_UNION_READ_STAT        (PDCI_UNION_READ_ARGS);
void PCGEN_UNION_READ_FIRST       (PDCI_UNION_READ_ARGS);
void PCGEN_UNION_READ_NEXT        (PDCI_UNION_READ_ARGS);
void PCGEN_UNION_READ_LAST        (PDCI_UNION_READ_ARGS);

void PCGEN_UNION_READ_STAT_CHECK  (PDCI_UNION_READ_ARGS, int usercheck);
void PCGEN_UNION_READ_FIRST_CHECK (PDCI_UNION_READ_ARGS, int usercheck);
void PCGEN_UNION_READ_NEXT_CHECK  (PDCI_UNION_READ_ARGS, int usercheck);
void PCGEN_UNION_READ_LAST_CHECK  (PDCI_UNION_READ_ARGS, int usercheck);

#define PDCI_UNION_READ_MAN_PRE_ARGS const char *fn_nm, int the_tag, void *rep_init, void *pd_init

void PCGEN_UNION_READ_MAN_STAT_PRE       (PDCI_UNION_READ_MAN_PRE_ARGS);
void PCGEN_UNION_READ_MAN_FIRST_PRE      (PDCI_UNION_READ_MAN_PRE_ARGS);
void PCGEN_UNION_READ_MAN_NEXT_PRE       (PDCI_UNION_READ_MAN_PRE_ARGS);

void PCGEN_UNION_READ_MAN_STAT_VIRT_PRE  (PDCI_UNION_READ_MAN_PRE_ARGS);
void PCGEN_UNION_READ_MAN_FIRST_VIRT_PRE (PDCI_UNION_READ_MAN_PRE_ARGS);
void PCGEN_UNION_READ_MAN_NEXT_VIRT_PRE  (PDCI_UNION_READ_MAN_PRE_ARGS);

void PCGEN_UNION_READ_MAN_STAT_POST       (const char *fn_nm);
void PCGEN_UNION_READ_MAN_POST            (const char *fn_nm);
void PCGEN_UNION_READ_MAN_STAT_POST_CHECK (const char *fn_nm, void *rep_cleanup, void *pd_cleanup, int usercheck);
void PCGEN_UNION_READ_MAN_POST_CHECK      (const char *fn_nm, void *rep_cleanup, void *pd_cleanup, int usercheck);

void PCGEN_UNION_READ_FAILED      (const char *fn_nm, const char *nm, int err_tag);
void PCGEN_UNION_READ_WHERE_CHECK (const char *fn_nm, int usercheck);
void PCGEN_UNION_READ_WHERE_END_CHECK (const char *fn_nm, int usercheck);

#define PDCI_SWUNION_READ_ARGS const char *fn_nm, int the_tag, int err_tag, \
                               void *rep_cleanup, void *rep_init, \
                               void *pd_cleanup, void *pd_init, Perror_t read_call

void PCGEN_SWUNION_READ_STAT (PDCI_SWUNION_READ_ARGS);
void PCGEN_SWUNION_READ      (PDCI_SWUNION_READ_ARGS);

#define PDCI_SWUNION_READ_MAN_PRE_ARGS const char *fn_nm, int the_tag, \
                                       void *rep_cleanup, void *rep_init, \
                                       void *pd_cleanup, void *pd_init

void PCGEN_SWUNION_READ_MAN_STAT_PRE      (PDCI_SWUNION_READ_MAN_PRE_ARGS);
void PCGEN_SWUNION_READ_MAN_STAT_VIRT_PRE (PDCI_SWUNION_READ_MAN_PRE_ARGS);
void PCGEN_SWUNION_READ_MAN_PRE           (PDCI_SWUNION_READ_MAN_PRE_ARGS);
void PCGEN_SWUNION_READ_MAN_VIRT_PRE      (PDCI_SWUNION_READ_MAN_PRE_ARGS);

void PCGEN_SWUNION_READ_POST_CHECK  (const char *fn_nm, int the_tag, int err_tag, int usercheck);
void PCGEN_SWUNION_READ_FAILED      (const char *fn_nm, const char *nm, int err_tag);
void PCGEN_SWUNION_READ_WHERE_CHECK (const char *fn_nm, int usercheck);
void PCGEN_SWUNION_READ_WHERE_END_CHECK (const char *fn_nm, int usercheck);

void PCGEN_FIND_EOR(const char *fn_nm);

void PCGEN_STRUCT_ACC_REP_NOVALS();
void PCGEN_UNION_ACC_REP_NOVALS();
void PCGEN_ARRAY_ACC_REP_NOVALS();
Perror_t PCGEN_ENUM_ACC_REP2IO(const char *default_what, Perror_t int_acc_call);
Perror_t PCGEN_TYPEDEF_ACC_REP2IO(const char *default_what, const char *basety_nm, Perror_t base_acc_call);

void PCGEN_WRITE2IO_USE_WRITE2BUF(const char *fn_nm, ssize_t write2buf_call);

void PCGEN_TLEN_UPDATES();
void PCGEN_FINAL_TLEN_UPDATES();
void PCGEN_TAG_OPEN_XML_OUT(const char *def_tag);
void PCGEN_TAG_CLOSE_XML_OUT();

void PCGEN_STRUCT_PD_XML_OUT();
void PCGEN_ARRAY_PD_XML_OUT();
void PCGEN_UNION_PD_XML_OUT();

void PCGEN_ENUM_XML_OUT(const char *def_tag, const char *(rep2str_fn)(int));

#else
/* The actual macros */
/* ********************************** END_HEADER ********************************** */

#define PCGEN_FIND_EOR(fn_nm)
do {
  Pbase_pd tpd;
  size_t bytes_skipped;
  PDCI_IO_BEGINLOC(pads, tpd.loc);
  if (P_OK == P_io_next_rec(pads, &bytes_skipped)) {
    if (bytes_skipped) {
      if (P_spec_level(pads)) return P_ERR;
      PDCI_IO_ENDLOC_MINUS1(pads, tpd.loc);
      if (!P_PS_isPanic(pd)) {
	PDCI_report_err(pads, P_LEV_WARN, &(tpd.loc), P_EXTRA_BEFORE_EOR, fn_nm, "Unexpected data before EOR");
	if (pd->nerr == 0) {
	  pd->errCode = P_EXTRA_BEFORE_EOR;
	  pd->loc = tpd.loc;
	}
	(pd->nerr)++;
      } else {
	PDCI_report_err(pads, P_LEV_INFO, &(tpd.loc), P_NO_ERR, fn_nm, "Resynching at EOR");
      }
    }
    P_PS_unsetPanic(pd);
  } else {
    if (P_spec_level(pads)) return P_ERR;
    P_PS_unsetPanic(pd);
    PDCI_IO_ENDLOC_MINUS1(pads, tpd.loc);
    PDCI_report_err(pads, P_LEV_WARN, &(tpd.loc), P_EOF_BEFORE_EOR, fn_nm, "Found EOF when searching for EOR");
    if (pd->nerr == 0) {
      pd->errCode = P_EOF_BEFORE_EOR;
      pd->loc = tpd.loc;
    }
    (pd->nerr)++;
  }
} while (0)
/* END_MACRO */

/* XXX should it be -1 or 0 below ??? */
#define PDCI_CONSTRAINT_ERR(fn_nm, ecode, msg)
do {
  (pd->nerr)++;
  pd->errCode = ecode;
  PDCI_IO_ENDLOC_MINUS1(pads, pd->loc);
  PDCI_report_err(pads, P_LEV_WARN, &(pd->loc), pd->errCode, fn_nm, msg);
} while (0)
/* END_MACRO */

/* XXX should it be -1 or 0 below ??? */
#define PDCI_ELT_CONSTRAINT_ERR(fn_nm, elt_pd, elt_ecode, top_ecode, msg)
do {
  (elt_pd).nerr = 1;
  (elt_pd).errCode = elt_ecode;
  PDCI_IO_ENDLOC_MINUS1(pads, (elt_pd).loc);
  PDCI_report_err(pads, P_LEV_WARN, &((elt_pd).loc), (elt_pd).errCode, fn_nm, msg);
  if (pd->nerr == 0) {
    pd->errCode = top_ecode;
    pd->loc = (elt_pd).loc;
  }
  (pd->nerr)++;
} while (0)
/* END_MACRO */

/* for the following 4 macros, pd and rep shared with base type */
/* base_read_call reports error, fills in pd->nerr */

/* invoke this macro, return (pd->nerr == 0) ? P_OK : P_ERR */
#define PCGEN_TYPEDEF_READ(fn_nm, base_read_call)
do {
  PDCI_IODISC_3P_CHECKS (fn_nm, m, pd, rep);
  base_read_call;
} while (0)
/* END_MACRO */

/* invoke this macro, return (pd->nerr == 0) ? P_OK : P_ERR */
#define PCGEN_TYPEDEF_READ_REC(fn_nm, base_read_call)
do {
  PDCI_IODISC_3P_CHECKS (fn_nm, m, pd, rep);
  base_read_call;
  PCGEN_FIND_EOR(fn_nm);
} while (0)
/* END_MACRO */

/* invoke this macro, return (pd->nerr == 0) ? P_OK : P_ERR */
#define PCGEN_TYPEDEF_READ_CHECK(fn_nm, base_read_call, usercheck)
do {
  PDCI_IODISC_3P_CHECKS (fn_nm, m, pd, rep);
  if (P_OK == (base_read_call) && P_Test_SemCheck (m->user) && (!(usercheck))) {
    PDCI_CONSTRAINT_ERR(fn_nm, P_TYPEDEF_CONSTRAINT_ERR, 0);
  }
} while (0)
/* END_MACRO */

/* invoke this macro, return (pd->nerr == 0) ? P_OK : P_ERR */
#define PCGEN_TYPEDEF_READ_CHECK_REC(fn_nm, base_read_call, usercheck)
do {
  PDCI_IODISC_3P_CHECKS (fn_nm, m, pd, rep);
  if (P_OK == (base_read_call) && P_Test_SemCheck (m->user) && (!(usercheck))) {
    PDCI_CONSTRAINT_ERR(fn_nm, P_TYPEDEF_CONSTRAINT_ERR, 0);
  }
  PCGEN_FIND_EOR(fn_nm);
} while (0)
/* END_MACRO */

#define PCGEN_STRUCT_READ_PRE(fn_nm, the_field)
do {
  pd->the_field.errCode = P_NO_ERR;
  PDCI_IO_BEGINLOC(pads, pd->the_field.loc);
} while (0)
/* END_MACRO */

#define PDCI_STRUCT_ELT_CONSTRAINT_ERR(fn_nm, the_field)
  if (P_spec_level(pads)) return P_ERR;
  PDCI_ELT_CONSTRAINT_ERR(fn_nm, pd->the_field, P_USER_CONSTRAINT_VIOLATION,
                          P_STRUCT_FIELD_ERR, "User constraint on field " PDCI_MacroArg2String(the_field) " violated")
/* END_MACRO */

#define PCGEN_STRUCT_READ_POST_CHECK(fn_nm, the_field, usercheck)
do {
  if (P_Test_SemCheck(m->the_field ## _con) && (!(usercheck))) {
    PDCI_STRUCT_ELT_CONSTRAINT_ERR(fn_nm, the_field);
  }
} while (0)
/* END_MACRO */

#define PCGEN_STRUCT_READ_POST_CHECK_ENDIAN(fn_nm, the_field, usercheck, swap_call)
do {
  if (P_Test_SemCheck(m->the_field ## _con) && (!(usercheck))) {
    PDCI_IO_ENDLOC_MINUS1(pads, pd->the_field.loc);
    swap_call;
    if (usercheck) {
       pads->disc->d_endian = ((pads->disc->d_endian == PbigEndian) ? PlittleEndian : PbigEndian);
       PDCI_report_err(pads, P_LEV_INFO, &(pd->the_field.loc), P_NO_ERR, fn_nm,
                       "New data endian value: %s.  Machine endian value: %s (from " PDCI_MacroArg2String(the_field) " field test)",
                       Pendian2str(pads->disc->d_endian), Pendian2str(pads->m_endian));
    } else {
      swap_call;
      PDCI_STRUCT_ELT_CONSTRAINT_ERR(fn_nm, the_field);
    }
  }
} while (0)
/* END_MACRO */

#define PDCI_STRUCT_READ_FIELD(fn_nm, the_field, read_call)
  PDCI_IO_BEGINLOC(pads, pd->the_field.loc);
  if (P_ERR == read_call) {
    if (P_spec_level(pads)) return P_ERR;
    if (P_PS_isPanic(&(pd->the_field))) {
      P_PS_setPanic(pd);
    }
    if (pd->nerr == 0) {
      pd->errCode = P_STRUCT_FIELD_ERR;
      pd->loc = pd->the_field.loc;
    }
    (pd->nerr)++;
  }
/* END_MACRO */

#define PDCI_STRUCT_READ_HANDLE_PANIC(fn_nm, the_field)
  P_PS_setPanic(&(pd->the_field));
  pd->the_field.errCode = P_PANIC_SKIPPED;
  pd->the_field.nerr = 1;
  PDCI_IO_GETLOC_SPAN0(pads, pd->the_field.loc);
  (pd->nerr)++;
/* END_MACRO */

#define PCGEN_STRUCT_READ_FIRST(fn_nm, the_field, read_call)
do {
  PDCI_STRUCT_READ_FIELD(fn_nm, the_field, read_call)
} while (0)
/* END_MACRO */

#define PCGEN_STRUCT_READ_FIRST_CHECK(fn_nm, the_field, read_call, usercheck)
do {
  PDCI_STRUCT_READ_FIELD(fn_nm, the_field, read_call)
  else PCGEN_STRUCT_READ_POST_CHECK(fn_nm, the_field, usercheck);
} while (0)
/* END_MACRO */

#define PCGEN_STRUCT_READ_FIRST_CHECK_ENDIAN(fn_nm, the_field, read_call, usercheck, swap_call)
do {
  PDCI_STRUCT_READ_FIELD(fn_nm, the_field, read_call)
  else PCGEN_STRUCT_READ_POST_CHECK_ENDIAN(fn_nm, the_field, usercheck, swap_call);
} while (0)
/* END_MACRO */

#define PCGEN_STRUCT_READ_NEXT(fn_nm, the_field, read_call)
do {
  if (P_PS_isPanic(pd)) {
    PDCI_STRUCT_READ_HANDLE_PANIC(fn_nm, the_field)
  } else {
    PDCI_STRUCT_READ_FIELD(fn_nm, the_field, read_call)
  }
} while (0)
/* END_MACRO */

#define PCGEN_STRUCT_READ_NEXT_CHECK(fn_nm, the_field, read_call, usercheck)
do {
  if (P_PS_isPanic(pd)) {
    PDCI_STRUCT_READ_HANDLE_PANIC(fn_nm, the_field)
  } else {
    PDCI_STRUCT_READ_FIELD(fn_nm, the_field, read_call)
    else PCGEN_STRUCT_READ_POST_CHECK(fn_nm, the_field, usercheck);
  }
} while (0)
/* END_MACRO */

#define PCGEN_STRUCT_READ_NEXT_CHECK_ENDIAN(fn_nm, the_field, read_call, usercheck, swap_call)
do {
  if (P_PS_isPanic(pd)) {
    PDCI_STRUCT_READ_HANDLE_PANIC(fn_nm, the_field)
  } else {
    PDCI_STRUCT_READ_FIELD(fn_nm, the_field, read_call)
    else PCGEN_STRUCT_READ_POST_CHECK_ENDIAN(fn_nm, the_field, usercheck, swap_call);
  }
} while (0)
/* END_MACRO */

#define PDCI_STRUCT_READ_CHAR_LIT(fn_nm, char_lit)
do {
  Pbase_pd tpd;
  size_t toffset;
  PDCI_IO_BEGINLOC(pads, tpd.loc);
  if (P_OK == Pchar_lit_scan1(pads, char_lit, 1, 0, &toffset)) {
    if (toffset) {
      if (P_spec_level(pads)) return P_ERR;
      if (pd->nerr == 0) {
	pd->errCode = P_STRUCT_EXTRA_BEFORE_SEP;
	PDCI_IO_ENDLOC_MINUS2(pads, tpd.loc);
	pd->loc = tpd.loc;
	PDCI_report_err(pads, P_LEV_WARN, &(tpd.loc), P_STRUCT_EXTRA_BEFORE_SEP, fn_nm,
			"Extra data before char separator %s", P_qfmt_char(char_lit));
      }
      (pd->nerr)++;
    }
  } else {
    if (P_spec_level(pads)) return P_ERR;
    if (pd->nerr == 0) {
      pd->errCode = P_MISSING_LITERAL;
      PDCI_IO_ENDLOC_MINUS1(pads, tpd.loc);
      pd->loc = tpd.loc;
      PDCI_report_err(pads, P_LEV_WARN, &(tpd.loc), P_MISSING_LITERAL, fn_nm,
		      "Missing char separator %s", P_qfmt_char(char_lit));
    }
    (pd->nerr)++;
    P_PS_setPanic(pd);
  }
} while (0)
/* END_MACRO */

#define PCGEN_STRUCT_READ_FIRST_CHAR_LIT(fn_nm, char_lit)
  PDCI_STRUCT_READ_CHAR_LIT(fn_nm, char_lit)
/* END_MACRO */

#define PCGEN_STRUCT_READ_NEXT_CHAR_LIT(fn_nm, char_lit)
do {
  if (P_PS_isPanic(pd)) {
    size_t toffset;
    if (P_ERR != Pchar_lit_scan1(pads, char_lit, 1, 1, &toffset)) {
      P_PS_unsetPanic(pd);
    }
  } else PDCI_STRUCT_READ_CHAR_LIT(fn_nm, char_lit);
} while (0)
/* END_MACRO */

#define PDCI_STRUCT_READ_STR_LIT(fn_nm, pads_str_lit)
do {
  Pbase_pd tpd;
  size_t toffset;
  PDCI_IO_BEGINLOC(pads, tpd.loc);
  if (P_OK == Pstr_lit_scan1(pads, pads_str_lit, 1, 0, &toffset)) {
    if (toffset) {
      if (P_spec_level(pads)) return P_ERR;
      if (pd->nerr == 0) {
	pd->errCode = P_STRUCT_EXTRA_BEFORE_SEP;
	PDCI_IO_ENDLOC_MINUS2(pads, tpd.loc);
	pd->loc = tpd.loc;
	PDCI_report_err(pads, P_LEV_WARN, &(tpd.loc), P_STRUCT_EXTRA_BEFORE_SEP, fn_nm,
			"Extra data before string separator %s", P_qfmt_str(pads_str_lit));
      }
      (pd->nerr)++;
    }
  } else {
    if (P_spec_level(pads)) return P_ERR;
    if (pd->nerr == 0) {
      pd->errCode = P_MISSING_LITERAL;
      PDCI_IO_ENDLOC_MINUS1(pads, tpd.loc);
      pd->loc = tpd.loc;
      PDCI_report_err(pads, P_LEV_WARN, &(tpd.loc), P_MISSING_LITERAL, fn_nm,
		      "Missing string separator %s", P_qfmt_str(pads_str_lit));
    }
    (pd->nerr)++;
    P_PS_setPanic(pd);
  }
} while (0)
/* END_MACRO */

#define PCGEN_STRUCT_READ_FIRST_STR_LIT(fn_nm, str_lit, str_len_expr)
do {
  P_STRING_DECL_CSTR_LEN(tstr, str_lit, str_len_expr);
  PDCI_STRUCT_READ_STR_LIT(fn_nm, &tstr);
} while (0)
/* END_MACRO */

#define PCGEN_STRUCT_READ_NEXT_STR_LIT(fn_nm, str_lit, str_len_expr)
do {
  P_STRING_DECL_CSTR_LEN(tstr, str_lit, str_len_expr);
  if (P_PS_isPanic(pd)) {
    size_t toffset;
    if (P_ERR != Pstr_lit_scan1(pads, &tstr, 1, 1, &toffset)) {
      P_PS_unsetPanic(pd);
    }
  } else PDCI_STRUCT_READ_STR_LIT(fn_nm, &tstr);
} while (0)
/* END_MACRO */

#define PDCI_STRUCT_READ_REGEXP(fn_nm, regexp, regexp_str)
do {
  Perror_t terr;
  Pbase_pd tpd;
  size_t   toffset;
  PDCI_IO_BEGINLOC(pads, tpd.loc);
  terr = Pre_scan1(pads, regexp, 1, 0, &toffset);
  Pregexp_cleanup(pads, regexp);
  if (terr == P_OK) {
    if (toffset) {
      if (P_spec_level(pads)) return P_ERR;
      if (pd->nerr == 0) {
	pd->errCode = P_STRUCT_EXTRA_BEFORE_SEP;
	PDCI_IO_ENDLOC_MINUS2(pads, tpd.loc);
	pd->loc = tpd.loc;
	PDCI_report_err(pads, P_LEV_WARN, &(tpd.loc), P_STRUCT_EXTRA_BEFORE_SEP, fn_nm,
			"Extra data before regexp separator Pre %s", P_qfmt_cstr(regexp_str));
      }
      (pd->nerr)++;
    }
  } else {
    if (P_spec_level(pads)) return P_ERR;
    if (pd->nerr == 0) {
      pd->errCode = P_MISSING_LITERAL;
      PDCI_IO_ENDLOC_MINUS1(pads, tpd.loc);
      pd->loc = tpd.loc;
      PDCI_report_err(pads, P_LEV_WARN, &(tpd.loc), P_MISSING_LITERAL, fn_nm,
		      "Missing regexp separator Pre %s", P_qfmt_cstr(regexp_str));
    }
    (pd->nerr)++;
    P_PS_setPanic(pd);
  }
} while (0)
/* END_MACRO */

#define PCGEN_STRUCT_READ_FIRST_REGEXP(fn_nm, regexp_str)
do {
  P_REGEXP_DECL_NULL(tregexp);
  PDCI_regexp_compile_cstr(pads, regexp_str, &tregexp, "Regexp-specified separator", fn_nm);
  if (!tregexp.valid) {
    pd->errCode = P_INVALID_REGEXP;
    (pd->nerr)++;
    P_PS_setPanic(pd);
  } else {
    PDCI_STRUCT_READ_REGEXP(fn_nm, &tregexp, regexp_str);
  }
} while (0)
/* END_MACRO */

#define PCGEN_STRUCT_READ_NEXT_REGEXP(fn_nm, regexp_str)
do {
  P_REGEXP_DECL_NULL(tregexp);
  PDCI_regexp_compile_cstr(pads, regexp_str, &tregexp, "Regexp-specified separator", fn_nm);
  if (!tregexp.valid) {
    pd->errCode = P_INVALID_REGEXP;
    (pd->nerr)++;
    P_PS_setPanic(pd);
  } else {
    if (P_PS_isPanic(pd)) {
      Perror_t terr;
      size_t toffset;
      terr = Pre_scan1(pads, &tregexp, 1, 1, &toffset);
      Pregexp_cleanup(pads, &tregexp);
      if (terr == P_OK) {
	P_PS_unsetPanic(pd);
      }
    } else {
      PDCI_STRUCT_READ_REGEXP(fn_nm, &tregexp, regexp_str);
    }
  }
} while (0)
/* END_MACRO */

#define PDCI_ALT_ELT_CONSTRAINT_ERR(fn_nm, the_field)
  if (!P_spec_level(pads)) {
    PDCI_ELT_CONSTRAINT_ERR(fn_nm, pd->the_field, P_USER_CONSTRAINT_VIOLATION,
			    P_STRUCT_FIELD_ERR, "User constraint on field " PDCI_MacroArg2String(the_field) " violated")
  }
/* END_MACRO */

#define PCGEN_ALT_READ_BEGIN(fn_nm)
  Ppos_t start_pos, cur_pos, max_pos;
  int no_panic = 0, moved_pos = 0;
  do {
    PDCI_IO_GETPOS(pads, start_pos);
    max_pos = start_pos;
    if (P_ERR == P_io_checkpoint(pads, 0)) {
      PDCI_report_err(pads, P_LEV_FATAL, 0, P_CHKPOINT_ERR, fn_nm, 0);
    }
  } while (0)
/* END_MACRO */

#define PCGEN_ALT_READ_END(fn_nm)
do {
  if (no_panic) {
    P_PS_unsetPanic(pd);
  } else {
    P_PS_setPanic(pd);
  }
  if (moved_pos) {
    if (P_ERR == P_io_commit_pos(pads, max_pos)) {
      PDCI_report_err(pads, P_LEV_FATAL, 0, P_COMMIT_ERR, fn_nm, 0);
    }
  } else {
    if (P_ERR == P_io_restore(pads)) {
      PDCI_report_err(pads, P_LEV_FATAL, 0, P_RESTORE_ERR, fn_nm, 0);
    }
    PDCI_report_err(pads, P_LEV_WARN, 0, P_NO_ERR, fn_nm, "No branch of this Palternates advanced the IO cursor");
  }
} while (0)
/* END_MACRO */

#define PDCI_ALT_READ_FIELD_PRE(fn_nm)
  if (P_ERR == P_io_checkpoint(pads, 0)) {
    PDCI_report_err(pads, P_LEV_FATAL, 0, P_CHKPOINT_ERR, fn_nm, 0);
  }
  P_PS_unsetPanic(pd);
/* END_MACRO */

#define PDCI_ALT_READ_FIELD_POST(fn_nm)
  if (!P_PS_isPanic(pd)) {
    no_panic = 1;
    PDCI_IO_GETPOS(pads, cur_pos);
    error(0, "XXX_REMOVE cur_pos.offset %lu max_pos.offset %lu",
	  (unsigned long)cur_pos.offset, (unsigned long)max_pos.offset); 
    if (P_POS_GT(cur_pos, max_pos)) {
      max_pos = cur_pos;
      moved_pos = 1;
      error(0, "XXX_REMOVE modified max_pos");
    }
  }
  if (P_ERR == P_io_restore(pads)) {
    PDCI_report_err(pads, P_LEV_FATAL, 0, P_RESTORE_ERR, fn_nm, 0);
  }
/* END_MACRO */

#define PCGEN_ALT_READ_PRE(fn_nm, the_field)
do {
  pd->the_field.errCode = P_NO_ERR;
  PDCI_IO_BEGINLOC(pads, pd->the_field.loc);
  P_PS_unsetPanic(pd);
} while (0)
/* END_MACRO */

#define PCGEN_ALT_READ_POST_CHECK(fn_nm, the_field, usercheck)
do {
  if (P_Test_SemCheck(m->the_field ## _con) && (!(usercheck))) {
    PDCI_ALT_ELT_CONSTRAINT_ERR(fn_nm, the_field);
  }
} while (0)
/* END_MACRO */

#define PCGEN_ALT_READ_POST_CHECK_ENDIAN(fn_nm, the_field, usercheck, swap_call)
do {
  if (P_Test_SemCheck(m->the_field ## _con) && (!(usercheck))) {
    PDCI_IO_ENDLOC_MINUS1(pads, pd->the_field.loc);
    swap_call;
    if (usercheck) {
       pads->disc->d_endian = ((pads->disc->d_endian == PbigEndian) ? PlittleEndian : PbigEndian);
       PDCI_report_err(pads, P_LEV_INFO, &(pd->the_field.loc), P_NO_ERR, fn_nm,
                       "New data endian value: %s.  Machine endian value: %s (from " PDCI_MacroArg2String(the_field) " field test)",
                       Pendian2str(pads->disc->d_endian), Pendian2str(pads->m_endian));
    } else {
      swap_call;
      PDCI_ALT_ELT_CONSTRAINT_ERR(fn_nm, the_field);
    }
  }
} while (0)
/* END_MACRO */

#define PDCI_ALT_READ_FIELD(fn_nm, the_field, read_call)
  PDCI_IO_BEGINLOC(pads, pd->the_field.loc);
  if (P_ERR == read_call) {
    if (P_PS_isPanic(&(pd->the_field))) {
      P_PS_setPanic(pd);
    }
    if (pd->nerr == 0) {
      pd->errCode = P_STRUCT_FIELD_ERR;
      pd->loc = pd->the_field.loc;
    }
    (pd->nerr)++;
  }
/* END_MACRO */

#define PCGEN_ALT_READ(fn_nm, the_field, read_call)
do {
  PDCI_ALT_READ_FIELD_PRE(fn_nm)
  PDCI_ALT_READ_FIELD(fn_nm, the_field, read_call)
  PDCI_ALT_READ_FIELD_POST(fn_nm)
} while (0)
/* END_MACRO */

#define PCGEN_ALT_READ_CHECK(fn_nm, the_field, read_call, usercheck)
do {
  PDCI_ALT_READ_FIELD_PRE(fn_nm)
  PDCI_ALT_READ_FIELD(fn_nm, the_field, read_call)
  else PCGEN_ALT_READ_POST_CHECK(fn_nm, the_field, usercheck);
  PDCI_ALT_READ_FIELD_POST(fn_nm)
} while (0)
/* END_MACRO */

#define PCGEN_ALT_READ_CHECK_ENDIAN(fn_nm, the_field, read_call, usercheck, swap_call)
do {
  PDCI_ALT_READ_FIELD_PRE(fn_nm)
  PDCI_ALT_READ_FIELD(fn_nm, the_field, read_call)
  else PCGEN_ALT_READ_POST_CHECK_ENDIAN(fn_nm, the_field, usercheck, swap_call);
  PDCI_ALT_READ_FIELD_POST(fn_nm)
} while (0)
/* END_MACRO */

#define PDCI_ALT_READ_CHAR_LIT(fn_nm, char_lit)
do {
  Pbase_pd tpd;
  size_t toffset;
  PDCI_IO_BEGINLOC(pads, tpd.loc);
  if (P_OK == Pchar_lit_scan1(pads, char_lit, 1, 0, &toffset)) {
    if (toffset) {
      if (pd->nerr == 0) {
	pd->errCode = P_STRUCT_EXTRA_BEFORE_SEP;
	PDCI_IO_ENDLOC_MINUS2(pads, tpd.loc);
	pd->loc = tpd.loc;
	if (!P_spec_level(pads)) {
	  PDCI_report_err(pads, P_LEV_WARN, &(tpd.loc), P_STRUCT_EXTRA_BEFORE_SEP, fn_nm,
			  "Extra data before char separator %s", P_qfmt_char(char_lit));
	}
      }
      (pd->nerr)++;
    }
  } else {
    if (pd->nerr == 0) {
      pd->errCode = P_MISSING_LITERAL;
      PDCI_IO_ENDLOC_MINUS1(pads, tpd.loc);
      pd->loc = tpd.loc;
      if (!P_spec_level(pads)) {
	PDCI_report_err(pads, P_LEV_WARN, &(tpd.loc), P_MISSING_LITERAL, fn_nm,
			"Missing char separator %s", P_qfmt_char(char_lit));
      }
    }
    (pd->nerr)++;
    P_PS_setPanic(pd);
  }
} while (0)
/* END_MACRO */

#define PCGEN_ALT_READ_CHAR_LIT(fn_nm, char_lit)
do {
  PDCI_ALT_READ_FIELD_PRE(fn_nm)
  PDCI_ALT_READ_CHAR_LIT(fn_nm, char_lit);
  PDCI_ALT_READ_FIELD_POST(fn_nm)
} while (0)
/* END_MACRO */

#define PDCI_ALT_READ_STR_LIT(fn_nm, pads_str_lit)
do {
  Pbase_pd tpd;
  size_t toffset;
  PDCI_IO_BEGINLOC(pads, tpd.loc);
  if (P_OK == Pstr_lit_scan1(pads, pads_str_lit, 1, 0, &toffset)) {
    if (toffset) {
      if (pd->nerr == 0) {
	pd->errCode = P_STRUCT_EXTRA_BEFORE_SEP;
	PDCI_IO_ENDLOC_MINUS2(pads, tpd.loc);
	pd->loc = tpd.loc;
	if (!P_spec_level(pads)) {
	  PDCI_report_err(pads, P_LEV_WARN, &(tpd.loc), P_STRUCT_EXTRA_BEFORE_SEP, fn_nm,
			  "Extra data before string separator %s", P_qfmt_str(pads_str_lit));
	}
      }
      (pd->nerr)++;
    }
  } else {
    if (pd->nerr == 0) {
      pd->errCode = P_MISSING_LITERAL;
      PDCI_IO_ENDLOC_MINUS1(pads, tpd.loc);
      pd->loc = tpd.loc;
      if (!P_spec_level(pads)) {
	PDCI_report_err(pads, P_LEV_WARN, &(tpd.loc), P_MISSING_LITERAL, fn_nm,
			"Missing string separator %s", P_qfmt_str(pads_str_lit));
      }
    }
    (pd->nerr)++;
    P_PS_setPanic(pd);
  }
} while (0)
/* END_MACRO */

#define PCGEN_ALT_READ_STR_LIT(fn_nm, str_lit, str_len_expr)
do {
  P_STRING_DECL_CSTR_LEN(tstr, str_lit, str_len_expr);
  PDCI_ALT_READ_FIELD_PRE(fn_nm)
  PDCI_ALT_READ_STR_LIT(fn_nm, &tstr);
  PDCI_ALT_READ_FIELD_POST(fn_nm)
} while (0)
/* END_MACRO */

#define PDCI_ALT_READ_REGEXP(fn_nm, regexp, regexp_str)
do {
  Perror_t terr;
  Pbase_pd tpd;
  size_t   toffset;
  PDCI_IO_BEGINLOC(pads, tpd.loc);
  terr = Pre_scan1(pads, regexp, 1, 0, &toffset);
  Pregexp_cleanup(pads, regexp);
  if (terr == P_OK) {
    if (toffset) {
      if (pd->nerr == 0) {
	pd->errCode = P_STRUCT_EXTRA_BEFORE_SEP;
	PDCI_IO_ENDLOC_MINUS2(pads, tpd.loc);
	pd->loc = tpd.loc;
	if (!P_spec_level(pads)) {
	  PDCI_report_err(pads, P_LEV_WARN, &(tpd.loc), P_STRUCT_EXTRA_BEFORE_SEP, fn_nm,
			  "Extra data before regexp separator Pre %s", P_qfmt_cstr(regexp_str));
	}
      }
      (pd->nerr)++;
    }
  } else {
    if (pd->nerr == 0) {
      pd->errCode = P_MISSING_LITERAL;
      PDCI_IO_ENDLOC_MINUS1(pads, tpd.loc);
      pd->loc = tpd.loc;
      if (P_spec_level(pads)) {
	PDCI_report_err(pads, P_LEV_WARN, &(tpd.loc), P_MISSING_LITERAL, fn_nm,
			"Missing regexp separator Pre %s", P_qfmt_cstr(regexp_str));
      }
    }
    (pd->nerr)++;
    P_PS_setPanic(pd);
  }
} while (0)
/* END_MACRO */

#define PCGEN_ALT_READ_REGEXP(fn_nm, regexp_str)
do {
  P_REGEXP_DECL_NULL(tregexp);
  PDCI_regexp_compile_cstr(pads, regexp_str, &tregexp, "Regexp-specified separator", fn_nm);
  if (!tregexp.valid) {
    pd->errCode = P_INVALID_REGEXP;
    (pd->nerr)++;
    P_PS_setPanic(pd);
  } else {
    PDCI_ALT_READ_FIELD_PRE(fn_nm)
    PDCI_ALT_READ_REGEXP(fn_nm, &tregexp, regexp_str);
    PDCI_ALT_READ_FIELD_POST(fn_nm)
  }
} while (0)
/* END_MACRO */

#define PCGEN_UNION_READ_SETUP_STAT(fn_nm, the_tag, rep_cleanup, rep_init, pd_cleanup, pd_init)
Ppos_t start_pos;
do {
  PDCI_IO_GETPOS(pads, start_pos);
  pd->errCode = P_NO_ERR;
} while (0)
/* END_MACRO */

#define PCGEN_UNION_READ_SETUP(fn_nm, the_tag, rep_cleanup, rep_init, pd_cleanup, pd_init)
Ppos_t start_pos;
do {
  PDCI_IO_GETPOS(pads, start_pos);
  if (rep->tag != the_tag) {
    rep_cleanup (pads, rep);
    pd_cleanup  (pads, pd);
    rep_init    (pads, rep);
    pd_init     (pads, pd);
  }
  pd->errCode = P_NO_ERR;
} while (0)
/* END_MACRO */

/* falls through on error, goes to branches_done on success */
#define PCGEN_UNION_READ_STAT(fn_nm, the_tag, rep_cleanup, rep_init, pd_cleanup, pd_init, read_call)
do {
  if (P_ERR == P_io_checkpoint(pads, 1)) {
    PDCI_report_err(pads, P_LEV_FATAL, 0, P_CHKPOINT_ERR, fn_nm, 0);
  }
  rep->tag = the_tag;
  pd->tag  = the_tag;
  if (P_OK == read_call) {
    if (P_ERR == P_io_commit(pads)) {
      PDCI_report_err(pads, P_LEV_FATAL, 0, P_COMMIT_ERR, fn_nm, 0);
    }
    pd->loc.b = start_pos;
    goto branches_done;
  }
  if (P_ERR == P_io_restore(pads)) {
    PDCI_report_err(pads, P_LEV_FATAL, 0, P_RESTORE_ERR, fn_nm, 0);
  }
} while (0)
/* END_MACRO */

/* falls through on error, goes to branches_done on success */
#define PCGEN_UNION_READ_STAT_CHECK(fn_nm, the_tag, rep_cleanup, rep_init, pd_cleanup, pd_init, read_call, usercheck)
do {
  if (P_ERR == P_io_checkpoint(pads, 1)) {
    PDCI_report_err(pads, P_LEV_FATAL, 0, P_CHKPOINT_ERR, fn_nm, 0);
  }
  rep->tag = the_tag;
  pd->tag  = the_tag;
  if ((P_OK == read_call) && (!P_Test_SemCheck(m->unionLevel) || (usercheck))) {
    if (P_ERR == P_io_commit(pads)) {
      PDCI_report_err(pads, P_LEV_FATAL, 0, P_COMMIT_ERR, fn_nm, 0);
    }
    pd->loc.b = start_pos;
    goto branches_done;
  }
  if (P_ERR == P_io_restore(pads)) {
    PDCI_report_err(pads, P_LEV_FATAL, 0, P_RESTORE_ERR, fn_nm, 0);
  }
} while (0)
/* END_MACRO */

/* goes to branches_done on success, falls through on failure */
#define PCGEN_UNION_READ_FIRST(fn_nm, the_tag, rep_cleanup, rep_init, pd_cleanup, pd_init, read_call)
do {
  if (P_ERR == P_io_checkpoint(pads, 1)) {
    PDCI_report_err(pads, P_LEV_FATAL, 0, P_CHKPOINT_ERR, fn_nm, 0);
  }
  rep->tag = the_tag;
  pd->tag  = the_tag;
  if (P_OK == read_call) {
    if (P_ERR == P_io_commit(pads)) {
      PDCI_report_err(pads, P_LEV_FATAL, 0, P_COMMIT_ERR, fn_nm, 0);
    }
    pd->loc.b = start_pos;
    goto branches_done;
  }
  if (P_ERR == P_io_restore(pads)) {
    PDCI_report_err(pads, P_LEV_FATAL, 0, P_RESTORE_ERR, fn_nm, 0);
  }
  rep_cleanup (pads, rep);
  pd_cleanup  (pads, pd);
} while (0)
/* END_MACRO */

/* goes to branches_done on success, falls through on failure */
#define PCGEN_UNION_READ_FIRST_CHECK(fn_nm, the_tag, rep_cleanup, rep_init, pd_cleanup, pd_init, read_call, usercheck)
do {
  if (P_ERR == P_io_checkpoint(pads, 1)) {
    PDCI_report_err(pads, P_LEV_FATAL, 0, P_CHKPOINT_ERR, fn_nm, 0);
  }
  rep->tag = the_tag;
  pd->tag  = the_tag;
  if ((P_OK == read_call) && (!P_Test_SemCheck(m->unionLevel) || (usercheck))) {
    if (P_ERR == P_io_commit(pads)) {
      PDCI_report_err(pads, P_LEV_FATAL, 0, P_COMMIT_ERR, fn_nm, 0);
    }
    pd->loc.b = start_pos;
    goto branches_done;
  }
  if (P_ERR == P_io_restore(pads)) {
    PDCI_report_err(pads, P_LEV_FATAL, 0, P_RESTORE_ERR, fn_nm, 0);
  }
  rep_cleanup (pads, rep);
  pd_cleanup  (pads, pd);
} while (0)
/* END_MACRO */

/* falls through on error, goes to branches_done on success */
#define PCGEN_UNION_READ_NEXT(fn_nm, the_tag, rep_cleanup, rep_init, pd_cleanup, pd_init, read_call)
do {
  rep_init (pads, rep);
  pd_init  (pads, pd);
  pd->errCode = P_NO_ERR;
  if (P_ERR == P_io_checkpoint(pads, 1)) {
    PDCI_report_err(pads, P_LEV_FATAL, 0, P_CHKPOINT_ERR, fn_nm, 0);
  }
  rep->tag = the_tag;
  pd->tag  = the_tag;
  if (P_OK == read_call) {
    if (P_ERR == P_io_commit(pads)) {
      PDCI_report_err(pads, P_LEV_FATAL, 0, P_COMMIT_ERR, fn_nm, 0);
    }
    pd->loc.b = start_pos;
    goto branches_done;
  }
  if (P_ERR == P_io_restore(pads)) {
    PDCI_report_err(pads, P_LEV_FATAL, 0, P_RESTORE_ERR, fn_nm, 0);
  }
  rep_cleanup (pads, rep);
  pd_cleanup  (pads, pd);
} while (0)
/* END_MACRO */

/* falls through on error, goes to branches_done on success */
#define PCGEN_UNION_READ_NEXT_CHECK(fn_nm, the_tag, rep_cleanup, rep_init, pd_cleanup, pd_init, read_call, usercheck)
do {
  rep_init (pads, rep);
  pd_init  (pads, pd);
  pd->errCode = P_NO_ERR;
  if (P_ERR == P_io_checkpoint(pads, 1)) {
    PDCI_report_err(pads, P_LEV_FATAL, 0, P_CHKPOINT_ERR, fn_nm, 0);
  }
  rep->tag = the_tag;
  pd->tag  = the_tag;
  if ((P_OK == read_call) && (!P_Test_SemCheck(m->unionLevel) || (usercheck))) {
    if (P_ERR == P_io_commit(pads)) {
      PDCI_report_err(pads, P_LEV_FATAL, 0, P_COMMIT_ERR, fn_nm, 0);
    }
    pd->loc.b = start_pos;
    goto branches_done;
  }
  if (P_ERR == P_io_restore(pads)) {
    PDCI_report_err(pads, P_LEV_FATAL, 0, P_RESTORE_ERR, fn_nm, 0);
  }
  rep_cleanup (pads, rep);
  pd_cleanup  (pads, pd);
} while (0)
/* END_MACRO */

/* falls through on error, goes to branches_done on success */
#define PCGEN_UNION_READ_LAST(fn_nm, the_tag, rep_cleanup, rep_init, pd_cleanup, pd_init, read_call)
do {
  rep_init (pads, rep);
  pd_init  (pads, pd);
  pd->errCode = P_NO_ERR;
  if (P_ERR == P_io_checkpoint(pads, 1)) {
    PDCI_report_err(pads, P_LEV_FATAL, 0, P_CHKPOINT_ERR, fn_nm, 0);
  }
  rep->tag = the_tag;
  pd->tag  = the_tag;
  if (P_OK == read_call) {
    if (P_ERR == P_io_commit(pads)) {
      PDCI_report_err(pads, P_LEV_FATAL, 0, P_COMMIT_ERR, fn_nm, 0);
    }
    pd->loc.b = start_pos;
    goto branches_done;
  }
  if (P_ERR == P_io_restore(pads)) {
    PDCI_report_err(pads, P_LEV_FATAL, 0, P_RESTORE_ERR, fn_nm, 0);
  }
} while (0)
/* END_MACRO */

/* falls through on error, goes to branches_done on success */
#define PCGEN_UNION_READ_LAST_CHECK(fn_nm, the_tag, rep_cleanup, rep_init, pd_cleanup, pd_init, read_call, usercheck)
do {
  rep_init (pads, rep);
  pd_init  (pads, pd);
  pd->errCode = P_NO_ERR;
  if (P_ERR == P_io_checkpoint(pads, 1)) {
    PDCI_report_err(pads, P_LEV_FATAL, 0, P_CHKPOINT_ERR, fn_nm, 0);
  }
  rep->tag = the_tag;
  pd->tag  = the_tag;
  if ((P_OK == read_call)&& (!P_Test_SemCheck(m->unionLevel) || (usercheck))) {
    if (P_ERR == P_io_commit(pads)) {
      PDCI_report_err(pads, P_LEV_FATAL, 0, P_COMMIT_ERR, fn_nm, 0);
    }
    pd->loc.b = start_pos;
    goto branches_done;
  }
  if (P_ERR == P_io_restore(pads)) {
    PDCI_report_err(pads, P_LEV_FATAL, 0, P_RESTORE_ERR, fn_nm, 0);
  }
} while (0)
/* END_MACRO */

#define PCGEN_UNION_READ_MAN_STAT_PRE(fn_nm, the_tag, rep_init, pd_init) 
do {
  rep->tag = the_tag;
  pd->tag = the_tag;
  PD_COMMON_INIT_NO_ERR(&(pd->val.the_tag));
  PDCI_IO_BEGINLOC(pads, pd->val.the_tag.loc);
} while (0)
/* END_MACRO */

#define PCGEN_UNION_READ_MAN_STAT_VIRT_PRE(fn_nm, the_tag, rep_init, pd_init) 
  PCGEN_UNION_READ_MAN_STAT_PRE(fn_nm, the_tag, rep_init, pd_init)
/* END_MACRO */

#define PCGEN_UNION_READ_MAN_FIRST_PRE(fn_nm, the_tag, rep_init, pd_init) 
  PCGEN_UNION_READ_MAN_STAT_PRE(fn_nm, the_tag, rep_init, pd_init)
/* END_MACRO */

#define PCGEN_UNION_READ_MAN_FIRST_VIRT_PRE(fn_nm, the_tag, rep_init, pd_init) 
  PCGEN_UNION_READ_MAN_STAT_VIRT_PRE(fn_nm, the_tag, rep_init, pd_init)
/* END_MACRO */

#define PCGEN_UNION_READ_MAN_NEXT_PRE(fn_nm, the_tag, rep_init, pd_init) 
do {
  rep_init(pads, rep);
  pd_init (pads, pd);
  pd->errCode = P_NO_ERR;
  PCGEN_UNION_READ_MAN_STAT_PRE(fn_nm, the_tag, rep_init, pd_init);
} while (0)
/* END_MACRO */

/* not sure that the init calls are needed */
#define PCGEN_UNION_READ_MAN_NEXT_VIRT_PRE(fn_nm, the_tag, rep_init, pd_init) 
do {
  rep_init(pads, rep);
  pd_init (pads, pd);
  pd->errCode = P_NO_ERR;
  PCGEN_UNION_READ_MAN_STAT_VIRT_PRE(fn_nm, the_tag, rep_init, pd_init);
} while (0)
/* END_MACRO */

#define PCGEN_UNION_READ_MAN_STAT_POST(fn_nm)
do {
  pd->loc.b = start_pos;
  goto branches_done;
} while (0)
/* END_MACRO */

#define PCGEN_UNION_READ_MAN_POST(fn_nm)
do {
  pd->loc.b = start_pos;
  goto branches_done;
} while (0)
/* END_MACRO */

#define PCGEN_UNION_READ_MAN_STAT_POST_CHECK(fn_nm, rep_cleanup, pd_cleanup, usercheck)
do {
  if (!P_Test_SemCheck(m->unionLevel) || (usercheck)) {
    pd->loc.b = start_pos;
    goto branches_done;
  }
} while (0)
/* END_MACRO */

#define PCGEN_UNION_READ_MAN_POST_CHECK(fn_nm, rep_cleanup, pd_cleanup, usercheck)
do {
  if (!P_Test_SemCheck(m->unionLevel) || (usercheck)) {
    pd->loc.b = start_pos;
    goto branches_done;
  }
  rep_cleanup(pads, rep);
  pd_cleanup(pads, pd);
} while (0)
/* END_MACRO */

#define PCGEN_UNION_READ_WHERE_CHECK(fn_nm, usercheck)
do {
    if (P_Test_SemCheck(m->unionLevel) && (!(usercheck))) {
      pd->loc.b = start_pos;
      PDCI_CONSTRAINT_ERR(fn_nm, P_USER_CONSTRAINT_VIOLATION, "Pwhere clause violation");
    }
} while (0)
/* END_MACRO */

#define PDCI_UNION_READ_WHERE_END_CHECK(fn_nm, usercheck)
do {
    PDCI_IO_ENDLOC(pads, pd->loc);
    PCGEN_UNION_READ_WHERE_CHECK(fn_nm, usercheck);
} while (0)
/* END_MACRO */

/* XXX why was K codegen using -2 below ??? XXX */
#define PCGEN_UNION_READ_FAILED(fn_nm, nm, err_tag)
do {
  (pd->nerr)++;
  pd->errCode = P_UNION_MATCH_ERR;
  pd->loc.b = start_pos;
  PDCI_IO_ENDLOC_MINUS1(pads, pd->loc);
  PDCI_report_err(pads, P_LEV_WARN, &(pd->loc), pd->errCode, fn_nm, "Failed to match any branch of union " nm);
  rep->tag = err_tag;
  pd->tag = err_tag;
  P_PS_setPanic(pd);
  goto final_check;
} while (0)
/* END_MACRO */

#define PDCI_SWUNION_READ_PRE(fn_nm, the_tag, rep_cleanup, rep_init, pd_cleanup, pd_init)
do {
  if ((rep->tag) != the_tag) {
    rep_cleanup (pads, rep);
    pd_cleanup  (pads, pd);
    rep_init    (pads, rep);
    pd_init     (pads, pd);
  }
} while (0)
/* END_MACRO */

/* falls through on success, goto branches_done on failure */
#define PCGEN_SWUNION_READ_STAT(fn_nm, the_tag, err_tag, rep_cleanup, rep_init, pd_cleanup, pd_init, read_call)
do {
  PDCI_IO_BEGINLOC(pads, pd->loc);
  rep->tag = the_tag;
  pd->tag  = the_tag;
  if (P_ERR == read_call) {
    pd->nerr = 1;
    pd->errCode = P_UNION_MATCH_ERR;
    pd->loc = pd->val.the_tag.loc;
    PDCI_report_err(pads, P_LEV_WARN, &(pd->loc), pd->errCode, fn_nm, "Failed to match branch " PDCI_MacroArg2String(the_tag));
    goto branches_done;
  }
  pd->errCode = P_NO_ERR;
} while (0)
/* END_MACRO */

/* falls through on success, goto branches_done on failure */
#define PCGEN_SWUNION_READ(fn_nm, the_tag, err_tag, rep_cleanup, rep_init, pd_cleanup, pd_init, read_call)
do {
  PDCI_SWUNION_READ_PRE(fn_nm, the_tag, rep_cleanup, rep_init, pd_cleanup, pd_init);
  PCGEN_SWUNION_READ_STAT(fn_nm, the_tag, err_tag, rep_cleanup, rep_init, pd_cleanup, pd_init, read_call);
} while (0)
/* END_MACRO */

#define PCGEN_SWUNION_READ_MAN_STAT_PRE(fn_nm, the_tag, rep_cleanup, rep_init, pd_cleanup, pd_init)
do {
  PDCI_IO_BEGINLOC(pads, pd->loc);
  pd->errCode = P_NO_ERR;
  rep->tag = the_tag;
  pd->tag = the_tag;
  PD_COMMON_INIT_NO_ERR(&(pd->val.the_tag));
  PDCI_IO_BEGINLOC(pads, pd->val.the_tag.loc);
} while (0)
/* END_MACRO */

#define PCGEN_SWUNION_READ_MAN_STAT_VIRT_PRE(fn_nm, the_tag, rep_cleanup, rep_init, pd_cleanup, pd_init)
  PCGEN_SWUNION_READ_MAN_STAT_PRE(fn_nm, the_tag, rep_cleanup, rep_init, pd_cleanup, pd_init)
/* END_MACRO */

#define PCGEN_SWUNION_READ_MAN_PRE(fn_nm, the_tag, rep_cleanup, rep_init, pd_cleanup, pd_init)
do {
  PDCI_SWUNION_READ_PRE(fn_nm, the_tag, rep_cleanup, rep_init, pd_cleanup, pd_init);
  PCGEN_SWUNION_READ_MAN_STAT_PRE(fn_nm, the_tag, rep_cleanup, rep_init, pd_cleanup, pd_init);
} while (0)
/* END_MACRO */

#define PCGEN_SWUNION_READ_MAN_VIRT_PRE(fn_nm, the_tag, rep_cleanup, rep_init, pd_cleanup, pd_init)
do {
  PDCI_SWUNION_READ_PRE(fn_nm, the_tag, rep_cleanup, rep_init, pd_cleanup, pd_init);
  PCGEN_SWUNION_READ_MAN_STAT_VIRT_PRE(fn_nm, the_tag, rep_cleanup, rep_init, pd_cleanup, pd_init);
} while (0)
/* END_MACRO */

/* falls through on success, goto branches_done on failure */
#define PCGEN_SWUNION_READ_POST_CHECK(fn_nm, the_tag, err_tag, usercheck)
do {
  if (P_Test_SemCheck(m->unionLevel) && (!(usercheck))) {
    PDCI_ELT_CONSTRAINT_ERR(fn_nm, pd->val.the_tag, P_USER_CONSTRAINT_VIOLATION, P_UNION_MATCH_ERR, "User constraint on branch " PDCI_MacroArg2String(the_tag) " violated");
    goto branches_done;
  }
} while (0)
/* END_MACRO */

/* always does goto branches_done */
#define PCGEN_SWUNION_READ_FAILED(fn_nm, nm, err_tag)
do {
  (pd->nerr)++;
  pd->errCode = P_UNION_MATCH_ERR;
  PDCI_IO_GETLOC_SPAN0(pads, pd->loc);
  PDCI_report_err(pads, P_LEV_WARN, &(pd->loc), pd->errCode, fn_nm, "Switch value failed to match any branch of switched union " nm);
  rep->tag = err_tag;
  pd->tag = err_tag;
  goto branches_done;
} while (0)
/* END_MACRO */

#define PCGEN_SWUNION_READ_WHERE_CHECK(fn_nm, usercheck)
do {
    if (pd->nerr == 0 && P_Test_SemCheck(m->unionLevel) && (!(usercheck))) {
      PDCI_CONSTRAINT_ERR(fn_nm, P_USER_CONSTRAINT_VIOLATION, "Pwhere clause violation");
    }
} while (0)
/* END_MACRO */

#define PCGEN_SWUNION_READ_WHERE_END_CHECK(fn_nm, usercheck)
do {
    PDCI_IO_ENDLOC(pads, pd->loc);
    PCGEN_SWUNION_READ_WHERE_CHECK(fn_nm, usercheck);
} while (0)
/* END_MACRO */


#define PCGEN_STRUCT_ACC_REP_NOVALS()
do {
  if (dtsize(acc->nerr.dict) == 0) {
    sfprintf(outstr, "(No %s values.)", what);
    sfstrclose(tmpstr);
    return P_OK;
  }
} while (0)
/* END_MACRO */

#define PCGEN_UNION_ACC_REP_NOVALS()
do {
  if (dtsize(acc->tag.dict) == 0) {
    sfprintf(outstr, "(No %s values.)", what);
    sfstrclose(tmpstr);
    return P_OK;
  }
} while (0)
/* END_MACRO */

#define PCGEN_ARRAY_ACC_REP_NOVALS()
do {
  int dtsz = dtsize(acc->length.dict);
  if (dtsz == 0) {
    sfprintf(outstr, "(No %s values.)", what);
    sfstrclose(tmpstr);
    return P_OK;
  }
  if (dtsz == 1) {
    Puint32_dt_key_t   lookup_key;
    lookup_key.val = 0;
    lookup_key.cnt = 0;
    if (dtmatch(acc->length.dict, &lookup_key)) {
      sfprintf(outstr, "(No element values accumulated; all arrays had zero length.)");
      sfstrclose(tmpstr);
      return P_OK;
    }
  }
} while (0)
/* END_MACRO */

#define PCGEN_ENUM_ACC_REP2IO(default_what, int_acc_call)
do {
  if (!what) {
    what = default_what;
  }
  return int_acc_call;
} while (0)
/* END_MACRO */

#define PCGEN_TYPEDEF_ACC_REP2IO(default_what, basety_nm, base_acc_call)
do {
  Perror_t res;
  Sfio_t *tmpstr;
  if (!(tmpstr = sfstropen())) {
    return P_ERR;
  }
  if (!what) {
    sfprintf (tmpstr, "%s [--> %s]", default_what, basety_nm);
    what = sfstruse(tmpstr);
  } else {
    sfprintf (tmpstr, "%s [%s --> %s]", what, default_what, basety_nm);
    what = sfstruse(tmpstr);
  }
  res = base_acc_call;
  sfstrclose(tmpstr);
  return res;
} while (0)
/* END_MACRO */

/* function body for a write2io function that has params pads, io, pd, rep */
/* always precede with decls for buf, buf_len, and buf_full and always follow with 'return -1' */
#define PCGEN_WRITE2IO_USE_WRITE2BUF(fn_nm, write2buf_call)
  int set_buf;
  ssize_t length;
  PDCI_IODISC_2P_CHECKS_RET_SSIZE(fn_nm, io, rep);
  buf_len = pads->outbuf_res;
  while (1) {
    set_buf  = 0;
    buf_full = 0;
    buf = PDCI_io_write_start(pads, io, &buf_len, &set_buf, fn_nm);
    if (!buf)  {
      /* Don't have to abort because start failed. */
      return -1;
    }
    length = write2buf_call;
    if (buf_full)  {
      /* Try again with a bigger buffer */
      PDCI_io_write_abort(pads, io, buf, set_buf, fn_nm);
      buf_len*=2;
      continue;
    }
    break;
  }
  if (length>=0) {
    return PDCI_io_write_commit(pads, io, buf, set_buf, length, fn_nm);
  }
  PDCI_io_write_abort(pads, io, buf, set_buf, fn_nm)
/* END_MACRO */

#define PCGEN_TLEN_UPDATES()
do {
  if (tlen<0) {
    return -1;
  }
  length     += tlen;
  buf_cursor += tlen;
  buf_len    -= tlen;
} while (0)
/* END_MACRO */

#define PCGEN_FINAL_TLEN_UPDATES()
do {
  if (tlen<0) {
    return -1;
  }
  length += tlen;
} while (0)
/* END_MACRO */

#define PDCI_TMP4_TLEN_UPDATES()
do {
  if (tlen <= 0) {
    return -1;
  }
  if (tlen > buf_len) {
    (*buf_full) = 1;
    return -1;
  }
  memcpy(buf_cursor, sfstruse(pads->tmp4), tlen);
  length     += tlen;
  buf_cursor += tlen;
  buf_len    -= tlen;
} while (0)
/* END_MACRO */

#define PDCI_FINAL_TMP4_TLEN_UPDATES()
do {
  if (tlen <= 0) {
    return -1;
  }
  if (tlen > buf_len) {
    (*buf_full) = 1;
    return -1;
  }
  memcpy(buf_cursor, sfstruse(pads->tmp4), tlen);
  length += tlen;
} while (0)
/* END_MACRO */

#define PCGEN_TAG_OPEN_XML_OUT(def_tag)
do {
  indent = (indent > 128) ? 128 : indent;
  if (!tag) { tag = def_tag; }
  sfstrset(pads->tmp4, 0);
  tlen = sfprintf(pads->tmp4, "%.*s<%s>\n", indent, PDCI_spaces, tag);
  PDCI_TMP4_TLEN_UPDATES();
} while (0)
/* END_MACRO */

#define PCGEN_TAG_CLOSE_XML_OUT()
do {
  sfstrset(pads->tmp4, 0);
  tlen = sfprintf(pads->tmp4, "%.*s</%s>\n", indent, PDCI_spaces, tag);
  PDCI_FINAL_TMP4_TLEN_UPDATES();
} while (0)
/* END_MACRO */

#define PDCI_UNION_TAG_XML_OUT(tag)
do {
  int tag_indent = (indent > 126) ? 128 : indent+2;
  sfstrset(pads->tmp4, 0);
  tlen = sfprintf(pads->tmp4, "%.*s<tag>%s</>\n", tag_indent, PDCI_spaces, tag);
  PDCI_TMP4_TLEN_UPDATES();
} while (0)
/* END_MACRO */

#define PCGEN_ENUM_XML_OUT(def_tag, rep2str_fn)
do {
  if (!tag) { tag = def_tag; }
  indent = ((indent) > 128) ? 128 : indent;
  sfstrset(pads->tmp4, 0);
  if ((pd)->errCode == P_NO_ERR) { /* no error */
    tlen = sfprintf(pads->tmp4, "%.*s<%s><val>%s</></>\n", indent, PDCI_spaces, tag, rep2str_fn(*rep));
  } else if ((pd)->errCode < 100) { /* error, no location */
    tlen = sfprintf(pads->tmp4, "%.*s<%s><pd><pstate>%s</pstate><nerr>%lu</nerr><errCode>%s</errCode></pd></%s>\n",
		    indent, PDCI_spaces, tag, P_pstate2str((pd)->pstate), (pd)->nerr, P_errCode2str((pd)->errCode), tag);
  } else { /* error, location */
    tlen = sfprintf(pads->tmp4, "%.*s<%s><pd><pstate>%s</pstate><nerr>%lu</nerr><errCode>%s</errCode><loc><b><num>%lld</><byte>%lld</><offset>%lld</></b><e><num>%lld</><byte>%lld</><offset>%lld</></e></loc></pd></%s>\n",
		    indent, PDCI_spaces, tag, P_pstate2str((pd)->pstate), (pd)->nerr, P_errCode2str((pd)->errCode),
		    (long long)(pd)->loc.b.num, (long long)(pd)->loc.b.byte, (long long)(pd)->loc.b.offset,
		    (long long)(pd)->loc.e.num, (long long)(pd)->loc.e.byte, (long long)(pd)->loc.e.offset,
		    tag);
  }
  PDCI_FINAL_TMP4_TLEN_UPDATES();
} while (0)
/* END_MACRO */

#define PCGEN_STRUCT_PD_XML_OUT()
do {
  if ((pd)->errCode != P_NO_ERR) {
    int pd_indent = ((indent) > 126) ? 128 : (indent)+2;
    sfstrset(pads->tmp4, 0);
    if ((pd)->errCode < 100) { /* no location */
      tlen = sfprintf(pads->tmp4, "%.*s<pd><pstate>%s</pstate><nerr>%lu</nerr><errCode>%s</errCode></pd>\n",
		      pd_indent, PDCI_spaces, P_pstate2str((pd)->pstate), (pd)->nerr, P_errCode2str((pd)->errCode));
    } else { /* location */
      tlen = sfprintf(pads->tmp4, "%.*s<pd><pstate>%s</pstate><nerr>%lu</nerr><errCode>%s</errCode><loc><b><num>%lld</><byte>%lld</><offset>%lld</></b><e><num>%lld</><byte>%lld</><offset>%lld</></e></loc></pd>\n",
		      pd_indent, PDCI_spaces, P_pstate2str((pd)->pstate), (pd)->nerr, P_errCode2str((pd)->errCode),
		      (long long)(pd)->loc.b.num, (long long)(pd)->loc.b.byte, (long long)(pd)->loc.b.offset,
		      (long long)(pd)->loc.e.num, (long long)(pd)->loc.e.byte, (long long)(pd)->loc.e.offset);
    }
    PDCI_TMP4_TLEN_UPDATES();
  }
} while (0)
/* END_MACRO */

#define PCGEN_UNION_PD_XML_OUT() PCGEN_STRUCT_PD_XML_OUT()
/* END_MACRO */

#define PCGEN_ARRAY_PD_XML_OUT()
do {
  if ((pd)->errCode != P_NO_ERR) {
    int pd_indent = ((indent) > 126) ? 128 : (indent)+2;
    sfstrset(pads->tmp4, 0);
    if ((pd)->errCode < 100) { /* no location */
      tlen = sfprintf(pads->tmp4, "%.*s<pd><pstate>%s</pstate><nerr>%lu</nerr><errCode>%s</errCode><neerr>%lu</neerr><firstError>%lu</firstError></pd>\n",
		      pd_indent, PDCI_spaces, P_pstate2str((pd)->pstate), (pd)->nerr, P_errCode2str((pd)->errCode), (pd)->neerr, (pd)->firstError);
    } else { /* location */
      tlen = sfprintf(pads->tmp4, "%.*s<pd><pstate>%s</pstate><nerr>%lu</nerr><errCode>%s</errCode><loc><b><num>%lld</><byte>%lld</><offset>%lld</></b><e><num>%lld</><byte>%lld</><offset>%lld</></e></loc><neerr>%lu</neerr><firstError>%lu</firstError></pd>\n",
		      pd_indent, PDCI_spaces, P_pstate2str((pd)->pstate), (pd)->nerr, P_errCode2str((pd)->errCode),
		      (long long)(pd)->loc.b.num, (long long)(pd)->loc.b.byte, (long long)(pd)->loc.b.offset,
		      (long long)(pd)->loc.e.num, (long long)(pd)->loc.e.byte, (long long)(pd)->loc.e.offset,
		      (pd)->neerr, (pd)->firstError);
    }
    PDCI_TMP4_TLEN_UPDATES();
  }
} while (0)
/* END_MACRO */

/* ********************************* BEGIN_TRAILER ******************************** */
#endif /* FOR_CKIT */
/* ********************************** END_MACROS ********************************** */
