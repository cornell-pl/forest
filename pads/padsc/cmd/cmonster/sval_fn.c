/*
 * switch value functions for cmonster
 *
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#include "cmonster.h"
#include "rw_fn.h"

/* 
 * CM_RW_FN_IMPL(fn_name,         name of function being defined
 *               params,          params of function
 *               targ_decl,       declare in-memory rep type targ
 *               read_call,       read function call
 *               assign_stmt,     either null statement or a (*res_out) assignment
 *               dbg_val_write,   stmt that writes resulting value to cm.errf, uses targ or res_out
 *               write2buf_nm,    name of write2buf fn (string)
 *               write_sz,        size in bytes to write, may be constant or qy->params.elts[0]
 *               write_call       write2buf function call, uses targ or res_out
 * );      
 */

/*
 * Implement all the SVAL functions
 */

CM_RW_FN_IMPL(  CM_SVAL_FN_NM(int32_FW),
		CM_SVAL_FN_PARAMS,
		PDC_int32 targ,
		PDC_int32_FW_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		(*res_out) = (PDC_int32)targ,
		sfprintf(cm->errf, "  ==> switch val = %ld\n", (long)targ),
		"PDC_sbl_int32_write2buf",
		4,
		PDC_sbl_int32_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 4, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_SVAL_FN_NM(a_int32_FW),
		CM_SVAL_FN_PARAMS,
		PDC_int32 targ,
		PDC_a_int32_FW_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		(*res_out) = (PDC_int32)targ,
		sfprintf(cm->errf, "  ==> switch val = %ld\n", (long)targ),
		"PDC_sbl_int32_write2buf",
		4,
		PDC_sbl_int32_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 4, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_SVAL_FN_NM(e_int32_FW),
		CM_SVAL_FN_PARAMS,
		PDC_int32 targ,
		PDC_e_int32_FW_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		(*res_out) = (PDC_int32)targ,
		sfprintf(cm->errf, "  ==> switch val = %ld\n", (long)targ),
		"PDC_sbl_int32_write2buf",
		4,
		PDC_sbl_int32_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 4, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_SVAL_FN_NM(ebc_int32),
		CM_SVAL_FN_PARAMS,
		PDC_int32 targ,
		PDC_ebc_int32_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		(*res_out) = (PDC_int32)targ,
		sfprintf(cm->errf, "  ==> switch val = %ld\n", (long)targ),
		"PDC_sbl_int32_write2buf",
		4,
		PDC_sbl_int32_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 4, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_SVAL_FN_NM(bcd_int32),
		CM_SVAL_FN_PARAMS,
		PDC_int32 targ,
		PDC_bcd_int32_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		(*res_out) = (PDC_int32)targ,
		sfprintf(cm->errf, "  ==> switch val = %ld\n", (long)targ),
		"PDC_sbl_int32_write2buf",
		4,
		PDC_sbl_int32_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 4, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_SVAL_FN_NM(sbl_int32),
		CM_SVAL_FN_PARAMS,
		PDC_int32 targ,
		PDC_sbl_int32_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		(*res_out) = (PDC_int32)targ,
		sfprintf(cm->errf, "  ==> switch val = %ld\n", (long)targ),
		"PDC_sbl_int32_write2buf",
		4,
		PDC_sbl_int32_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 4, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_SVAL_FN_NM(sbh_int32),
		CM_SVAL_FN_PARAMS,
		PDC_int32 targ,
		PDC_sbh_int32_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		(*res_out) = (PDC_int32)targ,
		sfprintf(cm->errf, "  ==> switch val = %ld\n", (long)targ),
		"PDC_sbl_int32_write2buf",
		4,
		PDC_sbl_int32_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 4, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_SVAL_FN_NM(b_int32),
		CM_SVAL_FN_PARAMS,
		PDC_int32 targ,
		PDC_b_int32_read(cm->pdc, &m, &pd, &targ),
		(*res_out) = (PDC_int32)targ,
		sfprintf(cm->errf, "  ==> switch val = %ld\n", (long)targ),
		"PDC_sbl_int32_write2buf",
		4,
		PDC_sbl_int32_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 4, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_SVAL_FN_NM(char),
		CM_SVAL_FN_PARAMS,
		PDC_char targ,
		PDC_char_read(cm->pdc, &m, &pd, &targ),
		(*res_out) = (PDC_int32)targ,
		sfprintf(cm->errf, "  ==> switch val = %ld\n", (long)(*res_out)),
		"PDC_sbl_int32_write2buf",
		4,
		PDC_sbl_int32_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 4, &pd, res_out)
);

CM_RW_FN_IMPL(  CM_SVAL_FN_NM(a_char),
		CM_SVAL_FN_PARAMS,
		PDC_char targ,
		PDC_a_char_read(cm->pdc, &m, &pd, &targ),
		(*res_out) = (PDC_int32)targ,
		sfprintf(cm->errf, "  ==> switch val = %ld\n", (long)(*res_out)),
		"PDC_sbl_int32_write2buf",
		4,
		PDC_sbl_int32_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 4, &pd, res_out)
);

CM_RW_FN_IMPL(  CM_SVAL_FN_NM(e_char),
		CM_SVAL_FN_PARAMS,
		PDC_char targ,
		PDC_e_char_read(cm->pdc, &m, &pd, &targ),
		(*res_out) = (PDC_int32)targ,
		sfprintf(cm->errf, "  ==> switch val = %ld\n", (long)(*res_out)),
		"PDC_sbl_int32_write2buf",
		4,
		PDC_sbl_int32_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 4, &pd, res_out)
);


