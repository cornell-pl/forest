/*
 * readwrite functions for cmonster
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
 *               dbg_val_write,   stmt that writes resulting value to cm->errf, uses targ or res_out
 *               write2buf_nm,    name of write2buf fn as string
 *               write_sz,        size in bytes to write, may be constant or qy->params.elts[0]
 *               write_call       write2buf function call, uses targ or res_out
 * );      
 */

/*
 * Implement all the RW functions
 */

CM_RW_FN_IMPL(  CM_RW_FN_NM(int8_FW),
		CM_RW_FN_PARAMS,
		PDC_int8 targ,
		PDC_int8_FW_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %d\n", (int)targ),
		"PDC_sbl_int8_write2buf",
		1,
		PDC_sbl_int8_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 1, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(int16_FW),
		CM_RW_FN_PARAMS,
		PDC_int16 targ,
		PDC_int16_FW_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %d\n", (int)targ),
		"PDC_sbl_int16_write2buf",
		2,
		PDC_sbl_int16_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 2, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(int32_FW),
		CM_RW_FN_PARAMS,
		PDC_int32 targ,
		PDC_int32_FW_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %ld\n", (long)targ),
		"PDC_sbl_int32_write2buf",
		4,
		PDC_sbl_int32_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 4, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(int64_FW),
		CM_RW_FN_PARAMS,
		PDC_int64 targ,
		PDC_int64_FW_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %lld\n", (long long)targ),
		"PDC_sbl_int64_write2buf",
		8,
		PDC_sbl_int64_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 8, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(uint8_FW),
		CM_RW_FN_PARAMS,
		PDC_uint8 targ,
		PDC_uint8_FW_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %u\n", (unsigned int)targ),
		"PDC_sbl_uint8_write2buf",
		1,
		PDC_sbl_uint8_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 1, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(uint16_FW),
		CM_RW_FN_PARAMS,
		PDC_uint16 targ,
		PDC_uint16_FW_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %u\n", (unsigned int)targ),
		"PDC_sbl_uint16_write2buf",
		2,
		PDC_sbl_uint16_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 2, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(uint32_FW),
		CM_RW_FN_PARAMS,
		PDC_uint32 targ,
		PDC_uint32_FW_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %lu\n", (unsigned long)targ),
		"PDC_sbl_uint32_write2buf",
		4,
		PDC_sbl_uint32_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 4, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(uint64_FW),
		CM_RW_FN_PARAMS,
		PDC_uint64 targ,
		PDC_uint64_FW_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %llu\n", (unsigned long long)targ),
		"PDC_sbl_uint64_write2buf",
		8,
		PDC_sbl_uint64_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 8, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(a_int8_FW),
		CM_RW_FN_PARAMS,
		PDC_int8 targ,
		PDC_a_int8_FW_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %d\n", (int)targ),
		"PDC_sbl_int8_write2buf",
		1,
		PDC_sbl_int8_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 1, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(a_int16_FW),
		CM_RW_FN_PARAMS,
		PDC_int16 targ,
		PDC_a_int16_FW_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %d\n", (int)targ),
		"PDC_sbl_int16_write2buf",
		2,
		PDC_sbl_int16_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 2, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(a_int32_FW),
		CM_RW_FN_PARAMS,
		PDC_int32 targ,
		PDC_a_int32_FW_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %ld\n", (long)targ),
		"PDC_sbl_int32_write2buf",
		4,
		PDC_sbl_int32_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 4, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(a_int64_FW),
		CM_RW_FN_PARAMS,
		PDC_int64 targ,
		PDC_a_int64_FW_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %lld\n", (long long)targ),
		"PDC_sbl_int64_write2buf",
		8,
		PDC_sbl_int64_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 8, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(a_uint8_FW),
		CM_RW_FN_PARAMS,
		PDC_uint8 targ,
		PDC_a_uint8_FW_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %u\n", (unsigned int)targ),
		"PDC_sbl_uint8_write2buf",
		1,
		PDC_sbl_uint8_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 1, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(a_uint16_FW),
		CM_RW_FN_PARAMS,
		PDC_uint16 targ,
		PDC_a_uint16_FW_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %u\n", (unsigned int)targ),
		"PDC_sbl_uint16_write2buf",
		2,
		PDC_sbl_uint16_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 2, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(a_uint32_FW),
		CM_RW_FN_PARAMS,
		PDC_uint32 targ,
		PDC_a_uint32_FW_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %lu\n", (unsigned long)targ),
		"PDC_sbl_uint32_write2buf",
		4,
		PDC_sbl_uint32_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 4, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(a_uint64_FW),
		CM_RW_FN_PARAMS,
		PDC_uint64 targ,
		PDC_a_uint64_FW_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %llu\n", (unsigned long long)targ),
		"PDC_sbl_uint64_write2buf",
		8,
		PDC_sbl_uint64_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 8, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(e_int8_FW),
		CM_RW_FN_PARAMS,
		PDC_int8 targ,
		PDC_e_int8_FW_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %d\n", (int)targ),
		"PDC_sbl_int8_write2buf",
		1,
		PDC_sbl_int8_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 1, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(e_int16_FW),
		CM_RW_FN_PARAMS,
		PDC_int16 targ,
		PDC_e_int16_FW_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %d\n", (int)targ),
		"PDC_sbl_int16_write2buf",
		2,
		PDC_sbl_int16_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 2, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(e_int32_FW),
		CM_RW_FN_PARAMS,
		PDC_int32 targ,
		PDC_e_int32_FW_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %ld\n", (long)targ),
		"PDC_sbl_int32_write2buf",
		4,
		PDC_sbl_int32_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 4, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(e_int64_FW),
		CM_RW_FN_PARAMS,
		PDC_int64 targ,
		PDC_e_int64_FW_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %lld\n", (long long)targ),
		"PDC_sbl_int64_write2buf",
		8,
		PDC_sbl_int64_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 8, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(e_uint8_FW),
		CM_RW_FN_PARAMS,
		PDC_uint8 targ,
		PDC_e_uint8_FW_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %u\n", (unsigned int)targ),
		"PDC_sbl_uint8_write2buf",
		1,
		PDC_sbl_uint8_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 1, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(e_uint16_FW),
		CM_RW_FN_PARAMS,
		PDC_uint16 targ,
		PDC_e_uint16_FW_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %u\n", (unsigned int)targ),
		"PDC_sbl_uint16_write2buf",
		2,
		PDC_sbl_uint16_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 2, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(e_uint32_FW),
		CM_RW_FN_PARAMS,
		PDC_uint32 targ,
		PDC_e_uint32_FW_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %lu\n", (unsigned long)targ),
		"PDC_sbl_uint32_write2buf",
		4,
		PDC_sbl_uint32_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 4, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(e_uint64_FW),
		CM_RW_FN_PARAMS,
		PDC_uint64 targ,
		PDC_e_uint64_FW_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %llu\n", (unsigned long long)targ),
		"PDC_sbl_uint64_write2buf",
		8,
		PDC_sbl_uint64_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 8, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(b_int8),
		CM_RW_FN_PARAMS,
		PDC_int8 targ,
		PDC_b_int8_read(cm->pdc, &m, &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %d\n", (int)targ),
		"PDC_sbl_int8_write2buf",
		1,
		PDC_sbl_int8_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 1, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(b_int16),
		CM_RW_FN_PARAMS,
		PDC_int16 targ,
		PDC_b_int16_read(cm->pdc, &m, &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %d\n", (int)targ),
		"PDC_sbl_int16_write2buf",
		2,
		PDC_sbl_int16_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 2, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(b_int32),
		CM_RW_FN_PARAMS,
		PDC_int32 targ,
		PDC_b_int32_read(cm->pdc, &m, &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %ld\n", (long)targ),
		"PDC_sbl_int32_write2buf",
		4,
		PDC_sbl_int32_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 4, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(b_int64),
		CM_RW_FN_PARAMS,
		PDC_int64 targ,
		PDC_b_int64_read(cm->pdc, &m, &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %lld\n", (long long)targ),
		"PDC_sbl_int64_write2buf",
		8,
		PDC_sbl_int64_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 8, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(b_uint8),
		CM_RW_FN_PARAMS,
		PDC_uint8 targ,
		PDC_b_uint8_read(cm->pdc, &m, &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %u\n", (unsigned int)targ),
		"PDC_sbl_uint8_write2buf",
		1,
		PDC_sbl_uint8_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 1, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(b_uint16),
		CM_RW_FN_PARAMS,
		PDC_uint16 targ,
		PDC_b_uint16_read(cm->pdc, &m, &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %u\n", (unsigned int)targ),
		"PDC_sbl_uint16_write2buf",
		2,
		PDC_sbl_uint16_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 2, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(b_uint32),
		CM_RW_FN_PARAMS,
		PDC_uint32 targ,
		PDC_b_uint32_read(cm->pdc, &m, &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %lu\n", (unsigned long)targ),
		"PDC_sbl_uint32_write2buf",
		4,
		PDC_sbl_uint32_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 4, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(b_uint64),
		CM_RW_FN_PARAMS,
		PDC_uint64 targ,
		PDC_b_uint64_read(cm->pdc, &m, &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %llu\n", (unsigned long long)targ),
		"PDC_sbl_uint64_write2buf",
		8,
		PDC_sbl_uint64_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 8, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(ebc_int8),
		CM_RW_FN_PARAMS,
		PDC_int8 targ,
		PDC_ebc_int8_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %d\n", (int)targ),
		"PDC_sbl_int8_write2buf",
		1,
		PDC_sbl_int8_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 1, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(ebc_int16),
		CM_RW_FN_PARAMS,
		PDC_int16 targ,
		PDC_ebc_int16_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %d\n", (int)targ),
		"PDC_sbl_int16_write2buf",
		2,
		PDC_sbl_int16_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 2, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(ebc_int32),
		CM_RW_FN_PARAMS,
		PDC_int32 targ,
		PDC_ebc_int32_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %ld\n", (long)targ),
		"PDC_sbl_int32_write2buf",
		4,
		PDC_sbl_int32_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 4, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(ebc_int64),
		CM_RW_FN_PARAMS,
		PDC_int64 targ,
		PDC_ebc_int64_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %lld\n", (long long)targ),
		"PDC_sbl_int64_write2buf",
		8,
		PDC_sbl_int64_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 8, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(ebc_uint8),
		CM_RW_FN_PARAMS,
		PDC_uint8 targ,
		PDC_ebc_uint8_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %u\n", (unsigned int)targ),
		"PDC_sbl_uint8_write2buf",
		1,
		PDC_sbl_uint8_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 1, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(ebc_uint16),
		CM_RW_FN_PARAMS,
		PDC_uint16 targ,
		PDC_ebc_uint16_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %u\n", (unsigned int)targ),
		"PDC_sbl_uint16_write2buf",
		2,
		PDC_sbl_uint16_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 2, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(ebc_uint32),
		CM_RW_FN_PARAMS,
		PDC_uint32 targ,
		PDC_ebc_uint32_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %lu\n", (unsigned long)targ),
		"PDC_sbl_uint32_write2buf",
		4,
		PDC_sbl_uint32_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 4, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(ebc_uint64),
		CM_RW_FN_PARAMS,
		PDC_uint64 targ,
		PDC_ebc_uint64_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %llu\n", (unsigned long long)targ),
		"PDC_sbl_uint64_write2buf",
		8,
		PDC_sbl_uint64_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 8, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(bcd_int8),
		CM_RW_FN_PARAMS,
		PDC_int8 targ,
		PDC_bcd_int8_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %d\n", (int)targ),
		"PDC_sbl_int8_write2buf",
		1,
		PDC_sbl_int8_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 1, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(bcd_int16),
		CM_RW_FN_PARAMS,
		PDC_int16 targ,
		PDC_bcd_int16_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %d\n", (int)targ),
		"PDC_sbl_int16_write2buf",
		2,
		PDC_sbl_int16_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 2, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(bcd_int32),
		CM_RW_FN_PARAMS,
		PDC_int32 targ,
		PDC_bcd_int32_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %ld\n", (long)targ),
		"PDC_sbl_int32_write2buf",
		4,
		PDC_sbl_int32_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 4, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(bcd_int64),
		CM_RW_FN_PARAMS,
		PDC_int64 targ,
		PDC_bcd_int64_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %lld\n", (long long)targ),
		"PDC_sbl_int64_write2buf",
		8,
		PDC_sbl_int64_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 8, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(bcd_uint8),
		CM_RW_FN_PARAMS,
		PDC_uint8 targ,
		PDC_bcd_uint8_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %u\n", (unsigned int)targ),
		"PDC_sbl_uint8_write2buf",
		1,
		PDC_sbl_uint8_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 1, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(bcd_uint16),
		CM_RW_FN_PARAMS,
		PDC_uint16 targ,
		PDC_bcd_uint16_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %u\n", (unsigned int)targ),
		"PDC_sbl_uint16_write2buf",
		2,
		PDC_sbl_uint16_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 2, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(bcd_uint32),
		CM_RW_FN_PARAMS,
		PDC_uint32 targ,
		PDC_bcd_uint32_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %lu\n", (unsigned long)targ),
		"PDC_sbl_uint32_write2buf",
		4,
		PDC_sbl_uint32_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 4, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(bcd_uint64),
		CM_RW_FN_PARAMS,
		PDC_uint64 targ,
		PDC_bcd_uint64_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %llu\n", (unsigned long long)targ),
		"PDC_sbl_uint64_write2buf",
		8,
		PDC_sbl_uint64_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 8, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(sbl_int8),
		CM_RW_FN_PARAMS,
		PDC_int8 targ,
		PDC_sbl_int8_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %d\n", (int)targ),
		"PDC_sbl_int8_write2buf",
		1,
		PDC_sbl_int8_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 1, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(sbl_int16),
		CM_RW_FN_PARAMS,
		PDC_int16 targ,
		PDC_sbl_int16_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %d\n", (int)targ),
		"PDC_sbl_int16_write2buf",
		2,
		PDC_sbl_int16_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 2, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(sbl_int32),
		CM_RW_FN_PARAMS,
		PDC_int32 targ,
		PDC_sbl_int32_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %ld\n", (long)targ),
		"PDC_sbl_int32_write2buf",
		4,
		PDC_sbl_int32_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 4, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(sbl_int64),
		CM_RW_FN_PARAMS,
		PDC_int64 targ,
		PDC_sbl_int64_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %lld\n", (long long)targ),
		"PDC_sbl_int64_write2buf",
		8,
		PDC_sbl_int64_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 8, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(sbl_uint8),
		CM_RW_FN_PARAMS,
		PDC_uint8 targ,
		PDC_sbl_uint8_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %u\n", (unsigned int)targ),
		"PDC_sbl_uint8_write2buf",
		1,
		PDC_sbl_uint8_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 1, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(sbl_uint16),
		CM_RW_FN_PARAMS,
		PDC_uint16 targ,
		PDC_sbl_uint16_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %u\n", (unsigned int)targ),
		"PDC_sbl_uint16_write2buf",
		2,
		PDC_sbl_uint16_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 2, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(sbl_uint32),
		CM_RW_FN_PARAMS,
		PDC_uint32 targ,
		PDC_sbl_uint32_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %lu\n", (unsigned long)targ),
		"PDC_sbl_uint32_write2buf",
		4,
		PDC_sbl_uint32_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 4, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(sbl_uint64),
		CM_RW_FN_PARAMS,
		PDC_uint64 targ,
		PDC_sbl_uint64_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %llu\n", (unsigned long long)targ),
		"PDC_sbl_uint64_write2buf",
		8,
		PDC_sbl_uint64_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 8, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(sbh_int8),
		CM_RW_FN_PARAMS,
		PDC_int8 targ,
		PDC_sbh_int8_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %d\n", (int)targ),
		"PDC_sbl_int8_write2buf",
		1,
		PDC_sbl_int8_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 1, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(sbh_int16),
		CM_RW_FN_PARAMS,
		PDC_int16 targ,
		PDC_sbh_int16_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %d\n", (int)targ),
		"PDC_sbl_int16_write2buf",
		2,
		PDC_sbl_int16_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 2, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(sbh_int32),
		CM_RW_FN_PARAMS,
		PDC_int32 targ,
		PDC_sbh_int32_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %ld\n", (long)targ),
		"PDC_sbl_int32_write2buf",
		4,
		PDC_sbl_int32_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 4, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(sbh_int64),
		CM_RW_FN_PARAMS,
		PDC_int64 targ,
		PDC_sbh_int64_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %lld\n", (long long)targ),
		"PDC_sbl_int64_write2buf",
		8,
		PDC_sbl_int64_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 8, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(sbh_uint8),
		CM_RW_FN_PARAMS,
		PDC_uint8 targ,
		PDC_sbh_uint8_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %u\n", (unsigned int)targ),
		"PDC_sbl_uint8_write2buf",
		1,
		PDC_sbl_uint8_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 1, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(sbh_uint16),
		CM_RW_FN_PARAMS,
		PDC_uint16 targ,
		PDC_sbh_uint16_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %u\n", (unsigned int)targ),
		"PDC_sbl_uint16_write2buf",
		2,
		PDC_sbl_uint16_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 2, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(sbh_uint32),
		CM_RW_FN_PARAMS,
		PDC_uint32 targ,
		PDC_sbh_uint32_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %lu\n", (unsigned long)targ),
		"PDC_sbl_uint32_write2buf",
		4,
		PDC_sbl_uint32_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 4, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(sbh_uint64),
		CM_RW_FN_PARAMS,
		PDC_uint64 targ,
		PDC_sbh_uint64_read(cm->pdc, &m, qy->params.elts[0], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %llu\n", (unsigned long long)targ),
		"PDC_sbl_uint64_write2buf",
		8,
		PDC_sbl_uint64_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 8, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(ebc_fpoint8),
		CM_RW_FN_PARAMS,
		PDC_fpoint8 targ,
		PDC_ebc_fpoint8_read(cm->pdc, &m, qy->params.elts[0], qy->params.elts[1], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val: num = %d  denom = %u\n", (int)targ.num, (unsigned int)targ.denom),
		"PDC_sbl_fpoint8_write2buf",
		1,
		PDC_sbl_fpoint8_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 1, qy->params.elts[1], &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(ebc_fpoint16),
		CM_RW_FN_PARAMS,
		PDC_fpoint16 targ,
		PDC_ebc_fpoint16_read(cm->pdc, &m, qy->params.elts[0], qy->params.elts[1], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val: num = %d  denom = %u\n", (int)targ.num, (unsigned int)targ.denom),
		"PDC_sbl_fpoint16_write2buf",
		2,
		PDC_sbl_fpoint16_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 2, qy->params.elts[1], &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(ebc_fpoint32),
		CM_RW_FN_PARAMS,
		PDC_fpoint32 targ,
		PDC_ebc_fpoint32_read(cm->pdc, &m, qy->params.elts[0], qy->params.elts[1], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val: num = %ld  denom = %lu\n", (long)targ.num, (unsigned long)targ.denom),
		"PDC_sbl_fpoint32_write2buf",
		4,
		PDC_sbl_fpoint32_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 4, qy->params.elts[1], &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(ebc_fpoint64),
		CM_RW_FN_PARAMS,
		PDC_fpoint64 targ,
		PDC_ebc_fpoint64_read(cm->pdc, &m, qy->params.elts[0], qy->params.elts[1], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val: num = %lld  denom = %llu\n", (long long)targ.num, (unsigned long long)targ.denom),
		"PDC_sbl_fpoint64_write2buf",
		8,
		PDC_sbl_fpoint64_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 8, qy->params.elts[1], &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(ebc_ufpoint8),
		CM_RW_FN_PARAMS,
		PDC_ufpoint8 targ,
		PDC_ebc_ufpoint8_read(cm->pdc, &m, qy->params.elts[0], qy->params.elts[1], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val: num = %u  denom = %u\n", (unsigned int)targ.num, (unsigned int)targ.denom),
		"PDC_sbl_ufpoint8_write2buf",
		1,
		PDC_sbl_ufpoint8_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 1, qy->params.elts[1], &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(ebc_ufpoint16),
		CM_RW_FN_PARAMS,
		PDC_ufpoint16 targ,
		PDC_ebc_ufpoint16_read(cm->pdc, &m, qy->params.elts[0], qy->params.elts[1], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val: num = %u  denom = %u\n", (unsigned int)targ.num, (unsigned int)targ.denom),
		"PDC_sbl_ufpoint16_write2buf",
		2,
		PDC_sbl_ufpoint16_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 2, qy->params.elts[1], &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(ebc_ufpoint32),
		CM_RW_FN_PARAMS,
		PDC_ufpoint32 targ,
		PDC_ebc_ufpoint32_read(cm->pdc, &m, qy->params.elts[0], qy->params.elts[1], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val: num = %lu  denom = %lu\n", (unsigned long)targ.num, (unsigned long)targ.denom),
		"PDC_sbl_ufpoint32_write2buf",
		4,
		PDC_sbl_ufpoint32_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 4, qy->params.elts[1], &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(ebc_ufpoint64),
		CM_RW_FN_PARAMS,
		PDC_ufpoint64 targ,
		PDC_ebc_ufpoint64_read(cm->pdc, &m, qy->params.elts[0], qy->params.elts[1], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val: num = %llu  denom = %llu\n", (unsigned long long)targ.num, (unsigned long long)targ.denom),
		"PDC_sbl_ufpoint64_write2buf",
		8,
		PDC_sbl_ufpoint64_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 8, qy->params.elts[1], &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(bcd_fpoint8),
		CM_RW_FN_PARAMS,
		PDC_fpoint8 targ,
		PDC_bcd_fpoint8_read(cm->pdc, &m, qy->params.elts[0], qy->params.elts[1], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val: num = %d  denom = %u\n", (int)targ.num, (unsigned int)targ.denom),
		"PDC_sbl_fpoint8_write2buf",
		1,
		PDC_sbl_fpoint8_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 1, qy->params.elts[1], &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(bcd_fpoint16),
		CM_RW_FN_PARAMS,
		PDC_fpoint16 targ,
		PDC_bcd_fpoint16_read(cm->pdc, &m, qy->params.elts[0], qy->params.elts[1], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val: num = %d  denom = %u\n", (int)targ.num, (unsigned int)targ.denom),
		"PDC_sbl_fpoint16_write2buf",
		2,
		PDC_sbl_fpoint16_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 2, qy->params.elts[1], &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(bcd_fpoint32),
		CM_RW_FN_PARAMS,
		PDC_fpoint32 targ,
		PDC_bcd_fpoint32_read(cm->pdc, &m, qy->params.elts[0], qy->params.elts[1], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val: num = %ld  denom = %lu\n", (long)targ.num, (unsigned long)targ.denom),
		"PDC_sbl_fpoint32_write2buf",
		4,
		PDC_sbl_fpoint32_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 4, qy->params.elts[1], &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(bcd_fpoint64),
		CM_RW_FN_PARAMS,
		PDC_fpoint64 targ,
		PDC_bcd_fpoint64_read(cm->pdc, &m, qy->params.elts[0], qy->params.elts[1], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val: num = %lld  denom = %llu\n", (long long)targ.num, (unsigned long long)targ.denom),
		"PDC_sbl_fpoint64_write2buf",
		8,
		PDC_sbl_fpoint64_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 8, qy->params.elts[1], &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(bcd_ufpoint8),
		CM_RW_FN_PARAMS,
		PDC_ufpoint8 targ,
		PDC_bcd_ufpoint8_read(cm->pdc, &m, qy->params.elts[0], qy->params.elts[1], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val: num = %u  denom = %u\n", (unsigned int)targ.num, (unsigned int)targ.denom),
		"PDC_sbl_ufpoint8_write2buf",
		1,
		PDC_sbl_ufpoint8_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 1, qy->params.elts[1], &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(bcd_ufpoint16),
		CM_RW_FN_PARAMS,
		PDC_ufpoint16 targ,
		PDC_bcd_ufpoint16_read(cm->pdc, &m, qy->params.elts[0], qy->params.elts[1], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val: num = %u  denom = %u\n", (unsigned int)targ.num, (unsigned int)targ.denom),
		"PDC_sbl_ufpoint16_write2buf",
		2,
		PDC_sbl_ufpoint16_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 2, qy->params.elts[1], &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(bcd_ufpoint32),
		CM_RW_FN_PARAMS,
		PDC_ufpoint32 targ,
		PDC_bcd_ufpoint32_read(cm->pdc, &m, qy->params.elts[0], qy->params.elts[1], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val: num = %lu  denom = %lu\n", (unsigned long)targ.num, (unsigned long)targ.denom),
		"PDC_sbl_ufpoint32_write2buf",
		4,
		PDC_sbl_ufpoint32_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 4, qy->params.elts[1], &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(bcd_ufpoint64),
		CM_RW_FN_PARAMS,
		PDC_ufpoint64 targ,
		PDC_bcd_ufpoint64_read(cm->pdc, &m, qy->params.elts[0], qy->params.elts[1], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val: num = %llu  denom = %llu\n", (unsigned long long)targ.num, (unsigned long long)targ.denom),
		"PDC_sbl_ufpoint64_write2buf",
		8,
		PDC_sbl_ufpoint64_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 8, qy->params.elts[1], &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(sbl_fpoint8),
		CM_RW_FN_PARAMS,
		PDC_fpoint8 targ,
		PDC_sbl_fpoint8_read(cm->pdc, &m, qy->params.elts[0], qy->params.elts[1], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val: num = %d  denom = %u\n", (int)targ.num, (unsigned int)targ.denom),
		"PDC_sbl_fpoint8_write2buf",
		1,
		PDC_sbl_fpoint8_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 1, qy->params.elts[1], &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(sbl_fpoint16),
		CM_RW_FN_PARAMS,
		PDC_fpoint16 targ,
		PDC_sbl_fpoint16_read(cm->pdc, &m, qy->params.elts[0], qy->params.elts[1], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val: num = %d  denom = %u\n", (int)targ.num, (unsigned int)targ.denom),
		"PDC_sbl_fpoint16_write2buf",
		2,
		PDC_sbl_fpoint16_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 2, qy->params.elts[1], &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(sbl_fpoint32),
		CM_RW_FN_PARAMS,
		PDC_fpoint32 targ,
		PDC_sbl_fpoint32_read(cm->pdc, &m, qy->params.elts[0], qy->params.elts[1], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val: num = %ld  denom = %lu\n", (long)targ.num, (unsigned long)targ.denom),
		"PDC_sbl_fpoint32_write2buf",
		4,
		PDC_sbl_fpoint32_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 4, qy->params.elts[1], &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(sbl_fpoint64),
		CM_RW_FN_PARAMS,
		PDC_fpoint64 targ,
		PDC_sbl_fpoint64_read(cm->pdc, &m, qy->params.elts[0], qy->params.elts[1], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val: num = %lld  denom = %llu\n", (long long)targ.num, (unsigned long long)targ.denom),
		"PDC_sbl_fpoint64_write2buf",
		8,
		PDC_sbl_fpoint64_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 8, qy->params.elts[1], &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(sbl_ufpoint8),
		CM_RW_FN_PARAMS,
		PDC_ufpoint8 targ,
		PDC_sbl_ufpoint8_read(cm->pdc, &m, qy->params.elts[0], qy->params.elts[1], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val: num = %u  denom = %u\n", (unsigned int)targ.num, (unsigned int)targ.denom),
		"PDC_sbl_ufpoint8_write2buf",
		1,
		PDC_sbl_ufpoint8_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 1, qy->params.elts[1], &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(sbl_ufpoint16),
		CM_RW_FN_PARAMS,
		PDC_ufpoint16 targ,
		PDC_sbl_ufpoint16_read(cm->pdc, &m, qy->params.elts[0], qy->params.elts[1], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val: num = %u  denom = %u\n", (unsigned int)targ.num, (unsigned int)targ.denom),
		"PDC_sbl_ufpoint16_write2buf",
		2,
		PDC_sbl_ufpoint16_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 2, qy->params.elts[1], &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(sbl_ufpoint32),
		CM_RW_FN_PARAMS,
		PDC_ufpoint32 targ,
		PDC_sbl_ufpoint32_read(cm->pdc, &m, qy->params.elts[0], qy->params.elts[1], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val: num = %lu  denom = %lu\n", (unsigned long)targ.num, (unsigned long)targ.denom),
		"PDC_sbl_ufpoint32_write2buf",
		4,
		PDC_sbl_ufpoint32_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 4, qy->params.elts[1], &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(sbl_ufpoint64),
		CM_RW_FN_PARAMS,
		PDC_ufpoint64 targ,
		PDC_sbl_ufpoint64_read(cm->pdc, &m, qy->params.elts[0], qy->params.elts[1], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val: num = %llu  denom = %llu\n", (unsigned long long)targ.num, (unsigned long long)targ.denom),
		"PDC_sbl_ufpoint64_write2buf",
		8,
		PDC_sbl_ufpoint64_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 8, qy->params.elts[1], &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(sbh_fpoint8),
		CM_RW_FN_PARAMS,
		PDC_fpoint8 targ,
		PDC_sbh_fpoint8_read(cm->pdc, &m, qy->params.elts[0], qy->params.elts[1], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val: num = %d  denom = %u\n", (int)targ.num, (unsigned int)targ.denom),
		"PDC_sbl_fpoint8_write2buf",
		1,
		PDC_sbl_fpoint8_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 1, qy->params.elts[1], &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(sbh_fpoint16),
		CM_RW_FN_PARAMS,
		PDC_fpoint16 targ,
		PDC_sbh_fpoint16_read(cm->pdc, &m, qy->params.elts[0], qy->params.elts[1], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val: num = %d  denom = %u\n", (int)targ.num, (unsigned int)targ.denom),
		"PDC_sbl_fpoint16_write2buf",
		2,
		PDC_sbl_fpoint16_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 2, qy->params.elts[1], &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(sbh_fpoint32),
		CM_RW_FN_PARAMS,
		PDC_fpoint32 targ,
		PDC_sbh_fpoint32_read(cm->pdc, &m, qy->params.elts[0], qy->params.elts[1], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val: num = %ld  denom = %lu\n", (long)targ.num, (unsigned long)targ.denom),
		"PDC_sbl_fpoint32_write2buf",
		4,
		PDC_sbl_fpoint32_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 4, qy->params.elts[1], &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(sbh_fpoint64),
		CM_RW_FN_PARAMS,
		PDC_fpoint64 targ,
		PDC_sbh_fpoint64_read(cm->pdc, &m, qy->params.elts[0], qy->params.elts[1], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val: num = %lld  denom = %llu\n", (long long)targ.num, (unsigned long long)targ.denom),
		"PDC_sbl_fpoint64_write2buf",
		8,
		PDC_sbl_fpoint64_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 8, qy->params.elts[1], &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(sbh_ufpoint8),
		CM_RW_FN_PARAMS,
		PDC_ufpoint8 targ,
		PDC_sbh_ufpoint8_read(cm->pdc, &m, qy->params.elts[0], qy->params.elts[1], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val: num = %u  denom = %u\n", (unsigned int)targ.num, (unsigned int)targ.denom),
		"PDC_sbl_ufpoint8_write2buf",
		1,
		PDC_sbl_ufpoint8_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 1, qy->params.elts[1], &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(sbh_ufpoint16),
		CM_RW_FN_PARAMS,
		PDC_ufpoint16 targ,
		PDC_sbh_ufpoint16_read(cm->pdc, &m, qy->params.elts[0], qy->params.elts[1], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val: num = %u  denom = %u\n", (unsigned int)targ.num, (unsigned int)targ.denom),
		"PDC_sbl_ufpoint16_write2buf",
		2,
		PDC_sbl_ufpoint16_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 2, qy->params.elts[1], &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(sbh_ufpoint32),
		CM_RW_FN_PARAMS,
		PDC_ufpoint32 targ,
		PDC_sbh_ufpoint32_read(cm->pdc, &m, qy->params.elts[0], qy->params.elts[1], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val: num = %lu  denom = %lu\n", (unsigned long)targ.num, (unsigned long)targ.denom),
		"PDC_sbl_ufpoint32_write2buf",
		4,
		PDC_sbl_ufpoint32_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 4, qy->params.elts[1], &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(sbh_ufpoint64),
		CM_RW_FN_PARAMS,
		PDC_ufpoint64 targ,
		PDC_sbh_ufpoint64_read(cm->pdc, &m, qy->params.elts[0], qy->params.elts[1], &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val: num = %llu  denom = %llu\n", (unsigned long long)targ.num, (unsigned long long)targ.denom),
		"PDC_sbl_ufpoint64_write2buf",
		8,
		PDC_sbl_ufpoint64_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, 8, qy->params.elts[1], &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(char),
		CM_RW_FN_PARAMS,
		PDC_char targ,
		PDC_char_read(cm->pdc, &m, &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %s\n", PDC_qfmt_char(targ)),
		"PDC_a_char_write2buf",
		1,
		PDC_a_char_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(a_char),
		CM_RW_FN_PARAMS,
		PDC_char targ,
		PDC_a_char_read(cm->pdc, &m, &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %s\n", PDC_qfmt_char(targ)),
		"PDC_a_char_write2buf",
		1,
		PDC_a_char_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(e_char),
		CM_RW_FN_PARAMS,
		PDC_char targ,
		PDC_e_char_read(cm->pdc, &m, &pd, &targ),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = %s\n", PDC_qfmt_char(targ)),
		"PDC_a_char_write2buf",
		1,
		PDC_a_char_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, &pd, &targ)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(string_FW),
		CM_RW_FN_PARAMS,
		PDC_string *targp = &(cm->tmp1),
		PDC_string_FW_read(cm->pdc, &m, qy->params.elts[0], &pd, targp),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = [%s]\n", PDC_qfmt_str(targp)),
		"PDC_a_string_FW_write2buf",
		qy->params.elts[0],
		PDC_a_string_FW_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, qy->params.elts[0], &pd, targp)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(a_string_FW),
		CM_RW_FN_PARAMS,
		PDC_string *targp = &(cm->tmp1),
		PDC_a_string_FW_read(cm->pdc, &m, qy->params.elts[0], &pd, targp),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = [%s]\n", PDC_qfmt_str(targp)),
		"PDC_a_string_FW_write2buf",
		qy->params.elts[0],
		PDC_a_string_FW_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, qy->params.elts[0], &pd, targp)
);

CM_RW_FN_IMPL(  CM_RW_FN_NM(e_string_FW),
		CM_RW_FN_PARAMS,
		PDC_string *targp = &(cm->tmp1),
		PDC_e_string_FW_read(cm->pdc, &m, qy->params.elts[0], &pd, targp),
		PDC_NULL_STMT,
		sfprintf(cm->errf, "  ==> val = [%s]\n", PDC_qfmt_str(targp)),
		"PDC_a_string_FW_write2buf",
		qy->params.elts[0],
		PDC_a_string_FW_write2buf(cm->pdc, cm->outbuf_cursor, remain_out, &buf_full, qy->params.elts[0], &pd, targp)
);

