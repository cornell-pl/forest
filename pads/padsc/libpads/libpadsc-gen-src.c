#pragma prototyped
/*
 * libpadsc generated code (expanded macros)
 *
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#include "libpadsc-read-macros.h"

/* ================================================================================ */
/* FIXED-WIDTH ASCII INTEGER READ FUNCTIONS */

/*
 * PDC_AINT_FW_READ_FN(fn_name, targ_type, int_type, strtonum_fn, invalid_err, opt_tmp_test)
 */

PDC_AINT_FW_READ_FN(PDC_aint8_fw_read,  PDC_int8,  long,      strtol,  PDC_INVALID_AINT,
 || tmp < PDC_MIN_INT8  || tmp > PDC_MAX_INT8);
PDC_AINT_FW_READ_FN(PDC_aint16_fw_read, PDC_int16, long,      strtol,  PDC_INVALID_AINT,
 || tmp < PDC_MIN_INT16 || tmp > PDC_MAX_INT16);
PDC_AINT_FW_READ_FN(PDC_aint32_fw_read, PDC_int32, long,      strtol,  PDC_INVALID_AINT, );
PDC_AINT_FW_READ_FN(PDC_aint64_fw_read, PDC_int64, long long, strtoll, PDC_INVALID_AINT, );

PDC_AINT_FW_READ_FN(PDC_auint8_fw_read,  PDC_uint8,  unsigned long,      strtoul,  PDC_INVALID_AUINT,
 || tmp > PDC_MAX_UINT8);
PDC_AINT_FW_READ_FN(PDC_auint16_fw_read, PDC_uint16, unsigned long,      strtoul,  PDC_INVALID_AUINT,
 || tmp > PDC_MAX_UINT16);
PDC_AINT_FW_READ_FN(PDC_auint32_fw_read, PDC_uint32, unsigned long,      strtoul,  PDC_INVALID_AUINT, );
PDC_AINT_FW_READ_FN(PDC_auint64_fw_read, PDC_uint64, unsigned long long, strtoull, PDC_INVALID_AUINT, );

/* ================================================================================ */
/* BINARY INTEGER READ FUNCTIONS */

/*
 * PDC_BINT_READ_FN(fn_name, targ_type, width, swapmem_op)
 *
 * swapmem ops:
 *    0 -> straight copy
 *    1 -> reverse each byte in each string of 2 bytes
 *    3 -> reverse each byte in each string of 4 bytes
 *    4 -> swap upper/lower 4 bytes in each 8 byte value
 *    7 -> reverse each byte in each string of 8 bytes
 */

PDC_BINT_READ_FN(PDC_bint8_read,         PDC_int8,   1, 0);
PDC_BINT_READ_FN(PDC_buint8_read,        PDC_uint8,  1, 0);

PDC_BINT_READ_FN(PDC_bint16_norev_read,  PDC_int16,  2, 0);
PDC_BINT_READ_FN(PDC_buint16_norev_read, PDC_uint16, 2, 0);
PDC_BINT_READ_FN(PDC_bint16_rev_read,    PDC_int16,  2, 1);
PDC_BINT_READ_FN(PDC_buint16_rev_read,   PDC_uint16, 2, 1);

PDC_BINT_READ_FN(PDC_bint32_norev_read,  PDC_int32,  4, 0);
PDC_BINT_READ_FN(PDC_buint32_norev_read, PDC_uint32, 4, 0);
PDC_BINT_READ_FN(PDC_bint32_rev_read,    PDC_int32,  4, 3);
PDC_BINT_READ_FN(PDC_buint32_rev_read,   PDC_uint32, 4, 3);

PDC_BINT_READ_FN(PDC_bint64_norev_noswap_read,  PDC_int64,  8, 0);
PDC_BINT_READ_FN(PDC_buint64_norev_noswap_read, PDC_uint64, 8, 0);
PDC_BINT_READ_FN(PDC_bint64_rev_noswap_read,    PDC_int64,  8, 7);
PDC_BINT_READ_FN(PDC_buint64_rev_noswap_read,   PDC_uint64, 8, 7);

PDC_BINT_READ_FN(PDC_bint64_norev_swap_read,    PDC_int64,  8, 4);
PDC_BINT_READ_FN(PDC_buint64_norev_swap_read,   PDC_uint64, 8, 4);
#if 0 
PDC_BINT_READ_FN(PDC_bint64_rev_swap_read,      PDC_int64,  8, ?);
PDC_BINT_READ_FN(PDC_buint64_rev_swap_read,     PDC_uint64, 8, ?);
#endif


