/*
 * readwrite functions for cmonster
 *
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#include "cmonster.h"

#define CM_RW_FN_START \
  size_t avail_in   = end - (begin + qy->off); \
  size_t remain_out = cm->outbuf_end - cm->outbuf_cursor; \
  error(0, "rw_fn for %s called", qy->entry->tname); \
  error(0, "  outbuf has %lu bytes remaining", (unsigned long)remain_out); \
  error(0, "  input has %lu bytes available starting at offset %lu", \
	(unsigned long)avail_in, (unsigned long)qy->off); \
  if (qy->out_sz > remain_out) { \
    error(0, \
	  "  Error: qy requires %lu output bytes but outbuf has only %lu bytes\n" \
	  "   remaining.  Skipping this data item.", \
	  (unsigned long)qy->out_sz, (unsigned long)remain_out); \
    return PDC_ERR; \
  } \
  if (qy->in_sz > avail_in) { \
    error(0, \
	  "  Error: qy requires %lu input bytes but input record has only %lu bytes\n" \
	  "  available starting at offset %lu.  Skipping this data item.", \
	  (unsigned long)qy->in_sz, (unsigned long)avail_in, (unsigned long)qy->off); \
    return PDC_ERR; \
  }

#define CM_RW_FN_END \
  error(0, "  advancing outbuf_cursor by %lu bytes", (unsigned long)qy->out_sz); \
  cm->outbuf_cursor += qy->out_sz; \
  return PDC_OK


CM_RW_FN_DECL(int8_FW)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(int16_FW)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(int32_FW)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(int64_FW)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(uint8_FW)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(uint16_FW)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(uint32_FW)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(uint64_FW)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(a_int8_FW)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(a_int16_FW)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(a_int32_FW)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(a_int64_FW)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(a_uint8_FW)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(a_uint16_FW)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(a_uint32_FW)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(a_uint64_FW)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(e_int8_FW)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(e_int16_FW)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(e_int32_FW)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(e_int64_FW)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(e_uint8_FW)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(e_uint16_FW)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(e_uint32_FW)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(e_uint64_FW)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(b_int8)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(b_int16)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(b_int32)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(b_int64)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(b_uint8)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(b_uint16)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(b_uint32)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(b_uint64)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(ebc_int8)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(ebc_int16)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(ebc_int32)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(ebc_int64)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(ebc_uint8)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(ebc_uint16)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(ebc_uint32)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(ebc_uint64)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(bcd_int8)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(bcd_int16)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(bcd_int32)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(bcd_int64)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(bcd_uint8)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(bcd_uint16)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(bcd_uint32)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(bcd_uint64)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(sbl_int8)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(sbl_int16)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(sbl_int32)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(sbl_int64)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(sbl_uint8)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(sbl_uint16)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(sbl_uint32)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(sbl_uint64)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}


CM_RW_FN_DECL(sbh_int8)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(sbh_int16)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(sbh_int32)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(sbh_int64)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(sbh_uint8)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(sbh_uint16)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(sbh_uint32)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(sbh_uint64)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}


CM_RW_FN_DECL(ebc_fpoint8)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(ebc_fpoint16)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(ebc_fpoint32)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(ebc_fpoint64)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(ebc_ufpoint8)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(ebc_ufpoint16)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(ebc_ufpoint32)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(ebc_ufpoint64)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(bcd_fpoint8)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(bcd_fpoint16)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(bcd_fpoint32)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(bcd_fpoint64)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(bcd_ufpoint8)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(bcd_ufpoint16)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(bcd_ufpoint32)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(bcd_ufpoint64)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(sbl_fpoint8)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(sbl_fpoint16)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(sbl_fpoint32)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(sbl_fpoint64)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(sbl_ufpoint8)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(sbl_ufpoint16)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(sbl_ufpoint32)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(sbl_ufpoint64)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(sbh_fpoint8)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(sbh_fpoint16)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(sbh_fpoint32)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(sbh_fpoint64)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(sbh_ufpoint8)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(sbh_ufpoint16)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(sbh_ufpoint32)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(sbh_ufpoint64)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(char)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(string_FW)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(a_char)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(a_string_FW)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(e_char)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

CM_RW_FN_DECL(e_string_FW)
{
  CM_RW_FN_START;
  CM_RW_FN_END;
}

