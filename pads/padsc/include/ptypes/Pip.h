#ifdef _USE_PROTO
#pragma prototyped
#endif

/*
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#ifndef __PIP_H__
#define __PIP_H__

#ifndef __PADS_H__
#error Pip.h is intended to be included from pads.h, do not include it directly
#endif

/* ================================================================================
 * IP ADDRESS READ FUNCTIONS
 *
 * DEFAULT                        ASCII                          EBCDIC
 * -----------------------------  -----------------------------  -----------------------------
 * Pip_read                       Pa_ip_read                     Pe_ip_read
 *
 * Attempts to read a numeric IP address string, i.e., an IP address form
 * consisting of one to four numeric parts with values 0-255,
 * separated by ".", with an optional trailing dot.  When there are
 * fewer than four parts, the missing parts are treated as implicitly
 * zero, and are inserted as shown in the following diagram,
 * which shows the 8 legal input forms and the equivalent form.
 *
 *  <part1>                          --> <part1>.0.0.0
 *  <part1>.                         --> <part1>.0.0.0.
 *  <part1>.<part4>                  --> <part1>.0.0.<part4>
 *  <part1>.<part4>.                 --> <part1>.0.0.<part4>.
 *  <part1>.<part2>.<part4>          --> <part1>.<part2>.0.<part4>
 *  <part1>.<part2>.<part4>.         --> <part1>.<part2>.0.<part4>.
 *  <part1>.<part2>.<part3>.<part4>  --> same
 *  <part1>.<part2>.<part3>.<part4>. --> same
 *
 * where each <part> is made up of 1 to 3 digits which specify a number
 * in the range [0, 255].
 *
 * The result is a single Puint32 value with each part encoded in one
 * of the four bytes.  part1 is stored in the high-order byte, part4
 * in the low-order byte.  You can obtain each part using the macro
 *
 *   P_IP_PART(addr, part)
 *
 * where part must be from 1 to 4.
 *
 * The digit chars and "." char are read as EBCDIC chars if the EBCDIC
 * form is used or if the DEFAULT form is used and
 * pads->disc->def_charset is Pcharset_EBCDIC.  Otherwise the data is
 * read as ASCII chars.
 *
 * If the current IO cursor position points to a valid IP address string:
 *   + Sets (*res_out) to the resulting Puint32
 *   + advances the IO cursor position to just after the last legal
 *      character in the IP address string
 *   + returns P_OK
 * Otherwise:
 *   + pd->loc.b/e set to the IO cursor position
 *   + IO cursor is not advanced
 *   + if P_Test_NotIgnore(*m), pd->errCode set to P_INVALID_IP,
 *         pd->nerr set to 1, and an error is reported
 *   + returns P_ERR
 */

#ifdef FOR_CKIT
#if P_CONFIG_READ_FUNCTIONS > 0

#if P_CONFIG_A_CHAR_STRING > 0
Perror_t Pa_ip_read(P_t *pads, const Pbase_m *m,
		    Pbase_pd *pd, Puint32 *res_out);
#endif

#if P_CONFIG_E_CHAR_STRING > 0
Perror_t Pe_ip_read(P_t *pads, const Pbase_m *m,
		    Pbase_pd *pd, Puint32 *res_out);
#endif

#if P_CONFIG_A_CHAR_STRING > 0 && P_CONFIG_E_CHAR_STRING > 0
Perror_t Pip_read  (P_t *pads, const Pbase_m *m,
		    Pbase_pd *pd, Puint32 *res_out);
#endif

#endif /* P_CONFIG_READ_FUNCTIONS */
#endif /* FOR_CKIT */

// The helper macro P_IP_PART(addr, part) takes a Puint32 addr
// and a part number from 1 to 4, and produces the specified part.
#define P_IP_PART(addr, part) \
  ((addr >> (8*(4-part))) & 0xFF)

/* ================================================================================
 * IP WRITE FUNCTIONS
 * DEFAULT                        ASCII                          EBCDIC
 * -----------------------------  -----------------------------  -----------------------------
 * Pip_write2io                   Pa_ip_write2io                 Pe_ip_write2io 
 *
 * Pip_write2buf                  Pa_ip_write2buf                Pe_ip_write2buf
 */

#ifdef FOR_CKIT
#if P_CONFIG_WRITE_FUNCTIONS > 0

#if P_CONFIG_A_CHAR_STRING > 0
ssize_t Pa_ip_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d);
ssize_t Pa_ip_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *d);

ssize_t Pa_ip_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent);
ssize_t Pa_ip_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *d, const char *tag, int indent);
#endif

#if P_CONFIG_E_CHAR_STRING > 0
ssize_t Pe_ip_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d);
ssize_t Pe_ip_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *d);

ssize_t Pe_ip_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent);
ssize_t Pe_ip_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *d, const char *tag, int indent);
#endif

#if P_CONFIG_A_CHAR_STRING > 0 && P_CONFIG_E_CHAR_STRING > 0
ssize_t Pip_write2io   (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d);
ssize_t Pip_write2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *d);

ssize_t Pip_write_xml_2io   (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent);
ssize_t Pip_write_xml_2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *d, const char *tag, int indent);
#endif

#endif /* P_CONFIG_WRITE_FUNCTIONS */
#endif /* FOR_CKIT */

#if P_CONFIG_A_CHAR_STRING > 0
ssize_t Pa_ip_fmt2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			Pbase_m *m, Pbase_pd *pd, Puint32  *rep);
ssize_t Pa_ip_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Puint32 *rep);
ssize_t Pa_ip_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
		      Pbase_m *m, Pbase_pd *pd, Puint32 *rep);
#endif

#if P_CONFIG_E_CHAR_STRING > 0
ssize_t Pe_ip_fmt2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			Pbase_m *m, Pbase_pd *pd, Puint32  *rep);
ssize_t Pe_ip_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Puint32 *rep);
ssize_t Pe_ip_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
		      Pbase_m *m, Pbase_pd *pd, Puint32 *rep);
#endif

#if P_CONFIG_A_CHAR_STRING > 0 || P_CONFIG_E_CHAR_STRING > 0
ssize_t Pip_fmt2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
		      Pbase_m *m, Pbase_pd *pd, Puint32  *rep);
ssize_t Pip_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			   Pbase_m *m, Pbase_pd *pd, Puint32 *rep);
ssize_t Pip_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
		    Pbase_m *m, Pbase_pd *pd, Puint32 *rep);
#endif

#endif /*  __PIP_H__  */

