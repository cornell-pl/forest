#ifdef _USE_PROTO
#pragma prototyped
#endif

/*
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#ifndef __PB_INT_H__
#define __PB_INT_H__

#ifndef __PADS_H__
#error Pb_int.h is intended to be included from pads.h, do not include it directly
#endif

#if P_CONFIG_B_INT > 0

/* ================================================================================
 * READ
 */

/* ================================================================================
 * COMMON WIDTH BINARY INTEGER READ FUNCTIONS
 *
 * These functions parse signed or unsigned binary integers
 * of common bit widths (8, 16, 32, and 64 bit widths).
 * Whether bytes are reversed is controlled by the endian-ness of
 * the machine (determined automatically) and pads->disc->d_endian. If they differ,
 * byte order is reversed in the in-memory representation, otherwise it is not.
 *
 * A good way to set the d_endian value in a machine-independent way is to
 * use PRAGMA CHECK_ENDIAN with the first multi-byte binary integer field that appears
 * in the data.  For example, this header definition:
 *
 * Pstruct header {
 *    Pb_uint16 version : version < 10; //- PRAGMA CHECK_ENDIAN
 *    ..etc..
 * };
 *
 * indicates the first value is a 2-byte unsigned binary integer, version,
 * whose value should be less than 10.   The pragma indicates that there
 * should be two attempts at reading the version field: once with the
 * current pads->disc->d_endian setting, and (if the read fails) once with the
 * opposite pads->disc->d_endian setting.  If the second read succeeds, then
 * the new pads->disc->d_endian setting is retained, otherwise the original
 * pads->disc->d_endian setting is retained.
 * 
 * N.B. The CHECK_ENDIAN pragma is only able to determine the correct endian
 * choice for a field that has an attached constraint, where the
 * wrong choice of endian setting will always cause the constraint to fail.
 * (In the above example, if a value < 10 is read with the wrong d_endian
 * setting, the result is a value that is much greater than 10.) 
 *
 * For all cases, if the specified number of bytes is available, it is always read.
 * If the width is not available:
 *    + pd->loc.b/e set to elt/char position of start/end of the 'too small' field
 *    + IO cursor is not advanced
 *    + if P_Test_NotIgnore(*m), pd->errCode set to P_WIDTH_NOT_AVAILABLE,
 *         pd->nerr set to 1, and an error is reported
 */

#if P_CONFIG_READ_FUNCTIONS > 0
Perror_t Pb_int8_read (P_t *pads, const Pbase_m *m,
		       Pbase_pd *pd, Pint8 *res_out);

Perror_t Pb_int16_read(P_t *pads, const Pbase_m *m,
		       Pbase_pd *pd, Pint16 *res_out);

Perror_t Pb_int32_read(P_t *pads, const Pbase_m *m,
		       Pbase_pd *pd, Pint32 *res_out);

Perror_t Pb_int64_read(P_t *pads, const Pbase_m *m,
		       Pbase_pd *pd, Pint64 *res_out);

Perror_t Pb_uint8_read (P_t *pads, const Pbase_m *m,
			Pbase_pd *pd, Puint8 *res_out);

Perror_t Pb_uint16_read(P_t *pads, const Pbase_m *m,
			Pbase_pd *pd, Puint16 *res_out);

Perror_t Pb_uint32_read(P_t *pads, const Pbase_m *m,
			Pbase_pd *pd, Puint32 *res_out);

Perror_t Pb_uint64_read(P_t *pads, const Pbase_m *m,
			Pbase_pd *pd, Puint64 *res_out);
#endif /* P_CONFIG_READ_FUNCTIONS */


/* ================================================================================
 * WRITE
 */

#if P_CONFIG_WRITE_FUNCTIONS > 0
ssize_t Pb_int8_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint8   *val);
ssize_t Pb_int16_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint16  *val);
ssize_t Pb_int32_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint32  *val);
ssize_t Pb_int64_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint64  *val);

ssize_t Pb_uint8_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint8  *val);
ssize_t Pb_uint16_write2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint16 *val);
ssize_t Pb_uint32_write2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *val);
ssize_t Pb_uint64_write2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint64 *val);

ssize_t Pb_int8_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint8   *val, const char *tag, int indent);
ssize_t Pb_int16_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint16  *val, const char *tag, int indent);
ssize_t Pb_int32_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint32  *val, const char *tag, int indent);
ssize_t Pb_int64_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pint64  *val, const char *tag, int indent);

ssize_t Pb_uint8_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint8  *val, const char *tag, int indent);
ssize_t Pb_uint16_write_xml_2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint16 *val, const char *tag, int indent);
ssize_t Pb_uint32_write_xml_2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *val, const char *tag, int indent);
ssize_t Pb_uint64_write_xml_2io(P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint64 *val, const char *tag, int indent);

ssize_t Pb_int8_write2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint8   *val);
ssize_t Pb_int16_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint16  *val);
ssize_t Pb_int32_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint32  *val);
ssize_t Pb_int64_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint64  *val);

ssize_t Pb_uint8_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint8  *val);
ssize_t Pb_uint16_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint16 *val);
ssize_t Pb_uint32_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *val);
ssize_t Pb_uint64_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint64 *val);

ssize_t Pb_int8_write_xml_2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint8   *val, const char *tag, int indent);
ssize_t Pb_int16_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint16  *val, const char *tag, int indent);
ssize_t Pb_int32_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint32  *val, const char *tag, int indent);
ssize_t Pb_int64_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pint64  *val, const char *tag, int indent);

ssize_t Pb_uint8_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint8  *val, const char *tag, int indent);
ssize_t Pb_uint16_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint16 *val, const char *tag, int indent);
ssize_t Pb_uint32_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *val, const char *tag, int indent);
ssize_t Pb_uint64_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint64 *val, const char *tag, int indent);

ssize_t Pb_int8_fmt2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Pint8  *rep);
ssize_t Pb_int8_fmt2buf_final  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				Pbase_m *m, Pbase_pd *pd, Pint8  *rep);
ssize_t Pb_int16_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Pint16 *rep);
ssize_t Pb_int16_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				Pbase_m *m, Pbase_pd *pd, Pint16 *rep);
ssize_t Pb_int32_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Pint32 *rep);
ssize_t Pb_int32_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				Pbase_m *m, Pbase_pd *pd, Pint32 *rep);
ssize_t Pb_int64_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Pint64 *rep);
ssize_t Pb_int64_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				Pbase_m *m, Pbase_pd *pd, Pint64 *rep);

ssize_t Pb_uint8_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Puint8  *rep);
ssize_t Pb_uint8_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				Pbase_m *m, Pbase_pd *pd, Puint8  *rep);
ssize_t Pb_uint16_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Puint16 *rep);
ssize_t Pb_uint16_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				Pbase_m *m, Pbase_pd *pd, Puint16 *rep);
ssize_t Pb_uint32_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Puint32 *rep);
ssize_t Pb_uint32_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				Pbase_m *m, Pbase_pd *pd, Puint32 *rep);
ssize_t Pb_uint64_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Puint64 *rep);
ssize_t Pb_uint64_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				Pbase_m *m, Pbase_pd *pd, Puint64 *rep);

ssize_t Pb_int8_fmt2io   (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Pint8  *rep);
ssize_t Pb_int16_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Pint16 *rep);
ssize_t Pb_int32_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Pint32 *rep);
ssize_t Pb_int64_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Pint64 *rep);

ssize_t Pb_uint8_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Puint8  *rep);
ssize_t Pb_uint16_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Puint16 *rep);
ssize_t Pb_uint32_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Puint32 *rep);
ssize_t Pb_uint64_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			     Pbase_m *m, Pbase_pd *pd, Puint64 *rep);
#endif /* P_CONFIG_WRITE_FUNCTIONS > 0 */

#endif /* P_CONFIG_B_INT > 0 */
#endif /*  __PB_INT_H__  */
