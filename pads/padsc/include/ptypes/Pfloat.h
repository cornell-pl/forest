#ifdef _USE_PROTO
#pragma prototyped
#endif

/*
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#ifndef __PFLOAT_H__
#define __PFLOAT_H__

#ifndef __PADS_H__
#error Pfloat.h is intended to be included from pads.h, do not include it directly
#endif

/* ================================================================================
 * READ
 */

/* ================================================================================
 * CHARACTER-BASED FLOAT READ FUNCTIONS
 * 
 * DEFAULT                        ASCII                          EBCDIC
 * -----------------------------  -----------------------------  -----------------------------
 * Pfloat32_read                  Pa_float32_read                Pe_float32_read
 * Pfloat64_read                  Pa_float64_read                Pe_float64_read
 */

#if P_CONFIG_READ_FUNCTIONS > 0

#if P_CONFIG_A_FLOAT > 0
Perror_t Pa_float32_read(P_t *pads, const Pbase_m *m,
			 Pbase_pd *pd, Pfloat32 *res_out);
Perror_t Pa_float64_read(P_t *pads, const Pbase_m *m,
			 Pbase_pd *pd, Pfloat64 *res_out);
#endif

#if P_CONFIG_E_FLOAT > 0
Perror_t Pe_float32_read(P_t *pads, const Pbase_m *m,
			 Pbase_pd *pd, Pfloat32 *res_out);
Perror_t Pe_float64_read(P_t *pads, const Pbase_m *m,
			 Pbase_pd *pd, Pfloat64 *res_out);
#endif

#ifdef FOR_CKIT
#if P_CONFIG_A_FLOAT > 0 && P_CONFIG_E_FLOAT > 0
Perror_t Pfloat32_read(P_t *pads, const Pbase_m *m,
		       Pbase_pd *pd, Pfloat32 *res_out);
Perror_t Pfloat64_read(P_t *pads, const Pbase_m *m,
		       Pbase_pd *pd, Pfloat64 *res_out);
#endif
#endif

#endif /* P_CONFIG_READ_FUNCTIONS */

/* ================================================================================
 * WRITE
 */

#if P_CONFIG_WRITE_FUNCTIONS > 0

#if P_CONFIG_A_FLOAT > 0
ssize_t Pa_float32_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfloat32  *val);
ssize_t Pa_float64_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfloat64  *val);

ssize_t Pa_float32_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfloat32  *val, const char *tag, int indent);
ssize_t Pa_float64_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfloat64  *val, const char *tag, int indent);

ssize_t Pa_float32_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Pfloat32 *rep);
ssize_t Pa_float32_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Pfloat32 *rep);
ssize_t Pa_float64_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Pfloat64 *rep);
ssize_t Pa_float64_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Pfloat64 *rep);

ssize_t Pa_float32_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Pfloat32 *rep);
ssize_t Pa_float64_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Pfloat64 *rep);
#endif

#if P_CONFIG_E_FLOAT > 0
ssize_t Pe_float32_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfloat32  *val);
ssize_t Pe_float64_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfloat64  *val);

ssize_t Pe_float32_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfloat32  *val, const char *tag, int indent);
ssize_t Pe_float64_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfloat64  *val, const char *tag, int indent);

ssize_t Pe_float32_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Pfloat32 *rep);
ssize_t Pe_float32_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Pfloat32 *rep);
ssize_t Pe_float64_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Pfloat64 *rep);
ssize_t Pe_float64_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Pfloat64 *rep);

ssize_t Pe_float32_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Pfloat32 *rep);
ssize_t Pe_float64_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Pfloat64 *rep);
#endif

#ifdef FOR_CKIT
#if P_CONFIG_A_FLOAT > 0 || P_CONFIG_E_FLOAT > 0
ssize_t Pfloat32_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pfloat32  *val);
ssize_t Pfloat64_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pfloat64  *val);

ssize_t Pfloat32_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfloat32  *val);
ssize_t Pfloat64_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfloat64  *val);

ssize_t Pfloat32_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pfloat32  *val, const char *tag, int indent);
ssize_t Pfloat64_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Pfloat64  *val, const char *tag, int indent);

ssize_t Pfloat32_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfloat32  *val, const char *tag, int indent);
ssize_t Pfloat64_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Pfloat64  *val, const char *tag, int indent);

ssize_t Pfloat32_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			  Pbase_m *m, Pbase_pd *pd, Pfloat32 *rep);
ssize_t Pfloat32_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				Pbase_m *m, Pbase_pd *pd, Pfloat32 *rep);
ssize_t Pfloat64_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			  Pbase_m *m, Pbase_pd *pd, Pfloat64 *rep);
ssize_t Pfloat64_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				Pbase_m *m, Pbase_pd *pd, Pfloat64 *rep);

ssize_t Pfloat32_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			  Pbase_m *m, Pbase_pd *pd, Pfloat32 *rep);
ssize_t Pfloat64_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			  Pbase_m *m, Pbase_pd *pd, Pfloat64 *rep);
#endif
#endif

#endif  /*  P_CONFIG_WRITE_FUNCTIONS > 0  */

#endif /*  __PFLOAT_H__  */

