#ifdef _USE_PROTO
#pragma prototyped
#endif

/*
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#ifndef __PDATE_H__
#define __PDATE_H__

#ifndef __PADS_H__
#error Pdate.h is intended to be included from pads.h, do not include it directly
#endif

/* ================================================================================
 * READ FUNCTIONS
 *
 * DEFAULT                        ASCII                           EBCDIC
 * -----------------------------  -----------------------------   -----------------------------
 * Ptimestamp_explicit_FW_read    Pa_timestamp_explicit_FW_read   Pe_timestamp_explicit_FW_read
 * Ptimestamp_explicit_read       Pa_timestamp_explicit_read      Pe_timestamp_explicit_read
 * Ptimestamp_explicit_ME_read    Pa_timestamp_explicit_ME_read   Pe_timestamp_explicit_ME_read
 * Ptimestamp_explicit_CME_read   Pa_timestamp_explicit_CME_read  Pe_timestamp_explicit_CME_read
 * Ptimestamp_explicit_SE_read    Pa_timestamp_explicit_SE_read   Pe_timestamp_explicit_SE_read
 * Ptimestamp_explicit_CSE_read   Pa_timestamp_explicit_CSE_read  Pe_timestamp_explicit_CSE_read
 *
 * Ptimestamp_FW_read             Pa_timestamp_FW_read            Pe_timestamp_FW_read
 * Ptimestamp_read                Pa_timestamp_read               Pe_timestamp_read
 * Ptimestamp_ME_read             Pa_timestamp_ME_read            Pe_timestamp_ME_read
 * Ptimestamp_CME_read            Pa_timestamp_CME_read           Pe_timestamp_CME_read
 * Ptimestamp_SE_read             Pa_timestamp_SE_read            Pe_timestamp_SE_read
 * Ptimestamp_CSE_read            Pa_timestamp_CSE_read           Pe_timestamp_CSE_read
 *
 * Pdate_FW_read                  Pa_date_FW_read                 Pe_date_FW_read
 * Pdate_read                     Pa_date_read                    Pe_date_read
 * Pdate_ME_read                  Pa_date_ME_read                 Pe_date_ME_read
 * Pdate_CME_read                 Pa_date_CME_read                Pe_date_CME_read
 * Pdate_SE_read                  Pa_date_SE_read                 Pe_date_SE_read
 * Pdate_CSE_read                 Pa_date_CSE_read                Pe_date_CSE_read
 *
 * Ptime_FW_read                  Pa_time_FW_read                 Pe_time_FW_read
 * Ptime_read                     Pa_time_read                    Pe_time_read
 * Ptime_ME_read                  Pa_time_ME_read                 Pe_time_ME_read
 * Ptime_CME_read                 Pa_time_CME_read                Pe_time_CME_read
 * Ptime_SE_read                  Pa_time_SE_read                 Pe_time_SE_read
 * Ptime_CSE_read                 Pa_time_CSE_read                Pe_time_CSE_read
 *
 * Ptimestamp_explicit variants: 
 *     Converts ASCII/EBCDIC char date/time description into seconds since Midnight Jan 1, 1970.
 *     Format-based parsing is based on libast's tmdate function.
 *     Input format and input time zone are specified explicitly.
 *     The format used controls whether input is date and time, just date, or just time.
 *
 * Ptimestamp variants:
 *     Like Ptimestamp_explicit, but input format and input time zone are taken from
 *     disc->in_formats.timestamp and disc->in_time_zone.
 *     INTENDED to be used for both a date and time, but format determines actual use.
 *
 * Pdate variants:
 *     Like Ptimestamp_explicit, but input format and input time zone are taken from
 *     disc->in_formats.date and disc->in_time_zone.
 *     INTENDED to be used for just date, but format determines actual use.
 *
 * Ptime variants:
 *     Like Ptimestamp_explicit, but input format and input time zone are taken from
 *     disc->in_formats.time and disc->in_time_zone.
 *     INTENDED to be used for just time, but format determines actual use.
 *
 * Each of the types above corresponds to one of the Pstring variants.
 * In each case one specifies the extent of a 'string' in the input
 * that is to be converted to a Puint32 representing the date in
 * seconds since the epoch.  For the different date formats that are
 * supported, see the discussion of disc->in_formats in pads.h.
 *
 * If the current IO cursor position points to a valid date string:
 *   + Sets (*res_out) to the resulting date in seconds since the epoch
 *   + advances the IO cursor position to just after the last legal character
 *     in the date string
 *   + returns P_OK
 * Otherwise:
 *   + does not advance the IO cursor pos
 *   + returns P_ERR
 */

#ifdef FOR_CKIT
#if P_CONFIG_READ_FUNCTIONS > 0

#if P_CONFIG_A_CHAR_STRING > 0
Perror_t Pa_timestamp_explicit_FW_read (P_t *pads, const Pbase_m *m,
					Pbase_pd *pd, Puint32 *res_out, size_t width,
					const char *format, Tm_zone_t *tzone);
Perror_t Pa_timestamp_explicit_read    (P_t *pads, const Pbase_m *m,
					Pbase_pd *pd, Puint32 *res_out, Pchar stopChar,
					const char *format, Tm_zone_t *tzone);
Perror_t Pa_timestamp_explicit_ME_read (P_t *pads, const Pbase_m *m,
					Pbase_pd *pd, Puint32 *res_out, const char *matchRegexp,
					const char *format, Tm_zone_t *tzone);
Perror_t Pa_timestamp_explicit_CME_read(P_t *pads, const Pbase_m *m,
					Pbase_pd *pd, Puint32 *res_out, Pregexp_t *matchRegexp,
					const char *format, Tm_zone_t *tzone);
Perror_t Pa_timestamp_explicit_SE_read (P_t *pads, const Pbase_m *m,
					Pbase_pd *pd, Puint32 *res_out, const char *stopRegexp,
					const char *format, Tm_zone_t *tzone);
Perror_t Pa_timestamp_explicit_CSE_read(P_t *pads, const Pbase_m *m,
					Pbase_pd *pd, Puint32 *res_out, Pregexp_t *stopRegexp,
					const char *format, Tm_zone_t *tzone);

Perror_t Pa_timestamp_FW_read (P_t *pads, const Pbase_m *m,
			       Pbase_pd *pd, Puint32 *res_out, size_t width);
Perror_t Pa_timestamp_read    (P_t *pads, const Pbase_m *m,
			       Pbase_pd *pd, Puint32 *res_out, Pchar stopChar);
Perror_t Pa_timestamp_ME_read (P_t *pads, const Pbase_m *m,
			       Pbase_pd *pd, Puint32 *res_out, const char *matchRegexp);
Perror_t Pa_timestamp_CME_read(P_t *pads, const Pbase_m *m,
			       Pbase_pd *pd, Puint32 *res_out, Pregexp_t *matchRegexp);
Perror_t Pa_timestamp_SE_read (P_t *pads, const Pbase_m *m,
			       Pbase_pd *pd, Puint32 *res_out, const char *stopRegexp);
Perror_t Pa_timestamp_CSE_read(P_t *pads, const Pbase_m *m,
			       Pbase_pd *pd, Puint32 *res_out, Pregexp_t *stopRegexp);

Perror_t Pa_date_FW_read (P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Puint32 *res_out, size_t width);
Perror_t Pa_date_read    (P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Puint32 *res_out, Pchar stopChar);
Perror_t Pa_date_ME_read (P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Puint32 *res_out, const char *matchRegexp);
Perror_t Pa_date_CME_read(P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Puint32 *res_out, Pregexp_t *matchRegexp);
Perror_t Pa_date_SE_read (P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Puint32 *res_out, const char *stopRegexp);
Perror_t Pa_date_CSE_read(P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Puint32 *res_out, Pregexp_t *stopRegexp);

Perror_t Pa_time_FW_read (P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Puint32 *res_out, size_t width);
Perror_t Pa_time_read    (P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Puint32 *res_out, Pchar stopChar);
Perror_t Pa_time_ME_read (P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Puint32 *res_out, const char *matchRegexp);
Perror_t Pa_time_CME_read(P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Puint32 *res_out, Pregexp_t *matchRegexp);
Perror_t Pa_time_SE_read (P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Puint32 *res_out, const char *stopRegexp);
Perror_t Pa_time_CSE_read(P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Puint32 *res_out, Pregexp_t *stopRegexp);
#endif

#if P_CONFIG_E_CHAR_STRING > 0
Perror_t Pe_timestamp_explicit_FW_read (P_t *pads, const Pbase_m *m,
					Pbase_pd *pd, Puint32 *res_out, size_t width,
					const char *format, Tm_zone_t *tzone);
Perror_t Pe_timestamp_explicit_read    (P_t *pads, const Pbase_m *m,
					Pbase_pd *pd, Puint32 *res_out, Pchar stopChar,
					const char *format, Tm_zone_t *tzone);
Perror_t Pe_timestamp_explicit_ME_read (P_t *pads, const Pbase_m *m,
					Pbase_pd *pd, Puint32 *res_out, const char *matchRegexp,
					const char *format, Tm_zone_t *tzone);
Perror_t Pe_timestamp_explicit_CME_read(P_t *pads, const Pbase_m *m,
					Pbase_pd *pd, Puint32 *res_out, Pregexp_t *matchRegexp,
					const char *format, Tm_zone_t *tzone);
Perror_t Pe_timestamp_explicit_SE_read (P_t *pads, const Pbase_m *m,
					Pbase_pd *pd, Puint32 *res_out, const char *stopRegexp,
					const char *format, Tm_zone_t *tzone);
Perror_t Pe_timestamp_explicit_CSE_read(P_t *pads, const Pbase_m *m,
					Pbase_pd *pd, Puint32 *res_out, Pregexp_t *stopRegexp,
					const char *format, Tm_zone_t *tzone);

Perror_t Pe_timestamp_FW_read (P_t *pads, const Pbase_m *m,
			       Pbase_pd *pd, Puint32 *res_out, size_t width);
Perror_t Pe_timestamp_read    (P_t *pads, const Pbase_m *m,
			       Pbase_pd *pd, Puint32 *res_out, Pchar stopChar);
Perror_t Pe_timestamp_ME_read (P_t *pads, const Pbase_m *m,
			       Pbase_pd *pd, Puint32 *res_out, const char *matchRegexp);
Perror_t Pe_timestamp_CME_read(P_t *pads, const Pbase_m *m,
			       Pbase_pd *pd, Puint32 *res_out, Pregexp_t *matchRegexp);
Perror_t Pe_timestamp_SE_read (P_t *pads, const Pbase_m *m,
			       Pbase_pd *pd, Puint32 *res_out, const char *stopRegexp);
Perror_t Pe_timestamp_CSE_read(P_t *pads, const Pbase_m *m,
			       Pbase_pd *pd, Puint32 *res_out, Pregexp_t *stopRegexp);

Perror_t Pe_date_FW_read (P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Puint32 *res_out, size_t width);
Perror_t Pe_date_read    (P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Puint32 *res_out, Pchar stopChar);
Perror_t Pe_date_ME_read (P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Puint32 *res_out, const char *matchRegexp);
Perror_t Pe_date_CME_read(P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Puint32 *res_out, Pregexp_t *matchRegexp);
Perror_t Pe_date_SE_read (P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Puint32 *res_out, const char *stopRegexp);
Perror_t Pe_date_CSE_read(P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Puint32 *res_out, Pregexp_t *stopRegexp);

Perror_t Pe_time_FW_read (P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Puint32 *res_out, size_t width);
Perror_t Pe_time_read    (P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Puint32 *res_out, Pchar stopChar);
Perror_t Pe_time_ME_read (P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Puint32 *res_out, const char *matchRegexp);
Perror_t Pe_time_CME_read(P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Puint32 *res_out, Pregexp_t *matchRegexp);
Perror_t Pe_time_SE_read (P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Puint32 *res_out, const char *stopRegexp);
Perror_t Pe_time_CSE_read(P_t *pads, const Pbase_m *m,
			  Pbase_pd *pd, Puint32 *res_out, Pregexp_t *stopRegexp);
#endif

#if P_CONFIG_A_CHAR_STRING > 0 && P_CONFIG_E_CHAR_STRING > 0
Perror_t Ptimestamp_explicit_FW_read (P_t *pads, const Pbase_m *m,
				      Pbase_pd *pd, Puint32 *res_out, size_t width,
				      const char *format, Tm_zone_t *tzone);
Perror_t Ptimestamp_explicit_read    (P_t *pads, const Pbase_m *m,
				      Pbase_pd *pd, Puint32 *res_out, Pchar stopChar,
				      const char *format, Tm_zone_t *tzone);
Perror_t Ptimestamp_explicit_ME_read (P_t *pads, const Pbase_m *m,
				      Pbase_pd *pd, Puint32 *res_out, const char *matchRegexp,
				      const char *format, Tm_zone_t *tzone);
Perror_t Ptimestamp_explicit_CME_read(P_t *pads, const Pbase_m *m,
				      Pbase_pd *pd, Puint32 *res_out, Pregexp_t *matchRegexp,
				      const char *format, Tm_zone_t *tzone);
Perror_t Ptimestamp_explicit_SE_read (P_t *pads, const Pbase_m *m,
				      Pbase_pd *pd, Puint32 *res_out, const char *stopRegexp,
				      const char *format, Tm_zone_t *tzone);
Perror_t Ptimestamp_explicit_CSE_read(P_t *pads, const Pbase_m *m,
				      Pbase_pd *pd, Puint32 *res_out, Pregexp_t *stopRegexp,
				      const char *format, Tm_zone_t *tzone);

Perror_t Ptimestamp_FW_read (P_t *pads, const Pbase_m *m,
			     Pbase_pd *pd, Puint32 *res_out, size_t width);
Perror_t Ptimestamp_read    (P_t *pads, const Pbase_m *m,
			     Pbase_pd *pd, Puint32 *res_out, Pchar stopChar);
Perror_t Ptimestamp_ME_read (P_t *pads, const Pbase_m *m,
			     Pbase_pd *pd, Puint32 *res_out, const char *matchRegexp);
Perror_t Ptimestamp_CME_read(P_t *pads, const Pbase_m *m,
			     Pbase_pd *pd, Puint32 *res_out, Pregexp_t *matchRegexp);
Perror_t Ptimestamp_SE_read (P_t *pads, const Pbase_m *m,
			     Pbase_pd *pd, Puint32 *res_out, const char *stopRegexp);
Perror_t Ptimestamp_CSE_read(P_t *pads, const Pbase_m *m,
			     Pbase_pd *pd, Puint32 *res_out, Pregexp_t *stopRegexp);

Perror_t Pdate_FW_read (P_t *pads, const Pbase_m *m,
			Pbase_pd *pd, Puint32 *res_out, size_t width);
Perror_t Pdate_read    (P_t *pads, const Pbase_m *m,
			Pbase_pd *pd, Puint32 *res_out, Pchar stopChar);
Perror_t Pdate_ME_read (P_t *pads, const Pbase_m *m,
			Pbase_pd *pd, Puint32 *res_out, const char *matchRegexp);
Perror_t Pdate_CME_read(P_t *pads, const Pbase_m *m,
			Pbase_pd *pd, Puint32 *res_out, Pregexp_t *matchRegexp);
Perror_t Pdate_SE_read (P_t *pads, const Pbase_m *m,
			Pbase_pd *pd, Puint32 *res_out, const char *stopRegexp);
Perror_t Pdate_CSE_read(P_t *pads, const Pbase_m *m,
			Pbase_pd *pd, Puint32 *res_out, Pregexp_t *stopRegexp);

Perror_t Ptime_FW_read (P_t *pads, const Pbase_m *m,
			Pbase_pd *pd, Puint32 *res_out, size_t width);
Perror_t Ptime_read    (P_t *pads, const Pbase_m *m,
			Pbase_pd *pd, Puint32 *res_out, Pchar stopChar);
Perror_t Ptime_ME_read (P_t *pads, const Pbase_m *m,
			Pbase_pd *pd, Puint32 *res_out, const char *matchRegexp);
Perror_t Ptime_CME_read(P_t *pads, const Pbase_m *m,
			Pbase_pd *pd, Puint32 *res_out, Pregexp_t *matchRegexp);
Perror_t Ptime_SE_read (P_t *pads, const Pbase_m *m,
			Pbase_pd *pd, Puint32 *res_out, const char *stopRegexp);
Perror_t Ptime_CSE_read(P_t *pads, const Pbase_m *m,
			Pbase_pd *pd, Puint32 *res_out, Pregexp_t *stopRegexp);
#endif

#endif /* P_CONFIG_READ_FUNCTIONS */
#endif /* FOR_CKIT */

/* ================================================================================
 * WRITE FUNCTIONS
 * DEFAULT                        ASCII                          EBCDIC
 * -----------------------------  -----------------------------  -----------------------------
 * Pdate_FW_write2io              Pa_date_FW_write2io            Pe_date_FW_write2io
 * Pdate_write2io                 Pa_date_write2io               Pe_date_write2io
 * Pdate_ME_write2io              Pa_date_ME_write2io            Pe_date_ME_write2io
 * Pdate_CME_write2io             Pa_date_CME_write2io           Pe_date_CME_write2io
 * Pdate_SE_write2io              Pa_date_SE_write2io            Pe_date_SE_write2io
 * Pdate_CSE_write2io             Pa_date_CSE_write2io           Pe_date_CSE_write2io
 *
 * Pdate_FW_write2buf             Pa_date_FW_write2buf           Pe_date_FW_write2buf
 * Pdate_write2buf                Pa_date_write2buf              Pe_date_write2buf
 * Pdate_ME_write2buf             Pa_date_ME_write2buf           Pe_date_ME_write2buf
 * Pdate_CME_write2buf            Pa_date_CME_write2buf          Pe_date_CME_write2buf
 * Pdate_SE_write2buf             Pa_date_SE_write2buf           Pe_date_SE_write2buf
 * Pdate_CSE_write2buf            Pa_date_CSE_write2buf          Pe_date_CSE_write2buf
 */

#if P_CONFIG_WRITE_FUNCTIONS > 0

/* Ptimestamp_explicit */

#if P_CONFIG_A_CHAR_STRING > 0
#ifdef FOR_CKIT
ssize_t Pa_timestamp_explicit_FW_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, size_t width,
					    const char *format, Tm_zone_t *tzone);
ssize_t Pa_timestamp_explicit_FW_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *d, size_t width,
					    const char *format, Tm_zone_t *tzone);
ssize_t Pa_timestamp_explicit_write2io     (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, Pchar stopChar,
					    const char *format, Tm_zone_t *tzone);
ssize_t Pa_timestamp_explicit_write2buf    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *d, Pchar stopChar,
					    const char *format, Tm_zone_t *tzone);
ssize_t Pa_timestamp_explicit_ME_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *matchRegexp,
					    const char *format, Tm_zone_t *tzone);
ssize_t Pa_timestamp_explicit_ME_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
					    Puint32 *d, const char *matchRegexp,
					    const char *format, Tm_zone_t *tzone);
ssize_t Pa_timestamp_explicit_CME_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, Pregexp_t *matchRegexp,
					    const char *format, Tm_zone_t *tzone);
ssize_t Pa_timestamp_explicit_CME_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
					    Puint32 *d, Pregexp_t *matchRegexp,
					    const char *format, Tm_zone_t *tzone);
ssize_t Pa_timestamp_explicit_SE_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *stopRegexp,
					    const char *format, Tm_zone_t *tzone);
ssize_t Pa_timestamp_explicit_SE_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
					    Puint32 *d, const char *stopRegexp,
					    const char *format, Tm_zone_t *tzone);
ssize_t Pa_timestamp_explicit_CSE_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, Pregexp_t *stopRegexp,
					    const char *format, Tm_zone_t *tzone);
ssize_t Pa_timestamp_explicit_CSE_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
					    Puint32 *d, Pregexp_t *stopRegexp,
					    const char *format, Tm_zone_t *tzone);

ssize_t Pa_timestamp_explicit_FW_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, size_t width,
						 const char *format, Tm_zone_t *tzone);
ssize_t Pa_timestamp_explicit_FW_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
						 Puint32 *d, const char *tag, int indent, size_t width,
						 const char *format, Tm_zone_t *tzone);

ssize_t Pa_timestamp_explicit_write_xml_2io     (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pchar stopChar,
						 const char *format, Tm_zone_t *tzone);
ssize_t Pa_timestamp_explicit_write_xml_2buf    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pchar stopChar,
						 const char *format, Tm_zone_t *tzone);
ssize_t Pa_timestamp_explicit_ME_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, const char *matchRegexp,
						 const char *format, Tm_zone_t *tzone);
ssize_t Pa_timestamp_explicit_ME_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
						 Puint32 *d, const char *tag, int indent, const char *matchRegexp,
						 const char *format, Tm_zone_t *tzone);
ssize_t Pa_timestamp_explicit_CME_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pregexp_t *matchRegexp,
						 const char *format, Tm_zone_t *tzone);
ssize_t Pa_timestamp_explicit_CME_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
						 Puint32 *d, const char *tag, int indent, Pregexp_t *matchRegexp,
						 const char *format, Tm_zone_t *tzone);
ssize_t Pa_timestamp_explicit_SE_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, const char *stopRegexp,
						 const char *format, Tm_zone_t *tzone);
ssize_t Pa_timestamp_explicit_SE_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
						 Puint32 *d, const char *tag, int indent, const char *stopRegexp,
						 const char *format, Tm_zone_t *tzone);
ssize_t Pa_timestamp_explicit_CSE_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pregexp_t *stopRegexp,
						 const char *format, Tm_zone_t *tzone);
ssize_t Pa_timestamp_explicit_CSE_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
						 Puint32 *d, const char *tag, int indent, Pregexp_t *stopRegexp,
						 const char *format, Tm_zone_t *tzone);
#endif /* FOR_CKIT */

ssize_t Pa_timestamp_explicit_FW_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
					  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width,
					  const char *format, Tm_zone_t *tzone);
ssize_t Pa_timestamp_explicit_FW_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
						Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width,
						const char *format, Tm_zone_t *tzone);
ssize_t Pa_timestamp_explicit_fmt2buf    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
					  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar,
					  const char *format, Tm_zone_t *tzone);
ssize_t Pa_timestamp_explicit_fmt2buf_final    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
						Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar,
						const char *format, Tm_zone_t *tzone);
ssize_t Pa_timestamp_explicit_ME_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
					  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *matchRegexp,
					  const char *format, Tm_zone_t *tzone);
ssize_t Pa_timestamp_explicit_ME_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
						Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *matchRegexp,
						const char *format, Tm_zone_t *tzone);
ssize_t Pa_timestamp_explicit_CME_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
					  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *matchRegexp,
					  const char *format, Tm_zone_t *tzone);
ssize_t Pa_timestamp_explicit_CME_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
						Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *matchRegexp,
						const char *format, Tm_zone_t *tzone);
ssize_t Pa_timestamp_explicit_SE_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
					  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *stopRegexp,
					  const char *format, Tm_zone_t *tzone);
ssize_t Pa_timestamp_explicit_SE_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
						Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *stopRegexp,
						const char *format, Tm_zone_t *tzone);
ssize_t Pa_timestamp_explicit_CSE_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
					  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *stopRegexp,
					  const char *format, Tm_zone_t *tzone);
ssize_t Pa_timestamp_explicit_CSE_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
						Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *stopRegexp,
						const char *format, Tm_zone_t *tzone);

ssize_t Pa_timestamp_explicit_FW_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
					  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width,
					  const char *format, Tm_zone_t *tzone);
ssize_t Pa_timestamp_explicit_fmt2io     (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
					  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar,
					  const char *format, Tm_zone_t *tzone);
ssize_t Pa_timestamp_explicit_ME_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
					  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *matchRegexp,
					  const char *format, Tm_zone_t *tzone);
ssize_t Pa_timestamp_explicit_CME_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
					  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *matchRegexp,
					  const char *format, Tm_zone_t *tzone);
ssize_t Pa_timestamp_explicit_SE_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
					  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *stopRegexp,
					  const char *format, Tm_zone_t *tzone);
ssize_t Pa_timestamp_explicit_CSE_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
					  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *stopRegexp,
					  const char *format, Tm_zone_t *tzone);
#endif   /*  P_CONFIG_A_CHAR_STRING > 0  */

#if P_CONFIG_E_CHAR_STRING > 0
#ifdef FOR_CKIT
ssize_t Pe_timestamp_explicit_FW_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, size_t width,
					    const char *format, Tm_zone_t *tzone);
ssize_t Pe_timestamp_explicit_FW_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
					    Puint32 *d, size_t width,
					    const char *format, Tm_zone_t *tzone);
ssize_t Pe_timestamp_explicit_write2io     (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, Pchar stopChar,
					    const char *format, Tm_zone_t *tzone);
ssize_t Pe_timestamp_explicit_write2buf    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *d, Pchar stopChar,
					    const char *format, Tm_zone_t *tzone);
ssize_t Pe_timestamp_explicit_ME_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *matchRegexp,
					    const char *format, Tm_zone_t *tzone);
ssize_t Pe_timestamp_explicit_ME_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
					    Puint32 *d, const char *matchRegexp,
					    const char *format, Tm_zone_t *tzone);
ssize_t Pe_timestamp_explicit_CME_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, Pregexp_t *matchRegexp,
					    const char *format, Tm_zone_t *tzone);
ssize_t Pe_timestamp_explicit_CME_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
					    Puint32 *d, Pregexp_t *matchRegexp,
					    const char *format, Tm_zone_t *tzone);
ssize_t Pe_timestamp_explicit_SE_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *stopRegexp,
					    const char *format, Tm_zone_t *tzone);
ssize_t Pe_timestamp_explicit_SE_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
					    Puint32 *d, const char *stopRegexp,
					    const char *format, Tm_zone_t *tzone);
ssize_t Pe_timestamp_explicit_CSE_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, Pregexp_t *stopRegexp,
					    const char *format, Tm_zone_t *tzone);
ssize_t Pe_timestamp_explicit_CSE_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
					    Puint32 *d, Pregexp_t *stopRegexp,
					    const char *format, Tm_zone_t *tzone);

ssize_t Pe_timestamp_explicit_FW_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, size_t width,
						 const char *format, Tm_zone_t *tzone);
ssize_t Pe_timestamp_explicit_FW_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
						 Puint32 *d, const char *tag, int indent, size_t width,
						 const char *format, Tm_zone_t *tzone);
ssize_t Pe_timestamp_explicit_write_xml_2io     (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pchar stopChar,
						 const char *format, Tm_zone_t *tzone);
ssize_t Pe_timestamp_explicit_write_xml_2buf    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pchar stopChar,
						 const char *format, Tm_zone_t *tzone);
ssize_t Pe_timestamp_explicit_ME_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, const char *matchRegexp,
						 const char *format, Tm_zone_t *tzone);
ssize_t Pe_timestamp_explicit_ME_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
						 Puint32 *d, const char *tag, int indent, const char *matchRegexp,
						 const char *format, Tm_zone_t *tzone);
ssize_t Pe_timestamp_explicit_CME_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pregexp_t *matchRegexp,
						 const char *format, Tm_zone_t *tzone);
ssize_t Pe_timestamp_explicit_CME_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
						 Puint32 *d, const char *tag, int indent, Pregexp_t *matchRegexp,
						 const char *format, Tm_zone_t *tzone);
ssize_t Pe_timestamp_explicit_SE_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, const char *stopRegexp,
						 const char *format, Tm_zone_t *tzone);
ssize_t Pe_timestamp_explicit_SE_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
						 Puint32 *d, const char *tag, int indent, const char *stopRegexp,
						 const char *format, Tm_zone_t *tzone);
ssize_t Pe_timestamp_explicit_CSE_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pregexp_t *stopRegexp,
						 const char *format, Tm_zone_t *tzone);
ssize_t Pe_timestamp_explicit_CSE_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
						 Puint32 *d, const char *tag, int indent, Pregexp_t *stopRegexp,
						 const char *format, Tm_zone_t *tzone);
#endif /* FOR_CKIT */

ssize_t Pe_timestamp_explicit_FW_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
					  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width,
					  const char *format, Tm_zone_t *tzone);
ssize_t Pe_timestamp_explicit_FW_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
						Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width,
						const char *format, Tm_zone_t *tzone);
ssize_t Pe_timestamp_explicit_fmt2buf    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
					  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar,
					  const char *format, Tm_zone_t *tzone);
ssize_t Pe_timestamp_explicit_fmt2buf_final    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
						Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar,
						const char *format, Tm_zone_t *tzone);
ssize_t Pe_timestamp_explicit_ME_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
					  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *matchRegexp,
					  const char *format, Tm_zone_t *tzone);
ssize_t Pe_timestamp_explicit_ME_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
						Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *matchRegexp,
						const char *format, Tm_zone_t *tzone);
ssize_t Pe_timestamp_explicit_CME_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
					  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *matchRegexp,
					  const char *format, Tm_zone_t *tzone);
ssize_t Pe_timestamp_explicit_CME_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
						Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *matchRegexp,
						const char *format, Tm_zone_t *tzone);
ssize_t Pe_timestamp_explicit_SE_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
					  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *stopRegexp,
					  const char *format, Tm_zone_t *tzone);
ssize_t Pe_timestamp_explicit_SE_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
						Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *stopRegexp,
						const char *format, Tm_zone_t *tzone);
ssize_t Pe_timestamp_explicit_CSE_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
					  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *stopRegexp,
					  const char *format, Tm_zone_t *tzone);
ssize_t Pe_timestamp_explicit_CSE_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
						Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *stopRegexp,
						const char *format, Tm_zone_t *tzone);

ssize_t Pe_timestamp_explicit_FW_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
					  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width,
					  const char *format, Tm_zone_t *tzone);
ssize_t Pe_timestamp_explicit_fmt2io     (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
					  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar,
					  const char *format, Tm_zone_t *tzone);
ssize_t Pe_timestamp_explicit_ME_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
					  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *matchRegexp,
					  const char *format, Tm_zone_t *tzone);
ssize_t Pe_timestamp_explicit_CME_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
					  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *matchRegexp,
					  const char *format, Tm_zone_t *tzone);
ssize_t Pe_timestamp_explicit_SE_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
					  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *stopRegexp,
					  const char *format, Tm_zone_t *tzone);
ssize_t Pe_timestamp_explicit_CSE_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
					  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *stopRegexp,
					  const char *format, Tm_zone_t *tzone);
#endif  /*  P_CONFIG_E_CHAR_STRING > 0  */

#if P_CONFIG_A_CHAR_STRING > 0 && P_CONFIG_E_CHAR_STRING > 0
#ifdef FOR_CKIT
ssize_t Ptimestamp_explicit_FW_write2io    (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, size_t width,
					    const char *format, Tm_zone_t *tzone);
ssize_t Ptimestamp_explicit_FW_write2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
					    Puint32 *d, size_t width,
					    const char *format, Tm_zone_t *tzone);
ssize_t Ptimestamp_explicit_write2io       (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, Pchar stopChar,
					    const char *format, Tm_zone_t *tzone);
ssize_t Ptimestamp_explicit_write2buf      (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *d, Pchar stopChar,
					    const char *format, Tm_zone_t *tzone);
ssize_t Ptimestamp_explicit_ME_write2io    (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *matchRegexp,
					    const char *format, Tm_zone_t *tzone);
ssize_t Ptimestamp_explicit_ME_write2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
					    Puint32 *d, const char *matchRegexp,
					    const char *format, Tm_zone_t *tzone);
ssize_t Ptimestamp_explicit_CME_write2io   (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, Pregexp_t *matchRegexp,
					    const char *format, Tm_zone_t *tzone);
ssize_t Ptimestamp_explicit_CME_write2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
					    Puint32 *d, Pregexp_t *matchRegexp,
					    const char *format, Tm_zone_t *tzone);
ssize_t Ptimestamp_explicit_SE_write2io    (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *stopRegexp,
					    const char *format, Tm_zone_t *tzone);
ssize_t Ptimestamp_explicit_SE_write2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
					    Puint32 *d, const char *stopRegexp,
					    const char *format, Tm_zone_t *tzone);
ssize_t Ptimestamp_explicit_CSE_write2io   (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, Pregexp_t *stopRegexp,
					    const char *format, Tm_zone_t *tzone);
ssize_t Ptimestamp_explicit_CSE_write2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
					    Puint32 *d, Pregexp_t *stopRegexp,
					    const char *format, Tm_zone_t *tzone);

ssize_t Ptimestamp_explicit_FW_write_xml_2io    (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, size_t width,
						 const char *format, Tm_zone_t *tzone);
ssize_t Ptimestamp_explicit_FW_write_xml_2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
						 Puint32 *d, const char *tag, int indent, size_t width,
						 const char *format, Tm_zone_t *tzone);
ssize_t Ptimestamp_explicit_write_xml_2io       (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pchar stopChar,
						 const char *format, Tm_zone_t *tzone);
ssize_t Ptimestamp_explicit_write_xml_2buf      (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pchar stopChar,
						 const char *format, Tm_zone_t *tzone);
ssize_t Ptimestamp_explicit_ME_write_xml_2io    (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, const char *matchRegexp,
						 const char *format, Tm_zone_t *tzone);
ssize_t Ptimestamp_explicit_ME_write_xml_2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
						 Puint32 *d, const char *tag, int indent, const char *matchRegexp,
						 const char *format, Tm_zone_t *tzone);
ssize_t Ptimestamp_explicit_CME_write_xml_2io   (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pregexp_t *matchRegexp,
						 const char *format, Tm_zone_t *tzone);
ssize_t Ptimestamp_explicit_CME_write_xml_2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
						 Puint32 *d, const char *tag, int indent, Pregexp_t *matchRegexp,
						 const char *format, Tm_zone_t *tzone);
ssize_t Ptimestamp_explicit_SE_write_xml_2io    (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, const char *stopRegexp,
						 const char *format, Tm_zone_t *tzone);
ssize_t Ptimestamp_explicit_SE_write_xml_2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
						 Puint32 *d, const char *tag, int indent, const char *stopRegexp,
						 const char *format, Tm_zone_t *tzone);
ssize_t Ptimestamp_explicit_CSE_write_xml_2io   (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pregexp_t *stopRegexp,
						 const char *format, Tm_zone_t *tzone);
ssize_t Ptimestamp_explicit_CSE_write_xml_2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
						 Puint32 *d, const char *tag, int indent, Pregexp_t *stopRegexp,
						 const char *format, Tm_zone_t *tzone);
#endif /* FOR_CKIT */

ssize_t Ptimestamp_explicit_FW_fmt2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
					  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width,
					  const char *format, Tm_zone_t *tzone);
ssize_t Ptimestamp_explicit_FW_fmt2buf_final   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
						Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width,
						const char *format, Tm_zone_t *tzone);
ssize_t Ptimestamp_explicit_fmt2buf      (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
					  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar,
					  const char *format, Tm_zone_t *tzone);
ssize_t Ptimestamp_explicit_fmt2buf_final      (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
						Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar,
						const char *format, Tm_zone_t *tzone);
ssize_t Ptimestamp_explicit_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
					  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar,
					  const char *format, Tm_zone_t *tzone);
ssize_t Ptimestamp_explicit_fmt2buf_final_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
						Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar,
						const char *format, Tm_zone_t *tzone);
ssize_t Ptimestamp_explicit_ME_fmt2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
					  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *matchRegexp,
					  const char *format, Tm_zone_t *tzone);
ssize_t Ptimestamp_explicit_ME_fmt2buf_final   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
						Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *matchRegexp,
						const char *format, Tm_zone_t *tzone);
ssize_t Ptimestamp_explicit_CME_fmt2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
					  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *matchRegexp,
					  const char *format, Tm_zone_t *tzone);
ssize_t Ptimestamp_explicit_CME_fmt2buf_final  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
						Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *matchRegexp,
						const char *format, Tm_zone_t *tzone);
ssize_t Ptimestamp_explicit_SE_fmt2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
					  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *stopRegexp,
					  const char *format, Tm_zone_t *tzone);
ssize_t Ptimestamp_explicit_SE_fmt2buf_final   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
						Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *stopRegexp,
						const char *format, Tm_zone_t *tzone);
ssize_t Ptimestamp_explicit_CSE_fmt2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
					  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *stopRegexp,
					  const char *format, Tm_zone_t *tzone);
ssize_t Ptimestamp_explicit_CSE_fmt2buf_final  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
						Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *stopRegexp,
						const char *format, Tm_zone_t *tzone);

ssize_t Ptimestamp_explicit_FW_fmt2io    (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
					  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width,
					  const char *format, Tm_zone_t *tzone);
ssize_t Ptimestamp_explicit_fmt2io       (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
					  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar,
					  const char *format, Tm_zone_t *tzone);
ssize_t Ptimestamp_explicit_ME_fmt2io    (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
					  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *matchRegexp,
					  const char *format, Tm_zone_t *tzone);
ssize_t Ptimestamp_explicit_CME_fmt2io   (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
					  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *matchRegexp,
					  const char *format, Tm_zone_t *tzone);
ssize_t Ptimestamp_explicit_SE_fmt2io    (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
					  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *stopRegexp,
					  const char *format, Tm_zone_t *tzone);
ssize_t Ptimestamp_explicit_CSE_fmt2io   (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
					  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *stopRegexp,
					  const char *format, Tm_zone_t *tzone);
#endif /*  P_CONFIG_A_CHAR_STRING > 0 && P_CONFIG_E_CHAR_STRING > 0  */

/* Ptimestamp */

#if P_CONFIG_A_CHAR_STRING > 0
#ifdef FOR_CKIT
ssize_t Pa_timestamp_FW_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, size_t width);
ssize_t Pa_timestamp_FW_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *d, size_t width);
ssize_t Pa_timestamp_write2io     (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, Pchar stopChar);
ssize_t Pa_timestamp_write2buf    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *d, Pchar stopChar);
ssize_t Pa_timestamp_ME_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *matchRegexp);
ssize_t Pa_timestamp_ME_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, const char *matchRegexp);
ssize_t Pa_timestamp_CME_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, Pregexp_t *matchRegexp);
ssize_t Pa_timestamp_CME_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, Pregexp_t *matchRegexp);
ssize_t Pa_timestamp_SE_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *stopRegexp);
ssize_t Pa_timestamp_SE_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, const char *stopRegexp);
ssize_t Pa_timestamp_CSE_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, Pregexp_t *stopRegexp);
ssize_t Pa_timestamp_CSE_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, Pregexp_t *stopRegexp);

ssize_t Pa_timestamp_FW_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, size_t width);
ssize_t Pa_timestamp_FW_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, size_t width);

ssize_t Pa_timestamp_write_xml_2io     (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pchar stopChar);
ssize_t Pa_timestamp_write_xml_2buf    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pchar stopChar);
ssize_t Pa_timestamp_ME_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, const char *matchRegexp);
ssize_t Pa_timestamp_ME_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, const char *matchRegexp);
ssize_t Pa_timestamp_CME_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pregexp_t *matchRegexp);
ssize_t Pa_timestamp_CME_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, Pregexp_t *matchRegexp);
ssize_t Pa_timestamp_SE_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, const char *stopRegexp);
ssize_t Pa_timestamp_SE_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, const char *stopRegexp);
ssize_t Pa_timestamp_CSE_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pregexp_t *stopRegexp);
ssize_t Pa_timestamp_CSE_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, Pregexp_t *stopRegexp);
#endif /* FOR_CKIT */

ssize_t Pa_timestamp_FW_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width);
ssize_t Pa_timestamp_FW_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width);
ssize_t Pa_timestamp_fmt2buf    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar);
ssize_t Pa_timestamp_fmt2buf_final    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar);
ssize_t Pa_timestamp_ME_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *matchRegexp);
ssize_t Pa_timestamp_ME_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *matchRegexp);
ssize_t Pa_timestamp_CME_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *matchRegexp);
ssize_t Pa_timestamp_CME_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *matchRegexp);
ssize_t Pa_timestamp_SE_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *stopRegexp);
ssize_t Pa_timestamp_SE_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *stopRegexp);
ssize_t Pa_timestamp_CSE_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *stopRegexp);
ssize_t Pa_timestamp_CSE_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *stopRegexp);

ssize_t Pa_timestamp_FW_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width);
ssize_t Pa_timestamp_fmt2io     (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar);
ssize_t Pa_timestamp_ME_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *matchRegexp);
ssize_t Pa_timestamp_CME_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *matchRegexp);
ssize_t Pa_timestamp_SE_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *stopRegexp);
ssize_t Pa_timestamp_CSE_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *stopRegexp);
#endif   /*  P_CONFIG_A_CHAR_STRING > 0  */

#if P_CONFIG_E_CHAR_STRING > 0
#ifdef FOR_CKIT
ssize_t Pe_timestamp_FW_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, size_t width);
ssize_t Pe_timestamp_FW_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, size_t width);
ssize_t Pe_timestamp_write2io     (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, Pchar stopChar);
ssize_t Pe_timestamp_write2buf    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *d, Pchar stopChar);
ssize_t Pe_timestamp_ME_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *matchRegexp);
ssize_t Pe_timestamp_ME_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, const char *matchRegexp);
ssize_t Pe_timestamp_CME_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, Pregexp_t *matchRegexp);
ssize_t Pe_timestamp_CME_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, Pregexp_t *matchRegexp);
ssize_t Pe_timestamp_SE_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *stopRegexp);
ssize_t Pe_timestamp_SE_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, const char *stopRegexp);
ssize_t Pe_timestamp_CSE_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, Pregexp_t *stopRegexp);
ssize_t Pe_timestamp_CSE_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, Pregexp_t *stopRegexp);

ssize_t Pe_timestamp_FW_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, size_t width);
ssize_t Pe_timestamp_FW_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, size_t width);
ssize_t Pe_timestamp_write_xml_2io     (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pchar stopChar);
ssize_t Pe_timestamp_write_xml_2buf    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pchar stopChar);
ssize_t Pe_timestamp_ME_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, const char *matchRegexp);
ssize_t Pe_timestamp_ME_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, const char *matchRegexp);
ssize_t Pe_timestamp_CME_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pregexp_t *matchRegexp);
ssize_t Pe_timestamp_CME_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, Pregexp_t *matchRegexp);
ssize_t Pe_timestamp_SE_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, const char *stopRegexp);
ssize_t Pe_timestamp_SE_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, const char *stopRegexp);
ssize_t Pe_timestamp_CSE_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pregexp_t *stopRegexp);
ssize_t Pe_timestamp_CSE_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, Pregexp_t *stopRegexp);
#endif /* FOR_CKIT */

ssize_t Pe_timestamp_FW_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width);
ssize_t Pe_timestamp_FW_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width);
ssize_t Pe_timestamp_fmt2buf    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar);
ssize_t Pe_timestamp_fmt2buf_final    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar);
ssize_t Pe_timestamp_ME_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *matchRegexp);
ssize_t Pe_timestamp_ME_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *matchRegexp);
ssize_t Pe_timestamp_CME_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *matchRegexp);
ssize_t Pe_timestamp_CME_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *matchRegexp);
ssize_t Pe_timestamp_SE_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *stopRegexp);
ssize_t Pe_timestamp_SE_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *stopRegexp);
ssize_t Pe_timestamp_CSE_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *stopRegexp);
ssize_t Pe_timestamp_CSE_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *stopRegexp);

ssize_t Pe_timestamp_FW_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width);
ssize_t Pe_timestamp_fmt2io     (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar);
ssize_t Pe_timestamp_ME_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *matchRegexp);
ssize_t Pe_timestamp_CME_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *matchRegexp);
ssize_t Pe_timestamp_SE_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *stopRegexp);
ssize_t Pe_timestamp_CSE_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *stopRegexp);
#endif  /*  P_CONFIG_E_CHAR_STRING > 0  */

#if P_CONFIG_A_CHAR_STRING > 0 && P_CONFIG_E_CHAR_STRING > 0
#ifdef FOR_CKIT
ssize_t Ptimestamp_FW_write2io    (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, size_t width);
ssize_t Ptimestamp_FW_write2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, size_t width);
ssize_t Ptimestamp_write2io       (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, Pchar stopChar);
ssize_t Ptimestamp_write2buf      (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *d, Pchar stopChar);
ssize_t Ptimestamp_ME_write2io    (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *matchRegexp);
ssize_t Ptimestamp_ME_write2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, const char *matchRegexp);
ssize_t Ptimestamp_CME_write2io   (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, Pregexp_t *matchRegexp);
ssize_t Ptimestamp_CME_write2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, Pregexp_t *matchRegexp);
ssize_t Ptimestamp_SE_write2io    (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *stopRegexp);
ssize_t Ptimestamp_SE_write2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, const char *stopRegexp);
ssize_t Ptimestamp_CSE_write2io   (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, Pregexp_t *stopRegexp);
ssize_t Ptimestamp_CSE_write2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, Pregexp_t *stopRegexp);

ssize_t Ptimestamp_FW_write_xml_2io    (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, size_t width);
ssize_t Ptimestamp_FW_write_xml_2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, size_t width);
ssize_t Ptimestamp_write_xml_2io       (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pchar stopChar);
ssize_t Ptimestamp_write_xml_2buf      (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pchar stopChar);
ssize_t Ptimestamp_ME_write_xml_2io    (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, const char *matchRegexp);
ssize_t Ptimestamp_ME_write_xml_2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, const char *matchRegexp);
ssize_t Ptimestamp_CME_write_xml_2io   (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pregexp_t *matchRegexp);
ssize_t Ptimestamp_CME_write_xml_2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, Pregexp_t *matchRegexp);
ssize_t Ptimestamp_SE_write_xml_2io    (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, const char *stopRegexp);
ssize_t Ptimestamp_SE_write_xml_2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, const char *stopRegexp);
ssize_t Ptimestamp_CSE_write_xml_2io   (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pregexp_t *stopRegexp);
ssize_t Ptimestamp_CSE_write_xml_2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, Pregexp_t *stopRegexp);
#endif /* FOR_CKIT */

ssize_t Ptimestamp_FW_fmt2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width);
ssize_t Ptimestamp_FW_fmt2buf_final   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width);
ssize_t Ptimestamp_fmt2buf      (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar);
ssize_t Ptimestamp_fmt2buf_final      (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar);
ssize_t Ptimestamp_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar);
ssize_t Ptimestamp_fmt2buf_final_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar);
ssize_t Ptimestamp_ME_fmt2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *matchRegexp);
ssize_t Ptimestamp_ME_fmt2buf_final   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *matchRegexp);
ssize_t Ptimestamp_CME_fmt2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *matchRegexp);
ssize_t Ptimestamp_CME_fmt2buf_final  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *matchRegexp);
ssize_t Ptimestamp_SE_fmt2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *stopRegexp);
ssize_t Ptimestamp_SE_fmt2buf_final   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *stopRegexp);
ssize_t Ptimestamp_CSE_fmt2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *stopRegexp);
ssize_t Ptimestamp_CSE_fmt2buf_final  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *stopRegexp);

ssize_t Ptimestamp_FW_fmt2io    (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width);
ssize_t Ptimestamp_fmt2io       (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar);
ssize_t Ptimestamp_ME_fmt2io    (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *matchRegexp);
ssize_t Ptimestamp_CME_fmt2io   (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *matchRegexp);
ssize_t Ptimestamp_SE_fmt2io    (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *stopRegexp);
ssize_t Ptimestamp_CSE_fmt2io   (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *stopRegexp);
#endif /*  P_CONFIG_A_CHAR_STRING > 0 && P_CONFIG_E_CHAR_STRING > 0  */

/* Pdate */

#if P_CONFIG_A_CHAR_STRING > 0
#ifdef FOR_CKIT
ssize_t Pa_date_FW_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, size_t width);
ssize_t Pa_date_FW_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *d, size_t width);
ssize_t Pa_date_write2io     (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, Pchar stopChar);
ssize_t Pa_date_write2buf    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *d, Pchar stopChar);
ssize_t Pa_date_ME_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *matchRegexp);
ssize_t Pa_date_ME_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, const char *matchRegexp);
ssize_t Pa_date_CME_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, Pregexp_t *matchRegexp);
ssize_t Pa_date_CME_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, Pregexp_t *matchRegexp);
ssize_t Pa_date_SE_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *stopRegexp);
ssize_t Pa_date_SE_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, const char *stopRegexp);
ssize_t Pa_date_CSE_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, Pregexp_t *stopRegexp);
ssize_t Pa_date_CSE_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, Pregexp_t *stopRegexp);

ssize_t Pa_date_FW_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, size_t width);
ssize_t Pa_date_FW_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, size_t width);

ssize_t Pa_date_write_xml_2io     (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pchar stopChar);
ssize_t Pa_date_write_xml_2buf    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pchar stopChar);
ssize_t Pa_date_ME_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, const char *matchRegexp);
ssize_t Pa_date_ME_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, const char *matchRegexp);
ssize_t Pa_date_CME_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pregexp_t *matchRegexp);
ssize_t Pa_date_CME_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, Pregexp_t *matchRegexp);
ssize_t Pa_date_SE_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, const char *stopRegexp);
ssize_t Pa_date_SE_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, const char *stopRegexp);
ssize_t Pa_date_CSE_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pregexp_t *stopRegexp);
ssize_t Pa_date_CSE_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, Pregexp_t *stopRegexp);
#endif /* FOR_CKIT */

ssize_t Pa_date_FW_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width);
ssize_t Pa_date_FW_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width);
ssize_t Pa_date_fmt2buf    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar);
ssize_t Pa_date_fmt2buf_final    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar);
ssize_t Pa_date_ME_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *matchRegexp);
ssize_t Pa_date_ME_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *matchRegexp);
ssize_t Pa_date_CME_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *matchRegexp);
ssize_t Pa_date_CME_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *matchRegexp);
ssize_t Pa_date_SE_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *stopRegexp);
ssize_t Pa_date_SE_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *stopRegexp);
ssize_t Pa_date_CSE_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *stopRegexp);
ssize_t Pa_date_CSE_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *stopRegexp);

ssize_t Pa_date_FW_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width);
ssize_t Pa_date_fmt2io     (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar);
ssize_t Pa_date_ME_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *matchRegexp);
ssize_t Pa_date_CME_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *matchRegexp);
ssize_t Pa_date_SE_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *stopRegexp);
ssize_t Pa_date_CSE_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *stopRegexp);
#endif   /*  P_CONFIG_A_CHAR_STRING > 0  */

#if P_CONFIG_E_CHAR_STRING > 0
#ifdef FOR_CKIT
ssize_t Pe_date_FW_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, size_t width);
ssize_t Pe_date_FW_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, size_t width);
ssize_t Pe_date_write2io     (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, Pchar stopChar);
ssize_t Pe_date_write2buf    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *d, Pchar stopChar);
ssize_t Pe_date_ME_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *matchRegexp);
ssize_t Pe_date_ME_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, const char *matchRegexp);
ssize_t Pe_date_CME_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, Pregexp_t *matchRegexp);
ssize_t Pe_date_CME_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, Pregexp_t *matchRegexp);
ssize_t Pe_date_SE_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *stopRegexp);
ssize_t Pe_date_SE_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, const char *stopRegexp);
ssize_t Pe_date_CSE_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, Pregexp_t *stopRegexp);
ssize_t Pe_date_CSE_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, Pregexp_t *stopRegexp);

ssize_t Pe_date_FW_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, size_t width);
ssize_t Pe_date_FW_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, size_t width);
ssize_t Pe_date_write_xml_2io     (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pchar stopChar);
ssize_t Pe_date_write_xml_2buf    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pchar stopChar);
ssize_t Pe_date_ME_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, const char *matchRegexp);
ssize_t Pe_date_ME_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, const char *matchRegexp);
ssize_t Pe_date_CME_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pregexp_t *matchRegexp);
ssize_t Pe_date_CME_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, Pregexp_t *matchRegexp);
ssize_t Pe_date_SE_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, const char *stopRegexp);
ssize_t Pe_date_SE_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, const char *stopRegexp);
ssize_t Pe_date_CSE_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pregexp_t *stopRegexp);
ssize_t Pe_date_CSE_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, Pregexp_t *stopRegexp);
#endif /* FOR_CKIT */

ssize_t Pe_date_FW_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width);
ssize_t Pe_date_FW_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width);
ssize_t Pe_date_fmt2buf    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar);
ssize_t Pe_date_fmt2buf_final    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar);
ssize_t Pe_date_ME_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *matchRegexp);
ssize_t Pe_date_ME_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *matchRegexp);
ssize_t Pe_date_CME_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *matchRegexp);
ssize_t Pe_date_CME_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *matchRegexp);
ssize_t Pe_date_SE_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *stopRegexp);
ssize_t Pe_date_SE_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *stopRegexp);
ssize_t Pe_date_CSE_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *stopRegexp);
ssize_t Pe_date_CSE_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *stopRegexp);

ssize_t Pe_date_FW_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width);
ssize_t Pe_date_fmt2io     (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar);
ssize_t Pe_date_ME_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *matchRegexp);
ssize_t Pe_date_CME_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *matchRegexp);
ssize_t Pe_date_SE_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *stopRegexp);
ssize_t Pe_date_CSE_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *stopRegexp);
#endif  /*  P_CONFIG_E_CHAR_STRING > 0  */

#if P_CONFIG_A_CHAR_STRING > 0 && P_CONFIG_E_CHAR_STRING > 0
#ifdef FOR_CKIT
ssize_t Pdate_FW_write2io    (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, size_t width);
ssize_t Pdate_FW_write2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, size_t width);
ssize_t Pdate_write2io       (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, Pchar stopChar);
ssize_t Pdate_write2buf      (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *d, Pchar stopChar);
ssize_t Pdate_ME_write2io    (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *matchRegexp);
ssize_t Pdate_ME_write2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, const char *matchRegexp);
ssize_t Pdate_CME_write2io   (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, Pregexp_t *matchRegexp);
ssize_t Pdate_CME_write2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, Pregexp_t *matchRegexp);
ssize_t Pdate_SE_write2io    (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *stopRegexp);
ssize_t Pdate_SE_write2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, const char *stopRegexp);
ssize_t Pdate_CSE_write2io   (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, Pregexp_t *stopRegexp);
ssize_t Pdate_CSE_write2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, Pregexp_t *stopRegexp);

ssize_t Pdate_FW_write_xml_2io    (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, size_t width);
ssize_t Pdate_FW_write_xml_2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, size_t width);
ssize_t Pdate_write_xml_2io       (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pchar stopChar);
ssize_t Pdate_write_xml_2buf      (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pchar stopChar);
ssize_t Pdate_ME_write_xml_2io    (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, const char *matchRegexp);
ssize_t Pdate_ME_write_xml_2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, const char *matchRegexp);
ssize_t Pdate_CME_write_xml_2io   (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pregexp_t *matchRegexp);
ssize_t Pdate_CME_write_xml_2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, Pregexp_t *matchRegexp);
ssize_t Pdate_SE_write_xml_2io    (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, const char *stopRegexp);
ssize_t Pdate_SE_write_xml_2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, const char *stopRegexp);
ssize_t Pdate_CSE_write_xml_2io   (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pregexp_t *stopRegexp);
ssize_t Pdate_CSE_write_xml_2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, Pregexp_t *stopRegexp);
#endif /* FOR_CKIT */

ssize_t Pdate_FW_fmt2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width);
ssize_t Pdate_FW_fmt2buf_final   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width);
ssize_t Pdate_fmt2buf      (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar);
ssize_t Pdate_fmt2buf_final      (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar);
ssize_t Pdate_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar);
ssize_t Pdate_fmt2buf_final_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar);
ssize_t Pdate_ME_fmt2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *matchRegexp);
ssize_t Pdate_ME_fmt2buf_final   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *matchRegexp);
ssize_t Pdate_CME_fmt2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *matchRegexp);
ssize_t Pdate_CME_fmt2buf_final  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *matchRegexp);
ssize_t Pdate_SE_fmt2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *stopRegexp);
ssize_t Pdate_SE_fmt2buf_final   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *stopRegexp);
ssize_t Pdate_CSE_fmt2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *stopRegexp);
ssize_t Pdate_CSE_fmt2buf_final  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *stopRegexp);

ssize_t Pdate_FW_fmt2io    (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width);
ssize_t Pdate_fmt2io       (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar);
ssize_t Pdate_ME_fmt2io    (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *matchRegexp);
ssize_t Pdate_CME_fmt2io   (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *matchRegexp);
ssize_t Pdate_SE_fmt2io    (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *stopRegexp);
ssize_t Pdate_CSE_fmt2io   (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *stopRegexp);
#endif /*  P_CONFIG_A_CHAR_STRING > 0 && P_CONFIG_E_CHAR_STRING > 0  */

/* Ptime */

#if P_CONFIG_A_CHAR_STRING > 0
#ifdef FOR_CKIT
ssize_t Pa_time_FW_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, size_t width);
ssize_t Pa_time_FW_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *d, size_t width);
ssize_t Pa_time_write2io     (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, Pchar stopChar);
ssize_t Pa_time_write2buf    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *d, Pchar stopChar);
ssize_t Pa_time_ME_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *matchRegexp);
ssize_t Pa_time_ME_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, const char *matchRegexp);
ssize_t Pa_time_CME_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, Pregexp_t *matchRegexp);
ssize_t Pa_time_CME_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, Pregexp_t *matchRegexp);
ssize_t Pa_time_SE_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *stopRegexp);
ssize_t Pa_time_SE_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, const char *stopRegexp);
ssize_t Pa_time_CSE_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, Pregexp_t *stopRegexp);
ssize_t Pa_time_CSE_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, Pregexp_t *stopRegexp);

ssize_t Pa_time_FW_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, size_t width);
ssize_t Pa_time_FW_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, size_t width);

ssize_t Pa_time_write_xml_2io     (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pchar stopChar);
ssize_t Pa_time_write_xml_2buf    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pchar stopChar);
ssize_t Pa_time_ME_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, const char *matchRegexp);
ssize_t Pa_time_ME_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, const char *matchRegexp);
ssize_t Pa_time_CME_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pregexp_t *matchRegexp);
ssize_t Pa_time_CME_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, Pregexp_t *matchRegexp);
ssize_t Pa_time_SE_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, const char *stopRegexp);
ssize_t Pa_time_SE_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, const char *stopRegexp);
ssize_t Pa_time_CSE_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pregexp_t *stopRegexp);
ssize_t Pa_time_CSE_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, Pregexp_t *stopRegexp);
#endif /* FOR_CKIT */

ssize_t Pa_time_FW_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width);
ssize_t Pa_time_FW_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width);
ssize_t Pa_time_fmt2buf    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar);
ssize_t Pa_time_fmt2buf_final    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar);
ssize_t Pa_time_ME_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *matchRegexp);
ssize_t Pa_time_ME_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *matchRegexp);
ssize_t Pa_time_CME_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *matchRegexp);
ssize_t Pa_time_CME_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *matchRegexp);
ssize_t Pa_time_SE_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *stopRegexp);
ssize_t Pa_time_SE_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *stopRegexp);
ssize_t Pa_time_CSE_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *stopRegexp);
ssize_t Pa_time_CSE_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *stopRegexp);

ssize_t Pa_time_FW_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width);
ssize_t Pa_time_fmt2io     (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar);
ssize_t Pa_time_ME_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *matchRegexp);
ssize_t Pa_time_CME_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *matchRegexp);
ssize_t Pa_time_SE_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *stopRegexp);
ssize_t Pa_time_CSE_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *stopRegexp);
#endif   /*  P_CONFIG_A_CHAR_STRING > 0  */

#if P_CONFIG_E_CHAR_STRING > 0
#ifdef FOR_CKIT
ssize_t Pe_time_FW_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, size_t width);
ssize_t Pe_time_FW_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, size_t width);
ssize_t Pe_time_write2io     (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, Pchar stopChar);
ssize_t Pe_time_write2buf    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *d, Pchar stopChar);
ssize_t Pe_time_ME_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *matchRegexp);
ssize_t Pe_time_ME_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, const char *matchRegexp);
ssize_t Pe_time_CME_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, Pregexp_t *matchRegexp);
ssize_t Pe_time_CME_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, Pregexp_t *matchRegexp);
ssize_t Pe_time_SE_write2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *stopRegexp);
ssize_t Pe_time_SE_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, const char *stopRegexp);
ssize_t Pe_time_CSE_write2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, Pregexp_t *stopRegexp);
ssize_t Pe_time_CSE_write2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, Pregexp_t *stopRegexp);

ssize_t Pe_time_FW_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, size_t width);
ssize_t Pe_time_FW_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, size_t width);
ssize_t Pe_time_write_xml_2io     (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pchar stopChar);
ssize_t Pe_time_write_xml_2buf    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pchar stopChar);
ssize_t Pe_time_ME_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, const char *matchRegexp);
ssize_t Pe_time_ME_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, const char *matchRegexp);
ssize_t Pe_time_CME_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pregexp_t *matchRegexp);
ssize_t Pe_time_CME_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, Pregexp_t *matchRegexp);
ssize_t Pe_time_SE_write_xml_2io  (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, const char *stopRegexp);
ssize_t Pe_time_SE_write_xml_2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, const char *stopRegexp);
ssize_t Pe_time_CSE_write_xml_2io (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pregexp_t *stopRegexp);
ssize_t Pe_time_CSE_write_xml_2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, Pregexp_t *stopRegexp);
#endif /* FOR_CKIT */

ssize_t Pe_time_FW_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width);
ssize_t Pe_time_FW_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width);
ssize_t Pe_time_fmt2buf    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar);
ssize_t Pe_time_fmt2buf_final    (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar);
ssize_t Pe_time_ME_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *matchRegexp);
ssize_t Pe_time_ME_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *matchRegexp);
ssize_t Pe_time_CME_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *matchRegexp);
ssize_t Pe_time_CME_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *matchRegexp);
ssize_t Pe_time_SE_fmt2buf (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *stopRegexp);
ssize_t Pe_time_SE_fmt2buf_final (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *stopRegexp);
ssize_t Pe_time_CSE_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *stopRegexp);
ssize_t Pe_time_CSE_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *stopRegexp);

ssize_t Pe_time_FW_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width);
ssize_t Pe_time_fmt2io     (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar);
ssize_t Pe_time_ME_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *matchRegexp);
ssize_t Pe_time_CME_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *matchRegexp);
ssize_t Pe_time_SE_fmt2io  (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *stopRegexp);
ssize_t Pe_time_CSE_fmt2io (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *stopRegexp);
#endif  /*  P_CONFIG_E_CHAR_STRING > 0  */

#if P_CONFIG_A_CHAR_STRING > 0 && P_CONFIG_E_CHAR_STRING > 0
#ifdef FOR_CKIT
ssize_t Ptime_FW_write2io    (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, size_t width);
ssize_t Ptime_FW_write2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, size_t width);
ssize_t Ptime_write2io       (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, Pchar stopChar);
ssize_t Ptime_write2buf      (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *d, Pchar stopChar);
ssize_t Ptime_ME_write2io    (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *matchRegexp);
ssize_t Ptime_ME_write2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, const char *matchRegexp);
ssize_t Ptime_CME_write2io   (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, Pregexp_t *matchRegexp);
ssize_t Ptime_CME_write2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, Pregexp_t *matchRegexp);
ssize_t Ptime_SE_write2io    (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *stopRegexp);
ssize_t Ptime_SE_write2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, const char *stopRegexp);
ssize_t Ptime_CSE_write2io   (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, Pregexp_t *stopRegexp);
ssize_t Ptime_CSE_write2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
			      Puint32 *d, Pregexp_t *stopRegexp);

ssize_t Ptime_FW_write_xml_2io    (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, size_t width);
ssize_t Ptime_FW_write_xml_2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, size_t width);
ssize_t Ptime_write_xml_2io       (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pchar stopChar);
ssize_t Ptime_write_xml_2buf      (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pchar stopChar);
ssize_t Ptime_ME_write_xml_2io    (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, const char *matchRegexp);
ssize_t Ptime_ME_write_xml_2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, const char *matchRegexp);
ssize_t Ptime_CME_write_xml_2io   (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pregexp_t *matchRegexp);
ssize_t Ptime_CME_write_xml_2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, Pregexp_t *matchRegexp);
ssize_t Ptime_SE_write_xml_2io    (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, const char *stopRegexp);
ssize_t Ptime_SE_write_xml_2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, const char *stopRegexp);
ssize_t Ptime_CSE_write_xml_2io   (P_t *pads, Sfio_t *io, Pbase_pd *pd, Puint32 *d, const char *tag, int indent, Pregexp_t *stopRegexp);
ssize_t Ptime_CSE_write_xml_2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, Pbase_pd *pd,
				   Puint32 *d, const char *tag, int indent, Pregexp_t *stopRegexp);
#endif /* FOR_CKIT */

ssize_t Ptime_FW_fmt2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width);
ssize_t Ptime_FW_fmt2buf_final   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width);
ssize_t Ptime_fmt2buf      (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar);
ssize_t Ptime_fmt2buf_final      (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar);
ssize_t Ptime_fmt2buf_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar);
ssize_t Ptime_fmt2buf_final_final(P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar);
ssize_t Ptime_ME_fmt2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *matchRegexp);
ssize_t Ptime_ME_fmt2buf_final   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *matchRegexp);
ssize_t Ptime_CME_fmt2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *matchRegexp);
ssize_t Ptime_CME_fmt2buf_final  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *matchRegexp);
ssize_t Ptime_SE_fmt2buf   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *stopRegexp);
ssize_t Ptime_SE_fmt2buf_final   (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *stopRegexp);
ssize_t Ptime_CSE_fmt2buf  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *stopRegexp);
ssize_t Ptime_CSE_fmt2buf_final  (P_t *pads, Pbyte *buf, size_t buf_len, int *buf_full, int *requested_out, const char *delims,
				  Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *stopRegexp);

ssize_t Ptime_FW_fmt2io    (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, size_t width);
ssize_t Ptime_fmt2io       (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pchar stopChar);
ssize_t Ptime_ME_fmt2io    (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *matchRegexp);
ssize_t Ptime_CME_fmt2io   (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *matchRegexp);
ssize_t Ptime_SE_fmt2io    (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, const char *stopRegexp);
ssize_t Ptime_CSE_fmt2io   (P_t *pads, Sfio_t *io, int *requested_out, const char *delims,
			    Pbase_m *m, Pbase_pd *pd, Puint32 *rep, Pregexp_t *stopRegexp);
#endif /*  P_CONFIG_A_CHAR_STRING > 0 && P_CONFIG_E_CHAR_STRING > 0  */

#endif /* P_CONFIG_WRITE_FUNCTIONS */

#endif  /*  __PDATE_H__   */
