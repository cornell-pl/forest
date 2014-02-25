/*@FILE @LEFT dibbler_library.tex*/

#ifndef __DIBBLER_NEW__H__
#define __DIBBLER_NEW__H__
#define entry_t_verify is_entry_t 
#include "pads.h"
typedef Puint32 zip_t;
typedef struct zip_t_m_s zip_t_m;
typedef Pbase_pd zip_t_pd;
struct zip_t_m_s {
  Pbase_m base;		/* Base mask */
  Pbase_m compoundLevel;		/* Typedef mask */
};

Perror_t zip_t_init (P_t *pads,zip_t *rep);

Perror_t zip_t_pd_init (P_t *pads,zip_t_pd *pd);

Perror_t zip_t_cleanup (P_t *pads,zip_t *rep);

Perror_t zip_t_pd_cleanup (P_t *pads,zip_t_pd *pd);

Perror_t zip_t_copy (P_t *pads,zip_t *rep_dst,zip_t *rep_src);

Perror_t zip_t_pd_copy (P_t *pads,zip_t_pd *pd_dst,zip_t_pd *pd_src);

void zip_t_m_init (P_t *pads,zip_t_m *mask,Pbase_m baseMask);

Perror_t zip_t_read (P_t *pads,zip_t_m *m,zip_t_pd *pd,zip_t *rep);

int is_zip_t (zip_t *rep);
typedef Puint32_acc zip_t_acc;

Perror_t zip_t_acc_init (P_t *pads,zip_t_acc *acc);

Perror_t zip_t_acc_reset (P_t *pads,zip_t_acc *acc);

Perror_t zip_t_acc_cleanup (P_t *pads,zip_t_acc *acc);

Perror_t zip_t_acc_add (P_t *pads,zip_t_acc *acc,zip_t_pd *pd,zip_t *rep);

Perror_t zip_t_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,zip_t_acc *acc);

Perror_t zip_t_acc_report (P_t *pads,char const *prefix,char const *what,int nst,zip_t_acc *acc);

ssize_t zip_t_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,zip_t_pd *pd,zip_t *rep);

ssize_t zip_t_write2io (P_t *pads,Sfio_t *io,zip_t_pd *pd,zip_t *rep);

ssize_t zip_t_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,zip_t_pd *pd,zip_t *rep,char const *tag,int indent);

ssize_t zip_t_write_xml_2io (P_t *pads,Sfio_t *io,zip_t_pd *pd,zip_t *rep,char const *tag,int indent);

ssize_t zip_t_fmt2buf_final (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,zip_t_m *m,zip_t_pd *pd,zip_t *rep);

ssize_t zip_t_fmt2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,zip_t_m *m,zip_t_pd *pd,zip_t *rep);

ssize_t zip_t_fmt2io (P_t *pads,Sfio_t *io,int *requestedOut,char const *delims,zip_t_m *m,zip_t_pd *pd,zip_t *rep);
typedef Puint64 pn_t;
typedef struct pn_t_m_s pn_t_m;
typedef Pbase_pd pn_t_pd;
struct pn_t_m_s {
  Pbase_m base;		/* Base mask */
  Pbase_m compoundLevel;		/* Typedef mask */
};

Perror_t pn_t_init (P_t *pads,pn_t *rep);

Perror_t pn_t_pd_init (P_t *pads,pn_t_pd *pd);

Perror_t pn_t_cleanup (P_t *pads,pn_t *rep);

Perror_t pn_t_pd_cleanup (P_t *pads,pn_t_pd *pd);

Perror_t pn_t_copy (P_t *pads,pn_t *rep_dst,pn_t *rep_src);

Perror_t pn_t_pd_copy (P_t *pads,pn_t_pd *pd_dst,pn_t_pd *pd_src);

void pn_t_m_init (P_t *pads,pn_t_m *mask,Pbase_m baseMask);

Perror_t pn_t_read (P_t *pads,pn_t_m *m,pn_t_pd *pd,pn_t *rep);

int is_pn_t (pn_t *rep);
typedef Puint64_acc pn_t_acc;

Perror_t pn_t_acc_init (P_t *pads,pn_t_acc *acc);

Perror_t pn_t_acc_reset (P_t *pads,pn_t_acc *acc);

Perror_t pn_t_acc_cleanup (P_t *pads,pn_t_acc *acc);

Perror_t pn_t_acc_add (P_t *pads,pn_t_acc *acc,pn_t_pd *pd,pn_t *rep);

Perror_t pn_t_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,pn_t_acc *acc);

Perror_t pn_t_acc_report (P_t *pads,char const *prefix,char const *what,int nst,pn_t_acc *acc);

ssize_t pn_t_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,pn_t_pd *pd,pn_t *rep);

ssize_t pn_t_write2io (P_t *pads,Sfio_t *io,pn_t_pd *pd,pn_t *rep);

ssize_t pn_t_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,pn_t_pd *pd,pn_t *rep,char const *tag,int indent);

ssize_t pn_t_write_xml_2io (P_t *pads,Sfio_t *io,pn_t_pd *pd,pn_t *rep,char const *tag,int indent);

ssize_t pn_t_fmt2buf_final (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,pn_t_m *m,pn_t_pd *pd,pn_t *rep);

ssize_t pn_t_fmt2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,pn_t_m *m,pn_t_pd *pd,pn_t *rep);

ssize_t pn_t_fmt2io (P_t *pads,Sfio_t *io,int *requestedOut,char const *delims,pn_t_m *m,pn_t_pd *pd,pn_t *rep);
typedef struct summary_header_t_s summary_header_t;
typedef struct summary_header_t_m_s summary_header_t_m;
typedef struct summary_header_t_pd_s summary_header_t_pd;
struct summary_header_t_m_s {
  Pbase_m compoundLevel;
  Pbase_m tstamp;		/* nested constraints */
};
struct summary_header_t_pd_s {
  Pflags_t pstate;
  Puint32 nerr;
  PerrCode_t errCode;
  Ploc_t loc;
  Pbase_pd tstamp;
};
struct summary_header_t_s {
  Puint32 tstamp;
};

Perror_t summary_header_t_init (P_t *pads,summary_header_t *rep);

Perror_t summary_header_t_pd_init (P_t *pads,summary_header_t_pd *pd);

Perror_t summary_header_t_cleanup (P_t *pads,summary_header_t *rep);

Perror_t summary_header_t_pd_cleanup (P_t *pads,summary_header_t_pd *pd);

Perror_t summary_header_t_copy (P_t *pads,summary_header_t *rep_dst,summary_header_t *rep_src);

Perror_t summary_header_t_pd_copy (P_t *pads,summary_header_t_pd *pd_dst,summary_header_t_pd *pd_src);

void summary_header_t_m_init (P_t *pads,summary_header_t_m *mask,Pbase_m baseMask);

Perror_t summary_header_t_read (P_t *pads,summary_header_t_m *m,summary_header_t_pd *pd,summary_header_t *rep);

int is_summary_header_t (summary_header_t *rep);
typedef struct summary_header_t_acc_s summary_header_t_acc;
struct summary_header_t_acc_s {
  Puint32_acc nerr;
  Puint32_acc tstamp;
};

Perror_t summary_header_t_acc_init (P_t *pads,summary_header_t_acc *acc);

Perror_t summary_header_t_acc_reset (P_t *pads,summary_header_t_acc *acc);

Perror_t summary_header_t_acc_cleanup (P_t *pads,summary_header_t_acc *acc);

Perror_t summary_header_t_acc_add (P_t *pads,summary_header_t_acc *acc,summary_header_t_pd *pd,summary_header_t *rep);

Perror_t summary_header_t_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,summary_header_t_acc *acc);

Perror_t summary_header_t_acc_report (P_t *pads,char const *prefix,char const *what,int nst,summary_header_t_acc *acc);

ssize_t summary_header_t_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,summary_header_t_pd *pd,summary_header_t *rep);

ssize_t summary_header_t_write2io (P_t *pads,Sfio_t *io,summary_header_t_pd *pd,summary_header_t *rep);

ssize_t summary_header_t_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,summary_header_t_pd *pd,summary_header_t *rep,char const *tag,int indent);

ssize_t summary_header_t_write_xml_2io (P_t *pads,Sfio_t *io,summary_header_t_pd *pd,summary_header_t *rep,char const *tag,int indent);

ssize_t summary_header_t_fmt2buf_final (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,summary_header_t_m *m,summary_header_t_pd *pd,summary_header_t *rep);

ssize_t summary_header_t_fmt2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,summary_header_t_m *m,summary_header_t_pd *pd,summary_header_t *rep);

ssize_t summary_header_t_fmt2io (P_t *pads,Sfio_t *io,int *requestedOut,char const *delims,summary_header_t_m *m,summary_header_t_pd *pd,summary_header_t *rep);
typedef struct no_ramp_t_s no_ramp_t;
typedef struct no_ramp_t_m_s no_ramp_t_m;
typedef struct no_ramp_t_pd_s no_ramp_t_pd;
struct no_ramp_t_m_s {
  Pbase_m compoundLevel;
  Pbase_m id;		/* nested constraints */
};
struct no_ramp_t_pd_s {
  Pflags_t pstate;
  Puint32 nerr;
  PerrCode_t errCode;
  Ploc_t loc;
  Pbase_pd id;
};
struct no_ramp_t_s {
  Puint64 id;
};

Perror_t no_ramp_t_init (P_t *pads,no_ramp_t *rep);

Perror_t no_ramp_t_pd_init (P_t *pads,no_ramp_t_pd *pd);

Perror_t no_ramp_t_cleanup (P_t *pads,no_ramp_t *rep);

Perror_t no_ramp_t_pd_cleanup (P_t *pads,no_ramp_t_pd *pd);

Perror_t no_ramp_t_copy (P_t *pads,no_ramp_t *rep_dst,no_ramp_t *rep_src);

Perror_t no_ramp_t_pd_copy (P_t *pads,no_ramp_t_pd *pd_dst,no_ramp_t_pd *pd_src);

void no_ramp_t_m_init (P_t *pads,no_ramp_t_m *mask,Pbase_m baseMask);

Perror_t no_ramp_t_read (P_t *pads,no_ramp_t_m *m,no_ramp_t_pd *pd,no_ramp_t *rep);

int is_no_ramp_t (no_ramp_t *rep);
typedef struct no_ramp_t_acc_s no_ramp_t_acc;
struct no_ramp_t_acc_s {
  Puint32_acc nerr;
  Puint64_acc id;
};

Perror_t no_ramp_t_acc_init (P_t *pads,no_ramp_t_acc *acc);

Perror_t no_ramp_t_acc_reset (P_t *pads,no_ramp_t_acc *acc);

Perror_t no_ramp_t_acc_cleanup (P_t *pads,no_ramp_t_acc *acc);

Perror_t no_ramp_t_acc_add (P_t *pads,no_ramp_t_acc *acc,no_ramp_t_pd *pd,no_ramp_t *rep);

Perror_t no_ramp_t_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,no_ramp_t_acc *acc);

Perror_t no_ramp_t_acc_report (P_t *pads,char const *prefix,char const *what,int nst,no_ramp_t_acc *acc);

ssize_t no_ramp_t_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,no_ramp_t_pd *pd,no_ramp_t *rep);

ssize_t no_ramp_t_write2io (P_t *pads,Sfio_t *io,no_ramp_t_pd *pd,no_ramp_t *rep);

ssize_t no_ramp_t_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,no_ramp_t_pd *pd,no_ramp_t *rep,char const *tag,int indent);

ssize_t no_ramp_t_write_xml_2io (P_t *pads,Sfio_t *io,no_ramp_t_pd *pd,no_ramp_t *rep,char const *tag,int indent);

ssize_t no_ramp_t_fmt2buf_final (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,no_ramp_t_m *m,no_ramp_t_pd *pd,no_ramp_t *rep);

ssize_t no_ramp_t_fmt2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,no_ramp_t_m *m,no_ramp_t_pd *pd,no_ramp_t *rep);

ssize_t no_ramp_t_fmt2io (P_t *pads,Sfio_t *io,int *requestedOut,char const *delims,no_ramp_t_m *m,no_ramp_t_pd *pd,no_ramp_t *rep);
typedef enum dib_ramp_t_tag_e dib_ramp_t_tag;
typedef union dib_ramp_t_u_u dib_ramp_t_u;
typedef struct dib_ramp_t_s dib_ramp_t;
typedef struct dib_ramp_t_m_s dib_ramp_t_m;
typedef union dib_ramp_t_pd_u_u dib_ramp_t_pd_u;
typedef struct dib_ramp_t_pd_s dib_ramp_t_pd;
enum dib_ramp_t_tag_e {
  dib_ramp_t_err=0,
  ramp=1,
  genRamp=2
  };
union dib_ramp_t_pd_u_u {
  Pbase_pd ramp;
  no_ramp_t_pd genRamp;
};
struct dib_ramp_t_pd_s {
  Pflags_t pstate;
  Puint32 nerr;
  PerrCode_t errCode;
  Ploc_t loc;
  dib_ramp_t_tag tag;
  dib_ramp_t_pd_u val;
};
union dib_ramp_t_u_u {
  Puint64 ramp;
  no_ramp_t genRamp;
};
struct dib_ramp_t_s {
  dib_ramp_t_tag tag;
  dib_ramp_t_u val;
};
struct dib_ramp_t_m_s {
  Pbase_m compoundLevel;
  Pbase_m ramp;		/* nested constraints */
  no_ramp_t_m genRamp;		/* nested constraints */
};

ssize_t dib_ramp_t_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,dib_ramp_t_pd *pd,dib_ramp_t *rep);

ssize_t dib_ramp_t_write2io (P_t *pads,Sfio_t *io,dib_ramp_t_pd *pd,dib_ramp_t *rep);

ssize_t dib_ramp_t_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,dib_ramp_t_pd *pd,dib_ramp_t *rep,char const *tag,int indent);

ssize_t dib_ramp_t_write_xml_2io (P_t *pads,Sfio_t *io,dib_ramp_t_pd *pd,dib_ramp_t *rep,char const *tag,int indent);

char const *dib_ramp_t_tag2str (dib_ramp_t_tag which);

Perror_t dib_ramp_t_init (P_t *pads,dib_ramp_t *rep);

Perror_t dib_ramp_t_pd_init (P_t *pads,dib_ramp_t_pd *pd);

Perror_t dib_ramp_t_cleanup (P_t *pads,dib_ramp_t *rep);

Perror_t dib_ramp_t_pd_cleanup (P_t *pads,dib_ramp_t_pd *pd);

Perror_t dib_ramp_t_copy (P_t *pads,dib_ramp_t *rep_dst,dib_ramp_t *rep_src);

Perror_t dib_ramp_t_pd_copy (P_t *pads,dib_ramp_t_pd *pd_dst,dib_ramp_t_pd *pd_src);

void dib_ramp_t_m_init (P_t *pads,dib_ramp_t_m *mask,Pbase_m baseMask);

Perror_t dib_ramp_t_read (P_t *pads,dib_ramp_t_m *m,dib_ramp_t_pd *pd,dib_ramp_t *rep);

int is_dib_ramp_t (dib_ramp_t *rep);
typedef struct dib_ramp_t_acc_s dib_ramp_t_acc;
struct dib_ramp_t_acc_s {
  Pint32_acc tag;
  Puint64_acc ramp;
  no_ramp_t_acc genRamp;
};

Perror_t dib_ramp_t_acc_init (P_t *pads,dib_ramp_t_acc *acc);

Perror_t dib_ramp_t_acc_reset (P_t *pads,dib_ramp_t_acc *acc);

Perror_t dib_ramp_t_acc_cleanup (P_t *pads,dib_ramp_t_acc *acc);

Perror_t dib_ramp_t_acc_add (P_t *pads,dib_ramp_t_acc *acc,dib_ramp_t_pd *pd,dib_ramp_t *rep);

Perror_t dib_ramp_t_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,dib_ramp_t_acc *acc);

Perror_t dib_ramp_t_acc_report (P_t *pads,char const *prefix,char const *what,int nst,dib_ramp_t_acc *acc);

ssize_t dib_ramp_t_fmt2buf_final (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,dib_ramp_t_m *m,dib_ramp_t_pd *pd,dib_ramp_t *rep);

ssize_t dib_ramp_t_fmt2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,dib_ramp_t_m *m,dib_ramp_t_pd *pd,dib_ramp_t *rep);

ssize_t dib_ramp_t_fmt2io (P_t *pads,Sfio_t *io,int *requestedOut,char const *delims,dib_ramp_t_m *m,dib_ramp_t_pd *pd,dib_ramp_t *rep);
typedef enum service_tn_t_tag_e service_tn_t_tag;
typedef union service_tn_t_u_u service_tn_t_u;
typedef struct service_tn_t_s service_tn_t;
typedef struct service_tn_t_m_s service_tn_t_m;
typedef union service_tn_t_pd_u_u service_tn_t_pd_u;
typedef struct service_tn_t_pd_s service_tn_t_pd;
enum service_tn_t_tag_e {
  service_tn_t_err=0,
  some_service_tn_t=1,
  none_service_tn_t=2
  };
union service_tn_t_pd_u_u {
  pn_t_pd some_service_tn_t;
  Pbase_pd none_service_tn_t;		/* value was not present. none_service_tn_t = 0 */
};
struct service_tn_t_pd_s {
  Pflags_t pstate;
  Puint32 nerr;
  PerrCode_t errCode;
  Ploc_t loc;
  service_tn_t_tag tag;
  service_tn_t_pd_u val;
};
union service_tn_t_u_u {
  pn_t some_service_tn_t;		/* value is present */
};
struct service_tn_t_s {
  service_tn_t_tag tag;
  service_tn_t_u val;
};
struct service_tn_t_m_s {
  Pbase_m compoundLevel;
  pn_t_m some_service_tn_t;		/* nested constraints */
  Pbase_m none_service_tn_t;		/* nested constraints */
};

ssize_t service_tn_t_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,service_tn_t_pd *pd,service_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version);

ssize_t service_tn_t_write2io (P_t *pads,Sfio_t *io,service_tn_t_pd *pd,service_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version);

ssize_t service_tn_t_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,service_tn_t_pd *pd,service_tn_t *rep,char const *tag,int indent,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version);

ssize_t service_tn_t_write_xml_2io (P_t *pads,Sfio_t *io,service_tn_t_pd *pd,service_tn_t *rep,char const *tag,int indent,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version);

char const *service_tn_t_tag2str (service_tn_t_tag which);

Perror_t service_tn_t_init (P_t *pads,service_tn_t *rep);

Perror_t service_tn_t_pd_init (P_t *pads,service_tn_t_pd *pd);

Perror_t service_tn_t_cleanup (P_t *pads,service_tn_t *rep);

Perror_t service_tn_t_pd_cleanup (P_t *pads,service_tn_t_pd *pd);

Perror_t service_tn_t_copy (P_t *pads,service_tn_t *rep_dst,service_tn_t *rep_src);

Perror_t service_tn_t_pd_copy (P_t *pads,service_tn_t_pd *pd_dst,service_tn_t_pd *pd_src);

void service_tn_t_m_init (P_t *pads,service_tn_t_m *mask,Pbase_m baseMask);

Perror_t service_tn_t_read (P_t *pads,service_tn_t_m *m,service_tn_t_pd *pd,service_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version);

int is_service_tn_t (service_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version);
typedef struct service_tn_t_acc_s service_tn_t_acc;
struct service_tn_t_acc_s {
  Pint32_acc tag;
  pn_t_acc some_service_tn_t;
};

Perror_t service_tn_t_acc_init (P_t *pads,service_tn_t_acc *acc);

Perror_t service_tn_t_acc_reset (P_t *pads,service_tn_t_acc *acc);

Perror_t service_tn_t_acc_cleanup (P_t *pads,service_tn_t_acc *acc);

Perror_t service_tn_t_acc_add (P_t *pads,service_tn_t_acc *acc,service_tn_t_pd *pd,service_tn_t *rep);

Perror_t service_tn_t_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,service_tn_t_acc *acc);

Perror_t service_tn_t_acc_report (P_t *pads,char const *prefix,char const *what,int nst,service_tn_t_acc *acc);

ssize_t service_tn_t_fmt2buf_final (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,service_tn_t_m *m,service_tn_t_pd *pd,service_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version);

ssize_t service_tn_t_fmt2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,service_tn_t_m *m,service_tn_t_pd *pd,service_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version);

ssize_t service_tn_t_fmt2io (P_t *pads,Sfio_t *io,int *requestedOut,char const *delims,service_tn_t_m *m,service_tn_t_pd *pd,service_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version);
typedef enum billing_tn_t_tag_e billing_tn_t_tag;
typedef union billing_tn_t_u_u billing_tn_t_u;
typedef struct billing_tn_t_s billing_tn_t;
typedef struct billing_tn_t_m_s billing_tn_t_m;
typedef union billing_tn_t_pd_u_u billing_tn_t_pd_u;
typedef struct billing_tn_t_pd_s billing_tn_t_pd;
enum billing_tn_t_tag_e {
  billing_tn_t_err=0,
  some_billing_tn_t=1,
  none_billing_tn_t=2
  };
union billing_tn_t_pd_u_u {
  pn_t_pd some_billing_tn_t;
  Pbase_pd none_billing_tn_t;		/* value was not present. none_billing_tn_t = 0 */
};
struct billing_tn_t_pd_s {
  Pflags_t pstate;
  Puint32 nerr;
  PerrCode_t errCode;
  Ploc_t loc;
  billing_tn_t_tag tag;
  billing_tn_t_pd_u val;
};
union billing_tn_t_u_u {
  pn_t some_billing_tn_t;		/* value is present */
};
struct billing_tn_t_s {
  billing_tn_t_tag tag;
  billing_tn_t_u val;
};
struct billing_tn_t_m_s {
  Pbase_m compoundLevel;
  pn_t_m some_billing_tn_t;		/* nested constraints */
  Pbase_m none_billing_tn_t;		/* nested constraints */
};

ssize_t billing_tn_t_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,billing_tn_t_pd *pd,billing_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn);

ssize_t billing_tn_t_write2io (P_t *pads,Sfio_t *io,billing_tn_t_pd *pd,billing_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn);

ssize_t billing_tn_t_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,billing_tn_t_pd *pd,billing_tn_t *rep,char const *tag,int indent,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn);

ssize_t billing_tn_t_write_xml_2io (P_t *pads,Sfio_t *io,billing_tn_t_pd *pd,billing_tn_t *rep,char const *tag,int indent,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn);

char const *billing_tn_t_tag2str (billing_tn_t_tag which);

Perror_t billing_tn_t_init (P_t *pads,billing_tn_t *rep);

Perror_t billing_tn_t_pd_init (P_t *pads,billing_tn_t_pd *pd);

Perror_t billing_tn_t_cleanup (P_t *pads,billing_tn_t *rep);

Perror_t billing_tn_t_pd_cleanup (P_t *pads,billing_tn_t_pd *pd);

Perror_t billing_tn_t_copy (P_t *pads,billing_tn_t *rep_dst,billing_tn_t *rep_src);

Perror_t billing_tn_t_pd_copy (P_t *pads,billing_tn_t_pd *pd_dst,billing_tn_t_pd *pd_src);

void billing_tn_t_m_init (P_t *pads,billing_tn_t_m *mask,Pbase_m baseMask);

Perror_t billing_tn_t_read (P_t *pads,billing_tn_t_m *m,billing_tn_t_pd *pd,billing_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn);

int is_billing_tn_t (billing_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn);
typedef struct billing_tn_t_acc_s billing_tn_t_acc;
struct billing_tn_t_acc_s {
  Pint32_acc tag;
  pn_t_acc some_billing_tn_t;
};

Perror_t billing_tn_t_acc_init (P_t *pads,billing_tn_t_acc *acc);

Perror_t billing_tn_t_acc_reset (P_t *pads,billing_tn_t_acc *acc);

Perror_t billing_tn_t_acc_cleanup (P_t *pads,billing_tn_t_acc *acc);

Perror_t billing_tn_t_acc_add (P_t *pads,billing_tn_t_acc *acc,billing_tn_t_pd *pd,billing_tn_t *rep);

Perror_t billing_tn_t_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,billing_tn_t_acc *acc);

Perror_t billing_tn_t_acc_report (P_t *pads,char const *prefix,char const *what,int nst,billing_tn_t_acc *acc);

ssize_t billing_tn_t_fmt2buf_final (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,billing_tn_t_m *m,billing_tn_t_pd *pd,billing_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn);

ssize_t billing_tn_t_fmt2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,billing_tn_t_m *m,billing_tn_t_pd *pd,billing_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn);

ssize_t billing_tn_t_fmt2io (P_t *pads,Sfio_t *io,int *requestedOut,char const *delims,billing_tn_t_m *m,billing_tn_t_pd *pd,billing_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn);
typedef enum nlp_service_tn_t_tag_e nlp_service_tn_t_tag;
typedef union nlp_service_tn_t_u_u nlp_service_tn_t_u;
typedef struct nlp_service_tn_t_s nlp_service_tn_t;
typedef struct nlp_service_tn_t_m_s nlp_service_tn_t_m;
typedef union nlp_service_tn_t_pd_u_u nlp_service_tn_t_pd_u;
typedef struct nlp_service_tn_t_pd_s nlp_service_tn_t_pd;
enum nlp_service_tn_t_tag_e {
  nlp_service_tn_t_err=0,
  some_nlp_service_tn_t=1,
  none_nlp_service_tn_t=2
  };
union nlp_service_tn_t_pd_u_u {
  pn_t_pd some_nlp_service_tn_t;
  Pbase_pd none_nlp_service_tn_t;		/* value was not present. none_nlp_service_tn_t = 0 */
};
struct nlp_service_tn_t_pd_s {
  Pflags_t pstate;
  Puint32 nerr;
  PerrCode_t errCode;
  Ploc_t loc;
  nlp_service_tn_t_tag tag;
  nlp_service_tn_t_pd_u val;
};
union nlp_service_tn_t_u_u {
  pn_t some_nlp_service_tn_t;		/* value is present */
};
struct nlp_service_tn_t_s {
  nlp_service_tn_t_tag tag;
  nlp_service_tn_t_u val;
};
struct nlp_service_tn_t_m_s {
  Pbase_m compoundLevel;
  pn_t_m some_nlp_service_tn_t;		/* nested constraints */
  Pbase_m none_nlp_service_tn_t;		/* nested constraints */
};

ssize_t nlp_service_tn_t_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,nlp_service_tn_t_pd *pd,nlp_service_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn);

ssize_t nlp_service_tn_t_write2io (P_t *pads,Sfio_t *io,nlp_service_tn_t_pd *pd,nlp_service_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn);

ssize_t nlp_service_tn_t_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,nlp_service_tn_t_pd *pd,nlp_service_tn_t *rep,char const *tag,int indent,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn);

ssize_t nlp_service_tn_t_write_xml_2io (P_t *pads,Sfio_t *io,nlp_service_tn_t_pd *pd,nlp_service_tn_t *rep,char const *tag,int indent,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn);

char const *nlp_service_tn_t_tag2str (nlp_service_tn_t_tag which);

Perror_t nlp_service_tn_t_init (P_t *pads,nlp_service_tn_t *rep);

Perror_t nlp_service_tn_t_pd_init (P_t *pads,nlp_service_tn_t_pd *pd);

Perror_t nlp_service_tn_t_cleanup (P_t *pads,nlp_service_tn_t *rep);

Perror_t nlp_service_tn_t_pd_cleanup (P_t *pads,nlp_service_tn_t_pd *pd);

Perror_t nlp_service_tn_t_copy (P_t *pads,nlp_service_tn_t *rep_dst,nlp_service_tn_t *rep_src);

Perror_t nlp_service_tn_t_pd_copy (P_t *pads,nlp_service_tn_t_pd *pd_dst,nlp_service_tn_t_pd *pd_src);

void nlp_service_tn_t_m_init (P_t *pads,nlp_service_tn_t_m *mask,Pbase_m baseMask);

Perror_t nlp_service_tn_t_read (P_t *pads,nlp_service_tn_t_m *m,nlp_service_tn_t_pd *pd,nlp_service_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn);

int is_nlp_service_tn_t (nlp_service_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn);
typedef struct nlp_service_tn_t_acc_s nlp_service_tn_t_acc;
struct nlp_service_tn_t_acc_s {
  Pint32_acc tag;
  pn_t_acc some_nlp_service_tn_t;
};

Perror_t nlp_service_tn_t_acc_init (P_t *pads,nlp_service_tn_t_acc *acc);

Perror_t nlp_service_tn_t_acc_reset (P_t *pads,nlp_service_tn_t_acc *acc);

Perror_t nlp_service_tn_t_acc_cleanup (P_t *pads,nlp_service_tn_t_acc *acc);

Perror_t nlp_service_tn_t_acc_add (P_t *pads,nlp_service_tn_t_acc *acc,nlp_service_tn_t_pd *pd,nlp_service_tn_t *rep);

Perror_t nlp_service_tn_t_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,nlp_service_tn_t_acc *acc);

Perror_t nlp_service_tn_t_acc_report (P_t *pads,char const *prefix,char const *what,int nst,nlp_service_tn_t_acc *acc);

ssize_t nlp_service_tn_t_fmt2buf_final (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,nlp_service_tn_t_m *m,nlp_service_tn_t_pd *pd,nlp_service_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn);

ssize_t nlp_service_tn_t_fmt2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,nlp_service_tn_t_m *m,nlp_service_tn_t_pd *pd,nlp_service_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn);

ssize_t nlp_service_tn_t_fmt2io (P_t *pads,Sfio_t *io,int *requestedOut,char const *delims,nlp_service_tn_t_m *m,nlp_service_tn_t_pd *pd,nlp_service_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn);
typedef enum nlp_billing_tn_t_tag_e nlp_billing_tn_t_tag;
typedef union nlp_billing_tn_t_u_u nlp_billing_tn_t_u;
typedef struct nlp_billing_tn_t_s nlp_billing_tn_t;
typedef struct nlp_billing_tn_t_m_s nlp_billing_tn_t_m;
typedef union nlp_billing_tn_t_pd_u_u nlp_billing_tn_t_pd_u;
typedef struct nlp_billing_tn_t_pd_s nlp_billing_tn_t_pd;
enum nlp_billing_tn_t_tag_e {
  nlp_billing_tn_t_err=0,
  some_nlp_billing_tn_t=1,
  none_nlp_billing_tn_t=2
  };
union nlp_billing_tn_t_pd_u_u {
  pn_t_pd some_nlp_billing_tn_t;
  Pbase_pd none_nlp_billing_tn_t;		/* value was not present. none_nlp_billing_tn_t = 0 */
};
struct nlp_billing_tn_t_pd_s {
  Pflags_t pstate;
  Puint32 nerr;
  PerrCode_t errCode;
  Ploc_t loc;
  nlp_billing_tn_t_tag tag;
  nlp_billing_tn_t_pd_u val;
};
union nlp_billing_tn_t_u_u {
  pn_t some_nlp_billing_tn_t;		/* value is present */
};
struct nlp_billing_tn_t_s {
  nlp_billing_tn_t_tag tag;
  nlp_billing_tn_t_u val;
};
struct nlp_billing_tn_t_m_s {
  Pbase_m compoundLevel;
  pn_t_m some_nlp_billing_tn_t;		/* nested constraints */
  Pbase_m none_nlp_billing_tn_t;		/* nested constraints */
};

ssize_t nlp_billing_tn_t_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,nlp_billing_tn_t_pd *pd,nlp_billing_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn,nlp_service_tn_t *nlp_service_tn);

ssize_t nlp_billing_tn_t_write2io (P_t *pads,Sfio_t *io,nlp_billing_tn_t_pd *pd,nlp_billing_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn,nlp_service_tn_t *nlp_service_tn);

ssize_t nlp_billing_tn_t_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,nlp_billing_tn_t_pd *pd,nlp_billing_tn_t *rep,char const *tag,int indent,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn,nlp_service_tn_t *nlp_service_tn);

ssize_t nlp_billing_tn_t_write_xml_2io (P_t *pads,Sfio_t *io,nlp_billing_tn_t_pd *pd,nlp_billing_tn_t *rep,char const *tag,int indent,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn,nlp_service_tn_t *nlp_service_tn);

char const *nlp_billing_tn_t_tag2str (nlp_billing_tn_t_tag which);

Perror_t nlp_billing_tn_t_init (P_t *pads,nlp_billing_tn_t *rep);

Perror_t nlp_billing_tn_t_pd_init (P_t *pads,nlp_billing_tn_t_pd *pd);

Perror_t nlp_billing_tn_t_cleanup (P_t *pads,nlp_billing_tn_t *rep);

Perror_t nlp_billing_tn_t_pd_cleanup (P_t *pads,nlp_billing_tn_t_pd *pd);

Perror_t nlp_billing_tn_t_copy (P_t *pads,nlp_billing_tn_t *rep_dst,nlp_billing_tn_t *rep_src);

Perror_t nlp_billing_tn_t_pd_copy (P_t *pads,nlp_billing_tn_t_pd *pd_dst,nlp_billing_tn_t_pd *pd_src);

void nlp_billing_tn_t_m_init (P_t *pads,nlp_billing_tn_t_m *mask,Pbase_m baseMask);

Perror_t nlp_billing_tn_t_read (P_t *pads,nlp_billing_tn_t_m *m,nlp_billing_tn_t_pd *pd,nlp_billing_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn,nlp_service_tn_t *nlp_service_tn);

int is_nlp_billing_tn_t (nlp_billing_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn,nlp_service_tn_t *nlp_service_tn);
typedef struct nlp_billing_tn_t_acc_s nlp_billing_tn_t_acc;
struct nlp_billing_tn_t_acc_s {
  Pint32_acc tag;
  pn_t_acc some_nlp_billing_tn_t;
};

Perror_t nlp_billing_tn_t_acc_init (P_t *pads,nlp_billing_tn_t_acc *acc);

Perror_t nlp_billing_tn_t_acc_reset (P_t *pads,nlp_billing_tn_t_acc *acc);

Perror_t nlp_billing_tn_t_acc_cleanup (P_t *pads,nlp_billing_tn_t_acc *acc);

Perror_t nlp_billing_tn_t_acc_add (P_t *pads,nlp_billing_tn_t_acc *acc,nlp_billing_tn_t_pd *pd,nlp_billing_tn_t *rep);

Perror_t nlp_billing_tn_t_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,nlp_billing_tn_t_acc *acc);

Perror_t nlp_billing_tn_t_acc_report (P_t *pads,char const *prefix,char const *what,int nst,nlp_billing_tn_t_acc *acc);

ssize_t nlp_billing_tn_t_fmt2buf_final (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,nlp_billing_tn_t_m *m,nlp_billing_tn_t_pd *pd,nlp_billing_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn,nlp_service_tn_t *nlp_service_tn);

ssize_t nlp_billing_tn_t_fmt2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,nlp_billing_tn_t_m *m,nlp_billing_tn_t_pd *pd,nlp_billing_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn,nlp_service_tn_t *nlp_service_tn);

ssize_t nlp_billing_tn_t_fmt2io (P_t *pads,Sfio_t *io,int *requestedOut,char const *delims,nlp_billing_tn_t_m *m,nlp_billing_tn_t_pd *pd,nlp_billing_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn,nlp_service_tn_t *nlp_service_tn);
typedef enum zip_code_t_tag_e zip_code_t_tag;
typedef union zip_code_t_u_u zip_code_t_u;
typedef struct zip_code_t_s zip_code_t;
typedef struct zip_code_t_m_s zip_code_t_m;
typedef union zip_code_t_pd_u_u zip_code_t_pd_u;
typedef struct zip_code_t_pd_s zip_code_t_pd;
enum zip_code_t_tag_e {
  zip_code_t_err=0,
  some_zip_code_t=1,
  none_zip_code_t=2
  };
union zip_code_t_pd_u_u {
  zip_t_pd some_zip_code_t;
  Pbase_pd none_zip_code_t;		/* value was not present. none_zip_code_t = 0 */
};
struct zip_code_t_pd_s {
  Pflags_t pstate;
  Puint32 nerr;
  PerrCode_t errCode;
  Ploc_t loc;
  zip_code_t_tag tag;
  zip_code_t_pd_u val;
};
union zip_code_t_u_u {
  zip_t some_zip_code_t;		/* value is present */
};
struct zip_code_t_s {
  zip_code_t_tag tag;
  zip_code_t_u val;
};
struct zip_code_t_m_s {
  Pbase_m compoundLevel;
  zip_t_m some_zip_code_t;		/* nested constraints */
  Pbase_m none_zip_code_t;		/* nested constraints */
};

ssize_t zip_code_t_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,zip_code_t_pd *pd,zip_code_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn,nlp_service_tn_t *nlp_service_tn,nlp_billing_tn_t *nlp_billing_tn);

ssize_t zip_code_t_write2io (P_t *pads,Sfio_t *io,zip_code_t_pd *pd,zip_code_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn,nlp_service_tn_t *nlp_service_tn,nlp_billing_tn_t *nlp_billing_tn);

ssize_t zip_code_t_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,zip_code_t_pd *pd,zip_code_t *rep,char const *tag,int indent,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn,nlp_service_tn_t *nlp_service_tn,nlp_billing_tn_t *nlp_billing_tn);

ssize_t zip_code_t_write_xml_2io (P_t *pads,Sfio_t *io,zip_code_t_pd *pd,zip_code_t *rep,char const *tag,int indent,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn,nlp_service_tn_t *nlp_service_tn,nlp_billing_tn_t *nlp_billing_tn);

char const *zip_code_t_tag2str (zip_code_t_tag which);

Perror_t zip_code_t_init (P_t *pads,zip_code_t *rep);

Perror_t zip_code_t_pd_init (P_t *pads,zip_code_t_pd *pd);

Perror_t zip_code_t_cleanup (P_t *pads,zip_code_t *rep);

Perror_t zip_code_t_pd_cleanup (P_t *pads,zip_code_t_pd *pd);

Perror_t zip_code_t_copy (P_t *pads,zip_code_t *rep_dst,zip_code_t *rep_src);

Perror_t zip_code_t_pd_copy (P_t *pads,zip_code_t_pd *pd_dst,zip_code_t_pd *pd_src);

void zip_code_t_m_init (P_t *pads,zip_code_t_m *mask,Pbase_m baseMask);

Perror_t zip_code_t_read (P_t *pads,zip_code_t_m *m,zip_code_t_pd *pd,zip_code_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn,nlp_service_tn_t *nlp_service_tn,nlp_billing_tn_t *nlp_billing_tn);

int is_zip_code_t (zip_code_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn,nlp_service_tn_t *nlp_service_tn,nlp_billing_tn_t *nlp_billing_tn);
typedef struct zip_code_t_acc_s zip_code_t_acc;
struct zip_code_t_acc_s {
  Pint32_acc tag;
  zip_t_acc some_zip_code_t;
};

Perror_t zip_code_t_acc_init (P_t *pads,zip_code_t_acc *acc);

Perror_t zip_code_t_acc_reset (P_t *pads,zip_code_t_acc *acc);

Perror_t zip_code_t_acc_cleanup (P_t *pads,zip_code_t_acc *acc);

Perror_t zip_code_t_acc_add (P_t *pads,zip_code_t_acc *acc,zip_code_t_pd *pd,zip_code_t *rep);

Perror_t zip_code_t_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,zip_code_t_acc *acc);

Perror_t zip_code_t_acc_report (P_t *pads,char const *prefix,char const *what,int nst,zip_code_t_acc *acc);

ssize_t zip_code_t_fmt2buf_final (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,zip_code_t_m *m,zip_code_t_pd *pd,zip_code_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn,nlp_service_tn_t *nlp_service_tn,nlp_billing_tn_t *nlp_billing_tn);

ssize_t zip_code_t_fmt2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,zip_code_t_m *m,zip_code_t_pd *pd,zip_code_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn,nlp_service_tn_t *nlp_service_tn,nlp_billing_tn_t *nlp_billing_tn);

ssize_t zip_code_t_fmt2io (P_t *pads,Sfio_t *io,int *requestedOut,char const *delims,zip_code_t_m *m,zip_code_t_pd *pd,zip_code_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn,nlp_service_tn_t *nlp_service_tn,nlp_billing_tn_t *nlp_billing_tn);
typedef struct order_header_t_s order_header_t;
typedef struct order_header_t_m_s order_header_t_m;
typedef struct order_header_t_pd_s order_header_t_pd;
struct order_header_t_m_s {
  Pbase_m compoundLevel;
  Pbase_m order_num;		/* nested constraints */
  Pbase_m att_order_num;		/* nested constraints */
  Pbase_m ord_version;		/* nested constraints */
  service_tn_t_m service_tn;		/* nested constraints */
  billing_tn_t_m billing_tn;		/* nested constraints */
  nlp_service_tn_t_m nlp_service_tn;		/* nested constraints */
  nlp_billing_tn_t_m nlp_billing_tn;		/* nested constraints */
  zip_code_t_m zip_code;		/* nested constraints */
  dib_ramp_t_m ramp;		/* nested constraints */
  Pbase_m order_type;		/* nested constraints */
  Pbase_m order_details;		/* nested constraints */
  Pbase_m unused;		/* nested constraints */
  Pbase_m stream;		/* nested constraints */
};
struct order_header_t_pd_s {
  Pflags_t pstate;
  Puint32 nerr;
  PerrCode_t errCode;
  Ploc_t loc;
  Pbase_pd order_num;
  Pbase_pd att_order_num;
  Pbase_pd ord_version;
  service_tn_t_pd service_tn;
  billing_tn_t_pd billing_tn;
  nlp_service_tn_t_pd nlp_service_tn;
  nlp_billing_tn_t_pd nlp_billing_tn;
  zip_code_t_pd zip_code;
  dib_ramp_t_pd ramp;
  Pbase_pd order_type;
  Pbase_pd order_details;
  Pbase_pd unused;
  Pbase_pd stream;
};
struct order_header_t_s {
  Puint32 order_num;
  Puint32 att_order_num;
  Puint32 ord_version;
  service_tn_t service_tn;
  billing_tn_t billing_tn;
  nlp_service_tn_t nlp_service_tn;
  nlp_billing_tn_t nlp_billing_tn;
  zip_code_t zip_code;
  dib_ramp_t ramp;
  Pstring order_type;
  Puint32 order_details;
  Pstring unused;
  Pstring stream;
};

Perror_t order_header_t_init (P_t *pads,order_header_t *rep);

Perror_t order_header_t_pd_init (P_t *pads,order_header_t_pd *pd);

Perror_t order_header_t_cleanup (P_t *pads,order_header_t *rep);

Perror_t order_header_t_pd_cleanup (P_t *pads,order_header_t_pd *pd);

Perror_t order_header_t_copy (P_t *pads,order_header_t *rep_dst,order_header_t *rep_src);

Perror_t order_header_t_pd_copy (P_t *pads,order_header_t_pd *pd_dst,order_header_t_pd *pd_src);

void order_header_t_m_init (P_t *pads,order_header_t_m *mask,Pbase_m baseMask);

Perror_t order_header_t_read (P_t *pads,order_header_t_m *m,order_header_t_pd *pd,order_header_t *rep);

int is_order_header_t (order_header_t *rep);
typedef struct order_header_t_acc_s order_header_t_acc;
struct order_header_t_acc_s {
  Puint32_acc nerr;
  Puint32_acc order_num;
  Puint32_acc att_order_num;
  Puint32_acc ord_version;
  service_tn_t_acc service_tn;
  billing_tn_t_acc billing_tn;
  nlp_service_tn_t_acc nlp_service_tn;
  nlp_billing_tn_t_acc nlp_billing_tn;
  zip_code_t_acc zip_code;
  dib_ramp_t_acc ramp;
  Pstring_acc order_type;
  Puint32_acc order_details;
  Pstring_acc unused;
  Pstring_acc stream;
};

Perror_t order_header_t_acc_init (P_t *pads,order_header_t_acc *acc);

Perror_t order_header_t_acc_reset (P_t *pads,order_header_t_acc *acc);

Perror_t order_header_t_acc_cleanup (P_t *pads,order_header_t_acc *acc);

Perror_t order_header_t_acc_add (P_t *pads,order_header_t_acc *acc,order_header_t_pd *pd,order_header_t *rep);

Perror_t order_header_t_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,order_header_t_acc *acc);

Perror_t order_header_t_acc_report (P_t *pads,char const *prefix,char const *what,int nst,order_header_t_acc *acc);

ssize_t order_header_t_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,order_header_t_pd *pd,order_header_t *rep);

ssize_t order_header_t_write2io (P_t *pads,Sfio_t *io,order_header_t_pd *pd,order_header_t *rep);

ssize_t order_header_t_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,order_header_t_pd *pd,order_header_t *rep,char const *tag,int indent);

ssize_t order_header_t_write_xml_2io (P_t *pads,Sfio_t *io,order_header_t_pd *pd,order_header_t *rep,char const *tag,int indent);

ssize_t order_header_t_fmt2buf_final (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,order_header_t_m *m,order_header_t_pd *pd,order_header_t *rep);

ssize_t order_header_t_fmt2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,order_header_t_m *m,order_header_t_pd *pd,order_header_t *rep);

ssize_t order_header_t_fmt2io (P_t *pads,Sfio_t *io,int *requestedOut,char const *delims,order_header_t_m *m,order_header_t_pd *pd,order_header_t *rep);
typedef struct event_t_s event_t;
typedef struct event_t_m_s event_t_m;
typedef struct event_t_pd_s event_t_pd;
struct event_t_m_s {
  Pbase_m compoundLevel;
  Pbase_m state;		/* nested constraints */
  Pbase_m tstamp;		/* nested constraints */
};
struct event_t_pd_s {
  Pflags_t pstate;
  Puint32 nerr;
  PerrCode_t errCode;
  Ploc_t loc;
  Pbase_pd state;
  Pbase_pd tstamp;
};
struct event_t_s {
  Pstring state;
  Puint32 tstamp;
};

Perror_t event_t_init (P_t *pads,event_t *rep);

Perror_t event_t_pd_init (P_t *pads,event_t_pd *pd);

Perror_t event_t_cleanup (P_t *pads,event_t *rep);

Perror_t event_t_pd_cleanup (P_t *pads,event_t_pd *pd);

Perror_t event_t_copy (P_t *pads,event_t *rep_dst,event_t *rep_src);

Perror_t event_t_pd_copy (P_t *pads,event_t_pd *pd_dst,event_t_pd *pd_src);

void event_t_m_init (P_t *pads,event_t_m *mask,Pbase_m baseMask);

Perror_t event_t_read (P_t *pads,event_t_m *m,event_t_pd *pd,event_t *rep);

int is_event_t (event_t *rep);
typedef struct event_t_acc_s event_t_acc;
struct event_t_acc_s {
  Puint32_acc nerr;
  Pstring_acc state;
  Puint32_acc tstamp;
};

Perror_t event_t_acc_init (P_t *pads,event_t_acc *acc);

Perror_t event_t_acc_reset (P_t *pads,event_t_acc *acc);

Perror_t event_t_acc_cleanup (P_t *pads,event_t_acc *acc);

Perror_t event_t_acc_add (P_t *pads,event_t_acc *acc,event_t_pd *pd,event_t *rep);

Perror_t event_t_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,event_t_acc *acc);

Perror_t event_t_acc_report (P_t *pads,char const *prefix,char const *what,int nst,event_t_acc *acc);

ssize_t event_t_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,event_t_pd *pd,event_t *rep);

ssize_t event_t_write2io (P_t *pads,Sfio_t *io,event_t_pd *pd,event_t *rep);

ssize_t event_t_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,event_t_pd *pd,event_t *rep,char const *tag,int indent);

ssize_t event_t_write_xml_2io (P_t *pads,Sfio_t *io,event_t_pd *pd,event_t *rep,char const *tag,int indent);

ssize_t event_t_fmt2buf_final (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,event_t_m *m,event_t_pd *pd,event_t *rep);

ssize_t event_t_fmt2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,event_t_m *m,event_t_pd *pd,event_t *rep);

ssize_t event_t_fmt2io (P_t *pads,Sfio_t *io,int *requestedOut,char const *delims,event_t_m *m,event_t_pd *pd,event_t *rep);
typedef struct eventSeq_t_s eventSeq_t;
typedef struct eventSeq_t_m_s eventSeq_t_m;
typedef struct eventSeq_t_pd_s eventSeq_t_pd;
typedef struct eventSeq_t_ro_params_t_s eventSeq_t_ro_params_t;
struct eventSeq_t_m_s {
  event_t_m element;		/* per-element */
  Pbase_m compoundLevel;		/* entire array */
};
struct eventSeq_t_pd_s {
  Pflags_t pstate;
  Puint32 nerr;		/* Number of array errors */
  PerrCode_t errCode;
  Ploc_t loc;
  Puint32 neerr;		/* Number of element errors */
  Puint32 firstError;		/* if errCode == ARRAY_ELEM_ERR, index of first error */
  Puint32 numRead;		/* Number of elements read */
  Puint32 length;		/* Number of elements in memory */
  event_t_pd *elts;
  RBuf_t *_internal;
};
struct eventSeq_t_s {
  Puint32 length;
  event_t *elts;
  RBuf_t *_internal;
};
struct eventSeq_t_ro_params_t_s {
  Ploc_t beginLoc;		/* location of array beginning */
  Pregexp_t separator_regexp;
  Pregexp_t *separator_regexp_ptr;		/* pointer to separator_regexp */
  Pregexp_t terminator_regexp;
  Pregexp_t *terminator_regexp_ptr;		/* pointer to terminator_regexp */
};

Perror_t eventSeq_t_init (P_t *pads,eventSeq_t *rep);

Perror_t eventSeq_t_pd_init (P_t *pads,eventSeq_t_pd *pd);

Perror_t eventSeq_t_cleanup (P_t *pads,eventSeq_t *rep);

Perror_t eventSeq_t_pd_cleanup (P_t *pads,eventSeq_t_pd *pd);

Perror_t eventSeq_t_copy (P_t *pads,eventSeq_t *rep_dst,eventSeq_t *rep_src);

Perror_t eventSeq_t_pd_copy (P_t *pads,eventSeq_t_pd *pd_dst,eventSeq_t_pd *pd_src);

void eventSeq_t_m_init (P_t *pads,eventSeq_t_m *mask,Pbase_m baseMask);

Perror_t eventSeq_t_read_old (P_t *pads,eventSeq_t_m *m,eventSeq_t_pd *pd,eventSeq_t *rep);

void eventSeq_t_ro_params_init (eventSeq_t_ro_params_t *params);

Pread_res_t eventSeq_t_final_checks (P_t *pads,eventSeq_t_m *m,eventSeq_t_pd *pd,eventSeq_t *rep,Ploc_t *loc_ptr,Pregexp_t *separator_regexp_ptr,Pregexp_t *terminator_regexp_ptr,int foundTerm,int consume);

Pread_res_t eventSeq_t_read_one_init (P_t *pads,eventSeq_t_m *m,eventSeq_t_pd *pd,eventSeq_t *rep,Ploc_t *loc_ptr,Pregexp_t *separator_regexp_ptr,Pregexp_t *terminator_regexp_ptr);

Pread_res_t eventSeq_t_read_one (P_t *pads,eventSeq_t_m *m,eventSeq_t_pd *pd,eventSeq_t *rep,Ploc_t *loc_ptr,event_t_pd *elt_pd,event_t *elt_rep,Pregexp_t *separator_regexp_ptr,Pregexp_t *terminator_regexp_ptr);

Perror_t eventSeq_t_read (P_t *pads,eventSeq_t_m *m,eventSeq_t_pd *pd,eventSeq_t *rep);

Pread_res_t eventSeq_t_reread_one (Pregexp_t *separator_regexp_ptr,Pregexp_t *terminator_regexp_ptr,P_t *pads,eventSeq_t_m *m,eventSeq_t_pd *pd,eventSeq_t *rep,Ploc_t *loc_ptr,event_t_pd *elt_pd,event_t *elt_rep,int notFirstElt);

int is_eventSeq_t (eventSeq_t *rep);
typedef struct eventSeq_t_acc_s eventSeq_t_acc;
struct eventSeq_t_acc_s {
  Puint32_acc length;		/* Accumulator for array length */
  event_t_acc compoundLevel;		/* Accumulator for all array elements */
  event_t_acc arrayDetail[10];		/* Accumulator for first 10 array elements */
};

Perror_t eventSeq_t_acc_init (P_t *pads,eventSeq_t_acc *acc);

Perror_t eventSeq_t_acc_reset (P_t *pads,eventSeq_t_acc *acc);

Perror_t eventSeq_t_acc_cleanup (P_t *pads,eventSeq_t_acc *acc);

Perror_t eventSeq_t_acc_add (P_t *pads,eventSeq_t_acc *acc,eventSeq_t_pd *pd,eventSeq_t *rep);

Perror_t eventSeq_t_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,eventSeq_t_acc *acc);

Perror_t eventSeq_t_acc_report (P_t *pads,char const *prefix,char const *what,int nst,eventSeq_t_acc *acc);

ssize_t eventSeq_t_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,eventSeq_t_pd *pd,eventSeq_t *rep);

ssize_t eventSeq_t_write2io (P_t *pads,Sfio_t *io,eventSeq_t_pd *pd,eventSeq_t *rep);

ssize_t eventSeq_t_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,eventSeq_t_pd *pd,eventSeq_t *rep,char const *tag,int indent);

ssize_t eventSeq_t_write_xml_2io (P_t *pads,Sfio_t *io,eventSeq_t_pd *pd,eventSeq_t *rep,char const *tag,int indent);

ssize_t eventSeq_t_fmt2buf_final (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,eventSeq_t_m *m,eventSeq_t_pd *pd,eventSeq_t *rep);

ssize_t eventSeq_t_fmt2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,eventSeq_t_m *m,eventSeq_t_pd *pd,eventSeq_t *rep);

ssize_t eventSeq_t_fmt2io (P_t *pads,Sfio_t *io,int *requestedOut,char const *delims,eventSeq_t_m *m,eventSeq_t_pd *pd,eventSeq_t *rep);
/*@BEGIN dibbler_library.tex */
typedef struct {
  Pbase_m compoundLevel;   // Struct-level controls, eg., check Pwhere clause
  order_header_t_m h;	    
  eventSeq_t_m events;	
} entry_t_m;

typedef struct {
  Pflags_t pstate;         // Normal, Partial, or Panicking 
  Puint32 nerr;            // Number of detected errors.
  PerrCode_t errCode;      // Error code of first detected error
  Ploc_t loc;              // Location of first error
  order_header_t_pd h;     // Nested header information
  eventSeq_t_pd events;    // Nested event sequence information
} entry_t_pd;

typedef struct {
  order_header_t h;
  eventSeq_t events;
} entry_t;

/*@END dibbler_library.tex */

Perror_t entry_t_init (P_t *pads,entry_t *rep);

Perror_t entry_t_pd_init (P_t *pads,entry_t_pd *pd);

Perror_t entry_t_cleanup (P_t *pads,entry_t *rep);

Perror_t entry_t_pd_cleanup (P_t *pads,entry_t_pd *pd);

Perror_t entry_t_copy (P_t *pads,entry_t *rep_dst,entry_t *rep_src);

Perror_t entry_t_pd_copy (P_t *pads,entry_t_pd *pd_dst,entry_t_pd *pd_src);

/*@BEGIN dibbler_library.tex */
/* Selected utility functions */
void entry_t_m_init (P_t *pads,entry_t_m *mask,Pbase_m baseMask);
int entry_t_verify (entry_t *rep);
/*@END dibbler_library.tex */

/*@BEGIN dibbler_library.tex */
/* Core parsing library */
Perror_t entry_t_read (P_t *pads,entry_t_m *m,entry_t_pd *pd,entry_t *rep);
/*@END dibbler_library.tex */
typedef struct entry_t_acc_s entry_t_acc;
struct entry_t_acc_s {
  Puint32_acc nerr;
  order_header_t_acc h;
  eventSeq_t_acc events;
};

/*@BEGIN dibbler_library.tex */
/* Selected accumulator functions */
Perror_t entry_t_acc_init (P_t *pads,entry_t_acc *acc);
/*@END dibbler_library.tex */

Perror_t entry_t_acc_reset (P_t *pads,entry_t_acc *acc);

Perror_t entry_t_acc_cleanup (P_t *pads,entry_t_acc *acc);
/*@BEGIN dibbler_library.tex */
Perror_t entry_t_acc_add (P_t *pads,entry_t_acc *acc,entry_t_pd *pd,entry_t *rep);
/*@END dibbler_library.tex */

Perror_t entry_t_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,entry_t_acc *acc);
/*@BEGIN dibbler_library.tex */
Perror_t entry_t_acc_report (P_t *pads,char const *prefix,char const *what,
			     int nst,entry_t_acc *acc);
/*@END dibbler_library.tex */

ssize_t entry_t_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,entry_t_pd *pd,entry_t *rep);

/*@BEGIN dibbler_library.tex */
ssize_t entry_t_write2io (P_t *pads,Sfio_t *io,entry_t_pd *pd,entry_t *rep);
/*@END dibbler_library.tex */

ssize_t entry_t_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,entry_t_pd *pd,entry_t *rep,char const *tag,int indent);

ssize_t entry_t_write_xml_2io (P_t *pads,Sfio_t *io,entry_t_pd *pd,entry_t *rep,char const *tag,int indent);


ssize_t entry_t_fmt2buf_final (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,entry_t_m *m,entry_t_pd *pd,entry_t *rep);

ssize_t entry_t_fmt2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,
			 int *requestedOut,char const *delims,
			 entry_t_m *m,entry_t_pd *pd,entry_t *rep);

/*@BEGIN dibbler_library.tex */
/* Formatting */
ssize_t entry_t_fmt2io (P_t *pads,Sfio_t *io,int *requestedOut,
			char const *delims,entry_t_m *m,entry_t_pd *pd,entry_t *rep);
/*@END dibbler_library.tex */
typedef struct entries_t_s entries_t;
typedef struct entries_t_m_s entries_t_m;
typedef struct entries_t_pd_s entries_t_pd;
typedef struct entries_t_ro_params_t_s entries_t_ro_params_t;
struct entries_t_m_s {
  entry_t_m element;		/* per-element */
  Pbase_m compoundLevel;		/* entire array */
};
struct entries_t_pd_s {
  Pflags_t pstate;
  Puint32 nerr;		/* Number of array errors */
  PerrCode_t errCode;
  Ploc_t loc;
  Puint32 neerr;		/* Number of element errors */
  Puint32 firstError;		/* if errCode == ARRAY_ELEM_ERR, index of first error */
  Puint32 numRead;		/* Number of elements read */
  Puint32 length;		/* Number of elements in memory */
  entry_t_pd *elts;
  RBuf_t *_internal;
};
struct entries_t_s {
  Puint32 length;
  entry_t *elts;
  RBuf_t *_internal;
};
struct entries_t_ro_params_t_s {
  Ploc_t beginLoc;		/* location of array beginning */
};

Perror_t entries_t_init (P_t *pads,entries_t *rep);

Perror_t entries_t_pd_init (P_t *pads,entries_t_pd *pd);

Perror_t entries_t_cleanup (P_t *pads,entries_t *rep);

Perror_t entries_t_pd_cleanup (P_t *pads,entries_t_pd *pd);

Perror_t entries_t_copy (P_t *pads,entries_t *rep_dst,entries_t *rep_src);

Perror_t entries_t_pd_copy (P_t *pads,entries_t_pd *pd_dst,entries_t_pd *pd_src);

void entries_t_m_init (P_t *pads,entries_t_m *mask,Pbase_m baseMask);

Perror_t entries_t_read_old (P_t *pads,entries_t_m *m,entries_t_pd *pd,entries_t *rep);

void entries_t_ro_params_init (entries_t_ro_params_t *params);

Pread_res_t entries_t_final_checks (P_t *pads,entries_t_m *m,entries_t_pd *pd,entries_t *rep,Ploc_t *loc_ptr,int consume);

Pread_res_t entries_t_read_one_init (P_t *pads,entries_t_m *m,entries_t_pd *pd,entries_t *rep,Ploc_t *loc_ptr);

Pread_res_t entries_t_read_one (P_t *pads,entries_t_m *m,entries_t_pd *pd,entries_t *rep,Ploc_t *loc_ptr,entry_t_pd *elt_pd,entry_t *elt_rep);

Perror_t entries_t_read (P_t *pads,entries_t_m *m,entries_t_pd *pd,entries_t *rep);

Pread_res_t entries_t_reread_one (P_t *pads,entries_t_m *m,entries_t_pd *pd,entries_t *rep,Ploc_t *loc_ptr,entry_t_pd *elt_pd,entry_t *elt_rep,int notFirstElt);

int is_entries_t (entries_t *rep);
typedef struct entries_t_acc_s entries_t_acc;
struct entries_t_acc_s {
  Puint32_acc length;		/* Accumulator for array length */
  entry_t_acc compoundLevel;		/* Accumulator for all array elements */
  entry_t_acc arrayDetail[10];		/* Accumulator for first 10 array elements */
};

Perror_t entries_t_acc_init (P_t *pads,entries_t_acc *acc);

Perror_t entries_t_acc_reset (P_t *pads,entries_t_acc *acc);

Perror_t entries_t_acc_cleanup (P_t *pads,entries_t_acc *acc);

Perror_t entries_t_acc_add (P_t *pads,entries_t_acc *acc,entries_t_pd *pd,entries_t *rep);

Perror_t entries_t_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,entries_t_acc *acc);

Perror_t entries_t_acc_report (P_t *pads,char const *prefix,char const *what,int nst,entries_t_acc *acc);

ssize_t entries_t_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,entries_t_pd *pd,entries_t *rep);

ssize_t entries_t_write2io (P_t *pads,Sfio_t *io,entries_t_pd *pd,entries_t *rep);

ssize_t entries_t_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,entries_t_pd *pd,entries_t *rep,char const *tag,int indent);

ssize_t entries_t_write_xml_2io (P_t *pads,Sfio_t *io,entries_t_pd *pd,entries_t *rep,char const *tag,int indent);

ssize_t entries_t_fmt2buf_final (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,entries_t_m *m,entries_t_pd *pd,entries_t *rep);

ssize_t entries_t_fmt2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,entries_t_m *m,entries_t_pd *pd,entries_t *rep);

ssize_t entries_t_fmt2io (P_t *pads,Sfio_t *io,int *requestedOut,char const *delims,entries_t_m *m,entries_t_pd *pd,entries_t *rep);
typedef struct out_sum_s out_sum;
typedef struct out_sum_m_s out_sum_m;
typedef struct out_sum_pd_s out_sum_pd;
struct out_sum_m_s {
  Pbase_m compoundLevel;
  summary_header_t_m h;		/* nested constraints */
  entries_t_m es;		/* nested constraints */
};
struct out_sum_pd_s {
  Pflags_t pstate;
  Puint32 nerr;
  PerrCode_t errCode;
  Ploc_t loc;
  summary_header_t_pd h;
  entries_t_pd es;
};
struct out_sum_s {
  summary_header_t h;
  entries_t es;
};

Perror_t out_sum_init (P_t *pads,out_sum *rep);

Perror_t out_sum_pd_init (P_t *pads,out_sum_pd *pd);

Perror_t out_sum_cleanup (P_t *pads,out_sum *rep);

Perror_t out_sum_pd_cleanup (P_t *pads,out_sum_pd *pd);

Perror_t out_sum_copy (P_t *pads,out_sum *rep_dst,out_sum *rep_src);

Perror_t out_sum_pd_copy (P_t *pads,out_sum_pd *pd_dst,out_sum_pd *pd_src);

void out_sum_m_init (P_t *pads,out_sum_m *mask,Pbase_m baseMask);

Perror_t out_sum_read (P_t *pads,out_sum_m *m,out_sum_pd *pd,out_sum *rep);

int is_out_sum (out_sum *rep);
typedef struct out_sum_acc_s out_sum_acc;
struct out_sum_acc_s {
  Puint32_acc nerr;
  summary_header_t_acc h;
  entries_t_acc es;
};

Perror_t out_sum_acc_init (P_t *pads,out_sum_acc *acc);

Perror_t out_sum_acc_reset (P_t *pads,out_sum_acc *acc);

Perror_t out_sum_acc_cleanup (P_t *pads,out_sum_acc *acc);

Perror_t out_sum_acc_add (P_t *pads,out_sum_acc *acc,out_sum_pd *pd,out_sum *rep);

Perror_t out_sum_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,out_sum_acc *acc);

Perror_t out_sum_acc_report (P_t *pads,char const *prefix,char const *what,int nst,out_sum_acc *acc);

ssize_t out_sum_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,out_sum_pd *pd,out_sum *rep);

ssize_t out_sum_write2io (P_t *pads,Sfio_t *io,out_sum_pd *pd,out_sum *rep);

ssize_t out_sum_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,out_sum_pd *pd,out_sum *rep,char const *tag,int indent);

ssize_t out_sum_write_xml_2io (P_t *pads,Sfio_t *io,out_sum_pd *pd,out_sum *rep,char const *tag,int indent);

ssize_t out_sum_fmt2buf_final (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,out_sum_m *m,out_sum_pd *pd,out_sum *rep);

ssize_t out_sum_fmt2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,out_sum_m *m,out_sum_pd *pd,out_sum *rep);

ssize_t out_sum_fmt2io (P_t *pads,Sfio_t *io,int *requestedOut,char const *delims,out_sum_m *m,out_sum_pd *pd,out_sum *rep);

void P_lib_init ();

#endif /*  __DIBBLER_NEW__H__  */
