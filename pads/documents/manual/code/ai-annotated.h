/*@FILE @LEFT ai.httpRequestRep.tex  ai.httpRequestCSM.tex ai.httpRequestPD.tex ai.httpRequestOps.tex ai.arrayRep.tex ai.arrayPD.tex ai.arrayMask.tex ai.arrayOps.tex */
#ifndef __AI__H__
#define __AI__H__
#include "pads.h"
typedef struct nIP_s nIP;

/*@BEGIN ai.arrayMask.tex*/
typedef struct nIP_m_s nIP_m;

struct nIP_m_s {
  Pbase_m element;		/* per-element */
  Pbase_m arrayLevel;		/* entire array */
};
/*@END ai.arrayMask.tex*/

/*@BEGIN ai.arrayPD.tex*/
typedef struct nIP_pd_s nIP_pd;

struct nIP_pd_s {
  Pflags_t pstate;
  Puint32 nerr;		        /* Number of array errors */
  PerrCode_t errCode;
  Ploc_t loc;
  Puint32 neerr;		/* Number of element errors */
  Puint32 firstError;		/* if errCode == ARRAY_ELEM_ERR, 
				   index of first error */
  Puint32 numRead;		/* Number of elements read */
  Puint32 length;		/* Number of elements in memory */
  Pbase_pd *elts;
  RBuf_t *_internal;
};
/*@END ai.arrayPD.tex*/

/*@BEGIN ai.arrayRep.tex*/
typedef struct nIP_s nIP;

struct nIP_s {
  Puint32 length;
  Puint8 *elts;
  RBuf_t *_internal;
};
/*@END ai.arrayRep.tex*/

/*@BEGIN ai.arrayOps.tex */
Perror_t nIP_init (P_t *pads,nIP *rep);

Perror_t nIP_pd_init (P_t *pads,nIP_pd *pd);

Perror_t nIP_cleanup (P_t *pads,nIP *rep);

Perror_t nIP_pd_cleanup (P_t *pads,nIP_pd *pd);

Perror_t nIP_copy (P_t *pads,nIP *rep_dst,nIP *rep_src);

Perror_t nIP_pd_copy (P_t *pads,nIP_pd *pd_dst,nIP_pd *pd_src);

void nIP_m_init (P_t *pads,nIP_m *mask,Pbase_m baseMask);

Perror_t nIP_read (P_t *pads,nIP_m *m,nIP_pd *pd,nIP *rep);

int nIP_verify (nIP *rep);

int nIP_genPD (P_t *pads, nIP *rep, nIP_pd *pd);
/*@END ai.arrayOps.tex */

typedef struct nIP_acc_s nIP_acc;
struct nIP_acc_s {
  Puint32_acc length;		/* Accumulator for array length */
  Puint8_acc arrayLevel;		/* Accumulator for all array elements */
  Puint8_acc arrayDetail[4];		/* Accumulator for first 4 array elements */
};

Perror_t nIP_acc_init (P_t *pads,nIP_acc *acc);

Perror_t nIP_acc_reset (P_t *pads,nIP_acc *acc);

Perror_t nIP_acc_cleanup (P_t *pads,nIP_acc *acc);

Perror_t nIP_acc_add (P_t *pads,nIP_acc *acc,nIP_pd *pd,nIP *rep);

Perror_t nIP_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,nIP_acc *acc);

Perror_t nIP_acc_report (P_t *pads,char const *prefix,char const *what,int nst,nIP_acc *acc);

ssize_t nIP_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,nIP_pd *pd,nIP *rep);

ssize_t nIP_write2io (P_t *pads,Sfio_t *io,nIP_pd *pd,nIP *rep);

ssize_t nIP_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,nIP_pd *pd,nIP *rep,char const *tag,int indent);

ssize_t nIP_write_xml_2io (P_t *pads,Sfio_t *io,nIP_pd *pd,nIP *rep,char const *tag,int indent);
typedef struct sIP_s sIP;
typedef struct sIP_m_s sIP_m;
typedef struct sIP_pd_s sIP_pd;
struct sIP_m_s {
  Pbase_m element;		/* per-element */
  Pbase_m arrayLevel;		/* entire array */
};
struct sIP_pd_s {
  Pflags_t pstate;
  Puint32 nerr;		/* Number of array errors */
  PerrCode_t errCode;
  Ploc_t loc;
  Puint32 neerr;		/* Number of element errors */
  Puint32 firstError;		/* if errCode == ARRAY_ELEM_ERR, index of first error */
  Puint32 numRead;		/* Number of elements read */
  Puint32 length;		/* Number of elements in memory */
  Pbase_pd *elts;
  RBuf_t *_internal;
};
struct sIP_s {
  Puint32 length;
  Pstring *elts;
  RBuf_t *_internal;
};

Perror_t sIP_init (P_t *pads,sIP *rep);

Perror_t sIP_pd_init (P_t *pads,sIP_pd *pd);

Perror_t sIP_cleanup (P_t *pads,sIP *rep);

Perror_t sIP_pd_cleanup (P_t *pads,sIP_pd *pd);

Perror_t sIP_copy (P_t *pads,sIP *rep_dst,sIP *rep_src);

Perror_t sIP_pd_copy (P_t *pads,sIP_pd *pd_dst,sIP_pd *pd_src);

void sIP_m_init (P_t *pads,sIP_m *mask,Pbase_m baseMask);

Perror_t sIP_read (P_t *pads,sIP_m *m,sIP_pd *pd,sIP *rep);

int sIP_verify (sIP *rep);
typedef struct sIP_acc_s sIP_acc;
struct sIP_acc_s {
  Puint32_acc length;		/* Accumulator for array length */
  Pstring_acc arrayLevel;		/* Accumulator for all array elements */
  Pstring_acc arrayDetail[10];		/* Accumulator for first 10 array elements */
};

Perror_t sIP_acc_init (P_t *pads,sIP_acc *acc);

Perror_t sIP_acc_reset (P_t *pads,sIP_acc *acc);

Perror_t sIP_acc_cleanup (P_t *pads,sIP_acc *acc);

Perror_t sIP_acc_add (P_t *pads,sIP_acc *acc,sIP_pd *pd,sIP *rep);

Perror_t sIP_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,sIP_acc *acc);

Perror_t sIP_acc_report (P_t *pads,char const *prefix,char const *what,int nst,sIP_acc *acc);

ssize_t sIP_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,sIP_pd *pd,sIP *rep);

ssize_t sIP_write2io (P_t *pads,Sfio_t *io,sIP_pd *pd,sIP *rep);

ssize_t sIP_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,sIP_pd *pd,sIP *rep,char const *tag,int indent);

ssize_t sIP_write_xml_2io (P_t *pads,Sfio_t *io,sIP_pd *pd,sIP *rep,char const *tag,int indent);
typedef enum host_t_tag_e host_t_tag;
typedef union host_t_u_u host_t_u;
typedef struct host_t_s host_t;
typedef struct host_t_m_s host_t_m;
typedef union host_t_pd_u_u host_t_pd_u;
typedef struct host_t_pd_s host_t_pd;
enum host_t_tag_e {
  host_t_err=0,
  resolved=1,
  symbolic=2
  };
union host_t_pd_u_u {
  nIP_pd resolved;
  sIP_pd symbolic;
};
struct host_t_pd_s {
  Pflags_t pstate;
  Puint32 nerr;
  PerrCode_t errCode;
  Ploc_t loc;
  host_t_tag tag;
  host_t_pd_u val;
};
union host_t_u_u {
  nIP resolved;		/*  135.207.23.32 */
  sIP symbolic;		/*  www.research.att.com */
};
struct host_t_s {
  host_t_tag tag;
  host_t_u val;
};
struct host_t_m_s {
  Pbase_m unionLevel;
  nIP_m resolved;		/* nested constraints */
  sIP_m symbolic;		/* nested constraints */
};

char const *host_t_tag2str (host_t_tag which);

Perror_t host_t_init (P_t *pads,host_t *rep);

Perror_t host_t_pd_init (P_t *pads,host_t_pd *pd);

Perror_t host_t_cleanup (P_t *pads,host_t *rep);

Perror_t host_t_pd_cleanup (P_t *pads,host_t_pd *pd);

Perror_t host_t_copy (P_t *pads,host_t *rep_dst,host_t *rep_src);

Perror_t host_t_pd_copy (P_t *pads,host_t_pd *pd_dst,host_t_pd *pd_src);

void host_t_m_init (P_t *pads,host_t_m *mask,Pbase_m baseMask);

Perror_t host_t_read (P_t *pads,host_t_m *m,host_t_pd *pd,host_t *rep);

int host_t_verify (host_t *rep);
typedef struct host_t_acc_s host_t_acc;
struct host_t_acc_s {
  Pint32_acc tag;
  nIP_acc resolved;
  sIP_acc symbolic;
};

Perror_t host_t_acc_init (P_t *pads,host_t_acc *acc);

Perror_t host_t_acc_reset (P_t *pads,host_t_acc *acc);

Perror_t host_t_acc_cleanup (P_t *pads,host_t_acc *acc);

Perror_t host_t_acc_add (P_t *pads,host_t_acc *acc,host_t_pd *pd,host_t *rep);

Perror_t host_t_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,host_t_acc *acc);

Perror_t host_t_acc_report (P_t *pads,char const *prefix,char const *what,int nst,host_t_acc *acc);

ssize_t host_t_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,host_t_pd *pd,host_t *rep);

ssize_t host_t_write2io (P_t *pads,Sfio_t *io,host_t_pd *pd,host_t *rep);

ssize_t host_t_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,host_t_pd *pd,host_t *rep,char const *tag,int indent);

ssize_t host_t_write_xml_2io (P_t *pads,Sfio_t *io,host_t_pd *pd,host_t *rep,char const *tag,int indent);
typedef Pchar unknown_t;
typedef struct unknown_t_m_s unknown_t_m;
typedef Pbase_pd unknown_t_pd;
struct unknown_t_m_s {
  Pbase_m base;		/* Base mask */
  Pbase_m user;		/* Typedef mask */
};

Perror_t unknown_t_init (P_t *pads,unknown_t *rep);

Perror_t unknown_t_pd_init (P_t *pads,unknown_t_pd *pd);

Perror_t unknown_t_cleanup (P_t *pads,unknown_t *rep);

Perror_t unknown_t_pd_cleanup (P_t *pads,unknown_t_pd *pd);

Perror_t unknown_t_copy (P_t *pads,unknown_t *rep_dst,unknown_t *rep_src);

Perror_t unknown_t_pd_copy (P_t *pads,unknown_t_pd *pd_dst,unknown_t_pd *pd_src);

void unknown_t_m_init (P_t *pads,unknown_t_m *mask,Pbase_m baseMask);

Perror_t unknown_t_read (P_t *pads,unknown_t_m *m,unknown_t_pd *pd,unknown_t *rep);

int unknown_t_verify (unknown_t *rep);
typedef Pchar_acc unknown_t_acc;

Perror_t unknown_t_acc_init (P_t *pads,unknown_t_acc *acc);

Perror_t unknown_t_acc_reset (P_t *pads,unknown_t_acc *acc);

Perror_t unknown_t_acc_cleanup (P_t *pads,unknown_t_acc *acc);

Perror_t unknown_t_acc_add (P_t *pads,unknown_t_acc *acc,unknown_t_pd *pd,unknown_t *rep);

Perror_t unknown_t_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,unknown_t_acc *acc);

Perror_t unknown_t_acc_report (P_t *pads,char const *prefix,char const *what,int nst,unknown_t_acc *acc);

ssize_t unknown_t_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,unknown_t_pd *pd,unknown_t *rep);

ssize_t unknown_t_write2io (P_t *pads,Sfio_t *io,unknown_t_pd *pd,unknown_t *rep);

ssize_t unknown_t_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,unknown_t_pd *pd,unknown_t *rep,char const *tag,int indent);

ssize_t unknown_t_write_xml_2io (P_t *pads,Sfio_t *io,unknown_t_pd *pd,unknown_t *rep,char const *tag,int indent);
typedef enum auth_id_t_tag_e auth_id_t_tag;
typedef union auth_id_t_u_u auth_id_t_u;
typedef struct auth_id_t_s auth_id_t;
typedef struct auth_id_t_m_s auth_id_t_m;
typedef union auth_id_t_pd_u_u auth_id_t_pd_u;
typedef struct auth_id_t_pd_s auth_id_t_pd;
enum auth_id_t_tag_e {
  auth_id_t_err=0,
  unauthorized=1,
  id=2
  };
union auth_id_t_pd_u_u {
  unknown_t_pd unauthorized;
  Pbase_pd id;
};
struct auth_id_t_pd_s {
  Pflags_t pstate;
  Puint32 nerr;
  PerrCode_t errCode;
  Ploc_t loc;
  auth_id_t_tag tag;
  auth_id_t_pd_u val;
};
union auth_id_t_u_u {
  unknown_t unauthorized;		/*  non-authenticated http session */
  Pstring id;		/*  login supplied during authentication */
};
struct auth_id_t_s {
  auth_id_t_tag tag;
  auth_id_t_u val;
};
struct auth_id_t_m_s {
  Pbase_m unionLevel;
  unknown_t_m unauthorized;		/* nested constraints */
  Pbase_m id;		/* nested constraints */
};

char const *auth_id_t_tag2str (auth_id_t_tag which);

Perror_t auth_id_t_init (P_t *pads,auth_id_t *rep);

Perror_t auth_id_t_pd_init (P_t *pads,auth_id_t_pd *pd);

Perror_t auth_id_t_cleanup (P_t *pads,auth_id_t *rep);

Perror_t auth_id_t_pd_cleanup (P_t *pads,auth_id_t_pd *pd);

Perror_t auth_id_t_copy (P_t *pads,auth_id_t *rep_dst,auth_id_t *rep_src);

Perror_t auth_id_t_pd_copy (P_t *pads,auth_id_t_pd *pd_dst,auth_id_t_pd *pd_src);

void auth_id_t_m_init (P_t *pads,auth_id_t_m *mask,Pbase_m baseMask);

Perror_t auth_id_t_read (P_t *pads,auth_id_t_m *m,auth_id_t_pd *pd,auth_id_t *rep);

int auth_id_t_verify (auth_id_t *rep);
typedef struct auth_id_t_acc_s auth_id_t_acc;
struct auth_id_t_acc_s {
  Pint32_acc tag;
  unknown_t_acc unauthorized;
  Pstring_acc id;
};

Perror_t auth_id_t_acc_init (P_t *pads,auth_id_t_acc *acc);

Perror_t auth_id_t_acc_reset (P_t *pads,auth_id_t_acc *acc);

Perror_t auth_id_t_acc_cleanup (P_t *pads,auth_id_t_acc *acc);

Perror_t auth_id_t_acc_add (P_t *pads,auth_id_t_acc *acc,auth_id_t_pd *pd,auth_id_t *rep);

Perror_t auth_id_t_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,auth_id_t_acc *acc);

Perror_t auth_id_t_acc_report (P_t *pads,char const *prefix,char const *what,int nst,auth_id_t_acc *acc);

ssize_t auth_id_t_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,auth_id_t_pd *pd,auth_id_t *rep);

ssize_t auth_id_t_write2io (P_t *pads,Sfio_t *io,auth_id_t_pd *pd,auth_id_t *rep);

ssize_t auth_id_t_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,auth_id_t_pd *pd,auth_id_t *rep,char const *tag,int indent);

ssize_t auth_id_t_write_xml_2io (P_t *pads,Sfio_t *io,auth_id_t_pd *pd,auth_id_t *rep,char const *tag,int indent);
typedef enum contentOpt_t_tag_e contentOpt_t_tag;
typedef union contentOpt_t_u_u contentOpt_t_u;
typedef struct contentOpt_t_s contentOpt_t;
typedef struct contentOpt_t_m_s contentOpt_t_m;
typedef union contentOpt_t_pd_u_u contentOpt_t_pd_u;
typedef struct contentOpt_t_pd_s contentOpt_t_pd;
enum contentOpt_t_tag_e {
  contentOpt_t_err=0,
  len=1,
  unavailable=2
  };
union contentOpt_t_pd_u_u {
  Pbase_pd len;
  unknown_t_pd unavailable;
};
struct contentOpt_t_pd_s {
  Pflags_t pstate;
  Puint32 nerr;
  PerrCode_t errCode;
  Ploc_t loc;
  contentOpt_t_tag tag;
  contentOpt_t_pd_u val;
};
union contentOpt_t_u_u {
  Puint32 len;		/*  length available */
  unknown_t unavailable;
};
struct contentOpt_t_s {
  contentOpt_t_tag tag;
  contentOpt_t_u val;
};
struct contentOpt_t_m_s {
  Pbase_m unionLevel;
  Pbase_m len;		/* nested constraints */
  unknown_t_m unavailable;		/* nested constraints */
};

char const *contentOpt_t_tag2str (contentOpt_t_tag which);

Perror_t contentOpt_t_init (P_t *pads,contentOpt_t *rep);

Perror_t contentOpt_t_pd_init (P_t *pads,contentOpt_t_pd *pd);

Perror_t contentOpt_t_cleanup (P_t *pads,contentOpt_t *rep);

Perror_t contentOpt_t_pd_cleanup (P_t *pads,contentOpt_t_pd *pd);

Perror_t contentOpt_t_copy (P_t *pads,contentOpt_t *rep_dst,contentOpt_t *rep_src);

Perror_t contentOpt_t_pd_copy (P_t *pads,contentOpt_t_pd *pd_dst,contentOpt_t_pd *pd_src);

void contentOpt_t_m_init (P_t *pads,contentOpt_t_m *mask,Pbase_m baseMask);

Perror_t contentOpt_t_read (P_t *pads,contentOpt_t_m *m,contentOpt_t_pd *pd,contentOpt_t *rep);

int contentOpt_t_verify (contentOpt_t *rep);
typedef struct contentOpt_t_acc_s contentOpt_t_acc;
struct contentOpt_t_acc_s {
  Pint32_acc tag;
  Puint32_acc len;
  unknown_t_acc unavailable;
};

Perror_t contentOpt_t_acc_init (P_t *pads,contentOpt_t_acc *acc);

Perror_t contentOpt_t_acc_reset (P_t *pads,contentOpt_t_acc *acc);

Perror_t contentOpt_t_acc_cleanup (P_t *pads,contentOpt_t_acc *acc);

Perror_t contentOpt_t_acc_add (P_t *pads,contentOpt_t_acc *acc,contentOpt_t_pd *pd,contentOpt_t *rep);

Perror_t contentOpt_t_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,contentOpt_t_acc *acc);

Perror_t contentOpt_t_acc_report (P_t *pads,char const *prefix,char const *what,int nst,contentOpt_t_acc *acc);

ssize_t contentOpt_t_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,contentOpt_t_pd *pd,contentOpt_t *rep);

ssize_t contentOpt_t_write2io (P_t *pads,Sfio_t *io,contentOpt_t_pd *pd,contentOpt_t *rep);

ssize_t contentOpt_t_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,contentOpt_t_pd *pd,contentOpt_t *rep,char const *tag,int indent);

ssize_t contentOpt_t_write_xml_2io (P_t *pads,Sfio_t *io,contentOpt_t_pd *pd,contentOpt_t *rep,char const *tag,int indent);
typedef struct http_v_t_s http_v_t;
typedef struct http_v_t_m_s http_v_t_m;
typedef struct http_v_t_pd_s http_v_t_pd;
struct http_v_t_m_s {
  Pbase_m structLevel;
  Pbase_m major;		/* nested constraints */
  Pbase_m minor;		/* nested constraints */
};
struct http_v_t_pd_s {
  Pflags_t pstate;
  Puint32 nerr;
  PerrCode_t errCode;
  Ploc_t loc;
  Pbase_pd major;
  Pbase_pd minor;
};
struct http_v_t_s {
  Puint8 major;
  Puint8 minor;		/*  http minor mode */
};

Perror_t http_v_t_init (P_t *pads,http_v_t *rep);

Perror_t http_v_t_pd_init (P_t *pads,http_v_t_pd *pd);

Perror_t http_v_t_cleanup (P_t *pads,http_v_t *rep);

Perror_t http_v_t_pd_cleanup (P_t *pads,http_v_t_pd *pd);

Perror_t http_v_t_copy (P_t *pads,http_v_t *rep_dst,http_v_t *rep_src);

Perror_t http_v_t_pd_copy (P_t *pads,http_v_t_pd *pd_dst,http_v_t_pd *pd_src);

void http_v_t_m_init (P_t *pads,http_v_t_m *mask,Pbase_m baseMask);

Perror_t http_v_t_read (P_t *pads,http_v_t_m *m,http_v_t_pd *pd,http_v_t *rep);

int http_v_t_verify (http_v_t *rep);
typedef struct http_v_t_acc_s http_v_t_acc;
struct http_v_t_acc_s {
  Puint32_acc nerr;
  Puint8_acc major;
  Puint8_acc minor;		/*  http minor mode */
};

Perror_t http_v_t_acc_init (P_t *pads,http_v_t_acc *acc);

Perror_t http_v_t_acc_reset (P_t *pads,http_v_t_acc *acc);

Perror_t http_v_t_acc_cleanup (P_t *pads,http_v_t_acc *acc);

Perror_t http_v_t_acc_add (P_t *pads,http_v_t_acc *acc,http_v_t_pd *pd,http_v_t *rep);

Perror_t http_v_t_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,http_v_t_acc *acc);

Perror_t http_v_t_acc_report (P_t *pads,char const *prefix,char const *what,int nst,http_v_t_acc *acc);

ssize_t http_v_t_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,http_v_t_pd *pd,http_v_t *rep);

ssize_t http_v_t_write2io (P_t *pads,Sfio_t *io,http_v_t_pd *pd,http_v_t *rep);

ssize_t http_v_t_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,http_v_t_pd *pd,http_v_t *rep,char const *tag,int indent);

ssize_t http_v_t_write_xml_2io (P_t *pads,Sfio_t *io,http_v_t_pd *pd,http_v_t *rep,char const *tag,int indent);
typedef enum http_method_t_e http_method_t;
typedef Pbase_m http_method_t_m;
typedef Pbase_pd http_method_t_pd;
enum http_method_t_e {
  GET=0,
  PUT=1,
  POST=2,
  HEAD=3,
  DELETE=4,
  LINK=5,		/*  Unused after http 1.1 */
  UNLINK=6		/*  Unused after http 1.1 */
  };

char const *http_method_t2str (http_method_t which);

Perror_t http_method_t_init (P_t *pads,http_method_t *rep);

Perror_t http_method_t_pd_init (P_t *pads,http_method_t_pd *pd);

Perror_t http_method_t_cleanup (P_t *pads,http_method_t *rep);

Perror_t http_method_t_pd_cleanup (P_t *pads,http_method_t_pd *pd);

Perror_t http_method_t_copy (P_t *pads,http_method_t *rep_dst,http_method_t *rep_src);

Perror_t http_method_t_pd_copy (P_t *pads,http_method_t_pd *pd_dst,http_method_t_pd *pd_src);

void http_method_t_m_init (P_t *pads,http_method_t_m *mask,Pbase_m baseMask);

Perror_t http_method_t_read (P_t *pads,http_method_t_m *m,http_method_t_pd *pd,http_method_t *rep);

int http_method_t_verify (http_method_t *rep);
typedef Pint32_acc http_method_t_acc;

Perror_t http_method_t_acc_init (P_t *pads,http_method_t_acc *acc);

Perror_t http_method_t_acc_reset (P_t *pads,http_method_t_acc *acc);

Perror_t http_method_t_acc_cleanup (P_t *pads,http_method_t_acc *acc);

Perror_t http_method_t_acc_add (P_t *pads,http_method_t_acc *acc,http_method_t_pd *pd,http_method_t *rep);

Perror_t http_method_t_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,http_method_t_acc *acc);

Perror_t http_method_t_acc_report (P_t *pads,char const *prefix,char const *what,int nst,http_method_t_acc *acc);

ssize_t http_method_t_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,http_method_t_pd *pd,http_method_t *rep);

ssize_t http_method_t_write2io (P_t *pads,Sfio_t *io,http_method_t_pd *pd,http_method_t *rep);

ssize_t http_method_t_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,http_method_t_pd *pd,http_method_t *rep,char const *tag,int indent);

ssize_t http_method_t_write_xml_2io (P_t *pads,Sfio_t *io,http_method_t_pd *pd,http_method_t *rep,char const *tag,int indent);

int checkVersion (http_v_t version,http_method_t meth);
typedef struct http_request_t_s http_request_t;



/*@BEGIN ai.httpRequestCSM.tex*/
typedef struct http_request_t_m_s http_request_t_m;

struct http_request_t_m_s {
  Pbase_m structLevel;
  http_method_t_m meth;		/* nested constraints */
  Pbase_m req_uri;		/* nested constraints */
  http_v_t_m version;		/* nested constraints */
  Pbase_m version_con;		/* struct constraints */
};
/*@END ai.httpRequestCSM.tex*/

/*@BEGIN ai.httpRequestPD.tex*/
typedef struct http_request_t_pd_s http_request_t_pd;

struct http_request_t_pd_s {
  Pflags_t pstate;
  Puint32 nerr;
  PerrCode_t errCode;
  Ploc_t loc;
  http_method_t_pd meth;
  Pbase_pd req_uri;
  http_v_t_pd version;
};
/*@END ai.httpRequestPD.tex*/

struct http_request_t_s {
  http_method_t meth;		/*  Method used during request */
  Pstring req_uri;		/*  Requested uri. */
  http_v_t version;		/*  HTTP version. checkVersion(version, meth) */
};

/*@BEGIN ai.httpRequestOps.tex */
Perror_t http_request_t_init (P_t *pads,http_request_t *rep);

Perror_t http_request_t_pd_init (P_t *pads,http_request_t_pd *pd);

Perror_t http_request_t_cleanup (P_t *pads,http_request_t *rep);

Perror_t http_request_t_pd_cleanup (P_t *pads,http_request_t_pd *pd);

Perror_t http_request_t_copy (P_t *pads,http_request_t *rep_dst,
			      http_request_t *rep_src);

Perror_t http_request_t_pd_copy (P_t *pads,http_request_t_pd *pd_dst,
				 http_request_t_pd *pd_src);

void http_request_t_m_init (P_t *pads,http_request_t_m *mask,
			    Pbase_m baseMask);

Perror_t http_request_t_read (P_t *pads,http_request_t_m *m,
			      http_request_t_pd *pd,http_request_t *rep);

ssize_t http_request_t_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,
				  http_request_t_pd *pd,http_request_t *rep);

ssize_t http_request_t_write2io (P_t *pads,Sfio_t *io,
				 http_request_t_pd *pd,http_request_t *rep);

int http_request_t_verify (http_request_t *rep);

int http_request_t_genPD (P_t *pads, http_request_t *rep, 
			  http_request_t_pd *pd);
/*@END ai.httpRequestOps.tex*/

typedef struct http_request_t_acc_s http_request_t_acc;
struct http_request_t_acc_s {
  Puint32_acc nerr;
  http_method_t_acc meth;		/*  Method used during request */
  Pstring_acc req_uri;		/*  Requested uri. */
  http_v_t_acc version;		/*  HTTP version. checkVersion(version, meth) */
};

Perror_t http_request_t_acc_init (P_t *pads,http_request_t_acc *acc);

Perror_t http_request_t_acc_reset (P_t *pads,http_request_t_acc *acc);

Perror_t http_request_t_acc_cleanup (P_t *pads,http_request_t_acc *acc);

Perror_t http_request_t_acc_add (P_t *pads,http_request_t_acc *acc,http_request_t_pd *pd,http_request_t *rep);

Perror_t http_request_t_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,http_request_t_acc *acc);

Perror_t http_request_t_acc_report (P_t *pads,char const *prefix,char const *what,int nst,http_request_t_acc *acc);

ssize_t http_request_t_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,http_request_t_pd *pd,http_request_t *rep);

ssize_t http_request_t_write2io (P_t *pads,Sfio_t *io,http_request_t_pd *pd,http_request_t *rep);

ssize_t http_request_t_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,http_request_t_pd *pd,http_request_t *rep,char const *tag,int indent);

ssize_t http_request_t_write_xml_2io (P_t *pads,Sfio_t *io,http_request_t_pd *pd,http_request_t *rep,char const *tag,int indent);
typedef struct http_clf_t_s http_clf_t;
typedef struct http_clf_t_m_s http_clf_t_m;
typedef struct http_clf_t_pd_s http_clf_t_pd;
struct http_clf_t_m_s {
  Pbase_m structLevel;
  host_t_m host;		/* nested constraints */
  auth_id_t_m remoteID;		/* nested constraints */
  auth_id_t_m auth;		/* nested constraints */
  Pbase_m date;		/* nested constraints */
  http_request_t_m request;		/* nested constraints */
  Pbase_m response;		/* nested constraints */
  contentOpt_t_m contentLength;		/* nested constraints */
};
struct http_clf_t_pd_s {
  Pflags_t pstate;
  Puint32 nerr;
  PerrCode_t errCode;
  Ploc_t loc;
  host_t_pd host;
  auth_id_t_pd remoteID;
  auth_id_t_pd auth;
  Pbase_pd date;
  http_request_t_pd request;
  Pbase_pd response;
  contentOpt_t_pd contentLength;
};
struct http_clf_t_s {
  host_t host;		/*  IP address of client requesting service */
  auth_id_t remoteID;		/*  Remote identity; '-' indicates not obtained. */
  auth_id_t auth;		/*  Name of authenticated user. */
  Puint32 date;		/*  Timestamp of request. */
  http_request_t request;		/*  Request. */
  Puint16 response;		/*  3-digit response code */
  contentOpt_t contentLength;		/*  Number of bytes in request response. */
};

Perror_t http_clf_t_init (P_t *pads,http_clf_t *rep);

Perror_t http_clf_t_pd_init (P_t *pads,http_clf_t_pd *pd);

Perror_t http_clf_t_cleanup (P_t *pads,http_clf_t *rep);

Perror_t http_clf_t_pd_cleanup (P_t *pads,http_clf_t_pd *pd);

Perror_t http_clf_t_copy (P_t *pads,http_clf_t *rep_dst,http_clf_t *rep_src);

Perror_t http_clf_t_pd_copy (P_t *pads,http_clf_t_pd *pd_dst,http_clf_t_pd *pd_src);

void http_clf_t_m_init (P_t *pads,http_clf_t_m *mask,Pbase_m baseMask);

Perror_t http_clf_t_read (P_t *pads,http_clf_t_m *m,http_clf_t_pd *pd,http_clf_t *rep);

int http_clf_t_verify (http_clf_t *rep);
typedef struct http_clf_t_acc_s http_clf_t_acc;
struct http_clf_t_acc_s {
  Puint32_acc nerr;
  host_t_acc host;		/*  IP address of client requesting service */
  auth_id_t_acc remoteID;		/*  Remote identity; '-' indicates not obtained. */
  auth_id_t_acc auth;		/*  Name of authenticated user. */
  http_request_t_acc request;		/*  Request. */
  Puint16_acc response;		/*  3-digit response code */
  contentOpt_t_acc contentLength;		/*  Number of bytes in request response. */
};

Perror_t http_clf_t_acc_init (P_t *pads,http_clf_t_acc *acc);

Perror_t http_clf_t_acc_reset (P_t *pads,http_clf_t_acc *acc);

Perror_t http_clf_t_acc_cleanup (P_t *pads,http_clf_t_acc *acc);

Perror_t http_clf_t_acc_add (P_t *pads,http_clf_t_acc *acc,http_clf_t_pd *pd,http_clf_t *rep);

Perror_t http_clf_t_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,http_clf_t_acc *acc);

Perror_t http_clf_t_acc_report (P_t *pads,char const *prefix,char const *what,int nst,http_clf_t_acc *acc);

ssize_t http_clf_t_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,http_clf_t_pd *pd,http_clf_t *rep);

ssize_t http_clf_t_write2io (P_t *pads,Sfio_t *io,http_clf_t_pd *pd,http_clf_t *rep);

ssize_t http_clf_t_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,http_clf_t_pd *pd,http_clf_t *rep,char const *tag,int indent);

ssize_t http_clf_t_write_xml_2io (P_t *pads,Sfio_t *io,http_clf_t_pd *pd,http_clf_t *rep,char const *tag,int indent);
typedef struct log_t_s log_t;
typedef struct log_t_m_s log_t_m;
typedef struct log_t_pd_s log_t_pd;
struct log_t_m_s {
  http_clf_t_m element;		/* per-element */
  Pbase_m arrayLevel;		/* entire array */
};
struct log_t_pd_s {
  Pflags_t pstate;
  Puint32 nerr;		/* Number of array errors */
  PerrCode_t errCode;
  Ploc_t loc;
  Puint32 neerr;		/* Number of element errors */
  Puint32 firstError;		/* if errCode == ARRAY_ELEM_ERR, index of first error */
  Puint32 numRead;		/* Number of elements read */
  Puint32 length;		/* Number of elements in memory */
  http_clf_t_pd *elts;
  RBuf_t *_internal;
};
struct log_t_s {
  Puint32 length;
  http_clf_t *elts;
  RBuf_t *_internal;
};

Perror_t log_t_init (P_t *pads,log_t *rep);

Perror_t log_t_pd_init (P_t *pads,log_t_pd *pd);

Perror_t log_t_cleanup (P_t *pads,log_t *rep);

Perror_t log_t_pd_cleanup (P_t *pads,log_t_pd *pd);

Perror_t log_t_copy (P_t *pads,log_t *rep_dst,log_t *rep_src);

Perror_t log_t_pd_copy (P_t *pads,log_t_pd *pd_dst,log_t_pd *pd_src);

void log_t_m_init (P_t *pads,log_t_m *mask,Pbase_m baseMask);

Perror_t log_t_read (P_t *pads,log_t_m *m,log_t_pd *pd,log_t *rep);

int log_t_verify (log_t *rep);
typedef struct log_t_acc_s log_t_acc;
struct log_t_acc_s {
  Puint32_acc length;		/* Accumulator for array length */
  http_clf_t_acc arrayLevel;		/* Accumulator for all array elements */
  http_clf_t_acc arrayDetail[10];		/* Accumulator for first 10 array elements */
};

Perror_t log_t_acc_init (P_t *pads,log_t_acc *acc);

Perror_t log_t_acc_reset (P_t *pads,log_t_acc *acc);

Perror_t log_t_acc_cleanup (P_t *pads,log_t_acc *acc);

Perror_t log_t_acc_add (P_t *pads,log_t_acc *acc,log_t_pd *pd,log_t *rep);

Perror_t log_t_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,log_t_acc *acc);

Perror_t log_t_acc_report (P_t *pads,char const *prefix,char const *what,int nst,log_t_acc *acc);

ssize_t log_t_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,log_t_pd *pd,log_t *rep);

ssize_t log_t_write2io (P_t *pads,Sfio_t *io,log_t_pd *pd,log_t *rep);

ssize_t log_t_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,log_t_pd *pd,log_t *rep,char const *tag,int indent);

ssize_t log_t_write_xml_2io (P_t *pads,Sfio_t *io,log_t_pd *pd,log_t *rep,char const *tag,int indent);

void P_lib_init ();

#endif /*  __AI__H__  */
