/*@FILE @LEFT ai.httpRequestRep.tex  ai.httpRequestCSM.tex ai.httpRequestED.tex ai.httpRequestPD.tex ai.httpRequestOps.tex */
#ifndef __AI__H__
#define __AI__H__
#include "padsc.h"
typedef struct nIP_s nIP;
typedef struct nIP_m_s nIP_m;
typedef struct nIP_pd_s nIP_pd;
typedef struct nIP_acc_s nIP_acc;
struct nIP_m_s {
  PDC_base_m element;		/* per-element */
  PDC_base_m array;		/* entire array */
};
struct nIP_pd_s {
  PDC_uint32 nerr;		/* Number of array errors */
  PDC_errCode_t errCode;
  PDC_uint32 neerr;		/* Number of element errors */
  PDC_loc_t loc;
  PDC_uint32 panic;
  PDC_uint32 firstError;		/* if errCode == ARRAY_ELEM_ERR, index of first error */
  PDC_uint32 length;
  PDC_base_pd *elts;
  RBuf_t *_internal;
};
struct nIP_acc_s {
  PDC_int32_acc length;		/* Accumulator for array length */
  PDC_uint8_acc array;		/* Accumulator for all array elements */
  PDC_uint8_acc arrayDetail[4];		/* Accumulator for first 4 array elements */
};
struct nIP_s {
  PDC_int32 length;
  PDC_uint8 *elts;
  RBuf_t *_internal;
};

PDC_error_t nIP_init (PDC_t *pdc,nIP *rep);

PDC_error_t nIP_pd_init (PDC_t *pdc,nIP_pd *pd);

PDC_error_t nIP_cleanup (PDC_t *pdc,nIP *rep);

PDC_error_t nIP_pd_cleanup (PDC_t *pdc,nIP_pd *pd);

PDC_error_t nIP_copy (PDC_t *pdc,nIP *rep_dst,nIP *rep_src);

PDC_error_t nIP_pd_copy (PDC_t *pdc,nIP_pd *pd_dst,nIP_pd *pd_src);

PDC_error_t nIP_read (PDC_t *pdc,nIP_m *m,nIP_pd *pd,nIP *rep);

void nIP_maskFill (PDC_t *pdc,nIP_m *mask,PDC_base_m baseMask);

ssize_t nIP_write2buf (PDC_t *pdc,PDC_byte *buf,size_t buf_len,int *buf_full,nIP_pd *pd,nIP *rep);

ssize_t nIP_write2io (PDC_t *pdc,Sfio_t *io,nIP_pd *pd,nIP *rep);

PDC_error_t nIP_acc_init (PDC_t *pdc,nIP_acc *acc);

PDC_error_t nIP_acc_reset (PDC_t *pdc,nIP_acc *acc);

PDC_error_t nIP_acc_cleanup (PDC_t *pdc,nIP_acc *acc);

PDC_error_t nIP_acc_add (PDC_t *pdc,nIP_acc *acc,nIP_pd *pd,nIP *rep);

PDC_error_t nIP_acc_report2io (PDC_t *pdc,Sfio_t *outstr,char const *prefix,char const *what,int nst,nIP_acc *acc);

PDC_error_t nIP_acc_report (PDC_t *pdc,char const *prefix,char const *what,int nst,nIP_acc *acc);
typedef struct sIP_s sIP;
typedef struct sIP_m_s sIP_m;
typedef struct sIP_pd_s sIP_pd;
typedef struct sIP_acc_s sIP_acc;
struct sIP_m_s {
  PDC_base_m element;		/* per-element */
  PDC_base_m array;		/* entire array */
};
struct sIP_pd_s {
  PDC_uint32 nerr;		/* Number of array errors */
  PDC_errCode_t errCode;
  PDC_uint32 neerr;		/* Number of element errors */
  PDC_loc_t loc;
  PDC_uint32 panic;
  PDC_uint32 firstError;		/* if errCode == ARRAY_ELEM_ERR, index of first error */
  PDC_uint32 length;
  PDC_base_pd *elts;
  RBuf_t *_internal;
};
struct sIP_acc_s {
  PDC_int32_acc length;		/* Accumulator for array length */
  PDC_string_acc array;		/* Accumulator for all array elements */
  PDC_string_acc arrayDetail[10];		/* Accumulator for first 10 array elements */
};
struct sIP_s {
  PDC_int32 length;
  PDC_string *elts;
  RBuf_t *_internal;
};

PDC_error_t sIP_init (PDC_t *pdc,sIP *rep);

PDC_error_t sIP_pd_init (PDC_t *pdc,sIP_pd *pd);

PDC_error_t sIP_cleanup (PDC_t *pdc,sIP *rep);

PDC_error_t sIP_pd_cleanup (PDC_t *pdc,sIP_pd *pd);

PDC_error_t sIP_copy (PDC_t *pdc,sIP *rep_dst,sIP *rep_src);

PDC_error_t sIP_pd_copy (PDC_t *pdc,sIP_pd *pd_dst,sIP_pd *pd_src);

PDC_error_t sIP_read (PDC_t *pdc,sIP_m *m,sIP_pd *pd,sIP *rep);

void sIP_maskFill (PDC_t *pdc,sIP_m *mask,PDC_base_m baseMask);

ssize_t sIP_write2buf (PDC_t *pdc,PDC_byte *buf,size_t buf_len,int *buf_full,sIP_pd *pd,sIP *rep);

ssize_t sIP_write2io (PDC_t *pdc,Sfio_t *io,sIP_pd *pd,sIP *rep);

PDC_error_t sIP_acc_init (PDC_t *pdc,sIP_acc *acc);

PDC_error_t sIP_acc_reset (PDC_t *pdc,sIP_acc *acc);

PDC_error_t sIP_acc_cleanup (PDC_t *pdc,sIP_acc *acc);

PDC_error_t sIP_acc_add (PDC_t *pdc,sIP_acc *acc,sIP_pd *pd,sIP *rep);

PDC_error_t sIP_acc_report2io (PDC_t *pdc,Sfio_t *outstr,char const *prefix,char const *what,int nst,sIP_acc *acc);

PDC_error_t sIP_acc_report (PDC_t *pdc,char const *prefix,char const *what,int nst,sIP_acc *acc);
typedef enum host_t_tag_e host_t_tag;
typedef union host_t_u_u host_t_u;
typedef struct host_t_s host_t;
typedef struct host_t_m_s host_t_m;
typedef union host_t_pd_u_u host_t_pd_u;
typedef struct host_t_pd_s host_t_pd;
typedef struct host_t_acc_s host_t_acc;
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
  PDC_uint32 nerr;
  PDC_errCode_t errCode;
  PDC_loc_t loc;
  PDC_uint32 panic;
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
  nIP_m resolved;
  sIP_m symbolic;
};
struct host_t_acc_s {
  PDC_int32_acc tag;
  nIP_acc resolved;
  sIP_acc symbolic;
};

char const *host_t_tag2str (host_t_tag which);

PDC_error_t host_t_init (PDC_t *pdc,host_t *rep);

PDC_error_t host_t_pd_init (PDC_t *pdc,host_t_pd *pd);

PDC_error_t host_t_cleanup (PDC_t *pdc,host_t *rep);

PDC_error_t host_t_pd_cleanup (PDC_t *pdc,host_t_pd *pd);

PDC_error_t host_t_copy (PDC_t *pdc,host_t *rep_dst,host_t *rep_src);

PDC_error_t host_t_pd_copy (PDC_t *pdc,host_t_pd *pd_dst,host_t_pd *pd_src);

PDC_error_t host_t_read (PDC_t *pdc,host_t_m *m,host_t_pd *pd,host_t *rep);

void host_t_maskFill (PDC_t *pdc,host_t_m *mask,PDC_base_m baseMask);

ssize_t host_t_write2buf (PDC_t *pdc,PDC_byte *buf,size_t buf_len,int *buf_full,host_t_pd *pd,host_t *rep);

ssize_t host_t_write2io (PDC_t *pdc,Sfio_t *io,host_t_pd *pd,host_t *rep);

PDC_error_t host_t_acc_init (PDC_t *pdc,host_t_acc *acc);

PDC_error_t host_t_acc_reset (PDC_t *pdc,host_t_acc *acc);

PDC_error_t host_t_acc_cleanup (PDC_t *pdc,host_t_acc *acc);

PDC_error_t host_t_acc_add (PDC_t *pdc,host_t_acc *acc,host_t_pd *pd,host_t *rep);

PDC_error_t host_t_acc_report2io (PDC_t *pdc,Sfio_t *outstr,char const *prefix,char const *what,int nst,host_t_acc *acc);

PDC_error_t host_t_acc_report (PDC_t *pdc,char const *prefix,char const *what,int nst,host_t_acc *acc);
typedef PDC_char unknown_t;
typedef struct unknown_t_m_s unknown_t_m;
typedef struct unknown_t_pd_s unknown_t_pd;
typedef PDC_char_acc unknown_t_acc;
struct unknown_t_m_s {
  PDC_base_m base;		/* Base mask */
  PDC_base_m user;		/* Typedef mask */
};
struct unknown_t_pd_s {
  PDC_uint32 nerr;
  PDC_errCode_t errCode;
  PDC_loc_t loc;
  PDC_uint32 panic;
  PDC_base_pd base;		/* Base parse description */
};

PDC_error_t unknown_t_init (PDC_t *pdc,unknown_t *rep);

PDC_error_t unknown_t_pd_init (PDC_t *pdc,unknown_t_pd *pd);

PDC_error_t unknown_t_cleanup (PDC_t *pdc,unknown_t *rep);

PDC_error_t unknown_t_pd_cleanup (PDC_t *pdc,unknown_t_pd *pd);

PDC_error_t unknown_t_copy (PDC_t *pdc,unknown_t *rep_dst,unknown_t *rep_src);

PDC_error_t unknown_t_pd_copy (PDC_t *pdc,unknown_t_pd *pd_dst,unknown_t_pd *pd_src);

PDC_error_t unknown_t_read (PDC_t *pdc,unknown_t_m *m,unknown_t_pd *pd,unknown_t *rep);

void unknown_t_maskFill (PDC_t *pdc,unknown_t_m *mask,PDC_base_m baseMask);

ssize_t unknown_t_write2buf (PDC_t *pdc,PDC_byte *buf,size_t buf_len,int *buf_full,unknown_t_pd *pd,unknown_t *rep);

ssize_t unknown_t_write2io (PDC_t *pdc,Sfio_t *io,unknown_t_pd *pd,unknown_t *rep);

PDC_error_t unknown_t_acc_init (PDC_t *pdc,unknown_t_acc *acc);

PDC_error_t unknown_t_acc_reset (PDC_t *pdc,unknown_t_acc *acc);

PDC_error_t unknown_t_acc_cleanup (PDC_t *pdc,unknown_t_acc *acc);

PDC_error_t unknown_t_acc_add (PDC_t *pdc,unknown_t_acc *acc,unknown_t_pd *pd,unknown_t *rep);

PDC_error_t unknown_t_acc_report2io (PDC_t *pdc,Sfio_t *outstr,char const *prefix,char const *what,int nst,unknown_t_acc *acc);

PDC_error_t unknown_t_acc_report (PDC_t *pdc,char const *prefix,char const *what,int nst,unknown_t_acc *acc);
typedef enum auth_id_t_tag_e auth_id_t_tag;
typedef union auth_id_t_u_u auth_id_t_u;
typedef struct auth_id_t_s auth_id_t;
typedef struct auth_id_t_m_s auth_id_t_m;
typedef union auth_id_t_pd_u_u auth_id_t_pd_u;
typedef struct auth_id_t_pd_s auth_id_t_pd;
typedef struct auth_id_t_acc_s auth_id_t_acc;
enum auth_id_t_tag_e {
  auth_id_t_err=0,
  unauthorized=1,
  id=2
  };
union auth_id_t_pd_u_u {
  unknown_t_pd unauthorized;
  PDC_base_pd id;
};
struct auth_id_t_pd_s {
  PDC_uint32 nerr;
  PDC_errCode_t errCode;
  PDC_loc_t loc;
  PDC_uint32 panic;
  auth_id_t_tag tag;
  auth_id_t_pd_u val;
};
union auth_id_t_u_u {
  unknown_t unauthorized;		/*  non-authenticated http session */
  PDC_string id;		/*  login supplied during authentication */
};
struct auth_id_t_s {
  auth_id_t_tag tag;
  auth_id_t_u val;
};
struct auth_id_t_m_s {
  unknown_t_m unauthorized;
  PDC_base_m id;
};
struct auth_id_t_acc_s {
  PDC_int32_acc tag;
  unknown_t_acc unauthorized;
  PDC_string_acc id;
};

char const *auth_id_t_tag2str (auth_id_t_tag which);

PDC_error_t auth_id_t_init (PDC_t *pdc,auth_id_t *rep);

PDC_error_t auth_id_t_pd_init (PDC_t *pdc,auth_id_t_pd *pd);

PDC_error_t auth_id_t_cleanup (PDC_t *pdc,auth_id_t *rep);

PDC_error_t auth_id_t_pd_cleanup (PDC_t *pdc,auth_id_t_pd *pd);

PDC_error_t auth_id_t_copy (PDC_t *pdc,auth_id_t *rep_dst,auth_id_t *rep_src);

PDC_error_t auth_id_t_pd_copy (PDC_t *pdc,auth_id_t_pd *pd_dst,auth_id_t_pd *pd_src);

PDC_error_t auth_id_t_read (PDC_t *pdc,auth_id_t_m *m,auth_id_t_pd *pd,auth_id_t *rep);

void auth_id_t_maskFill (PDC_t *pdc,auth_id_t_m *mask,PDC_base_m baseMask);

ssize_t auth_id_t_write2buf (PDC_t *pdc,PDC_byte *buf,size_t buf_len,int *buf_full,auth_id_t_pd *pd,auth_id_t *rep);

ssize_t auth_id_t_write2io (PDC_t *pdc,Sfio_t *io,auth_id_t_pd *pd,auth_id_t *rep);

PDC_error_t auth_id_t_acc_init (PDC_t *pdc,auth_id_t_acc *acc);

PDC_error_t auth_id_t_acc_reset (PDC_t *pdc,auth_id_t_acc *acc);

PDC_error_t auth_id_t_acc_cleanup (PDC_t *pdc,auth_id_t_acc *acc);

PDC_error_t auth_id_t_acc_add (PDC_t *pdc,auth_id_t_acc *acc,auth_id_t_pd *pd,auth_id_t *rep);

PDC_error_t auth_id_t_acc_report2io (PDC_t *pdc,Sfio_t *outstr,char const *prefix,char const *what,int nst,auth_id_t_acc *acc);

PDC_error_t auth_id_t_acc_report (PDC_t *pdc,char const *prefix,char const *what,int nst,auth_id_t_acc *acc);
typedef enum contentOpt_t_tag_e contentOpt_t_tag;
typedef union contentOpt_t_u_u contentOpt_t_u;
typedef struct contentOpt_t_s contentOpt_t;
typedef struct contentOpt_t_m_s contentOpt_t_m;
typedef union contentOpt_t_pd_u_u contentOpt_t_pd_u;
typedef struct contentOpt_t_pd_s contentOpt_t_pd;
typedef struct contentOpt_t_acc_s contentOpt_t_acc;
enum contentOpt_t_tag_e {
  contentOpt_t_err=0,
  len=1,
  unavailable=2
  };
union contentOpt_t_pd_u_u {
  PDC_base_pd len;
  unknown_t_pd unavailable;
};
struct contentOpt_t_pd_s {
  PDC_uint32 nerr;
  PDC_errCode_t errCode;
  PDC_loc_t loc;
  PDC_uint32 panic;
  contentOpt_t_tag tag;
  contentOpt_t_pd_u val;
};
union contentOpt_t_u_u {
  PDC_uint32 len;		/*  length available */
  unknown_t unavailable;
};
struct contentOpt_t_s {
  contentOpt_t_tag tag;
  contentOpt_t_u val;
};
struct contentOpt_t_m_s {
  PDC_base_m len;
  unknown_t_m unavailable;
};
struct contentOpt_t_acc_s {
  PDC_int32_acc tag;
  PDC_uint32_acc len;
  unknown_t_acc unavailable;
};

char const *contentOpt_t_tag2str (contentOpt_t_tag which);

PDC_error_t contentOpt_t_init (PDC_t *pdc,contentOpt_t *rep);

PDC_error_t contentOpt_t_pd_init (PDC_t *pdc,contentOpt_t_pd *pd);

PDC_error_t contentOpt_t_cleanup (PDC_t *pdc,contentOpt_t *rep);

PDC_error_t contentOpt_t_pd_cleanup (PDC_t *pdc,contentOpt_t_pd *pd);

PDC_error_t contentOpt_t_copy (PDC_t *pdc,contentOpt_t *rep_dst,contentOpt_t *rep_src);

PDC_error_t contentOpt_t_pd_copy (PDC_t *pdc,contentOpt_t_pd *pd_dst,contentOpt_t_pd *pd_src);

PDC_error_t contentOpt_t_read (PDC_t *pdc,contentOpt_t_m *m,contentOpt_t_pd *pd,contentOpt_t *rep);

void contentOpt_t_maskFill (PDC_t *pdc,contentOpt_t_m *mask,PDC_base_m baseMask);

ssize_t contentOpt_t_write2buf (PDC_t *pdc,PDC_byte *buf,size_t buf_len,int *buf_full,contentOpt_t_pd *pd,contentOpt_t *rep);

ssize_t contentOpt_t_write2io (PDC_t *pdc,Sfio_t *io,contentOpt_t_pd *pd,contentOpt_t *rep);

PDC_error_t contentOpt_t_acc_init (PDC_t *pdc,contentOpt_t_acc *acc);

PDC_error_t contentOpt_t_acc_reset (PDC_t *pdc,contentOpt_t_acc *acc);

PDC_error_t contentOpt_t_acc_cleanup (PDC_t *pdc,contentOpt_t_acc *acc);

PDC_error_t contentOpt_t_acc_add (PDC_t *pdc,contentOpt_t_acc *acc,contentOpt_t_pd *pd,contentOpt_t *rep);

PDC_error_t contentOpt_t_acc_report2io (PDC_t *pdc,Sfio_t *outstr,char const *prefix,char const *what,int nst,contentOpt_t_acc *acc);

PDC_error_t contentOpt_t_acc_report (PDC_t *pdc,char const *prefix,char const *what,int nst,contentOpt_t_acc *acc);
typedef struct http_v_t_s http_v_t;
typedef struct http_v_t_m_s http_v_t_m;
typedef struct http_v_t_pd_s http_v_t_pd;
typedef struct http_v_t_acc_s http_v_t_acc;
struct http_v_t_m_s {
  PDC_base_m structLevel;
  PDC_base_m major;
  PDC_base_m minor;
};
struct http_v_t_pd_s {
  PDC_uint32 nerr;
  PDC_errCode_t errCode;
  PDC_loc_t loc;
  PDC_uint32 panic;
  PDC_base_pd major;
  PDC_base_pd minor;
};
struct http_v_t_acc_s {
  PDC_int32_acc nerr;
  PDC_uint8_acc major;
  PDC_uint8_acc minor;		/*  http minor mode */
};
struct http_v_t_s {
  PDC_uint8 major;
  PDC_uint8 minor;		/*  http minor mode */
};

PDC_error_t http_v_t_init (PDC_t *pdc,http_v_t *rep);

PDC_error_t http_v_t_pd_init (PDC_t *pdc,http_v_t_pd *pd);

PDC_error_t http_v_t_cleanup (PDC_t *pdc,http_v_t *rep);

PDC_error_t http_v_t_pd_cleanup (PDC_t *pdc,http_v_t_pd *pd);

PDC_error_t http_v_t_copy (PDC_t *pdc,http_v_t *rep_dst,http_v_t *rep_src);

PDC_error_t http_v_t_pd_copy (PDC_t *pdc,http_v_t_pd *pd_dst,http_v_t_pd *pd_src);

PDC_error_t http_v_t_read (PDC_t *pdc,http_v_t_m *m,http_v_t_pd *pd,http_v_t *rep);

void http_v_t_maskFill (PDC_t *pdc,http_v_t_m *mask,PDC_base_m baseMask);

ssize_t http_v_t_write2buf (PDC_t *pdc,PDC_byte *buf,size_t buf_len,int *buf_full,http_v_t_pd *pd,http_v_t *rep);

ssize_t http_v_t_write2io (PDC_t *pdc,Sfio_t *io,http_v_t_pd *pd,http_v_t *rep);

PDC_error_t http_v_t_acc_init (PDC_t *pdc,http_v_t_acc *acc);

PDC_error_t http_v_t_acc_reset (PDC_t *pdc,http_v_t_acc *acc);

PDC_error_t http_v_t_acc_cleanup (PDC_t *pdc,http_v_t_acc *acc);

PDC_error_t http_v_t_acc_add (PDC_t *pdc,http_v_t_acc *acc,http_v_t_pd *pd,http_v_t *rep);

PDC_error_t http_v_t_acc_report2io (PDC_t *pdc,Sfio_t *outstr,char const *prefix,char const *what,int nst,http_v_t_acc *acc);

PDC_error_t http_v_t_acc_report (PDC_t *pdc,char const *prefix,char const *what,int nst,http_v_t_acc *acc);
typedef enum http_method_t_e http_method_t;
typedef PDC_base_m http_method_t_m;
typedef PDC_base_pd http_method_t_pd;
typedef PDC_int32_acc http_method_t_acc;
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

PDC_error_t http_method_t_init (PDC_t *pdc,http_method_t *rep);

PDC_error_t http_method_t_pd_init (PDC_t *pdc,http_method_t_pd *pd);

PDC_error_t http_method_t_cleanup (PDC_t *pdc,http_method_t *rep);

PDC_error_t http_method_t_pd_cleanup (PDC_t *pdc,http_method_t_pd *pd);

PDC_error_t http_method_t_copy (PDC_t *pdc,http_method_t *rep_dst,http_method_t *rep_src);

PDC_error_t http_method_t_pd_copy (PDC_t *pdc,http_method_t_pd *pd_dst,http_method_t_pd *pd_src);

PDC_error_t http_method_t_read (PDC_t *pdc,http_method_t_m *m,http_method_t_pd *pd,http_method_t *rep);

void http_method_t_maskFill (PDC_t *pdc,http_method_t_m *mask,PDC_base_m baseMask);

ssize_t http_method_t_write2buf (PDC_t *pdc,PDC_byte *buf,size_t buf_len,int *buf_full,http_method_t_pd *pd,http_method_t *rep);

ssize_t http_method_t_write2io (PDC_t *pdc,Sfio_t *io,http_method_t_pd *pd,http_method_t *rep);

PDC_error_t http_method_t_acc_init (PDC_t *pdc,http_method_t_acc *acc);

PDC_error_t http_method_t_acc_reset (PDC_t *pdc,http_method_t_acc *acc);

PDC_error_t http_method_t_acc_cleanup (PDC_t *pdc,http_method_t_acc *acc);

PDC_error_t http_method_t_acc_add (PDC_t *pdc,http_method_t_acc *acc,http_method_t_pd *pd,http_method_t *rep);

PDC_error_t http_method_t_acc_report2io (PDC_t *pdc,Sfio_t *outstr,char const *prefix,char const *what,int nst,http_method_t_acc *acc);

PDC_error_t http_method_t_acc_report (PDC_t *pdc,char const *prefix,char const *what,int nst,http_method_t_acc *acc);

int checkVersion (http_v_t version,http_method_t meth);
typedef struct http_request_t_acc_s http_request_t_acc;

/*@BEGIN ai.httpRequestCSM.tex*/
typedef struct http_request_t_m_s http_request_t_m;

struct http_request_t_m_s {
  PDC_base_m structLevel;
  http_method_t_m meth;
  PDC_base_m req_uri;
  http_v_t_m version;
};
/*@END ai.httpRequestCSM.tex*/

/*@BEGIN ai.httpRequestPD.tex*/
typedef struct http_request_t_pd_s http_request_t_pd;

struct http_request_t_pd_s {
  PDC_uint32 pstate;
  PDC_errCode_t errCode;
  PDC_loc_t loc;
  PDC_uint32 nerr;
  http_method_t_pd meth;
  PDC_base_pd req_uri;
  http_v_t_pd version;
};
/*@END ai.httpRequestPD.tex*/

struct http_request_t_acc_s {
  PDC_int32_acc nerr;
  http_method_t_acc meth;		/*  Method used during request */
  PDC_string_acc req_uri;		/*  Requested uri. */
  http_v_t_acc version;		/*  HTTP version. checkVersion(version, meth) */
};

/*@BEGIN ai.httpRequestRep.tex*/
typedef struct http_request_t_s http_request_t;

struct http_request_t_s {
  http_method_t meth;	  /*  Method used during request */
  PDC_string req_uri;	  /*  Requested uri. */
  http_v_t version;	  /*  HTTP version. 
                              checkVersion(version, meth) */
};
/*@END ai.httpRequestRep.tex*/

/*@BEGIN ai.httpRequestOps.tex */
PDC_error_t http_request_t_init (PDC_t *pdc,http_request_t *rep);

PDC_error_t http_request_t_pd_init (PDC_t *pdc,http_request_t_pd *pd);

PDC_error_t http_request_t_cleanup (PDC_t *pdc,http_request_t *rep);

PDC_error_t http_request_t_pd_cleanup (PDC_t *pdc,http_request_t_pd *pd);

void http_request_t_maskFill (PDC_t *pdc,http_request_t_m *mask,PDC_base_m baseMask);

PDC_error_t http_request_t_copy (PDC_t *pdc,http_request_t *rep_dst,
                                            http_request_t *rep_src);

PDC_error_t http_request_t_pd_copy (PDC_t *pdc,http_request_t_pd *pd_dst,
                                              http_request_t_pd *pd_src);

PDC_error_t http_request_t_read (PDC_t *pdc,http_request_t_m *m,
                                 http_request_t_pd *pd,http_request_t *rep);


ssize_t http_request_t_write2buf (PDC_t *pdc,PDC_byte *buf,size_t buf_len,
                                  int *buf_full,http_request_t_pd *pd,
                                  http_request_t *rep);

ssize_t http_request_t_write2io (PDC_t *pdc,Sfio_t *io,http_request_t_pd *pd,
                                 http_request_t *rep);
/*@END ai.httpRequestOps.tex*/

PDC_error_t http_request_t_acc_init (PDC_t *pdc,http_request_t_acc *acc);

PDC_error_t http_request_t_acc_reset (PDC_t *pdc,http_request_t_acc *acc);

PDC_error_t http_request_t_acc_cleanup (PDC_t *pdc,http_request_t_acc *acc);

PDC_error_t http_request_t_acc_add (PDC_t *pdc,http_request_t_acc *acc,http_request_t_pd *pd,http_request_t *rep);

PDC_error_t http_request_t_acc_report2io (PDC_t *pdc,Sfio_t *outstr,char const *prefix,char const *what,int nst,http_request_t_acc *acc);

PDC_error_t http_request_t_acc_report (PDC_t *pdc,char const *prefix,char const *what,int nst,http_request_t_acc *acc);
typedef struct http_clf_t_s http_clf_t;
typedef struct http_clf_t_m_s http_clf_t_m;
typedef struct http_clf_t_pd_s http_clf_t_pd;
typedef struct http_clf_t_acc_s http_clf_t_acc;
struct http_clf_t_m_s {
  PDC_base_m structLevel;
  host_t_m host;
  auth_id_t_m remoteID;
  auth_id_t_m auth;
  PDC_base_m date;
  http_request_t_m request;
  PDC_base_m response;
  contentOpt_t_m contentLength;
};
struct http_clf_t_pd_s {
  PDC_uint32 nerr;
  PDC_errCode_t errCode;
  PDC_loc_t loc;
  PDC_uint32 panic;
  host_t_pd host;
  auth_id_t_pd remoteID;
  auth_id_t_pd auth;
  PDC_base_pd date;
  http_request_t_pd request;
  PDC_base_pd response;
  contentOpt_t_pd contentLength;
};
struct http_clf_t_acc_s {
  PDC_int32_acc nerr;
  host_t_acc host;		/*  IP address of client requesting service */
  auth_id_t_acc remoteID;		/*  Remote identity; '-' indicates not obtained. */
  auth_id_t_acc auth;		/*  Name of authenticated user. */
  http_request_t_acc request;		/*  Request. */
  PDC_uint16_acc response;		/*  3-digit response code */
  contentOpt_t_acc contentLength;		/*  Number of bytes in request response. */
};
struct http_clf_t_s {
  host_t host;		/*  IP address of client requesting service */
  auth_id_t remoteID;		/*  Remote identity; '-' indicates not obtained. */
  auth_id_t auth;		/*  Name of authenticated user. */
  PDC_uint32 date;		/*  Timestamp of request. */
  http_request_t request;		/*  Request. */
  PDC_uint16 response;		/*  3-digit response code */
  contentOpt_t contentLength;		/*  Number of bytes in request response. */
};

PDC_error_t http_clf_t_init (PDC_t *pdc,http_clf_t *rep);

PDC_error_t http_clf_t_pd_init (PDC_t *pdc,http_clf_t_pd *pd);

PDC_error_t http_clf_t_cleanup (PDC_t *pdc,http_clf_t *rep);

PDC_error_t http_clf_t_pd_cleanup (PDC_t *pdc,http_clf_t_pd *pd);

PDC_error_t http_clf_t_copy (PDC_t *pdc,http_clf_t *rep_dst,http_clf_t *rep_src);

PDC_error_t http_clf_t_pd_copy (PDC_t *pdc,http_clf_t_pd *pd_dst,http_clf_t_pd *pd_src);

PDC_error_t http_clf_t_read (PDC_t *pdc,http_clf_t_m *m,http_clf_t_pd *pd,http_clf_t *rep);

void http_clf_t_maskFill (PDC_t *pdc,http_clf_t_m *mask,PDC_base_m baseMask);

ssize_t http_clf_t_write2buf (PDC_t *pdc,PDC_byte *buf,size_t buf_len,int *buf_full,http_clf_t_pd *pd,http_clf_t *rep);

ssize_t http_clf_t_write2io (PDC_t *pdc,Sfio_t *io,http_clf_t_pd *pd,http_clf_t *rep);

PDC_error_t http_clf_t_acc_init (PDC_t *pdc,http_clf_t_acc *acc);

PDC_error_t http_clf_t_acc_reset (PDC_t *pdc,http_clf_t_acc *acc);

PDC_error_t http_clf_t_acc_cleanup (PDC_t *pdc,http_clf_t_acc *acc);

PDC_error_t http_clf_t_acc_add (PDC_t *pdc,http_clf_t_acc *acc,http_clf_t_pd *pd,http_clf_t *rep);

PDC_error_t http_clf_t_acc_report2io (PDC_t *pdc,Sfio_t *outstr,char const *prefix,char const *what,int nst,http_clf_t_acc *acc);

PDC_error_t http_clf_t_acc_report (PDC_t *pdc,char const *prefix,char const *what,int nst,http_clf_t_acc *acc);
typedef struct log_t_s log_t;
typedef struct log_t_m_s log_t_m;
typedef struct log_t_pd_s log_t_pd;
typedef struct log_t_acc_s log_t_acc;
struct log_t_m_s {
  http_clf_t_m element;		/* per-element */
  PDC_base_m array;		/* entire array */
};
struct log_t_pd_s {
  PDC_uint32 nerr;		/* Number of array errors */
  PDC_errCode_t errCode;
  PDC_uint32 neerr;		/* Number of element errors */
  PDC_loc_t loc;
  PDC_uint32 panic;
  PDC_uint32 firstError;		/* if errCode == ARRAY_ELEM_ERR, index of first error */
  PDC_uint32 length;
  http_clf_t_pd *elts;
  RBuf_t *_internal;
};
struct log_t_acc_s {
  PDC_int32_acc length;		/* Accumulator for array length */
  http_clf_t_acc array;		/* Accumulator for all array elements */
  http_clf_t_acc arrayDetail[10];		/* Accumulator for first 10 array elements */
};
struct log_t_s {
  PDC_int32 length;
  http_clf_t *elts;
  RBuf_t *_internal;
};

PDC_error_t log_t_init (PDC_t *pdc,log_t *rep);

PDC_error_t log_t_pd_init (PDC_t *pdc,log_t_pd *pd);

PDC_error_t log_t_cleanup (PDC_t *pdc,log_t *rep);

PDC_error_t log_t_pd_cleanup (PDC_t *pdc,log_t_pd *pd);

PDC_error_t log_t_copy (PDC_t *pdc,log_t *rep_dst,log_t *rep_src);

PDC_error_t log_t_pd_copy (PDC_t *pdc,log_t_pd *pd_dst,log_t_pd *pd_src);

PDC_error_t log_t_read (PDC_t *pdc,log_t_m *m,log_t_pd *pd,log_t *rep);

void log_t_maskFill (PDC_t *pdc,log_t_m *mask,PDC_base_m baseMask);

ssize_t log_t_write2buf (PDC_t *pdc,PDC_byte *buf,size_t buf_len,int *buf_full,log_t_pd *pd,log_t *rep);

ssize_t log_t_write2io (PDC_t *pdc,Sfio_t *io,log_t_pd *pd,log_t *rep);

PDC_error_t log_t_acc_init (PDC_t *pdc,log_t_acc *acc);

PDC_error_t log_t_acc_reset (PDC_t *pdc,log_t_acc *acc);

PDC_error_t log_t_acc_cleanup (PDC_t *pdc,log_t_acc *acc);

PDC_error_t log_t_acc_add (PDC_t *pdc,log_t_acc *acc,log_t_pd *pd,log_t *rep);

PDC_error_t log_t_acc_report2io (PDC_t *pdc,Sfio_t *outstr,char const *prefix,char const *what,int nst,log_t_acc *acc);

PDC_error_t log_t_acc_report (PDC_t *pdc,char const *prefix,char const *what,int nst,log_t_acc *acc);

#endif /*  __AI__H__  */
