/*@FILE @LEFT ai.httpRequestRep.tex  ai.httpRequestCSM.tex ai.httpRequestED.tex */
#ifndef __AI__H__
#define __AI__H__
#include "libpadsc.h"
typedef struct nIP_s nIP;
typedef struct nIP_csm_s nIP_csm;
typedef struct nIP_ed_s nIP_ed;
typedef struct nIP_acc_s nIP_acc;
struct nIP_s {
  PDC_int32 length;
  PDC_uint8 *elts;
  RBuf_t *_internal;
};
struct nIP_csm_s {
  PDC_base_csm element;		/* per-element checks */
  PDC_base_csm array;		/* entire array checks */
};
struct nIP_ed_s {
  int nerr;		/* Number of array errors */
  PDC_errCode_t errCode;
  int neerr;		/* Number of element errors */
  PDC_loc_t loc;
  int panic;
  int firstError;		/* if errCode == ARRAY_ELEM_ERR, index of first error */
  int length;
  PDC_base_ed *elts;
  RBuf_t *_internal;
};
struct nIP_acc_s {
  PDC_int32_acc length;		/* Accumulator for array length */
  PDC_uint8_acc array;		/* Accumulator for all array elements */
  PDC_uint8_acc arrayDetail[4];		/* Accumulator for first 4 array elements */
};

PDC_error_t nIP_init (PDC_t *pdc,nIP *rep);

PDC_error_t nIP_ed_init (PDC_t *pdc,nIP_ed *ed);

PDC_error_t nIP_cleanup (PDC_t *pdc,nIP *rep);

PDC_error_t nIP_ed_cleanup (PDC_t *pdc,nIP_ed *ed);

PDC_error_t nIP_copy (PDC_t *pdc,nIP *rep_dst,nIP *rep_src);

PDC_error_t nIP_ed_copy (PDC_t *pdc,nIP_ed *ed_dst,nIP_ed *ed_src);

PDC_error_t nIP_read (PDC_t *pdc,nIP_csm *csm,nIP_ed *ed,nIP *rep);

PDC_error_t nIP_acc_init (PDC_t *pdc,nIP_acc *acc);

PDC_error_t nIP_acc_reset (PDC_t *pdc,nIP_acc *acc);

PDC_error_t nIP_acc_cleanup (PDC_t *pdc,nIP_acc *acc);

PDC_error_t nIP_acc_add (PDC_t *pdc,nIP_acc *acc,nIP_ed *ed,nIP *rep);

PDC_error_t nIP_acc_report (PDC_t *pdc,char const *prefix,char const *what,int nst,nIP_acc *acc);
typedef struct sIP_s sIP;
typedef struct sIP_csm_s sIP_csm;
typedef struct sIP_ed_s sIP_ed;
typedef struct sIP_acc_s sIP_acc;
struct sIP_s {
  PDC_int32 length;
  PDC_string *elts;
  RBuf_t *_internal;
};
struct sIP_csm_s {
  PDC_base_csm element;		/* per-element checks */
  PDC_base_csm array;		/* entire array checks */
};
struct sIP_ed_s {
  int nerr;		/* Number of array errors */
  PDC_errCode_t errCode;
  int neerr;		/* Number of element errors */
  PDC_loc_t loc;
  int panic;
  int firstError;		/* if errCode == ARRAY_ELEM_ERR, index of first error */
  int length;
  PDC_base_ed *elts;
  RBuf_t *_internal;
};
struct sIP_acc_s {
  PDC_int32_acc length;		/* Accumulator for array length */
  PDC_string_acc array;		/* Accumulator for all array elements */
  PDC_string_acc arrayDetail[10];		/* Accumulator for first 10 array elements */
};

PDC_error_t sIP_init (PDC_t *pdc,sIP *rep);

PDC_error_t sIP_ed_init (PDC_t *pdc,sIP_ed *ed);

PDC_error_t sIP_cleanup (PDC_t *pdc,sIP *rep);

PDC_error_t sIP_ed_cleanup (PDC_t *pdc,sIP_ed *ed);

PDC_error_t sIP_copy (PDC_t *pdc,sIP *rep_dst,sIP *rep_src);

PDC_error_t sIP_ed_copy (PDC_t *pdc,sIP_ed *ed_dst,sIP_ed *ed_src);

PDC_error_t sIP_read (PDC_t *pdc,sIP_csm *csm,sIP_ed *ed,sIP *rep);

PDC_error_t sIP_acc_init (PDC_t *pdc,sIP_acc *acc);

PDC_error_t sIP_acc_reset (PDC_t *pdc,sIP_acc *acc);

PDC_error_t sIP_acc_cleanup (PDC_t *pdc,sIP_acc *acc);

PDC_error_t sIP_acc_add (PDC_t *pdc,sIP_acc *acc,sIP_ed *ed,sIP *rep);

PDC_error_t sIP_acc_report (PDC_t *pdc,char const *prefix,char const *what,int nst,sIP_acc *acc);
typedef enum host_t_tag_e host_t_tag;
typedef union host_t_u_u host_t_u;
typedef struct host_t_s host_t;
typedef struct host_t_csm_s host_t_csm;
typedef union host_t_ed_u_u host_t_ed_u;
typedef struct host_t_ed_s host_t_ed;
typedef struct host_t_acc_s host_t_acc;
enum host_t_tag_e {
  host_t_err=0,
  resolved=1,
  symbolic=2
  };
union host_t_u_u {
  nIP resolved;		/*  135.207.23.32 */
  sIP symbolic;		/*  www.research.att.com */
};
struct host_t_s {
  host_t_tag tag;
  host_t_u val;
};
union host_t_ed_u_u {
  nIP_ed resolved;
  sIP_ed symbolic;
};
struct host_t_csm_s {
  nIP_csm resolved;
  sIP_csm symbolic;
};
struct host_t_ed_s {
  int nerr;
  PDC_errCode_t errCode;
  PDC_loc_t loc;
  int panic;
  host_t_tag tag;
  host_t_ed_u val;
};
struct host_t_acc_s {
  PDC_int32_acc tag;
  nIP_acc resolved;
  sIP_acc symbolic;
};

char const *host_t_tag2str (host_t_tag which);

PDC_error_t host_t_init (PDC_t *pdc,host_t *rep);

PDC_error_t host_t_ed_init (PDC_t *pdc,host_t_ed *ed);

PDC_error_t host_t_cleanup (PDC_t *pdc,host_t *rep);

PDC_error_t host_t_ed_cleanup (PDC_t *pdc,host_t_ed *ed);

PDC_error_t host_t_copy (PDC_t *pdc,host_t *rep_dst,host_t *rep_src);

PDC_error_t host_t_ed_copy (PDC_t *pdc,host_t_ed *ed_dst,host_t_ed *ed_src);

PDC_error_t host_t_read (PDC_t *pdc,host_t_csm *csm,host_t_ed *ed,host_t *rep);

PDC_error_t host_t_acc_init (PDC_t *pdc,host_t_acc *acc);

PDC_error_t host_t_acc_reset (PDC_t *pdc,host_t_acc *acc);

PDC_error_t host_t_acc_cleanup (PDC_t *pdc,host_t_acc *acc);

PDC_error_t host_t_acc_add (PDC_t *pdc,host_t_acc *acc,host_t_ed *ed,host_t *rep);

PDC_error_t host_t_acc_report (PDC_t *pdc,char const *prefix,char const *what,int nst,host_t_acc *acc);
typedef PDC_char unknown_t;
typedef struct unknown_t_csm_s unknown_t_csm;
typedef struct unknown_t_ed_s unknown_t_ed;
typedef PDC_char_acc unknown_t_acc;
struct unknown_t_csm_s {
  PDC_base_csm base;		/* Base CheckSet mask */
  PDC_base_csm user;		/* User constraint */
};
struct unknown_t_ed_s {
  int nerr;
  PDC_errCode_t errCode;
  PDC_loc_t loc;
  int panic;
  PDC_base_ed base;		/* Base error description */
};

PDC_error_t unknown_t_init (PDC_t *pdc,unknown_t *rep);

PDC_error_t unknown_t_ed_init (PDC_t *pdc,unknown_t_ed *ed);

PDC_error_t unknown_t_cleanup (PDC_t *pdc,unknown_t *rep);

PDC_error_t unknown_t_ed_cleanup (PDC_t *pdc,unknown_t_ed *ed);

PDC_error_t unknown_t_copy (PDC_t *pdc,unknown_t *rep_dst,unknown_t *rep_src);

PDC_error_t unknown_t_ed_copy (PDC_t *pdc,unknown_t_ed *ed_dst,unknown_t_ed *ed_src);

PDC_error_t unknown_t_read (PDC_t *pdc,unknown_t_csm *csm,unknown_t_ed *ed,unknown_t *rep);

PDC_error_t unknown_t_acc_init (PDC_t *pdc,unknown_t_acc *acc);

PDC_error_t unknown_t_acc_reset (PDC_t *pdc,unknown_t_acc *acc);

PDC_error_t unknown_t_acc_cleanup (PDC_t *pdc,unknown_t_acc *acc);

PDC_error_t unknown_t_acc_add (PDC_t *pdc,unknown_t_acc *acc,unknown_t_ed *ed,unknown_t *rep);

PDC_error_t unknown_t_acc_report (PDC_t *pdc,char const *prefix,char const *what,int nst,unknown_t_acc *acc);
typedef enum auth_id_t_tag_e auth_id_t_tag;
typedef union auth_id_t_u_u auth_id_t_u;
typedef struct auth_id_t_s auth_id_t;
typedef struct auth_id_t_csm_s auth_id_t_csm;
typedef union auth_id_t_ed_u_u auth_id_t_ed_u;
typedef struct auth_id_t_ed_s auth_id_t_ed;
typedef struct auth_id_t_acc_s auth_id_t_acc;
enum auth_id_t_tag_e {
  auth_id_t_err=0,
  unauthorized=1,
  id=2
  };
union auth_id_t_u_u {
  unknown_t unauthorized;		/*  non-authenticated http session */
  PDC_string id;		/*  login supplied during authentication */
};
struct auth_id_t_s {
  auth_id_t_tag tag;
  auth_id_t_u val;
};
union auth_id_t_ed_u_u {
  unknown_t_ed unauthorized;
  PDC_base_ed id;
};
struct auth_id_t_csm_s {
  unknown_t_csm unauthorized;
  PDC_base_csm id;
};
struct auth_id_t_ed_s {
  int nerr;
  PDC_errCode_t errCode;
  PDC_loc_t loc;
  int panic;
  auth_id_t_tag tag;
  auth_id_t_ed_u val;
};
struct auth_id_t_acc_s {
  PDC_int32_acc tag;
  unknown_t_acc unauthorized;
  PDC_string_acc id;
};

char const *auth_id_t_tag2str (auth_id_t_tag which);

PDC_error_t auth_id_t_init (PDC_t *pdc,auth_id_t *rep);

PDC_error_t auth_id_t_ed_init (PDC_t *pdc,auth_id_t_ed *ed);

PDC_error_t auth_id_t_cleanup (PDC_t *pdc,auth_id_t *rep);

PDC_error_t auth_id_t_ed_cleanup (PDC_t *pdc,auth_id_t_ed *ed);

PDC_error_t auth_id_t_copy (PDC_t *pdc,auth_id_t *rep_dst,auth_id_t *rep_src);

PDC_error_t auth_id_t_ed_copy (PDC_t *pdc,auth_id_t_ed *ed_dst,auth_id_t_ed *ed_src);

PDC_error_t auth_id_t_read (PDC_t *pdc,auth_id_t_csm *csm,auth_id_t_ed *ed,auth_id_t *rep);

PDC_error_t auth_id_t_acc_init (PDC_t *pdc,auth_id_t_acc *acc);

PDC_error_t auth_id_t_acc_reset (PDC_t *pdc,auth_id_t_acc *acc);

PDC_error_t auth_id_t_acc_cleanup (PDC_t *pdc,auth_id_t_acc *acc);

PDC_error_t auth_id_t_acc_add (PDC_t *pdc,auth_id_t_acc *acc,auth_id_t_ed *ed,auth_id_t *rep);

PDC_error_t auth_id_t_acc_report (PDC_t *pdc,char const *prefix,char const *what,int nst,auth_id_t_acc *acc);
typedef enum contentOpt_t_tag_e contentOpt_t_tag;
typedef union contentOpt_t_u_u contentOpt_t_u;
typedef struct contentOpt_t_s contentOpt_t;
typedef struct contentOpt_t_csm_s contentOpt_t_csm;
typedef union contentOpt_t_ed_u_u contentOpt_t_ed_u;
typedef struct contentOpt_t_ed_s contentOpt_t_ed;
typedef struct contentOpt_t_acc_s contentOpt_t_acc;
enum contentOpt_t_tag_e {
  contentOpt_t_err=0,
  len=1,
  unavailable=2
  };
union contentOpt_t_u_u {
  PDC_uint32 len;		/*  length available */
  unknown_t unavailable;
};
struct contentOpt_t_s {
  contentOpt_t_tag tag;
  contentOpt_t_u val;
};
union contentOpt_t_ed_u_u {
  PDC_base_ed len;
  unknown_t_ed unavailable;
};
struct contentOpt_t_csm_s {
  PDC_base_csm len;
  unknown_t_csm unavailable;
};
struct contentOpt_t_ed_s {
  int nerr;
  PDC_errCode_t errCode;
  PDC_loc_t loc;
  int panic;
  contentOpt_t_tag tag;
  contentOpt_t_ed_u val;
};
struct contentOpt_t_acc_s {
  PDC_int32_acc tag;
  PDC_uint32_acc len;
  unknown_t_acc unavailable;
};

char const *contentOpt_t_tag2str (contentOpt_t_tag which);

PDC_error_t contentOpt_t_init (PDC_t *pdc,contentOpt_t *rep);

PDC_error_t contentOpt_t_ed_init (PDC_t *pdc,contentOpt_t_ed *ed);

PDC_error_t contentOpt_t_cleanup (PDC_t *pdc,contentOpt_t *rep);

PDC_error_t contentOpt_t_ed_cleanup (PDC_t *pdc,contentOpt_t_ed *ed);

PDC_error_t contentOpt_t_copy (PDC_t *pdc,contentOpt_t *rep_dst,contentOpt_t *rep_src);

PDC_error_t contentOpt_t_ed_copy (PDC_t *pdc,contentOpt_t_ed *ed_dst,contentOpt_t_ed *ed_src);

PDC_error_t contentOpt_t_read (PDC_t *pdc,contentOpt_t_csm *csm,contentOpt_t_ed *ed,contentOpt_t *rep);

PDC_error_t contentOpt_t_acc_init (PDC_t *pdc,contentOpt_t_acc *acc);

PDC_error_t contentOpt_t_acc_reset (PDC_t *pdc,contentOpt_t_acc *acc);

PDC_error_t contentOpt_t_acc_cleanup (PDC_t *pdc,contentOpt_t_acc *acc);

PDC_error_t contentOpt_t_acc_add (PDC_t *pdc,contentOpt_t_acc *acc,contentOpt_t_ed *ed,contentOpt_t *rep);

PDC_error_t contentOpt_t_acc_report (PDC_t *pdc,char const *prefix,char const *what,int nst,contentOpt_t_acc *acc);
typedef struct http_v_t_s http_v_t;
typedef struct http_v_t_csm_s http_v_t_csm;
typedef struct http_v_t_ed_s http_v_t_ed;
typedef struct http_v_t_acc_s http_v_t_acc;
struct http_v_t_s {
  PDC_uint8 major;
  PDC_uint8 minor;		/*  http minor mode */
};
struct http_v_t_csm_s {
  PDC_base_csm structLevel;
  PDC_base_csm major;
  PDC_base_csm minor;
};
struct http_v_t_ed_s {
  int nerr;
  PDC_errCode_t errCode;
  PDC_loc_t loc;
  int panic;
  PDC_base_ed major;
  PDC_base_ed minor;
};
struct http_v_t_acc_s {
  PDC_int32_acc nerr;
  PDC_uint8_acc major;
  PDC_uint8_acc minor;		/*  http minor mode */
};

PDC_error_t http_v_t_init (PDC_t *pdc,http_v_t *rep);

PDC_error_t http_v_t_ed_init (PDC_t *pdc,http_v_t_ed *ed);

PDC_error_t http_v_t_cleanup (PDC_t *pdc,http_v_t *rep);

PDC_error_t http_v_t_ed_cleanup (PDC_t *pdc,http_v_t_ed *ed);

PDC_error_t http_v_t_copy (PDC_t *pdc,http_v_t *rep_dst,http_v_t *rep_src);

PDC_error_t http_v_t_ed_copy (PDC_t *pdc,http_v_t_ed *ed_dst,http_v_t_ed *ed_src);

PDC_error_t http_v_t_read (PDC_t *pdc,http_v_t_csm *csm,http_v_t_ed *ed,http_v_t *rep);

int http_v_t_write (PDC_t *pdc,Sfio_t *io,http_v_t_ed *ed,http_v_t *rep);

PDC_error_t http_v_t_acc_init (PDC_t *pdc,http_v_t_acc *acc);

PDC_error_t http_v_t_acc_reset (PDC_t *pdc,http_v_t_acc *acc);

PDC_error_t http_v_t_acc_cleanup (PDC_t *pdc,http_v_t_acc *acc);

PDC_error_t http_v_t_acc_add (PDC_t *pdc,http_v_t_acc *acc,http_v_t_ed *ed,http_v_t *rep);

PDC_error_t http_v_t_acc_report (PDC_t *pdc,char const *prefix,char const *what,int nst,http_v_t_acc *acc);
typedef enum http_method_t_e http_method_t;
typedef PDC_base_csm http_method_t_csm;
typedef PDC_base_ed http_method_t_ed;
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

PDC_error_t http_method_t_ed_init (PDC_t *pdc,http_method_t_ed *ed);

PDC_error_t http_method_t_cleanup (PDC_t *pdc,http_method_t *rep);

PDC_error_t http_method_t_ed_cleanup (PDC_t *pdc,http_method_t_ed *ed);

PDC_error_t http_method_t_copy (PDC_t *pdc,http_method_t *rep_dst,http_method_t *rep_src);

PDC_error_t http_method_t_ed_copy (PDC_t *pdc,http_method_t_ed *ed_dst,http_method_t_ed *ed_src);

PDC_error_t http_method_t_read (PDC_t *pdc,http_method_t_csm *csm,http_method_t_ed *ed,http_method_t *rep);

PDC_error_t http_method_t_acc_init (PDC_t *pdc,http_method_t_acc *acc);

PDC_error_t http_method_t_acc_reset (PDC_t *pdc,http_method_t_acc *acc);

PDC_error_t http_method_t_acc_cleanup (PDC_t *pdc,http_method_t_acc *acc);

PDC_error_t http_method_t_acc_add (PDC_t *pdc,http_method_t_acc *acc,http_method_t_ed *ed,http_method_t *rep);

PDC_error_t http_method_t_acc_report (PDC_t *pdc,char const *prefix,char const *what,int nst,http_method_t_acc *acc);

int checkVersion (http_v_t version,http_method_t meth);
typedef struct http_request_t_s http_request_t;
typedef struct http_request_t_csm_s http_request_t_csm;
typedef struct http_request_t_ed_s http_request_t_ed;
typedef struct http_request_t_acc_s http_request_t_acc;

/*@BEGIN ai.httpRequestRep.tex*/
typedef struct http_request_t_s http_request_t;

struct http_request_t_s {
  http_method_t meth;	  /*  Method used during request */
  PDC_string    req_uri;  /*  Requested uri. */
  http_v_t      version;  /*  HTTP version. 
                              checkVersion(version, meth) */
};
/*@END ai.httpRequestRep.tex*/

/*@BEGIN ai.httpRequestCSM.tex*/
typedef struct http_request_t_csm_s http_request_t_csm;

struct http_request_t_csm_s {
  PDC_base_csm structLevel;
  http_method_t_csm meth;
  PDC_base_csm req_uri;
  http_v_t_csm version;
};
/*@END ai.httpRequestCSM.tex*/

struct http_request_t_ed_s {
  int nerr;
  PDC_errCode_t errCode;
  PDC_loc_t loc;
  int panic;
  http_method_t_ed meth;
  PDC_base_ed req_uri;
  http_v_t_ed version;
};
struct http_request_t_acc_s {
  PDC_int32_acc nerr;
  http_method_t_acc meth;		/*  Method used during request */
  PDC_string_acc req_uri;		/*  Requested uri. */
  http_v_t_acc version;		/*  HTTP version. checkVersion(version, meth) */
};

PDC_error_t http_request_t_init (PDC_t *pdc,http_request_t *rep);

PDC_error_t http_request_t_ed_init (PDC_t *pdc,http_request_t_ed *ed);

PDC_error_t http_request_t_cleanup (PDC_t *pdc,http_request_t *rep);

PDC_error_t http_request_t_ed_cleanup (PDC_t *pdc,http_request_t_ed *ed);

PDC_error_t http_request_t_copy (PDC_t *pdc,http_request_t *rep_dst,http_request_t *rep_src);

PDC_error_t http_request_t_ed_copy (PDC_t *pdc,http_request_t_ed *ed_dst,http_request_t_ed *ed_src);

PDC_error_t http_request_t_read (PDC_t *pdc,http_request_t_csm *csm,http_request_t_ed *ed,http_request_t *rep);

int http_request_t_write (PDC_t *pdc,Sfio_t *io,http_request_t_ed *ed,http_request_t *rep);

PDC_error_t http_request_t_acc_init (PDC_t *pdc,http_request_t_acc *acc);

PDC_error_t http_request_t_acc_reset (PDC_t *pdc,http_request_t_acc *acc);

PDC_error_t http_request_t_acc_cleanup (PDC_t *pdc,http_request_t_acc *acc);

PDC_error_t http_request_t_acc_add (PDC_t *pdc,http_request_t_acc *acc,http_request_t_ed *ed,http_request_t *rep);

PDC_error_t http_request_t_acc_report (PDC_t *pdc,char const *prefix,char const *what,int nst,http_request_t_acc *acc);
typedef struct http_clf_t_s http_clf_t;
typedef struct http_clf_t_csm_s http_clf_t_csm;
typedef struct http_clf_t_ed_s http_clf_t_ed;
typedef struct http_clf_t_acc_s http_clf_t_acc;
struct http_clf_t_s {
  host_t host;		/*  IP address of client requesting service */
  auth_id_t remoteID;		/*  Remote identity; '-' indicates not obtained. */
  auth_id_t auth;		/*  Name of authenticated user. */
  PDC_uint32 request;		/*  Request. */
  PDC_uint16 response;		/*  3-digit response code */
  contentOpt_t contentLength;		/*  Number of bytes in request response. */
};
struct http_clf_t_csm_s {
  PDC_base_csm structLevel;
  host_t_csm host;
  auth_id_t_csm remoteID;
  auth_id_t_csm auth;
  PDC_base_csm request;
  PDC_base_csm response;
  contentOpt_t_csm contentLength;
};
struct http_clf_t_ed_s {
  int nerr;
  PDC_errCode_t errCode;
  PDC_loc_t loc;
  int panic;
  host_t_ed host;
  auth_id_t_ed remoteID;
  auth_id_t_ed auth;
  PDC_base_ed request;
  PDC_base_ed response;
  contentOpt_t_ed contentLength;
};
struct http_clf_t_acc_s {
  PDC_int32_acc nerr;
  host_t_acc host;		/*  IP address of client requesting service */
  auth_id_t_acc remoteID;		/*  Remote identity; '-' indicates not obtained. */
  auth_id_t_acc auth;		/*  Name of authenticated user. */
  PDC_uint16_acc response;		/*  3-digit response code */
  contentOpt_t_acc contentLength;		/*  Number of bytes in request response. */
};

PDC_error_t http_clf_t_init (PDC_t *pdc,http_clf_t *rep);

PDC_error_t http_clf_t_ed_init (PDC_t *pdc,http_clf_t_ed *ed);

PDC_error_t http_clf_t_cleanup (PDC_t *pdc,http_clf_t *rep);

PDC_error_t http_clf_t_ed_cleanup (PDC_t *pdc,http_clf_t_ed *ed);

PDC_error_t http_clf_t_copy (PDC_t *pdc,http_clf_t *rep_dst,http_clf_t *rep_src);

PDC_error_t http_clf_t_ed_copy (PDC_t *pdc,http_clf_t_ed *ed_dst,http_clf_t_ed *ed_src);

PDC_error_t http_clf_t_read (PDC_t *pdc,http_clf_t_csm *csm,http_clf_t_ed *ed,http_clf_t *rep);

int http_clf_t_write (PDC_t *pdc,Sfio_t *io,http_clf_t_ed *ed,http_clf_t *rep);

PDC_error_t http_clf_t_acc_init (PDC_t *pdc,http_clf_t_acc *acc);

PDC_error_t http_clf_t_acc_reset (PDC_t *pdc,http_clf_t_acc *acc);

PDC_error_t http_clf_t_acc_cleanup (PDC_t *pdc,http_clf_t_acc *acc);

PDC_error_t http_clf_t_acc_add (PDC_t *pdc,http_clf_t_acc *acc,http_clf_t_ed *ed,http_clf_t *rep);

PDC_error_t http_clf_t_acc_report (PDC_t *pdc,char const *prefix,char const *what,int nst,http_clf_t_acc *acc);
typedef struct log_t_s log_t;
typedef struct log_t_csm_s log_t_csm;
typedef struct log_t_ed_s log_t_ed;
typedef struct log_t_acc_s log_t_acc;
struct log_t_s {
  PDC_int32 length;
  http_clf_t *elts;
  RBuf_t *_internal;
};
struct log_t_csm_s {
  http_clf_t_csm element;		/* per-element checks */
  PDC_base_csm array;		/* entire array checks */
};
struct log_t_ed_s {
  int nerr;		/* Number of array errors */
  PDC_errCode_t errCode;
  int neerr;		/* Number of element errors */
  PDC_loc_t loc;
  int panic;
  int firstError;		/* if errCode == ARRAY_ELEM_ERR, index of first error */
  int length;
  http_clf_t_ed *elts;
  RBuf_t *_internal;
};
struct log_t_acc_s {
  PDC_int32_acc length;		/* Accumulator for array length */
  http_clf_t_acc array;		/* Accumulator for all array elements */
  http_clf_t_acc arrayDetail[10];		/* Accumulator for first 10 array elements */
};

PDC_error_t log_t_init (PDC_t *pdc,log_t *rep);

PDC_error_t log_t_ed_init (PDC_t *pdc,log_t_ed *ed);

PDC_error_t log_t_cleanup (PDC_t *pdc,log_t *rep);

PDC_error_t log_t_ed_cleanup (PDC_t *pdc,log_t_ed *ed);

PDC_error_t log_t_copy (PDC_t *pdc,log_t *rep_dst,log_t *rep_src);

PDC_error_t log_t_ed_copy (PDC_t *pdc,log_t_ed *ed_dst,log_t_ed *ed_src);

PDC_error_t log_t_read (PDC_t *pdc,log_t_csm *csm,log_t_ed *ed,log_t *rep);

PDC_error_t log_t_acc_init (PDC_t *pdc,log_t_acc *acc);

PDC_error_t log_t_acc_reset (PDC_t *pdc,log_t_acc *acc);

PDC_error_t log_t_acc_cleanup (PDC_t *pdc,log_t_acc *acc);

PDC_error_t log_t_acc_add (PDC_t *pdc,log_t_acc *acc,log_t_ed *ed,log_t *rep);

PDC_error_t log_t_acc_report (PDC_t *pdc,char const *prefix,char const *what,int nst,log_t_acc *acc);

#endif /*  __AI__H__  */
