#ifndef __DIBBLER2__H__
#define __DIBBLER2__H__
#include "libpadsc.h"
typedef struct auint32_vbar_s auint32_vbar;
typedef struct auint32_vbar_em_s auint32_vbar_em;
typedef struct auint32_vbar_ed_s auint32_vbar_ed;
typedef struct auint32_vbar_acc_s auint32_vbar_acc;
struct auint32_vbar_s {
  PDC_uint32 val;
};
struct auint32_vbar_em_s {
  PDC_base_em structLevel;
  PDC_base_em val;
};
struct auint32_vbar_ed_s {
  int nerr;
  PDC_errCode_t errCode;
  PDC_loc_t loc;
  int panic;
  PDC_base_ed val;
};
struct auint32_vbar_acc_s {
  PDC_uint32_acc val;
};

PDC_error_t auint32_vbar_read (PDC_t *pdc,auint32_vbar_em *em,auint32_vbar_ed *ed,auint32_vbar *rep);

PDC_error_t auint32_vbar_acc_init (PDC_t *pdc,auint32_vbar_acc *acc);

PDC_error_t auint32_vbar_acc_reset (PDC_t *pdc,auint32_vbar_acc *acc);

PDC_error_t auint32_vbar_acc_cleanup (PDC_t *pdc,auint32_vbar_acc *acc);

PDC_error_t auint32_vbar_acc_add (PDC_t *pdc,auint32_vbar_acc *acc,auint32_vbar_ed *ed,auint32_vbar *rep);

PDC_error_t auint32_vbar_acc_report (PDC_t *pdc,char const *prefix,char const *what,int nst,auint32_vbar_acc *acc);
typedef struct auint64_vbar_s auint64_vbar;
typedef struct auint64_vbar_em_s auint64_vbar_em;
typedef struct auint64_vbar_ed_s auint64_vbar_ed;
typedef struct auint64_vbar_acc_s auint64_vbar_acc;
struct auint64_vbar_s {
  PDC_uint64 val;
};
struct auint64_vbar_em_s {
  PDC_base_em structLevel;
  PDC_base_em val;
};
struct auint64_vbar_ed_s {
  int nerr;
  PDC_errCode_t errCode;
  PDC_loc_t loc;
  int panic;
  PDC_base_ed val;
};
struct auint64_vbar_acc_s {
  PDC_uint64_acc val;
};

PDC_error_t auint64_vbar_read (PDC_t *pdc,auint64_vbar_em *em,auint64_vbar_ed *ed,auint64_vbar *rep);

PDC_error_t auint64_vbar_acc_init (PDC_t *pdc,auint64_vbar_acc *acc);

PDC_error_t auint64_vbar_acc_reset (PDC_t *pdc,auint64_vbar_acc *acc);

PDC_error_t auint64_vbar_acc_cleanup (PDC_t *pdc,auint64_vbar_acc *acc);

PDC_error_t auint64_vbar_acc_add (PDC_t *pdc,auint64_vbar_acc *acc,auint64_vbar_ed *ed,auint64_vbar *rep);

PDC_error_t auint64_vbar_acc_report (PDC_t *pdc,char const *prefix,char const *what,int nst,auint64_vbar_acc *acc);
typedef struct just_vbar_s just_vbar;
typedef struct just_vbar_em_s just_vbar_em;
typedef struct just_vbar_ed_s just_vbar_ed;
typedef struct just_vbar_acc_s just_vbar_acc;
struct just_vbar_s {
  PDC_int32 d;
};
struct just_vbar_em_s {
  PDC_base_em structLevel;
  PDC_base_em d;
};
struct just_vbar_ed_s {
  int nerr;
  PDC_errCode_t errCode;
  PDC_loc_t loc;
  int panic;
  PDC_base_ed d;
};
struct just_vbar_acc_s {
  PDC_int32_acc d;
};

PDC_error_t just_vbar_read (PDC_t *pdc,just_vbar_em *em,just_vbar_ed *ed,just_vbar *rep);

PDC_error_t just_vbar_acc_init (PDC_t *pdc,just_vbar_acc *acc);

PDC_error_t just_vbar_acc_reset (PDC_t *pdc,just_vbar_acc *acc);

PDC_error_t just_vbar_acc_cleanup (PDC_t *pdc,just_vbar_acc *acc);

PDC_error_t just_vbar_acc_add (PDC_t *pdc,just_vbar_acc *acc,just_vbar_ed *ed,just_vbar *rep);

PDC_error_t just_vbar_acc_report (PDC_t *pdc,char const *prefix,char const *what,int nst,just_vbar_acc *acc);
typedef enum opt_auint32_vbar_tag_e opt_auint32_vbar_tag;
typedef union opt_auint32_vbar_u_u opt_auint32_vbar_u;
typedef struct opt_auint32_vbar_s opt_auint32_vbar;
typedef struct opt_auint32_vbar_em_s opt_auint32_vbar_em;
typedef struct opt_auint32_vbar_ed_s opt_auint32_vbar_ed;
typedef struct opt_auint32_vbar_acc_s opt_auint32_vbar_acc;
enum opt_auint32_vbar_tag_e {
  yes32=1,
  no32=2,
};
union opt_auint32_vbar_u_u {
  auint32_vbar yes32;
  just_vbar no32;
};
struct opt_auint32_vbar_s {
  opt_auint32_vbar_tag tag;
  opt_auint32_vbar_u val;
};
struct opt_auint32_vbar_em_s {
  auint32_vbar_em yes32;
  just_vbar_em no32;
};
struct opt_auint32_vbar_ed_s {
  int nerr;
  PDC_errCode_t errCode;
  PDC_loc_t loc;
  int panic;
  auint32_vbar_ed yes32;
  just_vbar_ed no32;
};
struct opt_auint32_vbar_acc_s {
  PDC_int32_acc tag;
  auint32_vbar_acc yes32;
  just_vbar_acc no32;
};

char const *opt_auint32_vbar_tag2str (opt_auint32_vbar_tag which);

PDC_error_t opt_auint32_vbar_read (PDC_t *pdc,opt_auint32_vbar_em *em,opt_auint32_vbar_ed *ed,opt_auint32_vbar *rep);

PDC_error_t opt_auint32_vbar_acc_init (PDC_t *pdc,opt_auint32_vbar_acc *acc);

PDC_error_t opt_auint32_vbar_acc_reset (PDC_t *pdc,opt_auint32_vbar_acc *acc);

PDC_error_t opt_auint32_vbar_acc_cleanup (PDC_t *pdc,opt_auint32_vbar_acc *acc);

PDC_error_t opt_auint32_vbar_acc_add (PDC_t *pdc,opt_auint32_vbar_acc *acc,opt_auint32_vbar_ed *ed,opt_auint32_vbar *rep);

PDC_error_t opt_auint32_vbar_acc_report (PDC_t *pdc,char const *prefix,char const *what,int nst,opt_auint32_vbar_acc *acc);
typedef enum opt_auint64_vbar_tag_e opt_auint64_vbar_tag;
typedef union opt_auint64_vbar_u_u opt_auint64_vbar_u;
typedef struct opt_auint64_vbar_s opt_auint64_vbar;
typedef struct opt_auint64_vbar_em_s opt_auint64_vbar_em;
typedef struct opt_auint64_vbar_ed_s opt_auint64_vbar_ed;
typedef struct opt_auint64_vbar_acc_s opt_auint64_vbar_acc;
enum opt_auint64_vbar_tag_e {
  yes64=1,
  no64=2,
};
union opt_auint64_vbar_u_u {
  auint64_vbar yes64;
  just_vbar no64;
};
struct opt_auint64_vbar_s {
  opt_auint64_vbar_tag tag;
  opt_auint64_vbar_u val;
};
struct opt_auint64_vbar_em_s {
  auint64_vbar_em yes64;
  just_vbar_em no64;
};
struct opt_auint64_vbar_ed_s {
  int nerr;
  PDC_errCode_t errCode;
  PDC_loc_t loc;
  int panic;
  auint64_vbar_ed yes64;
  just_vbar_ed no64;
};
struct opt_auint64_vbar_acc_s {
  PDC_int32_acc tag;
  auint64_vbar_acc yes64;
  just_vbar_acc no64;
};

char const *opt_auint64_vbar_tag2str (opt_auint64_vbar_tag which);

PDC_error_t opt_auint64_vbar_read (PDC_t *pdc,opt_auint64_vbar_em *em,opt_auint64_vbar_ed *ed,opt_auint64_vbar *rep);

PDC_error_t opt_auint64_vbar_acc_init (PDC_t *pdc,opt_auint64_vbar_acc *acc);

PDC_error_t opt_auint64_vbar_acc_reset (PDC_t *pdc,opt_auint64_vbar_acc *acc);

PDC_error_t opt_auint64_vbar_acc_cleanup (PDC_t *pdc,opt_auint64_vbar_acc *acc);

PDC_error_t opt_auint64_vbar_acc_add (PDC_t *pdc,opt_auint64_vbar_acc *acc,opt_auint64_vbar_ed *ed,opt_auint64_vbar *rep);

PDC_error_t opt_auint64_vbar_acc_report (PDC_t *pdc,char const *prefix,char const *what,int nst,opt_auint64_vbar_acc *acc);
typedef struct no_pn_vbar_s no_pn_vbar;
typedef struct no_pn_vbar_em_s no_pn_vbar_em;
typedef struct no_pn_vbar_ed_s no_pn_vbar_ed;
typedef struct no_pn_vbar_acc_s no_pn_vbar_acc;
struct no_pn_vbar_s {
  PDC_int32 d;
};
struct no_pn_vbar_em_s {
  PDC_base_em structLevel;
  PDC_base_em d;
};
struct no_pn_vbar_ed_s {
  int nerr;
  PDC_errCode_t errCode;
  PDC_loc_t loc;
  int panic;
  PDC_base_ed d;
};
struct no_pn_vbar_acc_s {
  PDC_int32_acc d;
};

PDC_error_t no_pn_vbar_read (PDC_t *pdc,no_pn_vbar_em *em,no_pn_vbar_ed *ed,no_pn_vbar *rep);

PDC_error_t no_pn_vbar_acc_init (PDC_t *pdc,no_pn_vbar_acc *acc);

PDC_error_t no_pn_vbar_acc_reset (PDC_t *pdc,no_pn_vbar_acc *acc);

PDC_error_t no_pn_vbar_acc_cleanup (PDC_t *pdc,no_pn_vbar_acc *acc);

PDC_error_t no_pn_vbar_acc_add (PDC_t *pdc,no_pn_vbar_acc *acc,no_pn_vbar_ed *ed,no_pn_vbar *rep);

PDC_error_t no_pn_vbar_acc_report (PDC_t *pdc,char const *prefix,char const *what,int nst,no_pn_vbar_acc *acc);
typedef enum dib_pn_vbar_tag_e dib_pn_vbar_tag;
typedef union dib_pn_vbar_u_u dib_pn_vbar_u;
typedef struct dib_pn_vbar_s dib_pn_vbar;
typedef struct dib_pn_vbar_em_s dib_pn_vbar_em;
typedef struct dib_pn_vbar_ed_s dib_pn_vbar_ed;
typedef struct dib_pn_vbar_acc_s dib_pn_vbar_acc;
enum dib_pn_vbar_tag_e {
  yesPN=1,
  noPN=2,
};
union dib_pn_vbar_u_u {
  auint64_vbar yesPN;
  no_pn_vbar noPN;
};
struct dib_pn_vbar_s {
  dib_pn_vbar_tag tag;
  dib_pn_vbar_u val;
};
struct dib_pn_vbar_em_s {
  auint64_vbar_em yesPN;
  no_pn_vbar_em noPN;
};
struct dib_pn_vbar_ed_s {
  int nerr;
  PDC_errCode_t errCode;
  PDC_loc_t loc;
  int panic;
  auint64_vbar_ed yesPN;
  no_pn_vbar_ed noPN;
};
struct dib_pn_vbar_acc_s {
  PDC_int32_acc tag;
  auint64_vbar_acc yesPN;
  no_pn_vbar_acc noPN;
};

char const *dib_pn_vbar_tag2str (dib_pn_vbar_tag which);

PDC_error_t dib_pn_vbar_read (PDC_t *pdc,dib_pn_vbar_em *em,dib_pn_vbar_ed *ed,dib_pn_vbar *rep);

PDC_error_t dib_pn_vbar_acc_init (PDC_t *pdc,dib_pn_vbar_acc *acc);

PDC_error_t dib_pn_vbar_acc_reset (PDC_t *pdc,dib_pn_vbar_acc *acc);

PDC_error_t dib_pn_vbar_acc_cleanup (PDC_t *pdc,dib_pn_vbar_acc *acc);

PDC_error_t dib_pn_vbar_acc_add (PDC_t *pdc,dib_pn_vbar_acc *acc,dib_pn_vbar_ed *ed,dib_pn_vbar *rep);

PDC_error_t dib_pn_vbar_acc_report (PDC_t *pdc,char const *prefix,char const *what,int nst,dib_pn_vbar_acc *acc);
typedef struct event_s event;
typedef struct event_em_s event_em;
typedef struct event_ed_s event_ed;
typedef struct event_acc_s event_acc;
struct event_s {
  PDC_string state;
  PDC_uint32 tstamp;
};
struct event_em_s {
  PDC_base_em structLevel;
  PDC_base_em state;
  PDC_base_em tstamp;
};
struct event_ed_s {
  int nerr;
  PDC_errCode_t errCode;
  PDC_loc_t loc;
  int panic;
  PDC_base_ed state;
  PDC_base_ed tstamp;
};
struct event_acc_s {
  PDC_string_acc state;
  PDC_uint32_acc tstamp;
};

PDC_error_t event_read (PDC_t *pdc,event_em *em,event_ed *ed,event *rep);

PDC_error_t event_acc_init (PDC_t *pdc,event_acc *acc);

PDC_error_t event_acc_reset (PDC_t *pdc,event_acc *acc);

PDC_error_t event_acc_cleanup (PDC_t *pdc,event_acc *acc);

PDC_error_t event_acc_add (PDC_t *pdc,event_acc *acc,event_ed *ed,event *rep);

PDC_error_t event_acc_report (PDC_t *pdc,char const *prefix,char const *what,int nst,event_acc *acc);

PDC_error_t event_init (PDC_t *pdc,event *rep);

PDC_error_t event_ed_init (PDC_t *pdc,event_ed *ed);

PDC_error_t event_cleanup (PDC_t *pdc,event *rep);

PDC_error_t event_ed_cleanup (PDC_t *pdc,event_ed *ed);
typedef struct out_sum_header_s out_sum_header;
typedef struct out_sum_header_em_s out_sum_header_em;
typedef struct out_sum_header_ed_s out_sum_header_ed;
typedef struct out_sum_header_acc_s out_sum_header_acc;
struct out_sum_header_s {
  PDC_uint32 tstamp;
};
struct out_sum_header_em_s {
  PDC_base_em structLevel;
  PDC_base_em tstamp;
};
struct out_sum_header_ed_s {
  int nerr;
  PDC_errCode_t errCode;
  PDC_loc_t loc;
  int panic;
  PDC_base_ed tstamp;
};
struct out_sum_header_acc_s {
  PDC_uint32_acc tstamp;
};

PDC_error_t out_sum_header_read (PDC_t *pdc,out_sum_header_em *em,out_sum_header_ed *ed,out_sum_header *rep);

PDC_error_t out_sum_header_acc_init (PDC_t *pdc,out_sum_header_acc *acc);

PDC_error_t out_sum_header_acc_reset (PDC_t *pdc,out_sum_header_acc *acc);

PDC_error_t out_sum_header_acc_cleanup (PDC_t *pdc,out_sum_header_acc *acc);

PDC_error_t out_sum_header_acc_add (PDC_t *pdc,out_sum_header_acc *acc,out_sum_header_ed *ed,out_sum_header *rep);

PDC_error_t out_sum_header_acc_report (PDC_t *pdc,char const *prefix,char const *what,int nst,out_sum_header_acc *acc);
typedef struct eventSeq_s eventSeq;
typedef struct eventSeq_em_s eventSeq_em;
typedef struct eventSeq_ed_s eventSeq_ed;
typedef struct eventSeq_acc_s eventSeq_acc;
struct eventSeq_s {
  PDC_int32 length;
  event *eventSeq;
  RBuf_t *_internal;
};
struct eventSeq_em_s {
  event_em element;		/* per-element checks */
  PDC_base_em array;		/* entire array checks */
};
struct eventSeq_ed_s {
  int nerr;		/* Number of array errors */
  PDC_errCode_t errCode;
  int neerr;		/* Number of element errors */
  PDC_loc_t loc;
  int panic;
  int firstError;		/* if errCode == ARRAY_ELEM_ERR, index of first error */
  int length;
  event_ed *eventSeq;
  RBuf_t *_internal;
};
struct eventSeq_acc_s {
  PDC_int32_acc length;		/* Accumulator for array length */
  event_acc array;		/* Accumulator for all array elements */
  event_acc arrayDetail[10];		/* Accumulator for first 10 array elements */
};

PDC_error_t eventSeq_read (PDC_t *pdc,eventSeq_em *em,int size,eventSeq_ed *ed,eventSeq *rep);

PDC_error_t eventSeq_acc_init (PDC_t *pdc,eventSeq_acc *acc);

PDC_error_t eventSeq_acc_reset (PDC_t *pdc,eventSeq_acc *acc);

PDC_error_t eventSeq_acc_cleanup (PDC_t *pdc,eventSeq_acc *acc);

PDC_error_t eventSeq_acc_add (PDC_t *pdc,eventSeq_acc *acc,eventSeq_ed *ed,eventSeq *rep);

PDC_error_t eventSeq_acc_report (PDC_t *pdc,char const *prefix,char const *what,int nst,eventSeq_acc *acc);

PDC_error_t eventSeq_init (PDC_t *pdc,eventSeq *rep);

PDC_error_t eventSeq_ed_init (PDC_t *pdc,eventSeq_ed *ed);

PDC_error_t eventSeq_cleanup (PDC_t *pdc,eventSeq *rep);

PDC_error_t eventSeq_ed_cleanup (PDC_t *pdc,eventSeq_ed *ed);

int getLen (int numBars);
typedef struct out_sum_fixed1_s out_sum_fixed1;
typedef struct out_sum_fixed1_em_s out_sum_fixed1_em;
typedef struct out_sum_fixed1_ed_s out_sum_fixed1_ed;
typedef struct out_sum_fixed1_acc_s out_sum_fixed1_acc;
struct out_sum_fixed1_s {
  auint32_vbar order_num;
  auint32_vbar order_item;
  dib_pn_vbar servicen;
  dib_pn_vbar billing_tn;
  auint32_vbar zip_code;
  dib_pn_vbar nlp_service_tn;
  dib_pn_vbar nlp_billing_tn;
};
struct out_sum_fixed1_em_s {
  PDC_base_em structLevel;
  auint32_vbar_em order_num;
  auint32_vbar_em order_item;
  dib_pn_vbar_em servicen;
  dib_pn_vbar_em billing_tn;
  auint32_vbar_em zip_code;
  dib_pn_vbar_em nlp_service_tn;
  dib_pn_vbar_em nlp_billing_tn;
};
struct out_sum_fixed1_ed_s {
  int nerr;
  PDC_errCode_t errCode;
  PDC_loc_t loc;
  int panic;
  auint32_vbar_ed order_num;
  auint32_vbar_ed order_item;
  dib_pn_vbar_ed servicen;
  dib_pn_vbar_ed billing_tn;
  auint32_vbar_ed zip_code;
  dib_pn_vbar_ed nlp_service_tn;
  dib_pn_vbar_ed nlp_billing_tn;
};
struct out_sum_fixed1_acc_s {
  auint32_vbar_acc order_num;
  auint32_vbar_acc order_item;
  dib_pn_vbar_acc servicen;
  dib_pn_vbar_acc billing_tn;
  auint32_vbar_acc zip_code;
  dib_pn_vbar_acc nlp_service_tn;
  dib_pn_vbar_acc nlp_billing_tn;
};

PDC_error_t out_sum_fixed1_read (PDC_t *pdc,out_sum_fixed1_em *em,out_sum_fixed1_ed *ed,out_sum_fixed1 *rep);

PDC_error_t out_sum_fixed1_acc_init (PDC_t *pdc,out_sum_fixed1_acc *acc);

PDC_error_t out_sum_fixed1_acc_reset (PDC_t *pdc,out_sum_fixed1_acc *acc);

PDC_error_t out_sum_fixed1_acc_cleanup (PDC_t *pdc,out_sum_fixed1_acc *acc);

PDC_error_t out_sum_fixed1_acc_add (PDC_t *pdc,out_sum_fixed1_acc *acc,out_sum_fixed1_ed *ed,out_sum_fixed1 *rep);

PDC_error_t out_sum_fixed1_acc_report (PDC_t *pdc,char const *prefix,char const *what,int nst,out_sum_fixed1_acc *acc);
typedef struct out_sum_fixed2_s out_sum_fixed2;
typedef struct out_sum_fixed2_em_s out_sum_fixed2_em;
typedef struct out_sum_fixed2_ed_s out_sum_fixed2_ed;
typedef struct out_sum_fixed2_acc_s out_sum_fixed2_acc;
struct out_sum_fixed2_s {
  opt_auint32_vbar siid;
  opt_auint32_vbar create_id;
  opt_auint64_vbar rampII;
  auint32_vbar order_type;
  PDC_uint32 parent_order;
};
struct out_sum_fixed2_em_s {
  PDC_base_em structLevel;
  opt_auint32_vbar_em siid;
  opt_auint32_vbar_em create_id;
  opt_auint64_vbar_em rampII;
  auint32_vbar_em order_type;
  PDC_base_em parent_order;
};
struct out_sum_fixed2_ed_s {
  int nerr;
  PDC_errCode_t errCode;
  PDC_loc_t loc;
  int panic;
  opt_auint32_vbar_ed siid;
  opt_auint32_vbar_ed create_id;
  opt_auint64_vbar_ed rampII;
  auint32_vbar_ed order_type;
  PDC_base_ed parent_order;
};
struct out_sum_fixed2_acc_s {
  opt_auint32_vbar_acc siid;
  opt_auint32_vbar_acc create_id;
  opt_auint64_vbar_acc rampII;
  auint32_vbar_acc order_type;
  PDC_uint32_acc parent_order;
};

PDC_error_t out_sum_fixed2_read (PDC_t *pdc,out_sum_fixed2_em *em,out_sum_fixed2_ed *ed,out_sum_fixed2 *rep);

PDC_error_t out_sum_fixed2_acc_init (PDC_t *pdc,out_sum_fixed2_acc *acc);

PDC_error_t out_sum_fixed2_acc_reset (PDC_t *pdc,out_sum_fixed2_acc *acc);

PDC_error_t out_sum_fixed2_acc_cleanup (PDC_t *pdc,out_sum_fixed2_acc *acc);

PDC_error_t out_sum_fixed2_acc_add (PDC_t *pdc,out_sum_fixed2_acc *acc,out_sum_fixed2_ed *ed,out_sum_fixed2 *rep);

PDC_error_t out_sum_fixed2_acc_report (PDC_t *pdc,char const *prefix,char const *what,int nst,out_sum_fixed2_acc *acc);
typedef struct do_ev_count_s do_ev_count;
typedef struct do_ev_count_em_s do_ev_count_em;
typedef struct do_ev_count_ed_s do_ev_count_ed;
typedef struct do_ev_count_acc_s do_ev_count_acc;
struct do_ev_count_s {
  PDC_int32 ev_count;
};
struct do_ev_count_em_s {
  PDC_base_em structLevel;
  PDC_base_em bars;
  PDC_base_em ev_count;
};
struct do_ev_count_ed_s {
  int nerr;
  PDC_errCode_t errCode;
  PDC_loc_t loc;
  int panic;
  PDC_base_ed bars;
  PDC_base_ed ev_count;
};
struct do_ev_count_acc_s {
  PDC_int32_acc ev_count;
};

PDC_error_t do_ev_count_read (PDC_t *pdc,do_ev_count_em *em,do_ev_count_ed *ed,do_ev_count *rep);

PDC_error_t do_ev_count_acc_init (PDC_t *pdc,do_ev_count_acc *acc);

PDC_error_t do_ev_count_acc_reset (PDC_t *pdc,do_ev_count_acc *acc);

PDC_error_t do_ev_count_acc_cleanup (PDC_t *pdc,do_ev_count_acc *acc);

PDC_error_t do_ev_count_acc_add (PDC_t *pdc,do_ev_count_acc *acc,do_ev_count_ed *ed,do_ev_count *rep);

PDC_error_t do_ev_count_acc_report (PDC_t *pdc,char const *prefix,char const *what,int nst,do_ev_count_acc *acc);
typedef struct out_sum_data_line_s out_sum_data_line;
typedef struct out_sum_data_line_em_s out_sum_data_line_em;
typedef struct out_sum_data_line_ed_s out_sum_data_line_ed;
typedef struct out_sum_data_line_acc_s out_sum_data_line_acc;
struct out_sum_data_line_s {
  out_sum_fixed1 f1;
  do_ev_count c;
  eventSeq events;
  out_sum_fixed2 f2;
};
struct out_sum_data_line_em_s {
  PDC_base_em structLevel;
  out_sum_fixed1_em f1;
  do_ev_count_em c;
  eventSeq_em events;
  out_sum_fixed2_em f2;
};
struct out_sum_data_line_ed_s {
  int nerr;
  PDC_errCode_t errCode;
  PDC_loc_t loc;
  int panic;
  out_sum_fixed1_ed f1;
  do_ev_count_ed c;
  eventSeq_ed events;
  out_sum_fixed2_ed f2;
};
struct out_sum_data_line_acc_s {
  out_sum_fixed1_acc f1;
  do_ev_count_acc c;
  eventSeq_acc events;
  out_sum_fixed2_acc f2;
};

PDC_error_t out_sum_data_line_read (PDC_t *pdc,out_sum_data_line_em *em,out_sum_data_line_ed *ed,out_sum_data_line *rep);

PDC_error_t out_sum_data_line_acc_init (PDC_t *pdc,out_sum_data_line_acc *acc);

PDC_error_t out_sum_data_line_acc_reset (PDC_t *pdc,out_sum_data_line_acc *acc);

PDC_error_t out_sum_data_line_acc_cleanup (PDC_t *pdc,out_sum_data_line_acc *acc);

PDC_error_t out_sum_data_line_acc_add (PDC_t *pdc,out_sum_data_line_acc *acc,out_sum_data_line_ed *ed,out_sum_data_line *rep);

PDC_error_t out_sum_data_line_acc_report (PDC_t *pdc,char const *prefix,char const *what,int nst,out_sum_data_line_acc *acc);

PDC_error_t out_sum_data_line_init (PDC_t *pdc,out_sum_data_line *rep);

PDC_error_t out_sum_data_line_ed_init (PDC_t *pdc,out_sum_data_line_ed *ed);

PDC_error_t out_sum_data_line_cleanup (PDC_t *pdc,out_sum_data_line *rep);

PDC_error_t out_sum_data_line_ed_cleanup (PDC_t *pdc,out_sum_data_line_ed *ed);

#endif /*  __DIBBLER2__H__  */
