/*@FILE @LEFT union-impl-branches-tag.tex union-impl-branches-rep.tex union-impl-branches-pd.tex union-impl-branches-mask.tex union-impl-branches-ops.tex*/
#ifndef __PUNION__H__
#define __PUNION__H__
#include "padsc.h"

/*@BEGIN union-impl-branches-tag.tex*/
typedef enum branches_tag_e branches_tag;

enum branches_tag_e {
  branches_err=0,
  number=1,
  name=2,
  other=3
  };
/*@END union-impl-branches-tag.tex*/

/*@BEGIN union-impl-branches-pd.tex */
typedef union branches_pd_u_u branches_pd_u;

union branches_pd_u_u {
  PDC_base_pd number;
  PDC_base_pd name;
  PDC_base_pd other;		/* which */
};

typedef struct branches_pd_s branches_pd;

struct branches_pd_s {
  PDC_flags_t pstate;
  PDC_errCode_t errCode;
  PDC_loc_t loc;
  PDC_uint32 nerr;
  branches_tag tag;
  branches_pd_u val;
};
/*@END union-impl-branches-pd.tex */

/*@BEGIN union-impl-branches-rep.tex */
typedef union branches_u_u branches_u;

union branches_u_u {
  PDC_int32 number;		/* number % 2 == 0 */
  PDC_string name;
  PDC_uint32 other;		/* which */
};

typedef struct branches_s branches;

struct branches_s {
  branches_tag tag;
  branches_u val;
};
/*@END union-impl-branches-rep.tex */

/*@BEGIN union-impl-branches-mask.tex*/
typedef struct branches_m_s branches_m;

struct branches_m_s {
  PDC_base_m number;
  PDC_base_m name;
};
/*@END union-impl-branches-mask.tex*/

/*@BEGIN union-impl-branches-ops.tex*/
char const *branches_tag2str (branches_tag which);

PDC_error_t branches_init (PDC_t *pdc,branches *rep);

PDC_error_t branches_pd_init (PDC_t *pdc,branches_pd *pd);

PDC_error_t branches_cleanup (PDC_t *pdc,branches *rep);

PDC_error_t branches_pd_cleanup (PDC_t *pdc,branches_pd *pd);

PDC_error_t branches_copy (PDC_t *pdc,branches *rep_dst,branches *rep_src);

PDC_error_t branches_pd_copy (PDC_t *pdc,branches_pd *pd_dst,
			      branches_pd *pd_src);

void branches_m_init (PDC_t *pdc,branches_m *mask,PDC_base_m baseMask);

PDC_error_t branches_read (PDC_t *pdc,branches_m *m,PDC_uint32 which,
			   branches_pd *pd,branches *rep);

ssize_t branches_write2buf (PDC_t *pdc,PDC_byte *buf,size_t buf_len,
			    int *buf_full,PDC_uint32 which,branches_pd *pd,
			    branches *rep);

ssize_t branches_write2io (PDC_t *pdc,Sfio_t *io,PDC_uint32 which,
			   branches_pd *pd,branches *rep);

/*@END union-impl-branches-ops.tex*/

typedef struct branches_acc_s branches_acc;
struct branches_acc_s {
  PDC_int32_acc tag;
  PDC_int32_acc number;
  PDC_string_acc name;
  PDC_uint32_acc other;		/* which */
};

PDC_error_t branches_acc_init (PDC_t *pdc,branches_acc *acc);

PDC_error_t branches_acc_reset (PDC_t *pdc,branches_acc *acc);

PDC_error_t branches_acc_cleanup (PDC_t *pdc,branches_acc *acc);

PDC_error_t branches_acc_add (PDC_t *pdc,branches_acc *acc,branches_pd *pd,branches *rep);

PDC_error_t branches_acc_report2io (PDC_t *pdc,Sfio_t *outstr,char const *prefix,char const *what,int nst,branches_acc *acc);

PDC_error_t branches_acc_report (PDC_t *pdc,char const *prefix,char const *what,int nst,branches_acc *acc);

typedef struct choice_s choice;
typedef struct choice_m_s choice_m;
typedef struct choice_pd_s choice_pd;
struct choice_m_s {
  PDC_base_m structLevel;
  PDC_base_m which;
  branches_m branch;
};
struct choice_pd_s {
  PDC_flags_t pstate;
  PDC_errCode_t errCode;
  PDC_loc_t loc;
  PDC_uint32 nerr;
  PDC_base_pd which;
  branches_pd branch;
};
struct choice_s {
  PDC_uint32 which;
  branches branch;
};

PDC_error_t choice_init (PDC_t *pdc,choice *rep);

PDC_error_t choice_pd_init (PDC_t *pdc,choice_pd *pd);

PDC_error_t choice_cleanup (PDC_t *pdc,choice *rep);

PDC_error_t choice_pd_cleanup (PDC_t *pdc,choice_pd *pd);

PDC_error_t choice_copy (PDC_t *pdc,choice *rep_dst,choice *rep_src);

PDC_error_t choice_pd_copy (PDC_t *pdc,choice_pd *pd_dst,choice_pd *pd_src);

void choice_m_init (PDC_t *pdc,choice_m *mask,PDC_base_m baseMask);

PDC_error_t choice_read (PDC_t *pdc,choice_m *m,choice_pd *pd,choice *rep);
typedef struct choice_acc_s choice_acc;
struct choice_acc_s {
  PDC_int32_acc nerr;
  PDC_uint32_acc which;
  branches_acc branch;
};

PDC_error_t choice_acc_init (PDC_t *pdc,choice_acc *acc);

PDC_error_t choice_acc_reset (PDC_t *pdc,choice_acc *acc);

PDC_error_t choice_acc_cleanup (PDC_t *pdc,choice_acc *acc);

PDC_error_t choice_acc_add (PDC_t *pdc,choice_acc *acc,choice_pd *pd,choice *rep);

PDC_error_t choice_acc_report2io (PDC_t *pdc,Sfio_t *outstr,char const *prefix,char const *what,int nst,choice_acc *acc);

PDC_error_t choice_acc_report (PDC_t *pdc,char const *prefix,char const *what,int nst,choice_acc *acc);

ssize_t choice_write2buf (PDC_t *pdc,PDC_byte *buf,size_t buf_len,int *buf_full,choice_pd *pd,choice *rep);

ssize_t choice_write2io (PDC_t *pdc,Sfio_t *io,choice_pd *pd,choice *rep);
typedef enum intOpt_tag_e intOpt_tag;
typedef union intOpt_u_u intOpt_u;
typedef struct intOpt_s intOpt;
typedef struct intOpt_m_s intOpt_m;
typedef union intOpt_pd_u_u intOpt_pd_u;
typedef struct intOpt_pd_s intOpt_pd;
enum intOpt_tag_e {
  intOpt_err=0,
  val=1,
  def=2
  };
union intOpt_pd_u_u {
  PDC_base_pd val;
  PDC_base_pd def;		/* defVal */
};
struct intOpt_pd_s {
  PDC_flags_t pstate;
  PDC_errCode_t errCode;
  PDC_loc_t loc;
  PDC_uint32 nerr;
  intOpt_tag tag;
  intOpt_pd_u val;
};
union intOpt_u_u {
  PDC_uint32 val;
  PDC_uint32 def;		/* defVal */
};
struct intOpt_s {
  intOpt_tag tag;
  intOpt_u val;
};
struct intOpt_m_s {
  PDC_base_m val;
};

char const *intOpt_tag2str (intOpt_tag which);

PDC_error_t intOpt_init (PDC_t *pdc,intOpt *rep);

PDC_error_t intOpt_pd_init (PDC_t *pdc,intOpt_pd *pd);

PDC_error_t intOpt_cleanup (PDC_t *pdc,intOpt *rep);

PDC_error_t intOpt_pd_cleanup (PDC_t *pdc,intOpt_pd *pd);

PDC_error_t intOpt_copy (PDC_t *pdc,intOpt *rep_dst,intOpt *rep_src);

PDC_error_t intOpt_pd_copy (PDC_t *pdc,intOpt_pd *pd_dst,intOpt_pd *pd_src);

void intOpt_m_init (PDC_t *pdc,intOpt_m *mask,PDC_base_m baseMask);

PDC_error_t intOpt_read (PDC_t *pdc,intOpt_m *m,PDC_uint32 defVal,intOpt_pd *pd,intOpt *rep);
typedef struct intOpt_acc_s intOpt_acc;
struct intOpt_acc_s {
  PDC_int32_acc tag;
  PDC_uint32_acc val;
  PDC_uint32_acc def;		/* defVal */
};

PDC_error_t intOpt_acc_init (PDC_t *pdc,intOpt_acc *acc);

PDC_error_t intOpt_acc_reset (PDC_t *pdc,intOpt_acc *acc);

PDC_error_t intOpt_acc_cleanup (PDC_t *pdc,intOpt_acc *acc);

PDC_error_t intOpt_acc_add (PDC_t *pdc,intOpt_acc *acc,intOpt_pd *pd,intOpt *rep);

PDC_error_t intOpt_acc_report2io (PDC_t *pdc,Sfio_t *outstr,char const *prefix,char const *what,int nst,intOpt_acc *acc);

PDC_error_t intOpt_acc_report (PDC_t *pdc,char const *prefix,char const *what,int nst,intOpt_acc *acc);

ssize_t intOpt_write2buf (PDC_t *pdc,PDC_byte *buf,size_t buf_len,int *buf_full,PDC_uint32 defVal,intOpt_pd *pd,intOpt *rep);

ssize_t intOpt_write2io (PDC_t *pdc,Sfio_t *io,PDC_uint32 defVal,intOpt_pd *pd,intOpt *rep);
typedef struct data_s data;
typedef struct data_m_s data_m;
typedef struct data_pd_s data_pd;
struct data_m_s {
  PDC_base_m structLevel;
  intOpt_m field1;
  intOpt_m field2;
  intOpt_m field3;
};
struct data_pd_s {
  PDC_flags_t pstate;
  PDC_errCode_t errCode;
  PDC_loc_t loc;
  PDC_uint32 nerr;
  intOpt_pd field1;
  intOpt_pd field2;
  intOpt_pd field3;
};
struct data_s {
  intOpt field1;
  intOpt field2;
  intOpt field3;
};

PDC_error_t data_init (PDC_t *pdc,data *rep);

PDC_error_t data_pd_init (PDC_t *pdc,data_pd *pd);

PDC_error_t data_cleanup (PDC_t *pdc,data *rep);

PDC_error_t data_pd_cleanup (PDC_t *pdc,data_pd *pd);

PDC_error_t data_copy (PDC_t *pdc,data *rep_dst,data *rep_src);

PDC_error_t data_pd_copy (PDC_t *pdc,data_pd *pd_dst,data_pd *pd_src);

void data_m_init (PDC_t *pdc,data_m *mask,PDC_base_m baseMask);

PDC_error_t data_read (PDC_t *pdc,data_m *m,data_pd *pd,data *rep);
typedef struct data_acc_s data_acc;
struct data_acc_s {
  PDC_int32_acc nerr;
  intOpt_acc field1;
  intOpt_acc field2;
  intOpt_acc field3;
};

PDC_error_t data_acc_init (PDC_t *pdc,data_acc *acc);

PDC_error_t data_acc_reset (PDC_t *pdc,data_acc *acc);

PDC_error_t data_acc_cleanup (PDC_t *pdc,data_acc *acc);

PDC_error_t data_acc_add (PDC_t *pdc,data_acc *acc,data_pd *pd,data *rep);

PDC_error_t data_acc_report2io (PDC_t *pdc,Sfio_t *outstr,char const *prefix,char const *what,int nst,data_acc *acc);

PDC_error_t data_acc_report (PDC_t *pdc,char const *prefix,char const *what,int nst,data_acc *acc);

ssize_t data_write2buf (PDC_t *pdc,PDC_byte *buf,size_t buf_len,int *buf_full,data_pd *pd,data *rep);

ssize_t data_write2io (PDC_t *pdc,Sfio_t *io,data_pd *pd,data *rep);

#endif /*  __PUNION__H__  */
