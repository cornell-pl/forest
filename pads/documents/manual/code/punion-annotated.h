/*@FILE @LEFT union-impl-branches-tag.tex union-impl-branches-rep.tex union-impl-branches-pd.tex union-impl-branches-mask.tex union-impl-branches-ops.tex*/
#ifndef __PUNION__H__
#define __PUNION__H__
#include "pads.h"
typedef union branches_pd_u_u branches_pd_u;
typedef struct branches_pd_s branches_pd;

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
  Pbase_pd number;
  Pbase_pd name;
  Pbase_pd other;		/* other = which */
};

typedef struct branches_pd_s branches_pd;

struct branches_pd_s {
  Pflags_t pstate;
  Puint32 nerr;
  PerrCode_t errCode;
  Ploc_t loc;
  branches_tag tag;
  branches_pd_u val;
};
/*@END union-impl-branches-pd.tex */

/*@BEGIN union-impl-branches-rep.tex */
typedef union branches_u_u branches_u;

union branches_u_u {
  Pint32 number;		/* number % 2 == 0 */
  Pstring name;
  Puint32 other;		/* other = which */
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
  Pbase_m unionLevel;
  Pbase_m number;		/* nested constraints */
  Pbase_m number_con;		/* union constraints */
  Pbase_m name;		/* nested constraints */
};
/*@END union-impl-branches-mask.tex*/

/*@BEGIN union-impl-branches-ops.tex*/
char const *branches_tag2str (branches_tag which);

Perror_t branches_init (P_t *pads,branches *rep);

Perror_t branches_pd_init (P_t *pads,branches_pd *pd);

Perror_t branches_cleanup (P_t *pads,branches *rep);

Perror_t branches_pd_cleanup (P_t *pads,branches_pd *pd);

Perror_t branches_copy (P_t *pads,branches *rep_dst,branches *rep_src);

Perror_t branches_pd_copy (P_t *pads,branches_pd *pd_dst,
			   branches_pd *pd_src);

void branches_m_init (P_t *pads,branches_m *mask,Pbase_m baseMask);

Perror_t branches_read (P_t *pads,branches_m *m,Puint32 which,
			branches_pd *pd,branches *rep);

ssize_t branches_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,
			    Puint32 which,branches_pd *pd,branches *rep);

ssize_t branches_write2io (P_t *pads,Sfio_t *io,Puint32 which,
			   branches_pd *pd,branches *rep);

int branches_verify (branches *rep,Puint32 which);
/*@END union-impl-branches-ops.tex*/

typedef struct branches_acc_s branches_acc;
struct branches_acc_s {
  Pint32_acc tag;
  Pint32_acc number;
  Pstring_acc name;
  Puint32_acc other;		/* other = which */
};

Perror_t branches_acc_init (P_t *pads,branches_acc *acc);

Perror_t branches_acc_reset (P_t *pads,branches_acc *acc);

Perror_t branches_acc_cleanup (P_t *pads,branches_acc *acc);

Perror_t branches_acc_add (P_t *pads,branches_acc *acc,branches_pd *pd,branches *rep);

Perror_t branches_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,branches_acc *acc);

Perror_t branches_acc_report (P_t *pads,char const *prefix,char const *what,int nst,branches_acc *acc);

ssize_t branches_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,Puint32 which,branches_pd *pd,branches *rep);

ssize_t branches_write2io (P_t *pads,Sfio_t *io,Puint32 which,branches_pd *pd,branches *rep);

ssize_t branches_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,Puint32 which,branches_pd *pd,branches *rep,char const *tag,int indent);

ssize_t branches_write_xml_2io (P_t *pads,Sfio_t *io,Puint32 which,branches_pd *pd,branches *rep,char const *tag,int indent);
typedef struct choice_s choice;
typedef struct choice_m_s choice_m;
typedef struct choice_pd_s choice_pd;
struct choice_m_s {
  Pbase_m structLevel;
  Pbase_m which;		/* nested constraints */
  branches_m branch;		/* nested constraints */
};
struct choice_pd_s {
  Pflags_t pstate;
  Puint32 nerr;
  PerrCode_t errCode;
  Ploc_t loc;
  Pbase_pd which;
  branches_pd branch;
};
struct choice_s {
  Puint32 which;
  branches branch;
};

Perror_t choice_init (P_t *pads,choice *rep);

Perror_t choice_pd_init (P_t *pads,choice_pd *pd);

Perror_t choice_cleanup (P_t *pads,choice *rep);

Perror_t choice_pd_cleanup (P_t *pads,choice_pd *pd);

Perror_t choice_copy (P_t *pads,choice *rep_dst,choice *rep_src);

Perror_t choice_pd_copy (P_t *pads,choice_pd *pd_dst,choice_pd *pd_src);

void choice_m_init (P_t *pads,choice_m *mask,Pbase_m baseMask);

Perror_t choice_read (P_t *pads,choice_m *m,choice_pd *pd,choice *rep);

int choice_verify (choice *rep);
typedef struct choice_acc_s choice_acc;
struct choice_acc_s {
  Puint32_acc nerr;
  Puint32_acc which;
  branches_acc branch;
};

Perror_t choice_acc_init (P_t *pads,choice_acc *acc);

Perror_t choice_acc_reset (P_t *pads,choice_acc *acc);

Perror_t choice_acc_cleanup (P_t *pads,choice_acc *acc);

Perror_t choice_acc_add (P_t *pads,choice_acc *acc,choice_pd *pd,choice *rep);

Perror_t choice_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,choice_acc *acc);

Perror_t choice_acc_report (P_t *pads,char const *prefix,char const *what,int nst,choice_acc *acc);

ssize_t choice_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,choice_pd *pd,choice *rep);

ssize_t choice_write2io (P_t *pads,Sfio_t *io,choice_pd *pd,choice *rep);

ssize_t choice_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,choice_pd *pd,choice *rep,char const *tag,int indent);

ssize_t choice_write_xml_2io (P_t *pads,Sfio_t *io,choice_pd *pd,choice *rep,char const *tag,int indent);
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
  Pbase_pd val;
  Pbase_pd def;		/* def = defVal */
};
struct intOpt_pd_s {
  Pflags_t pstate;
  Puint32 nerr;
  PerrCode_t errCode;
  Ploc_t loc;
  intOpt_tag tag;
  intOpt_pd_u val;
};
union intOpt_u_u {
  Puint32 val;
  Puint32 def;		/* def = defVal */
};
struct intOpt_s {
  intOpt_tag tag;
  intOpt_u val;
};
struct intOpt_m_s {
  Pbase_m unionLevel;
  Pbase_m val;		/* nested constraints */
};

char const *intOpt_tag2str (intOpt_tag which);

Perror_t intOpt_init (P_t *pads,intOpt *rep);

Perror_t intOpt_pd_init (P_t *pads,intOpt_pd *pd);

Perror_t intOpt_cleanup (P_t *pads,intOpt *rep);

Perror_t intOpt_pd_cleanup (P_t *pads,intOpt_pd *pd);

Perror_t intOpt_copy (P_t *pads,intOpt *rep_dst,intOpt *rep_src);

Perror_t intOpt_pd_copy (P_t *pads,intOpt_pd *pd_dst,intOpt_pd *pd_src);

void intOpt_m_init (P_t *pads,intOpt_m *mask,Pbase_m baseMask);

Perror_t intOpt_read (P_t *pads,intOpt_m *m,Puint32 defVal,intOpt_pd *pd,intOpt *rep);

int intOpt_verify (intOpt *rep,Puint32 defVal);
typedef struct intOpt_acc_s intOpt_acc;
struct intOpt_acc_s {
  Pint32_acc tag;
  Puint32_acc val;
  Puint32_acc def;		/* def = defVal */
};

Perror_t intOpt_acc_init (P_t *pads,intOpt_acc *acc);

Perror_t intOpt_acc_reset (P_t *pads,intOpt_acc *acc);

Perror_t intOpt_acc_cleanup (P_t *pads,intOpt_acc *acc);

Perror_t intOpt_acc_add (P_t *pads,intOpt_acc *acc,intOpt_pd *pd,intOpt *rep);

Perror_t intOpt_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,intOpt_acc *acc);

Perror_t intOpt_acc_report (P_t *pads,char const *prefix,char const *what,int nst,intOpt_acc *acc);

ssize_t intOpt_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,Puint32 defVal,intOpt_pd *pd,intOpt *rep);

ssize_t intOpt_write2io (P_t *pads,Sfio_t *io,Puint32 defVal,intOpt_pd *pd,intOpt *rep);

ssize_t intOpt_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,Puint32 defVal,intOpt_pd *pd,intOpt *rep,char const *tag,int indent);

ssize_t intOpt_write_xml_2io (P_t *pads,Sfio_t *io,Puint32 defVal,intOpt_pd *pd,intOpt *rep,char const *tag,int indent);
typedef struct data_s data;
typedef struct data_m_s data_m;
typedef struct data_pd_s data_pd;
struct data_m_s {
  Pbase_m structLevel;
  intOpt_m field1;		/* nested constraints */
  intOpt_m field2;		/* nested constraints */
  intOpt_m field3;		/* nested constraints */
};
struct data_pd_s {
  Pflags_t pstate;
  Puint32 nerr;
  PerrCode_t errCode;
  Ploc_t loc;
  intOpt_pd field1;
  intOpt_pd field2;
  intOpt_pd field3;
};
struct data_s {
  intOpt field1;
  intOpt field2;
  intOpt field3;
};

Perror_t data_init (P_t *pads,data *rep);

Perror_t data_pd_init (P_t *pads,data_pd *pd);

Perror_t data_cleanup (P_t *pads,data *rep);

Perror_t data_pd_cleanup (P_t *pads,data_pd *pd);

Perror_t data_copy (P_t *pads,data *rep_dst,data *rep_src);

Perror_t data_pd_copy (P_t *pads,data_pd *pd_dst,data_pd *pd_src);

void data_m_init (P_t *pads,data_m *mask,Pbase_m baseMask);

Perror_t data_read (P_t *pads,data_m *m,data_pd *pd,data *rep);

int data_verify (data *rep);
typedef struct data_acc_s data_acc;
struct data_acc_s {
  Puint32_acc nerr;
  intOpt_acc field1;
  intOpt_acc field2;
  intOpt_acc field3;
};

Perror_t data_acc_init (P_t *pads,data_acc *acc);

Perror_t data_acc_reset (P_t *pads,data_acc *acc);

Perror_t data_acc_cleanup (P_t *pads,data_acc *acc);

Perror_t data_acc_add (P_t *pads,data_acc *acc,data_pd *pd,data *rep);

Perror_t data_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,data_acc *acc);

Perror_t data_acc_report (P_t *pads,char const *prefix,char const *what,int nst,data_acc *acc);

ssize_t data_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,data_pd *pd,data *rep);

ssize_t data_write2io (P_t *pads,Sfio_t *io,data_pd *pd,data *rep);

ssize_t data_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,data_pd *pd,data *rep,char const *tag,int indent);

ssize_t data_write_xml_2io (P_t *pads,Sfio_t *io,data_pd *pd,data *rep,char const *tag,int indent);

void P_lib_init ();

#endif /*  __PUNION__H__  */
