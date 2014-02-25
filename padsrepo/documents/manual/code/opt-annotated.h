/*@FILE @LEFT opt.TyOptTags.tex opt.TyOptRep.tex  opt.TyOptCSM.tex opt.TyOptPD.tex opt.TyOptOps.tex */
#ifndef __OPT__H__
#define __OPT__H__
#include "pads.h"

typedef Puint32 Ty;
typedef struct Ty_m_s Ty_m;
typedef Pbase_pd Ty_pd;
struct Ty_m_s {
  Pbase_m base;		/* Base mask */
  Pbase_m user;		/* Typedef mask */
};

Perror_t Ty_init (P_t *pads,Ty *rep);

Perror_t Ty_pd_init (P_t *pads,Ty_pd *pd);

Perror_t Ty_cleanup (P_t *pads,Ty *rep);

Perror_t Ty_pd_cleanup (P_t *pads,Ty_pd *pd);

Perror_t Ty_copy (P_t *pads,Ty *rep_dst,Ty *rep_src);

Perror_t Ty_pd_copy (P_t *pads,Ty_pd *pd_dst,Ty_pd *pd_src);

void Ty_m_init (P_t *pads,Ty_m *mask,Pbase_m baseMask);

Perror_t Ty_read (P_t *pads,Ty_m *m,Ty_pd *pd,Ty *rep);

int Ty_verify (Ty *rep);
typedef Puint32_acc Ty_acc;

Perror_t Ty_acc_init (P_t *pads,Ty_acc *acc);

Perror_t Ty_acc_reset (P_t *pads,Ty_acc *acc);

Perror_t Ty_acc_cleanup (P_t *pads,Ty_acc *acc);

Perror_t Ty_acc_add (P_t *pads,Ty_acc *acc,Ty_pd *pd,Ty *rep);

Perror_t Ty_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,Ty_acc *acc);

Perror_t Ty_acc_report (P_t *pads,char const *prefix,char const *what,int nst,Ty_acc *acc);

ssize_t Ty_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,Ty_pd *pd,Ty *rep);

ssize_t Ty_write2io (P_t *pads,Sfio_t *io,Ty_pd *pd,Ty *rep);

ssize_t Ty_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,Ty_pd *pd,Ty *rep,char const *tag,int indent);

ssize_t Ty_write_xml_2io (P_t *pads,Sfio_t *io,Ty_pd *pd,Ty *rep,char const *tag,int indent);



/*@BEGIN opt.TyOptTags.tex */
typedef enum OptTy_tag_e OptTy_tag;

enum OptTy_tag_e {
  OptTy_err=0,
  some_OptTy=1,
  none_OptTy=2
  };
/*@END opt.TyOptTags.tex */

/*@BEGIN opt.TyOptPD.tex */
typedef union OptTy_pd_u_u OptTy_pd_u;
union OptTy_pd_u_u {
  Ty_pd some_OptTy;
  Pbase_pd none_OptTy;		/* value was not present. none_OptTy = 0 */
};

typedef struct OptTy_pd_s OptTy_pd;
struct OptTy_pd_s {
  Pflags_t pstate;
  Puint32 nerr;
  PerrCode_t errCode;
  Ploc_t loc;
  OptTy_tag tag;
  OptTy_pd_u val;
};
/*@END opt.TyOptPD.tex */

/*@BEGIN opt.TyOptRep.tex */
typedef union OptTy_u_u OptTy_u;
union OptTy_u_u {
  Ty some_OptTy;		/* value is present */
};

typedef struct OptTy_s OptTy;
struct OptTy_s {
  OptTy_tag tag;
  OptTy_u val;
};
/*@END opt.TyOptRep.tex */

/*@BEGIN opt.TyOptCSM.tex */
typedef struct OptTy_m_s OptTy_m;

struct OptTy_m_s {
  Pbase_m unionLevel;
  Ty_m some_OptTy;		/* nested constraints */
};
/*@END opt.TyOptCSM.tex */

/*@BEGIN opt.TyOptOps.tex */
char const *OptTy_tag2str (OptTy_tag which);

Perror_t OptTy_init (P_t *pads,OptTy *rep);

Perror_t OptTy_pd_init (P_t *pads,OptTy_pd *pd);

Perror_t OptTy_cleanup (P_t *pads,OptTy *rep);

Perror_t OptTy_pd_cleanup (P_t *pads,OptTy_pd *pd);

Perror_t OptTy_copy (P_t *pads,OptTy *rep_dst,OptTy *rep_src);

Perror_t OptTy_pd_copy (P_t *pads,OptTy_pd *pd_dst,OptTy_pd *pd_src);

void OptTy_m_init (P_t *pads,OptTy_m *mask,Pbase_m baseMask);

Perror_t OptTy_read (P_t *pads,OptTy_m *m,OptTy_pd *pd,OptTy *rep);

ssize_t OptTy_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,
			 int *buf_full,OptTy_pd *pd,OptTy *rep);

ssize_t OptTy_write2io (P_t *pads,Sfio_t *io,OptTy_pd *pd,OptTy *rep);

int OptTy_verify (OptTy *rep);

int OptTy_genPD (P_t *pads, OptTy *rep, OptTy_pd *pd);
/*@END opt.TyOptOps.tex */
ssize_t OptTy_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,OptTy_pd *pd,OptTy *rep,char const *tag,int indent);

ssize_t OptTy_write_xml_2io (P_t *pads,Sfio_t *io,OptTy_pd *pd,OptTy *rep,char const *tag,int indent);




typedef struct OptTy_acc_s OptTy_acc;
struct OptTy_acc_s {
  Pint32_acc tag;
  Ty_acc some_OptTy;
};

Perror_t OptTy_acc_init (P_t *pads,OptTy_acc *acc);

Perror_t OptTy_acc_reset (P_t *pads,OptTy_acc *acc);

Perror_t OptTy_acc_cleanup (P_t *pads,OptTy_acc *acc);

Perror_t OptTy_acc_add (P_t *pads,OptTy_acc *acc,OptTy_pd *pd,OptTy *rep);

Perror_t OptTy_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,OptTy_acc *acc);

Perror_t OptTy_acc_report (P_t *pads,char const *prefix,char const *what,int nst,OptTy_acc *acc);

typedef enum OptTyTrans_tag_e OptTyTrans_tag;
typedef union OptTyTrans_u_u OptTyTrans_u;
typedef struct OptTyTrans_s OptTyTrans;
typedef struct OptTyTrans_m_s OptTyTrans_m;
typedef union OptTyTrans_pd_u_u OptTyTrans_pd_u;
typedef struct OptTyTrans_pd_s OptTyTrans_pd;
enum OptTyTrans_tag_e {
  OptTyTrans_err=0,
  OptTyTrans_some_OptTyTrans=1,
  OptTyTrans_none_OptTyTrans=2
  };
union OptTyTrans_pd_u_u {
  Ty_pd some_OptTyTrans;
  Pbase_pd none_OptTyTrans;		/* none_OptTyTrans = 0 */
};
struct OptTyTrans_pd_s {
  Pflags_t pstate;
  Puint32 nerr;
  PerrCode_t errCode;
  Ploc_t loc;
  OptTyTrans_tag tag;
  OptTyTrans_pd_u val;
};
union OptTyTrans_u_u {
  Ty some_OptTyTrans;
  Puint32 none_OptTyTrans;		/* none_OptTyTrans = 0 */
};
struct OptTyTrans_s {
  OptTyTrans_tag tag;
  OptTyTrans_u val;
};
struct OptTyTrans_m_s {
  Pbase_m unionLevel;
  Ty_m some_OptTyTrans;		/* nested constraints */
};

ssize_t OptTyTrans_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,OptTyTrans_pd *pd,OptTyTrans *rep);

ssize_t OptTyTrans_write2io (P_t *pads,Sfio_t *io,OptTyTrans_pd *pd,OptTyTrans *rep);

ssize_t OptTyTrans_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,OptTyTrans_pd *pd,OptTyTrans *rep,char const *tag,int indent);

ssize_t OptTyTrans_write_xml_2io (P_t *pads,Sfio_t *io,OptTyTrans_pd *pd,OptTyTrans *rep,char const *tag,int indent);

char const *OptTyTrans_tag2str (OptTyTrans_tag which);

Perror_t OptTyTrans_init (P_t *pads,OptTyTrans *rep);

Perror_t OptTyTrans_pd_init (P_t *pads,OptTyTrans_pd *pd);

Perror_t OptTyTrans_cleanup (P_t *pads,OptTyTrans *rep);

Perror_t OptTyTrans_pd_cleanup (P_t *pads,OptTyTrans_pd *pd);

Perror_t OptTyTrans_copy (P_t *pads,OptTyTrans *rep_dst,OptTyTrans *rep_src);

Perror_t OptTyTrans_pd_copy (P_t *pads,OptTyTrans_pd *pd_dst,OptTyTrans_pd *pd_src);

void OptTyTrans_m_init (P_t *pads,OptTyTrans_m *mask,Pbase_m baseMask);

Perror_t OptTyTrans_read (P_t *pads,OptTyTrans_m *m,OptTyTrans_pd *pd,OptTyTrans *rep);

int OptTyTrans_verify (OptTyTrans *rep);
typedef struct OptTyTrans_acc_s OptTyTrans_acc;
struct OptTyTrans_acc_s {
  Pint32_acc tag;
  Ty_acc some_OptTyTrans;
  Puint32_acc none_OptTyTrans;		/* none_OptTyTrans = 0 */
};

Perror_t OptTyTrans_acc_init (P_t *pads,OptTyTrans_acc *acc);

Perror_t OptTyTrans_acc_reset (P_t *pads,OptTyTrans_acc *acc);

Perror_t OptTyTrans_acc_cleanup (P_t *pads,OptTyTrans_acc *acc);

Perror_t OptTyTrans_acc_add (P_t *pads,OptTyTrans_acc *acc,OptTyTrans_pd *pd,OptTyTrans *rep);

Perror_t OptTyTrans_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,OptTyTrans_acc *acc);

Perror_t OptTyTrans_acc_report (P_t *pads,char const *prefix,char const *what,int nst,OptTyTrans_acc *acc);

void P_lib_init ();

#endif /*  __OPT__H__  */
