/*@FILE @LEFT typedef.TypedefRep.tex  typedef.TypedefCSM.tex typedef.TypedefPD.tex typedef.TypedefOps.tex */
#ifndef __PTYPEDEF__H__
#define __PTYPEDEF__H__
#include "pads.h"

/*@BEGIN typedef.TypedefRep.tex */
typedef Puint32 bid_t;
/*@END typedef.TypedefRep.tex */

/*@BEGIN typedef.TypedefPD.tex */
typedef Pbase_pd bid_t_pd;
/*@END typedef.TypedefPD.tex */

/*@BEGIN typedef.TypedefCSM.tex */
typedef struct bid_t_m_s bid_t_m;
struct bid_t_m_s {
  Pbase_m base;		/* Base mask */
  Pbase_m user;		/* Typedef mask */
};
/*@END typedef.TypedefCSM.tex */

/*@BEGIN typedef.TypedefOps.tex */
Perror_t bid_t_init (P_t *pads,bid_t *rep);

Perror_t bid_t_pd_init (P_t *pads,bid_t_pd *pd);

Perror_t bid_t_cleanup (P_t *pads,bid_t *rep);

Perror_t bid_t_pd_cleanup (P_t *pads,bid_t_pd *pd);

Perror_t bid_t_copy (P_t *pads,bid_t *rep_dst,bid_t *rep_src);

Perror_t bid_t_pd_copy (P_t *pads,bid_t_pd *pd_dst,bid_t_pd *pd_src);

void bid_t_m_init (P_t *pads,bid_t_m *mask,Pbase_m baseMask);

Perror_t bid_t_read (P_t *pads,bid_t_m *m,bid_t_pd *pd,bid_t *rep);

int bid_t_verify (bid_t *rep);

int bid_t_genPD (P_t *pads, bid_t *rep, bid_t_pd *pd);
/*@END typedef.TypedefOps.tex */

typedef Puint32_acc bid_t_acc;

Perror_t bid_t_acc_init (P_t *pads,bid_t_acc *acc);

Perror_t bid_t_acc_reset (P_t *pads,bid_t_acc *acc);

Perror_t bid_t_acc_cleanup (P_t *pads,bid_t_acc *acc);

Perror_t bid_t_acc_add (P_t *pads,bid_t_acc *acc,bid_t_pd *pd,bid_t *rep);

Perror_t bid_t_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,bid_t_acc *acc);

Perror_t bid_t_acc_report (P_t *pads,char const *prefix,char const *what,int nst,bid_t_acc *acc);

ssize_t bid_t_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,bid_t_pd *pd,bid_t *rep);

ssize_t bid_t_write2io (P_t *pads,Sfio_t *io,bid_t_pd *pd,bid_t *rep);

ssize_t bid_t_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,bid_t_pd *pd,bid_t *rep,char const *tag,int indent);

ssize_t bid_t_write_xml_2io (P_t *pads,Sfio_t *io,bid_t_pd *pd,bid_t *rep,char const *tag,int indent);
typedef Puint64 pn_t;
typedef struct pn_t_m_s pn_t_m;
typedef Pbase_pd pn_t_pd;
struct pn_t_m_s {
  Pbase_m base;		/* Base mask */
  Pbase_m user;		/* Typedef mask */
};

Perror_t pn_t_init (P_t *pads,pn_t *rep);

Perror_t pn_t_pd_init (P_t *pads,pn_t_pd *pd);

Perror_t pn_t_cleanup (P_t *pads,pn_t *rep);

Perror_t pn_t_pd_cleanup (P_t *pads,pn_t_pd *pd);

Perror_t pn_t_copy (P_t *pads,pn_t *rep_dst,pn_t *rep_src);

Perror_t pn_t_pd_copy (P_t *pads,pn_t_pd *pd_dst,pn_t_pd *pd_src);

void pn_t_m_init (P_t *pads,pn_t_m *mask,Pbase_m baseMask);

Perror_t pn_t_read (P_t *pads,pn_t_m *m,Puint64 lo,Puint64 hi,pn_t_pd *pd,pn_t *rep);

int pn_t_verify (pn_t *rep,Puint64 lo,Puint64 hi);
typedef Puint64_acc pn_t_acc;

Perror_t pn_t_acc_init (P_t *pads,pn_t_acc *acc);

Perror_t pn_t_acc_reset (P_t *pads,pn_t_acc *acc);

Perror_t pn_t_acc_cleanup (P_t *pads,pn_t_acc *acc);

Perror_t pn_t_acc_add (P_t *pads,pn_t_acc *acc,pn_t_pd *pd,pn_t *rep);

Perror_t pn_t_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,pn_t_acc *acc);

Perror_t pn_t_acc_report (P_t *pads,char const *prefix,char const *what,int nst,pn_t_acc *acc);

ssize_t pn_t_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,Puint64 lo,Puint64 hi,pn_t_pd *pd,pn_t *rep);

ssize_t pn_t_write2io (P_t *pads,Sfio_t *io,Puint64 lo,Puint64 hi,pn_t_pd *pd,pn_t *rep);

ssize_t pn_t_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,Puint64 lo,Puint64 hi,pn_t_pd *pd,pn_t *rep,char const *tag,int indent);

ssize_t pn_t_write_xml_2io (P_t *pads,Sfio_t *io,Puint64 lo,Puint64 hi,pn_t_pd *pd,pn_t *rep,char const *tag,int indent);

void P_lib_init ();

#endif /*  __PTYPEDEF__H__  */
