/*@FILE @LEFT enumRep.tex enumPD.tex enumMask.tex enumOps.tex*/
#ifndef __ENUM2__H__
#define __ENUM2__H__
#include "pads.h"
/*@BEGIN enumRep.tex */
typedef enum orderStates_e orderStates;
enum orderStates_e {
  S_init=0,
  S_lec=2,
  S_care=3,
  S_my_for=10,
  S_my_if=11,
  S_tpv=5
  };
/*@END enumRep.tex */

/*@BEGIN enumPD.tex */
typedef Pbase_m orderStates_m;
/*@END enumPD.tex */
/*@BEGIN enumMask.tex */
typedef Pbase_pd orderStates_pd;
/*@END enumMask.tex */
/*@BEGIN enumOps.tex*/
char const *orderStates2str (orderStates which);

Perror_t orderStates_init (P_t *pads,orderStates *rep);

Perror_t orderStates_pd_init (P_t *pads,orderStates_pd *pd);

Perror_t orderStates_cleanup (P_t *pads,orderStates *rep);

Perror_t orderStates_pd_cleanup (P_t *pads,orderStates_pd *pd);

Perror_t orderStates_copy (P_t *pads,orderStates *rep_dst,orderStates *rep_src);

Perror_t orderStates_pd_copy (P_t *pads,orderStates_pd *pd_dst,orderStates_pd *pd_src);

void orderStates_m_init (P_t *pads,orderStates_m *mask,Pbase_m baseMask);

Perror_t orderStates_read (P_t *pads,orderStates_m *m,orderStates_pd *pd,orderStates *rep);

int is_orderStates (orderStates *rep);
/*@END enumOps.tex*/

typedef Pint32_acc orderStates_acc;

Perror_t orderStates_acc_init (P_t *pads,orderStates_acc *acc);

Perror_t orderStates_acc_reset (P_t *pads,orderStates_acc *acc);

Perror_t orderStates_acc_cleanup (P_t *pads,orderStates_acc *acc);

Perror_t orderStates_acc_add (P_t *pads,orderStates_acc *acc,orderStates_pd *pd,orderStates *rep);

Perror_t orderStates_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,orderStates_acc *acc);

Perror_t orderStates_acc_report (P_t *pads,char const *prefix,char const *what,int nst,orderStates_acc *acc);

ssize_t orderStates_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,orderStates_pd *pd,orderStates *rep);

ssize_t orderStates_write2io (P_t *pads,Sfio_t *io,orderStates_pd *pd,orderStates *rep);

ssize_t orderStates_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,orderStates_pd *pd,orderStates *rep,char const *tag,int indent);

ssize_t orderStates_write_xml_2io (P_t *pads,Sfio_t *io,orderStates_pd *pd,orderStates *rep,char const *tag,int indent);

ssize_t orderStates_fmt2buf_final (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,orderStates_m *m,orderStates_pd *pd,orderStates *rep);

ssize_t orderStates_fmt2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,orderStates_m *m,orderStates_pd *pd,orderStates *rep);

ssize_t orderStates_fmt2io (P_t *pads,Sfio_t *io,int *requestedOut,char const *delims,orderStates_m *m,orderStates_pd *pd,orderStates *rep);

void P_lib_init ();

#endif /*  __ENUM2__H__  */
