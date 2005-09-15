#ifndef __DIBBLER_NEW__H__
#define __DIBBLER_NEW__H__
#include "pads.h"
#include "pglx-internal.h"
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

int pn_t_verify (pn_t *rep);
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

PDCI_node_t *pn_t_node_new (PDCI_node_t *parent,char const *name,void *m,void *pd,void *rep,char const *kind,char const *whatfn);

PDCI_node_t *pn_t_cachedNode_init (PDCI_node_t *self);

PDCI_node_t *pn_t_node_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

PDCI_node_t *pn_t_node_kthChildNamed (PDCI_node_t *self,PDCI_childIndex_t idx,char const *name);

PDCI_node_t *pn_t_cachedNode_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

PDCI_node_t *pn_t_sndNode_init (PDCI_node_t *self,PDCI_manager_t *manager,PDCI_childIndex_t ancestor_idx,PDCI_gen_t gen,PDCI_childIndex_t idx);

PDCI_node_t *pn_t_sndNode_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

Perror_t pn_t_node_pathWalk (P_t *pads,pn_t_m *m,pn_t_pd *pd,pn_t *rep,PDCI_path_t path,void **m_out,void **pd_out,void **rep_out);
extern PDCI_vtable_t const pn_t_node_vtable;
extern PDCI_vtable_t const pn_t_cachedNode_vtable;
extern PDCI_vtable_t const pn_t_sndNode_vtable;
typedef Pchar zipSep_t;
typedef struct zipSep_t_m_s zipSep_t_m;
typedef Pbase_pd zipSep_t_pd;
struct zipSep_t_m_s {
  Pbase_m base;		/* Base mask */
  Pbase_m compoundLevel;		/* Typedef mask */
};

Perror_t zipSep_t_init (P_t *pads,zipSep_t *rep);

Perror_t zipSep_t_pd_init (P_t *pads,zipSep_t_pd *pd);

Perror_t zipSep_t_cleanup (P_t *pads,zipSep_t *rep);

Perror_t zipSep_t_pd_cleanup (P_t *pads,zipSep_t_pd *pd);

Perror_t zipSep_t_copy (P_t *pads,zipSep_t *rep_dst,zipSep_t *rep_src);

Perror_t zipSep_t_pd_copy (P_t *pads,zipSep_t_pd *pd_dst,zipSep_t_pd *pd_src);

void zipSep_t_m_init (P_t *pads,zipSep_t_m *mask,Pbase_m baseMask);

Perror_t zipSep_t_read (P_t *pads,zipSep_t_m *m,zipSep_t_pd *pd,zipSep_t *rep);

int zipSep_t_verify (zipSep_t *rep);
typedef Pchar_acc zipSep_t_acc;

Perror_t zipSep_t_acc_init (P_t *pads,zipSep_t_acc *acc);

Perror_t zipSep_t_acc_reset (P_t *pads,zipSep_t_acc *acc);

Perror_t zipSep_t_acc_cleanup (P_t *pads,zipSep_t_acc *acc);

Perror_t zipSep_t_acc_add (P_t *pads,zipSep_t_acc *acc,zipSep_t_pd *pd,zipSep_t *rep);

Perror_t zipSep_t_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,zipSep_t_acc *acc);

Perror_t zipSep_t_acc_report (P_t *pads,char const *prefix,char const *what,int nst,zipSep_t_acc *acc);

ssize_t zipSep_t_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,zipSep_t_pd *pd,zipSep_t *rep);

ssize_t zipSep_t_write2io (P_t *pads,Sfio_t *io,zipSep_t_pd *pd,zipSep_t *rep);

ssize_t zipSep_t_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,zipSep_t_pd *pd,zipSep_t *rep,char const *tag,int indent);

ssize_t zipSep_t_write_xml_2io (P_t *pads,Sfio_t *io,zipSep_t_pd *pd,zipSep_t *rep,char const *tag,int indent);

ssize_t zipSep_t_fmt2buf_final (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,zipSep_t_m *m,zipSep_t_pd *pd,zipSep_t *rep);

ssize_t zipSep_t_fmt2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,zipSep_t_m *m,zipSep_t_pd *pd,zipSep_t *rep);

ssize_t zipSep_t_fmt2io (P_t *pads,Sfio_t *io,int *requestedOut,char const *delims,zipSep_t_m *m,zipSep_t_pd *pd,zipSep_t *rep);

PDCI_node_t *zipSep_t_node_new (PDCI_node_t *parent,char const *name,void *m,void *pd,void *rep,char const *kind,char const *whatfn);

PDCI_node_t *zipSep_t_cachedNode_init (PDCI_node_t *self);

PDCI_node_t *zipSep_t_node_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

PDCI_node_t *zipSep_t_node_kthChildNamed (PDCI_node_t *self,PDCI_childIndex_t idx,char const *name);

PDCI_node_t *zipSep_t_cachedNode_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

PDCI_node_t *zipSep_t_sndNode_init (PDCI_node_t *self,PDCI_manager_t *manager,PDCI_childIndex_t ancestor_idx,PDCI_gen_t gen,PDCI_childIndex_t idx);

PDCI_node_t *zipSep_t_sndNode_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

Perror_t zipSep_t_node_pathWalk (P_t *pads,zipSep_t_m *m,zipSep_t_pd *pd,zipSep_t *rep,PDCI_path_t path,void **m_out,void **pd_out,void **rep_out);
extern PDCI_vtable_t const zipSep_t_node_vtable;
extern PDCI_vtable_t const zipSep_t_cachedNode_vtable;
extern PDCI_vtable_t const zipSep_t_sndNode_vtable;
typedef struct extended_zip_t_s extended_zip_t;
typedef struct extended_zip_t_m_s extended_zip_t_m;
typedef struct extended_zip_t_pd_s extended_zip_t_pd;
struct extended_zip_t_m_s {
  Pbase_m compoundLevel;
  Pbase_m zip;		/* nested constraints */
  zipSep_t_m sep;		/* nested constraints */
  Pbase_m suffix;		/* nested constraints */
};
struct extended_zip_t_pd_s {
  Pflags_t pstate;
  Puint32 nerr;
  PerrCode_t errCode;
  Ploc_t loc;
  PDCI_id_t _id_;		/* Identifier tag for Galax */
  Pbase_pd zip;
  zipSep_t_pd sep;
  Pbase_pd suffix;
};
struct extended_zip_t_s {
  Puint32 zip;
  zipSep_t sep;
  Puint32 suffix;
};

Perror_t extended_zip_t_init (P_t *pads,extended_zip_t *rep);

Perror_t extended_zip_t_pd_init (P_t *pads,extended_zip_t_pd *pd);

Perror_t extended_zip_t_cleanup (P_t *pads,extended_zip_t *rep);

Perror_t extended_zip_t_pd_cleanup (P_t *pads,extended_zip_t_pd *pd);

Perror_t extended_zip_t_copy (P_t *pads,extended_zip_t *rep_dst,extended_zip_t *rep_src);

Perror_t extended_zip_t_pd_copy (P_t *pads,extended_zip_t_pd *pd_dst,extended_zip_t_pd *pd_src);

void extended_zip_t_m_init (P_t *pads,extended_zip_t_m *mask,Pbase_m baseMask);

Perror_t extended_zip_t_read (P_t *pads,extended_zip_t_m *m,extended_zip_t_pd *pd,extended_zip_t *rep);

int extended_zip_t_verify (extended_zip_t *rep);
typedef struct extended_zip_t_acc_s extended_zip_t_acc;
struct extended_zip_t_acc_s {
  Puint32_acc nerr;
  Puint32_acc zip;
  zipSep_t_acc sep;
  Puint32_acc suffix;
};

Perror_t extended_zip_t_acc_init (P_t *pads,extended_zip_t_acc *acc);

Perror_t extended_zip_t_acc_reset (P_t *pads,extended_zip_t_acc *acc);

Perror_t extended_zip_t_acc_cleanup (P_t *pads,extended_zip_t_acc *acc);

Perror_t extended_zip_t_acc_add (P_t *pads,extended_zip_t_acc *acc,extended_zip_t_pd *pd,extended_zip_t *rep);

Perror_t extended_zip_t_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,extended_zip_t_acc *acc);

Perror_t extended_zip_t_acc_report (P_t *pads,char const *prefix,char const *what,int nst,extended_zip_t_acc *acc);

ssize_t extended_zip_t_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,extended_zip_t_pd *pd,extended_zip_t *rep);

ssize_t extended_zip_t_write2io (P_t *pads,Sfio_t *io,extended_zip_t_pd *pd,extended_zip_t *rep);

ssize_t extended_zip_t_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,extended_zip_t_pd *pd,extended_zip_t *rep,char const *tag,int indent);

ssize_t extended_zip_t_write_xml_2io (P_t *pads,Sfio_t *io,extended_zip_t_pd *pd,extended_zip_t *rep,char const *tag,int indent);

ssize_t extended_zip_t_fmt2buf_final (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,extended_zip_t_m *m,extended_zip_t_pd *pd,extended_zip_t *rep);

ssize_t extended_zip_t_fmt2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,extended_zip_t_m *m,extended_zip_t_pd *pd,extended_zip_t *rep);

ssize_t extended_zip_t_fmt2io (P_t *pads,Sfio_t *io,int *requestedOut,char const *delims,extended_zip_t_m *m,extended_zip_t_pd *pd,extended_zip_t *rep);

PDCI_node_t *extended_zip_t_node_new (PDCI_node_t *parent,char const *name,void *m,void *pd,void *rep,char const *kind,char const *whatfn);

PDCI_node_t *extended_zip_t_cachedNode_init (PDCI_node_t *self);

PDCI_node_t *extended_zip_t_node_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

PDCI_node_t *extended_zip_t_node_kthChildNamed (PDCI_node_t *self,PDCI_childIndex_t idx,char const *name);

PDCI_node_t *extended_zip_t_cachedNode_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

PDCI_node_t *extended_zip_t_sndNode_init (PDCI_node_t *self,PDCI_manager_t *manager,PDCI_childIndex_t ancestor_idx,PDCI_gen_t gen,PDCI_childIndex_t idx);

PDCI_node_t *extended_zip_t_sndNode_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

Perror_t extended_zip_t_node_pathWalk (P_t *pads,extended_zip_t_m *m,extended_zip_t_pd *pd,extended_zip_t *rep,PDCI_path_t path,void **m_out,void **pd_out,void **rep_out);
extern PDCI_vtable_t const extended_zip_t_node_vtable;
extern PDCI_vtable_t const extended_zip_t_cachedNode_vtable;
extern PDCI_vtable_t const extended_zip_t_sndNode_vtable;
typedef enum Pzip_tag_e Pzip_tag;
typedef union Pzip_u_u Pzip_u;
typedef struct Pzip_s Pzip;
typedef struct Pzip_m_s Pzip_m;
typedef union Pzip_pd_u_u Pzip_pd_u;
typedef struct Pzip_pd_s Pzip_pd;
enum Pzip_tag_e {
  Pzip_err=0,
  extendedZip=1,
  smallZip=2,
  largeZip=3
  };
union Pzip_pd_u_u {
  extended_zip_t_pd extendedZip;
  Pbase_pd smallZip;
  Pbase_pd largeZip;
};
struct Pzip_pd_s {
  Pflags_t pstate;
  Puint32 nerr;
  PerrCode_t errCode;
  Ploc_t loc;
  PDCI_id_t _id_;		/* Identifier tag for Galax */
  Pzip_tag tag;
  Pzip_pd_u val;
};
union Pzip_u_u {
  extended_zip_t extendedZip;
  Puint32 smallZip;
  Puint64 largeZip;
};
struct Pzip_s {
  Pzip_tag tag;
  Pzip_u val;
};
struct Pzip_m_s {
  Pbase_m compoundLevel;
  extended_zip_t_m extendedZip;		/* nested constraints */
  Pbase_m smallZip;		/* nested constraints */
  Pbase_m largeZip;		/* nested constraints */
};

ssize_t Pzip_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,Pzip_pd *pd,Pzip *rep);

ssize_t Pzip_write2io (P_t *pads,Sfio_t *io,Pzip_pd *pd,Pzip *rep);

ssize_t Pzip_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,Pzip_pd *pd,Pzip *rep,char const *tag,int indent);

ssize_t Pzip_write_xml_2io (P_t *pads,Sfio_t *io,Pzip_pd *pd,Pzip *rep,char const *tag,int indent);

char const *Pzip_tag2str (Pzip_tag which);

Perror_t Pzip_init (P_t *pads,Pzip *rep);

Perror_t Pzip_pd_init (P_t *pads,Pzip_pd *pd);

Perror_t Pzip_cleanup (P_t *pads,Pzip *rep);

Perror_t Pzip_pd_cleanup (P_t *pads,Pzip_pd *pd);

Perror_t Pzip_copy (P_t *pads,Pzip *rep_dst,Pzip *rep_src);

Perror_t Pzip_pd_copy (P_t *pads,Pzip_pd *pd_dst,Pzip_pd *pd_src);

void Pzip_m_init (P_t *pads,Pzip_m *mask,Pbase_m baseMask);

Perror_t Pzip_read (P_t *pads,Pzip_m *m,Pzip_pd *pd,Pzip *rep);

int Pzip_verify (Pzip *rep);
typedef struct Pzip_acc_s Pzip_acc;
struct Pzip_acc_s {
  Pint32_acc tag;
  extended_zip_t_acc extendedZip;
  Puint32_acc smallZip;
  Puint64_acc largeZip;
};

Perror_t Pzip_acc_init (P_t *pads,Pzip_acc *acc);

Perror_t Pzip_acc_reset (P_t *pads,Pzip_acc *acc);

Perror_t Pzip_acc_cleanup (P_t *pads,Pzip_acc *acc);

Perror_t Pzip_acc_add (P_t *pads,Pzip_acc *acc,Pzip_pd *pd,Pzip *rep);

Perror_t Pzip_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,Pzip_acc *acc);

Perror_t Pzip_acc_report (P_t *pads,char const *prefix,char const *what,int nst,Pzip_acc *acc);

ssize_t Pzip_fmt2buf_final (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,Pzip_m *m,Pzip_pd *pd,Pzip *rep);

ssize_t Pzip_fmt2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,Pzip_m *m,Pzip_pd *pd,Pzip *rep);

ssize_t Pzip_fmt2io (P_t *pads,Sfio_t *io,int *requestedOut,char const *delims,Pzip_m *m,Pzip_pd *pd,Pzip *rep);

PDCI_node_t *Pzip_node_new (PDCI_node_t *parent,char const *name,void *m,void *pd,void *rep,char const *kind,char const *whatfn);

PDCI_node_t *Pzip_cachedNode_init (PDCI_node_t *self);

PDCI_node_t *Pzip_node_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

PDCI_node_t *Pzip_node_kthChildNamed (PDCI_node_t *self,PDCI_childIndex_t idx,char const *name);

PDCI_node_t *Pzip_cachedNode_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

PDCI_node_t *Pzip_sndNode_init (PDCI_node_t *self,PDCI_manager_t *manager,PDCI_childIndex_t ancestor_idx,PDCI_gen_t gen,PDCI_childIndex_t idx);

PDCI_node_t *Pzip_sndNode_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

Perror_t Pzip_node_pathWalk (P_t *pads,Pzip_m *m,Pzip_pd *pd,Pzip *rep,PDCI_path_t path,void **m_out,void **pd_out,void **rep_out);
extern PDCI_vtable_t const Pzip_node_vtable;
extern PDCI_vtable_t const Pzip_cachedNode_vtable;
extern PDCI_vtable_t const Pzip_sndNode_vtable;
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
  PDCI_id_t _id_;		/* Identifier tag for Galax */
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

int summary_header_t_verify (summary_header_t *rep);
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

PDCI_node_t *summary_header_t_node_new (PDCI_node_t *parent,char const *name,void *m,void *pd,void *rep,char const *kind,char const *whatfn);

PDCI_node_t *summary_header_t_cachedNode_init (PDCI_node_t *self);

PDCI_node_t *summary_header_t_node_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

PDCI_node_t *summary_header_t_node_kthChildNamed (PDCI_node_t *self,PDCI_childIndex_t idx,char const *name);

PDCI_node_t *summary_header_t_cachedNode_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

PDCI_node_t *summary_header_t_sndNode_init (PDCI_node_t *self,PDCI_manager_t *manager,PDCI_childIndex_t ancestor_idx,PDCI_gen_t gen,PDCI_childIndex_t idx);

PDCI_node_t *summary_header_t_sndNode_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

Perror_t summary_header_t_node_pathWalk (P_t *pads,summary_header_t_m *m,summary_header_t_pd *pd,summary_header_t *rep,PDCI_path_t path,void **m_out,void **pd_out,void **rep_out);
extern PDCI_vtable_t const summary_header_t_node_vtable;
extern PDCI_vtable_t const summary_header_t_cachedNode_vtable;
extern PDCI_vtable_t const summary_header_t_sndNode_vtable;
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
  PDCI_id_t _id_;		/* Identifier tag for Galax */
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

int no_ramp_t_verify (no_ramp_t *rep);
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

PDCI_node_t *no_ramp_t_node_new (PDCI_node_t *parent,char const *name,void *m,void *pd,void *rep,char const *kind,char const *whatfn);

PDCI_node_t *no_ramp_t_cachedNode_init (PDCI_node_t *self);

PDCI_node_t *no_ramp_t_node_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

PDCI_node_t *no_ramp_t_node_kthChildNamed (PDCI_node_t *self,PDCI_childIndex_t idx,char const *name);

PDCI_node_t *no_ramp_t_cachedNode_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

PDCI_node_t *no_ramp_t_sndNode_init (PDCI_node_t *self,PDCI_manager_t *manager,PDCI_childIndex_t ancestor_idx,PDCI_gen_t gen,PDCI_childIndex_t idx);

PDCI_node_t *no_ramp_t_sndNode_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

Perror_t no_ramp_t_node_pathWalk (P_t *pads,no_ramp_t_m *m,no_ramp_t_pd *pd,no_ramp_t *rep,PDCI_path_t path,void **m_out,void **pd_out,void **rep_out);
extern PDCI_vtable_t const no_ramp_t_node_vtable;
extern PDCI_vtable_t const no_ramp_t_cachedNode_vtable;
extern PDCI_vtable_t const no_ramp_t_sndNode_vtable;
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
  PDCI_id_t _id_;		/* Identifier tag for Galax */
  dib_ramp_t_tag tag;
  dib_ramp_t_pd_u val;
};
union dib_ramp_t_u_u {
  Pint64 ramp;
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

int dib_ramp_t_verify (dib_ramp_t *rep);
typedef struct dib_ramp_t_acc_s dib_ramp_t_acc;
struct dib_ramp_t_acc_s {
  Pint32_acc tag;
  Pint64_acc ramp;
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

PDCI_node_t *dib_ramp_t_node_new (PDCI_node_t *parent,char const *name,void *m,void *pd,void *rep,char const *kind,char const *whatfn);

PDCI_node_t *dib_ramp_t_cachedNode_init (PDCI_node_t *self);

PDCI_node_t *dib_ramp_t_node_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

PDCI_node_t *dib_ramp_t_node_kthChildNamed (PDCI_node_t *self,PDCI_childIndex_t idx,char const *name);

PDCI_node_t *dib_ramp_t_cachedNode_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

PDCI_node_t *dib_ramp_t_sndNode_init (PDCI_node_t *self,PDCI_manager_t *manager,PDCI_childIndex_t ancestor_idx,PDCI_gen_t gen,PDCI_childIndex_t idx);

PDCI_node_t *dib_ramp_t_sndNode_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

Perror_t dib_ramp_t_node_pathWalk (P_t *pads,dib_ramp_t_m *m,dib_ramp_t_pd *pd,dib_ramp_t *rep,PDCI_path_t path,void **m_out,void **pd_out,void **rep_out);
extern PDCI_vtable_t const dib_ramp_t_node_vtable;
extern PDCI_vtable_t const dib_ramp_t_cachedNode_vtable;
extern PDCI_vtable_t const dib_ramp_t_sndNode_vtable;
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
  PDCI_id_t _id_;		/* Identifier tag for Galax */
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

int service_tn_t_verify (service_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version);
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

PDCI_node_t *service_tn_t_node_new (PDCI_node_t *parent,char const *name,void *m,void *pd,void *rep,char const *kind,char const *whatfn);

PDCI_node_t *service_tn_t_cachedNode_init (PDCI_node_t *self);

PDCI_node_t *service_tn_t_node_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

PDCI_node_t *service_tn_t_node_kthChildNamed (PDCI_node_t *self,PDCI_childIndex_t idx,char const *name);

PDCI_node_t *service_tn_t_cachedNode_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

PDCI_node_t *service_tn_t_sndNode_init (PDCI_node_t *self,PDCI_manager_t *manager,PDCI_childIndex_t ancestor_idx,PDCI_gen_t gen,PDCI_childIndex_t idx);

PDCI_node_t *service_tn_t_sndNode_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

Perror_t service_tn_t_node_pathWalk (P_t *pads,service_tn_t_m *m,service_tn_t_pd *pd,service_tn_t *rep,PDCI_path_t path,void **m_out,void **pd_out,void **rep_out);
extern PDCI_vtable_t const service_tn_t_node_vtable;
extern PDCI_vtable_t const service_tn_t_cachedNode_vtable;
extern PDCI_vtable_t const service_tn_t_sndNode_vtable;
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
  PDCI_id_t _id_;		/* Identifier tag for Galax */
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

int billing_tn_t_verify (billing_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn);
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

PDCI_node_t *billing_tn_t_node_new (PDCI_node_t *parent,char const *name,void *m,void *pd,void *rep,char const *kind,char const *whatfn);

PDCI_node_t *billing_tn_t_cachedNode_init (PDCI_node_t *self);

PDCI_node_t *billing_tn_t_node_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

PDCI_node_t *billing_tn_t_node_kthChildNamed (PDCI_node_t *self,PDCI_childIndex_t idx,char const *name);

PDCI_node_t *billing_tn_t_cachedNode_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

PDCI_node_t *billing_tn_t_sndNode_init (PDCI_node_t *self,PDCI_manager_t *manager,PDCI_childIndex_t ancestor_idx,PDCI_gen_t gen,PDCI_childIndex_t idx);

PDCI_node_t *billing_tn_t_sndNode_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

Perror_t billing_tn_t_node_pathWalk (P_t *pads,billing_tn_t_m *m,billing_tn_t_pd *pd,billing_tn_t *rep,PDCI_path_t path,void **m_out,void **pd_out,void **rep_out);
extern PDCI_vtable_t const billing_tn_t_node_vtable;
extern PDCI_vtable_t const billing_tn_t_cachedNode_vtable;
extern PDCI_vtable_t const billing_tn_t_sndNode_vtable;
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
  PDCI_id_t _id_;		/* Identifier tag for Galax */
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

int nlp_service_tn_t_verify (nlp_service_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn);
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

PDCI_node_t *nlp_service_tn_t_node_new (PDCI_node_t *parent,char const *name,void *m,void *pd,void *rep,char const *kind,char const *whatfn);

PDCI_node_t *nlp_service_tn_t_cachedNode_init (PDCI_node_t *self);

PDCI_node_t *nlp_service_tn_t_node_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

PDCI_node_t *nlp_service_tn_t_node_kthChildNamed (PDCI_node_t *self,PDCI_childIndex_t idx,char const *name);

PDCI_node_t *nlp_service_tn_t_cachedNode_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

PDCI_node_t *nlp_service_tn_t_sndNode_init (PDCI_node_t *self,PDCI_manager_t *manager,PDCI_childIndex_t ancestor_idx,PDCI_gen_t gen,PDCI_childIndex_t idx);

PDCI_node_t *nlp_service_tn_t_sndNode_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

Perror_t nlp_service_tn_t_node_pathWalk (P_t *pads,nlp_service_tn_t_m *m,nlp_service_tn_t_pd *pd,nlp_service_tn_t *rep,PDCI_path_t path,void **m_out,void **pd_out,void **rep_out);
extern PDCI_vtable_t const nlp_service_tn_t_node_vtable;
extern PDCI_vtable_t const nlp_service_tn_t_cachedNode_vtable;
extern PDCI_vtable_t const nlp_service_tn_t_sndNode_vtable;
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
  PDCI_id_t _id_;		/* Identifier tag for Galax */
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

int nlp_billing_tn_t_verify (nlp_billing_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn,nlp_service_tn_t *nlp_service_tn);
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

PDCI_node_t *nlp_billing_tn_t_node_new (PDCI_node_t *parent,char const *name,void *m,void *pd,void *rep,char const *kind,char const *whatfn);

PDCI_node_t *nlp_billing_tn_t_cachedNode_init (PDCI_node_t *self);

PDCI_node_t *nlp_billing_tn_t_node_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

PDCI_node_t *nlp_billing_tn_t_node_kthChildNamed (PDCI_node_t *self,PDCI_childIndex_t idx,char const *name);

PDCI_node_t *nlp_billing_tn_t_cachedNode_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

PDCI_node_t *nlp_billing_tn_t_sndNode_init (PDCI_node_t *self,PDCI_manager_t *manager,PDCI_childIndex_t ancestor_idx,PDCI_gen_t gen,PDCI_childIndex_t idx);

PDCI_node_t *nlp_billing_tn_t_sndNode_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

Perror_t nlp_billing_tn_t_node_pathWalk (P_t *pads,nlp_billing_tn_t_m *m,nlp_billing_tn_t_pd *pd,nlp_billing_tn_t *rep,PDCI_path_t path,void **m_out,void **pd_out,void **rep_out);
extern PDCI_vtable_t const nlp_billing_tn_t_node_vtable;
extern PDCI_vtable_t const nlp_billing_tn_t_cachedNode_vtable;
extern PDCI_vtable_t const nlp_billing_tn_t_sndNode_vtable;
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
  Pzip_pd some_zip_code_t;
  Pbase_pd none_zip_code_t;		/* value was not present. none_zip_code_t = 0 */
};
struct zip_code_t_pd_s {
  Pflags_t pstate;
  Puint32 nerr;
  PerrCode_t errCode;
  Ploc_t loc;
  PDCI_id_t _id_;		/* Identifier tag for Galax */
  zip_code_t_tag tag;
  zip_code_t_pd_u val;
};
union zip_code_t_u_u {
  Pzip some_zip_code_t;		/* value is present */
};
struct zip_code_t_s {
  zip_code_t_tag tag;
  zip_code_t_u val;
};
struct zip_code_t_m_s {
  Pbase_m compoundLevel;
  Pzip_m some_zip_code_t;		/* nested constraints */
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

int zip_code_t_verify (zip_code_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn,nlp_service_tn_t *nlp_service_tn,nlp_billing_tn_t *nlp_billing_tn);
typedef struct zip_code_t_acc_s zip_code_t_acc;
struct zip_code_t_acc_s {
  Pint32_acc tag;
  Pzip_acc some_zip_code_t;
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

PDCI_node_t *zip_code_t_node_new (PDCI_node_t *parent,char const *name,void *m,void *pd,void *rep,char const *kind,char const *whatfn);

PDCI_node_t *zip_code_t_cachedNode_init (PDCI_node_t *self);

PDCI_node_t *zip_code_t_node_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

PDCI_node_t *zip_code_t_node_kthChildNamed (PDCI_node_t *self,PDCI_childIndex_t idx,char const *name);

PDCI_node_t *zip_code_t_cachedNode_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

PDCI_node_t *zip_code_t_sndNode_init (PDCI_node_t *self,PDCI_manager_t *manager,PDCI_childIndex_t ancestor_idx,PDCI_gen_t gen,PDCI_childIndex_t idx);

PDCI_node_t *zip_code_t_sndNode_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

Perror_t zip_code_t_node_pathWalk (P_t *pads,zip_code_t_m *m,zip_code_t_pd *pd,zip_code_t *rep,PDCI_path_t path,void **m_out,void **pd_out,void **rep_out);
extern PDCI_vtable_t const zip_code_t_node_vtable;
extern PDCI_vtable_t const zip_code_t_cachedNode_vtable;
extern PDCI_vtable_t const zip_code_t_sndNode_vtable;
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
  PDCI_id_t _id_;		/* Identifier tag for Galax */
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

int order_header_t_verify (order_header_t *rep);
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

PDCI_node_t *order_header_t_node_new (PDCI_node_t *parent,char const *name,void *m,void *pd,void *rep,char const *kind,char const *whatfn);

PDCI_node_t *order_header_t_cachedNode_init (PDCI_node_t *self);

PDCI_node_t *order_header_t_node_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

PDCI_node_t *order_header_t_node_kthChildNamed (PDCI_node_t *self,PDCI_childIndex_t idx,char const *name);

PDCI_node_t *order_header_t_cachedNode_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

PDCI_node_t *order_header_t_sndNode_init (PDCI_node_t *self,PDCI_manager_t *manager,PDCI_childIndex_t ancestor_idx,PDCI_gen_t gen,PDCI_childIndex_t idx);

PDCI_node_t *order_header_t_sndNode_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

Perror_t order_header_t_node_pathWalk (P_t *pads,order_header_t_m *m,order_header_t_pd *pd,order_header_t *rep,PDCI_path_t path,void **m_out,void **pd_out,void **rep_out);
extern PDCI_vtable_t const order_header_t_node_vtable;
extern PDCI_vtable_t const order_header_t_cachedNode_vtable;
extern PDCI_vtable_t const order_header_t_sndNode_vtable;
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
  PDCI_id_t _id_;		/* Identifier tag for Galax */
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

int event_t_verify (event_t *rep);
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

PDCI_node_t *event_t_node_new (PDCI_node_t *parent,char const *name,void *m,void *pd,void *rep,char const *kind,char const *whatfn);

PDCI_node_t *event_t_cachedNode_init (PDCI_node_t *self);

PDCI_node_t *event_t_node_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

PDCI_node_t *event_t_node_kthChildNamed (PDCI_node_t *self,PDCI_childIndex_t idx,char const *name);

PDCI_node_t *event_t_cachedNode_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

PDCI_node_t *event_t_sndNode_init (PDCI_node_t *self,PDCI_manager_t *manager,PDCI_childIndex_t ancestor_idx,PDCI_gen_t gen,PDCI_childIndex_t idx);

PDCI_node_t *event_t_sndNode_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

Perror_t event_t_node_pathWalk (P_t *pads,event_t_m *m,event_t_pd *pd,event_t *rep,PDCI_path_t path,void **m_out,void **pd_out,void **rep_out);
extern PDCI_vtable_t const event_t_node_vtable;
extern PDCI_vtable_t const event_t_cachedNode_vtable;
extern PDCI_vtable_t const event_t_sndNode_vtable;
typedef struct eventSeq_s eventSeq;
typedef struct eventSeq_m_s eventSeq_m;
typedef struct eventSeq_pd_s eventSeq_pd;
typedef struct eventSeq_ro_params_t_s eventSeq_ro_params_t;
struct eventSeq_m_s {
  event_t_m element;		/* per-element */
  Pbase_m compoundLevel;		/* entire array */
};
struct eventSeq_pd_s {
  Pflags_t pstate;
  Puint32 nerr;		/* Number of array errors */
  PerrCode_t errCode;
  Ploc_t loc;
  PDCI_id_t _id_;		/* Identifier tag for Galax */
  Puint32 neerr;		/* Number of element errors */
  Puint32 firstError;		/* if errCode == ARRAY_ELEM_ERR, index of first error */
  Puint32 numRead;		/* Number of elements read */
  Puint32 length;		/* Number of elements in memory */
  event_t_pd *elts;
  RBuf_t *_internal;
};
struct eventSeq_s {
  Puint32 length;
  event_t *elts;
  RBuf_t *_internal;
};
struct eventSeq_ro_params_t_s {
  Ploc_t beginLoc;		/* location of array beginning */
};

Perror_t eventSeq_init (P_t *pads,eventSeq *rep);

Perror_t eventSeq_pd_init (P_t *pads,eventSeq_pd *pd);

Perror_t eventSeq_cleanup (P_t *pads,eventSeq *rep);

Perror_t eventSeq_pd_cleanup (P_t *pads,eventSeq_pd *pd);

Perror_t eventSeq_copy (P_t *pads,eventSeq *rep_dst,eventSeq *rep_src);

Perror_t eventSeq_pd_copy (P_t *pads,eventSeq_pd *pd_dst,eventSeq_pd *pd_src);

void eventSeq_m_init (P_t *pads,eventSeq_m *mask,Pbase_m baseMask);

void eventSeq_ro_params_init (eventSeq_ro_params_t *params);

Pread_res_t eventSeq_final_checks (P_t *pads,eventSeq_m *m,eventSeq_pd *pd,eventSeq *rep,Ploc_t *loc_ptr,int consume);

Pread_res_t eventSeq_read_one_init (P_t *pads,eventSeq_m *m,eventSeq_pd *pd,eventSeq *rep,Ploc_t *loc_ptr);

Pread_res_t eventSeq_read_one (P_t *pads,eventSeq_m *m,eventSeq_pd *pd,eventSeq *rep,Ploc_t *loc_ptr,event_t_pd *elt_pd,event_t *elt_rep);

Perror_t eventSeq_read (P_t *pads,eventSeq_m *m,eventSeq_pd *pd,eventSeq *rep);

Pread_res_t eventSeq_reread_one (P_t *pads,eventSeq_m *m,eventSeq_pd *pd,eventSeq *rep,Ploc_t *loc_ptr,event_t_pd *elt_pd,event_t *elt_rep,int notFirstElt);

int eventSeq_verify (eventSeq *rep);
typedef struct eventSeq_acc_s eventSeq_acc;
struct eventSeq_acc_s {
  Puint32_acc length;		/* Accumulator for array length */
  event_t_acc compoundLevel;		/* Accumulator for all array elements */
  event_t_acc arrayDetail[10];		/* Accumulator for first 10 array elements */
};

Perror_t eventSeq_acc_init (P_t *pads,eventSeq_acc *acc);

Perror_t eventSeq_acc_reset (P_t *pads,eventSeq_acc *acc);

Perror_t eventSeq_acc_cleanup (P_t *pads,eventSeq_acc *acc);

Perror_t eventSeq_acc_add (P_t *pads,eventSeq_acc *acc,eventSeq_pd *pd,eventSeq *rep);

Perror_t eventSeq_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,eventSeq_acc *acc);

Perror_t eventSeq_acc_report (P_t *pads,char const *prefix,char const *what,int nst,eventSeq_acc *acc);

ssize_t eventSeq_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,eventSeq_pd *pd,eventSeq *rep);

ssize_t eventSeq_write2io (P_t *pads,Sfio_t *io,eventSeq_pd *pd,eventSeq *rep);

ssize_t eventSeq_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,eventSeq_pd *pd,eventSeq *rep,char const *tag,int indent);

ssize_t eventSeq_write_xml_2io (P_t *pads,Sfio_t *io,eventSeq_pd *pd,eventSeq *rep,char const *tag,int indent);

ssize_t eventSeq_fmt2buf_final (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,eventSeq_m *m,eventSeq_pd *pd,eventSeq *rep);

ssize_t eventSeq_fmt2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,eventSeq_m *m,eventSeq_pd *pd,eventSeq *rep);

ssize_t eventSeq_fmt2io (P_t *pads,Sfio_t *io,int *requestedOut,char const *delims,eventSeq_m *m,eventSeq_pd *pd,eventSeq *rep);

PDCI_node_t *eventSeq_node_new (PDCI_node_t *parent,char const *name,void *m,void *pd,void *rep,char const *kind,char const *whatfn);

PDCI_node_t *eventSeq_cachedNode_init (PDCI_node_t *self);

PDCI_node_t *eventSeq_node_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

PDCI_node_t *eventSeq_node_kthChildNamed (PDCI_node_t *self,PDCI_childIndex_t idx,char const *name);

PDCI_node_t *eventSeq_cachedNode_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

PDCI_node_t *eventSeq_sndNode_init (PDCI_node_t *self,PDCI_manager_t *manager,PDCI_childIndex_t ancestor_idx,PDCI_gen_t gen,PDCI_childIndex_t idx);

PDCI_node_t *eventSeq_sndNode_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

Perror_t eventSeq_node_pathWalk (P_t *pads,eventSeq_m *m,eventSeq_pd *pd,eventSeq *rep,PDCI_path_t path,void **m_out,void **pd_out,void **rep_out);
extern PDCI_vtable_t const eventSeq_node_vtable;
extern PDCI_vtable_t const eventSeq_cachedNode_vtable;
extern PDCI_vtable_t const eventSeq_sndNode_vtable;
typedef struct entry_t_s entry_t;
typedef struct entry_t_m_s entry_t_m;
typedef struct entry_t_pd_s entry_t_pd;
struct entry_t_m_s {
  Pbase_m compoundLevel;
  order_header_t_m header;		/* nested constraints */
  eventSeq_m events;		/* nested constraints */
};
struct entry_t_pd_s {
  Pflags_t pstate;
  Puint32 nerr;
  PerrCode_t errCode;
  Ploc_t loc;
  PDCI_id_t _id_;		/* Identifier tag for Galax */
  order_header_t_pd header;
  eventSeq_pd events;
};
struct entry_t_s {
  order_header_t header;
  eventSeq events;
};

Perror_t entry_t_init (P_t *pads,entry_t *rep);

Perror_t entry_t_pd_init (P_t *pads,entry_t_pd *pd);

Perror_t entry_t_cleanup (P_t *pads,entry_t *rep);

Perror_t entry_t_pd_cleanup (P_t *pads,entry_t_pd *pd);

Perror_t entry_t_copy (P_t *pads,entry_t *rep_dst,entry_t *rep_src);

Perror_t entry_t_pd_copy (P_t *pads,entry_t_pd *pd_dst,entry_t_pd *pd_src);

void entry_t_m_init (P_t *pads,entry_t_m *mask,Pbase_m baseMask);

Perror_t entry_t_read (P_t *pads,entry_t_m *m,entry_t_pd *pd,entry_t *rep);

int entry_t_verify (entry_t *rep);
typedef struct entry_t_acc_s entry_t_acc;
struct entry_t_acc_s {
  Puint32_acc nerr;
  order_header_t_acc header;
  eventSeq_acc events;
};

Perror_t entry_t_acc_init (P_t *pads,entry_t_acc *acc);

Perror_t entry_t_acc_reset (P_t *pads,entry_t_acc *acc);

Perror_t entry_t_acc_cleanup (P_t *pads,entry_t_acc *acc);

Perror_t entry_t_acc_add (P_t *pads,entry_t_acc *acc,entry_t_pd *pd,entry_t *rep);

Perror_t entry_t_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,entry_t_acc *acc);

Perror_t entry_t_acc_report (P_t *pads,char const *prefix,char const *what,int nst,entry_t_acc *acc);

ssize_t entry_t_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,entry_t_pd *pd,entry_t *rep);

ssize_t entry_t_write2io (P_t *pads,Sfio_t *io,entry_t_pd *pd,entry_t *rep);

ssize_t entry_t_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,entry_t_pd *pd,entry_t *rep,char const *tag,int indent);

ssize_t entry_t_write_xml_2io (P_t *pads,Sfio_t *io,entry_t_pd *pd,entry_t *rep,char const *tag,int indent);

ssize_t entry_t_fmt2buf_final (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,entry_t_m *m,entry_t_pd *pd,entry_t *rep);

ssize_t entry_t_fmt2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,entry_t_m *m,entry_t_pd *pd,entry_t *rep);

ssize_t entry_t_fmt2io (P_t *pads,Sfio_t *io,int *requestedOut,char const *delims,entry_t_m *m,entry_t_pd *pd,entry_t *rep);

PDCI_node_t *entry_t_node_new (PDCI_node_t *parent,char const *name,void *m,void *pd,void *rep,char const *kind,char const *whatfn);

PDCI_node_t *entry_t_cachedNode_init (PDCI_node_t *self);

PDCI_node_t *entry_t_node_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

PDCI_node_t *entry_t_node_kthChildNamed (PDCI_node_t *self,PDCI_childIndex_t idx,char const *name);

PDCI_node_t *entry_t_cachedNode_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

PDCI_node_t *entry_t_sndNode_init (PDCI_node_t *self,PDCI_manager_t *manager,PDCI_childIndex_t ancestor_idx,PDCI_gen_t gen,PDCI_childIndex_t idx);

PDCI_node_t *entry_t_sndNode_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

Perror_t entry_t_node_pathWalk (P_t *pads,entry_t_m *m,entry_t_pd *pd,entry_t *rep,PDCI_path_t path,void **m_out,void **pd_out,void **rep_out);
extern PDCI_vtable_t const entry_t_node_vtable;
extern PDCI_vtable_t const entry_t_cachedNode_vtable;
extern PDCI_vtable_t const entry_t_sndNode_vtable;
typedef struct entries_t_s entries_t;
typedef struct entries_t_m_s entries_t_m;
typedef struct entries_t_pd_s entries_t_pd;
typedef struct entries_t_ro_params_t_s entries_t_ro_params_t;
typedef struct entries_t_array_info_t_s entries_t_array_info_t;
struct entries_t_m_s {
  entry_t_m element;		/* per-element */
  Pbase_m compoundLevel;		/* entire array */
};
struct entries_t_pd_s {
  Pflags_t pstate;
  Puint32 nerr;		/* Number of array errors */
  PerrCode_t errCode;
  Ploc_t loc;
  PDCI_id_t _id_;		/* Identifier tag for Galax */
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
struct entries_t_array_info_t_s {
  PDCI_smart_array_info_t base;
  entries_t_ro_params_t params;		/* Type-specific parameters to read related funcitons. */
};

/*
 **************************** HAND-CODED : BEGIN ****************************
 */

PDCI_node_t *entries_t_linearNode_init (PDCI_node_t *self);

PDCI_node_t *entries_t_linearNode_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

PDCI_node_t *entries_t_linearNode_kthChildNamed (PDCI_node_t *self,PDCI_childIndex_t idx,
						 char const *name);
extern PDCI_vtable_t const entries_t_linearNode_vtable;
/*
 **************************** HAND-CODED : END ****************************
 */

Perror_t entries_t_init (P_t *pads,entries_t *rep);

Perror_t entries_t_pd_init (P_t *pads,entries_t_pd *pd);

Perror_t entries_t_cleanup (P_t *pads,entries_t *rep);

Perror_t entries_t_pd_cleanup (P_t *pads,entries_t_pd *pd);

Perror_t entries_t_copy (P_t *pads,entries_t *rep_dst,entries_t *rep_src);

Perror_t entries_t_pd_copy (P_t *pads,entries_t_pd *pd_dst,entries_t_pd *pd_src);

void entries_t_m_init (P_t *pads,entries_t_m *mask,Pbase_m baseMask);

void entries_t_ro_params_init (entries_t_ro_params_t *params);

Pread_res_t entries_t_final_checks (P_t *pads,entries_t_m *m,entries_t_pd *pd,entries_t *rep,Ploc_t *loc_ptr,int consume);

Pread_res_t entries_t_read_one_init (P_t *pads,entries_t_m *m,entries_t_pd *pd,entries_t *rep,Ploc_t *loc_ptr);

Pread_res_t entries_t_read_one (P_t *pads,entries_t_m *m,entries_t_pd *pd,entries_t *rep,Ploc_t *loc_ptr,entry_t_pd *elt_pd,entry_t *elt_rep);

Perror_t entries_t_read (P_t *pads,entries_t_m *m,entries_t_pd *pd,entries_t *rep);

Pread_res_t entries_t_reread_one (P_t *pads,entries_t_m *m,entries_t_pd *pd,entries_t *rep,Ploc_t *loc_ptr,entry_t_pd *elt_pd,entry_t *elt_rep,int notFirstElt);

int entries_t_verify (entries_t *rep);
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

PDCI_node_t *entries_t_node_new (PDCI_node_t *parent,char const *name,void *m,void *pd,void *rep,char const *kind,char const *whatfn);

PDCI_node_t *entries_t_cachedNode_init (PDCI_node_t *self);

PDCI_node_t *entries_t_node_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

PDCI_node_t *entries_t_node_kthChildNamed (PDCI_node_t *self,PDCI_childIndex_t idx,char const *name);

PDCI_node_t *entries_t_cachedNode_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

PDCI_node_t *entries_t_sndNode_init (PDCI_node_t *self,PDCI_manager_t *manager,PDCI_childIndex_t ancestor_idx,PDCI_gen_t gen,PDCI_childIndex_t idx);

PDCI_node_t *entries_t_sndNode_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

Perror_t entries_t_node_pathWalk (P_t *pads,entries_t_m *m,entries_t_pd *pd,entries_t *rep,PDCI_path_t path,void **m_out,void **pd_out,void **rep_out);
extern PDCI_vtable_t const entries_t_node_vtable;
extern PDCI_vtable_t const entries_t_cachedNode_vtable;
extern PDCI_vtable_t const entries_t_sndNode_vtable;

Perror_t entries_t_smartNode_eltAlloc (PDCI_node_t *smartNode,P_t *pads,void **elt_pd,void **elt_rep);

Pread_res_t entries_t_smartNode_eltRead (PDCI_node_t *smartNode,P_t *pads,PDCI_smart_elt_info_t *info);

Perror_t entries_t_smartNode_eltFree (P_t *pads,PDCI_smart_elt_info_t *info);

Perror_t entries_t_smartNode_eltPathWalk (P_t *pads,void *m,void *pd,void *rep,PDCI_path_t path,void **m_out,void **pd_out,void **rep_out);

PDCI_smart_array_info_t *entries_t_array_info_init (P_t *pads,unsigned int max_elts);

PDCI_node_t *entries_t_smartNode_init (PDCI_node_t *self,unsigned int max_elts);

PDCI_node_t *entries_t_smartNode_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

PDCI_node_t *entries_t_smartNode_kthChildNamed (PDCI_node_t *self,PDCI_childIndex_t idx,char const *name);
extern PDCI_vtable_t const entries_t_smartNode_vtable;



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
  PDCI_id_t _id_;		/* Identifier tag for Galax */
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

int out_sum_verify (out_sum *rep);
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

PDCI_node_t *out_sum_node_new (PDCI_node_t *parent,char const *name,void *m,void *pd,void *rep,char const *kind,char const *whatfn);

PDCI_node_t *out_sum_cachedNode_init (PDCI_node_t *self);

PDCI_node_t *out_sum_node_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

PDCI_node_t *out_sum_node_kthChildNamed (PDCI_node_t *self,PDCI_childIndex_t idx,char const *name);

PDCI_node_t *out_sum_cachedNode_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

PDCI_node_t *out_sum_sndNode_init (PDCI_node_t *self,PDCI_manager_t *manager,PDCI_childIndex_t ancestor_idx,PDCI_gen_t gen,PDCI_childIndex_t idx);

PDCI_node_t *out_sum_sndNode_kthChild (PDCI_node_t *self,PDCI_childIndex_t idx);

Perror_t out_sum_node_pathWalk (P_t *pads,out_sum_m *m,out_sum_pd *pd,out_sum *rep,PDCI_path_t path,void **m_out,void **pd_out,void **rep_out);
extern PDCI_vtable_t const out_sum_node_vtable;
extern PDCI_vtable_t const out_sum_cachedNode_vtable;
extern PDCI_vtable_t const out_sum_sndNode_vtable;

void P_lib_init ();

#endif /*  __DIBBLER_NEW__H__  */
