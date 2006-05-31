/*@FILE @LEFT binarytree.BinarytreeRep.tex  binarytree.BinarytreeCSM.tex binarytree.BinarytreePD.tex binarytree.BinarytreeOps.tex */
#ifndef __BINARYTREE__H__
#define __BINARYTREE__H__
#include "pads.h"

/*@BEGIN binarytree.BinarytreePD.tex */
typedef struct tree_pd_s tree_pd;

struct tree_pd_s {
  Pflags_t pstate;
  Puint32 nerr;
  PerrCode_t errCode;
  Ploc_t loc;
  struct _tree_pd_s *val;
};
/*@END binarytree.BinarytreePD.tex */

/*@BEGIN binarytree.BinarytreeRep.tex */
typedef struct _tree_s *tree;
/*@END binarytree.BinarytreeRep.tex */

/*@BEGIN binarytree.BinarytreeCSM.tex */
typedef struct _tree_m_s *tree_m;
/*@END binarytree.BinarytreeCSM.tex */

typedef void *tree_acc;

/*@BEGIN binarytree.BinarytreeOps.tex */
Perror_t tree_init (P_t *,tree *);

Perror_t tree_pd_init (P_t *,tree_pd *);

Perror_t tree_cleanup (P_t *,tree *);

Perror_t tree_pd_cleanup (P_t *,tree_pd *);

Perror_t tree_copy (P_t *,tree *,tree *);

Perror_t tree_pd_copy (P_t *,tree_pd *,tree_pd *);

void tree_m_init (P_t *,tree_m *,Pbase_m);

Perror_t tree_read (P_t *,tree_m *,tree_pd *,tree *);

int tree_verify (tree *);

int tree_genPD (P_t *,tree *,tree_pd *);

ssize_t tree_write2buf (P_t *,Pbyte *,size_t,int *,tree_pd *,tree *);

ssize_t tree_write2io (P_t *,Sfio_t *,tree_pd *,tree *);

ssize_t tree_write_xml_2buf (P_t *,Pbyte *,size_t,int *,tree_pd *,tree *,char const *,int);

ssize_t tree_write_xml_2io (P_t *,Sfio_t *,tree_pd *,tree *,char const *,int);
/*@END binarytree.BinarytreeOps.tex */

extern Perror_t tree_acc_init (P_t *,tree_acc *);
extern Perror_t tree_acc_reset (P_t *,tree_acc *);
extern Perror_t tree_acc_cleanup (P_t *,tree_acc *);
extern Perror_t tree_acc_add (P_t *,tree_acc *,tree_pd *,tree *);
extern Perror_t tree_acc_report2io (P_t *,Sfio_t *,char const *,char const *,int,tree_acc *);
extern Perror_t tree_acc_report (P_t *,char const *,char const *,int,tree_acc *);
extern Perror_t tree_acc_report2xml_io (P_t *,Sfio_t *,int,tree_acc *);

extern ssize_t tree_fmt2buf_final (P_t *,Pbyte *,size_t,int *,int *,char const *,tree_m *,tree_pd *,tree *);
extern ssize_t tree_fmt2buf (P_t *,Pbyte *,size_t,int *,int *,char const *,tree_m *,tree_pd *,tree *);
extern ssize_t tree_fmt2io (P_t *,Sfio_t *,int *,char const *,tree_m *,tree_pd *,tree *);


typedef struct fullTree_s fullTree;
typedef struct fullTree_m_s fullTree_m;
typedef struct fullTree_pd_s fullTree_pd;
struct fullTree_m_s {
  Pbase_m compoundLevel;
  tree_m left;		/* nested constraints */
  tree_m right;		/* nested constraints */
};
struct fullTree_pd_s {
  Pflags_t pstate;
  Puint32 nerr;
  PerrCode_t errCode;
  Ploc_t loc;
  tree_pd left;
  tree_pd right;
};
struct fullTree_s {
  tree left;
  tree right;
};

Perror_t fullTree_init (P_t *pads,fullTree *rep);

Perror_t fullTree_pd_init (P_t *pads,fullTree_pd *pd);

Perror_t fullTree_cleanup (P_t *pads,fullTree *rep);

Perror_t fullTree_pd_cleanup (P_t *pads,fullTree_pd *pd);

Perror_t fullTree_copy (P_t *pads,fullTree *rep_dst,fullTree *rep_src);

Perror_t fullTree_pd_copy (P_t *pads,fullTree_pd *pd_dst,fullTree_pd *pd_src);

void fullTree_m_init (P_t *pads,fullTree_m *mask,Pbase_m baseMask);

Perror_t fullTree_read (P_t *pads,fullTree_m *m,fullTree_pd *pd,fullTree *rep);

int fullTree_verify (fullTree *rep);

int fullTree_genPD (P_t *pads,fullTree *rep,fullTree_pd *pd);
typedef struct fullTree_acc_s fullTree_acc;
struct fullTree_acc_s {
  Puint32_acc nerr;
  tree_acc left;
  tree_acc right;
};

Perror_t fullTree_acc_init (P_t *pads,fullTree_acc *acc);

Perror_t fullTree_acc_reset (P_t *pads,fullTree_acc *acc);

Perror_t fullTree_acc_cleanup (P_t *pads,fullTree_acc *acc);

Perror_t fullTree_acc_add (P_t *pads,fullTree_acc *acc,fullTree_pd *pd,fullTree *rep);

Perror_t fullTree_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,fullTree_acc *acc);

Perror_t fullTree_acc_report (P_t *pads,char const *prefix,char const *what,int nst,fullTree_acc *acc);

Perror_t fullTree_acc_report2xml_io (P_t *pads,Sfio_t *outstr,int nst,fullTree_acc *acc);

ssize_t fullTree_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,fullTree_pd *pd,fullTree *rep);

ssize_t fullTree_write2io (P_t *pads,Sfio_t *io,fullTree_pd *pd,fullTree *rep);

ssize_t fullTree_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,fullTree_pd *pd,fullTree *rep,char const *_tag,int indent);

ssize_t fullTree_write_xml_2io (P_t *pads,Sfio_t *io,fullTree_pd *pd,fullTree *rep,char const *_tag,int indent);

ssize_t fullTree_fmt2buf_final (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,fullTree_m *m,fullTree_pd *pd,fullTree *rep);

ssize_t fullTree_fmt2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,fullTree_m *m,fullTree_pd *pd,fullTree *rep);

ssize_t fullTree_fmt2io (P_t *pads,Sfio_t *io,int *requestedOut,char const *delims,fullTree_m *m,fullTree_pd *pd,fullTree *rep);
typedef enum _tree_tag_e _tree_tag;
typedef union _tree_u_u _tree_u;
typedef struct _tree_s _tree;
typedef struct _tree_m_s _tree_m;
typedef union _tree_pd_u_u _tree_pd_u;
typedef struct _tree_pd_s _tree_pd;
enum _tree_tag_e {
  _tree_err=0,
  value=1,
  nested=2
  };
union _tree_pd_u_u {
  Pbase_pd value;
  fullTree_pd nested;
};
struct _tree_pd_s {
  Pflags_t pstate;
  Puint32 nerr;
  PerrCode_t errCode;
  Ploc_t loc;
  _tree_tag tag;
  _tree_pd_u val;
};
union _tree_u_u {
  Pint32 value;
  fullTree nested;
};
struct _tree_s {
  _tree_tag tag;
  _tree_u val;
};
struct _tree_m_s {
  Pbase_m compoundLevel;
  Pbase_m value;		/* nested constraints */
  fullTree_m nested;		/* nested constraints */
};

ssize_t _tree_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,_tree_pd *pd,_tree *rep);

ssize_t _tree_write2io (P_t *pads,Sfio_t *io,_tree_pd *pd,_tree *rep);

ssize_t _tree_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,_tree_pd *pd,_tree *rep,char const *_tag,int indent);

ssize_t _tree_write_xml_2io (P_t *pads,Sfio_t *io,_tree_pd *pd,_tree *rep,char const *_tag,int indent);

char const *_tree_tag2str (_tree_tag which);

Perror_t _tree_init (P_t *pads,_tree *rep);

Perror_t _tree_pd_init (P_t *pads,_tree_pd *pd);

Perror_t _tree_cleanup (P_t *pads,_tree *rep);

Perror_t _tree_pd_cleanup (P_t *pads,_tree_pd *pd);

Perror_t _tree_copy (P_t *pads,_tree *rep_dst,_tree *rep_src);

Perror_t _tree_pd_copy (P_t *pads,_tree_pd *pd_dst,_tree_pd *pd_src);

void _tree_m_init (P_t *pads,_tree_m *mask,Pbase_m baseMask);

Perror_t _tree_read (P_t *pads,_tree_m *m,_tree_pd *pd,_tree *rep);

int _tree_verify (_tree *rep);

int _tree_genPD (P_t *pads,_tree *rep,_tree_pd *pd);
typedef struct _tree_acc_s _tree_acc;
struct _tree_acc_s {
  Pint32_acc tag;
  Pint32_acc value;
  fullTree_acc nested;
};

Perror_t _tree_acc_init (P_t *pads,_tree_acc *acc);

Perror_t _tree_acc_reset (P_t *pads,_tree_acc *acc);

Perror_t _tree_acc_cleanup (P_t *pads,_tree_acc *acc);

Perror_t _tree_acc_add (P_t *pads,_tree_acc *acc,_tree_pd *pd,_tree *rep);

Perror_t _tree_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,_tree_acc *acc);

Perror_t _tree_acc_report (P_t *pads,char const *prefix,char const *what,int nst,_tree_acc *acc);

Perror_t _tree_acc_report2xml_io (P_t *pads,Sfio_t *outstr,int nst,_tree_acc *acc);

ssize_t _tree_fmt2buf_final (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,_tree_m *m,_tree_pd *pd,_tree *rep);

ssize_t _tree_fmt2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,_tree_m *m,_tree_pd *pd,_tree *rep);

ssize_t _tree_fmt2io (P_t *pads,Sfio_t *io,int *requestedOut,char const *delims,_tree_m *m,_tree_pd *pd,_tree *rep);

Perror_t tree_init (P_t *pads,tree *rep);

Perror_t tree_pd_init (P_t *pads,tree_pd *pd);

Perror_t tree_cleanup (P_t *pads,tree *rep);

Perror_t tree_pd_cleanup (P_t *pads,tree_pd *pd);

Perror_t tree_copy (P_t *pads,tree *rep_dst,tree *rep_src);

Perror_t tree_pd_copy (P_t *pads,tree_pd *pd_dst,tree_pd *pd_src);

void tree_m_init (P_t *pads,tree_m *mask,Pbase_m baseMask);

Perror_t tree_read (P_t *pads,tree_m *m,tree_pd *pd,tree *rep);

int tree_verify (tree *rep);

int tree_genPD (P_t *pads,tree *rep,tree_pd *pd);

Perror_t tree_acc_init (P_t *pads,tree_acc *acc);

Perror_t tree_acc_reset (P_t *pads,tree_acc *acc);

Perror_t tree_acc_cleanup (P_t *pads,tree_acc *acc);

Perror_t tree_acc_add (P_t *pads,tree_acc *acc,tree_pd *pd,tree *rep);

Perror_t tree_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,tree_acc *acc);

Perror_t tree_acc_report (P_t *pads,char const *prefix,char const *what,int nst,tree_acc *acc);

Perror_t tree_acc_report2xml_io (P_t *pads,Sfio_t *outstr,int nst,tree_acc *acc);

ssize_t tree_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,tree_pd *pd,tree *rep);

ssize_t tree_write2io (P_t *pads,Sfio_t *io,tree_pd *pd,tree *rep);

ssize_t tree_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,tree_pd *pd,tree *rep,char const *_tag,int indent);

ssize_t tree_write_xml_2io (P_t *pads,Sfio_t *io,tree_pd *pd,tree *rep,char const *_tag,int indent);

ssize_t tree_fmt2buf_final (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,tree_m *m,tree_pd *pd,tree *rep);

ssize_t tree_fmt2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,tree_m *m,tree_pd *pd,tree *rep);

ssize_t tree_fmt2io (P_t *pads,Sfio_t *io,int *requestedOut,char const *delims,tree_m *m,tree_pd *pd,tree *rep);

void P_lib_init ();

#endif /*  __BINARYTREE__H__  */
