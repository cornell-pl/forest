#ifdef _USE_PROTO
#pragma prototyped
#endif
/*
 * padc library interface -- private types : should be ignored by users of the library
 * 
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#ifndef __PADSC_PRIVATE_H__
#define __PADSC_PRIVATE_H__

/* private string state used to manage string memory */
#define PDC_STRING_PRIVATE_STATE \
  RBuf_t       *rbuf; \
  int          sharing

#define PDC_PRIVATE_DECLS typedef struct PDCI_stkElt_s PDCI_stkElt_t

/* PDC private state */
#define PDC_PRIVATE_STATE \
  Vmalloc_t        *vm;          /* vm handle */ \
  Sfio_t           *tmp1;        /* tmp sfprintf area 1 */ \
  Sfio_t           *tmp2;        /* tmp sfprintf area 2 */ \
  PDC_string        stmp1;       /* tmp string 1 */ \
  PDC_string        stmp2;       /* tmp string 2 */ \
  RMM_t            *rmm_z;       /* rbuf memory mgr -- zeroes allocated memory */  \
  RMM_t            *rmm_nz;      /* rbuf memory mgr -- does not zero allocated memory */  \
  PDC_endian        m_endian;    /* endian-ness of the machine */ \
  /* The following are used for write functions */ \
  Void_t           *outbuf;      /* tmp buf to use with sfio out stream (alloc outbuf_len + 1) */ \
  size_t            outbuf_len;  /* len of outbuf */ \
  size_t            outbuf_res;  /* space to reserve per top-level write call */ \
  /* The following are all related to IO state / checkpointing */ \
  char             *path;        /* original path -- eventually want to support a set of input files */ \
  Sfio_t           *io;          /* sfio stream */ \
  PDC_byte         *sfbuf;       /* buffer that is installed in any sfio that is opened */ \
  PDC_IO_elt_t     *head;        /* head of list of input elts */ \
  PDCI_stkElt_t    *stack;       /* stack - resized dynamically */ \
  size_t            salloc;      /* total elts allocated for stack */ \
  size_t            top;         /* index of top stack elt */ \
  unsigned int      speclev;     /* speculative nesting level */ \
  /* The following are related to nested internal calls */ \
  unsigned int      inestlev;    /* internal call nesting level */ \
  /* dummy used for error case */ \
  char              dummy[1]

/* ================================================================================ */
/* HELPER MACROS FOR SHARED STRING LIST */

#define PDC_SOME_SHSTR(elt) ((elt)->shstr_head->next != (elt)->shstr_head)
#define PDC_FIRST_SHSTR(elt) ((elt)->shstr_head->next)
#define PDC_LAST_SHSTR(elt)  ((elt)->shstr_head->prev)

#define PDC_REMOVE_SHSTR(s) do { \
  PDC_string *tmp = (s); \
  tmp->priv.prev->next = tmp->priv.next; \
  tmp->priv.next->prev = tmp->priv.prev; \
} while (0)

#define PDC_APPEND_SHSTR(elt, s) do { \
  PDC_string *head = (elt)->shstr_head; \
  PDC_string *tmp = (s); \
  tmp->priv.prev = head->priv.prev; \
  tmp->priv.next = head; \
  tmp->priv.prev->next = tmp; \
  tmp->priv.next->prev = tmp; \
} while (0)

#define PDC_PREPEND_SHSTR(elt, s) do { \
  PDC_string *head = (elt)->shstr_head; \
  PDC_string *tmp = (s); \
  tmp->priv.prev = head; \
  tmp->priv.next = head->priv.next; \
  tmp->priv.prev->next = tmp; \
  tmp->priv.next->prev = tmp; \
} while (0)

#endif  /*  __PADSC_PRIVATE_H__  */
