/*
 * Implementations of some standard IO disciplines
 *
 *   fwrec:        an IO discipline for data with fixed-width records.
 *
 *   ctrec:        an IO discipline for character-terminated variable-width records
 *
 *                   For ASCII newline-terminated records use, '\n' or PDC_ASCII_NEWLINE
 *                   as the term character.  for EBCDIC newline-terminated records, use
 *                   PDC_EBCDIC_NEWLINE as the term character.
 *
 *   vlrec:        an IO discipline for IBM style variable length records
 *                 with length specified at front
 *
 *   norec:        a raw bytes IO discipline that does not use EOR
 *
 *   fwrec_noseek: a version of fwrec that does not require that
 *                 the sfio stream be seekable
 *
 *   ctrec_noseek: a version of ctrec that does not require that
 *                 the sfio stream be seekable
 *
 *   vlrec_noseek: a version of vlrec that does not require that
 *                 the sfio stream be seekable
 *
 *   norec_noseek: a version of norec that does not require that
 *                 the sfio stream be seekable
 *
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#include "padsc-internal.h" /* XXX for debugging, should be padsc.h XXX */
#include "padsc-macros-gen.h"
#include "pdc_out_macros.h"
#include <stdio.h>

#define IODISC_NEED_AN_ELT(fn_name, elt, f_head, vm) \
  do { \
    if (PDC_SOME_ELTS(f_head)) { \
      (elt) = PDC_FIRST_ELT(f_head); \
      PDC_REMOVE_ELT(elt); \
    } else { \
      if (!((elt) = vmnewof((vm), 0, PDC_IO_elt_t, 1, 0))) { \
	PDC_WARN(pdc->disc, fn_name ": could not alloc space for input record"); \
      } \
    } \
  } while (0)

#define IODISC_NEED_AN_ELT_W_IODATA(fn_name, elt, f_head, vm, iodata_t, xtra_bytes) \
  do { \
    if (PDC_SOME_ELTS(f_head)) { \
      (elt) = PDC_FIRST_ELT(f_head); \
      PDC_REMOVE_ELT(elt); \
    } else { \
      if (!((elt) = vmnewof((vm), 0, PDC_IO_elt_t, 1, 0)) || \
	  !((elt)->disc_ptr = vmoldof((vm), 0, iodata_t, 1, xtra_bytes))) { \
	PDC_WARN(pdc->disc, fn_name ": could not alloc space for input record"); \
      } \
    } \
  } while (0)

#if 0
#define IODISC_RELOC_DBG(diff) \
  do { \
    if (pdc->disc->errorf) { \
      pdc->disc->errorf(NiL, 0, "XXX_REMOVE DATA SHIFTED BY %d BYTES", diff); \
    } \
  } while (0)
#else
#define IODISC_RELOC_DBG(diff)  PDC_NULL_STMT
#endif

#if 0
#define IODISC_RELOC_DBG_ELT(elt) \
  do { \
    if (pdc->disc->errorf) { \
      pdc->disc->errorf(NiL, 0, "XXX_REMOVE(%s %d disc_off %d) = AT NEW LOC =>\n[%s]", elt->unit, elt->num, elt->disc_off, PDC_fmt_Cstr(elt->begin, elt->len)); \
    } \
  } while (0)
#else
#define IODISC_RELOC_DBG_ELT(elt)          PDC_NULL_STMT
#endif

#define IODISC_RELOC_ELTS(fn_name, head, diff, tmpelt) \
  do { \
    IODISC_RELOC_DBG(diff); \
    for (tmpelt = PDC_FIRST_ELT(head); tmpelt != (head); tmpelt = tmpelt->next) { \
      (tmpelt->begin) += diff; \
      (tmpelt->end)   += diff; \
      IODISC_RELOC_DBG_ELT(tmpelt); \
    } \
  } while (0)

/* remove all elts from list -- put them on the free list */
#define IODISC_REMOVE_ALL_ELTS(head, f_head, tmpelt) \
  do { \
    while (PDC_SOME_ELTS(head)) { \
      tmpelt = PDC_FIRST_ELT(head); \
      PDC_REMOVE_ELT(tmpelt); \
      PDC_APPEND_ELT((f_head), tmpelt); \
    } \
  } while (0)

/* remove some elts from list -- put them on the free list */
#define IODISC_REMOVE_SOME_ELTS(head, f_head, keepelt, tmpelt) \
  do { \
    while (PDC_SOME_ELTS(head) && (tmpelt = PDC_FIRST_ELT(head)) != (keepelt)) { \
      PDC_REMOVE_ELT(tmpelt); \
      PDC_APPEND_ELT((f_head), tmpelt); \
    } \
  } while (0)

/* ================================================================================ */
/* PDC_fwrec_noseek IMPLEMENTATION */

/* private types */
typedef struct PDC_fwrec_noseek_data_s {
  /* configuration fields */
  size_t          leader_len;
  size_t          data_len;
  size_t          trailer_len;
  size_t          block_size; /* leader_len + data_len + trailer_len */
  /* other fields */
  Vmalloc_t      *disc_vm;    /* lifetime: make/unmake pairing */
  PDC_IO_elt_t   *head;       /* head of IO rec list */
  PDC_IO_elt_t   *f_head;     /* head of free list   */
  Sfio_t         *io;         /* Sfio stream to read from */
  int             eof;        /* hit EOF? */
  size_t          num;        /* unit number */
  char            unit[100];  /* unit description when reading blocks */
  char            punit[100]; /* ditto -- partial read case */
} PDC_fwrec_noseek_data_t;

typedef struct PDC_fwrec_noseek_iodata_s {
  size_t    eof_putback; /* non-zero only if partial block was read */
  PDC_byte  saved_byte;  /* we NULL byte after last data byte, use saved_byte to restore it */
  PDC_byte  dbuf[1];     /* actual size : block_size+1 */
} PDC_fwrec_noseek_iodata_t;

PDC_error_t
PDC_fwrec_noseek_sfopen(PDC_t *pdc, PDC_IO_disc_t* io_disc, Sfio_t *sfio, PDC_IO_elt_t *head)
{
  PDC_fwrec_noseek_data_t *data;

  if (!pdc || !pdc->disc) {
    return PDC_ERR;
  }
  if (!sfio || !head || !io_disc || !io_disc->data) {
    PDC_WARN(pdc->disc, "PDC_fwrec_noseek_sfopen: bad param(s)");
    return PDC_ERR;
  }
  if (head->next != head || head->prev != head) {
    PDC_WARN(pdc->disc, "PDC_fwrec_noseek_sfopen: head->next and head->prev must both point to head");
    return PDC_ERR;
  }
  data = (PDC_fwrec_noseek_data_t*)io_disc->data;
  if (data->io) {
    PDC_WARN(pdc->disc, "PDC_fwrec_noseek_sfopen: not in a valid closed state");
    return PDC_ERR;
  }
  data->io       = sfio;
  data->eof      = 0;
  data->head     = head;
  data->num      = 1;
  return PDC_OK;
}

PDC_error_t
PDC_fwrec_noseek_sfclose(PDC_t *pdc, PDC_IO_disc_t* io_disc, PDC_IO_elt_t *io_cur_elt, size_t remain)
{
  PDC_fwrec_noseek_data_t     *data;
  PDC_fwrec_noseek_iodata_t   *iodata;
  PDC_IO_elt_t                *elt;
  int                          c, ctr;
  PDC_byte                    *b, *bmin;

  if (!pdc || !pdc->disc) {
    return PDC_ERR;
  }
  if (!io_disc || !io_disc->data) {
    PDC_WARN(pdc->disc, "PDC_fwrec_noseek_sfclose: bad param(s)");
    return PDC_ERR;
  }
  data = (PDC_fwrec_noseek_data_t*)io_disc->data;
  if (!data->io) {
    PDC_WARN(pdc->disc, "PDC_fwrec_noseek_sfclose: not in a valid open state");
    return PDC_ERR;
  }
  if (io_cur_elt == data->head) {
    PDC_WARN(pdc->disc, "PDC_fwrec_noseek_sfclose: io_cur_elt == head not a valid elt!");
    return PDC_ERR;
  }
  if (io_cur_elt && remain > io_cur_elt->len) {
    PDC_WARN(pdc->disc, "PDC_fwrec_noseek_sfclose: remain > io_cur_elt->len!");
    return PDC_ERR;
  }
  if (io_cur_elt) {
    /* return remaining bytes to Sfio stream using sfungetc */
    ctr = 0;
    for (elt = PDC_LAST_ELT(data->head); (elt != io_cur_elt->prev) && (elt != data->head); elt = elt->prev) {
      iodata = (PDC_fwrec_noseek_iodata_t*)elt->disc_ptr;
      if (elt->eof) {
	/* eof elt, restore eof_putback bytes */
	bmin = iodata->dbuf;
	b = bmin + iodata->eof_putback - 1;
      } else {
	*(elt->end) = iodata->saved_byte;
	if (elt != io_cur_elt || elt->len == remain) {
	  /* restore all elt bytes, including leader and trailer bytes */
	  bmin = iodata->dbuf;
	  b = bmin + data->block_size - 1;
	} else {
	  /* elt is io_cur_elt and remain < elt->len; restore trailer plus remain only */
	  b = &(iodata->dbuf[data->block_size - 1]);
	  bmin = b - (remain + data->trailer_len);
	}
      }
      for (; b >= bmin; b--, ctr++) {
	c = *b;
	if (c != sfungetc(data->io, c)) {
	  PDC_WARN1(pdc->disc, "PDC_fwrec_noseek_sfclose: sfungetc failed, some bytes not restored -- restored %d bytes", ctr);
	  goto after_restore;
	}
      }
    }
    if (ctr) {
      PDC_WARN1(pdc->disc, "XXX_CHANGE_TO_DBG PDC_fwrec_noseek_sfclose: restored %d bytes using sfungetc", ctr);
    }
  }
 after_restore:

  /* remove all elts from list -- put them on the free list */
  IODISC_REMOVE_ALL_ELTS(data->head, data->f_head, elt);
  data->io      = 0;
  data->head    = 0; 
  return PDC_OK;
}

PDC_error_t
PDC_fwrec_noseek_read(PDC_t *pdc, PDC_IO_disc_t* io_disc, PDC_IO_elt_t *io_cur_elt, size_t remain, PDC_IO_elt_t** next_elt_out)
{
  PDC_fwrec_noseek_data_t     *data;
  PDC_fwrec_noseek_iodata_t   *iodata;
  PDC_IO_elt_t                *elt, *keepelt;
  ssize_t                      readlen;

  if (!pdc || !pdc->disc) {
    return PDC_ERR;
  }
  if (!io_disc || !io_disc->data || !next_elt_out) {
    PDC_WARN(pdc->disc, "PDC_fwrec_noseek_read: bad param(s)");
    return PDC_ERR;
  }
  (*next_elt_out) = 0;
  data = (PDC_fwrec_noseek_data_t*)io_disc->data;

  if (!data->io) {
    PDC_WARN(pdc->disc, "PDC_fwrec_noseek_read: not in a valid open state");
    return PDC_ERR;
  }
  if (data->eof) {
    PDC_WARN(pdc->disc, "PDC_fwrec_noseek_read: called after returning eof flag in previous call");
    return PDC_ERR;
  }
  if (io_cur_elt == data->head) {
    PDC_WARN(pdc->disc, "PDC_fwrec_noseek_read: io_cur_elt == head not a valid elt!");
    return PDC_ERR;
  }
  /* move some elts to the free list */
  keepelt = io_cur_elt ? io_cur_elt : data->head;
  IODISC_REMOVE_SOME_ELTS(data->head, data->f_head, keepelt, elt);
  IODISC_NEED_AN_ELT_W_IODATA("PDC_fwrec_noseek_read", elt, data->f_head, data->disc_vm, PDC_fwrec_noseek_iodata_t, data->block_size);
  if (!elt) {
    return PDC_ERR;
  }
  iodata = (PDC_fwrec_noseek_iodata_t*)elt->disc_ptr;
  readlen = sfread(data->io, iodata->dbuf, data->block_size);
  if (readlen < 0) {
    PDC_SYSERR(pdc->disc, "PDC_fwrec_noseek_read: Error reading IO stream");
    readlen = 0;
  }
  elt->num = (data->num)++;
  if (readlen < data->block_size) { /* EOF case */
    elt->eor = 0;
    elt->eof = 1;
    data->eof = 1;
    elt->len = 0; /* ignore partial data except for putback */
    elt->begin = elt->end = iodata->dbuf;
    iodata->eof_putback = readlen;
    elt->unit = "(EOF)";
  } else {
    elt->eor = 1;
    elt->eof = 0;
    elt->len = data->data_len;
    elt->begin = iodata->dbuf + data->leader_len;
    elt->end   = elt->begin + elt->len;
    iodata->eof_putback = 0;
    iodata->saved_byte = *(elt->end);
    elt->unit = (const char*)data->unit;
  }
  *(elt->end) = 0; /* null-terminate the bytes read */
  PDC_APPEND_ELT(data->head, elt);
#if 0
  if (pdc->disc->errorf) {
    pdc->disc->errorf(NiL, 0, "XXX_REMOVE(%s %d)\n[%s]", elt->unit, elt->num, PDC_fmt_Cstr(elt->begin, elt->len));
  }
#endif
  (*next_elt_out) = elt;
  return PDC_OK;
}

ssize_t
PDC_fwrec_noseek_rec_close(PDC_t *pdc, PDC_IO_disc_t* io_disc, PDC_byte *buf, PDC_byte *rec_start, size_t num_bytes)
{
  /* nothing to fill in */
  return 0;
}

ssize_t
PDC_fwrec_noseek_blk_close(PDC_t *pdc, PDC_IO_disc_t* io_disc, PDC_byte *buf, PDC_byte *blk_start, size_t num_bytes, PDC_uint32 num_recs)
{
  if (!pdc || !pdc->disc) {
    return -1;
  }
  PDC_WARN(pdc->disc, "PDC_fwrec_noseek_blk_close: not a record-block-based discipline!");
  return -1;
}

PDC_error_t
PDC_fwrec_noseek_unmake(PDC_t *pdc, PDC_IO_disc_t* io_disc)
{
  PDC_fwrec_noseek_data_t   *data;

  if (!pdc || !pdc->disc) {
    return PDC_ERR;
  }
  if (!io_disc) {
    PDC_WARN(pdc->disc, "PDC_fwrec_noseek_unmake: bad param(s)");
    return PDC_ERR;
  }
  data = (PDC_fwrec_noseek_data_t*)io_disc->data;
  if (data) {
    if (data->io) {
      PDC_WARN(pdc->disc, "PDC_fwrec_noseek_unmake: sfclose should have been called first, calling it now");
      PDC_fwrec_noseek_sfclose(pdc, io_disc, 0, 0);
    }
    if (data->disc_vm) {
      vmclose(data->disc_vm);
    }
  }
  return PDC_ERR;
}

PDC_IO_disc_t *
PDC_fwrec_noseek_make(size_t leader_len, size_t data_len, size_t trailer_len)
{
  Vmalloc_t                 *disc_vm = 0;
  PDC_fwrec_noseek_data_t   *data;
  PDC_IO_disc_t             *io_disc;
  PDC_IO_elt_t              *f_head;

  if (data_len < 1) {
    PDC_WARN(&PDC_default_disc, "PDC_fwrec_noseek_make: data_len must be > 0");
    return 0;
  }

  if (!(disc_vm = vmopen(Vmdcheap, Vmbest, 0))) {
    goto alloc_err;
  }
  if (!(io_disc = vmnewof(disc_vm, 0, PDC_IO_disc_t, 1, 0))) {
    goto alloc_err;
  }
  if (!(data = vmnewof(disc_vm, 0, PDC_fwrec_noseek_data_t, 1, 0))) {
    goto alloc_err;
  }
  if (!(f_head = vmnewof(disc_vm, 0, PDC_IO_elt_t, 1, 0))) {
    goto alloc_err;
  }

  f_head->prev = f_head;
  f_head->next = f_head;

  data->disc_vm       = disc_vm;
  data->leader_len    = leader_len;
  data->data_len      = data_len;
  data->trailer_len   = trailer_len;
  data->block_size    = leader_len + data_len + trailer_len;
  data->f_head        = f_head;

  if ((data->block_size % 1024) == 0) {
    sprintf(data->unit,  "%dKB Block", data->block_size / 1024);
  } else {
    sprintf(data->unit,  "%dB Block", data->block_size);
  }

  io_disc->unmake_fn    = PDC_fwrec_noseek_unmake;
  io_disc->sfopen_fn    = PDC_fwrec_noseek_sfopen;
  io_disc->sfclose_fn   = PDC_fwrec_noseek_sfclose;
  io_disc->read_fn      = PDC_fwrec_noseek_read;
  io_disc->rec_close_fn = PDC_fwrec_noseek_rec_close;
  io_disc->blk_close_fn = PDC_fwrec_noseek_blk_close;

  io_disc->name         = "fwrec_noseek";
  io_disc->descr        = "an IO discipline for data with fixed-width records";
  io_disc->rec_based    = 1;
  io_disc->has_rblks    = 0;
  io_disc->rec_obytes   = 0;
  io_disc->rec_cbytes   = 0;
  io_disc->blk_obytes   = 0;
  io_disc->blk_cbytes   = 0;
  io_disc->data         = data;

  return io_disc;

 alloc_err:
  PDC_WARN(&PDC_default_disc, "PDC_fwrec_noseek_make: out of space");
  if (disc_vm) {
    vmclose(disc_vm);
  }
  return 0;
}

/* ================================================================================ */
/* PDC_norec_noseek IMPLEMENTATION */

/* when not specified, use blocks of this size (bytes): */
#define PDC_NOREC_NOSEEK_DEF_BSIZE    512
/* initial block alloc */
#define PDC_NOREC_NOSEEK_INIT_BLOCKS   16
/* once we can free at least this many blocks, do so */
#define PDC_NOREC_NOSEEK_GC_AT_BLOCK    8
/* if growth is necessary, add this many blocks at a time */
#define PDC_NOREC_NOSEEK_ADD_BLOCKS     8

/* private types */
typedef struct PDC_norec_noseek_data_s {
  /* configuration fields */
  size_t          block_size;
  /* other fields */
  Vmalloc_t      *disc_vm;    /* lifetime: make/unmake pairing */
  PDC_IO_elt_t   *head;       /* head of IO rec list */
  PDC_IO_elt_t   *f_head;     /* head of free list   */
  Sfio_t         *io;         /* Sfio stream to read from */
  int             eof;        /* hit EOF? */
  size_t          num;        /* unit number */
  char            unit[100];  /* unit description when reading blocks */
  char            punit[100]; /* ditto -- partial read case */
  PDC_byte       *dbuf;       /* resizable data buffer */
  PDC_byte       *dbuf_end;   /* 1 beyond last byte read */
  size_t          balloc;     /* # blocks allocated */
  size_t          btail;      /* idx of last block in use */
  size_t          gc_point;
} PDC_norec_noseek_data_t;

PDC_error_t
PDC_norec_noseek_sfopen(PDC_t *pdc, PDC_IO_disc_t* io_disc, Sfio_t* sfio, PDC_IO_elt_t* head)
{
  PDC_norec_noseek_data_t *data;

  if (!pdc || !pdc->disc) {
      return PDC_ERR;
  }
  if (!sfio || !head || !io_disc || !io_disc->data) {
    PDC_WARN(pdc->disc, "PDC_norec_noseek_sfopen: bad param(s)");
    return PDC_ERR;
  }
  if (head->next != head || head->prev != head) {
    PDC_WARN(pdc->disc, "PDC_norec_noseek_sfopen: head->next and head->prev must both point to head");
    return PDC_ERR;
  }
  data = (PDC_norec_noseek_data_t*)io_disc->data;
  if (data->io) {
    PDC_WARN(pdc->disc, "PDC_norec_noseek_sfopen: not in a valid closed state");
    return PDC_ERR;
  }

  data->io       = sfio;
  data->eof      = 0;
  data->head     = head;
  data->num      = 1;
  data->btail    = 0;
  data->dbuf_end = data->dbuf;

  return PDC_OK;
}

PDC_error_t
PDC_norec_noseek_sfclose(PDC_t *pdc, PDC_IO_disc_t* io_disc, PDC_IO_elt_t *io_cur_elt, size_t remain)
{
  PDC_norec_noseek_data_t  *data;
  PDC_IO_elt_t             *elt;
  int                       c, ctr;
  PDC_byte                 *b, *bmin;

  if (!pdc || !pdc->disc) {
    return PDC_ERR;
  }
  if (!io_disc || !io_disc->data) {
    PDC_WARN(pdc->disc, "PDC_norec_noseek_sfclose: bad param(s)");
    return PDC_ERR;
  }
  data = (PDC_norec_noseek_data_t*)io_disc->data;
  if (!data->io) {
    PDC_WARN(pdc->disc, "PDC_norec_noseek_sfclose: not in a valid open state");
    return PDC_ERR;
  }
  if (io_cur_elt == data->head) {
    PDC_WARN(pdc->disc, "PDC_norec_noseek_sfclose: io_cur_elt == head not a valid elt!");
    return PDC_ERR;
  }
  if (io_cur_elt && remain > io_cur_elt->len) {
    PDC_WARN(pdc->disc, "PDC_norec_noseek_sfclose: remain > io_cur_elt->len!");
    return PDC_ERR;
  }
  if (io_cur_elt) {
    /* return remaining bytes to Sfio stream using sfungetc */
    ctr = 0;
    b = data->dbuf_end - 1;
    bmin = io_cur_elt->end - remain;
    for (; b >= bmin; b--, ctr++) {
      c = *b;
      if (c != sfungetc(data->io, c)) {
	PDC_WARN1(pdc->disc, "PDC_norec_noseek_sfclose: sfungetc failed, some bytes not restored -- restored %d bytes", ctr);
	goto after_restore;
      }
    }
    if (ctr) {
      PDC_WARN1(pdc->disc, "XXX_CHANGE_TO_DBG PDC_norec_noseek_sfclose: restored %d bytes using sfungetc", ctr);
    }
  }
 after_restore:

  /* remove all elts from list -- put them on the free list */
  IODISC_REMOVE_ALL_ELTS(data->head, data->f_head, elt);
  data->io      = 0;
  data->head    = 0; 

  return PDC_OK;
}

PDC_error_t
PDC_norec_noseek_read(PDC_t *pdc, PDC_IO_disc_t* io_disc, PDC_IO_elt_t *io_cur_elt, size_t remain, PDC_IO_elt_t** next_elt_out)
{
  PDC_norec_noseek_data_t  *data;
  PDC_IO_elt_t             *elt, *keepelt;
  size_t                    keep_len;
  ssize_t                   readlen;
  Sfoff_t                   diff;

  if (!pdc || !pdc->disc) {
    return PDC_ERR;
  }
  if (!io_disc || !io_disc->data || !next_elt_out) {
    PDC_WARN(pdc->disc, "PDC_norec_noseek_read: bad param(s)");
    return PDC_ERR;
  }
  (*next_elt_out) = 0;
  data = (PDC_norec_noseek_data_t*)io_disc->data;
  if (!data->io) {
    PDC_WARN(pdc->disc, "PDC_norec_noseek_read: not in a valid open state");
    return PDC_ERR;
  }
  if (data->eof) {
    PDC_WARN(pdc->disc, "PDC_norec_noseek_read: called after returning EOF flag in previous call");
    return PDC_ERR;
  }
  if (io_cur_elt == data->head) {
    PDC_WARN(pdc->disc, "PDC_norec_noseek_read: io_cur_elt == head not a valid elt!");
    return PDC_ERR;
  }
  /* move some elts to the free list */
  keepelt = io_cur_elt ? io_cur_elt : data->head;
  IODISC_REMOVE_SOME_ELTS(data->head, data->f_head, keepelt, elt);

  if (!io_cur_elt) {
    data->btail = 0; /* starting from scratch */
    data->dbuf_end = data->dbuf;
  } else {
    /* choose or alloc block to use */
    if (++(data->btail) >= data->balloc) {
      /* need space */
      if (io_cur_elt->begin - data->dbuf >= data->gc_point) {
	/* garbage collect */
	diff = data->dbuf - io_cur_elt->begin; /* data moving down - negative offset */
	keep_len = data->dbuf_end - io_cur_elt->begin;
	memmove((void*)data->dbuf, (const void*)io_cur_elt->begin, keep_len);
	data->btail = (keep_len/data->block_size); /* # blocks copied => correct block index for next read */
	data->dbuf_end = data->dbuf + keep_len;
      } else {
	/* grow dbuf */
	PDC_byte *dbuf_next;
	size_t balloc_next = data->balloc + PDC_NOREC_NOSEEK_ADD_BLOCKS;
	keep_len = data->dbuf_end - data->dbuf;
	if (!(dbuf_next = vmcpyoldof(data->disc_vm, data->dbuf, PDC_byte, balloc_next * data->block_size, 1))) {
	  PDC_WARN(pdc->disc, "PDC_norec_noseek_read: could not alloc space for input record");
	  (data->btail)--;
	  return PDC_ERR;
	}
	diff           = dbuf_next - data->dbuf;
	data->balloc   = balloc_next;
	data->dbuf     = dbuf_next;
	data->dbuf_end = dbuf_next + keep_len;
	data->gc_point += (PDC_NOREC_NOSEEK_ADD_BLOCKS * data->block_size);
      }
      if (diff) { /* data moved, redo begin/end pointers */
	IODISC_RELOC_ELTS("PDC_norec_noseek_read", data->head, diff, elt);
      }
    } else {
      /* dbuf already had space at block btail */
    }
  }

  IODISC_NEED_AN_ELT("PDC_norec_noseek_read", elt, data->f_head, data->disc_vm);
  if (!elt) {
    return PDC_ERR;
  }

  elt->eor = 0; /* norec_noseek never uses eor */
  elt->begin = data->dbuf_end;
  readlen = sfread(data->io, elt->begin, data->block_size);
  if (readlen < 0) {
    PDC_SYSERR(pdc->disc, "PDC_norec_noseek_read: Error reading IO stream");
    readlen = 0;
  }
  data->dbuf_end += readlen;
  *(data->dbuf_end) = 0; /* null-terminate dbuf -- note use of extra byte in vmoldof calls */
  elt->len = readlen;
  elt->end = data->dbuf_end;
  elt->num = (data->num)++;
  if (readlen == data->block_size) { /* full read */
    elt->eof = 0;
    elt->unit = (const char*)data->unit;
  } else { /* partial read, hit EOF */
    elt->eof = 1;
    data->eof = 1;
    if (elt->len == 0) { /* trivial EOF record */
      elt->unit = "(EOF)";
    } else { /* partial-read EOF record */
      elt->unit = (const char*)data->punit;
    }
  }
  PDC_APPEND_ELT(data->head, elt);
#if 0
  if (pdc->disc->errorf) {
    pdc->disc->errorf(NiL, 0, "XXX_REMOVE(%s %d)\n[%s]", elt->unit, elt->num, PDC_fmt_Cstr(elt->begin, elt->len));
  }
#endif
  (*next_elt_out) = elt;
  return PDC_OK;
}

ssize_t
PDC_norec_noseek_rec_close(PDC_t *pdc, PDC_IO_disc_t* io_disc, PDC_byte *buf, PDC_byte *rec_start, size_t num_bytes)
{
  if (!pdc || !pdc->disc) {
    return -1;
  }
  PDC_WARN(pdc->disc, "PDC_norec_noseek_rec_close: not a record-based discipline!");
  return -1;
}

ssize_t
PDC_norec_noseek_blk_close(PDC_t *pdc, PDC_IO_disc_t* io_disc, PDC_byte *buf, PDC_byte *blk_start, size_t num_bytes, PDC_uint32 num_recs)
{
  if (!pdc || !pdc->disc) {
    return -1;
  }
  PDC_WARN(pdc->disc, "PDC_norec_noseek_blk_close: not a record-block-based discipline!");
  return -1;
}

PDC_error_t
PDC_norec_noseek_unmake(PDC_t *pdc, PDC_IO_disc_t* io_disc)
{
  PDC_norec_noseek_data_t   *data;

  if (!pdc | !pdc->disc) {
    return PDC_ERR;
  }
  if (!io_disc) {
    PDC_WARN(pdc->disc, "PDC_norec_noseek_unmake: bad param(s)");
    return PDC_ERR;
  }
  data = (PDC_norec_noseek_data_t*)io_disc->data;
  if (data) {
    if (data->io) {
      PDC_WARN(pdc->disc, "PDC_norec_noseek_unmake: sfclose should have been called first, calling it now");
      PDC_norec_noseek_sfclose(pdc, io_disc, 0, 0);
    }
    if (data->disc_vm) {
      vmclose(data->disc_vm);
    }
  }
  return PDC_ERR;
}

PDC_IO_disc_t *
PDC_norec_noseek_make(size_t block_size_hint)
{
  Vmalloc_t                 *disc_vm = 0;
  PDC_norec_noseek_data_t   *data;
  PDC_IO_disc_t             *io_disc;
  PDC_IO_elt_t              *f_head;
  PDC_byte                  *dbuf;
  size_t                     block_size;

  block_size = (block_size_hint) ? block_size_hint : PDC_NOREC_NOSEEK_DEF_BSIZE;

  if (!(disc_vm = vmopen(Vmdcheap, Vmbest, 0))) {
    goto alloc_err;
  }
  if (!(io_disc = vmnewof(disc_vm, 0, PDC_IO_disc_t, 1, 0))) {
    goto alloc_err;
  }
  if (!(data = vmnewof(disc_vm, 0, PDC_norec_noseek_data_t, 1, 0))) {
    goto alloc_err;
  }
  if (!(f_head = vmnewof(disc_vm, 0, PDC_IO_elt_t, 1, 0))) {
    goto alloc_err;
  }
  if (!(dbuf = vmoldof(disc_vm, 0, PDC_byte, PDC_NOREC_NOSEEK_INIT_BLOCKS * block_size, 1))) {
    goto alloc_err;
  }

  f_head->prev = f_head;
  f_head->next = f_head;

  data->disc_vm       = disc_vm;
  data->block_size    = block_size;
  data->f_head        = f_head;
  data->dbuf          = dbuf;
  data->gc_point      = PDC_NOREC_NOSEEK_GC_AT_BLOCK * block_size;
  data->balloc        = PDC_NOREC_NOSEEK_INIT_BLOCKS;

  if ((block_size % 1024) == 0) {
    sprintf(data->unit,  "%dKB Block", block_size / 1024);
    sprintf(data->punit, "(last: partial) %dKB Block", block_size / 1024);
  } else {
    sprintf(data->unit,  "%dB Block", block_size);
    sprintf(data->punit, "(last: partial) %dB Block", block_size);
  }

  io_disc->unmake_fn    = PDC_norec_noseek_unmake;
  io_disc->sfopen_fn    = PDC_norec_noseek_sfopen;
  io_disc->sfclose_fn   = PDC_norec_noseek_sfclose;
  io_disc->read_fn      = PDC_norec_noseek_read;
  io_disc->rec_close_fn = PDC_norec_noseek_rec_close;
  io_disc->blk_close_fn = PDC_norec_noseek_blk_close;

  io_disc->name         = "norec_noseek";
  io_disc->descr        = "a raw bytes IO discipline that does not use EOR";
  io_disc->rec_based    = 0;
  io_disc->has_rblks    = 0;
  io_disc->rec_obytes   = 0;
  io_disc->rec_cbytes   = 0;
  io_disc->blk_obytes   = 0;
  io_disc->blk_cbytes   = 0;
  io_disc->data         = data;

  return io_disc;

 alloc_err:
  PDC_WARN(&PDC_default_disc, "PDC_norec_noseek_make: out of space");
  if (disc_vm) {
    vmclose(disc_vm);
  }
  return 0;
}

/* ================================================================================ */
/* PDC_ctrec_noseek IMPLEMENTATION */

/* when not specified, use blocks of this size (bytes): */
#define PDC_CTREC_NOSEEK_DEF_BSIZE     512
/* initial block alloc */
#define PDC_CTREC_NOSEEK_INIT_BLOCKS    16
/* once we can free at least this many blocks, do so */
#define PDC_CTREC_NOSEEK_GC_AT_BLOCK     8
/* but only check if there are <= this many blocks left */
#define PDC_CTREC_NOSEEK_GC_BLOCKS_LEFT  4
/* if growth is necessary, add this many blocks at a time */
#define PDC_CTREC_NOSEEK_ADD_BLOCKS      8

/* private types */
typedef struct PDC_ctrec_noseek_data_s {
  /* configuration fields */
  PDC_byte        cterm;
  size_t          block_size;
  /* other fields */
  Vmalloc_t      *disc_vm;     /* lifetime: make/unmake pairing */
  PDC_IO_elt_t   *head;        /* head of IO rec list */
  PDC_IO_elt_t   *f_head;      /* head of free list   */
  PDC_IO_elt_t   *eof_elt;     /* always keep around an elt to be used as EOF elt */
  Sfio_t         *io;          /* Sfio stream to read from */
  int             eof;         /* hit EOF? */
  size_t          num;         /* unit number */
  PDC_byte       *dbuf;        /* resizable data buffer */
  PDC_byte       *dbuf_end;    /* 1 beyond last byte read */ 
  size_t          un_bytes;    /* unread bytes: # bytes not yet part of IO rec list */
  size_t          balloc;      /* # blocks allocated */
  size_t          btail;       /* idx of last block in use */
  size_t          gc_point;
  char            descr[100];  /* description */ 
} PDC_ctrec_noseek_data_t;

PDC_error_t
PDC_ctrec_noseek_sfopen(PDC_t *pdc, PDC_IO_disc_t* io_disc, Sfio_t* sfio, PDC_IO_elt_t* head)
{
  PDC_ctrec_noseek_data_t  *data;
  PDC_IO_elt_t             *elt; 

  if (!pdc || !pdc->disc) {
    return PDC_ERR;
  }
  if (!sfio || !head || !io_disc || !io_disc->data) {
    PDC_WARN(pdc->disc, "PDC_ctrec_noseek_sfopen: bad param(s)");
    return PDC_ERR;
  }
  if (head->next != head || head->prev != head) {
    PDC_WARN(pdc->disc, "PDC_ctrec_noseek_sfopen: head->next and head->prev must both point to head");
    return PDC_ERR;
  }
  data = (PDC_ctrec_noseek_data_t*)io_disc->data;
  if (data->io) {
    PDC_WARN(pdc->disc, "PDC_ctrec_noseek_sfopen: not in a valid closed state");
    return PDC_ERR;
  }
  IODISC_NEED_AN_ELT("PDC_ctrec_noseek_sfopen", elt, data->f_head, data->disc_vm);
  if (!elt) {
    return PDC_ERR;
  }

  data->io           = sfio;
  data->eof          = 0;
  data->eof_elt      = elt;
  data->head         = head;
  data->num          = 1;
  data->btail        = 0;
  data->dbuf_end     = data->dbuf;
  data->un_bytes     = 0;

  return PDC_OK;
}

PDC_error_t
PDC_ctrec_noseek_sfclose(PDC_t *pdc, PDC_IO_disc_t* io_disc, PDC_IO_elt_t *io_cur_elt, size_t remain)
{
  PDC_ctrec_noseek_data_t  *data;
  PDC_IO_elt_t             *elt;
  int                       c, ctr;
  PDC_byte                 *b, *bmin;

  if (!pdc || !pdc->disc) {
    return PDC_ERR;
  }
  if (!io_disc || !io_disc->data) {
    PDC_WARN(pdc->disc, "PDC_ctrec_noseek_sfclose: bad param(s)");
    return PDC_ERR;
  }
  data = (PDC_ctrec_noseek_data_t*)io_disc->data;
  if (!data->io) {
    PDC_WARN(pdc->disc, "PDC_ctrec_noseek_sfclose: not in a valid open state");
    return PDC_ERR;
  }
  if (io_cur_elt == data->head) {
    PDC_WARN(pdc->disc, "PDC_ctrec_noseek_sfclose: io_cur_elt == head not a valid elt!");
    return PDC_ERR;
  }
  if (io_cur_elt && remain > io_cur_elt->len) {
    PDC_WARN(pdc->disc, "PDC_ctrec_noseek_sfclose: remain > io_cur_elt->len!");
    return PDC_ERR;
  }
  if (io_cur_elt) {
    /* return remaining bytes to Sfio stream using sfungetc */
    ctr = 0;
    b = data->dbuf_end - 1;
    bmin = io_cur_elt->end - remain;
    for (; b >= bmin; b--, ctr++) {
      c = *b;
      if (c != sfungetc(data->io, c)) {
	PDC_WARN1(pdc->disc, "PDC_ctrec_noseek_sfclose: sfungetc failed, some bytes not restored -- restored %d bytes", ctr);
	goto after_restore;
      }
    }
    if (ctr) {
      PDC_WARN1(pdc->disc, "XXX_CHANGE_TO_DBG PDC_ctrec_noseek_sfclose: restored %d bytes using sfungetc", ctr);
    }
  }
 after_restore:

  /* remove all elts from list -- put them on the free list */
  IODISC_REMOVE_ALL_ELTS(data->head, data->f_head, elt);
  if (data->eof_elt) {
    elt = data->eof_elt;
    data->eof_elt = 0;
    PDC_APPEND_ELT(data->f_head, elt);
  }

  data->io      = 0;
  data->head    = 0; 

  return PDC_OK;
}

PDC_error_t
PDC_ctrec_noseek_read(PDC_t *pdc, PDC_IO_disc_t* io_disc, PDC_IO_elt_t *io_cur_elt, size_t remain, PDC_IO_elt_t** next_elt_out)
{
  PDC_ctrec_noseek_data_t   *data;
  PDC_IO_elt_t              *elt, *keepelt;
  ssize_t                    readlen, keep_len, discard_len, bytes_read;
  PDC_byte                  *tmp;
  PDC_byte                  *found_cterm;
  Sfoff_t                    diff;

  if (!pdc || !pdc->disc) {
    return PDC_ERR;
  }
  if (!io_disc || !io_disc->data || !next_elt_out) {
    PDC_WARN(pdc->disc, "PDC_ctrec_noseek_read: bad param(s)");
    return PDC_ERR;
  }
  (*next_elt_out) = 0;
  data = (PDC_ctrec_noseek_data_t*)io_disc->data;

  if (!data->io) {
    PDC_WARN(pdc->disc, "PDC_ctrec_noseek_read: not in a valid open state");
    return PDC_ERR;
  }
  if (data->eof) {
    PDC_WARN(pdc->disc, "PDC_ctrec_noseek_read: called after returning EOF flag in previous call");
    return PDC_ERR;
  }
  if (io_cur_elt == data->head) {
    PDC_WARN(pdc->disc, "PDC_ctrec_noseek_read: io_cur_elt == head not a valid elt!");
    return PDC_ERR;
  }
  keepelt = io_cur_elt ? io_cur_elt : data->head;
  /* move some elts to the free list */
  IODISC_REMOVE_SOME_ELTS(data->head, data->f_head, keepelt, elt);
  /*
   * garbage collect if <= PDC_CTREC_NOSEEK_GC_BLOCKS_LEFT and there is
   * a good chunk of data to be discarded
   */
  if (io_cur_elt) {
    keep_len = data->dbuf_end - io_cur_elt->begin;
    if (keep_len % data->block_size) { /* round up to nearest block size */
      keep_len += (data->block_size - (keep_len % data->block_size));
    }
  } else if (data->un_bytes) {
    keep_len = data->block_size;
  } else {
    keep_len = 0;
  }
  discard_len = (data->dbuf_end - data->dbuf) - keep_len;
  if (keep_len) {
    if (data->balloc - data->btail < PDC_CTREC_NOSEEK_GC_BLOCKS_LEFT && discard_len >= data->gc_point) {
      /* garbage collect */
      diff = - discard_len; /* data moving down - negative offset */
      memmove((void*)data->dbuf, (const void*)(data->dbuf_end - keep_len), keep_len);
      data->btail = keep_len/data->block_size; /* # blocks copied -> index of next block index */
      data->dbuf_end = data->dbuf + keep_len;
      /* redo begin/end ptrs */
      IODISC_RELOC_ELTS("PDC_ctrec_noseek_read", data->head, diff, elt);
      *(data->dbuf_end) = 0; /* null-terminate dbuf */
    } /* keep everything -- postpone the memmove */
  } else {
    /* starting from scratch, no io_cur_elt or unread bytes */
    data->btail = 0;
    data->dbuf_end = data->dbuf;
    *(data->dbuf_end) = 0; /* null-terminate dbuf */
  }

  bytes_read    = 0;
  found_cterm = 0;
  while (1) { /* read blocks until find cterm or EOF */
    /* choose or alloc block to use */
    if (data->btail >= data->balloc) {
      /* need space -- grow dbuf */
      PDC_byte *dbuf_next;
      size_t balloc_next = data->balloc + PDC_CTREC_NOSEEK_ADD_BLOCKS;
      keep_len = data->dbuf_end - data->dbuf;
      if (!(dbuf_next = vmcpyoldof(data->disc_vm, data->dbuf, PDC_byte, balloc_next * data->block_size, 1))) {
	PDC_WARN(pdc->disc, "PDC_ctrec_noseek_read: could not alloc space for input record");
	readlen = 0;
	break; /* continue after while to take care of earlier bytes_read, if any */
      }
      diff           = dbuf_next - data->dbuf;
      data->balloc   = balloc_next;
      data->dbuf     = dbuf_next;
      data->dbuf_end = dbuf_next + keep_len;
      data->gc_point += (PDC_CTREC_NOSEEK_ADD_BLOCKS * data->block_size);
      if (diff) { /* data moved, redo begin/end pointers */
	IODISC_RELOC_ELTS("PDC_ctrec_noseek_read", data->head, diff, elt);
      }
    } else {
      /* dbuf already had space at block index btail */
    }
    readlen = sfread(data->io, data->dbuf_end, data->block_size);
    if (readlen < 0) {
      PDC_SYSERR(pdc->disc, "PDC_ctrec_noseek_read: Error reading IO stream");
      readlen = 0;
      break;
    }
    if (readlen == 0) {
      break;
    }
    (data->btail)++;
    bytes_read += readlen;
    tmp = data->dbuf_end;
    data->dbuf_end += readlen;
    *(data->dbuf_end) = 0; /* null-terminate dbuf -- note use of extra byte in vmoldof calls */
    if ((found_cterm = PDCI_findfirst(tmp, data->dbuf_end, data->cterm))) {
      break;
    }
    /* read another block */
  }
  /* bytes_read reflects all reads, readlen reflects last read */
  data->un_bytes += bytes_read;
  tmp = data->dbuf_end - data->un_bytes;
  /* first add any EOR-based records */
  while (found_cterm) {
    IODISC_NEED_AN_ELT("PDC_ctrec_noseek_read", elt, data->f_head, data->disc_vm);
    if (!elt) { /* turn this error into an EOF case */
      PDC_WARN(pdc->disc, "PDC_ctrec_noseek_read: internal memory alloc error, entering EOF state");
      readlen = 0;
      break;
    }
    elt->begin     = tmp;
    elt->end       = found_cterm;
    elt->len       = found_cterm - tmp;
    elt->eor       = 1;
    elt->eof       = 0;
    elt->num       = (data->num)++;
    elt->unit      = "record";
    data->un_bytes -= (elt->len + 1); /* acount for cterm */
    elt->begin[elt->len] = 0; /* null-terminate the record, replaces cterm with NULL */
    PDC_APPEND_ELT(data->head, elt);
#if 0
    if (pdc->disc->errorf) {
      pdc->disc->errorf(NiL, 0, "XXX_REMOVE(%s %d)\n[%s]", elt->unit, elt->num, PDC_fmt_Cstr(elt->begin, elt->len));
    }
#endif
    if (!(*next_elt_out)) {
      (*next_elt_out) = elt;
    }
    tmp = data->dbuf_end - data->un_bytes;
    found_cterm = PDCI_findfirst(tmp, data->dbuf_end, data->cterm);
  }
  if (readlen < data->block_size) { /* put rest of bytes in EOF IO rec */
    elt = data->eof_elt;
    data->eof_elt  = 0;
    elt->eor       = 0;
    elt->eof       = 1;
    data->eof      = 1;
    elt->begin     = tmp;
    elt->end       = data->dbuf_end;
    elt->len       = data->un_bytes;
    data->un_bytes = 0;
    elt->num  = (data->num)++;
    if (elt->len == 0) { /* trivial EOF record */
      elt->unit = "(EOF)";
    } else { /* partial-read EOF record */
      if (data->cterm == '\n') {
	elt->unit = "(partial: missing newline) record";
      } else {
	elt->unit = "(partial: missing record terminator) record";
      }
    }
    elt->begin[elt->len] = 0; /* null-terminate the record */
    PDC_APPEND_ELT(data->head, elt);
#if 0
    if (pdc->disc->errorf) {
      pdc->disc->errorf(NiL, 0, "XXX_REMOVE(%s %d)\n[%s]", elt->unit, elt->num, PDC_fmt_Cstr(elt->begin, elt->len));
    }
#endif
    if (!(*next_elt_out)) {
      (*next_elt_out) = elt;
    }
  } /* else found cterm in a full block read */
  return PDC_OK;
}

ssize_t
PDC_ctrec_noseek_rec_close(PDC_t *pdc, PDC_IO_disc_t* io_disc, PDC_byte *buf, PDC_byte *rec_start, size_t num_bytes)
{
  PDC_ctrec_noseek_data_t  *data;

  if (!pdc || !pdc->disc) {
    return -1;
  }
  if (!io_disc || !io_disc->data || !buf || !rec_start) {
    PDC_WARN(pdc->disc, "PDC_ctrec_noseek_rec_close: bad param(s)");
    return -1;
  }
  data = (PDC_ctrec_noseek_data_t*)io_disc->data;
  *buf = data->cterm;
  return 1;
}

ssize_t
PDC_ctrec_noseek_blk_close(PDC_t *pdc, PDC_IO_disc_t* io_disc, PDC_byte *buf, PDC_byte *blk_start, size_t num_bytes, PDC_uint32 num_recs)
{
  if (!pdc || !pdc->disc) {
    return -1;
  }
  PDC_WARN(pdc->disc, "PDC_ctrec_noseek_blk_close: not a record-block-based discipline!");
  return -1;
}

PDC_error_t
PDC_ctrec_noseek_unmake(PDC_t *pdc, PDC_IO_disc_t* io_disc)
{
  PDC_ctrec_noseek_data_t  *data;

  if (!pdc || !pdc->disc) {
    return PDC_ERR;
  }
  data = (PDC_ctrec_noseek_data_t*)io_disc->data;
  if (data) {
    if (data->io) {
      PDC_WARN(pdc->disc, "PDC_ctrec_noseek_unmake: sfclose should have been called first, calling it now");
      PDC_ctrec_noseek_sfclose(pdc, io_disc, 0, 0);
    }
    if (data->disc_vm) {
      vmclose(data->disc_vm);
    }
  }
  return PDC_ERR;
}

PDC_IO_disc_t *
PDC_ctrec_noseek_make(PDC_byte termChar, size_t block_size_hint)
{
  Vmalloc_t                 *disc_vm = 0;
  PDC_ctrec_noseek_data_t   *data;
  PDC_IO_disc_t             *io_disc;
  PDC_IO_elt_t              *f_head;
  PDC_byte                  *dbuf;
  size_t                     block_size;

  block_size = (block_size_hint) ? block_size_hint : PDC_CTREC_NOSEEK_DEF_BSIZE;

  if (!(disc_vm = vmopen(Vmdcheap, Vmbest, 0))) {
    goto alloc_err;
  }
  if (!(io_disc = vmnewof(disc_vm, 0, PDC_IO_disc_t, 1, 0))) {
    goto alloc_err;
  }
  if (!(data = vmnewof(disc_vm, 0, PDC_ctrec_noseek_data_t, 1, 0))) {
    goto alloc_err;
  }
  if (!(f_head = vmnewof(disc_vm, 0, PDC_IO_elt_t, 1, 0))) {
    goto alloc_err;
  }
  if (!(dbuf = vmoldof(disc_vm, 0, PDC_byte, PDC_CTREC_NOSEEK_INIT_BLOCKS * block_size, 1))) {
    goto alloc_err;
  }

  f_head->prev = f_head;
  f_head->next = f_head;

  data->disc_vm       = disc_vm;
  data->cterm         = termChar;
  data->block_size    = block_size;
  data->f_head        = f_head;
  data->dbuf          = dbuf;
  data->gc_point      = PDC_CTREC_NOSEEK_GC_AT_BLOCK * block_size;
  data->balloc        = PDC_CTREC_NOSEEK_INIT_BLOCKS;
  sprintf(data->descr, "an IO discipline for variable-width records terminated by char %d (ASCII char %s)",
	  termChar, PDC_qfmt_char(termChar));

  io_disc->unmake_fn    = PDC_ctrec_noseek_unmake;
  io_disc->sfopen_fn    = PDC_ctrec_noseek_sfopen;
  io_disc->sfclose_fn   = PDC_ctrec_noseek_sfclose;
  io_disc->read_fn      = PDC_ctrec_noseek_read;
  io_disc->rec_close_fn = PDC_ctrec_noseek_rec_close;
  io_disc->blk_close_fn = PDC_ctrec_noseek_blk_close;

  io_disc->name         = "ctrec_noseek";
  io_disc->descr        = data->descr;
  io_disc->rec_based    = 1;
  io_disc->has_rblks    = 0;
  io_disc->rec_obytes   = 0;
  io_disc->rec_cbytes   = 1;
  io_disc->blk_obytes   = 0;
  io_disc->blk_cbytes   = 0;
  io_disc->data         = data;

  return io_disc;

 alloc_err:
  PDC_WARN(&PDC_default_disc, "PDC_ctrec_noseek_make: out of space");
  if (disc_vm) {
    vmclose(disc_vm);
  }
  return 0;
}

/* ================================================================================ */
/* PDC_vlrec_noseek IMPLEMENTATION */

/* default expected avg rlen, must be > 4 */
#define PDC_VLREC_NOSEEK_DEF_AVG_RLEN 512
/* multiply expected avg rlen by the following to get intial allocation */
#define PDC_VLREC_NOSEEK_INIT_RECS 8
/* multiply expected avg rlen by the following to get growth increment */
#define PDC_VLREC_NOSEEK_GROW_RECS 8
/* once we can free at least this pcnt of bytes, do so (specify pcnt using 1--100) */
#define PDC_VLREC_NOSEEK_GC_PCNT 25

/* private types */
typedef struct PDC_vlrec_noseek_data_s {
  /* configuration fields */
  int             blocked;     /* are records grouped into blocks? */
  size_t          rlen_hint;   /* if set, a running avg is not computed */
  /* other fields */
  Vmalloc_t      *disc_vm;     /* lifetime: make/unmake pairing */
  PDC_IO_elt_t   *head;        /* head of IO rec list */
  PDC_IO_elt_t   *f_head;      /* head of free list   */
  PDC_IO_elt_t   *eof_elt;     /* always keep around an elt to be used as EOF elt */
  Sfio_t         *io;          /* Sfio stream to read from */
  int             eof;         /* hit EOF? */
  size_t          num;         /* unit number */
  PDC_byte       *dbuf;        /* resizable data buffer */
  PDC_byte       *dbuf_end;    /* 1 beyond last byte read */ 
  size_t          dbuf_alloc;  /* current size of dbuf */
  size_t          gc_point;    /* always == (dbuf_alloc * PDC_VLREC_NOSEEK_GC_PCNT)/100 */
  PDC_uint32_acc  acc;         /* used to accumulate record lengths */
  PDC_base_pd     pd;          /* used with acc */
  size_t          avg_rlen;    /* latest computed record length avg */
  size_t          blk_expect;  /* total bytes in current block (expected) -- only used if blocked is set */
  size_t          blk_seen;    /* bytes read in current block -- only used if blocked is set */
  char            descr[100];  /* description */ 
} PDC_vlrec_noseek_data_t;

PDC_error_t
PDC_vlrec_noseek_sfopen(PDC_t *pdc, PDC_IO_disc_t* io_disc, Sfio_t* sfio, PDC_IO_elt_t* head)
{
  PDC_vlrec_noseek_data_t  *data;
  PDC_IO_elt_t             *elt; 

  if (!pdc || !pdc->disc) {
    return PDC_ERR;
  }
  if (!sfio || !head || !io_disc || !io_disc->data) {
    PDC_WARN(pdc->disc, "PDC_vlrec_noseek_sfopen: bad param(s)");
    return PDC_ERR;
  }
  if (head->next != head || head->prev != head) {
    PDC_WARN(pdc->disc, "PDC_vlrec_noseek_sfopen: head->next and head->prev must both point to head");
    return PDC_ERR;
  }
  data = (PDC_vlrec_noseek_data_t*)io_disc->data;
  if (data->io) {
    PDC_WARN(pdc->disc, "PDC_vlrec_noseek_sfopen: not in a valid closed state");
    return PDC_ERR;
  }
  IODISC_NEED_AN_ELT("PDC_vlrec_noseek_sfopen", elt, data->f_head, data->disc_vm);
  if (!elt) {
    return PDC_ERR;
  }

  data->io           = sfio;
  data->eof          = 0;
  data->eof_elt      = elt;
  data->head         = head;
  data->num          = 1;
  data->dbuf_end     = data->dbuf;
  data->blk_expect   = (data->blocked) ? 0 : 1;
  /* the combination blocked == 0, blk_expect != 0 is sufficient to suppress all block processing */
  /* if blocked is set, we set blk_expect to zero to force the block length to be read */

  if (data->rlen_hint) {
    data->avg_rlen = data->rlen_hint;
  } else {
    data->avg_rlen = PDC_VLREC_NOSEEK_DEF_AVG_RLEN;
    PDC_uint32_acc_init(pdc, &(data->acc));
  }
  return PDC_OK;
}

PDC_error_t
PDC_vlrec_noseek_sfclose(PDC_t *pdc, PDC_IO_disc_t* io_disc, PDC_IO_elt_t *io_cur_elt, size_t remain)
{
  PDC_vlrec_noseek_data_t  *data;
  PDC_IO_elt_t             *elt;
  int                       c, ctr;
  PDC_byte                 *b, *bmin;

  if (!pdc || !pdc->disc) {
    return PDC_ERR;
  }
  if (!io_disc || !io_disc->data) {
    PDC_WARN(pdc->disc, "PDC_vlrec_noseek_sfclose: bad param(s)");
    return PDC_ERR;
  }
  data = (PDC_vlrec_noseek_data_t*)io_disc->data;
  if (!data->io) {
    PDC_WARN(pdc->disc, "PDC_vlrec_noseek_sfclose: not in a valid open state");
    return PDC_ERR;
  }
  if (io_cur_elt == data->head) {
    PDC_WARN(pdc->disc, "PDC_vlrec_noseek_sfclose: io_cur_elt == head not a valid elt!");
    return PDC_ERR;
  }
  if (io_cur_elt && remain > io_cur_elt->len) {
    PDC_WARN(pdc->disc, "PDC_vlrec_noseek_sfclose: remain > io_cur_elt->len!");
    return PDC_ERR;
  }
  if (io_cur_elt) {
    /* return remaining bytes to Sfio stream using sfungetc */
    ctr = 0;
    b = data->dbuf_end - 1;
    bmin = io_cur_elt->end - remain;
    for (; b >= bmin; b--, ctr++) {
      c = *b;
      if (c != sfungetc(data->io, c)) {
	PDC_WARN1(pdc->disc, "PDC_vlrec_noseek_sfclose: sfungetc failed, some bytes not restored -- restored %d bytes", ctr);
	goto after_restore;
      }
    }
    if (ctr) {
      PDC_WARN1(pdc->disc, "XXX_CHANGE_TO_DBG PDC_vlrec_noseek_sfclose: restored %d bytes using sfungetc", ctr);
    }
  }
 after_restore:

  /* remove all elts from list -- put them on the free list */
  IODISC_REMOVE_ALL_ELTS(data->head, data->f_head, elt);
  if (data->eof_elt) {
    elt = data->eof_elt;
    data->eof_elt = 0;
    PDC_APPEND_ELT(data->f_head, elt);
  }

  data->io      = 0;
  data->head    = 0; 
#if 0
  PDC_uint32_acc_report (pdc, "XXX_REMOVE_vlrec_noseek.record_len", "PDC_uint32", 0, &(data->acc));
#endif
  PDC_uint32_acc_cleanup(pdc, &(data->acc));

  return PDC_OK;
}

PDC_error_t
PDC_vlrec_noseek_read(PDC_t *pdc, PDC_IO_disc_t* io_disc, PDC_IO_elt_t *io_cur_elt, size_t remain, PDC_IO_elt_t** next_elt_out)
{
  PDC_vlrec_noseek_data_t    *data;
  PDC_IO_elt_t               *elt, *keepelt;
  ssize_t                     readlen, keep_len, discard_len;
  size_t                      record_len, xtra;
  Sfoff_t                     diff;

  if (!pdc || !pdc->disc) {
    return PDC_ERR;
  }
  if (!io_disc || !io_disc->data || !next_elt_out) {
    PDC_WARN(pdc->disc, "PDC_vlrec_noseek_read: bad param(s)");
    return PDC_ERR;
  }
  (*next_elt_out) = 0;
  data = (PDC_vlrec_noseek_data_t*)io_disc->data;

  if (!data->io) {
    PDC_WARN(pdc->disc, "PDC_vlrec_noseek_read: not in a valid open state");
    return PDC_ERR;
  }
  if (data->eof) {
    PDC_WARN(pdc->disc, "PDC_vlrec_noseek_read: called after returning EOF flag in previous call");
    return PDC_ERR;
  }
  if (io_cur_elt == data->head) {
    PDC_WARN(pdc->disc, "PDC_vlrec_noseek_read: io_cur_elt == head not a valid elt!");
    return PDC_ERR;
  }
  keepelt = io_cur_elt ? io_cur_elt : data->head;
  /* move some elts to the free list */
  IODISC_REMOVE_SOME_ELTS(data->head, data->f_head, keepelt, elt);
  /*
   * garbage collect if there is a good chunk of data to be discarded
   */
  if (io_cur_elt) {
    keep_len = data->dbuf_end - io_cur_elt->begin;
  } else {
    keep_len = 0;
  }
  if (keep_len == 0) {
    /* starting from scratch, no io_cur_elt or unread bytes */
    data->dbuf_end = data->dbuf;
    *(data->dbuf_end) = 0; /* null-terminate dbuf */
  } else {
    discard_len = (data->dbuf_end - data->dbuf) - keep_len;
    if (discard_len > data->gc_point) {
      /* garbage collect */
      diff = - discard_len; /* data moving down - negative offset */
      memmove((void*)data->dbuf, (const void*)(data->dbuf_end - keep_len), keep_len);
      data->dbuf_end = data->dbuf + keep_len;
      /* redo begin/end ptrs */
      IODISC_RELOC_ELTS("PDC_vlrec_noseek_read", data->head, diff, elt);
      *(data->dbuf_end) = 0; /* null-terminate dbuf */
    }
  }

  /* set extra to 4 if it is time to read a new block length, else 0 */
  xtra = (data->blk_expect == 0) ? 4 : 0;

  /* grow dbuf if there are fewer than avg_rlen bytes left (note always > 4) */
  readlen = 0;
  diff = data->dbuf_end - data->dbuf;
  if (data->dbuf_alloc - diff < data->avg_rlen + xtra) {
    /* grow with room for more than one record */
    PDC_byte *dbuf_next; 
    size_t    dbuf_alloc_next = data->dbuf_alloc + (data->avg_rlen * PDC_VLREC_NOSEEK_GROW_RECS) + xtra + 1;
    if (!(dbuf_next = vmcpyoldof(data->disc_vm, data->dbuf, PDC_byte, dbuf_alloc_next, 1))) {
      PDC_WARN(pdc->disc, "PDC_vlrec_noseek_read: could not alloc space for input record");
      goto eof_case;
    }
#if 0
    PDC_WARN2(pdc->disc, "XXX_REMOVE grew dbuf from %lu to %lu",
	      (unsigned long)data->dbuf_alloc, (unsigned long)dbuf_alloc_next);
#endif
    data->dbuf_alloc = dbuf_alloc_next;
    data->gc_point   = (data->dbuf_alloc * PDC_VLREC_NOSEEK_GC_PCNT) / 100;
    diff = dbuf_next - data->dbuf;
    if (diff) {
      keep_len         = data->dbuf_end - data->dbuf;
      data->dbuf       = dbuf_next;
      data->dbuf_end   = dbuf_next + keep_len;
      IODISC_RELOC_ELTS("PDC_vlrec_noseek_read", data->head, diff, elt);
    }
  }

  /* read 4 (or 8) bytes to determine record_len, goto eof_case if not enough bytes */
  readlen = sfread(data->io, data->dbuf_end, 4+xtra);
  if (readlen != 4+xtra) {
    if (readlen < 0) {
      PDC_SYSERR(pdc->disc, "PDC_vlrec_noseek_read: Error reading IO stream, entering EOF state");
      readlen = 0;
    } else if (readlen > 0) {
      PDC_WARN(pdc->disc, "PDC_vlrec_noseek_read: bytes remaining < record length field, entering EOF state");
    }
    goto eof_case;
  }

  if (xtra) {
    /* initialize blk_expect and blk_seen */
    data->blk_expect = (data->dbuf_end[0] << 8) + data->dbuf_end[1]; /* third and fourth bytes ignored */
    /* XXX should only do this if some paranoid flag is set */
    if (data->dbuf_end[2] || data->dbuf_end[3]) {
      PDC_WARN(pdc->disc, "PDC_vlrec_noseek_read: block length field has non-null bytes 3/4, ignoring");
    }
    /*    PDC_WARN1(pdc->disc, "XXX_REMOVE starting new block of size %lu", (unsigned long)data->blk_expect); */
    data->dbuf_end += 4;
    data->blk_seen = 4;
    if (data->blk_expect < 8) {
      PDC_WARN(pdc->disc, "PDC_vlrec_noseek_read: Invalid block length, entering EOF state");
      goto eof_case;
    }
  }

  record_len = (data->dbuf_end[0] << 8) + data->dbuf_end[1]; /* third and fourth bytes ignored */
  /* XXX should only do this if some paranoid flag is set */
  if (data->dbuf_end[2] || data->dbuf_end[3]) {
    PDC_WARN(pdc->disc, "PDC_vlrec_noseek_read: record length field has non-null bytes 3/4, ignoring");
  }
  /* XXX should only do this if some paranoid flag is set */
  if (record_len > PDC_MAX_INT16) {
    PDC_WARN2(pdc->disc, "PDC_vlrec_noseek_read: record_len %ld is greater than %ld as established by IBM",
	      (long)record_len, (long)PDC_MAX_INT16);
  }
  data->dbuf_end += 4;
  if (data->rlen_hint == 0) {
    PDC_uint32 ui32 = record_len + 5;
    PDC_uint32_acc_add(pdc, &(data->acc), &(data->pd), &ui32);
    if (data->num % 10 == 0) {
      data->avg_rlen = PDC_uint32_acc_ravg(pdc, &(data->acc));
      /*  PDC_WARN1(pdc->disc, "XXX_REMOVE changed avg_rlen to %ld", (long)data->avg_rlen); */
    }
  }
  if (record_len < 4) {
    PDC_WARN(pdc->disc, "PDC_vlrec_noseek_read: Invalid record length, entering EOF state");
    goto eof_case;
  }
  record_len -= 4; /* record_len is now == bytes remaining to be read */

  /* grow dbuf if there are fewer than (record_len + 1) bytes left */
  readlen = 0;
  diff = data->dbuf_end - data->dbuf;
  if (data->dbuf_alloc - diff < record_len + 1) {
    /* grow with room for more than one record */
    PDC_byte *dbuf_next; 
    size_t    dbuf_alloc_next = data->dbuf_alloc + record_len + (data->avg_rlen * PDC_VLREC_NOSEEK_GROW_RECS) + 1;
    if (!(dbuf_next = vmcpyoldof(data->disc_vm, data->dbuf, PDC_byte, dbuf_alloc_next, 1))) {
      PDC_WARN(pdc->disc, "PDC_vlrec_noseek_read: could not alloc space for input record");
      goto eof_case;
    }
#if 0
    PDC_WARN2(pdc->disc, "XXX_REMOVE grew dbuf from %lu to %lu", data->dbuf_alloc, dbuf_alloc_next);
#endif
    data->dbuf_alloc = dbuf_alloc_next;
    data->gc_point   = (data->dbuf_alloc * PDC_VLREC_NOSEEK_GC_PCNT) / 100;
    diff = dbuf_next - data->dbuf;
    if (diff) {
      keep_len         = data->dbuf_end - data->dbuf;
      data->dbuf       = dbuf_next;
      data->dbuf_end   = dbuf_next + keep_len;
      IODISC_RELOC_ELTS("PDC_vlrec_noseek_read", data->head, diff, elt);
    }
  }

  /* read record_len bytes, goto eof_case if not enough bytes */
  readlen = sfread(data->io, data->dbuf_end, record_len);
  if (readlen != record_len) {
    if (readlen < 0) {
      PDC_SYSERR(pdc->disc, "PDC_vlrec_noseek_read: Error reading IO stream");
      readlen = 0;
    } else if (readlen > 0) {
      PDC_WARN(pdc->disc, "PDC_vlrec_noseek_read: bytes remaining smaller than record length");
    }
    goto eof_case;
  }

  if (data->blocked) {
    data->blk_seen += (readlen+4);
    if (data->blk_seen > data->blk_expect) {
#if 0
      PDC_WARN2(pdc->disc, "XXX_REMOVE seen: %lu  expect:  %lu",
		(unsigned long)data->blk_seen, (unsigned long)data->blk_expect);
#endif
      PDC_WARN(pdc->disc, "PDC_vlrec_noseek_read: length of records in block exceeds block length, entering EOF state");
      goto eof_case;
    }
    if (data->blk_seen == data->blk_expect) {
      /* PDC_WARN(pdc->disc, "XXX_REMOVE finished reading a block"); */
      data->blk_expect = 0; /* force next block length to be read */
    }
  }

  /* make an elt using the bytes just read */
  IODISC_NEED_AN_ELT("PDC_vlrec_noseek_read", elt, data->f_head, data->disc_vm);
  if (!elt) { /* turn this error into an EOF case */
    PDC_WARN(pdc->disc, "PDC_vlrec_noseek_read: internal memory alloc error, entering EOF state");
    goto eof_case;
  }
  elt->begin     = data->dbuf_end;
  data->dbuf_end += readlen;
  elt->end       = data->dbuf_end;
  elt->len       = readlen;
  elt->eor       = 1;
  elt->eof       = 0;
  elt->unit      = "record";
  goto done;

 eof_case:
  elt = data->eof_elt;
  data->eof_elt     = 0;
  data->dbuf_end += readlen;
  elt->begin        = data->dbuf_end;
  elt->end          = data->dbuf_end;
  elt->len          = 0; /* ignore readlen except for putback (captured in dbuf_end) */
  elt->eor          = 0;
  elt->eof          = 1;
  data->eof         = 1;
  elt->unit         = "(EOF)";

 done:
  elt->num             = (data->num)++;
  elt->begin[elt->len] = 0; /* null-terminate the record */
  PDC_APPEND_ELT(data->head, elt);
#if 0
  if (pdc->disc->errorf) {
    pdc->disc->errorf(NiL, 0, "XXX_REMOVE(%s %d)\n[%s]", elt->unit, elt->num, PDC_fmt_Cstr(elt->begin, elt->len));
  }
#endif
  (*next_elt_out) = elt;
  return PDC_OK;
}

ssize_t
PDC_vlrec_noseek_rec_close(PDC_t *pdc, PDC_IO_disc_t* io_disc, PDC_byte *buf, PDC_byte *rec_start, size_t num_bytes)
{
  PDC_byte *ibytes;

  if (!pdc || !pdc->disc) {
    return -1;
  }
  if (!io_disc || !buf || !rec_start) {
    PDC_WARN(pdc->disc, "PDC_vlrec_noseek_rec_close: bad param(s)");
    return -1;
  }
  /* num_bytes already equal to total bytes in record */
  ibytes = (PDC_byte*)(&num_bytes);
  if (pdc->m_endian == PDC_littleEndian) {
    rec_start[0] = ibytes[1];
    rec_start[1] = ibytes[0]; 
    rec_start[2] = 0;
    rec_start[3] = 0;
  } else {
    rec_start[0] = ibytes[2];
    rec_start[1] = ibytes[3];
    rec_start[2] = 0;
    rec_start[3] = 0;
  }
  return 0; /* no bytes added at end */
}

ssize_t
PDC_vlrec_noseek_blk_close(PDC_t *pdc, PDC_IO_disc_t* io_disc, PDC_byte *buf, PDC_byte *blk_start, size_t num_bytes, PDC_uint32 num_recs)
{
  PDC_byte *ibytes;

  if (!pdc || !pdc->disc) {
    return -1;
  }
  if (!io_disc || !io_disc->data) {
    PDC_WARN(pdc->disc, "PDC_vlrec_noseek_blk_close: bad param(s)");
    return -1;
  }
  /* num_bytes already equal to total bytes in block */
  ibytes = (PDC_byte*)(&num_bytes);
  if (pdc->m_endian == PDC_littleEndian) {
    blk_start[0] = ibytes[1];
    blk_start[1] = ibytes[0];
  } else {
    blk_start[0] = ibytes[0];
    blk_start[1] = ibytes[1];
  }
  blk_start[2] = 0;
  blk_start[3] = 0;
  return 0; /* no bytes added at end */
}

PDC_error_t
PDC_vlrec_noseek_unmake(PDC_t *pdc, PDC_IO_disc_t* io_disc)
{
  PDC_vlrec_noseek_data_t  *data;

  if (!pdc || !pdc->disc) {
    return PDC_ERR;
  }
  data = (PDC_vlrec_noseek_data_t*)io_disc->data;
  if (data) {
    if (data->io) {
      PDC_WARN(pdc->disc, "PDC_vlrec_noseek_unmake: sfclose should have been called first, calling it now");
      PDC_vlrec_noseek_sfclose(pdc, io_disc, 0, 0);
    }
    if (data->disc_vm) {
      vmclose(data->disc_vm);
    }
  }
  return PDC_ERR;
}

PDC_IO_disc_t *
PDC_vlrec_noseek_make(int blocked, size_t avg_rlen_hint)
{
  Vmalloc_t                 *disc_vm = 0;
  PDC_vlrec_noseek_data_t   *data;
  PDC_IO_disc_t             *io_disc;
  PDC_IO_elt_t              *f_head;
  PDC_byte                  *dbuf;
  size_t                     dbuf_alloc;
  size_t                     rlen;

  if (avg_rlen_hint > 0 && avg_rlen_hint < 5) {
    PDC_WARN(&PDC_default_disc, "PDC_vlrec_noseek_make: avg_rlen_hint is > 0 but less than min of 5; changing to 5");
    avg_rlen_hint = 5;
  }
  rlen = (avg_rlen_hint) ? avg_rlen_hint : PDC_VLREC_NOSEEK_DEF_AVG_RLEN;

  if (!(disc_vm = vmopen(Vmdcheap, Vmbest, 0))) {
    goto alloc_err;
  }
  if (!(io_disc = vmnewof(disc_vm, 0, PDC_IO_disc_t, 1, 0))) {
    goto alloc_err;
  }
  if (!(data = vmnewof(disc_vm, 0, PDC_vlrec_noseek_data_t, 1, 0))) {
    goto alloc_err;
  }
  if (!(f_head = vmnewof(disc_vm, 0, PDC_IO_elt_t, 1, 0))) {
    goto alloc_err;
  }
  dbuf_alloc = PDC_VLREC_NOSEEK_INIT_RECS * rlen;
  if (!(dbuf = vmoldof(disc_vm, 0, PDC_byte, dbuf_alloc, 1))) {
    goto alloc_err;
  }

  f_head->prev = f_head;
  f_head->next = f_head;

  data->disc_vm       = disc_vm;
  data->blocked       = blocked;
  data->rlen_hint     = avg_rlen_hint;
  data->f_head        = f_head;
  data->dbuf          = dbuf;
  data->dbuf_alloc    = dbuf_alloc;
  data->gc_point      = (data->dbuf_alloc * PDC_VLREC_NOSEEK_GC_PCNT) / 100;
  sprintf(data->descr, "an IO discipline for IBM-stle variable-length records");

  io_disc->unmake_fn    = PDC_vlrec_noseek_unmake;
  io_disc->sfopen_fn    = PDC_vlrec_noseek_sfopen;
  io_disc->sfclose_fn   = PDC_vlrec_noseek_sfclose;
  io_disc->read_fn      = PDC_vlrec_noseek_read;
  io_disc->rec_close_fn = PDC_vlrec_noseek_rec_close;
  io_disc->blk_close_fn = PDC_vlrec_noseek_blk_close;

  io_disc->name         = "vlrec_noseek";
  io_disc->descr        = data->descr;
  io_disc->rec_based    = 1;
  io_disc->has_rblks    = 1;
  io_disc->rec_obytes   = 4;
  io_disc->rec_cbytes   = 0;
  io_disc->blk_obytes   = 4;
  io_disc->blk_cbytes   = 0;
  io_disc->data         = data;

  /* data->pd starts zeroed due to vmnewof */

  return io_disc;

 alloc_err:
  PDC_WARN(&PDC_default_disc, "PDC_vlrec_noseek_make: out of space");
  if (disc_vm) {
    vmclose(disc_vm);
  }
  return 0;
}

/* ================================================================================ */
/* PDC_fwrec IMPLEMENTATION */

PDC_IO_disc_t *
PDC_fwrec_make(size_t leader_len, size_t data_len, size_t trailer_len)
{
  /* XXX_TODO */
  return PDC_fwrec_noseek_make(leader_len, data_len, trailer_len);
}

/* ================================================================================ */
/* PDC_ctrec IMPLEMENTATION */

PDC_IO_disc_t *
PDC_ctrec_make(PDC_byte termChar, size_t block_size_hint)
{
  /* XXX_TODO */
  return PDC_ctrec_noseek_make(termChar, block_size_hint);
}

/* ================================================================================ */
/* PDC_vlrec IMPLEMENTATION */

PDC_IO_disc_t *
PDC_vlrec_make(int blocked, size_t avg_rlen_hint)
{
  /* XXX_TODO */
  return PDC_vlrec_noseek_make(blocked, avg_rlen_hint);
}

/* ================================================================================ */
/* PDC_norec IMPLEMENTATION */

/* when not specified, use blocks of this size (bytes): */
#define PDC_NOREC_DEF_BSIZE    512

/* private types */
typedef struct PDC_norec_data_s {
  /* configuration fields */
  size_t          block_size;
  /* other fields */
  Vmalloc_t      *disc_vm;    /* lifetime: make/unmake pairing */
  PDC_IO_elt_t   *head;       /* head of IO rec list */
  PDC_IO_elt_t   *f_head;     /* head of free list   */
  PDC_IO_elt_t   *eof_elt;    /* always keep around an elt to be used as EOF elt */
  Sfio_t         *io;         /* Sfio stream to read from */
  int             eof;        /* hit EOF? */
  size_t          num;        /* unit number */
  PDC_byte        dummy[1];   /* for immediate EOF error */
  char            unit[100];  /* unit description when reading blocks */
  char            punit[100]; /* ditto -- partial read case */
  PDC_byte       *r_begin;    /* begin of reserved sfio bytes */
  PDC_byte       *r_end;      /* 1 byte past end of reserved sfio bytes */
  PDC_byte        saved_byte; /* we NULL *(r_end) after each sfreserve, restore it prior to next sfreserve */
  Sfoff_t         tail_off;   /* tail offset (obtained after each sfreserve call) */
} PDC_norec_data_t;

PDC_error_t
PDC_norec_sfopen(PDC_t *pdc, PDC_IO_disc_t* io_disc, Sfio_t *sfio, PDC_IO_elt_t *head)
{
  PDC_norec_data_t *data;
  PDC_IO_elt_t      *elt; 

  if (!pdc || !pdc->disc) {
    return PDC_ERR;
  }
  if (!sfio || !head || !io_disc || !io_disc->data) {
    PDC_WARN(pdc->disc, "PDC_norec_sfopen: bad param(s)");
    return PDC_ERR;
  }
  if (head->next != head || head->prev != head) {
    PDC_WARN(pdc->disc, "PDC_norec_sfopen: head->next and head->prev must both point to head");
    return PDC_ERR;
  }
  data = (PDC_norec_data_t*)io_disc->data;
  if (data->io) {
    PDC_WARN(pdc->disc, "PDC_norec_sfopen: not in a valid closed state");
    return PDC_ERR;
  }
  IODISC_NEED_AN_ELT("PDC_norec_sfopen", elt, data->f_head, data->disc_vm);
  if (!elt) {
    return PDC_ERR;
  }

  data->io       = sfio;
  data->eof      = 0;
  data->eof_elt  = elt;
  data->head     = head;
  data->num      = 1;
  data->tail_off = sftell(sfio);
  data->r_begin  = 0;
  data->r_end    = 0;

  return PDC_OK;
}

PDC_error_t
PDC_norec_sfclose(PDC_t *pdc, PDC_IO_disc_t* io_disc, PDC_IO_elt_t *io_cur_elt, size_t remain)
{
  PDC_norec_data_t   *data;
  PDC_IO_elt_t       *elt;

  if (!pdc || !pdc->disc) {
    return PDC_ERR;
  }
  if (!io_disc || !io_disc->data) {
    PDC_WARN(pdc->disc, "PDC_norec_sfclose: bad param(s)");
    return PDC_ERR;
  }
  data = (PDC_norec_data_t*)io_disc->data;
  if (!data->io) {
    PDC_WARN(pdc->disc, "PDC_norec_sfclose: not in a valid open state");
    return PDC_ERR;
  }
  if (io_cur_elt == data->head) {
    PDC_WARN(pdc->disc, "PDC_norec_sfclose: io_cur_elt == head not a valid elt!");
    return PDC_ERR;
  }
  if (io_cur_elt && remain > io_cur_elt->len) {
    PDC_WARN(pdc->disc, "PDC_norec_sfclose: remain > io_cur_elt->len!");
    return PDC_ERR;
  }
  if (io_cur_elt) {
    Sfoff_t offset;

    if (data->r_end) { /* restore in case we are returning saved_byte to stream */ 
      *(data->r_end) = data->saved_byte;
    }
    offset = io_cur_elt->disc_off + io_cur_elt->len - remain;
    if (offset < data->tail_off) {
      unsigned long to_return = data->tail_off - offset;
      if (-1 == sfseek(data->io, offset, 0)) {
	PDC_WARN1(pdc->disc, "PDC_norec_sfclose: failed to return %lu bytes to IO stream", to_return);
      } else {
	PDC_WARN1(pdc->disc, "XXX_CHANGE_TO_DBG PDC_norec_sfclose: restored %lu bytes to IO stream using sfseek", to_return);
      }
    }
  }

  /* remove all elts from list -- put them on the free list */
  IODISC_REMOVE_ALL_ELTS(data->head, data->f_head, elt);
  if (data->eof_elt) {
    elt = data->eof_elt;
    data->eof_elt = 0;
    PDC_APPEND_ELT(data->f_head, elt);
  }

  data->io      = 0;
  data->head    = 0; 

  return PDC_OK;
}

PDC_error_t
PDC_norec_read(PDC_t *pdc, PDC_IO_disc_t* io_disc, PDC_IO_elt_t *io_cur_elt, size_t remain, PDC_IO_elt_t **next_elt_out)
{
  PDC_norec_data_t    *data;
  PDC_IO_elt_t        *elt, *firstelt, *keepelt;
  ssize_t              readlen, to_keep, to_discard, to_reserve;
  Sfoff_t              new_data_off, diff;
  PDC_byte            *new_r_begin, *new_data;

  if (!pdc || !pdc->disc) {
    return PDC_ERR;
  }
  if (!io_disc || !io_disc->data || !next_elt_out) {
    PDC_WARN(pdc->disc, "PDC_norec_read: bad param(s)");
    return PDC_ERR;
  }
  (*next_elt_out) = 0;
  data = (PDC_norec_data_t*)io_disc->data;

  if (!data->io) {
    PDC_WARN(pdc->disc, "PDC_norec_read: not in a valid open state");
    return PDC_ERR;
  }
  if (data->eof) {
    PDC_WARN(pdc->disc, "PDC_norec_read: called after returning EOF flag in previous call");
    return PDC_ERR;
  }
  if (io_cur_elt == data->head) {
    PDC_WARN(pdc->disc, "PDC_norec_read: io_cur_elt == head not a valid elt!");
    return PDC_ERR;
  }
  if (data->r_end) {
    *(data->r_end) = data->saved_byte;
  }
  /* move some elts to the free list */
  keepelt = io_cur_elt ? io_cur_elt : data->head;
  IODISC_REMOVE_SOME_ELTS(data->head, data->f_head, keepelt, elt);

  firstelt = PDC_FIRST_ELT(data->head);
  if (firstelt == data->head) {
    to_keep = 0;
  } else {
    /* seek to offset of first elt */
    if (-1 == sfseek(data->io, firstelt->disc_off, 0)) {
      PDC_FATAL(pdc->disc, "PDC_norec_read: unexpected sfseek failure");
    }
    to_keep = data->tail_off - firstelt->disc_off;
  }
  new_data_off = data->tail_off;
  to_discard = (data->r_end - data->r_begin) - to_keep;
  to_reserve = to_keep + data->block_size;
  new_r_begin = sfreserve(data->io, to_reserve, 0);
  readlen = sfvalue(data->io);
  if (readlen > to_reserve) { /* hint from sfio that we could have reserved more -- ignore */
    readlen = to_reserve;
  }
  if (!new_r_begin && readlen) {
    if (!(new_r_begin = sfreserve(data->io, 0, SF_LASTR))) {
      /*
       * For some reason if data->io was created from a string, we need to use
       * the following to get the final bytes (SF_LASTR is not working).
       */
      if (!(new_r_begin = sfreserve(data->io, readlen, 0))) {
	readlen = 0; 
      }
    }
  }
  data->tail_off = sftell(data->io);
  data->r_end = 0;
  if (readlen) {
    data->r_end = new_r_begin + readlen;
    if (readlen < to_keep) {
      PDC_FATAL(pdc->disc, "PDC_norec_read: unexpected failure, reserve got even fewer bytes than last time");
    }
    readlen -= to_keep; /* readlen now == the bytes that go in the new elt(s) */
    if (data->r_begin) {
      data->r_begin += to_discard; /* need this adjustment before we compute diff */
      diff = new_r_begin - data->r_begin;
      if (diff) { /* data moved, redo begin/end pointers */
	IODISC_RELOC_ELTS("PDC_norec_read", data->head, diff, elt);
      }
    }
    data->r_begin = new_r_begin;
  }
  if (data->r_end) {
    data->saved_byte = *(data->r_end);
    *(data->r_end) = 0; /* null-terminate the data bytes -- note sfio's sfbuf must be allocated with extra byte */
  }
  if (readlen == data->block_size) { /* full block read */
    IODISC_NEED_AN_ELT("PDC_norec_read", elt, data->f_head, data->disc_vm);
    if (!elt) {
      PDC_WARN(pdc->disc, "PDC_norec_read memory alloc error, entering EOF state");
      goto eof_case;
    }
    new_data  = data->r_begin + to_keep; /* new bytes start here */
    elt->eof  = 0;
    elt->unit = (const char*)data->unit;
    goto done;
  }

 eof_case:
  if (data->r_begin) {
    new_data = data->r_begin + to_keep; /* new bytes start here */
  } else {
    new_data = data->dummy; /* no bytes ever read before EOF, need a dummy location */ 
  }
  elt = data->eof_elt;
  data->eof_elt = 0;
  elt->eof  = 1;
  data->eof = 1;
  if (readlen == data->block_size) {
    elt->unit = (const char*)data->unit;
  } else if (readlen) {
    elt->unit = (const char*)data->punit;
  } else {
    elt->unit = "(EOF)";
  }

 done:
  elt->eor = 0;
  elt->num = (data->num)++;
  elt->begin = new_data;
  elt->end   = new_data + readlen;
  elt->len   = readlen;
  elt->disc_off = new_data_off;
  PDC_APPEND_ELT(data->head, elt);
#if 0
  if (pdc->disc->errorf) {
    pdc->disc->errorf(NiL, 0, "XXX_REMOVE(%s %d disc_off %d)\n[%s]", elt->unit, elt->num, elt->disc_off, PDC_fmt_Cstr(elt->begin, elt->len));
  }
#endif
  (*next_elt_out) = elt;
  return PDC_OK;
}

ssize_t
PDC_norec_rec_close(PDC_t *pdc, PDC_IO_disc_t* io_disc, PDC_byte *buf, PDC_byte *rec_start, size_t num_bytes)
{
  if (!pdc || !pdc->disc) {
    return -1;
  }
  PDC_WARN(pdc->disc, "PDC_norec_rec_close: not a record-based discipline!");
  return -1;
}

ssize_t
PDC_norec_blk_close(PDC_t *pdc, PDC_IO_disc_t* io_disc, PDC_byte *buf, PDC_byte *blk_start, size_t num_bytes, PDC_uint32 num_recs)
{
  if (!pdc || !pdc->disc) {
    return -1;
  }
  PDC_WARN(pdc->disc, "PDC_norec_blk_close: not a record-block-based discipline!");
  return -1;
}

PDC_error_t
PDC_norec_unmake(PDC_t *pdc, PDC_IO_disc_t* io_disc)
{
  PDC_norec_data_t  *data;

  if (!pdc || !pdc->disc) {
    return PDC_ERR;
  }
  data = (PDC_norec_data_t*)io_disc->data;
  if (data) {
    if (data->io) {
      PDC_WARN(pdc->disc, "PDC_norec_unmake: sfclose should have been called first, calling it now");
      PDC_norec_sfclose(pdc, io_disc, 0, 0);
    }
    if (data->disc_vm) {
      vmclose(data->disc_vm);
    }
  }
  return PDC_ERR;
}

PDC_IO_disc_t *
PDC_norec_make(size_t block_size_hint)
{
  Vmalloc_t          *disc_vm = 0;
  PDC_norec_data_t   *data;
  PDC_IO_disc_t      *io_disc;
  PDC_IO_elt_t       *f_head;
  size_t              block_size;

  block_size = (block_size_hint) ? block_size_hint : PDC_NOREC_DEF_BSIZE;

  if (!(disc_vm = vmopen(Vmdcheap, Vmbest, 0))) {
    goto alloc_err;
  }
  if (!(io_disc = vmnewof(disc_vm, 0, PDC_IO_disc_t, 1, 0))) {
    goto alloc_err;
  }
  if (!(data = vmnewof(disc_vm, 0, PDC_norec_data_t, 1, 0))) {
    goto alloc_err;
  }
  if (!(f_head = vmnewof(disc_vm, 0, PDC_IO_elt_t, 1, 0))) {
    goto alloc_err;
  }

  f_head->prev = f_head;
  f_head->next = f_head;

  data->disc_vm       = disc_vm;
  data->block_size    = block_size;
  data->f_head        = f_head;

  if ((block_size % 1024) == 0) {
    sprintf(data->unit,  "%dKB Block", block_size / 1024);
    sprintf(data->punit, "(last: partial) %dKB Block", block_size / 1024);
  } else {
    sprintf(data->unit,  "%dB Block", block_size);
    sprintf(data->punit, "(last: partial) %dB Block", block_size);
  }

  io_disc->unmake_fn    = PDC_norec_unmake;
  io_disc->sfopen_fn    = PDC_norec_sfopen;
  io_disc->sfclose_fn   = PDC_norec_sfclose;
  io_disc->read_fn      = PDC_norec_read;
  io_disc->rec_close_fn = PDC_norec_rec_close;
  io_disc->blk_close_fn = PDC_norec_blk_close;

  io_disc->name         = "norec";
  io_disc->descr        = "a raw bytes IO discipline that does not use EOR";
  io_disc->rec_based    = 0;
  io_disc->has_rblks    = 0;
  io_disc->rec_obytes   = 0;
  io_disc->rec_cbytes   = 0;
  io_disc->blk_obytes   = 0;
  io_disc->blk_cbytes   = 0;
  io_disc->data         = data;

  return io_disc;

 alloc_err:
  PDC_WARN(&PDC_default_disc, "PDC_norec_make: out of space");
  if (disc_vm) {
    vmclose(disc_vm);
  }
  return 0;
}

