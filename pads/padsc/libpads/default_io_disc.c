/*
 * Implementations of some standard IO disciplines
 *
 *   fwrec:        an IO discipline for data with fixed-width records.
 *
 *   nlrec:        an IO discipline for newline-terminated variable-width records
 *
 *   norec:        a raw bytes IO discipline that does not use EOR
 *
 *   fwrec_noseek: a version of fwrec that does not require that
 *                 the sfio stream be seekable
 *
 *   nlrec_noseek: a version of nlrec that does not require that
 *                 the sfio stream be seekable
 *
 *   norec_noseek: a version of norec that does not require that
 *                 the sfio stream be seekable
 *
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

/* Documentation of the state vars for an IO discipline
 *
 * name: 
 *    a short name for the IO discipline
 *
 * descr:
 *    a short description of the IO discpline
 *
 * uses_eor:
 *    set to 1 if this discipline implements record-based reading
 *    (i.e., if the discipline sets the eor flag), to 0 if it does not.
 *
 * data:
 *    IO-discpline-specific data.
 *
 */

/* Documentation for each of the required functions in an IO discipline
 *
 * PDC_unmake_fn:
 *    The unmake function cleans up all space allocated by the IO
 *    discipline, including the space for the discipline itself as
 *    allocated by the _make function.  The discipline should not be
 *    used after this call.
 *
 * PDC_sfopen_fn:
 *    sfopen takes an already-open Sfio stream; the open call here simply
 *    enables reading from this stream using the IO discpline.
 *
 * PDC_sfclose_fn:
 *    sfclose does not close the Sfio stream; the close call here simply
 *    disables reading from the IO discpline until another open call occurs;
 *    its main purpose is to return unused bytes to the Sfio stream.  
 *    An IO discipline should be closed before it is unmade.
 *
 * PDC_read_fn:
 *    After adding one or more elts to head, for the final elt,
 *    elt->end points to a final NULL byte one byte beyond the last valid data byte
 *    that was read.  (Note that this means for a fixed width record of len L,
 *    L+1 bytes of space must be allocated to allow for null-termination of the bytes.)
 *    If both eor and eof are 0, then the terminating null byte is over-written with the
 *    first data byte produced by the next read_fn call.  If eor or eof is 1,
 *    then the terminating null byte should not be overwritten by a subsequent
 *    read_fn call if the cur_io_elt position refers to a byte before this byte.
 *
 *    invariants:
 *        1. elt->eor or elt->eof set, but not both
 *        2. elt->len always > 0 (unless elt->eof == 1, in which case elt->len can be 0)
 *
 *    out params:
 *        + If PDC_OK is returned, (*next_elt_out) is set to the
 *          first new element added to the IO elt list.  Note that if
 *          cur_io_elt is NULL, then the read function is allowed to
 *          remove all existing elements and this 'next element' may
 *          end up being the only element in the list.  If some elt
 *          lastelt is the last element prior to a read_fn call, then
 *          it is only safe to use lastelt->next if a non-NULL cur_io_elt
 *          is used.  */

#include "pdc_io_disc.h"
#include "pdc_out_macros.h"

#define PDC_NEED_AN_ELT(fn_name, elt, f_head, vm) \
  do { \
    if (PDC_SOME_ELTS(f_head)) { \
      elt = PDC_FIRST_ELT(f_head); \
      PDC_REMOVE_ELT(elt); \
    } else { \
      if (!(elt = vmnewof((vm), 0, PDC_IO_elt_t, 1, 0))) { \
	PDC_WARN(pdc->disc, fn_name ": could not alloc space for input record"); \
      } \
    } \
  } while (0)


/* ================================================================================ */
/* PDC_fwrec_noseek IMPLEMENTATION */

/* private types */
typedef struct PDC_fwrec_noseek_data_s {
  /* configuration fields */
  size_t         leader_len;
  size_t         data_len;
  size_t         trailer_len;
  size_t         block_size; /* data_len + trailer_len (leader read/skipped separately) */
  /* other fields */
  Vmalloc_t      *disc_vm;   /* lifetime: make/unmake pairing */
  PDC_IO_elt_t   *head;      /* head of IO rec list */
  PDC_IO_elt_t   *f_head;    /* head of free list   */
  Sfio_t         *io;        /* Sfio stream to read from */
  int            eof;        /* hit EOF? */
  size_t         num;        /* record/block number */
  char           unit[100];  /* unit description when reading blocks */
  char           punit[100]; /* ditto -- partial read case */
} PDC_fwrec_noseek_data_t;

typedef struct PDC_fwrec_noseek_iodata_s {
  size_t eor_putback;
  char dbuf[1]; /* actual size : block_size+1 */
} PDC_fwrec_noseek_iodata_t;

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
  if (data && data->disc_vm) {
    vmclose(data->disc_vm);
  }
  return PDC_ERR;
}

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
  PDC_IO_elt_t         *elt;

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
    /* add up the number of bytes to return to the Sfio stream */
    for (elt = io_cur_elt; elt != data->head; elt = elt->next) {
      iodata = (PDC_fwrec_noseek_iodata_t*)elt->disc_ptr;
      if (elt != io_cur_elt) {
	remain += elt->len;
      }
      remain += iodata->eor_putback;
    }
    if (remain) { /* try to seek backwards remain bytes */
      unsigned long remain_ul = remain;
      Sfoff_t       offset    = -1 * remain;
      if (-1 == sfseek(data->io, offset, SEEK_CUR)) {
	PDC_WARN1(pdc->disc, "PDC_fwrec_noseek_sfclose: failed to return %lu bytes to IO stream", remain_ul);
      } else {
	PDC_WARN1(pdc->disc, "XXX_CHANGE_TO_DBG PDC_fwrec_noseek_sfclose: returned %lu bytes to IO stream", remain_ul);
      }
    }
  }

  /* remove all elts from list -- put them on the free list */
  while (PDC_SOME_ELTS(data->head)) {
    elt = PDC_FIRST_ELT(data->head);
    PDC_REMOVE_ELT(elt);
    PDC_APPEND_ELT(data->f_head, elt);
  }

  data->io      = 0;
  data->head    = 0; 

  return PDC_OK;
}

PDC_error_t
PDC_fwrec_noseek_read(PDC_t *pdc, PDC_IO_disc_t* io_disc, PDC_IO_elt_t *io_cur_elt, size_t remain, PDC_IO_elt_t** next_elt_out)
{
  PDC_fwrec_noseek_data_t     *data;
  PDC_fwrec_noseek_iodata_t   *iodata;
  PDC_IO_elt_t         *elt, *keepelt;
  ssize_t              readlen  = 0;

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
  keepelt = io_cur_elt ? io_cur_elt : data->head;
  /* move some elts to the free list */
  while (PDC_SOME_ELTS(data->head) && PDC_FIRST_ELT(data->head) != keepelt) {
    elt = PDC_FIRST_ELT(data->head);
    PDC_REMOVE_ELT(elt);
    PDC_APPEND_ELT(data->f_head, elt);
  }
  if (PDC_SOME_ELTS(data->f_head)) {
    elt = PDC_FIRST_ELT(data->f_head);
    PDC_REMOVE_ELT(elt);
  } else {
    if (!(elt = vmnewof(data->disc_vm, 0, PDC_IO_elt_t, 1, 0)) ||
	!(elt->disc_ptr = vmoldof(data->disc_vm, 0, PDC_fwrec_noseek_iodata_t, 1, data->block_size))) {
      PDC_WARN(pdc->disc, "PDC_fwrec_noseek_read: could not alloc space for input record");
      return PDC_ERR;
    }
  }
  iodata = (PDC_fwrec_noseek_iodata_t*)elt->disc_ptr;
  if (data->leader_len) {
    if (-1 == sfseek(data->io, data->leader_len, SEEK_CUR)) {
      readlen = -1;
      goto skip_sfread;
    }
  }
  readlen = sfread(data->io, (Void_t*)iodata->dbuf, data->block_size);
 skip_sfread:
  if (readlen < 0) {
    PDC_SYSERR(pdc->disc, "PDC_fwrec_noseek_read: Error reading IO stream");
    readlen = 0;
  }
  elt->len = (readlen >= data->trailer_len) ? (readlen - data->trailer_len) : 0;
  iodata->dbuf[elt->len] = 0; /* null-terminate the bytes read */
  iodata->eor_putback = readlen - elt->len; /* normally data->trailer_len */
  elt->begin = iodata->dbuf;
  elt->end   = elt->begin + elt->len;
  elt->num = (data->num)++;
  if (readlen == data->block_size) { /* normal EOR case */
    elt->eor = 1;
    elt->eof = 0;
    elt->unit = (const char*)data->unit;
  } else { /* partial read, hit EOF before EOR */
    elt->eor = 0;
    elt->eof = 1;
    data->eof = 1;
    if (elt->len == 0) { /* trivial EOF record */
      elt->unit = "empty block (EOF)";
    } else { /* partial-read EOF record */
      elt->unit = (const char*)data->punit;
    }
  }
  PDC_APPEND_ELT(data->head, elt);
#if 0
  if (pdc->disc->errorf) {
    pdc->disc->errorf(NiL, 0, "XXX_REMOVE(%s %d)\n[%s]", elt->unit, elt->num, fmtquote(elt->begin, NiL, NiL, elt->len, 0));
  }
#endif
  (*next_elt_out) = elt;
  return PDC_OK;
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
  data->block_size    = data_len + trailer_len;
  data->f_head        = f_head;

  if ((data->block_size % 1024) == 0) {
    sprintf(data->unit,  "%dKB Block", data->block_size / 1024);
    sprintf(data->punit, "%dKB Block (last: partial)", data->block_size / 1024);
  } else {
    sprintf(data->unit,  "%dB Block", data->block_size);
    sprintf(data->punit, "%dB Block (last: partial)", data->block_size);
  }

  io_disc->unmake_fn    = PDC_fwrec_noseek_unmake;
  io_disc->sfopen_fn    = PDC_fwrec_noseek_sfopen;
  io_disc->sfclose_fn   = PDC_fwrec_noseek_sfclose;
  io_disc->read_fn      = PDC_fwrec_noseek_read;

  io_disc->name         = "fwrec_noseek";
  io_disc->descr        = "an IO discipline for data with fixed-width records";
  io_disc->uses_eor     = 1;
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
#define PDC_NOREC_NOSEEK_INIT_BLOCKS   16
#define PDC_NOREC_NOSEEK_GC_AT_BLOCK    8
#define PDC_NOREC_NOSEEK_ADD_BLOCKS     8

/* private types */
typedef struct PDC_norec_noseek_data_s {
  /* configuration fields */
  size_t         block_size;
  /* other fields */
  Vmalloc_t      *disc_vm;   /* lifetime: make/unmake pairing */
  PDC_IO_elt_t   *head;      /* head of IO rec list */
  PDC_IO_elt_t   *f_head;    /* head of free list   */
  Sfio_t         *io;        /* Sfio stream to read from */
  int            eof;        /* hit EOF? */
  size_t         num;        /* record/block number */
  char           unit[100];  /* unit description when reading blocks */
  char           punit[100]; /* ditto -- partial read case */
  char           *dbuf;      /* resizable data buffer */
  char           *dbuf_end;  /* 1 beyond last byte read */
  size_t         balloc;     /* # blocks allocated */
  size_t         btail;      /* idx of last block in use */
  size_t         gc_point;
} PDC_norec_noseek_data_t;

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
  if (data && data->disc_vm) {
    vmclose(data->disc_vm);
  }
  return PDC_ERR;
}

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
  PDC_IO_elt_t      *elt;

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
    Sfoff_t        offset;
    unsigned long  remain_ul;
    remain        = (data->dbuf_end - io_cur_elt->end) + remain;
    remain_ul     = remain;
    offset        = -1 * remain;
    if (-1 == sfseek(data->io, offset, SEEK_CUR)) {
      PDC_WARN1(pdc->disc, "PDC_norec_noseek_sfclose: failed to return %lu bytes to IO stream", remain_ul);
    } else {
      PDC_WARN1(pdc->disc, "XXX_CHANGE_TO_DBG PDC_norec_noseek_sfclose: returned %lu bytes to IO stream", remain_ul);
    }
  }

  /* remove all elts from list -- put them on the free list */
  while (PDC_SOME_ELTS(data->head)) {
    elt = PDC_FIRST_ELT(data->head);
    PDC_REMOVE_ELT(elt);
    PDC_APPEND_ELT(data->f_head, elt);
  }

  data->io      = 0;
  data->head    = 0; 

  return PDC_OK;
}

PDC_error_t
PDC_norec_noseek_read(PDC_t *pdc, PDC_IO_disc_t* io_disc, PDC_IO_elt_t *io_cur_elt, size_t remain, PDC_IO_elt_t** next_elt_out)
{
  PDC_norec_noseek_data_t     *data;
  PDC_IO_elt_t         *elt, *keepelt;
  size_t               keep_len;
  ssize_t              readlen  = 0;
  char                 *tmp;

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
  keepelt = io_cur_elt ? io_cur_elt : data->head;
  /* move some elts to the free list */
  while (PDC_SOME_ELTS(data->head) && PDC_FIRST_ELT(data->head) != keepelt) {
    elt = PDC_FIRST_ELT(data->head);
    PDC_REMOVE_ELT(elt);
    PDC_APPEND_ELT(data->f_head, elt);
  }

  PDC_NEED_AN_ELT("PDC_norec_noseek_read", elt, data->f_head, data->disc_vm);
  if (!elt) {
    return PDC_ERR;
  }

  if (!io_cur_elt) {
    data->btail = 0; /* starting from scratch */
    data->dbuf_end = data->dbuf;
  } else {
    /* choose or alloc block to use */
    if (++(data->btail) >= data->balloc) {
      /* need space */
      if (io_cur_elt->begin - data->dbuf >= data->gc_point) {
	/* garbage collect */
	keep_len = data->dbuf_end - io_cur_elt->begin;
	memmove((void*)data->dbuf, (const void*)io_cur_elt->begin, keep_len);
	data->btail = (keep_len/data->block_size); /* # blocks copied => correct block index for next read */
	data->dbuf_end = data->dbuf + keep_len;
      } else {
	/* grow dbuf */
	char* dbuf_next;
	size_t balloc_next = data->balloc + PDC_NOREC_NOSEEK_ADD_BLOCKS;
	keep_len = data->dbuf_end - data->dbuf;
	if (!(dbuf_next = vmoldof(data->disc_vm, data->dbuf, char, balloc_next * data->block_size, 1))) {
	  PDC_WARN(pdc->disc, "PDC_norec_noseek_read: could not alloc space for input record");
	  (data->btail)--;
	  return PDC_ERR;
	}
	data->balloc   = balloc_next;
	data->dbuf     = dbuf_next;
	data->dbuf_end = dbuf_next + keep_len;
	data->gc_point += (PDC_NOREC_NOSEEK_ADD_BLOCKS * data->block_size);
      }
      /* data may have moved, so redo begin/end pointers */
      tmp = data->dbuf;
      for (elt = PDC_FIRST_ELT(data->head); elt != data->head; elt = elt->next) {
	elt->begin = tmp;
	tmp += elt->len;
	elt->end = tmp;
      }
    } else {
      /* dbuf already had space at block btail */
    }
  }

  elt->eor = 0; /* norec_noseek never uses eor */
  elt->begin = data->dbuf_end;
  readlen = sfread(data->io, (Void_t*)elt->begin, data->block_size);
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
      elt->unit = "empty block (EOF)";
    } else { /* partial-read EOF record */
      elt->unit = (const char*)data->punit;
    }
  }
  PDC_APPEND_ELT(data->head, elt);
#if 0
  if (pdc->disc->errorf) {
    pdc->disc->errorf(NiL, 0, "XXX_REMOVE(%s %d)\n[%s]", elt->unit, elt->num, fmtquote(elt->begin, NiL, NiL, elt->len, 0));
  }
#endif
  (*next_elt_out) = elt;
  return PDC_OK;
}

PDC_IO_disc_t *
PDC_norec_noseek_make(size_t block_size_hint)
{
  Vmalloc_t                 *disc_vm = 0;
  PDC_norec_noseek_data_t   *data;
  PDC_IO_disc_t             *io_disc;
  PDC_IO_elt_t              *f_head;
  char                      *dbuf;
  size_t                    block_size;

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
  if (!(dbuf = vmoldof(disc_vm, 0, char, PDC_NOREC_NOSEEK_INIT_BLOCKS * block_size, 1))) {
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
    sprintf(data->punit, "%dKB Block (last: partial)", block_size / 1024);
  } else {
    sprintf(data->unit,  "%dB Block", block_size);
    sprintf(data->punit, "%dB Block (last: partial)", block_size);
  }

  io_disc->unmake_fn    = PDC_norec_noseek_unmake;
  io_disc->sfopen_fn    = PDC_norec_noseek_sfopen;
  io_disc->sfclose_fn   = PDC_norec_noseek_sfclose;
  io_disc->read_fn      = PDC_norec_noseek_read;

  io_disc->name         = "norec_noseek";
  io_disc->descr        = "a raw bytes IO discipline that does not use EOR";
  io_disc->uses_eor     = 0;
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
/* PDC_nlrec_noseek IMPLEMENTATION */

/* when not specified, use blocks of this size (bytes): */
#define PDC_NLREC_NOSEEK_DEF_BSIZE     512
#define PDC_NLREC_NOSEEK_INIT_BLOCKS    16
#define PDC_NLREC_NOSEEK_GC_AT_BLOCK     8
#define PDC_NLREC_NOSEEK_GC_BLOCKS_LEFT  4
#define PDC_NLREC_NOSEEK_ADD_BLOCKS      8

/* private types */
typedef struct PDC_nlrec_noseek_data_s {
  /* configuration fields */
  size_t         block_size;
  /* other fields */
  Vmalloc_t      *disc_vm;    /* lifetime: make/unmake pairing */
  PDC_IO_elt_t   *head;       /* head of IO rec list */
  PDC_IO_elt_t   *f_head;     /* head of free list   */
  PDC_IO_elt_t   *eof_elt;    /* always keep around an elt to be used as EOF elt */
  Sfio_t         *io;         /* Sfio stream to read from */
  int            eof;         /* hit EOF? */
  size_t         num;         /* line number */
  char           *dbuf;       /* resizable data buffer */
  char           *dbuf_end;   /* 1 beyond last byte read */ 
  size_t         un_bytes;    /* unread bytes: # bytes not yet part of IO rec list */
  size_t         balloc;      /* # blocks allocated */
  size_t         btail;       /* idx of last block in use */
  size_t         gc_point;
} PDC_nlrec_noseek_data_t;

PDC_error_t
PDC_nlrec_noseek_unmake(PDC_t *pdc, PDC_IO_disc_t* io_disc)
{
  PDC_nlrec_noseek_data_t  *data;

  if (!pdc || !pdc->disc) {
    return PDC_ERR;
  }
  data = (PDC_nlrec_noseek_data_t*)io_disc->data;
  if (data && data->disc_vm) {
    vmclose(data->disc_vm);
  }
  return PDC_ERR;
}

PDC_error_t
PDC_nlrec_noseek_sfopen(PDC_t *pdc, PDC_IO_disc_t* io_disc, Sfio_t* sfio, PDC_IO_elt_t* head)
{
  PDC_nlrec_noseek_data_t  *data;
  PDC_IO_elt_t      *elt; 

  if (!pdc || !pdc->disc) {
    return PDC_ERR;
  }
  if (!sfio || !head || !io_disc || !io_disc->data) {
    PDC_WARN(pdc->disc, "PDC_nlrec_noseek_sfopen: bad param(s)");
    return PDC_ERR;
  }
  if (head->next != head || head->prev != head) {
    PDC_WARN(pdc->disc, "PDC_nlrec_noseek_sfopen: head->next and head->prev must both point to head");
    return PDC_ERR;
  }
  data = (PDC_nlrec_noseek_data_t*)io_disc->data;
  if (data->io) {
    PDC_WARN(pdc->disc, "PDC_nlrec_noseek_sfopen: not in a valid closed state");
    return PDC_ERR;
  }
  PDC_NEED_AN_ELT("PDC_nlrec_noseek_sfopen", elt, data->f_head, data->disc_vm);
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
PDC_nlrec_noseek_sfclose(PDC_t *pdc, PDC_IO_disc_t* io_disc, PDC_IO_elt_t *io_cur_elt, size_t remain)
{
  PDC_nlrec_noseek_data_t  *data;
  PDC_IO_elt_t      *elt;

  if (!pdc || !pdc->disc) {
    return PDC_ERR;
  }
  if (!io_disc || !io_disc->data) {
    PDC_WARN(pdc->disc, "PDC_nlrec_noseek_sfclose: bad param(s)");
    return PDC_ERR;
  }
  data = (PDC_nlrec_noseek_data_t*)io_disc->data;
  if (!data->io) {
    PDC_WARN(pdc->disc, "PDC_nlrec_noseek_sfclose: not in a valid open state");
    return PDC_ERR;
  }
  if (io_cur_elt == data->head) {
    PDC_WARN(pdc->disc, "PDC_nlrec_noseek_sfclose: io_cur_elt == head not a valid elt!");
    return PDC_ERR;
  }
  if (io_cur_elt && remain > io_cur_elt->len) {
    PDC_WARN(pdc->disc, "PDC_nlrec_noseek_sfclose: remain > io_cur_elt->len!");
    return PDC_ERR;
  }
  if (io_cur_elt) {
    Sfoff_t        offset;
    unsigned long  remain_ul;
    remain        = (data->dbuf_end - io_cur_elt->end) + remain;
    remain_ul     = remain;
    offset        = -1 * remain;
    if (-1 == sfseek(data->io, offset, SEEK_CUR)) {
      PDC_WARN1(pdc->disc, "PDC_nlrec_noseek_sfclose: failed to return %lu bytes to IO stream", remain_ul);
    } else {
      PDC_WARN1(pdc->disc, "XXX_CHANGE_TO_DBG PDC_nlrec_noseek_sfclose: returned %lu bytes to IO stream", remain_ul);
    }
  }

  /* remove all elts from list -- put them on the free list */
  while (PDC_SOME_ELTS(data->head)) {
    elt = PDC_FIRST_ELT(data->head);
    PDC_REMOVE_ELT(elt);
    PDC_APPEND_ELT(data->f_head, elt);
  }
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
PDC_nlrec_noseek_read(PDC_t *pdc, PDC_IO_disc_t* io_disc, PDC_IO_elt_t *io_cur_elt, size_t remain, PDC_IO_elt_t** next_elt_out)
{
  PDC_nlrec_noseek_data_t    *data;
  PDC_IO_elt_t        *elt, *keepelt;
  ssize_t             readlen, keep_len, bytes_read;
  char                *tmp;
  char                *first_save_byte;
  char                *found_newline;

  if (!pdc || !pdc->disc) {
    return PDC_ERR;
  }
  if (!io_disc || !io_disc->data || !next_elt_out) {
    PDC_WARN(pdc->disc, "PDC_nlrec_noseek_read: bad param(s)");
    return PDC_ERR;
  }
  (*next_elt_out) = 0;
  data = (PDC_nlrec_noseek_data_t*)io_disc->data;

  if (!data->io) {
    PDC_WARN(pdc->disc, "PDC_nlrec_noseek_read: not in a valid open state");
    return PDC_ERR;
  }
  if (data->eof) {
    PDC_WARN(pdc->disc, "PDC_nlrec_noseek_read: called after returning EOF flag in previous call");
    return PDC_ERR;
  }
  if (io_cur_elt == data->head) {
    PDC_WARN(pdc->disc, "PDC_nlrec_noseek_read: io_cur_elt == head not a valid elt!");
    return PDC_ERR;
  }
  keepelt = io_cur_elt ? io_cur_elt : data->head;
  /* move some elts to the free list */
  while (PDC_SOME_ELTS(data->head) && PDC_FIRST_ELT(data->head) != keepelt) {
    elt = PDC_FIRST_ELT(data->head);
    PDC_REMOVE_ELT(elt);
    PDC_APPEND_ELT(data->f_head, elt);
  }
  /*
   * garbage collect if <= PDC_NLREC_NOSEEK_GC_BLOCKS_LEFT and there is
   * a good chunk of data to be discarded
   */
  first_save_byte = io_cur_elt ? io_cur_elt->begin :
    (data->un_bytes ? (data->dbuf_end - data->block_size) : data->dbuf_end);
  if ((data->balloc - data->btail < PDC_NLREC_NOSEEK_GC_BLOCKS_LEFT) && 
      (first_save_byte - data->dbuf >= data->gc_point)) {
    keep_len = data->dbuf_end - first_save_byte;
    if (keep_len) {
      memmove((void*)data->dbuf, (const void*)first_save_byte, keep_len);
      first_save_byte = data->dbuf;
      data->btail = (keep_len/data->block_size) - 1; /* # blocks copied - 1 => last block index */
      data->dbuf_end = data->dbuf + keep_len;
      /* redo begin/end pointers */
      tmp = data->dbuf;
      for (elt = PDC_FIRST_ELT(data->head); elt != data->head; elt = elt->next) {
	elt->begin = tmp;
	tmp += elt->len;
	elt->end = tmp;
	if (elt->eor) {
	  tmp++; /* account for newline */
	}
      }
    }
  }
  bytes_read    = 0;
  found_newline = 0;
  while (1) { /* read blocks until find newline or EOF */
    if (first_save_byte == data->dbuf_end) {
      data->btail = 0; /* starting from scratch */
      data->dbuf_end = data->dbuf;
    } else {
      /* choose or alloc block to use */
      if (++(data->btail) >= data->balloc) {
	/* need space -- grow dbuf */
	char* dbuf_next;
	size_t balloc_next = data->balloc + PDC_NLREC_NOSEEK_ADD_BLOCKS;
	keep_len = data->dbuf_end - data->dbuf;
	if (!(dbuf_next = vmoldof(data->disc_vm, data->dbuf, char, balloc_next * data->block_size, 1))) {
	  PDC_WARN(pdc->disc, "PDC_nlrec_noseek_read: could not alloc space for input record");
	  (data->btail)--;
	  readlen = 0;
	  break; /* continue after while to take care of earlier bytes_read, if any */
	}
	data->balloc   = balloc_next;
	data->dbuf     = dbuf_next;
	data->dbuf_end = dbuf_next + keep_len;
	data->gc_point += (PDC_NLREC_NOSEEK_ADD_BLOCKS * data->block_size);
	/* data may have moved, so redo begin/end pointers */
	tmp = data->dbuf;
	for (elt = PDC_FIRST_ELT(data->head); elt != data->head; elt = elt->next) {
	  elt->begin = tmp;
	  tmp += elt->len;
	  elt->end = tmp;
	  if (elt->eor) {
	    tmp++; /* account for newline */
	  }
	}
      } else {
	/* dbuf already had space at block btail */
      }
    }
    first_save_byte = 0; /* prevents setting btail to zero multiple times throught loop */
    readlen = sfread(data->io, (Void_t*)data->dbuf_end, data->block_size);
    if (readlen < 0) {
      PDC_SYSERR(pdc->disc, "PDC_nlrec_noseek_read: Error reading IO stream");
      readlen = 0;
      break;
    }
    if (readlen == 0) {
      break;
    }
    bytes_read += readlen;
    tmp = data->dbuf_end;
    data->dbuf_end += readlen;
    *(data->dbuf_end) = 0; /* null-terminate dbuf -- note use of extra byte in vmoldof calls */
    if ((found_newline = strchr(tmp, '\n'))) {
      break;
    }
    /* read another block */
  }
  /* bytes_read reflects all reads, readlen reflects last read */
  data->un_bytes += bytes_read;
  tmp = data->dbuf_end - data->un_bytes;
  /* first add any EOR-based records */
  while (found_newline) {
    PDC_NEED_AN_ELT("PDC_nlrec_noseek_read", elt, data->f_head, data->disc_vm);
    if (!elt) { /* turn this error into an EOF case */
      readlen = 0;
      break;
    }
    elt->begin     = tmp;
    elt->end       = found_newline;
    elt->len       = found_newline - tmp;
    elt->eor       = 1;
    elt->eof       = 0;
    elt->num       = (data->num)++;
    elt->unit      = "line";
    data->un_bytes -= (elt->len + 1); /* acount for newline */
    elt->begin[elt->len] = 0; /* null-terminate the record, replaces newline with NULL */
    PDC_APPEND_ELT(data->head, elt);
#if 0
    if (pdc->disc->errorf) {
      pdc->disc->errorf(NiL, 0, "XXX_REMOVE(%s %d)\n[%s]", elt->unit, elt->num, fmtquote(elt->begin, NiL, NiL, elt->len, 0));
    }
#endif
    if (!(*next_elt_out)) {
      (*next_elt_out) = elt;
    }
    tmp = data->dbuf_end - data->un_bytes;
    found_newline = strchr(tmp, '\n');
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
      elt->unit = "empty line (EOF)";
    } else { /* partial-read EOF record */
      elt->unit = "line (partial: missing newline)";
    }
    elt->begin[elt->len] = 0; /* null-terminate the record */
    PDC_APPEND_ELT(data->head, elt);
#if 0
    if (pdc->disc->errorf) {
      pdc->disc->errorf(NiL, 0, "XXX_REMOVE(%s %d)\n[%s]", elt->unit, elt->num, fmtquote(elt->begin, NiL, NiL, elt->len, 0));
    }
#endif
    if (!(*next_elt_out)) {
      (*next_elt_out) = elt;
    }
  } /* else found newline in a full block read */
  return PDC_OK;
}

PDC_IO_disc_t *
PDC_nlrec_noseek_make(size_t block_size_hint)
{
  Vmalloc_t         *disc_vm = 0;
  PDC_nlrec_noseek_data_t  *data;
  PDC_IO_disc_t     *io_disc;
  PDC_IO_elt_t      *f_head;
  char              *dbuf;
  size_t            block_size;

  block_size = (block_size_hint) ? block_size_hint : PDC_NLREC_NOSEEK_DEF_BSIZE;

  if (!(disc_vm = vmopen(Vmdcheap, Vmbest, 0))) {
    goto alloc_err;
  }
  if (!(io_disc = vmnewof(disc_vm, 0, PDC_IO_disc_t, 1, 0))) {
    goto alloc_err;
  }
  if (!(data = vmnewof(disc_vm, 0, PDC_nlrec_noseek_data_t, 1, 0))) {
    goto alloc_err;
  }
  if (!(f_head = vmnewof(disc_vm, 0, PDC_IO_elt_t, 1, 0))) {
    goto alloc_err;
  }
  if (!(dbuf = vmoldof(disc_vm, 0, char, PDC_NLREC_NOSEEK_INIT_BLOCKS * block_size, 1))) {
    goto alloc_err;
  }

  f_head->prev = f_head;
  f_head->next = f_head;

  data->disc_vm       = disc_vm;
  data->block_size    = block_size;
  data->f_head        = f_head;
  data->dbuf          = dbuf;
  data->gc_point      = PDC_NLREC_NOSEEK_GC_AT_BLOCK * block_size;
  data->balloc        = PDC_NLREC_NOSEEK_INIT_BLOCKS;

  io_disc->unmake_fn    = PDC_nlrec_noseek_unmake;
  io_disc->sfopen_fn    = PDC_nlrec_noseek_sfopen;
  io_disc->sfclose_fn   = PDC_nlrec_noseek_sfclose;
  io_disc->read_fn      = PDC_nlrec_noseek_read;

  io_disc->name         = "nlrec_noseek";
  io_disc->descr        = "an IO discipline for newline-terminated variable-width records";
  io_disc->uses_eor     = 1;
  io_disc->data         = data;

  return io_disc;

 alloc_err:
  PDC_WARN(&PDC_default_disc, "PDC_nlrec_noseek_make: out of space");
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
  return 0;
}

/* ================================================================================ */
/* PDC_nlrec IMPLEMENTATION */

PDC_IO_disc_t *
PDC_nlrec_make(size_t block_size_hint)
{
  /* XXX_TODO */
  return 0;
}

/* ================================================================================ */
/* PDC_norec IMPLEMENTATION */

/* when not specified, use blocks of this size (bytes): */
#define PDC_NOREC_DEF_BSIZE    512

/* private types */
typedef struct PDC_norec_data_s {
  /* configuration fields */
  size_t         block_size;
  /* other fields */
  Vmalloc_t      *disc_vm;   /* lifetime: make/unmake pairing */
  PDC_IO_elt_t   *head;      /* head of IO rec list */
  PDC_IO_elt_t   *f_head;    /* head of free list   */
  PDC_IO_elt_t   *eof_elt;   /* always keep around an elt to be used as EOF elt */
  Sfio_t         *io;        /* Sfio stream to read from */
  int            eof;        /* hit EOF? */
  size_t         num;        /* record/block number */
  char           dummy[1];   /* for immediate EOF error */
  char           unit[100];  /* unit description when reading blocks */
  char           punit[100]; /* ditto -- partial read case */
  char           *r_begin;   /* begin of reserved sfio bytes */
  char           *r_end;     /* 1 byte past end of reserved sfio bytes */
  unsigned char  saved_byte; /* we NULL *(r_end) after each sfreserve, restore it prior to next sfreserve */
  Sfoff_t        tail_off;   /* tail offset (obtained after each sfreserve call) */
} PDC_norec_data_t;

PDC_error_t
PDC_norec_unmake(PDC_t *pdc, PDC_IO_disc_t* io_disc)
{
  PDC_norec_data_t  *data;

  if (!pdc || !pdc->disc) {
    return PDC_ERR;
  }
  data = (PDC_norec_data_t*)io_disc->data;
  if (data && data->disc_vm) {
    vmclose(data->disc_vm);
  }
  return PDC_ERR;
}

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
  PDC_NEED_AN_ELT("PDC_nlrec_noseek_sfopen", elt, data->f_head, data->disc_vm);
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
  PDC_norec_data_t  *data;
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
    Sfoff_t offset = io_cur_elt->disc_off + io_cur_elt->len - remain;
    if (offset < data->tail_off) {
      unsigned long to_return = data->tail_off - offset;
      if (-1 == sfseek(data->io, offset, 0)) {
	PDC_WARN1(pdc->disc, "PDC_norec_sfclose: failed to return %lu bytes to IO stream", to_return);
      } else {
	PDC_WARN1(pdc->disc, "XXX_CHANGE_TO_DBG PDC_norec_sfclose: returned %lu bytes to IO stream", to_return);
      }
    }
  }

  /* remove all elts from list -- put them on the free list */
  while (PDC_SOME_ELTS(data->head)) {
    elt = PDC_FIRST_ELT(data->head);
    PDC_REMOVE_ELT(elt);
    PDC_APPEND_ELT(data->f_head, elt);
  }
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
  PDC_IO_elt_t         *elt, *firstelt, *keepelt;
  ssize_t              readlen, to_keep, to_discard, to_reserve, diff;
  Sfoff_t              new_data_off;
  char                 *new_r_begin, *new_data;

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
    *(data->r_end) = data->saved_byte; /* restore in case sfio assumes it is there */
  }
  /* move some elts to the free list */
  keepelt = io_cur_elt ? io_cur_elt : data->head;
  while (PDC_SOME_ELTS(data->head) && PDC_FIRST_ELT(data->head) != keepelt) {
    elt = PDC_FIRST_ELT(data->head);
    PDC_REMOVE_ELT(elt);
    PDC_APPEND_ELT(data->f_head, elt);
  }
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
  if (!new_r_begin) {
    new_r_begin = sfreserve(data->io, 0, SF_LASTR);
    if (!new_r_begin) {
      readlen = 0; 
    }
  }
  data->tail_off = sftell(data->io);
  if (new_r_begin) {
    data->r_end = new_r_begin + readlen;
    if (readlen < to_keep) {
      PDC_FATAL(pdc->disc, "PDC_norec_read: unexpected failure, reserve got even fewer bytes than last time");
    }
    readlen -= to_keep; /* readlen now == the bytes that go in the new elt(s) */
    if (data->r_begin) {
      data->r_begin += to_discard; /* need this adjustment before we compute diff */
      if (new_r_begin > data->r_begin) { /* data moved to higher memory loc */ 
	diff = new_r_begin - data->r_begin;
#if 0
	if (pdc->disc->errorf) {
	  pdc->disc->errorf(NiL, 0, "XXX_REMOVE DATA SHIFTED HIGHER BY %d BYTES", diff);
	}
#endif
	for (elt = firstelt; elt != data->head; elt = elt->next) {
	  elt->begin += diff;
	  elt->end   += diff;
#if 0
	  if (pdc->disc->errorf) {
	    pdc->disc->errorf(NiL, 0, "XXX_REMOVE(%s %d disc_off %d) = AT NEW LOC =>\n[%s]", elt->unit, elt->num, elt->disc_off, fmtquote(elt->begin, NiL, NiL, elt->len, 0));
	  }
#endif
	}
      } else if (data->r_begin > new_r_begin) { /* data moved to lower memory loc */
	diff = data->r_begin - new_r_begin;
#if 0
	if (pdc->disc->errorf) {
	  pdc->disc->errorf(NiL, 0, "XXX_REMOVE DATA SHIFTED LOWER BY %d BYTES", diff);
	}
#endif
	for (elt = firstelt; elt != data->head; elt = elt->next) {
	  elt->begin -= diff;
	  elt->end   -= diff;
#if 0
	  if (pdc->disc->errorf) {
	    pdc->disc->errorf(NiL, 0, "XXX_REMOVE(%s %d disc_off %d) = AT NEW LOC =>\n[%s]", elt->unit, elt->num, elt->disc_off, fmtquote(elt->begin, NiL, NiL, elt->len, 0));
	  }
#endif
	}
      }
    }
    data->r_begin = new_r_begin;
  }
  if (data->r_end) {
    data->saved_byte = *(data->r_end);
    *(data->r_end) = 0; /* null-terminate the data bytes -- note sfio's sfbuf must be allocated with extra byte */
  }
  if (readlen == data->block_size) { /* full block read */
    PDC_NEED_AN_ELT("PDC_norec_read", elt, data->f_head, data->disc_vm);
    if (!elt) {
      PDC_WARN(pdc->disc, "PDC_norec_read memory alloc error");
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
    elt->unit = "empty block (EOF)";
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
    pdc->disc->errorf(NiL, 0, "XXX_REMOVE(%s %d disc_off %d)\n[%s]", elt->unit, elt->num, elt->disc_off, fmtquote(elt->begin, NiL, NiL, elt->len, 0));
  }
#endif
  (*next_elt_out) = elt;
  return PDC_OK;
}

PDC_IO_disc_t *
PDC_norec_make(size_t block_size_hint)
{
  Vmalloc_t          *disc_vm = 0;
  PDC_norec_data_t  *data;
  PDC_IO_disc_t      *io_disc;
  PDC_IO_elt_t       *f_head;
  size_t             block_size;

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
    sprintf(data->punit, "%dKB Block (last: partial)", block_size / 1024);
  } else {
    sprintf(data->unit,  "%dB Block", block_size);
    sprintf(data->punit, "%dB Block (last: partial)", block_size);
  }

  io_disc->unmake_fn    = PDC_norec_unmake;
  io_disc->sfopen_fn    = PDC_norec_sfopen;
  io_disc->sfclose_fn   = PDC_norec_sfclose;
  io_disc->read_fn      = PDC_norec_read;

  io_disc->name         = "norec";
  io_disc->descr        = "a raw bytes IO discipline that does not use EOR";
  io_disc->uses_eor     = 0;
  io_disc->data         = data;

  return io_disc;

 alloc_err:
  PDC_WARN(&PDC_default_disc, "PDC_norec_make: out of space");
  if (disc_vm) {
    vmclose(disc_vm);
  }
  return 0;
}

