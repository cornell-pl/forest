#include "wsl.h"

#define DEF_INPUT_FILE "../../data/ai.big"

// fmt function used to override the default Pip fmt function
ssize_t
my_Pip_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len,
	       int *buf_full, int *requested_out, char const *delims,
	       void *m, void  *pd, void *rep, va_list type_args)
{
  Pip     *t_rep = (Pip*)    rep;
  Sfio_t     *tmpstr;
  ssize_t     res;

  (*requested_out) = 1;
  if (!(tmpstr = sfstropen ())) { 
    return -1;
  }
  res = sfprintf(tmpstr, "%d.%d.%d.%d", t_rep->elts[0], t_rep->elts[1],
		 t_rep->elts[2],t_rep->elts[3]);
  if (res > buf_len) { // not enough space in buffer
    (*buf_full) = 1;
    return -1;
  }
  memcpy(buf, sfstruse(tmpstr), res);
  return res;
}

// fmt function used to override the default Phostname fmt function
ssize_t
my_Phostname_fmt2buf(P_t *pads, Pbyte *buf, size_t buf_len,
	       int *buf_full, int *requested_out, char const *delims,
	       void *m, void  *pd, void *rep, va_list type_args)
{
  Phostname  *t_rep = (Phostname*)    rep;
  Sfio_t     *tmpstr;
  ssize_t     res = 0;
  int         i;

  (*requested_out) = 1;
  if (!(tmpstr = sfstropen ())) { 
    return -1;
  };
  for (i = 0; i < t_rep->length - 1; i++){
    Pstring_preserve(pads, &t_rep->elts[i]);
    res += sfprintf(tmpstr, "%s.", t_rep->elts[i].str);
    if (res > buf_len) { // not enough space in buffer
      (*buf_full) = 1;
      return -1;
    };
  };
  Pstring_preserve(pads, &t_rep->elts[t_rep->length - 1]);
  res += sfprintf(tmpstr, "%s", t_rep->elts[t_rep->length -1].str);

  memcpy(buf, sfstruse(tmpstr), res);
  return res;
}

//#define MASK_MOD   P_Dont_Write(m.compoundLevel);
//#define MASK_MOD   P_Dont_Write(m.header.c); P_Do_WriteVoid(m.header.c); P_Dont_Write(m.header.ts); P_Do_WriteVoid(m.header.ts);
//#define MASK_MOD   P_Dont_Write(m.header.compoundLevel);

#define FMT_OVERRIDE_TY1   "Pip"
#define FMT_OVERRIDE_FN1   my_Pip_fmt2buf
#define FMT_OVERRIDE_TY2   "Phostname"
#define FMT_OVERRIDE_FN2   my_Phostname_fmt2buf

//#define DATE_OUT_FMT "%K"

#define DATE_OUT_FMT "%D:%T"
#define PADS_TY(suf) entry_t ## suf
#define DELIMS "|"
#include "template/read_format.h"
