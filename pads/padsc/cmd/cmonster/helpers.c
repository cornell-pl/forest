/*
 * Helpers
 *
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#include "helpers.h"

void describe_query(CM_query *q)
{
  PDC_uint32 len = q->params.length;
  int ok_switch_type;
  PDC_uint32 i;

  ok_switch_type = (q->entry->sval_fn) ? 1 : 0;

  error(ERROR_PROMPT, "%s = %s (", PDC_fmt_str(&(q->qy_id)), PDC_fmt_str(&(q->ty_id)));
  for (i = 0; i < len; i++) {
    if (i) error(ERROR_PROMPT, ",");
    error(ERROR_PROMPT, "%lu", (unsigned long)q->params.elts[i]);
  }
  error(ERROR_PROMPT, ") [offset: %lu]", (unsigned long)q->off);
  error(ERROR_PROMPT, " type: %s, ok_switch_type: %d, out_sz: %lu\n",
	q->entry->tname, ok_switch_type, (unsigned long)q->out_sz);
}

void describe_queries(CM_queries *qs)
{
  PDC_uint32 len = qs->length;
  PDC_uint32 i;
  error(ERROR_PROMPT, "          QUERIES:\n");
  for (i = 0; i < len; i++) {
    error(ERROR_PROMPT, "            ");
    describe_query(&(qs->elts[i]));
  }
}

void describe_c_cookie(CM_c_cookie *c)
{
  size_t out_sz = out_sz_c_cookie(c);
  error(ERROR_PROMPT, "        C_COOKIE (out_sz: %ld):\n", (long)out_sz);
  describe_queries(&(c->queries));
}

void describe_s_cookie(CM_s_cookie *s)
{
  PDC_uint32 len = s->arms.length;
  PDC_uint32 i;
  size_t out_sz = out_sz_s_cookie(s);
  error(ERROR_PROMPT, "  S_COOKIE (out_sz: %ld):\n", (long)out_sz);
  error(ERROR_PROMPT, "    SWITCH:\n");
  error(ERROR_PROMPT, "            ");
  describe_query(&(s->s_qy));
  for (i = 0; i < len; i++) {
    error(ERROR_PROMPT, "      ARM %d:\n", (int)s->arms.elts[i].s_val);
    describe_c_cookie(&(s->arms.elts[i].cookie));
  }
}

void describe_cookie(CM_cspec *cspec)
{
  switch (cspec->cookie.tag) {
  case CM_c_or_s_err:
    error(ERROR_PROMPT, "  Invalid c_or_s tag\n");
    break;
  case c_cookie:
    describe_c_cookie(&(cspec->cookie.val.c_cookie));
    break;
  case s_cookie:
    describe_s_cookie(&(cspec->cookie.val.s_cookie));
    break;
  }
}

size_t out_sz_c_cookie(CM_c_cookie *c)
{
  PDC_uint32 len = c->queries.length;
  size_t res = 0;
  PDC_uint32 i;
  for (i = 0; i < len; i++) {
    res += (size_t)c->queries.elts[i].out_sz;
  }
  return res;
}

size_t out_sz_s_cookie(CM_s_cookie *s)
{
  PDC_uint32 len = s->arms.length;
  PDC_uint32 i;
  size_t res, res1;

  res = out_sz_c_cookie(&(s->arms.elts[0].cookie));
  for (i = 1; i < len; i++) {
    res1 = out_sz_c_cookie(&(s->arms.elts[i].cookie));
    if (res1 > res) {
      res = res1;
    }
  }
  res += s->s_qy.out_sz;
  return res;
}

size_t out_sz_cookie(CM_cspec *cspec)
{
  switch (cspec->cookie.tag) {
  case CM_c_or_s_err:
    break;
  case c_cookie:
    return out_sz_c_cookie(&(cspec->cookie.val.c_cookie));
  case s_cookie:
    return out_sz_s_cookie(&(cspec->cookie.val.s_cookie));
  }
  return -1;
}

PDC_error_t rw_c_cookie(CM_t *cm, CM_c_cookie *c, PDC_byte *begin, PDC_byte *end)
{
  PDC_uint32 len = c->queries.length;
  PDC_uint32 i;
  for (i = 0; i < len; i++) {
    if (PDC_ERR == c->queries.elts[i].entry->rw_fn(cm, &(c->queries.elts[i]), begin, end)) {
      return PDC_ERR;
    }
  }
  return PDC_OK;
}

PDC_error_t rw_s_cookie(CM_t *cm, CM_s_cookie *s, PDC_byte *begin, PDC_byte *end)
{
  PDC_uint32 len = s->arms.length;
  PDC_uint32 i;
  PDC_int32  sval;
  if (PDC_ERR == s->s_qy.entry->sval_fn(cm, &(s->s_qy), begin, end, &sval)) {
    /* failed to read switch value */
    return PDC_ERR;
  }
  for (i = 0; i < len; i++) {
    if (sval == s->arms.elts[i].s_val) {
      /* found matching switch arm */
      return rw_c_cookie(cm, &(s->arms.elts[i].cookie), begin, end);
    }
  }
  /* did not find matching switch arm */
  return PDC_ERR;
}

int CM_calc_out_sz(CM_query *q, PDC_int32 out_val)
{
  if (!out_val) return 1; /* leave out_sz == 0 */
  q->out_sz = q->entry->out_sz_fn(q);
  return 1;
}

size_t CM_out_sz_1(CM_query *qy)
{
  return 1;
}

size_t CM_out_sz_2(CM_query *qy)
{
  return 2;
}

size_t CM_out_sz_4(CM_query *qy)
{
  return 4;
}

size_t CM_out_sz_8(CM_query *qy)
{
  return 8;
}

size_t CM_out_sz_p1(CM_query *qy)
{
  return (size_t)(qy->params.elts[0]);
}

