/*
 * Helpers
 *
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#include "cmhelpers.h"

void describe_params(CM_params *ps)
{
  int i;
  for (i = 0; i < ps->length; i++) {
    if (i) error(ERROR_PROMPT, ",");
    error(ERROR_PROMPT, "%lu", (unsigned long)ps->elts[i]);
  }
}

void describe_query(CM_query *q)
{
  error(ERROR_PROMPT, "%s = %s (", PDC_fmt_str(&(q->qy_id)), PDC_fmt_str(&(q->ty_id)));
  describe_params(&(q->params));
  error(ERROR_PROMPT, ") [offset: %lu, size: %lu]\n", (unsigned long)q->off, (unsigned long)q->sz);
}

void describe_queries(CM_queries *qs)
{
  int i;
  error(ERROR_PROMPT, "      QUERIES:\n");
  for (i = 0; i < qs->length; i++) {
    error(ERROR_PROMPT, "          ");
    describe_query(&(qs->elts[i]));
  }
}

void describe_c_cookie(CM_c_cookie *c)
{
  error(ERROR_PROMPT, "    C_COOKIE:\n");
  describe_queries(&(c->queries));
}

void describe_arm(CM_arm *a)
{
  error(ERROR_PROMPT, "  ARM %d\n", (int)a->s_val);
  describe_c_cookie(&(a->cookie));
}

void describe_s_cookie(CM_s_cookie *s)
{
  int i;
  error(ERROR_PROMPT, "S_COOKIE:\n");
  error(ERROR_PROMPT, "  SWITCH:\n");
  describe_query(&(s->s_qy));
  for (i = 0; i < s->arms.length; i++) {
    describe_arm(&(s->arms.elts[i]));
  }
}

void describe_cookie(CM_cookie *cspec)
{
  switch (cspec->tag) {
  case CM_cookie_err:
    error(ERROR_PROMPT, "Invalid tag\n");
    break;
  case c_cookie:
    describe_c_cookie(&(cspec->val.c_cookie));
    break;
  case s_cookie:
    describe_s_cookie(&(cspec->val.s_cookie));
    break;
  }
}

