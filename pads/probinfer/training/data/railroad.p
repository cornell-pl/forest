#include "basetokens.p"

Punion word_t
{
  PPurl url1;
  PPword w1;
  PPint i1;
  PPpunc_colon colon1;
  PPpunc_hyphen hy1;
  PPpunc_lpar lp1;
  PPpunc_rpar rp1;
  PPpunc_dquote q1;
  PPpunc_comma comma1;
  PPpunc_slash slash1;
  PPpunc_dot dot1;
  PPpunc_scolon sc1;
  PPpunc_equa eq1;
  PPwhite wh1;
}

Precord Parray entry_t 
{
  word_t[] : Pterm(Peor);
}

Psource Parray entries_t {
	entry_t[];
};
