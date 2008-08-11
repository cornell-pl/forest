#include "./basetokens.p"

Punion word_t
{
  PPurl url1;
  PPword w1;
  PPint i1;
  PPpunc_colon colon1;
  PPpunc_hyphen hy1;
  PPpunc_lpar lp1;
  PPpunc_rpar rp1;
//  PPpunc_dquote q1;
//  PPpunc_comma comma1;
  PPpunc_slash slash1;
  PPpunc_dot dot1;
  PPpunc_scolon sc1;
  PPpunc_equa eq1;
  PPwhite wh1;
}

Parray text_t {
              word_t[] : Pterm(',');
}
/*
Parray text_t_efl {
              word_t[] : Pterm(Peor);
}
*/
Punion word_comma {
       word_t w90;
       PPpunc_comma c90;
                  }

Parray wct {
       word_comma[] : Pterm('"');
}

Pstruct q_text {
        PPpunc_dquote dquote1;
        wct text_t1;
        PPpunc_dquote dquote2;
}

Punion field {
        q_text q_text1;
        text_t text_t2;
}

Precord Pstruct entry_t 
{
  Popt field f1;
  PPpunc_comma c1;
  Popt field f2;
  PPpunc_comma c2;
  Popt field f3;
  PPpunc_comma c3;
  Popt field f4;
  PPpunc_comma c4;
  Popt field f5;
  PPpunc_comma c5;
  Popt field f6;
  PPpunc_comma c6;
  Popt field f7;
  PPpunc_comma c7;
  Popt field f8;
  PPpunc_comma c8;
  Popt field f9;
  PPpunc_comma c9;
  Popt field f10;
  PPpunc_comma c10;
  Popt field f11;
  PPpunc_comma c11;
  Popt field f12;
  PPpunc_comma c12;
  Popt field f13;
  PPpunc_comma c13;
  Popt field f14;
  PPpunc_comma c14;
  Popt field f15;
  PPpunc_comma c15;
Popt word_t f16;
}

Psource Parray entries_t {
	entry_t[];
};
