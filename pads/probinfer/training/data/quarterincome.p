#include "basetokens.p"

Punion word_t {
  PPint int1;
  PPword w1;
  PPpunc_colon colon1;
  PPpunc_comma comma1;
  PPpunc_dot dot1;
  PPwhite white1;
}

Parray text_t {
  word_t[] : Pterm('"');
}

Pstruct quoted_text {
  PPpunc_dquote q1;
  text_t text1;
  PPpunc_dquote q2;
}

Pstruct quarter_t {
        PPpunc_dquote dquote1;
        PPint myyear;
        PPpunc_dot dot1;
        PPint myquarter;
        PPpunc_dquote dquote2;
};

Parray quarters_t {
        quarter_t [] : Psep(',');
};

//Precord Ptypedef Pstring_SE(:Peor:) Eor_t;

Pstruct table_header_t {
        quoted_text message1;
        PPpunc_comma comma1;
        quoted_text message2;
        PPpunc_comma comma2;
        quoted_text message3;
        PPpunc_comma comma3;
        quarters_t my_quarters;
};

Parray incomeseq {
        PPint[]: Psep(',');
};

Pstruct my_entry_t {
        PPpunc_dquote dquote1;
        PPint int1;
        PPpunc_dquote dquote2;
        PPpunc_comma comma4;
        PPpunc_dquote dquote3;
        PPint int2;
        PPpunc_dquote dquote4;
        PPpunc_comma comma5;
        quoted_text text11;
        PPpunc_comma comma6;
        incomeseq incomes;
};

Pstruct tail_t {
       quoted_text message1;       
};

Precord Punion entry_t {
        table_header_t h;
        my_entry_t myentry;
        tail_t mytail;
};

Psource Pstruct quarter_income_t {
//        table_header_t h;
        entry_t[] es;
//        tail_t mytail;
//        Eor_t eor;
};
