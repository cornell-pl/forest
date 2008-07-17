#include "basetokens.p"

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

Precord Ptypedef Pstring_SE(:Peor:) Eor_t;

Precord Pstruct table_header_t {
        PPmessage message1;
        PPpunc_comma comma1;
        PPmessage message2;
        PPpunc_comma comma2;
        PPmessage message3;
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
        PPpunc_dquote dquote5;
        PPtext text11;
        PPpunc_dquote dquote6;
        PPpunc_comma comma6;
        incomeseq incomes;
};

Pstruct tail_t {
       PPmessage message1;       
};

Precord Punion entry_t {
        table_header_t h;
        my_entry_t myentry;
        tail_t mytail;
};

Psource Pstruct quarter_income_t {
//        table_header_t h;
        entry_t[] es: Pterm("\"Source:");
//        tail_t mytail;
//        Eor_t eor;
};
