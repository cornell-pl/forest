Parray seq_t {
  Pstring_ME(:"/[[:alnum:]]+/":) [] : Psep(Pre "/\s+/") && Plast(0 == Pstring_eq_cstr(&elts[current], "end"));
};

Precord Pstruct entry{
  seq_t seq;
};

