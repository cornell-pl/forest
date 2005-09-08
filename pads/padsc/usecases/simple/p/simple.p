Precord Pstruct simple_types {
  Ptimestamp(:'|':) birthtimestamp; '|';
  Ptime(:'|':) birthtime; '|';
  Pdate(:'|':) birthdate : birthtime + birthdate == birthtimestamp; '|';
  Pint32 num_accts; '|';
  Pstring_SE(:"/$/":) name;
};
Psource Parray simple_example {
  simple_types [];
};
