Ptypedef Pchar digit :: digit x => { x >= '0' && x <= '9'};

float my_is_digit (float rep)
{
  return 1.0;
}

Pcharclass digit {is_digit};

Precord Pstruct entry{
  Pre "/[[:digit:]]+/";
  Pchar c;
};
 
