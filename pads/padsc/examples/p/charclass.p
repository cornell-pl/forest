Ptypedef Pchar digit :: digit x => { x > '0' && x < '9'};

int my_is_digit (char rep)
{
  return ((rep)>48)&&((rep)<57);
}


Pcharclass digit {my_is_digit};


Precord Pstruct entry{
  Pre "/[[:digit:]]+/";
  Pchar c;
};
 
