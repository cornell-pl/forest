
Parray seq_t {
  Pint32 [] : Psep ==  Pre "/a/" Pand Pterm == "b";
}

Precord Pstruct entry{
  seq_t seq;
  "b"; 
}
