Parray seq_t{
  Pint32 [] : Psep('|') && Plast(elts[current]) && Pterm('b');
} 

Precord Psruct entry{
  seq_t seq;
  'b'
}




