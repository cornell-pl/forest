Punion branches(Pa_uint32 a) {
  switch (a) {
  case 1  : Pa_int32               number : number % 2 == 0;
  case 2  : Pa_string_SE(:"EOR":)  name;
  default : Pcompute PDC_int32     def = 3; 
  }
}

Precord Pstruct choice{
  Pa_uint32       a; 
  branches(:a:)   b; 
}
