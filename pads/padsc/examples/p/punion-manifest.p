pstruct str1 {
  a_int32 id : id < 100000;     /-- identity 
  '|';
  a_int32 ts : ts == 11 * id;   /-- time stamp
};

punion choice {
  str1 bigger;
  compute a_int32 first = 12; /-- default value
  a_int32 two;
};

precord pstruct str2{
  choice c;
  ' ';
  a_int32 t;
  compute a_int32 v = 11; /-- struct manifest field
}


