pstruct str1 {
  aint32 id : id < 100000;     //- identity 
  '|';
  aint32 ts : ts == 11 * id;   //- time stamp
};

punion choice {
  str1 bigger;
  compute aint32 first = 12; //- default value
  aint32 two;
};

precord pstruct str2{
  choice c;
  ' ';
  aint32 t;
  compute aint32 v = 11; //- struct manifest field
}


