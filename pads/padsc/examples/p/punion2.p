pstruct test {
  aint32 id : id < 100000;     //- identity 
  '|';
  aint32 ts : ts == 11 * id;   //- time stamp
};

record punion test2 {
  test bigger;
  aint32 two;
};


