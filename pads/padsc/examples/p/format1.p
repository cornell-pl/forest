precord pstruct test {
  a_int32 id : id < 100000;     //- identity 
  '|';
  a_int32 ts : ts == 11 * id;   //- time stamp
};

pstruct test2 {
  test bigger;
  a_int32 two;
};


