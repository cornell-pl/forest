
pstruct test {
  auint32 id : id < 100000; 
  '|';
  auint32 ts : ts > id;
};

/*
 pstruct test2 {
  test bigger;
  auint32 two;
};
*/

