pstruct test {
  auint32 id : id < 100000;     /-- identity 
  '|';
  auint32 ts : ts == 11 * id;   /-- time stamp
  '\n';
};

pstruct test2 {
  test bigger;
  auint32 two;
};


