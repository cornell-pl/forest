Pstruct test {
  Puint32 id : id < 100000;     /- identity 
  '|';
  Puint32 ts : ts == 11 * id;   /- time stamp
};

Precord Pstruct testtwo{
  test header : header.id == 12;
  '|';
  Puint32 f : f == header.ts;   
};
