pstruct test {
  auint32 id : id < 100000;     //- identity 
  '|';
  auint32 ts : ts == 11 * id;   //- time stamp
};

precord pstruct testtwo{
  test header;
  '|';
  auint32 f : f == header.ts;   
};
