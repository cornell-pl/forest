pstruct test {
  a_uint32 id : id < 100000;     //- identity 
  '|';
  a_uint32 ts : ts == 11 * id;   //- time stamp
};

precord pstruct testtwo{
  test header;
  '|';
  a_uint32 f : f == header.ts;   
};
