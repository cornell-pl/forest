record pstruct test {
       auint32 id : id < 100000;     //- identity 
  '|'; auint32 ts : ts == 11 * id;   //- time stamp
  compute auint32 nums = ts + id;
};


