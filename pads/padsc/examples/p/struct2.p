precord pstruct test {
       a_uint32 id : id < 100000;     //- identity 
  '|'; a_uint32 ts : ts == 11 * id;   //- time stamp
  compute a_uint32 nums = ts + id;
};


