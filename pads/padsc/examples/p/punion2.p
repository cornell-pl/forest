Pstruct test {
  Pa_int32 id : id < 100000;     //- identity 
  '|';
  Pa_int32 ts : ts == 11 * id;   //- time stamp
};

Precord Punion test2 {
  test       bigger;
  Pa_int32   two;
};


