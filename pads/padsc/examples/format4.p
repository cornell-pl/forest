pstruct test(auint32 h, auint32 s) {
  auint32 id : id == h * s;     //- identity 
};

pstruct test2 {
  auint32 h;
  '|';
  auint32 s;
  '|';
  test(:h,s:) i;
  /*
  astringFW(:10:) fw;
  astring(:'|':) v;     //- stop char
  '|';
   */
};


