/*
pstruct test {
  auint32 id : id < 100000;     /-- identity 
  '|';
  auint32 ts : ts == 11 * id;   /-- time stamp
  '\n';
};

#define RECLIST_SIZE 10
parray recList {
  test [RECLIST_SIZE] : forall i in test { i < RECLIST_SIZE && test[i] < test[i+1] };
};

*/
pstruct testtwo{
  auint32 id : id < 100000;     /-- identity
  '|';
  auint32 ts : ts == 11 * id;  /-- time stamp
};


parray recListtwo{
  testtwo [] : sep == '\n';
};

parray recListthree{
  auint32 [1 : ] : sep == '|' && term == '\n';
};
