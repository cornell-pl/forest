pstruct test {
  auint32 id : id < 100000;     //- identity 
  '|';
  auint32 ts : ts == 11 * id;   //- time stamp
  '\n';
};


#define RECLIST_SIZE 10
parray recList {
  test [RECLIST_SIZE] : forall i in recList { i < RECLIST_SIZE - 1 && recList[i].id < recList[i+1].id };
};


pstruct testtwo{
  auint32 id : id < 100000;     //- identity
  567;
  auint32 ts : ts == 11 * id;  //- time stamp
};


parray recListtwo{
  testtwo [] : sep == '\n';
};


#define MIN 2
#define MAX 5
parray intList {
  auint32 [MIN : MAX] : sep == '|' && term == '\n' &&
		   forall i in intList { i < length && intList[i] < intList[i+1]};
};


