pstruct test {
  a_uint32 id : id < 100000;     //- identity 
  '|';
  a_uint32 ts : ts == 11 * id;   //- time stamp
  '\n';
};

#define RECLIST_SIZE 10
parray recList {
  test [RECLIST_SIZE] : forall i in [0..length-2] { recList[i].id < recList[i+1].id };
};


pstruct testtwo{
  a_uint32 id : id < 100000;     //- identity
  '|';
  a_uint32 ts : ts == 11 * id;  //- time stamp
};


parray recListtwo{
  testtwo [] : sep == '\n';
};


#define MIN 2
#define MAX 5
parray intList {
  a_uint32 [MIN : MAX] : sep == '|' && term == '\n' &&
		   forall i in [0 .. length -2] {intList[i] < intList[i+1]};
};

