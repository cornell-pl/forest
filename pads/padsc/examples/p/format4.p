pstruct test(a_uint32 h, int x) {
  a_uint32 id : id == h * x;     //- identity 
};

#define RECLIST_SIZE 10
parray recList(a_uint32 h, int x, int size) {
  test(:h,x:) [size] : forall i in [0 .. length - 2]
       {  i==x || recList[i].id < recList[i+1].id };
};


parray recList2(a_uint32 h, int x, int size) {
  test(:h,x:) [size] : forall i in recList2  {  recList2[i].id % 2 == 0};
};


pstruct test2 {
  a_uint32 h;
  '|';
  a_uint32 s;
  '|';
  test(:h,10:) i;
#if 0
  a_string_FW(:10:) fw;
  a_string(:'|':) v;     //- stop char
  '|';
#endif
};


