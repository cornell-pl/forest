pstruct test(auint32 h, int x) {
  auint32 id : id == h * x;     //- identity 
};

#define RECLIST_SIZE 10
parray recList(auint32 h, int x, int size) {
  test(:h,x:) [size] : forall i in [0 .. length - 2]
       {  i==x || recList[i].id < recList[i+1].id };
};


pstruct test2 {
  auint32 h;
  '|';
  auint32 s;
  '|';
  test(:h,10:) i;
#if 0
  astringFW(:10:) fw;
  astring(:'|':) v;     //- stop char
  '|';
#endif
};


