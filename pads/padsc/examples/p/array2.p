#define LIST_SIZE 10
precord parray intList {
  a_int32 [LIST_SIZE] : sep == '|' && forall i in elts { elts[i] == intList[i]};
};
