#define LIST_SIZE 10
Precord Parray intList {
  Pint32 [LIST_SIZE] : Psep == '|' && Pforall i Pin elts { elts[i] == intList[i]};
};
