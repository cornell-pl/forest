#define INTLIST_SIZE 4

Precord Parray intList {
  Puint32 [INTLIST_SIZE] : Psep('|');
} Pwhere {
  Pforall (i Pin [0..length-2] : (intList[i] < intList[i+1]))  && 
  Pparsecheck(arrayEnd.offset - arrayBegin.offset > 10)        &&
  Pforall (i Pin [0..length-1] : (intList[i] % 2 == 0))        && 
    length > 0 ? intList[0] > 2 : 1;
};


