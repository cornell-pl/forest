#define INTLIST_SIZE 4

Precord Parray intList {
  Puint32 [INTLIST_SIZE] : Psep('|');
} Pwhere {
  Pforall i Pin [0..length-2] {(intList[i] < intList[i+1]) && arrayEnd.offset > 0 }  && 
  Pgeneral(arrayEnd.offset - arrayBegin.offset > 10);
};


