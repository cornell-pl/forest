Pstruct entry(:int len:){
  Puint32 foo;
} Pwhere {
  foo % 2 == 0 && Pparsecheck(structEnd.offset - structBegin.offset == len);
};


Precord Punion uni(:int len:){
  Puint32 goo;
} Pwhere {
  2 % 2 == 0 && Pparsecheck(unionEnd.offset - unionBegin.offset == len);
};


Punion suni(:int len:){
  Pswitch(len){
  Pcase 1 : Pint32  number : number %2 == 0;
  } 
} Pwhere{
  2 % 2 == 0 && Pparsecheck(unionEnd.offset - unionBegin.offset == len);
}
