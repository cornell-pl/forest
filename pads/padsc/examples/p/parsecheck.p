Pstruct str{
  Puint32 foo;
} Pwhere {
  foo % 2 == 0 && Pparsecheck(foo % 3 == 0) && Pparsecheck(foo % 6 == 0);
};


Punion uni{
  Puint32 goo;
} Pwhere {
  goo % 2 == 0 && Pparsecheck(goo % 3 == 0) && Pparsecheck(foo % 6 == 0);
};
