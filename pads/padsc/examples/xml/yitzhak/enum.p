
Penum foo {
    S_init Pfrom("while") = 10,
    S_care,
    S_tpv
};

Precord Pstruct bar {
  //  Pint32[] farr : Psep(':') && Pterm('|');
  Pint32[] farr : Psep(':');
  '|'; Pint16 f1;
  '|'; Pint32 f2;
  '|'; Pchar f3;
}

/*
Precord Pstruct bar {
  Pint16 f1;
  Pint32 f2;
  '|'; Pchar f3;
}
*/

Psource Parray barArray{
  bar[];
}

/*
Pstruct zab {
  bar z1;
  bar z2;
  foo z3;
}
*/
