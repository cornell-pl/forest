/*@FILE @LEFT union.switch.tex */

/*@BEGIN union.switch.tex */
Punion branches(Puint32 which) {
  Pswitch (which) {
    Pcase 1  : Pint32               number : number % 2 == 0;
    Pcase 2  : Pstring_SE(:"EOR":)  name;
    Pdefault : Pcompute Puint32     other = which; 
  }
}

Precord Pstruct choice{
  Puint32           which; 
  branches(:which:) branch; 
}
/*@END union.switch.tex */
