/*@FILE @LEFT union.switch.tex union.option.tex*/

/*@BEGIN union.switch.tex */
Punion branches(:Puint32 which:) {
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

/*@BEGIN union.option.tex */
Punion intOpt(:Puint32 defVal:) {
  Puint32           val;
  Pcompute Puint32  def = defVal; 
}

Pstruct data {
        intOpt(:0:)  field1;
  '|';  intOpt(:0:)  field2;
  '|';  intOpt(:-1:) field3;
/*@INSERT
     ...
*/
}
/*@END union.option.tex */
