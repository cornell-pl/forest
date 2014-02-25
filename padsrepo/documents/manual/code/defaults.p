/*@FILE @LEFT union.switch.default.tex union.default.tex*/

/*@BEGIN union.switch.default.tex */
Punion body_t(:Puint8 j:) {
  Pswitch (j) {
    Pcase 0 : Puint32 i;
    Pcase 1 : Pchar   c;
    Pdefault : Pempty;
  }
};

Precord Pstruct entry_t {
  Puint8 i;
  ':';
  body_t(:i:) body;
};
/*@END union.switch.default.tex */

/*@BEGIN union.default.tex */
Punion Choice_t {
  Puint32 i;
  Pempty; 
};
/*@END union.default.tex */
