/*@FILE @LEFT  seq_t.tex sorted_t.tex*/

/*@BEGIN seq_t.tex */
Parray seq_t{
  Pb_int32 [] : Plast(elts[current] > 10);
};
/*@END seq_t.tex */

/*@BEGIN sorted_t.tex */
Precord Pstruct elem_t{
  Puint32 id;
  '|';
  Puint32 val;
};

Parray sorted_t(: Pint32 size :) {
  elem_t [size];
} Pwhere {
  Pforall( i Pin [0..length-2] : elts[i].id < elts[i+1].id );
};
/*@END sorted_t.tex */



