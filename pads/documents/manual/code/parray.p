/*@FILE @LEFT  seq_t.tex sorted_t.tex nosep.tex last.tex size.tex longest.tex ended.tex array-omit.tex array-forall.tex array-inplace.tex*/

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


/*@BEGIN nosep.tex */
Parray nIP2 {
  Puint8 [4] : Psep('.') && Pterm(Pnosep);
};
/*@END nosep.tex */

/*@BEGIN last.tex */
Pstruct character_string {
  Psbh_uint8(:1:)        length;
  Pa_string_FW(:length:) bytes;
};

Parray TXT_t(:Puint16 rdlength:) {
  character_string [] : Plast(Pparsecheck(eltEnd.offset - begin.offset >= rdlength));
};
/*@END last.tex */


/*@BEGIN size.tex */
Parray list(:Puint32 min, Puint32 max:) {
  Pint32 [min : max] : Psep('|');
};
/*@END size.tex */

/*@BEGIN longest.tex */
Ptypedef Pint32 even_t : even_t x => {x % 2 == 0};
Ptypedef Pint32 odd_t  : odd_t  x => {x % 2 == 1};

Parray eseq_t {
  even_t [1:] : Psep('a') && Plongest;
};

Parray oseq_t {
  odd_t [1:] :  Psep('a') && Plongest && Pterm('b');
};

Precord Pstruct entry{
  eseq_t evens;
  'a';
  oseq_t odds;
  'b';
};
/*@END longest.tex */

/*@BEGIN ended.tex */
int isDone(Pint32 value, Pbase_pd p, int *consume){
  if (p.errCode != P_NO_ERR) return 0;  // continue sequence
  if (value == 1) {
    *consume = 1;       // consume value
    return 1;           // terminate sequence
  };
  if (value == -1){     
    *consume = 0;       // return value to input
    return 1;           // terminate sequence
  };
  return 0;             // continue sequence
};

Parray fseq_t {
  Pint32 [] : Psep(',') 
              && Pended(Pparsecheck(isDone(fseq_t[current], pds[current], &consume)));
};
/*@END ended.tex */

/*@BEGIN array-omit.tex */
Parray nseq_t{
  Pint32 [:4] : Psep(' ') && Pomit(elt < 0) && Pterm(Peor);
};
/*@END array-omit.tex */

#define INTLIST_SIZE 10
/*@BEGIN array-forall.tex */
Precord Parray intList {
  Puint32 [INTLIST_SIZE] : Psep('|');
} Pwhere {
    Pforall( i Pin [0..length-2] : (intList[i] < intList[i+1]) )  && 
    Pparsecheck(end.offset - begin.offset > 10);       
};
/*@END array-forall.tex */

/*@BEGIN array-inplace.tex */
 Pstruct log_t {
  Puint8 [4] ip  : Psep('.') && Pterm(Pnosep); /- resolved ip address
  '|';
  Puint32 numBytes;
};
/*@END array-inplace.tex */

