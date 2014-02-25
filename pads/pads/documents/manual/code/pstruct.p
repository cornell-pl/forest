/*@FILE @LEFT struct.computedfield.tex struct.whereclause.tex struct.parameters.tex*/

/*@BEGIN struct.computedfield.tex */
Pstruct computeExample(:int offset:){
  Pint32 base; 
  Pcompute int index = base + offset;
};
/*@END struct.computedfield.tex */

/*@BEGIN struct.whereclause.tex */
Pstruct whereExample(:int limit:){
  Pint32 first; 
  Pint32 second;
} Pwhere {first + second < limit;};
/*@END struct.whereclause.tex */

/*@BEGIN struct.parameters.tex */
Pstruct formalParamExample(:Pint32 limit:){
  Pint32 data  :  data <= limit; 
} 

Pstruct actualParamExample{
  Pint32 limit;
  formalParamExample(:limit:) nestedData;
}
/*@END struct.parameters.tex */
