/*@FILE @LEFT  simple-opt.tex simple-inline-opt.tex constraint-opt.tex constraint-inline-opt.tex */

/*@BEGIN simple-opt.tex */
Popt Puint32 oPuint32;

Precord Pstruct entry1{
  oPuint32 f;
  '|';
  oPuint32 g;
}
/*@END simple-opt.tex */

/*@BEGIN simple-inline-opt.tex */
Precord Pstruct entry2{
  Popt Puint32 f;
  '|';
  Popt Puint32 g;
}
/*@END simple-inline-opt.tex */

/*@BEGIN constraint-opt.tex */
Popt Puint32 even_t : Psome i => {i % 2 == 0};  
Popt Puint32 odd_t  : Psome i => {i % 2 != 0};

Precord Pstruct entry3{
  even_t x1;
  odd_t  x2;
  '|';
  even_t y1;
  odd_t  y2;
  '|';
};
/*@END constraint-opt.tex */

/*@BEGIN constraint-inline-opt.tex */
Precord Pstruct entry4{
  Popt Puint32 x1 : Psome i => { i % 2 == 0};
  Popt Puint32 x2 : Psome i => { i % 2 != 0};
  '|';
  Popt Puint32 y1 : Psome i => { i % 2 == 0};
  Popt Puint32 y  : Psome i => { i % 2 != 0};
  '|';
};
/*@END constraint-inline-opt.tex */
