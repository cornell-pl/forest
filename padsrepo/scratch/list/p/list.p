/* Precur list; */

/* Punion listSum{ */
/*   nil Pfrom('#'); */
/*   list next; */
/* }; */

/* Pstruct list{ */
/*   Pchar head; */
/*   listSum tail; */
/* }; */

/*
parameterized version:
Precur list(:Pchar c:);

Punion listSum(:Pchar c:){
  nil Pfrom('#');
  list(:c:) next;
};

Pstruct list(:Pchar c:){
  Pchar head : head < c;
  listSum(:c:) tail;
};

*/

Precur list;

Punion listSum(:char c:){
  Pswitch(c){
    Pcase '#' : Pcompute Pint8 nil = 0;
    Pdefault  : list next;
  }
};

Pstruct list{
  Pchar head;
  listSum(:head:) tail;
};


/* Pdatatype list; */

/* Pstruct cons_t(:Pchar c:){ */
/*   Pchar head : head < c; */
/*   list tail; */
/* }; */

/* Pdatatype list{ */
/*   nil Pfrom('#'); */
/*   cons_t cons; */
/* }; */

/* list_pd { */
/*   HDR_FIELDS */
/*   tag; */
/*   union* val; */
/* } */
