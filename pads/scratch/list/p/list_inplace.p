Precur list;

Punion foo{
  Pint32 f1;
  Pchar f2;
};

Punion listSum{
  nil Pfrom('#');
  list next;
};

Pstruct list{
  Pchar head;
  listSum tail;
};
