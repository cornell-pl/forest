/*@FILE @LEFT binarytree.tex */
/*@BEGIN binarytree.tex */
Precur tree;

Pstruct fullTree{
  '(';
  tree left;
  ',';
  tree right;
  ')';
};

Precur Punion tree{
  Pint32     value;
  fullTree   nested;  
};

/*@END binarytree.tex */
