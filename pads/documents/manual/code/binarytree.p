/*@FILE @LEFT binarytree.tex */
/*@BEGIN binarytree.tex */
Precur tree;

Pstruct treeInterior {
  tree left;
  ',';
  tree right;
};

Pstruct tree{
  '(';
  Popt treeInterior;
  ')';
};
/*@END binarytree.tex */
