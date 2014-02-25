/* *****************************************************************************
 * Mark Daly
 * June 2006
 * PADS_lang_pnode_tree - 'unified' tree, designed to represent an instance of 
 *  parsed data in the format corresponding to an AST from which this tree
 *  is derived - each child element of an AST node should be replaced with
 *  the full tree corresponding to the type of that child node (i.e. all
 *  elements of a composite type that are themselves composite are replaced 
 *  with the full trees corresponding to those types, resulting in a 'unified' 
 *  tree where all branch nodes are of elements of composite type and all leaf
 *  nodes are elements of base types).  This form should be isomorphic to the
 *  tree structure of data that conforms to the AST of the corresponding 
 *  PADS definition from which the parser for that data format was built.
 * ************************************************************************** */

#ifndef PADS_LANG_UNI_TREE_H_INCLUDED__
#define PADS_LANG_UNI_TREE_H_INCLUDED__

#include "PADS_lang.h"
#include "PADS_lang_val_container.h"
#include "PADS_lang_simple_tree.h"
#include "PADS_lang_val_container.h"

#define PADS_UNI_TREE_ADD_SPACE 3

class PADSUniTreeNode : public PADSBasicTreeNode
{
 public:
  PNodeP* linkNode;
  bool hasError;
  
  PSTRING varName;
  PSTRING typeName;

  PADSValContainer valContainer;

  PADSUniTreeNode(PNodeP* newLinkNode, PSTRING& newVarName, PADSUniTreeNode* newParent);

  void printTextualRepresentation(int spaceDepth, PSTRING& printStr);

  void setHasError(bool newError);
  bool getHasError();

  void setTypeName(PSTRING& newType);
  void getTypeName(PSTRING& getType);

  void setLinkNode(PNodeP* newLink);
  PNodeP* getLinkNode();

  void getThisNodeTextualRepresentation(PSTRING& printStr);

  int insertIntoTypePathMap(PADSTypeMap* pathMap, PSTRING& pathStr, int elementAtLevel);
  int insertIntoNamePathMap(PADSTypeMap* pathMap, PSTRING& pathStr, int elementAtLevel);
};

#endif
