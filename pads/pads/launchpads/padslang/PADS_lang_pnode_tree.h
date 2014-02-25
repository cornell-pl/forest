/* *****************************************************************************
 * Mark Daly
 * June 2006
 * PADS_lang_pnode_tree - PNode container tree, used to map proper PADS AST
 *  to other (potentially non-isomorphic) trees
 * ************************************************************************** */

#ifndef PADS_LANG_PNODE_TREE_H_INCLUDED__
#define PADS_LANG_PNODE_TREE_H_INCLUDED__

#include "PADS_lang_simple_tree.h"

#define PADS_PNODE_TREE_ADD_SPACE 3

class PADSPNodeTreeNode : public PADSBasicTreeNode
{
 public:
  PNodeP* linkNode;
  bool updateParent; //update parent node on change
  bool requiredElement;

  PADSPNodeTreeNode(PNodeP* newLinkNode, PADSPNodeTreeNode* newParent);
  PADSBasicTreeNode* makeNewNode(void* pointerContents, PADSPNodeTreeNode* newParent);
  void printTextualRepresentation(int spaceDepth, PSTRING& printStr);
  void setUpdateParentOnChange(bool newVal);
  bool getUpdateParentOnChange();

  void setElementIsRequired(bool newVal);
  bool getElementIsRequired();
};

#endif
