/* *****************************************************************************
 * Mark Daly
 * June 2006
 * PADS_lang_pnode_tree - PNode container tree, used to map proper PADS AST
 *  to other (potentially non-isomorphic) trees
 * ************************************************************************** */

#include "PADS_lang.h"
#include "PADS_lang_pnode_tree.h"

PADSPNodeTreeNode::PADSPNodeTreeNode(PNodeP* newLinkNode, PADSPNodeTreeNode* newParent)
{
  treeType = PBT_PNODE;
  linkNode = newLinkNode;
  nodeType = linkNode->nodeType;
  linkNode->getTextualRepresentation(nodeName);
  parent = newParent;
  nextSibling = NULL;
  previousSibling = NULL;
  firstChild = NULL;
  updateParent = false;
  requiredElement = false;
}

PADSBasicTreeNode* PADSPNodeTreeNode::makeNewNode(void* pointerContents, PADSPNodeTreeNode* newParent)
{
  PADSPNodeTreeNode* newNode = new PADSPNodeTreeNode((PNodeP*)pointerContents, newParent);
  return newNode;
}

void PADSPNodeTreeNode::printTextualRepresentation(int spaceDepth, PSTRING& printStr)
{
  #ifdef PADS_LANG_DEBUG
  for(int i = 0; i < spaceDepth * PADS_PNODE_TREE_ADD_SPACE; i++)
    {
      DB_P(" ");
    }
  #endif
  printStr.append(spaceDepth * PADS_PNODE_TREE_ADD_SPACE, ' ');
  if(requiredElement)
    DB_P("*");
  DB_P("%s [%d]: %s\n", PT_TypeNames[nodeType], nodeType, nodeName.c_str());
  printStr.append(nodeName);
  printStr.append("\n");
  if(firstChild != NULL)
    firstChild->printTextualRepresentation(spaceDepth + 1, printStr);
  if(nextSibling != NULL)
    nextSibling->printTextualRepresentation(spaceDepth, printStr);
}

void PADSPNodeTreeNode::setUpdateParentOnChange(bool newVal)
{
  updateParent = newVal;
}

bool PADSPNodeTreeNode::getUpdateParentOnChange()
{
  return updateParent;
}

void PADSPNodeTreeNode::setElementIsRequired(bool newVal)
{
  requiredElement = newVal;
}

bool PADSPNodeTreeNode::getElementIsRequired()
{
  return requiredElement;
}
