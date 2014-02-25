/* *****************************************************************************
 * Mark Daly
 * June 2006
 * PADS_lang_simple_tree - Parent class for generic tree types,
 *  supertype for tree types designed to map to AST nodes
 * ************************************************************************** */

#include "PADS_lang.h"
#include "PADS_lang_simple_tree.h"

PADSBasicTreeNode::PADSBasicTreeNode()
{
  treeType = PBT_BASE;
  nodeType = 0;
  nodeName = "";
  parent = NULL;
  nextSibling = NULL;
  previousSibling = NULL;
  firstChild = NULL;
}

PADSBasicTreeNode::~PADSBasicTreeNode()
{
  if(firstChild != NULL)
    delete firstChild;
  if(nextSibling != NULL)
    delete nextSibling;
}

void PADSBasicTreeNode::printTextualRepresentation(int spaceDepth, PSTRING& printStr)
{
  #ifdef PADS_LANG_DEBUG
  for(int i = 0; i < spaceDepth * PADS_BASIC_TREE_ADD_SPACE; i++)
    {
      DB_P(" ");
    }
  #endif
  printStr.append(spaceDepth * PADS_BASIC_TREE_ADD_SPACE, ' ');
  DB_P("%d: %s\n", nodeType, nodeName.c_str());
  printStr.append(nodeName);
  printStr.append("\n");
  if(firstChild != NULL)
    firstChild->printTextualRepresentation(spaceDepth + 1, printStr);
  if(nextSibling != NULL)
    nextSibling->printTextualRepresentation(spaceDepth, printStr);
}

PADSBasicTreeNode* PADSBasicTreeNode::makeNewNode(void* pointerContents)
{
  PADSBasicTreeNode* newNode = new PADSBasicTreeNode();
  return newNode;
}

void PADSBasicTreeNode::setNodeName(PSTRING& nameStr)
{
  nodeName = nameStr;
}
