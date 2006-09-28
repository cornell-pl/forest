/* *****************************************************************************
 * Mark Daly
 * June 2006
 * PADS_lang_simple_tree - Parent class for generic tree types,
 *  supertype for tree types designed to map to AST nodes
 * ************************************************************************** */

#ifndef PADS_LANG_SIMPLE_TREE_H_INCLUDED__
#define PADS_LANG_SIMPLE_TREE_H_INCLUDED__

#define PADS_BASIC_TREE_ADD_SPACE 1

enum
  {
    PBT_BASE = 0,
    PBT_PNODE,
    PBT_UNIFIED,
  };

class PADSBasicTreeNode
{
 public:
  int treeType;

  int nodeType;
  PSTRING nodeName;

  PADSBasicTreeNode* parent;
  PADSBasicTreeNode* nextSibling;
  PADSBasicTreeNode* previousSibling;
  PADSBasicTreeNode* firstChild;

  PADSBasicTreeNode();
  ~PADSBasicTreeNode();
  virtual void printTextualRepresentation(int spaceDepth, PSTRING& printStr);

  virtual PADSBasicTreeNode* makeNewNode(void* pointerContents);
  virtual void setNodeName(PSTRING& nameStr);
};

#endif
