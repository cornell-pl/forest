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

#include "PADS_lang_unified_tree.h"
#include "PADS_lang.h"
#include "PADS_lang_helper_classes.h"
#include "PADS_lang_val_container.h"

PADSUniTreeNode::PADSUniTreeNode(PNodeP* newLinkNode, PSTRING& newVarName, PADSUniTreeNode* newParent)
{
  linkNode = newLinkNode;
  treeType = PBT_UNIFIED;
  parent = newParent;
  nextSibling = NULL;
  previousSibling = NULL;
  firstChild = NULL;
  hasError = false;
  varName = newVarName;
  typeName = PT_TypeNames[newLinkNode->nodeType];
}

void PADSUniTreeNode::printTextualRepresentation(int spaceDepth, PSTRING& printStr)
{
#ifdef PADS_LANG_DEBUG
  for(int i = 0; i < spaceDepth * PADS_UNI_TREE_ADD_SPACE; i++)
    {
      DB_P(" ");
    }
  #endif
  printStr.append(spaceDepth * PADS_UNI_TREE_ADD_SPACE, ' ');
  DB_P("%s ::%s", varName.c_str(), typeName.c_str());
  if(linkNode != NULL)
    {
      DB_P(" [::%s]", PT_TypeNames[linkNode->nodeType]);
    }
  DB_P("\n");
  printStr.append(varName);
  printStr.append("\n");
  if(firstChild != NULL)
    firstChild->printTextualRepresentation(spaceDepth + 1, printStr);
  if(nextSibling != NULL)
    nextSibling->printTextualRepresentation(spaceDepth, printStr);
}

void PADSUniTreeNode::setHasError(bool newError)
{
  hasError = newError;
}

bool PADSUniTreeNode::getHasError()
{
  return hasError;
}

void PADSUniTreeNode::setTypeName(PSTRING& newType)
{
  typeName = newType;
}

void PADSUniTreeNode::getTypeName(PSTRING& getType)
{
  getType = typeName;
}

void PADSUniTreeNode::setLinkNode(PNodeP* newLink)
{
  linkNode = newLink;
}

PNodeP* PADSUniTreeNode::getLinkNode()
{
  return linkNode;
}

void PADSUniTreeNode::getThisNodeTextualRepresentation(PSTRING& printStr)
{
  printStr.append(typeName);
  printStr.append(" ");
  printStr.append(varName);
}

int PADSUniTreeNode::insertIntoTypePathMap(PADSTypeMap* pathMap, PSTRING& pathStr, int elementAtLevel)
{
  PSTRING newPathStr = pathStr;
  char sbuf[16];
  sprintf(sbuf, "#%d:",elementAtLevel);
  newPathStr.append(typeName);
  newPathStr.append(sbuf);
  int retVal = pathMap->insert(newPathStr, linkNode);

  if(firstChild != NULL)
    ((PADSUniTreeNode*)firstChild)->insertIntoTypePathMap(pathMap, newPathStr, 0);
  if(nextSibling != NULL)
    ((PADSUniTreeNode*)nextSibling)->insertIntoTypePathMap(pathMap, pathStr, elementAtLevel+1);

  return retVal;
}

int PADSUniTreeNode::insertIntoNamePathMap(PADSTypeMap* pathMap, PSTRING& pathStr, int elementAtLevel)
{
  PSTRING newPathStr = pathStr;
  //char sbuf[16];
  //sprintf(sbuf, "#%d:",elementAtLevel);
  newPathStr.append(varName);
  newPathStr.append(":");
  //newPathStr.append(sbuf);
  int retVal = pathMap->insert(newPathStr, linkNode);

  if(firstChild != NULL)
    ((PADSUniTreeNode*)firstChild)->insertIntoNamePathMap(pathMap, newPathStr, 0);
  if(nextSibling != NULL)
    ((PADSUniTreeNode*)nextSibling)->insertIntoNamePathMap(pathMap, pathStr, elementAtLevel+1);

  return retVal;
}
