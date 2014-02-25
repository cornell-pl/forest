/* *****************************************************************************
 * Mark Daly
 * June 2006
 * PADS_lang - PADS AST implementation for use in LaunchPADS
 *  Main class definitions
 * ************************************************************************** */

#include <stdio.h>
#include <stdlib.h>
#include <string>

#include "PADS_lang.h"
#include "PADS_lang_simple_tree.h"
#include "PADS_lang_pnode_tree.h"
#include "PADS_lang_unified_tree.h"
#include "PADS_lang_helper_classes.h"

#include "../lp_PADS_constants.h"

// **********
// PADS AST exceptions
PTException::PTException(char* msg, int type, int line)
{  
  message = msg; 
  lineNum = line;  
  hasMsg = true; 
  hasCode = false;
  throwerType = type;
}
PTException::PTException(int code, int type, int line)
{  
  errorCode = (code < PTE_LAST_MSG && code >= 0) ? code : PTE_UNDEFINED;
  lineNum = line;
  hasMsg = false; 
  hasCode = true;
  throwerType = type;
}
PTException::PTException(char* msg, int code, int type, int line)
{  
  message = msg;
  errorCode = (code < PTE_LAST_MSG && code >= 0) ? code : PTE_UNDEFINED;
  lineNum = line;
  hasMsg = true; 
  hasCode = true;
  throwerType = type;
}

char* PTException::printReport(char* sbuf, int len)
{
  PSTRING outputStr;
  outputStr = PTE_HEADER;

  if(hasMsg && !hasCode)
    {
      sprintf(sbuf, " ->%s [%d] @%d ", PT_TypeNames[throwerType],  throwerType, lineNum);
      outputStr = outputStr + message;
      outputStr = outputStr + sbuf;
    }
  else if(!hasMsg && hasCode)
    {
      sprintf(sbuf, " #%d ->%s [%d] @%d ", errorCode, PT_TypeNames[throwerType], throwerType, lineNum);
      outputStr = outputStr + PTE_MSGS[errorCode];
      outputStr = outputStr + sbuf;
    }
  else if(hasMsg && hasCode)
    {
      sprintf(sbuf, ") #%d ->%s [%d] @%d ", errorCode, PT_TypeNames[throwerType], throwerType, lineNum);
      outputStr = outputStr + message;
      outputStr = outputStr + " (";
      outputStr = outputStr + PTE_MSGS[errorCode];
      outputStr = outputStr + sbuf;
    }
  outputStr += '\0';
  if(outputStr.length() >= len)
    outputStr.copy(sbuf, len-1, 0);
  else
    outputStr.copy(sbuf, outputStr.length(), 0);
  return sbuf;
}

// ********************
// PNodeP Implementation
// PNodeP is the base class for all PADS type system node classes
// **********
PNodeP::PNodeP()
{
  nodeType = PT_UNDEF;
  nodeFlags = 0;
  nodeLabel = UNNAMED_STR;
  refCount = 1;
  parent = NULL;
  nextSibling = NULL;
  previousSibling = NULL;
}

PNodeP::PNodeP(int w)
{
  nodeType = w;
  nodeFlags = 0;
  nodeLabel = UNNAMED_STR;
  refCount = 1;
  parent = NULL;
  nextSibling = NULL;
  previousSibling = NULL;
}

PNodeP::PNodeP(int w, PSTRING n)
{
  nodeType = w;
  nodeFlags = 0;
  nodeLabel = n;
  refCount = 1;
  parent = NULL;
  nextSibling = NULL;
  previousSibling = NULL;
}

PNodeP::PNodeP(int w, int f)
{
  nodeType = w;
  nodeFlags = f;
  nodeLabel = UNNAMED_STR;
  refCount = 1;
  parent = NULL;
  nextSibling = NULL;
  previousSibling = NULL;
}

PNodeP::PNodeP(int w, PSTRING n, int f)
{
  nodeType = w;
  nodeFlags = f;
  nodeLabel = n;
  refCount = 1;
  parent = NULL;
  nextSibling = NULL;
  previousSibling = NULL;
}

PNodeP::~PNodeP()
{
  
}

int PNodeP::getType()
{
  return nodeType;
}

int PNodeP::getFlags()
{
  return nodeFlags;
}

PSTRING PNodeP::getName()
{
  return nodeLabel;
}

int PNodeP::getRefCount()
{
  return refCount;
}

void PNodeP::setType(int type)
{
  nodeType = type;
}

void PNodeP::setFlags(int f)
{
  nodeFlags = f;
}

void PNodeP::setName(PSTRING n)
{
  nodeLabel = n;
}

void PNodeP::setRefCount(int count)
{
  refCount = count;
}

int PNodeP::incRefCount()
{
  refCount += 1;
  return refCount;
}

int PNodeP::decRefCount()
{
  refCount -= 1;
  return refCount;
}

PNodeP* PNodeP::addChild(PNodeP* newChild)
{
  throw PTException(PTE_CHILD_ERR, nodeType, __LINE__);
  ////throw except;
  return NULL;
}

PNodeP* PNodeP::addValue(PSTRING& newVal)
{
  throw PTException(PTE_VAL_ERR, nodeType, __LINE__);
  //throw except;
  return NULL;
}

bool PNodeP::getValue(PSTRING& printStr)
{
  return false;
}

int PNodeP::setAttribute(PSTRING attrName, PSTRING attrValue)
{
  throw PTException(PTE_NO_ATTR_ERR, nodeType, __LINE__);
  //throw except;
  return -1;
}

int PNodeP::comparePStrings(PSTRING& a, PSTRING& b)
{
  return a.compare(b);
}

bool PNodeP::isBaseType(int type)
{
  for(int i = 0; i < PT_NumBaseTypes; i++)
    {
      if(type == PT_BaseTypes[i])
	return true;
    }
  return false;
}

bool PNodeP::isBaseTypeNode()
{
  int thisType = nodeType;
  for(int i = 0; i < PT_NumBaseTypes; i++)
    {
      if(thisType == PT_BaseTypes[i])
	return true;
    }
  return false;
}

bool PNodeP::isCompositeTypeNode()
{
  int thisType = nodeType;
  if(thisType <= PT_BEGIN_VALID_TYPES ||
     thisType >= PT_LAST_TYPE)
    return false;
  for(int i = 0; i < PT_NumBaseTypes; i++)
    {
      if(thisType == PT_BaseTypes[i])
	return false;
    }
  return true;
}

void PNodeP::serializeXMLEmpty(int spaceDepth, PSTRING& printStr)
{
  DB_P("<%s", PT_XMLNames[nodeType]);
  printStr.append("<");
  printStr.append(PT_XMLNames[nodeType]);
  serializeXMLAttributes(printStr);
  DB_P("/>");
  printStr.append("/>");
}

void PNodeP::serializeXMLOpen(int spaceDepth, PSTRING& printStr)
{
  DB_P("<%s", PT_XMLNames[nodeType]);
  printStr.append("<");
  printStr.append(PT_XMLNames[nodeType]);
  serializeXMLAttributes(printStr);
  DB_P(">");
  printStr.append(">");
}

void PNodeP::serializeXMLClose(int spaceDepth, PSTRING& printStr)
{
  DB_P("</%s>", PT_XMLNames[nodeType]);
  printStr.append("</");
  printStr.append(PT_XMLNames[nodeType]);
  printStr.append(">");
  if(PT_TypeNames[nodeType][0] == 'P' ||
     nodeType == PT_DECL      ||
     nodeType == PT_LITERAL   ||
     nodeType == PT_FIELD     ||
     nodeType == PT_ENUMFIELD)
    {
      DB_P("\n");
      printStr.append("\n");
      int numSpaces = spaceDepth * XML_ADD_LEVEL_SPACE;
      for(int i = 0; i < numSpaces ; i++)
	printStr.append(" ");;
	DB_P(" ");
    }
}

void PNodeP::serializeXMLContents(int spaceDepth, PSTRING& printStr)
{
  throw PTException(PTE_NO_PRINT_FUN_DEFINED_ERR, nodeType, __LINE__);
  //throw except;
}

void PNodeP::serializeXMLAttributes(PSTRING& printStr)
{
  return;
}

void PNodeP::serializeXMLRepresentation(int spaceDepth, PSTRING& printStr)
{
  serializeXMLOpen(spaceDepth, printStr);
  serializeXMLContents(spaceDepth, printStr);
  serializeXMLClose(spaceDepth, printStr);
}

bool PNodeP::validate(PNodeP** errorNode)
{
  throw PTException(PTE_NO_VALIDATE_FUN_DEFINED_ERR, nodeType, __LINE__);
  //throw except;
}

void PNodeP::codifyOpen(int spaceDepth, PSTRING& printStr)
{
  DB_P("%s", "   ");
  printStr.append(spaceDepth * CODE_ADD_LEVEL_SPACE, ' ');
}

void PNodeP::codifyClose(int spaceDepth, PSTRING& printStr)
{
  DB_P("%s", ";\n");
  printStr.append(";\n");
}

void PNodeP::codifyContents(int spaceDepth, PSTRING& printStr)
{
  return;
}

void PNodeP::codifyRepresentation(int spaceDepth, PSTRING& printStr)
{
  codifyOpen(spaceDepth, printStr);
  codifyContents(spaceDepth, printStr);
  codifyClose(spaceDepth, printStr);
}

/*
void PNodeP::setTreeElement(wxTreeItemData* newElement)
{
  treeItem = newElement;
}

wxTreeItemData* PNodeP::getTreeElemenmt()
{
  return treeItem;
}
*/

bool PNodeP::canChangeToType(int newType)
{
  return false;
}

PNodeP* PNodeP::convertAndCopy(int newType, PNodeP* copyTo)
{
  return NULL;
}

int PNodeP::getTextualRepresentation(PSTRING& nameStr)
{
  nameStr.append(PT_TypeNames[nodeType]);
  return 0;
}

PADSPNodeTreeNode* PNodeP::makeBasicTree(PADSPNodeTreeNode* parent)
{
  DB_P("making base node::%s\n", PT_TypeNames[nodeType]);
  PADSPNodeTreeNode* thisNode = new PADSPNodeTreeNode(this, parent);    
  return thisNode;
}

bool PNodeP::isStringWhitespace(char* c)
{
  for(int i = 0; c[i] != '\0'; i++)
    {
      if(c[i] != ' ' &&
	 c[i] != '\n' &&
	 c[i] != '\t' &&
	 c[i] != '\r')
	return false;
    }
  return true;
}

bool PNodeP::stringContainsIllegalXMLEntities(const char* c)
{
  for(int i = 0; c[i] != '\0'; i++)
    {
      if(c[i] == '&'  ||
	 //c[i] == '"'  ||
	 //c[i] == '\'' ||
	 c[i] == '<'  ||
	 c[i] == '>'  )
	return true;
    }
  return false;
}

void PNodeP::getTypeStr(PSTRING& typeStr)
{
  typeStr.append(PT_TypeNames[nodeType]);
}

void PNodeP::getNameStr(PSTRING& nameStr)
{
  nameStr = "";
}

int PNodeP::canAddChildTypes(int* typeList, int max)
{
  return 0;
}

int PNodeP::removeChildNode(PNodeP* removeThis)
{
  return -1;
}

int PNodeP::makeCompleteNode()
{
  return 0;
}

int PNodeP::insertIntoNameMap(PADSTypeMap* nameMap)
{
  PSTRING nameStr;
  getNameStr(nameStr);
  return nameMap->insert(nameStr, this);
}

int PNodeP::insertIntoPathMap(PADSTypeMap* pathMap, PSTRING& pathStr)
{
  PSTRING nameStr;
  PSTRING fullPath;
  getNameStr(nameStr);
  fullPath = pathStr;
  fullPath.append(":");
  fullPath.append(nameStr);
  return pathMap->insert(fullPath, this);
}

int PNodeP::insertIntoTypeMap(PADSTypeMap* typeMap)
{
  PSTRING nameStr;
  getNameStr(nameStr);
  return typeMap->insert(nameStr, this);
}

PADSUniTreeNode* PNodeP::makeUnifiedTree(PADSUniTreeNode* parent, PSTRING& varStr, PADSTypeMap* typeMap)
{
  DB_P("making base node::%s, name = %s \n", PT_TypeNames[nodeType], varStr.c_str());
  PADSUniTreeNode* thisNode = new PADSUniTreeNode(this, varStr, parent);
  return thisNode;  
}

// *************************************************
// PADS Type System 
// Here we go...
// *************************************************

unitS::unitS() : PNodeP(PT_UNIT)
{
  unit = true;
}

void unitS::serializeXMLRepresentation(int spaceDepth, PSTRING& printStr)
{
  serializeXMLEmpty(spaceDepth, printStr);
}

bool unitS::validate(PNodeP** errorNode)
{
  return true;
}

void unitS::codifyRepresentation(int spaceDepth, PSTRING& printStr)
{
  DB_P("%s ", PT_TypeNames[nodeType]);
  printStr.append(PT_TypeNames[nodeType]);
  printStr.append(" ");
}

PADSPNodeTreeNode* unitS::makeBasicTree(PADSPNodeTreeNode* parent)
{
  DB_P("making base node::%s\n", PT_TypeNames[nodeType]);
  PADSPNodeTreeNode* thisNode = new PADSPNodeTreeNode(this, parent);    
  return thisNode;
}

// new_methods_def_point


// **********

nameS::nameS() : PNodeP(PT_NAME)
{
  val = "";
}

PNodeP* nameS::addValue(PSTRING& newVal)
{
  val = newVal; 
  return this;
}

bool nameS::getValue(PSTRING& printStr)
{
  printStr.append(val);
  return true;
}

void nameS::serializeXMLContents(int spaceDepth, PSTRING& printStr)
{
  DB_P("%s", val.c_str());
  if(PNodeP::stringContainsIllegalXMLEntities(val.c_str()))
    {
      printStr.append("<![CDATA[");
      printStr.append(val);
      printStr.append("]]>\n");
    }
    else
      printStr.append(val);
}

bool nameS::validate(PNodeP** errorNode)
{
  return true;
}

void nameS::codifyRepresentation(int spaceDepth, PSTRING& printStr)
{
  DB_P("%s", val.c_str());
  printStr.append(val);
}

bool nameS::canChangeToType(int newType)
{
  switch(newType)
    {
    case PT_ID:
    case PT_VAR:
    case PT_EXPR:
    case PT_PRE:
    case PT_CHAR:
    case PT_STRING:
    case PT_TYPENAME:
    case PT_EXPR2:
    case PT_LITERAL:
    case PT_PPARSECHECK:
      return true;
    default:
      break;
    }
  return false;
}

PNodeP* nameS::convertAndCopy(int newType, PNodeP* copyTo)
{
  return NULL;
}

int nameS::getTextualRepresentation(PSTRING& nameStr)
{
  //nameStr.append(PT_TypeNames[nodeType]);
  //nameStr.append(" ");
  nameStr.append(val);
  return 0;
}

void nameS::getTypeStr(PSTRING& typeStr)
{
  typeStr = "";
}

void nameS::getNameStr(PSTRING& nameStr)
{
  nameStr = val;
}

PADSPNodeTreeNode* nameS::makeBasicTree(PADSPNodeTreeNode* parent)
{
  DB_P("making base node::%s\n", PT_TypeNames[nodeType]);
  PADSPNodeTreeNode* thisNode = new PADSPNodeTreeNode(this, parent);    
  PSTRING tmpStr;
  tmpStr = PT_TypeNames[nodeType];
  tmpStr.append(": ");
  DB_P("getting textual representation for node of type %s\n", PT_TypeNames[nodeType]);
  getTextualRepresentation(tmpStr);
  DB_P("setting node name to %s\n", tmpStr.c_str());
  thisNode->setNodeName(tmpStr);
  return thisNode;
}

// new_methods_def_point


// **********

varS::varS() : PNodeP(PT_VAR)
{
  val = "";
}

PNodeP* varS::addValue(PSTRING& newVal)
{  
  val = newVal; 
  return this;
}

bool varS::getValue(PSTRING& printStr)
{
  printStr.append(val);
  return true;
}

void varS::serializeXMLContents(int spaceDepth, PSTRING& printStr)
{
  DB_P("%s", val.c_str());
  if(PNodeP::stringContainsIllegalXMLEntities(val.c_str()))
    {
      printStr.append("<![CDATA[");
      printStr.append(val);
      printStr.append("]]>\n");
    }
    else
      printStr.append(val);
}

bool varS::validate(PNodeP** errorNode)
{
  return true;
}

void varS::codifyRepresentation(int spaceDepth, PSTRING& printStr)
{
  DB_P("%s", val.c_str());
  printStr.append(val);
}

bool varS::canChangeToType(int newType)
{
  switch(newType)
    {
    default:
      break;
    }
  return false;
}

PNodeP* varS::convertAndCopy(int newType, PNodeP* copyTo)
{
  return NULL;
}

int varS::getTextualRepresentation(PSTRING& nameStr)
{
  //nameStr.append(PT_TypeNames[nodeType]);
  //nameStr.append(": ");
  nameStr.append(val);
  return 0;
}

void varS::getTypeStr(PSTRING& typeStr)
{
  typeStr = "";
}

void varS::getNameStr(PSTRING& nameStr)
{
  nameStr = val;
}

PADSPNodeTreeNode* varS::makeBasicTree(PADSPNodeTreeNode* parent)
{
  DB_P("making base node::%s\n", PT_TypeNames[nodeType]);
  PADSPNodeTreeNode* thisNode = new PADSPNodeTreeNode(this, parent);    
  PSTRING tmpStr;
  tmpStr = PT_TypeNames[nodeType];
  tmpStr.append(": ");
  DB_P("getting textual representation for node of type %s\n", PT_TypeNames[nodeType]);
  getTextualRepresentation(tmpStr);
  DB_P("setting node name to %s\n", tmpStr.c_str());
  thisNode->setNodeName(tmpStr);
  return thisNode;
}

// new_methods_def_point


// **********

idS::idS() : PNodeP(PT_ID)
{
  val = "";
}

PNodeP* idS::addValue(PSTRING& newVal)
{  
  val = newVal; 
  return this;
}

bool idS::getValue(PSTRING& printStr)
{
  printStr.append(val);
  return true;
}

void idS::serializeXMLContents(int spaceDepth, PSTRING& printStr)
{
  DB_P("%s", val.c_str());
  if(PNodeP::stringContainsIllegalXMLEntities(val.c_str()))
    {
      printStr.append("<![CDATA[");
      printStr.append(val);
      printStr.append("]]>\n");
    }
    else
      printStr.append(val);
}

bool idS::validate(PNodeP** errorNode)
{
  return true;
}

void idS::codifyRepresentation(int spaceDepth, PSTRING& printStr)
{
  DB_P("%s", val.c_str());
  printStr.append(val);
}

bool idS::canChangeToType(int newType)
{
  switch(newType)
    {
    default:
      break;
    }
  return false;
}

PNodeP* idS::convertAndCopy(int newType, PNodeP* copyTo)
{
  return NULL;
}

int idS::getTextualRepresentation(PSTRING& nameStr)
{
  //nameStr.append(PT_TypeNames[nodeType]);
  //nameStr.append(" ");
  nameStr.append(val);
  return 0;
}

void idS::getTypeStr(PSTRING& typeStr)
{
  typeStr = "";
}

void idS::getNameStr(PSTRING& nameStr)
{
  nameStr = val;
}

PADSPNodeTreeNode* idS::makeBasicTree(PADSPNodeTreeNode* parent)
{
  DB_P("making base node::%s\n", PT_TypeNames[nodeType]);
  PADSPNodeTreeNode* thisNode = new PADSPNodeTreeNode(this, parent);    
  PSTRING tmpStr;
  tmpStr = PT_TypeNames[nodeType];
  tmpStr.append(": ");
  DB_P("getting textual representation for node of type %s\n", PT_TypeNames[nodeType]);
  getTextualRepresentation(tmpStr);
  DB_P("setting node name to %s\n", tmpStr.c_str());
  thisNode->setNodeName(tmpStr);
  return thisNode;
}

// new_methods_def_point


// **********

/*
languageS::languageS() : PNodeP(PT_LANGUAGE)
{
  val = "";
}

PNodeP* languageS::addValue(PSTRING& newVal)
{  val = newVal; }

*/
// **********

ExprS::ExprS() : PNodeP(PT_EXPR)
{
  has_language = false;
  language = ""; //= NULL;
  textBlock = "";
}

PNodeP* ExprS::addValue(PSTRING& newVal)
{  
  textBlock = newVal; 
  return this;
}

bool ExprS::getValue(PSTRING& printStr)
{
  printStr.append(textBlock);
  return true;
}

int ExprS::setAttribute(PSTRING attrName, PSTRING attrValue)
{
  PSTRING compareString;
  compareString = "language";
  if(PNodeP::comparePStrings(attrName, compareString) == 0)
    {
      has_language = true;
      language = attrValue;
      return 0;
    }
  return -1;
}

void ExprS::serializeXMLAttributes(PSTRING& printStr)
{
  if(has_language)
    {
      DB_P(" %s = \"%s\"", "language", language.c_str());
      printStr.append(" language = \"");
      printStr.append(language);
      printStr.append("\"");
    }
}

void ExprS::serializeXMLContents(int spaceDepth, PSTRING& printStr)
{
  DB_P("%s", textBlock.c_str());
  if(PNodeP::stringContainsIllegalXMLEntities(textBlock.c_str()))
    {
      printStr.append("<![CDATA[");
      printStr.append(textBlock);
      printStr.append("]]>\n");
    }
    else
      printStr.append(textBlock);
}

bool ExprS::validate(PNodeP** errorNode)
{
  return true;
}

void ExprS::codifyRepresentation(int spaceDepth, PSTRING& printStr)
{
  if(nodeType == PT_PRE)
    {
      DB_P("Pre ");
      printStr.append(PT_TypeNames[PT_PRE]);
      printStr.append(" ");
    }
  if(nodeType == PT_PPARSECHECK)
    {
      DB_P("Pparsecheck ");
      printStr.append(PT_TypeNames[PT_PPARSECHECK]);
      printStr.append(" ");
    }
  DB_P("%s", textBlock.c_str());
  printStr.append(textBlock);
}

bool ExprS::canChangeToType(int newType)
{
  switch(newType)
    {
    default:
      break;
    }
  return false;
}

PNodeP* ExprS::convertAndCopy(int newType, PNodeP* copyTo)
{
  return NULL;
}

int ExprS::getTextualRepresentation(PSTRING& nameStr)
{
  //nameStr.append(PT_TypeNames[nodeType]);
  //nameStr.append(" ");
  nameStr.append(textBlock);
  return 0;
}

void ExprS::getTypeStr(PSTRING& typeStr)
{
  typeStr = "";
}

void ExprS::getNameStr(PSTRING& nameStr)
{
  nameStr = textBlock;
}

PADSPNodeTreeNode* ExprS::makeBasicTree(PADSPNodeTreeNode* parent)
{
  DB_P("making base node::%s\n", PT_TypeNames[nodeType]);
  PADSPNodeTreeNode* thisNode = new PADSPNodeTreeNode(this, parent);    
  PSTRING tmpStr;
  tmpStr = PT_TypeNames[nodeType];
  tmpStr.append(": ");
  DB_P("getting textual representation for node of type %s\n", PT_TypeNames[nodeType]);
  getTextualRepresentation(tmpStr);
  DB_P("setting node name to %s\n", tmpStr.c_str());
  thisNode->setNodeName(tmpStr);
  return thisNode;
}

// new_methods_def_point


// **********

stringS::stringS() : PNodeP(PT_STRING)
{
  val = "";
}

PNodeP* stringS::addValue(PSTRING& newVal)
{  
  val = newVal; 
  return this;
}

bool stringS::getValue(PSTRING& printStr)
{
  printStr.append(val);
  return true;
}

void stringS::serializeXMLContents(int spaceDepth, PSTRING& printStr)
{
  DB_P("%s", val.c_str());
  if(PNodeP::stringContainsIllegalXMLEntities(val.c_str()))
    {
      printStr.append("<![CDATA[");
      printStr.append(val);
      printStr.append("]]>\n");
    }
    else
      printStr.append(val);
}

bool stringS::validate(PNodeP** errorNode)
{
  return true;
}

void stringS::codifyRepresentation(int spaceDepth, PSTRING& printStr)
{
  if(val.size() > 1 &&
     (val.at(0) != '"' &&
      val.at(0) != '\''))
    {
      printStr.append("\"");
      printStr.append(val);
      printStr.append("\"");
    }
  else
    {
      printStr.append("'");
      if(val.at(0) == '\'')
	printStr.append("\\");
      printStr.append(val);
      printStr.append("'");
    }
}

bool stringS::canChangeToType(int newType)
{
  switch(newType)
    {
    default:
      break;
    }
  return false;
}

PNodeP* stringS::convertAndCopy(int newType, PNodeP* copyTo)
{
  return NULL;
}

int stringS::getTextualRepresentation(PSTRING& nameStr)
{
  //nameStr.append(PT_TypeNames[nodeType]);
  //nameStr.append(" ");
  if(val.size() > 1)
    {
      nameStr.append("\"");
      nameStr.append(val);
      nameStr.append("\"");

    }
  else
    {
      nameStr.append("'");
      nameStr.append(val);
      nameStr.append("'");
    }
  return 0;
}

void stringS::getTypeStr(PSTRING& typeStr)
{
  typeStr = "";
}

void stringS::getNameStr(PSTRING& nameStr)
{
  nameStr = val;
}

// new_methods_def_point


// **********

charS::charS() : PNodeP(PT_CHAR)
{
  val = "";
}

PNodeP* charS::addValue(PSTRING& newVal)
{  
  val = newVal; 
  return this;
}

bool charS::getValue(PSTRING& printStr)
{
  printStr.append(val);
  return true;
}

void charS::serializeXMLContents(int spaceDepth, PSTRING& printStr)
{
  DB_P("%s", val.c_str());
  if(PNodeP::stringContainsIllegalXMLEntities(val.c_str()))
    {
      printStr.append("<![CDATA[");
      printStr.append(val);
      printStr.append("]]>\n");
    }
    else
      printStr.append(val);
}

bool charS::validate(PNodeP** errorNode)
{
  return true;
}

void charS::codifyRepresentation(int spaceDepth, PSTRING& printStr)
{
  DB_P("'%s'", val.c_str());
  if(val.size() > 1)
    {
      printStr.append("\"");
      printStr.append(val);
      printStr.append("\"");

    }
  else
    {
      printStr.append("'");
      printStr.append(val);
      printStr.append("'");
    }
}

bool charS::canChangeToType(int newType)
{
  switch(newType)
    {
    default:
      break;
    }
  return false;
}

PNodeP* charS::convertAndCopy(int newType, PNodeP* copyTo)
{
  return NULL;
}

int charS::getTextualRepresentation(PSTRING& nameStr)
{
  //nameStr.append(PT_TypeNames[nodeType]);
  //nameStr.append(" ");
  if(val.size() > 1)
    {
      nameStr.append("\"");
      nameStr.append(val);
      nameStr.append("\"");
    }
  else
    {
      nameStr.append("'");
      nameStr.append(val);
      nameStr.append("'");
    }
  return 0;
}

void charS::getTypeStr(PSTRING& typeStr)
{
  typeStr = "";
}

void charS::getNameStr(PSTRING& nameStr)
{
  nameStr = val;
}

// new_methods_def_point


// **********

REExprS::REExprS() : PNodeP(PT_REEXPR)
{
  which = UNDEFINED;
  Pre = NULL;
  str = NULL;
  chr = NULL;
  id =  NULL;
}

PNodeP* REExprS::addValue(PSTRING& newVal)
{
  switch(which)
    {
    case PRE:
      Pre->addValue(newVal);
      break;
    case STRING:
      str->addValue(newVal);
      break;      
    case CHAR:
      chr->addValue(newVal);
      break;
    case ID:
      id->addValue(newVal);
      break;
    default:
      {
	throw PTException(PTE_CHILD_TYPE_ERR, nodeType, __LINE__);
	//throw except;
	return NULL;
      }
      break;
    }
  return this;
}

bool REExprS::getValue(PSTRING& printStr)
{
  switch(which)
    {
    case PRE:
      Pre->getValue(printStr);
      break;
    case STRING:
      str->getValue(printStr);
      break;      
    case CHAR:
      chr->getValue(printStr);
      break;
    case ID:
      id->getValue(printStr);
      break;
    default:
      {
	DB_P("REExpr - can't get value!\n");
	throw PTException(PTE_CHILD_TYPE_ERR, nodeType, __LINE__);
	//throw except;
	return false;
      }
      break;
    }
  return true;
}

PNodeP* REExprS::addChild(PNodeP* newChild)
{
  switch(newChild->nodeType)
    {
    case PT_PRE:
      Pre = (PreT)newChild;
      which = PRE;
      break;
    case PT_STRING:
      str = (stringT)newChild;
      which = STRING;
      break;      
    case PT_CHAR:
      chr = (charT)newChild;
      which = CHAR;
      break;
    case PT_ID:
      id = (idT)newChild;
      which = ID;
      break;
    default:
      {
	throw PTException(PTE_CHILD_TYPE_ERR, nodeType, __LINE__);
	//throw except;
	return NULL;
      }
      break;
    }
  newChild->parent = (PNodeP*)this;
  return this;
}

void REExprS::serializeXMLContents(int spaceDepth, PSTRING& printStr)
{
  switch(which)
    {
    case PRE:
      Pre->serializeXMLRepresentation(spaceDepth + 1, printStr);
      break;
    case STRING:
      str->serializeXMLRepresentation(spaceDepth + 1, printStr);
      break;
    case CHAR:
      chr->serializeXMLRepresentation(spaceDepth + 1, printStr);
      break;
    case ID:
      id->serializeXMLRepresentation(spaceDepth + 1, printStr);
      break;
    default:
      {
	throw PTException(PTE_CHILD_UNDEF_ERR, nodeType, __LINE__);
	//throw except;
      }
      
    }
}

bool REExprS::validate(PNodeP** errorNode)
{
  if(which == PRE    && Pre != NULL)
    return true;
  if(which == STRING && str != NULL)
    return true;
  if(which == CHAR   && chr != NULL)
    return true;
  if(which == ID     && id != NULL)
    return true;
  *errorNode = this;
  return false;
}

void REExprS::codifyRepresentation(int spaceDepth, PSTRING& printStr)
{
  switch(which)
    {
    case PRE:
      Pre->codifyRepresentation(spaceDepth, printStr);
      break;
    case STRING:
      str->codifyRepresentation(spaceDepth, printStr);
      break;
    case CHAR:
      chr->codifyRepresentation(spaceDepth, printStr);
      break;
    case ID:
      id->codifyRepresentation(spaceDepth, printStr);
      break;
    default:
      {
	throw PTException(PTE_CHILD_UNDEF_ERR, nodeType, __LINE__);
	//throw except;
      } 
    }
  if(nodeType == PT_LITERAL)
    {
      DB_P(";");
      printStr.append(";");
    }
}

bool REExprS::canChangeToType(int newType)
{
  switch(newType)
    {
    default:
      break;
    }
  return false;
}

PNodeP* REExprS::convertAndCopy(int newType, PNodeP* copyTo)
{
  return NULL;
}

int REExprS::getTextualRepresentation(PSTRING& nameStr)
{
  switch(which)
    {
    case PRE:
      Pre->getTextualRepresentation(nameStr);
      break;
    case STRING:
      str->getTextualRepresentation(nameStr);
      break;
    case CHAR:
      chr->getTextualRepresentation(nameStr);
      break;
    case ID:
      id->getTextualRepresentation(nameStr);
      break;
    default:
      {
	return -1;
      } 
    }
  return 0;
}

void REExprS::getTypeStr(PSTRING& typeStr)
{
  typeStr = "";
}

void REExprS::getNameStr(PSTRING& nameStr)
{
  switch(which)
    {
    case PRE:
      Pre->getNameStr(nameStr);
      break;
    case STRING:
      str->getNameStr(nameStr);
      break;
    case CHAR:
      chr->getNameStr(nameStr);
      break;
    case ID:
      id->getNameStr(nameStr);
      break;
    default:
      break;
    }
}

PADSPNodeTreeNode* REExprS::makeBasicTree(PADSPNodeTreeNode* parent)
{
  DB_P("making base node::%s\n", PT_TypeNames[nodeType]);
  PADSPNodeTreeNode* thisNode = new PADSPNodeTreeNode(this, parent);    
  PSTRING tmpStr;
  tmpStr = PT_TypeNames[nodeType];
  tmpStr.append(": ");
  DB_P("getting textual representation for node of type %s\n", PT_TypeNames[nodeType]);
  getTextualRepresentation(tmpStr);
  DB_P("setting node name to %s\n", tmpStr.c_str());
  thisNode->setNodeName(tmpStr);
  return thisNode;
}

int REExprS::makeCompleteNode()
{
  Pre = new PreS();
  Pre->nodeType = PT_PRE;
  which = PRE;
  return Pre->makeCompleteNode();
}

// new_methods_def_point


// **********

typenameS::typenameS() : PNodeP(PT_TYPENAME)
{
  val = "";
}

PNodeP* typenameS::addValue(PSTRING& newVal)
{  
  val = newVal;
  return this;
}

bool typenameS::getValue(PSTRING& printStr)
{
  printStr.append(val);
  return true;
}

void typenameS::serializeXMLContents(int spaceDepth, PSTRING& printStr)

{
  DB_P("%s", val.c_str());
  if(PNodeP::stringContainsIllegalXMLEntities(val.c_str()))
    {
      printStr.append("<![CDATA[");
      printStr.append(val);
      printStr.append("]]>\n");
    }
    else
      printStr.append(val);
}

bool typenameS::validate(PNodeP** errorNode)
{
  return true;
}

void typenameS::codifyRepresentation(int spaceDepth, PSTRING& printStr)
{
  DB_P("%s", val.c_str());
  printStr.append(val);
}

bool typenameS::canChangeToType(int newType)
{
  switch(newType)
    {
    default:
      break;
    }
  return false;
}

PNodeP* typenameS::convertAndCopy(int newType, PNodeP* copyTo)
{
  return NULL;
}

int typenameS::getTextualRepresentation(PSTRING& nameStr)
{
  nameStr.append(val);
  return 0;
}

void typenameS::getTypeStr(PSTRING& typeStr)
{
  typeStr = "";
}

void typenameS::getNameStr(PSTRING& nameStr)
{
  nameStr = val;
}

PADSPNodeTreeNode* typenameS::makeBasicTree(PADSPNodeTreeNode* parent)
{
  DB_P("making base node::%s\n", PT_TypeNames[nodeType]);
  PADSPNodeTreeNode* thisNode = new PADSPNodeTreeNode(this, parent);    
  PSTRING tmpStr;
  tmpStr = PT_TypeNames[nodeType];
  tmpStr.append(": ");
  DB_P("getting textual representation for node of type %s\n", PT_TypeNames[nodeType]);
  getTextualRepresentation(tmpStr);
  DB_P("setting node name to %s\n", tmpStr.c_str());
  thisNode->setNodeName(tmpStr);
  return thisNode;
}

// new_methods_def_point


// **********

vardeclS::vardeclS() : PNodeP(PT_VARDECL)
{
  typname = NULL;
  var = NULL;
}

PNodeP* vardeclS::addChild(PNodeP* newChild)
{
  switch(newChild->nodeType)
    {
    case PT_TYPENAME:
      typname = (typenameT)newChild;
      break;
    case PT_VAR:
      var = (varT)newChild;
      break;
    default:
      {
	throw PTException(PTE_CHILD_TYPE_ERR, nodeType, __LINE__);
	//throw except;
	return NULL;
      }
      break;
    }
  newChild->parent = (PNodeP*)this;
  return this;
}

void vardeclS::serializeXMLContents(int spaceDepth, PSTRING& printStr)
{
  typname->serializeXMLRepresentation(spaceDepth + 1, printStr);
  var->serializeXMLRepresentation(spaceDepth + 1, printStr);
}

bool vardeclS::validate(PNodeP** errorNode)
{
  if(typname == NULL || var == NULL)
    {
      *errorNode = this;
      return false;
    }
  if(typname->validate(errorNode) == true &&
     var->validate(errorNode) == true)
    return true;
  return false;
}

void vardeclS::codifyRepresentation(int spaceDepth, PSTRING& printStr)
{
  printStr.append(PT_TypeNames[nodeType]);
  printStr.append(": ");
  typname->codifyRepresentation(spaceDepth, printStr);
  DB_P(" ");
  printStr.append(" ");
  var->codifyRepresentation(spaceDepth, printStr);
}

bool vardeclS::canChangeToType(int newType)
{
  switch(newType)
    {
    default:
      break;
    }
  return false;
}

PNodeP* vardeclS::convertAndCopy(int newType, PNodeP* copyTo)
{
  return NULL;
}

int vardeclS::getTextualRepresentation(PSTRING& nameStr)
{
  typname->getTextualRepresentation(nameStr);
  nameStr.append(" ");
  var->getTextualRepresentation(nameStr);
  
  return 0;
}

void vardeclS::getTypeStr(PSTRING& typeStr)
{
  typname->getNameStr(typeStr);
}

void vardeclS::getNameStr(PSTRING& nameStr)
{
  var->getNameStr(nameStr);
}

PADSPNodeTreeNode* vardeclS::makeBasicTree(PADSPNodeTreeNode* parent)
{
  DB_P("making composite node::%s\n", PT_TypeNames[nodeType]);
  PADSPNodeTreeNode* thisNode = new PADSPNodeTreeNode(this, parent);

  PADSPNodeTreeNode *newChild = NULL, *currentChild = NULL;

  PSTRING tmpStr;
  tmpStr = PT_TypeNames[nodeType];
  tmpStr.append(": ");
  DB_P("getting textual representation for node of type %s\n", PT_TypeNames[nodeType]);
  getTextualRepresentation(tmpStr);
  DB_P("setting node name to %s\n", tmpStr.c_str());
  thisNode->setNodeName(tmpStr);
  
  newChild = typname->makeBasicTree(thisNode);
  newChild->setUpdateParentOnChange(true);
  newChild->setElementIsRequired(true);
  if(newChild != NULL)
    {
      if(currentChild == NULL)
	{
	  thisNode->firstChild = newChild;
	}
      else
	{
	  currentChild->nextSibling = newChild;
	  newChild->previousSibling = currentChild;
	}
    }
  currentChild = newChild;

  newChild = var->makeBasicTree(thisNode);
  newChild->setUpdateParentOnChange(true);
  newChild->setElementIsRequired(true);
  if(newChild != NULL)
    {
      if(currentChild == NULL)
	{
	  thisNode->firstChild = newChild;
	}
      else
	{
	  currentChild->nextSibling = newChild;
	  newChild->previousSibling = currentChild;
	}
    }
  currentChild = newChild;

  return thisNode;
}

int vardeclS::makeCompleteNode()
{
  typname = new typenameS();
  var = new varS();
  if(typname->makeCompleteNode() != 0)
    return -1;
  if(var->makeCompleteNode())
    return -1;
  return 0;
}

// new_methods_def_point


// **********

ConstraintsS::ConstraintsS() : PNodeP(PT_CONSTRAINTS)
{
  // nothing to do with vectors
}

PNodeP* ConstraintsS::addChild(PNodeP* newChild)
{
  switch(newChild->nodeType)
    {
    case PT_VARDECL:
      vardecl.push_back((vardeclT)newChild);
      break;
    case PT_EXPR:
    case PT_EXPR2:
    case PT_PPARSECHECK:
      {
	expr_PparsecheckT ep = new expr_PparsecheckS();
	ep->which = newChild->nodeType == PT_EXPR2 ? ep->EXPR : ep->PPARSECHECK;
	if(ep->which == ep->EXPR)
	  ep->expr_Pparsecheck.expr = (exprT)newChild;
	else
	  ep->expr_Pparsecheck.Pparsecheck = (PparsecheckT)newChild;
	expr_Pparsecheck.push_back(ep);
      }
      break;
    default:
      {
	throw PTException(PTE_CHILD_TYPE_ERR, nodeType, __LINE__);
	//throw except;
	return NULL;
      }
      break;
    }
  newChild->parent = (PNodeP*)this;
  return this;
}

void ConstraintsS::serializeXMLContents(int spaceDepth, PSTRING& printStr)
{
  int size;
  size = vardecl.size();
  for(int i = 0; i < size; i++)
    {
      if(vardecl.at(i) != NULL)
	vardecl.at(i)->serializeXMLRepresentation(spaceDepth + 1, printStr);
    }
  size = expr_Pparsecheck.size();
  expr_PparsecheckT this_expr_Pparsecheck;
  for(int i = 0; i < size; i++)
    {
      this_expr_Pparsecheck = expr_Pparsecheck.at(i);
      if(this_expr_Pparsecheck->which == this_expr_Pparsecheck->EXPR)
	this_expr_Pparsecheck->expr_Pparsecheck.expr->serializeXMLRepresentation(spaceDepth + 1, printStr);
      else if(this_expr_Pparsecheck->which == this_expr_Pparsecheck->PPARSECHECK)
	this_expr_Pparsecheck->expr_Pparsecheck.Pparsecheck->serializeXMLRepresentation(spaceDepth + 1, printStr);
      else
	{
	  throw PTException(PTE_CHILD_UNDEF_ERR, nodeType, __LINE__);
	  //throw except;
	}
    }
}

bool ConstraintsS::validate(PNodeP** errorNode)
{
  int size = vardecl.size();
  for(int i = 0; i < size; i++)
    {
      if(vardecl.at(i) == NULL)
	{
	  *errorNode = this;
	  return false;
	}
      if(vardecl.at(i)->validate(errorNode) == false)
	return false;
    }

  size = expr_Pparsecheck.size();
  expr_PparsecheckT this_expr_Pparsecheck;
  for(int i = 0; i < size; i++)
    {
      this_expr_Pparsecheck = expr_Pparsecheck.at(i);
      if(this_expr_Pparsecheck == NULL)
	{
	  *errorNode = this;
	  return false;
	}
      if(this_expr_Pparsecheck->which == this_expr_Pparsecheck->UNDEFINED)
	return false;
      else if(this_expr_Pparsecheck->which == this_expr_Pparsecheck->EXPR)
	{
	  if(this_expr_Pparsecheck->expr_Pparsecheck.expr->validate(errorNode) == false)
	    return false;
	}
      else if(this_expr_Pparsecheck->which == this_expr_Pparsecheck->PPARSECHECK)
	{
	  if(this_expr_Pparsecheck->expr_Pparsecheck.Pparsecheck->validate(errorNode) == false)
	    return false;
	}
      else 
	return false;
    }
  return true;
}

void ConstraintsS::codifyRepresentation(int spaceDepth, PSTRING& printStr)
{
  int size;
  expr_PparsecheckT this_expr_Pparsecheck;
  bool closeParens = false;

  size = vardecl.size();
  for(int i = 0; i < size; i++)
    {
      if(vardecl.at(i) != NULL)
	{
	  if(i != 0)
	    {
	      DB_P(", ");
	      printStr.append(", ");
	      //printStr.append(spaceDepth * CODE_ADD_LEVEL_SPACE, ' ');
	    }
	  vardecl.at(i)->codifyRepresentation(spaceDepth, printStr);
	}
    }

  if(vardecl.size() > 0)
    {
      DB_P(" => ");
      printStr.append(" => ");
    }

  size = expr_Pparsecheck.size();
  for(int i = 0; i < size; i++)
    {
      if(i != 0)
	{
	  DB_P(" &&\n   ");
	  printStr.append(" &&\n");
	  printStr.append(spaceDepth * CODE_ADD_LEVEL_SPACE, ' ');
	}
      this_expr_Pparsecheck = expr_Pparsecheck.at(i);
      //DB_P("{ ");
      //printStr.append("{ ");
      if(this_expr_Pparsecheck->which == this_expr_Pparsecheck->EXPR)
	this_expr_Pparsecheck->expr_Pparsecheck.expr->codifyRepresentation(spaceDepth, printStr);
      else if(this_expr_Pparsecheck->which == this_expr_Pparsecheck->PPARSECHECK)
	this_expr_Pparsecheck->expr_Pparsecheck.Pparsecheck->codifyRepresentation(spaceDepth, printStr);
      else
	{
	  throw PTException(PTE_CHILD_UNDEF_ERR, nodeType, __LINE__);
	  //throw except;
	}
      //DB_P("} ");
      //printStr.append("} ");
    }
}

bool ConstraintsS::canChangeToType(int newType)
{
  switch(newType)
    {
    default:
      break;
    }
  return false;
}

PNodeP* ConstraintsS::convertAndCopy(int newType, PNodeP* copyTo)
{
  return NULL;
}

PADSPNodeTreeNode* ConstraintsS::makeBasicTree(PADSPNodeTreeNode* parent)
{
  DB_P("making composite node::%s\n", PT_TypeNames[nodeType]);
  PADSPNodeTreeNode* thisNode = new PADSPNodeTreeNode(this, parent);

  PADSPNodeTreeNode *newChild = NULL, *currentChild = NULL;
  
  vardeclT this_vardecl = NULL;
  int vectorSize = vardecl.size();
  for(int i = 0; i < vectorSize; i++)
    {
      this_vardecl = vardecl.at(i);
      if(this_vardecl != NULL)
	{
	  newChild = this_vardecl->makeBasicTree(thisNode);
	  if(currentChild == NULL)
	    {
	      thisNode->firstChild = newChild;
	    }
	  else
	    {
	      currentChild->nextSibling = newChild;
	      newChild->previousSibling = currentChild;
	    }
	  currentChild = newChild;
	}
    }
  newChild = NULL;
  vectorSize = expr_Pparsecheck.size();
  expr_PparsecheckT this_expr_Pparsecheck;
  for(int i = 0; i < vectorSize; i++)
    {
      this_expr_Pparsecheck = expr_Pparsecheck.at(i);
      if(this_expr_Pparsecheck != NULL)
	{
	  if(this_expr_Pparsecheck->which == this_expr_Pparsecheck->EXPR)
	    {
	      newChild = this_expr_Pparsecheck->expr_Pparsecheck.expr->makeBasicTree(thisNode);
	    }
	  else if(this_expr_Pparsecheck->which == this_expr_Pparsecheck->PPARSECHECK)
	    {
	      newChild = this_expr_Pparsecheck->expr_Pparsecheck.Pparsecheck->makeBasicTree(thisNode);
	    }

	  if(newChild != NULL)
	    {
	      if(currentChild == NULL)
		{
		  thisNode->firstChild = newChild;
		}
	      else
		{
		  currentChild->nextSibling = newChild;
		  newChild->previousSibling = currentChild;
		}
	    }
	  currentChild = newChild;
	}
    }
  return thisNode;
}

int ConstraintsS::canAddChildTypes(int* typeList, int max)
{
  if(max < 3)
    return -1;
  typeList[0] = PT_VARDECL;
  typeList[1] = PT_EXPR2;
  typeList[2] = PT_PPARSECHECK;
  return 3;
}

int ConstraintsS::removeChildNode(PNodeP* removeThis)
{
  int vectorSize;
  int i, j;
  PNodeP* thisPNode;

  if(removeThis == NULL)
    return -1;

  vectorSize = vardecl.size();
  for(i = 0; i < vectorSize; i++)
    {
      thisPNode = vardecl[i];
      if(thisPNode == removeThis)
	{
	  vectorSize--;
	  for(j = i; j < vectorSize; j++)
	    {
	      vardecl[j] = vardecl[j+1];
	    }
	  vardecl.pop_back();
	  return 0;
	}
    }

  expr_PparsecheckT this_expr_Pparsecheck;
  vectorSize = expr_Pparsecheck.size();
  for(i = 0; i < vectorSize; i++)
    {
      this_expr_Pparsecheck = expr_Pparsecheck[i];
      if(this_expr_Pparsecheck->which == this_expr_Pparsecheck->EXPR)
	thisPNode = this_expr_Pparsecheck->expr_Pparsecheck.expr;
      else if(this_expr_Pparsecheck->which == this_expr_Pparsecheck->PPARSECHECK)
	thisPNode = this_expr_Pparsecheck->expr_Pparsecheck.Pparsecheck;
      else
	thisPNode = NULL;
      if(thisPNode == removeThis)
	{
	  delete this_expr_Pparsecheck;
	  vectorSize--;
	  for(j = i; j < vectorSize; j++)
	    {
	      expr_Pparsecheck[j] = expr_Pparsecheck[j+1];
	    }
	  expr_Pparsecheck.pop_back();
	  return 0;
	}
    }
  return -1;
}

// new_methods_def_point


// **********

functionS::functionS() : PNodeP(PT_FUNCTION)
{
  name = NULL;
  expr = NULL;
}

PNodeP* functionS::addChild(PNodeP* newChild)
{
  switch(newChild->nodeType)
    {
    case PT_NAME:
      name = (nameT)newChild;
      break;
    case PT_EXPR:
    case PT_EXPR2:
      expr = (exprT)newChild;
      break;
    default:
      {
	throw PTException(PTE_CHILD_TYPE_ERR, nodeType, __LINE__);
	//throw except;
	return NULL;
      }
      break;
    }
  newChild->parent = (PNodeP*)this;
  return this;
}

void functionS::serializeXMLContents(int spaceDepth, PSTRING& printStr)
{
  name->serializeXMLRepresentation(spaceDepth + 1, printStr);
  expr->serializeXMLRepresentation(spaceDepth + 1, printStr);
}

bool functionS::validate(PNodeP** errorNode)
{
  if(name == NULL || expr == NULL)
    return false;
  if(name->validate(errorNode) == true &&
     expr->validate(errorNode) == true)
    return true;
  return false;
}

void functionS::codifyContents(int spaceDepth, PSTRING& printStr)
{
  //name->codifyRepresentation(spaceDepth, printStr);
  //DB_P(" ");
  //printStr.append(" ");
  expr->codifyRepresentation(spaceDepth, printStr);
}

bool functionS::canChangeToType(int newType)
{
  switch(newType)
    {
    default:
      break;
    }
  return false;
}

PNodeP* functionS::convertAndCopy(int newType, PNodeP* copyTo)
{
  return NULL;
}

int functionS::getTextualRepresentation(PSTRING& nameStr)
{
  nameStr.append("[function] ");
  name->getTextualRepresentation(nameStr);
  return 0;
}

void functionS::getNameStr(PSTRING& nameStr)
{
  name->getNameStr(nameStr);
}

PADSPNodeTreeNode* functionS::makeBasicTree(PADSPNodeTreeNode* parent)
{
  DB_P("making composite node::%s\n", PT_TypeNames[nodeType]);
  PADSPNodeTreeNode* thisNode = new PADSPNodeTreeNode(this, parent);

  PADSPNodeTreeNode *newChild = NULL, *currentChild = NULL;
  
  newChild = name->makeBasicTree(thisNode);
  newChild->setUpdateParentOnChange(true);
  newChild->setElementIsRequired(true);
  if(newChild != NULL)
    {
      if(currentChild == NULL)
	{
	  thisNode->firstChild = newChild;
	}
      else
	{
	  currentChild->nextSibling = newChild;
	  newChild->previousSibling = currentChild;
	}
    }
  currentChild = newChild;

  newChild = expr->makeBasicTree(thisNode);
  newChild->setElementIsRequired(true);
  if(newChild != NULL)
    {
      if(currentChild == NULL)
	{
	  thisNode->firstChild = newChild;
	}
      else
	{
	  currentChild->nextSibling = newChild;
	  newChild->previousSibling = currentChild;
	}
    }
  currentChild = newChild;

  return thisNode;
}

int functionS::makeCompleteNode()
{
  name = new nameS();
  expr = new exprS();
  expr->nodeType = PT_EXPR2;
  if(name->makeCompleteNode() != 0)
    return -1;
  if(expr->makeCompleteNode() != 0)
    return -1;
  return 0;
}

// new_methods_def_point


// **********

parameterS::parameterS() : PNodeP(PT_PARAMETER)
{
  var = NULL;
}

PNodeP* parameterS::addChild(PNodeP* newChild)
{
  switch(newChild->nodeType)
    {
    case PT_VAR:
      var = (varT)newChild;
      break;
    case PT_TYPENAME:
      typname = (typenameT)newChild;
      break;
    default:
      {
	throw PTException(PTE_CHILD_TYPE_ERR, nodeType, __LINE__);
	////throw except;
	return NULL;
      }
    }
  newChild->parent = (PNodeP*)this;
  return this;
}

void parameterS::serializeXMLContents(int spaceDepth, PSTRING& printStr)
{
  var->serializeXMLRepresentation(spaceDepth + 1, printStr);
  typname->serializeXMLRepresentation(spaceDepth + 1, printStr);
}

bool parameterS::validate(PNodeP** errorNode)
{
  if(var == NULL || typname == NULL)
    return false;
  if(var->validate(errorNode) == true &&
     typname->validate(errorNode) == true)
    return true;
  return false;
}

void parameterS::codifyContents(int spaceDepth, PSTRING& printStr)
{
  var->codifyRepresentation(spaceDepth, printStr);
  DB_P(" ");
  printStr.append(" ");
  typname->codifyRepresentation(spaceDepth, printStr);  
}

bool parameterS::canChangeToType(int newType)
{
  switch(newType)
    {
    default:
      break;
    }
  return false;
}

PNodeP* parameterS::convertAndCopy(int newType, PNodeP* copyTo)
{
  return NULL;
}

void parameterS::getTypeStr(PSTRING& typeStr)
{
  typname->getNameStr(typeStr);
}

void parameterS::getNameStr(PSTRING& nameStr)
{
  var->getNameStr(nameStr);
}

PADSPNodeTreeNode* parameterS::makeBasicTree(PADSPNodeTreeNode* parent)
{
  DB_P("making composite node::%s\n", PT_TypeNames[nodeType]);
  PADSPNodeTreeNode* thisNode = new PADSPNodeTreeNode(this, parent);

  PADSPNodeTreeNode *newChild = NULL, *currentChild = NULL;

  PSTRING tmpStr;
  tmpStr = PT_TypeNames[nodeType];
  tmpStr.append(": ");
  DB_P("getting textual representation for node of type %s\n", PT_TypeNames[nodeType]);
  getTextualRepresentation(tmpStr);
  DB_P("setting node name to %s\n", tmpStr.c_str());
  thisNode->setNodeName(tmpStr);
  
  newChild = typname->makeBasicTree(thisNode);
  newChild->setUpdateParentOnChange(true);
  newChild->setElementIsRequired(true);
  if(newChild != NULL)
    {
      if(currentChild == NULL)
	{
	  thisNode->firstChild = newChild;
	}
      else
	{
	  currentChild->nextSibling = newChild;
	  newChild->previousSibling = currentChild;
	}
    }
  currentChild = newChild;

  newChild = var->makeBasicTree(thisNode);
  newChild->setUpdateParentOnChange(true);
  newChild->setElementIsRequired(true);
  if(newChild != NULL)
    {
      if(currentChild == NULL)
	{
	  thisNode->firstChild = newChild;
	}
      else
	{
	  currentChild->nextSibling = newChild;
	  newChild->previousSibling = currentChild;
	}
    }
  currentChild = newChild;

  return thisNode;
}

int parameterS::makeCompleteNode()
{
  typname = new typenameS();
  var = new varS();
  if(typname->makeCompleteNode() != 0)
    return -1;
  if(var->makeCompleteNode() != 0)
    return -1;
  return 0;
}

// new_methods_def_point


// **********

modifiersS::modifiersS() : PNodeP(PT_MODIFIERS)
{
  has_Psource = false;
  Psource = NULL;

  has_Precord = false;
  Precord = NULL;

  has_Plongest = false;
  Plongest = NULL;
}

PNodeP* modifiersS::addChild(PNodeP* newChild)
{
  switch(newChild->nodeType)
    {
    case PT_PSOURCE:
      has_Psource = true;
      Psource = (PsourceT)newChild;
      break;
    case PT_PRECORD:
      has_Precord = true;
      Precord = (PrecordT)newChild;
      break;
    case PT_PLONGEST:
      has_Plongest = true;
      Plongest = (PlongestT)newChild;
      break;
    default:
      {
	throw PTException(PTE_CHILD_TYPE_ERR, nodeType, __LINE__);
	//throw except;
	return NULL;
      }
    }
  newChild->parent = (PNodeP*)this;
  return this;
}
/*
void modifiersS::serializeXMLContents(int spaceDepth)
{
  if(has_Psource)
    Psource->serializeXMLRepresentation(spaceDepth + 1, printStr);
  if(has_Precord)
    Precord->serializeXMLRepresentation(spaceDepth + 1, printStr);
  if(has_Plongest)
    Plongest->serializeXMLRepresentation(spaceDepth + 1, printStr);
}
*/
void modifiersS::serializeXMLRepresentation(int spaceDepth, PSTRING& printStr)
{
  if(has_Psource)
    Psource->serializeXMLRepresentation(spaceDepth + 1, printStr);
  if(has_Precord)
    Precord->serializeXMLRepresentation(spaceDepth + 1, printStr);
  if(has_Plongest)
    Plongest->serializeXMLRepresentation(spaceDepth + 1, printStr);
}

void modifiersS::codifyRepresentation(int spaceDepth, PSTRING& printStr)
{
  if(has_Psource)
    {
      Psource->codifyRepresentation(spaceDepth, printStr);
      DB_P(" ");
      printStr.append(" ");
    }
  if(has_Precord)
    {
      Precord->codifyRepresentation(spaceDepth, printStr);
      DB_P(" ");
      printStr.append(" ");
    }
  if(has_Plongest)
    {
      Plongest->codifyRepresentation(spaceDepth, printStr);
      DB_P(" ");
      printStr.append(" ");
    }
}

bool modifiersS::canChangeToType(int newType)
{
  switch(newType)
    {
    default:
      break;
    }
  return false;
}

PNodeP* modifiersS::convertAndCopy(int newType, PNodeP* copyTo)
{
  return NULL;
}

PADSPNodeTreeNode* modifiersS::makeBasicTree(PADSPNodeTreeNode* parent)
{
  DB_P("making composite node::%s\n", PT_TypeNames[nodeType]);
  PADSPNodeTreeNode* thisNode = new PADSPNodeTreeNode(this, parent);

  PADSPNodeTreeNode *newChild = NULL, *currentChild = NULL;
  
  if(has_Psource)
    {
      newChild = Psource->makeBasicTree(thisNode);
      if(newChild != NULL)
	{
	  if(currentChild == NULL)
	    {
	      thisNode->firstChild = newChild;
	    }
	  else
	    {
	      currentChild->nextSibling = newChild;
	      newChild->previousSibling = currentChild;
	    }
	}
      currentChild = newChild;
    }

  if(has_Precord)
    {
      newChild = Precord->makeBasicTree(thisNode);
      if(newChild != NULL)
	{
	  if(currentChild == NULL)
	    {
	      thisNode->firstChild = newChild;
	    }
	  else
	    {
	      currentChild->nextSibling = newChild;
	      newChild->previousSibling = currentChild;
	    }
	}
      currentChild = newChild;
    }

  if(has_Plongest)
    {
      newChild = Plongest->makeBasicTree(thisNode);
      if(newChild != NULL)
	{
	  if(currentChild == NULL)
	    {
	      thisNode->firstChild = newChild;
	    }
	  else
	    {
	      currentChild->nextSibling = newChild;
	      newChild->previousSibling = currentChild;
	    }
	}
      currentChild = newChild;
    }

  return thisNode;
}

int modifiersS::canAddChildTypes(int* typeList, int max)
{
  if(max < 3)
    return -1;
  int i = 0;
  if(!has_Psource)
    {
      typeList[i] = PT_PSOURCE;
      i++;
    }
  if(!has_Precord)
    {
      typeList[i] = PT_PRECORD;
      i++;
    }
  if(!has_Plongest)
    {
      typeList[i] = PT_PLONGEST;
      i++;
    }
  return i;
}

int modifiersS::removeChildNode(PNodeP* removeThis)
{
  if(Psource == removeThis && has_Psource)
    {
      has_Psource = false;
      Psource = NULL;
      return 0;
    }

  if(Precord == removeThis && has_Precord)
    {
      has_Precord = false;
      Precord = NULL;
      return 0;
    }
  
  if(Plongest == removeThis && has_Plongest)
    {
      has_Plongest = false;
      Plongest = NULL;
      return 0;
    }

  return -1;
}

// new_methods_def_point


// **********

declS::declS() : PNodeP(PT_DECL)
{
  name = NULL;
}

PNodeP* declS::addChild(PNodeP* newChild)
{
  switch(newChild->nodeType)
    {
    case PT_NAME:
      name = (nameT)newChild;
      break;
    case PT_PARAMETER:
      parameter.push_back((parameterT)newChild);
      break;
    default:
      {
	throw PTException(PTE_CHILD_TYPE_ERR, nodeType, __LINE__);
	//throw except;
	return NULL;
      }
    }
  newChild->parent = (PNodeP*)this;
  return this;
}

void declS::serializeXMLContents(int spaceDepth, PSTRING& printStr)
{
  name->serializeXMLRepresentation(spaceDepth + 1, printStr);
  int size = parameter.size();
  for(int i = 0; i < size; i++)
    {
      parameter.at(i)->serializeXMLRepresentation(spaceDepth + 1, printStr);
    }
}
/*
void declS::serializeXMLClose(int spaceDepth)
{
  printf("</%s>", PT_TypeNames[nodeType]);
  printf("\n");
  int numSpaces = spaceDepth * XML_ADD_LEVEL_SPACE
  for(int i = 0; i < numSpaces; i++)
    printf(" ");
}
*/

void declS::codifyRepresentation(int spaceDepth, PSTRING& printStr)
{
  name->codifyRepresentation(spaceDepth, printStr);
  int size = parameter.size();
  if(size > 0)
    {
      DB_P("(");
      printStr.append("(");
      for(int i = 0; i < size; i++)
	{
	  if(i > 0)
	    {
	      DB_P(", ");
	      printStr.append(", ");
	    }
	  parameter.at(i)->codifyRepresentation(spaceDepth, printStr);
	}
      DB_P(")");
      printStr.append(")");
    }
}

bool declS::canChangeToType(int newType)
{
  switch(newType)
    {
    default:
      break;
    }
  return false;
}

PNodeP* declS::convertAndCopy(int newType, PNodeP* copyTo)
{
  return NULL;
}

int declS::getTextualRepresentation(PSTRING& nameStr)
{
  name->getTextualRepresentation(nameStr);
  return 0;
}

void declS::getNameStr(PSTRING& nameStr)
{
  name->getNameStr(nameStr);
}

PADSPNodeTreeNode* declS::makeBasicTree(PADSPNodeTreeNode* parent)
{
  DB_P("making composite node::%s\n", PT_TypeNames[nodeType]);
  PADSPNodeTreeNode* thisNode = new PADSPNodeTreeNode(this, parent);

  PADSPNodeTreeNode *newChild = NULL, *currentChild = NULL;

  PSTRING tmpStr;
  tmpStr = PT_TypeNames[nodeType];
  tmpStr.append(": ");
  DB_P("getting textual representation for node of type %s\n", PT_TypeNames[nodeType]);
  getTextualRepresentation(tmpStr);
  DB_P("setting node name to %s\n", tmpStr.c_str());
  thisNode->setNodeName(tmpStr);

  newChild = name->makeBasicTree(thisNode);
  newChild->setUpdateParentOnChange(true);
  newChild->setElementIsRequired(true);
  if(currentChild == NULL)
    {
      thisNode->firstChild = newChild;
    }
  else
    {
      currentChild->nextSibling = newChild;
      newChild->previousSibling = currentChild;
    }
  currentChild = newChild;

  parameterT this_parameter = NULL;
  int vectorSize = parameter.size();
  for(int i = 0; i < vectorSize; i++)
    {
      this_parameter = parameter.at(i);
      newChild = this_parameter->makeBasicTree(thisNode);
      if(currentChild == NULL)
	{
	  thisNode->firstChild = newChild;
	}
      else
	{
	  currentChild->nextSibling = newChild;
	  newChild->previousSibling = currentChild;
	}
      currentChild = newChild;
    }
  return thisNode;
}

int declS::canAddChildTypes(int* typeList, int max)
{
  if(max < 1)
    return -1;
  typeList[0] = PT_PARAMETER;
  return 1;
}

int declS::makeCompleteNode()
{
  name = new nameS();
  return name->makeCompleteNode();
}

int declS::removeChildNode(PNodeP* removeThis)
{
  int vectorSize;
  int i, j;
  PNodeP* thisPNode;

  if(removeThis == NULL)
    return -1;

  vectorSize = parameter.size();
  for(i = 0; i < vectorSize; i++)
    {
      thisPNode = parameter[i];
      if(thisPNode == removeThis)
	{
	  vectorSize--;
	  for(j = i; j < vectorSize; j++)
	    {
	      parameter[j] = parameter[j+1];
	    }
	  parameter.pop_back();
	  return 0;
	}
    }
  return -1;
}

// new_methods_def_point


// **********

ptypeS::ptypeS() : PNodeP(PT_PTYPE)
{
  name = NULL;
}

PNodeP* ptypeS::addChild(PNodeP* newChild)
{
  switch(newChild->nodeType)
    {
    case PT_NAME:
      name = (nameT)newChild;
      break;
    case PT_ARGUMENT:
      argument.push_back((argumentT)newChild);
      break;
    default:
      {
	throw PTException(PTE_CHILD_TYPE_ERR, nodeType, __LINE__);
	//throw except;
	return NULL;
      }
      break;
    }
  newChild->parent = (PNodeP*)this;
  return this;
}

void ptypeS::serializeXMLContents(int spaceDepth, PSTRING& printStr)
{
  name->serializeXMLRepresentation(spaceDepth + 1, printStr);
  int size = argument.size();
  for(int i = 0; i < size; i++)
    {
      argument.at(i)->serializeXMLRepresentation(spaceDepth + 1, printStr);
    }
}

void ptypeS::codifyRepresentation(int spaceDepth, PSTRING& printStr)
{
  name->codifyRepresentation(spaceDepth, printStr);
  int size = argument.size();
  if(size > 0)
    {
      DB_P("(:");
      printStr.append("(:");
      for(int i = 0; i < size; i++)
	{
	  if(i > 0)
	    {
	      DB_P(", ");
	      printStr.append(", ");
	    }
	  DB_P("\"");
	  printStr.append("\"");
	  argument.at(i)->codifyRepresentation(spaceDepth, printStr);
	  DB_P("\"");
	  printStr.append("\"");
	}
      DB_P(":)");
      printStr.append(":)");
    }
}

bool ptypeS::canChangeToType(int newType)
{
  switch(newType)
    {
    default:
      break;
    }
  return false;
}

PNodeP* ptypeS::convertAndCopy(int newType, PNodeP* copyTo)
{
  return NULL;
}

int ptypeS::getTextualRepresentation(PSTRING& nameStr)
{
  name->getTextualRepresentation(nameStr);
  return 0;
}

void ptypeS::getNameStr(PSTRING& nameStr)
{
  name->getNameStr(nameStr);
}

PADSPNodeTreeNode* ptypeS::makeBasicTree(PADSPNodeTreeNode* parent)
{
  DB_P("making composite node::%s\n", PT_TypeNames[nodeType]);
  PADSPNodeTreeNode* thisNode = new PADSPNodeTreeNode(this, parent);

  PADSPNodeTreeNode *newChild = NULL, *currentChild = NULL;

  PSTRING tmpStr;
  tmpStr = PT_TypeNames[nodeType];
  tmpStr.append(": ");
  DB_P("getting textual representation for node of type %s\n", PT_TypeNames[nodeType]);
  getTextualRepresentation(tmpStr);
  DB_P("setting node name to %s\n", tmpStr.c_str());
  thisNode->setNodeName(tmpStr);

  newChild = name->makeBasicTree(thisNode);
  newChild->setUpdateParentOnChange(true);
  newChild->setElementIsRequired(true);
  if(currentChild == NULL)
    {
      thisNode->firstChild = newChild;
    }
  else
    {
      currentChild->nextSibling = newChild;
      newChild->previousSibling = currentChild;
    }
  currentChild = newChild;
  
  argumentT this_argument = NULL;
  int vectorSize = argument.size();
  for(int i = 0; i < vectorSize; i++)
    {
      this_argument = argument.at(i);
      newChild = this_argument->makeBasicTree(thisNode);
      if(currentChild == NULL)
	{
	  thisNode->firstChild = newChild;
	}
      else
	{
	  currentChild->nextSibling = newChild;
	  newChild->previousSibling = currentChild;
	}
      currentChild = newChild;
    }
  return thisNode;
}

int ptypeS::canAddChildTypes(int* typeList, int max)
{
  if(max < 1)
    return -1;
  typeList[0] = PT_ARGUMENT;
  return 1;
}

int ptypeS::makeCompleteNode()
{
  name = new nameS();
  return name->makeCompleteNode();
}

int ptypeS::removeChildNode(PNodeP* removeThis)
{
  int vectorSize;
  int i, j;
  PNodeP* thisPNode;

  if(removeThis == NULL)
    return -1;

  vectorSize = argument.size();
  for(i = 0; i < vectorSize; i++)
    {
      thisPNode = argument[i];
      if(thisPNode == removeThis)
	{
	  vectorSize--;
	  for(j = i; j < vectorSize; j++)
	    {
	      argument[j] = argument[j+1];
	    }
	  argument.pop_back();
	  return 0;
	}
    }
  return -1;
}

// new_methods_def_point


// **********

forAllS::forAllS() : PNodeP(PT_FORALL)
{
  var = NULL;
  min = NULL;
  max = NULL;
  constraints = NULL;
}

PNodeP* forAllS::addChild(PNodeP* newChild)
{
  switch(newChild->nodeType)
    {
    case PT_VAR:
      var = (varT)newChild;
      break;
    case PT_MIN_LOCAL:
      min = (ExprT)newChild;
      break;
    case PT_MAX_LOCAL:
      max = (ExprT)newChild;
      break;
    case PT_CONSTRAINTS:
    case PT_CONSTRAINTS2:
      constraints = (constraintsT)newChild;
      break;
    default:
      {
	throw PTException(PTE_CHILD_TYPE_ERR, nodeType, __LINE__);
	//throw except;
	return NULL;
      }
    }
  newChild->parent = (PNodeP*)this;
  return this;
}

void forAllS::serializeXMLContents(int spaceDepth, PSTRING& printStr)
{
  var->serializeXMLRepresentation(spaceDepth + 1, printStr);
  min->serializeXMLRepresentation(spaceDepth + 1, printStr);
  max->serializeXMLRepresentation(spaceDepth + 1, printStr);
  constraints->serializeXMLRepresentation(spaceDepth + 1, printStr);
}

void forAllS::codifyContents(int spaceDepth, PSTRING& printStr)
{
  DB_P("Pforall (");
  printStr.append("Pforall (");
  var->codifyRepresentation(0, printStr);
  DB_P(" Pin [");
  printStr.append(" Pin [");
  min->codifyRepresentation(0, printStr);
  DB_P("..");
  printStr.append("..");
  max->codifyRepresentation(0, printStr);
  DB_P("] :\n");
  printStr.append("] :\n");
  constraints->codifyRepresentation(spaceDepth + 1, printStr);
  DB_P(")");
  printStr.append(")");
}

bool forAllS::canChangeToType(int newType)
{
  switch(newType)
    {
    default:
      break;
    }
  return false;
}

PNodeP* forAllS::convertAndCopy(int newType, PNodeP* copyTo)
{
  return NULL;
}

PADSPNodeTreeNode* forAllS::makeBasicTree(PADSPNodeTreeNode* parent)
{
  DB_P("making composite node::%s\n", PT_TypeNames[nodeType]);
  PADSPNodeTreeNode* thisNode = new PADSPNodeTreeNode(this, parent);

  PADSPNodeTreeNode *newChild = NULL, *currentChild = NULL;

  newChild = var->makeBasicTree(thisNode);
  newChild->setElementIsRequired(true);
  if(currentChild == NULL)
    {
      thisNode->firstChild = newChild;
    }
  else
    {
      currentChild->nextSibling = newChild;
      newChild->previousSibling = currentChild;
    }
  currentChild = newChild;
  
  newChild = min->makeBasicTree(thisNode);
  newChild->setElementIsRequired(true);
  if(currentChild == NULL)
    {
      thisNode->firstChild = newChild;
    }
  else
    {
      currentChild->nextSibling = newChild;
      newChild->previousSibling = currentChild;
    }
  currentChild = newChild;

  newChild = max->makeBasicTree(thisNode);
  newChild->setElementIsRequired(true);
  if(currentChild == NULL)
    {
      thisNode->firstChild = newChild;
    }
  else
    {
      currentChild->nextSibling = newChild;
      newChild->previousSibling = currentChild;
    }
  currentChild = newChild;

  newChild = constraints->makeBasicTree(thisNode);
  newChild->setElementIsRequired(true);
  if(currentChild == NULL)
    {
      thisNode->firstChild = newChild;
    }
  else
    {
      currentChild->nextSibling = newChild;
      newChild->previousSibling = currentChild;
    }
  currentChild = newChild;

  return thisNode;
}

int forAllS::makeCompleteNode()
{
  var = new varS();
  min = new ExprS();
  max = new ExprS();
  constraints = new constraintsS();
  min->nodeType = PT_MIN_LOCAL;
  max->nodeType = PT_MAX_LOCAL;
  constraints->nodeType = PT_CONSTRAINTS2;
  if(var->makeCompleteNode() != 0)
    return -1;
  if(max->makeCompleteNode() != 0)
    return -1;
  if(min->makeCompleteNode() != 0)
    return -1;
  if(constraints->makeCompleteNode() != 0)
    return -1;
  return 0;
}

// new_methods_def_point


// **********

sizeS::sizeS() : PNodeP(PT_SIZE)
{
  has_min = false;
  min = NULL;

  has_max = false;
  max = NULL;
}

PNodeP* sizeS::addChild(PNodeP* newChild)
{
  switch(newChild->nodeType)
    {
    case PT_MIN_LOCAL:
      has_min = true;
      min = (ExprT)newChild;
      break;
    case PT_MAX_LOCAL:
      has_max = true;
      max = (ExprT)newChild;
      break;
    default:
      {
	throw PTException(PTE_CHILD_TYPE_ERR, nodeType, __LINE__);
	//throw except;
	return NULL;
      }
    }
  newChild->parent = (PNodeP*)this;
  return this;
}

void sizeS::serializeXMLContents(int spaceDepth, PSTRING& printStr)
{
  if(has_min)
    min->serializeXMLRepresentation(spaceDepth + 1, printStr);
  if(has_max)
    max->serializeXMLRepresentation(spaceDepth + 1, printStr);
}

void sizeS::codifyRepresentation(int spaceDepth, PSTRING& printStr)
{
  if(has_min)
    min->codifyRepresentation(0, printStr);
  if(has_min && has_max)
    {
      DB_P(" : ");
      printStr.append(" : ");
    }
  if(has_max)
    max->codifyRepresentation(0, printStr);
}

bool sizeS::canChangeToType(int newType)
{
  switch(newType)
    {
    default:
      break;
    }
  return false;
}

PNodeP* sizeS::convertAndCopy(int newType, PNodeP* copyTo)
{
  return NULL;
}

PADSPNodeTreeNode* sizeS::makeBasicTree(PADSPNodeTreeNode* parent)
{
  DB_P("making composite node::%s\n", PT_TypeNames[nodeType]);
  PADSPNodeTreeNode* thisNode = new PADSPNodeTreeNode(this, parent);

  PADSPNodeTreeNode *newChild = NULL, *currentChild = NULL;

  if(has_min)
    {
      newChild = min->makeBasicTree(thisNode);
      if(currentChild == NULL)
	{
	  thisNode->firstChild = newChild;
	}
      else
	{
	  currentChild->nextSibling = newChild;
	  newChild->previousSibling = currentChild;
	}
      currentChild = newChild;
    }

  if(has_max)
    {
      newChild = max->makeBasicTree(thisNode);
      if(currentChild == NULL)
	{
	  thisNode->firstChild = newChild;
	}
      else
	{
	  currentChild->nextSibling = newChild;
	  newChild->previousSibling = currentChild;
	}
      currentChild = newChild;
    }

  return thisNode;
}

int sizeS::canAddChildTypes(int* typeList, int max)
{
  if(max < 2)
    return -1;
  int i = 0;
  if(!has_min)
    {
      typeList[i] = PT_MIN_LOCAL;
      i++;
    }
  if(!has_max)
    {
      typeList[i] = PT_MAX_LOCAL;
      i++;
    }
  return i;
}


int sizeS::removeChildNode(PNodeP* removeThis)
{
  if(min == removeThis && has_min)
    {
      has_min = false;
      min = NULL;
      return 0;
    }

  if(max == removeThis && has_max)
    {
      has_max = false;
      max = NULL;
      return 0;
    }
  
  return -1;
}

// new_methods_def_point


// **********

arrayConstraintsS::arrayConstraintsS() : PNodeP(PT_ARRAYCONSTRAINTS)
{
  has_size = false;
  size = NULL;
}

PNodeP* arrayConstraintsS::addChild(PNodeP* newChild)
{
  switch(newChild->nodeType)
    {
    case PT_SIZE:
      has_size = true;
      size = (sizeT)newChild;
      break;
    case PT_FORALL:
    case PT_PPARSECHECK:
      {
	forAll_PparsecheckT fp = new forAll_PparsecheckS();
	fp->which = newChild->nodeType == PT_FORALL ? fp->FORALL : fp->PPARSECHECK;
	if(fp->which == fp->FORALL)
	  fp->forAll_Pparsecheck.forAll = (forAllT)newChild;
	else
	  fp->forAll_Pparsecheck.Pparsecheck = (PparsecheckT)newChild;
	forAll_Pparsecheck.push_back(fp);
      }
      break;
    default:
      {
	throw PTException(PTE_CHILD_TYPE_ERR, nodeType, __LINE__);
	//throw except;
	return NULL;
      }
    }
  newChild->parent = (PNodeP*)this;
  return this;
}

void arrayConstraintsS::serializeXMLContents(int spaceDepth, PSTRING& printStr)
{
  if(has_size)
    size->serializeXMLRepresentation(spaceDepth + 1, printStr);
  int vsize;
  vsize = forAll_Pparsecheck.size();
  forAll_PparsecheckT this_forAll_Pparsecheck;
  for(int i = 0; i < vsize; i++)
    {
      this_forAll_Pparsecheck = forAll_Pparsecheck.at(i);
      if(this_forAll_Pparsecheck->which == this_forAll_Pparsecheck->FORALL)
	this_forAll_Pparsecheck->forAll_Pparsecheck.forAll->serializeXMLRepresentation(spaceDepth + 1, printStr);
      else if(this_forAll_Pparsecheck->which == this_forAll_Pparsecheck->PPARSECHECK)
	this_forAll_Pparsecheck->forAll_Pparsecheck.Pparsecheck->serializeXMLRepresentation(spaceDepth + 1, printStr);
      else
	{
	  throw PTException(PTE_CHILD_UNDEF_ERR, nodeType, __LINE__);
	  //throw except;
	}
    }
}

void arrayConstraintsS::codifyRepresentation(int spaceDepth, PSTRING& printStr)
{
  int vsize;
  vsize = forAll_Pparsecheck.size();
  forAll_PparsecheckT this_forAll_Pparsecheck;
  for(int i = 0; i < vsize; i++)
    {
      if(i != 0)
	{
	  DB_P(" && \n   ");
	  printStr.append(" && \n");
	  printStr.append(spaceDepth * CODE_ADD_LEVEL_SPACE, ' ');
	}
      this_forAll_Pparsecheck = forAll_Pparsecheck.at(i);
      if(this_forAll_Pparsecheck->which == this_forAll_Pparsecheck->FORALL)
	this_forAll_Pparsecheck->forAll_Pparsecheck.forAll->codifyRepresentation(spaceDepth + 1, printStr);
      else if(this_forAll_Pparsecheck->which == this_forAll_Pparsecheck->PPARSECHECK)
	this_forAll_Pparsecheck->forAll_Pparsecheck.Pparsecheck->codifyRepresentation(spaceDepth + 1, printStr);
      else
	{
	  throw PTException(PTE_CHILD_UNDEF_ERR, nodeType, __LINE__);
	  //throw except;
	}
    }
}

bool arrayConstraintsS::canChangeToType(int newType)
{
  switch(newType)
    {
    default:
      break;
    }
  return false;
}

PNodeP* arrayConstraintsS::convertAndCopy(int newType, PNodeP* copyTo)
{
  return NULL;
}

PADSPNodeTreeNode* arrayConstraintsS::makeBasicTree(PADSPNodeTreeNode* parent)
{
  DB_P("making composite node::%s\n", PT_TypeNames[nodeType]);
  PADSPNodeTreeNode* thisNode = new PADSPNodeTreeNode(this, parent);

  PADSPNodeTreeNode *newChild = NULL, *currentChild = NULL;

  if(has_size)
    {
      newChild = size->makeBasicTree(thisNode);
      if(newChild != NULL)
	{
	  if(currentChild == NULL)
	    {
	      thisNode->firstChild = newChild;
	    }
	  else
	    {
	      currentChild->nextSibling = newChild;
	      newChild->previousSibling = currentChild;
	    }
	  currentChild = newChild;
	}

    }
  
  forAll_PparsecheckT this_forAll_Pparsecheck = NULL;
  int vectorSize = forAll_Pparsecheck.size();
  for(int i = 0; i < vectorSize; i++)
    {
      this_forAll_Pparsecheck = forAll_Pparsecheck.at(i);
      if(this_forAll_Pparsecheck->which == this_forAll_Pparsecheck->FORALL)
	newChild = this_forAll_Pparsecheck->forAll_Pparsecheck.forAll->makeBasicTree(thisNode);
      else if(this_forAll_Pparsecheck->which == this_forAll_Pparsecheck->PPARSECHECK)
	newChild = this_forAll_Pparsecheck->forAll_Pparsecheck.forAll->makeBasicTree(thisNode);

      if(newChild != NULL)
	{
	  if(currentChild == NULL)
	    {
	      thisNode->firstChild = newChild;
	    }
	  else
	    {
	      currentChild->nextSibling = newChild;
	      newChild->previousSibling = currentChild;
	    }
	  currentChild = newChild;
	}
      newChild = NULL;
    }
  return thisNode;
}

int arrayConstraintsS::canAddChildTypes(int* typeList, int max)
{
  if(max < 2)
    return -1;
  int i = 0;
  if(!has_size)
    {
      typeList[i] = PT_SIZE;
      i++;
    }
  typeList[i] = PT_FORALL;
  i++;
  typeList[i] = PT_PPARSECHECK;
  i++;
  return i;
}

int arrayConstraintsS::removeChildNode(PNodeP* removeThis)
{
  int vectorSize;
  int i, j;
  PNodeP* thisPNode;

  if(removeThis == NULL)
    return -1;

  if(size == removeThis && has_size)
    {
      has_size = false;
      size = NULL;
      return 0;
    }
  
  forAll_PparsecheckT this_forAll_Pparsecheck;
  vectorSize = forAll_Pparsecheck.size();
  for(i = 0; i < vectorSize; i++)
    {
      this_forAll_Pparsecheck = forAll_Pparsecheck[i];
      if(this_forAll_Pparsecheck->which == this_forAll_Pparsecheck->FORALL)
	thisPNode = this_forAll_Pparsecheck->forAll_Pparsecheck.forAll;
      else if(this_forAll_Pparsecheck->which == this_forAll_Pparsecheck->PPARSECHECK)
	thisPNode = this_forAll_Pparsecheck->forAll_Pparsecheck.Pparsecheck;
      else
	thisPNode = NULL;
      if(thisPNode == removeThis)
	{
	  delete this_forAll_Pparsecheck;
	  vectorSize--;
	  for(j = i; j < vectorSize; j++)
	    {
	      forAll_Pparsecheck[j] = forAll_Pparsecheck[j+1];
	    }
	  forAll_Pparsecheck.pop_back();
	  return 0;
	}
    }
  return -1;
}

// new_methods_def_point


// **********

TermExprS::TermExprS() : PNodeP(PT_TERMEXPR)
{
  which = UNDEFINED;
  Pnosep = false;
  regexp = false;
}

PNodeP* TermExprS::addChild(PNodeP* newChild)
{
  switch(newChild->nodeType)
    {
    case PT_NOSEP_LOCAL:
      which = PNOSEP;
      Pnosep = (unitT)newChild;
      break;
    case PT_REEXPR:
      which = REGEXP;
      regexp = (REExprT)newChild;
      break;
    default:
      {
	throw PTException(PTE_CHILD_TYPE_ERR, nodeType, __LINE__);
	//throw except;
	return NULL;
      }
    }
  newChild->parent = (PNodeP*)this;
  return this;
}

void TermExprS::serializeXMLContents(int spaceDepth, PSTRING& printStr)
{
  if(which == PNOSEP)
    Pnosep->serializeXMLRepresentation(spaceDepth + 1, printStr);
  else if(which == REGEXP)
    regexp->serializeXMLRepresentation(spaceDepth + 1, printStr);
  else
    {
      throw PTException(PTE_CHILD_UNDEF_ERR, nodeType, __LINE__);
      //throw except;
    }
}

void TermExprS::codifyRepresentation(int spaceDepth, PSTRING& printStr)
{
  if(which == PNOSEP)
    Pnosep->codifyRepresentation(spaceDepth, printStr);
  else if(which == REGEXP)
    regexp->codifyRepresentation(spaceDepth, printStr);
  else
    {
      throw PTException(PTE_CHILD_UNDEF_ERR, nodeType, __LINE__);
      //throw except;
    }
}

bool TermExprS::canChangeToType(int newType)
{
  switch(newType)
    {
    default:
      break;
    }
  return false;
}

PNodeP* TermExprS::convertAndCopy(int newType, PNodeP* copyTo)
{
  return NULL;
}

int TermExprS::getTextualRepresentation(PSTRING& nameStr)
{
  nameStr.append(PT_TypeNames[nodeType]);
  nameStr.append(": ");
  if(which == REGEXP)
    regexp->getTextualRepresentation(nameStr);
  else if(which == PNOSEP)
    Pnosep->getTextualRepresentation(nameStr);
  return 0;
}

PADSPNodeTreeNode* TermExprS::makeBasicTree(PADSPNodeTreeNode* parent)
{
  DB_P("making composite node::%s\n", PT_TypeNames[nodeType]);
  PADSPNodeTreeNode* thisNode = new PADSPNodeTreeNode(this, parent);

  PADSPNodeTreeNode *newChild = NULL, *currentChild = NULL;

  if(which == PNOSEP)
    {
      newChild = Pnosep->makeBasicTree(thisNode);
      newChild->setElementIsRequired(true);
      if(newChild != NULL)
	{
	  if(currentChild == NULL)
	    {
	      thisNode->firstChild = newChild;
	    }
	  else
	    {
	      currentChild->nextSibling = newChild;
	      newChild->previousSibling = currentChild;
	    }
	  currentChild = newChild;
	}

    }
  else if(which == REGEXP)
    {
      newChild = regexp->makeBasicTree(thisNode);
      newChild->setElementIsRequired(true);
      if(newChild != NULL)
	{
	  if(currentChild == NULL)
	    {
	      thisNode->firstChild = newChild;
	    }
	  else
	    {
	      currentChild->nextSibling = newChild;
	      newChild->previousSibling = currentChild;
	    }
	  currentChild = newChild;
	}

    }

  return thisNode;
}

int TermExprS::canAddChildTypes(int* typeList, int max)
{
  if(max < 1)
    return -1;
  int i = 0;
  if(which != PNOSEP)
    {
      typeList[i] = PT_NOSEP_LOCAL;
      i++;
    }
  if(which != REGEXP)
    {
      typeList[i] = PT_REEXPR;
      i++;
    }
  return 1;
}

// new_methods_def_point


// **********

delimiterConstraintsS::delimiterConstraintsS() : PNodeP(PT_DELIMITERCONSTRAINTS)
{
  has_sep = false;
  sep = NULL;

  has_term = false;
  term = NULL;

  has_last = false;
  last = NULL;
  
  has_ended = false;
  ended = NULL;

  has_omit = false;
  omit = NULL;

  has_longest = false;
  Plongest = false;
}

PNodeP* delimiterConstraintsS::addChild(PNodeP* newChild)
{
  switch(newChild->nodeType)
    {
    case PT_SEP_LOCAL:
      has_sep = true;
      sep = (REExprT)newChild;
      break;
    case PT_TERM_LOCAL:
      has_term = true;
      term = (TermExprT)newChild;
      break;
    case PT_LAST_LOCAL:
      has_last = true;
      last = (ConstraintsT)newChild;
      break;
    case PT_ENDED_LOCAL:
      has_ended = true;
      ended = (ConstraintsT)newChild;
      break;
    case PT_OMIT_LOCAL:
      has_omit = true;
      omit = (ConstraintsT)newChild;
      break;
    case PT_PLONGEST:
      has_longest = true;
      Plongest = (PlongestT)newChild;
      break;
    default:
      {
	throw PTException(PTE_CHILD_TYPE_ERR, nodeType, __LINE__);
	//throw except;
	return NULL;
      }
    }
  newChild->parent = (PNodeP*)this;
  return this;
}

void delimiterConstraintsS::serializeXMLContents(int spaceDepth, PSTRING& printStr)
{
  if(has_sep)
    sep->serializeXMLRepresentation(spaceDepth + 1, printStr);
  if(has_term)
    term->serializeXMLRepresentation(spaceDepth + 1, printStr);
  if(has_last)
    last->serializeXMLRepresentation(spaceDepth + 1, printStr);
  if(has_ended)
    ended->serializeXMLRepresentation(spaceDepth + 1, printStr);
  if(has_omit)
    omit->serializeXMLRepresentation(spaceDepth + 1, printStr);
  if(has_longest)
    Plongest->serializeXMLRepresentation(spaceDepth + 1, printStr);
}

void delimiterConstraintsS::codifyContents(int spaceDepth, PSTRING& printStr)
{

  if(has_sep)
    {
      DB_P("Psep(");
      printStr.append("Psep(");
      sep->codifyRepresentation(0, printStr);
      DB_P(")");
      printStr.append(")");
  
      if(has_term || has_last || has_ended || has_omit || has_longest)
	{
	  DB_P(" && ");
	  printStr.append(" && ");
	}
    }

  if(has_term)
    {
      DB_P("Pterm(");
      printStr.append("Pterm(");
      term->codifyRepresentation(0, printStr);
      DB_P(")");
      printStr.append(")");
    
      if(has_last || has_ended || has_omit || has_longest)
	{
	  DB_P(" && ");
	  printStr.append(" && ");
	}
    }

  if(has_last)
    {
      DB_P("Plast(");
      printStr.append("Plast(");
      last->codifyRepresentation(0, printStr);
      DB_P(")");
      printStr.append(")");
      
      if(has_ended || has_omit || has_longest)
	{
	  DB_P(" && ");
	  printStr.append(" && ");
	}
    }
  
  if(has_ended)
    {
      DB_P("Pended(");
      printStr.append("Pended(");
      ended->codifyRepresentation(0, printStr);
      DB_P(")");
      printStr.append(")");
      
      if(has_omit || has_longest)
	{
	  DB_P(" && ");
	  printStr.append(" && ");
	}
    }

  if(has_omit)
    {
      DB_P("Pomit(");
      printStr.append("Pomit(");
      omit->codifyRepresentation(0, printStr);
      DB_P(")");
      printStr.append(")");

      if(has_longest)
	{
	  DB_P(" && ");
	  printStr.append(" && ");
	}
    }

  if(has_longest)
    {
      DB_P("Plongest(");
      printStr.append("Plongest(");
      Plongest->codifyRepresentation(0, printStr);
      DB_P(")");
      printStr.append(")");
    }
}

bool delimiterConstraintsS::canChangeToType(int newType)
{
  switch(newType)
    {
    default:
      break;
    }
  return false;
}

PNodeP* delimiterConstraintsS::convertAndCopy(int newType, PNodeP* copyTo)
{
  return NULL;
}

PADSPNodeTreeNode* delimiterConstraintsS::makeBasicTree(PADSPNodeTreeNode* parent)
{
  DB_P("making composite node::%s\n", PT_TypeNames[nodeType]);
  PADSPNodeTreeNode* thisNode = new PADSPNodeTreeNode(this, parent);

  PADSPNodeTreeNode *newChild = NULL, *currentChild = NULL;

  if(has_sep)
    {
      newChild = sep->makeBasicTree(thisNode);
      if(newChild != NULL)
	{
	  if(currentChild == NULL)
	    {
	      thisNode->firstChild = newChild;
	    }
	  else
	    {
	      currentChild->nextSibling = newChild;
	      newChild->previousSibling = currentChild;
	    }
	  currentChild = newChild;
	}
    }
  if(has_term)
    {
      newChild = term->makeBasicTree(thisNode);
      if(newChild != NULL)
	{
	  if(currentChild == NULL)
	    {
	      thisNode->firstChild = newChild;
	    }
	  else
	    {
	      currentChild->nextSibling = newChild;
	      newChild->previousSibling = currentChild;
	    }
	  currentChild = newChild;
	}
    }
  
  if(has_last)
    {
      newChild = last->makeBasicTree(thisNode);

      if(newChild != NULL)
	{
	  if(currentChild == NULL)
	    {
	      thisNode->firstChild = newChild;
	    }
	  else
	    {
	      currentChild->nextSibling = newChild;
	      newChild->previousSibling = currentChild;
	    }
	  currentChild = newChild;
	}
    }
  if(has_ended)
    {
      newChild = ended->makeBasicTree(thisNode);
      if(newChild != NULL)
	{
	  if(currentChild == NULL)
	    {
	      thisNode->firstChild = newChild;
	    }
	  else
	    {
	      currentChild->nextSibling = newChild;
	      newChild->previousSibling = currentChild;
	    }
	  currentChild = newChild;
	}
    }
  if(has_omit)
    {
      newChild = omit->makeBasicTree(thisNode);
      if(newChild != NULL)
	{
	  if(currentChild == NULL)
	    {
	      thisNode->firstChild = newChild;
	    }
	  else
	    {
	      currentChild->nextSibling = newChild;
	      newChild->previousSibling = currentChild;
	    }
	  currentChild = newChild;
	}
    }
  if(has_longest)
    {
      newChild = Plongest->makeBasicTree(thisNode);
      if(newChild != NULL)
	{
	  if(currentChild == NULL)
	    {
	      thisNode->firstChild = newChild;
	    }
	  else
	    {
	      currentChild->nextSibling = newChild;
	      newChild->previousSibling = currentChild;
	    }
	  currentChild = newChild;
	}
    }

  return thisNode;
}

int delimiterConstraintsS::canAddChildTypes(int* typeList, int max)
{
  if(max < 6)
    return -1;
  int i = 0;
  if(!has_sep)
    {
      typeList[i] = PT_SEP_LOCAL;
      i++;
    }
  if(!has_term)
    {
      typeList[i] = PT_TERM_LOCAL;
      i++;
    }
  if(!has_last)
    {
      typeList[i] = PT_LAST_LOCAL;
      i++;
    }
  if(!has_ended)
    {
      typeList[i] = PT_ENDED_LOCAL;
      i++;
    }
  if(!has_omit)
    {
      typeList[i] = PT_OMIT_LOCAL;
      i++;
    }
  if(!has_longest)
    {
      typeList[i] = PT_PLONGEST;
      i++;
    }
  return i;
}

int delimiterConstraintsS::removeChildNode(PNodeP* removeThis)
{

  if(removeThis == NULL)
    return -1;

  if(removeThis == sep && has_sep)
    {
      has_sep = false;
      sep = NULL;
      return 0;
    }

  if(removeThis == term && has_term)
    {
      has_term = false;
      term = NULL;
      return 0;
    }

  if(removeThis == last && has_last)
    {
      has_last = false;
      last = NULL;
      return 0;
    }

  if(removeThis == omit && has_omit)
    {
      has_omit = false;
      omit = NULL;
      return 0;
    }

  if(removeThis == Plongest && has_longest)
    {
      has_longest = false;
      Plongest = NULL;
      return 0;
    }

  return -1;
}

// new_methods_def_point


// **********

ParrayS::ParrayS() : PNodeP(PT_PARRAY)
{
  //modifiers = NULL;
  modifiers = new modifiersS();
  decl = NULL;
  ptype = NULL;

  has_delimiterConstraints = false;
  delimiterConstraints = NULL;

  has_arrayConstraints = false;
  arrayConstraints = NULL;
}

PNodeP* ParrayS::addChild(PNodeP* newChild)
{
  switch(newChild->nodeType)
    {
    case PT_PSOURCE:
    case PT_PRECORD:
    case PT_PLONGEST:
      modifiers->addChild(newChild);
      break;
    case PT_DECL:
      decl = (declT)newChild;
      break;
    case PT_PTYPE:
      ptype = (ptypeT)newChild;
      break;
    case PT_DELIMITERCONSTRAINTS:
      has_delimiterConstraints = true;
      delimiterConstraints = (delimiterConstraintsT)newChild;
      break;
    case PT_ARRAYCONSTRAINTS:
      has_arrayConstraints = true;
      arrayConstraints = (arrayConstraintsT)newChild;
      break;
    default:
      {
	throw PTException(PTE_CHILD_TYPE_ERR, nodeType, __LINE__);
	//throw except;
	return NULL;
      }
    }
  newChild->parent = (PNodeP*)this;
  return this;
}

void ParrayS::serializeXMLContents(int spaceDepth, PSTRING& printStr)
{
  DB_P("\n");
  printStr.append("\n");
  int i;
  int numSpaces = (spaceDepth + 1) * XML_ADD_LEVEL_SPACE;
  for(i = 0; i < numSpaces; i++)
    printStr.append(" ");
    DB_P(" "); 
  modifiers->serializeXMLRepresentation(spaceDepth + 1, printStr);
  decl->serializeXMLRepresentation(spaceDepth + 1, printStr);
  ptype->serializeXMLRepresentation(spaceDepth + 1, printStr);
  if(has_delimiterConstraints)
    delimiterConstraints->serializeXMLRepresentation(spaceDepth + 1, printStr);
  if(has_arrayConstraints)
    arrayConstraints->serializeXMLRepresentation(spaceDepth + 1, printStr);
  //  printf("\n");
  //for(i = 0; i < numSpaces - XML_ADD_LEVEL_SPACE; i++)
  //  printf(" ");
}

void ParrayS::codifyContents(int spaceDepth, PSTRING& printStr)
{
  modifiers->codifyRepresentation(spaceDepth, printStr);
  DB_P("Parray ");
  printStr.append(PT_TypeNames[nodeType]);
  printStr.append(" ");
  decl->codifyRepresentation(spaceDepth, printStr);
  DB_P(" {\n");
  printStr.append(" {\n");

  DB_P(" ");
  printStr.append(spaceDepth * CODE_ADD_LEVEL_SPACE, ' ');

  printStr.append(" ");
  ptype->codifyRepresentation(spaceDepth, printStr);

  if(has_arrayConstraints)
    {
      if(arrayConstraints->has_size)
	{
	  DB_P("[");
	  printStr.append("[");
	  arrayConstraints->size->codifyRepresentation(spaceDepth, printStr);
	  DB_P("]");
	  printStr.append("]");
	}
      else
	{
	  DB_P("[]");
	  printStr.append("[]");
	}
    }
  else
    {
      DB_P("[]");
      printStr.append("[]");
    }


  if(has_delimiterConstraints)
    {
      DB_P(" : ");
      printStr.append(" : ");
      delimiterConstraints->codifyRepresentation(spaceDepth + 1, printStr);
    }
  else
    {
      DB_P(";\n");
      printStr.append(";\n");
    }
  DB_P("   }");
  //  printStr.append("\n");
  printStr.append(spaceDepth * CODE_ADD_LEVEL_SPACE, ' ');
  printStr.append("}");
  if(has_arrayConstraints && arrayConstraints->forAll_Pparsecheck.size() > 0)
    {
      DB_P(" Pwhere {\n");
      printStr.append(" Pwhere {\n");
      arrayConstraints->codifyRepresentation(spaceDepth + 1, printStr);
      //DB_P(";\n");
      //printStr.append(";\n");
      DB_P("   }");
      printStr.append(spaceDepth * CODE_ADD_LEVEL_SPACE, ' ');
      printStr.append("}");
    }
  else
    {
      //DB_P(";\n");
      //printStr.append(";\n");
    }
  //DB_P("   }");
  //  printStr.append("\n");
  //printStr.append(" ", spaceDepth * CODE_ADD_LEVEL_SPACE);
  //printStr.append("}");
}

bool ParrayS::canChangeToType(int newType)
{
  switch(newType)
    {
    default:
      break;
    }
  return false;
}

PNodeP* ParrayS::convertAndCopy(int newType, PNodeP* copyTo)
{
  return NULL;
}

int ParrayS::getTextualRepresentation(PSTRING& nameStr)
{
  nameStr.append(PT_TypeNames[nodeType]);
  nameStr.append(" ");
  decl->getTextualRepresentation(nameStr);
  return 0;
}

void ParrayS::getNameStr(PSTRING& nameStr)
{
  decl->getNameStr(nameStr);
}

PADSPNodeTreeNode* ParrayS::makeBasicTree(PADSPNodeTreeNode* parent)
{
  DB_P("making composite node::%s\n", PT_TypeNames[nodeType]);
  PADSPNodeTreeNode* thisNode = new PADSPNodeTreeNode(this, parent);

  PADSPNodeTreeNode *newChild = NULL, *currentChild = NULL;

  newChild = modifiers->makeBasicTree(thisNode);
  newChild->setElementIsRequired(true);
  if(newChild != NULL)
    {
      if(currentChild == NULL)
	{
	  thisNode->firstChild = newChild;
	}
      else
	{
	  currentChild->nextSibling = newChild;
	  newChild->previousSibling = currentChild;
	}
      currentChild = newChild;
    }

  newChild = decl->makeBasicTree(thisNode);
  newChild->setUpdateParentOnChange(true);
  newChild->setElementIsRequired(true);
  if(newChild != NULL)
    {
      if(currentChild == NULL)
	{
	  thisNode->firstChild = newChild;
	}
      else
	{
	  currentChild->nextSibling = newChild;
	  newChild->previousSibling = currentChild;
	}
      currentChild = newChild;
    }

  newChild = ptype->makeBasicTree(thisNode);
  newChild->setElementIsRequired(true);
  if(newChild != NULL)
    {
      if(currentChild == NULL)
	{
	  thisNode->firstChild = newChild;
	}
      else
	{
	  currentChild->nextSibling = newChild;
	  newChild->previousSibling = currentChild;
	}
      currentChild = newChild;
    }
  
  if(has_delimiterConstraints)
    {
      newChild = delimiterConstraints->makeBasicTree(thisNode);

      if(newChild != NULL)
	{
	  if(currentChild == NULL)
	    {
	      thisNode->firstChild = newChild;
	    }
	  else
	    {
	      currentChild->nextSibling = newChild;
	      newChild->previousSibling = currentChild;
	    }
	  currentChild = newChild;
	}
    }
  if(has_arrayConstraints)
    {
      newChild = arrayConstraints->makeBasicTree(thisNode);

      if(newChild != NULL)
	{
	  if(currentChild == NULL)
	    {
	      thisNode->firstChild = newChild;
	    }
	  else
	    {
	      currentChild->nextSibling = newChild;
	      newChild->previousSibling = currentChild;
	    }
	  currentChild = newChild;
	}
    }
  return thisNode;
}

int ParrayS::canAddChildTypes(int* typeList, int max)
{
  if(max < 6)
    return -1;
  int i = 0;
  if(!has_delimiterConstraints)
    {
      typeList[i] = PT_DELIMITERCONSTRAINTS;
      i++;
    }
  if(!has_arrayConstraints)
    {
      typeList[i] = PT_ARRAYCONSTRAINTS;
      i++;
    }
  return i;
}

int ParrayS::makeCompleteNode()
{
  decl = new declS();
  ptype = new ptypeS();
  if(modifiers->makeCompleteNode() != 0)
    return -1;
  if(decl->makeCompleteNode() != 0)
    return -1;
  if(ptype->makeCompleteNode() != 0)
    return -1;
  return 0;
}

int ParrayS::removeChildNode(PNodeP* removeThis)
{
  if(removeThis == delimiterConstraints && has_delimiterConstraints)
    {
      has_delimiterConstraints = false;
      delimiterConstraints = NULL;
      return 0;
    }

  if(removeThis == arrayConstraints && has_arrayConstraints)
    {
      has_arrayConstraints = false;
      arrayConstraints = NULL;
      return 0;
    }

  return -1;
}

PADSUniTreeNode* ParrayS::makeUnifiedTree(PADSUniTreeNode* parent, PSTRING& varStr, PADSTypeMap* typeMap)
{
  DB_P("making composite node::%s\n", PT_TypeNames[nodeType]);
  PADSUniTreeNode* thisNode = new PADSUniTreeNode(this, varStr, parent);

  PADSUniTreeNode *newChild = NULL, *currentChild = NULL;

  PSTRING childTypeStr = "";
  PSTRING childNameStr = "";
  ptype->getNameStr(childTypeStr);
  DB_P("  Parray got childTypeStr: '%s'\n", childTypeStr.c_str());
  DB_P("     typeMap->find returned %d\n", typeMap->find(childTypeStr));
  PNodeP* childNode = typeMap->find(childTypeStr);
  if(childNode != NULL)
    {
      DB_P("## calling makeUnifiedTree on child node\n");
      newChild = childNode->makeUnifiedTree(thisNode, childTypeStr, typeMap);
      if(newChild != NULL)
	{
	  if(currentChild == NULL)
	    {
	      thisNode->firstChild = newChild;
	    }
	  else
	    {
	      currentChild->nextSibling = newChild;
	      newChild->previousSibling = currentChild;
	    }
	  currentChild = newChild;

	  if(newChild->getLinkNode()->nodeType = PT_PTYPEDEF)
	    {
	      newChild->setLinkNode(this);
	    }
	}
    }
  return thisNode;
}

// new_methods_def_point


// **********

PsomeS::PsomeS() : PNodeP(PT_PSOME)
{
  var = NULL;
  constraints = NULL;
}

PNodeP* PsomeS::addChild(PNodeP* newChild)
{
  switch(newChild->nodeType)
    {
    case PT_VAR:
      var = (varT)newChild;
      break;
    case PT_CONSTRAINTS:
    case PT_CONSTRAINTS2:
      constraints = (constraintsT)newChild;
      break;
    default:
      {
	throw PTException(PTE_CHILD_TYPE_ERR, nodeType, __LINE__);
	//throw except;
	return NULL;
      }
    }
  newChild->parent = (PNodeP*)this;
  return this;
}

void PsomeS::serializeXMLContents(int spaceDepth, PSTRING& printStr)
{
  var->serializeXMLRepresentation(spaceDepth + 1, printStr);
  constraints->serializeXMLRepresentation(spaceDepth + 1, printStr);
}

void PsomeS::codifyContents(int spaceDepth, PSTRING& printStr)
{
  DB_P("Psome ");
  printStr.append("Psome ");
  var->codifyRepresentation(0, printStr);
  DB_P(" => {");
  printStr.append(" => {");
  constraints->codifyRepresentation(0, printStr);
  DB_P("}");
  printStr.append("}");
}

bool PsomeS::canChangeToType(int newType)
{
  switch(newType)
    {
    default:
      break;
    }
  return false;
}

PNodeP* PsomeS::convertAndCopy(int newType, PNodeP* copyTo)
{
  return NULL;
}

PADSPNodeTreeNode* PsomeS::makeBasicTree(PADSPNodeTreeNode* parent)
{
  DB_P("making composite node::%s\n", PT_TypeNames[nodeType]);
  PADSPNodeTreeNode* thisNode = new PADSPNodeTreeNode(this, parent);

  PADSPNodeTreeNode *newChild = NULL, *currentChild = NULL;

  newChild = var->makeBasicTree(thisNode);
  newChild->setElementIsRequired(true);
  if(newChild != NULL)
    {
      if(currentChild == NULL)
	{
	  thisNode->firstChild = newChild;
	}
      else
	{
	  currentChild->nextSibling = newChild;
	  newChild->previousSibling = currentChild;
	}
      currentChild = newChild;
    }
  
  newChild = constraints->makeBasicTree(thisNode);
  newChild->setElementIsRequired(true);
  if(newChild != NULL)
    {
      if(currentChild == NULL)
	{
	  thisNode->firstChild = newChild;
	}
      else
	{
	  currentChild->nextSibling = newChild;
	  newChild->previousSibling = currentChild;
	}
      currentChild = newChild;
    }
  return thisNode;
}

int PsomeS::makeCompleteNode()
{
  var = new varS();
  constraints = new constraintsS();
  constraints->nodeType = PT_CONSTRAINTS2;
  if(var->makeCompleteNode() != 0)
    return -1;
  if(constraints->makeCompleteNode() != 0)
    return -1;
  return 0;
}
// new_methods_def_point


// **********

PnoneS::PnoneS() : PNodeP(PT_PNONE)
{
  constraints = NULL;
}

PNodeP* PnoneS::addChild(PNodeP* newChild)
{
  switch(newChild->nodeType)
    {
    case PT_CONSTRAINTS:
    case PT_CONSTRAINTS2:
      constraints = (constraintsT)newChild;
      break;
    default:
      {
	throw PTException(PTE_CHILD_TYPE_ERR, nodeType, __LINE__);
	//throw except;
	return NULL;
      }
    }
  newChild->parent = (PNodeP*)this;
  return this;
}

void PnoneS::serializeXMLContents(int spaceDepth, PSTRING& printStr)
{
  constraints->serializeXMLRepresentation(spaceDepth + 1, printStr);
}

void PnoneS::codifyContents(int spaceDepth, PSTRING& printStr)
{
  DB_P("Pnone => {");
  printStr.append("Pnone  => {");
  constraints->codifyRepresentation(0, printStr);
  DB_P("}");
  printStr.append("}");
}

bool PnoneS::canChangeToType(int newType)
{
  switch(newType)
    {
    default:
      break;
    }
  return false;
}

PNodeP* PnoneS::convertAndCopy(int newType, PNodeP* copyTo)
{
  return NULL;
}

PADSPNodeTreeNode* PnoneS::makeBasicTree(PADSPNodeTreeNode* parent)
{
  DB_P("making composite node::%s\n", PT_TypeNames[nodeType]);
  PADSPNodeTreeNode* thisNode = new PADSPNodeTreeNode(this, parent);

  PADSPNodeTreeNode *newChild = NULL, *currentChild = NULL;
  
  newChild = constraints->makeBasicTree(thisNode);
  newChild->setElementIsRequired(true);
  if(newChild != NULL)
    {
      if(currentChild == NULL)
	{
	  thisNode->firstChild = newChild;
	}
      else
	{
	  currentChild->nextSibling = newChild;
	  newChild->previousSibling = currentChild;
	}
      currentChild = newChild;
    }

  return thisNode;
}

int PnoneS::makeCompleteNode()
{
  constraints = new constraintsS();
  constraints->nodeType = PT_CONSTRAINTS2;
  return constraints->makeCompleteNode();
}
// new_methods_def_point


// **********

PoptS::PoptS() : PNodeP(PT_POPT)
{
  which = UNDEFINED;
  constraints = NULL;

  has_Psome = false;
  Psome = NULL;

  has_Pnone = false;
  Pnone = NULL;
}

PNodeP* PoptS::addChild(PNodeP* newChild)
{
  switch(newChild->nodeType)
    {
    case PT_CONSTRAINTS:
    case PT_CONSTRAINTS2:
      which = CONSTRAINTS;
      constraints = (constraintsT)newChild;
      break;
    case PT_PSOME:
      which = SOMENONE;
      has_Psome = true;
      Psome = (PsomeT)newChild;
      break;
    case PT_PNONE:
      which = SOMENONE;
      has_Pnone = true;
      Pnone = (PnoneT)newChild;
      break;
    default:
      {
	throw PTException(PTE_CHILD_TYPE_ERR, nodeType, __LINE__);
	//throw except;
	return NULL;
      }
    }
  newChild->parent = (PNodeP*)this;
  return this;
}

void PoptS::serializeXMLContents(int spaceDepth, PSTRING& printStr)
{
  if(which == CONSTRAINTS)
    constraints->serializeXMLRepresentation(spaceDepth + 1, printStr);
  else if(which == SOMENONE)
    {
      if(has_Psome)
	Psome->serializeXMLRepresentation(spaceDepth + 1, printStr);
      if(has_Pnone)
	Pnone->serializeXMLRepresentation(spaceDepth + 1, printStr);
    }
  else
    {
      throw PTException(PTE_CHILD_UNDEF_ERR, nodeType, __LINE__);
      //throw except;
    }
}

void PoptS::serializeXMLRepresentation(int spaceDepth, PSTRING& printStr)
{
  if(which == UNDEFINED)
    {
      serializeXMLEmpty(spaceDepth, printStr);
    }
  else
    {
      serializeXMLOpen(spaceDepth, printStr);
      serializeXMLContents(spaceDepth, printStr);
      serializeXMLClose(spaceDepth, printStr);
    }
}

void PoptS::codifyRepresentation(int spaceDepth, PSTRING& printStr)
{
  if(which == CONSTRAINTS)
    {
      DB_P(" : ");
      printStr.append(" : ");
      constraints->codifyRepresentation(0, printStr);
    }
  else if(which == SOMENONE)
    {
      if(has_Psome || has_Pnone)
	{
	  DB_P(" : ");
	  printStr.append(" : ");
	}
      if(has_Psome)
	Psome->serializeXMLRepresentation(spaceDepth + 1, printStr);
      if(has_Pnone)
	Pnone->serializeXMLRepresentation(spaceDepth + 1, printStr);
    }
  else
    {

    }
}

bool PoptS::canChangeToType(int newType)
{
  switch(newType)
    {
    default:
      break;
    }
  return false;
}

PNodeP* PoptS::convertAndCopy(int newType, PNodeP* copyTo)
{
  return NULL;
}

int PoptS::getTextualRepresentation(PSTRING& nameStr)
{
  nameStr.append(PT_TypeNames[nodeType]);
  return 0;
}

PADSPNodeTreeNode* PoptS::makeBasicTree(PADSPNodeTreeNode* parent)
{
  DB_P("making composite node::%s\n", PT_TypeNames[nodeType]);
  PADSPNodeTreeNode* thisNode = new PADSPNodeTreeNode(this, parent);

  PADSPNodeTreeNode *newChild = NULL, *currentChild = NULL;
  
  if(which == CONSTRAINTS)
    {
      newChild = constraints->makeBasicTree(thisNode);
      newChild->setElementIsRequired(true);
      if(newChild != NULL)
	{
	  if(currentChild == NULL)
	    {
	      thisNode->firstChild = newChild;
	    }
	  else
	    {
	      currentChild->nextSibling = newChild;
	      newChild->previousSibling = currentChild;
	    }
	  currentChild = newChild;
	}
    }
  else if(which == SOMENONE)
    {
      if(has_Psome)
	{
	  newChild = Psome->makeBasicTree(thisNode);
	  if(newChild != NULL)
	    {
	      if(currentChild == NULL)
		{
		  thisNode->firstChild = newChild;
		}
	      else
		{
		  currentChild->nextSibling = newChild;
		  newChild->previousSibling = currentChild;
		}
	      currentChild = newChild;
	    }
	  newChild = NULL;
	}
      if(has_Pnone)
	{
	  newChild = Pnone->makeBasicTree(thisNode);
	  if(newChild != NULL)
	    {
	      if(currentChild == NULL)
		{
		  thisNode->firstChild = newChild;
		}
	      else
		{
		  currentChild->nextSibling = newChild;
		  newChild->previousSibling = currentChild;
		}
	      currentChild = newChild;
	    }
	  newChild = NULL;
	}
    }
  return thisNode;
}

int PoptS::canAddChildTypes(int* typeList, int max)
{
  if(max < 3)
    return -1;
  int i = 0;
  if(which != SOMENONE)
    {
      if(!has_Psome)
	{
	  typeList[i] = PT_PSOME;
	  i++;
	}
      if(!has_Pnone)
	{
	  typeList[i] = PT_PNONE;
	  i++;
	}
      typeList[i] = PT_CONSTRAINTS;
    }
  else 
    {
      if(!has_Psome)
	{
	  typeList[i] = PT_PSOME;
	  i++;
	}
      if(!has_Pnone)
	{
	  typeList[i] = PT_PNONE;
	  i++;
	}
    }
  return i;
}

int PoptS::makeCompleteNode()
{
  constraints = new constraintsS();
  constraints->nodeType = PT_CONSTRAINTS2;
  return constraints->makeCompleteNode() != 0;
}

int PoptS::removeChildNode(PNodeP* removeThis)
{
  if(which == SOMENONE)
    {
      if(removeThis == Psome && has_Psome)
	{
	  has_Psome = false;
	  Psome = NULL;
	  return 0;
	}

      if(removeThis == Pnone && has_Pnone)
	{
	  has_Pnone = false;
	  Pnone = NULL;
	  return 0;
	}
    }

  return -1;
}

// new_methods_def_point


// **********

// This is a mistake - structModifiers is only a grouping, and thus shouldn't get its own type
structModifiersS::structModifiersS() : PNodeP(PT_STRUCTMODIFIERS)
{
  has_Pomit = false;
  Pomit = NULL;

  has_Pendian = false;
  Pendian = NULL;

  has_Popt = false;
  Popt = NULL;

  has_Pcompute = false;
  Pcompute = NULL;
}

PNodeP* structModifiersS::addChild(PNodeP* newChild)
{
  switch(newChild->nodeType)
    {
    case PT_POMIT:
      has_Pomit = true;
      Pomit = (PomitT)newChild;
      break;
    case PT_PENDIAN:
      has_Pendian = true;
      Pendian = (PendianT)newChild;
      break;
    case PT_POPT:
      has_Popt = true;
      Popt = (PoptT)newChild;
      break;
    case PT_PCOMPUTE:
      has_Pcompute = true;
      Pcompute = (PcomputeT)newChild;
      break;
    default:
      {
	throw PTException(PTE_CHILD_TYPE_ERR, nodeType, __LINE__);
	//throw except;
	return NULL;
      }
    }
  newChild->parent = (PNodeP*)this;
  return this;
}

void structModifiersS::serializeXMLContents(int spaceDepth, PSTRING& printStr)
{
  printf("ERROR - DEPRICATED TYPE: structModifiers\n");
}

void structModifiersS::codifyContents(int spaceDepth, PSTRING& printStr)
{
  if(has_Pomit)
    {
      Pomit->codifyContents(0, printStr);
      DB_P(" ");
      printStr.append(" ");
    }
  if(has_Pendian)
    {
      Pomit->codifyContents(0, printStr);
      DB_P(" ");
      printStr.append(" ");
    }
  if(has_Popt)
    {
      printStr.append(PT_TypeNames[PT_POPT]);
      DB_P(" ");
      printStr.append(" ");
    }
  if(has_Pcompute)
    {
      printStr.append(PT_TypeNames[PT_PCOMPUTE]);
      DB_P(" ");
      printStr.append(" ");
    }
}

bool structModifiersS::canChangeToType(int newType)
{
  switch(newType)
    {
    default:
      break;
    }
  return false;
}

PNodeP* structModifiersS::convertAndCopy(int newType, PNodeP* copyTo)
{
  return NULL;
}

// new_methods_def_point


// **********

fieldS::fieldS() : PNodeP(PT_FIELD)
{
  has_Pomit = false;
  Pomit = NULL;

  has_Pendian = false;
  Pendian = NULL;

  has_Popt = false;
  Popt = NULL;

  has_Pcompute = false;
  Pcompute = NULL;

  ptype = NULL;
  name = NULL;

  has_from = false;
  from = NULL;

  has_arrayConstraints = false;
  arrayConstraints = NULL;

  has_constraints = false;
  constraints = NULL; 
}

PNodeP* fieldS::addChild(PNodeP* newChild)
{
  switch(newChild->nodeType)
    {
    case PT_POMIT:
      has_Pomit = true;
      Pomit = (PomitT)newChild;
      break;
    case PT_PENDIAN:
      has_Pendian = true;
      Pendian = (PendianT)newChild;
      break;
    case PT_POPT:
      has_Popt = true;
      Popt = (PoptT)newChild;
      break;
    case PT_PCOMPUTE:
      has_Pcompute = true;
      Pcompute = (PcomputeT)newChild;
      break;
    case PT_PTYPE:
      ptype = (ptypeT)newChild;
      break;
    case PT_NAME:
      name = (nameT)newChild;
      break;
    case PT_FROM:
      has_from = true;
      from = (fromT)newChild;
      break;
    case PT_ARRAYCONSTRAINTS:
      has_arrayConstraints = true;
      arrayConstraints = (arrayConstraintsT)newChild;      
      break;
    case PT_CONSTRAINTS:
    case PT_CONSTRAINTS2:
      has_constraints = true;
      constraints = (constraintsT)newChild;
      break;
    default:
      {
	throw PTException(PTE_CHILD_TYPE_ERR, nodeType, __LINE__);
	//throw except;
	return NULL;
      }
    }
  newChild->parent = (PNodeP*)this;
  return this;
}

void fieldS::serializeXMLContents(int spaceDepth, PSTRING& printStr)
{
  if(has_Pomit)
    Pomit->serializeXMLRepresentation(spaceDepth + 1, printStr);
  if(has_Pendian)
    Pendian->serializeXMLRepresentation(spaceDepth + 1, printStr);
  if(has_Popt)
    Popt->serializeXMLRepresentation(spaceDepth + 1, printStr);
  if(has_Pcompute)
    Pcompute->serializeXMLRepresentation(spaceDepth + 1, printStr);

  ptype->serializeXMLRepresentation(spaceDepth + 1, printStr);
  name->serializeXMLRepresentation(spaceDepth + 1, printStr);

  if(has_from)
    from->serializeXMLRepresentation(spaceDepth + 1, printStr);
  if(has_arrayConstraints)
    arrayConstraints->serializeXMLRepresentation(spaceDepth + 1, printStr);
  if(has_constraints)
    constraints->serializeXMLRepresentation(spaceDepth + 1, printStr);
}

void fieldS::codifyContents(int spaceDepth, PSTRING& printStr)
{
  //printStr.append("here2!\n");
  //DB_P("@@@fieldS codify call!@@@\n");
  //DB_P("###%s###\n", printStr.c_str());
  if(has_Pomit) // Pomit and Pendian are unit types, so they can be called as is
    Pomit->codifyRepresentation(spaceDepth, printStr);
  if(has_Pendian)
    Pomit->codifyRepresentation(spaceDepth, printStr);
  if(has_Popt) // Popt, however, is a bit more tricky - it requires the modifier before and the constraint after the type/name
    {
      DB_P("%s ", PT_TypeNames[PT_POPT]);
      printStr.append(PT_TypeNames[PT_POPT]);
      printStr.append(" ");
    }
  if(has_Pcompute)
    {
      DB_P("%s ", PT_TypeNames[PT_PCOMPUTE]);
      printStr.append(PT_TypeNames[PT_PCOMPUTE]);
      printStr.append(" ");
    }
  ptype->codifyRepresentation(spaceDepth + 1, printStr);
  DB_P(" ");
  printStr.append(" ");
  name->codifyRepresentation(spaceDepth + 1, printStr);
  if(has_arrayConstraints)
    {  // this isn't pretty, but it'll have to do for now
      if(arrayConstraints->has_size) // size constraints appear in the grammar before the array constraints
	{                            // but exist as child elements in the XSchema
	  DB_P(" [");
	  printStr.append(" [");
	  arrayConstraints->size->codifyRepresentation(0, printStr);
	  DB_P("]");
	  printStr.append("]");
	}
      else
	{
	  DB_P(" []");
	  printStr.append(" []");
	}
    }

  if(has_from)
    {
      DB_P(" Pfrom (");
      printStr.append(" Pfrom (");
      from->codifyRepresentation(spaceDepth, printStr);
      DB_P(")");
      printStr.append(")");
    }

      if(has_Popt)
	{
	  if(Popt->which != Popt->UNDEFINED)
	    {
	      DB_P(" : ");
	      printStr.append(" : ");
	    }
	  Popt->codifyRepresentation(0, printStr);
	}
      if(has_Pcompute)
	{
	  DB_P(" : ");
	  printStr.append(" : ");
	  Pcompute->codifyRepresentation(0, printStr);
	}

  if(has_arrayConstraints || has_constraints)
    {
      DB_P(" : ");
      printStr.append(" : ");
      if(has_arrayConstraints)
	arrayConstraints->codifyRepresentation(0, printStr);
      if(has_constraints)
	constraints->codifyRepresentation(0, printStr);  
    }
}

bool fieldS::canChangeToType(int newType)
{
  switch(newType)
    {
    default:
      break;
    }
  return false;
}

PNodeP* fieldS::convertAndCopy(int newType, PNodeP* copyTo)
{
  return NULL;
}

int fieldS::getTextualRepresentation(PSTRING& nameStr)
{
  if(has_Popt)
    {
      Popt->getTextualRepresentation(nameStr);
      nameStr.append(" ");
    }
  //  ptype->getTextualRepresentation(nameStr);
  nameStr.append(" ");
  name->getTextualRepresentation(nameStr);
  return 0;
}

void fieldS::getTypeStr(PSTRING& typeStr)
{
  ptype->getNameStr(typeStr);
}

void fieldS::getNameStr(PSTRING& nameStr)
{
  name->getNameStr(nameStr);
}

PADSPNodeTreeNode* fieldS::makeBasicTree(PADSPNodeTreeNode* parent)
{
  DB_P("making composite node::%s\n", PT_TypeNames[nodeType]);
  PADSPNodeTreeNode* thisNode = new PADSPNodeTreeNode(this, parent);

  PADSPNodeTreeNode *newChild = NULL, *currentChild = NULL;

  PSTRING tmpStr;
  tmpStr = PT_TypeNames[nodeType];
  tmpStr.append(": ");
  DB_P("getting textual representation for node of type %s\n", PT_TypeNames[nodeType]);
  getTextualRepresentation(tmpStr);
  DB_P("setting node name to %s\n", tmpStr.c_str());
  thisNode->setNodeName(tmpStr);

  newChild = ptype->makeBasicTree(thisNode);
  newChild->setUpdateParentOnChange(true);
  newChild->setElementIsRequired(true);
  if(newChild != NULL)
    {
      if(currentChild == NULL)
	{
	  thisNode->firstChild = newChild;
	}
      else
	{
	  currentChild->nextSibling = newChild;
	  newChild->previousSibling = currentChild;
	}
      currentChild = newChild;
    }
  newChild = name->makeBasicTree(thisNode);
  newChild->setUpdateParentOnChange(true);
  newChild->setElementIsRequired(true);
  if(newChild != NULL)
    {
      if(currentChild == NULL)
	{
	  thisNode->firstChild = newChild;
	}
      else
	{
	  currentChild->nextSibling = newChild;
	  newChild->previousSibling = currentChild;
	}
      currentChild = newChild;
    }  

  if(has_Pomit)
    {
      newChild = Pomit->makeBasicTree(thisNode);
      if(newChild != NULL)
	{
	  if(currentChild == NULL)
	    {
	      thisNode->firstChild = newChild;
	    }
	  else
	    {
	      currentChild->nextSibling = newChild;
	      newChild->previousSibling = currentChild;
	    }
	  currentChild = newChild;
	}
    }  

  if(has_Pendian)
    {
      newChild = Pendian->makeBasicTree(thisNode);
      if(newChild != NULL)
	{
	  if(currentChild == NULL)
	    {
	      thisNode->firstChild = newChild;
	    }
	  else
	    {
	      currentChild->nextSibling = newChild;
	      newChild->previousSibling = currentChild;
	    }
	  currentChild = newChild;
	}
    }  

  if(has_Popt)
    {
      newChild = Popt->makeBasicTree(thisNode);
      if(newChild != NULL)
	{
	  if(currentChild == NULL)
	    {
	      thisNode->firstChild = newChild;
	    }
	  else
	    {
	      currentChild->nextSibling = newChild;
	      newChild->previousSibling = currentChild;
	    }
	  currentChild = newChild;
	}
    }  

  if(has_Pcompute)
    {
      newChild = Pcompute->makeBasicTree(thisNode);
      if(newChild != NULL)
	{
	  if(currentChild == NULL)
	    {
	      thisNode->firstChild = newChild;
	    }
	  else
	    {
	      currentChild->nextSibling = newChild;
	      newChild->previousSibling = currentChild;
	    }
	  currentChild = newChild;
	}
    }  

  if(has_from)
    {
      newChild = from->makeBasicTree(thisNode);
      if(newChild != NULL)
	{
	  if(currentChild == NULL)
	    {
	      thisNode->firstChild = newChild;
	    }
	  else
	    {
	      currentChild->nextSibling = newChild;
	      newChild->previousSibling = currentChild;
	    }
	  currentChild = newChild;
	}
    }  


  if(has_arrayConstraints)
    {
      newChild = arrayConstraints->makeBasicTree(thisNode);

      if(newChild != NULL)
	{
	  if(currentChild == NULL)
	    {
	      thisNode->firstChild = newChild;
	    }
	  else
	    {
	      currentChild->nextSibling = newChild;
	      newChild->previousSibling = currentChild;
	    }
	  currentChild = newChild;
	}
    }
  if(has_constraints)
    {
      newChild = constraints->makeBasicTree(thisNode);

      if(newChild != NULL)
	{
	  if(currentChild == NULL)
	    {
	      thisNode->firstChild = newChild;
	    }
	  else
	    {
	      currentChild->nextSibling = newChild;
	      newChild->previousSibling = currentChild;
	    }
	  currentChild = newChild;
	}
    }
  return thisNode;
}

int fieldS::canAddChildTypes(int* typeList, int max)
{
  if(max < 7)
    return -1;
  int i = 0;
  if(!has_Pomit)
    {
      typeList[i] = PT_POMIT;
      i++;
    }
  if(!has_Pendian)
    {
      typeList[i] = PT_PENDIAN;
      i++;
    }
  if(!has_Popt)
    {
      typeList[i] = PT_POPT;
      i++;
    }
  if(!has_Pcompute)
    {
      typeList[i] = PT_PCOMPUTE;
      i++;
    }

  if(!has_from)
    {
      typeList[i] = PT_FROM;
      i++;
    }
  if(!has_arrayConstraints)
    {
      typeList[i] = PT_ARRAYCONSTRAINTS;
      i++;
    }
  if(!has_constraints)
    {
      typeList[i] = PT_CONSTRAINTS2;
      i++;
    }
  return i;
}

int fieldS::removeChildNode(PNodeP* removeThis)
{
  if(removeThis == Pomit && has_Pomit)
    {
      has_Pomit = false;
      Pomit = NULL;
      return 0;
    }

  if(removeThis == Pendian && has_Pendian)
    {
      has_Pendian = false;
      Pendian = NULL;
      return 0;
    }

  if(removeThis == Popt && has_Popt)
    {
      has_Popt = false;
      Popt = NULL;
      return 0;
    }

  if(removeThis == Pcompute && has_Pcompute)
    {
      has_Pcompute = false;
      Pcompute = NULL;
      return 0;
    }

  if(removeThis == from && has_from)
    {
      has_from = false;
      from = NULL;
      return 0;
    }

  if(removeThis == arrayConstraints && has_arrayConstraints)
    {
      has_arrayConstraints = false;
      arrayConstraints = NULL;
      return 0;
    }

  if(removeThis == constraints && has_constraints)
    {
      has_constraints = false;
      constraints = NULL;
      return 0;
    }

  return -1;
}

int fieldS::makeCompleteNode()
{
  ptype = new ptypeS();
  name = new nameS();

  if(ptype->makeCompleteNode() != 0)
    return -1;
  if(name->makeCompleteNode() != 0)
    return -1;
  return 0;
}

PADSUniTreeNode* fieldS::makeUnifiedTree(PADSUniTreeNode* parent, PSTRING& varStr, PADSTypeMap* typeMap)
{
  DB_P("making base node::%s, name = %s \n", PT_TypeNames[nodeType], varStr.c_str());
  PADSUniTreeNode* thisNode = new PADSUniTreeNode(this, varStr, parent);
  PSTRING typeName = "";
  getTypeStr(typeName);
  thisNode->setTypeName(typeName);
  return thisNode;
}

// new_methods_def_point


// **********

PalternatesS::PalternatesS() : PNodeP(PT_PALTERNATES)
{
  modifiers = new modifiersS();;
  decl = NULL;

  has_constraints = false;
  constraints = NULL;
}

PNodeP* PalternatesS::addChild(PNodeP* newChild)
{
  switch(newChild->nodeType)
    {
    case PT_PSOURCE:
    case PT_PRECORD:
    case PT_PLONGEST:
      modifiers->addChild(newChild);
      break;
    case PT_DECL:
      decl = (declT)newChild;
      break;
    case PT_FIELD:
    case PT_LITERAL:
      {
	field_literalT fl = new field_literalS();
	fl->which = newChild->nodeType == PT_FIELD ? fl->FIELD : fl->LITERAL;
	if(fl->which == fl->FIELD)
	  fl->field_literal.field = (fieldT)newChild;
	else
	  fl->field_literal.literal = (literalT)newChild;
	field_literal.push_back(fl);
      }
      break;
    default:
      {
	throw PTException(PTE_CHILD_TYPE_ERR, nodeType, __LINE__);
	//throw except;
	return NULL;
      }
    }
  newChild->parent = (PNodeP*)this;
  return this;
}

void PalternatesS::serializeXMLContents(int spaceDepth, PSTRING& printStr)
{
  modifiers->serializeXMLRepresentation(spaceDepth + 1, printStr);
  decl->serializeXMLRepresentation(spaceDepth + 1, printStr);

  int size = field_literal.size();
  field_literalT this_field_literal;
  for(int i = 0; i < size; i++)
    {
      this_field_literal = field_literal.at(i);
      if(this_field_literal->which == this_field_literal->FIELD)
	this_field_literal->field_literal.field->serializeXMLRepresentation(spaceDepth + 1, printStr);
      else if(this_field_literal->which == this_field_literal->LITERAL)
	this_field_literal->field_literal.literal->serializeXMLRepresentation(spaceDepth + 1, printStr);
      else
	{
	  throw PTException(PTE_CHILD_UNDEF_ERR, nodeType, __LINE__);
	  //throw except;
	}
    }
  if(has_constraints)
    constraints->serializeXMLRepresentation(spaceDepth + 1, printStr);
}

void PalternatesS::codifyContents(int spaceDepth, PSTRING& printStr)
{
  // ambiguous - not in the manual!
  modifiers->codifyRepresentation(0, printStr);
  DB_P("Palternates ");
  printStr.append(PT_TypeNames[nodeType]);
  printStr.append(" ");
  decl->codifyRepresentation(0, printStr);
  DB_P(" {\n");
  printStr.append(" {\n");

  int size = field_literal.size();
  field_literalT this_field_literal;
  for(int i = 0; i < size; i++)
    {
      this_field_literal = field_literal.at(i);
      if(this_field_literal->which == this_field_literal->FIELD)
	this_field_literal->field_literal.field->codifyRepresentation(spaceDepth + 1, printStr);
      else if(this_field_literal->which == this_field_literal->LITERAL)
	this_field_literal->field_literal.literal->codifyRepresentation(spaceDepth + 1, printStr);
      else
	{
	  throw PTException(PTE_CHILD_UNDEF_ERR, nodeType, __LINE__);
	  //throw except;
	}
      DB_P("\n");
      printStr.append("\n");
    }
  DB_P("   ");
  printStr.append(spaceDepth * CODE_ADD_LEVEL_SPACE, ' ');
  if(has_constraints)
    {
      DB_P("} Pwhere {\n");
      printStr.append("} Pwhere {\n");
      constraints->codifyRepresentation(spaceDepth + 1, printStr);
    }
  DB_P("}");
  printStr.append("}");
}

bool PalternatesS::canChangeToType(int newType)
{
  switch(newType)
    {
    default:
      break;
    }
  return false;
}

PNodeP* PalternatesS::convertAndCopy(int newType, PNodeP* copyTo)
{
  return NULL;
}

int PalternatesS::getTextualRepresentation(PSTRING& nameStr)
{
  nameStr.append(PT_TypeNames[nodeType]);
  nameStr.append(" ");
  decl->getTextualRepresentation(nameStr);
  return 0;
}

void PalternatesS::getNameStr(PSTRING& nameStr)
{
  decl->getNameStr(nameStr);
}

PADSPNodeTreeNode* PalternatesS::makeBasicTree(PADSPNodeTreeNode* parent)
{
  DB_P("making composite node::%s\n", PT_TypeNames[nodeType]);
  PADSPNodeTreeNode* thisNode = new PADSPNodeTreeNode(this, parent);

  PADSPNodeTreeNode *newChild = NULL, *currentChild = NULL;

  newChild = modifiers->makeBasicTree(thisNode);
  newChild->setElementIsRequired(true);
  if(currentChild == NULL)
    {
      thisNode->firstChild = newChild;
    }
  else
    {
      currentChild->nextSibling = newChild;
      newChild->previousSibling = currentChild;
    }
  currentChild = newChild;

  newChild = decl->makeBasicTree(thisNode);
  newChild->setUpdateParentOnChange(true);
  newChild->setElementIsRequired(true);
  if(currentChild == NULL)
    {
      thisNode->firstChild = newChild;
    }
  else
    {
      currentChild->nextSibling = newChild;
      newChild->previousSibling = currentChild;
    }
  currentChild = newChild;

  
  field_literalT this_field_literal = NULL;
  int vectorSize = field_literal.size();
  for(int i = 0; i < vectorSize; i++)
    {
      this_field_literal = field_literal.at(i);
      switch(this_field_literal->which)
	{
	case this_field_literal->FIELD:
	  newChild = this_field_literal->field_literal.field->makeBasicTree(thisNode);
	  break;
	case this_field_literal->LITERAL:
	  newChild = this_field_literal->field_literal.literal->makeBasicTree(thisNode);
	  break;
	default:
	  {
	    throw PTException(PTE_CHILD_UNDEF_ERR, nodeType, __LINE__);
	    //throw except;
	    return NULL;
	  }
	}
      if(currentChild == NULL)
	{
	  thisNode->firstChild = newChild;
	}
      else
	{
	  currentChild->nextSibling = newChild;
	  newChild->previousSibling = currentChild;
	}
      currentChild = newChild;
    }

  if(has_constraints)
    {
      newChild = constraints->makeBasicTree(thisNode);
      if(currentChild == NULL)
	{
	  thisNode->firstChild = newChild;
	}
      else
	{
	  currentChild->nextSibling = newChild;
	  newChild->previousSibling = currentChild;
	}
      currentChild = newChild;

    }
  return thisNode;
}

int PalternatesS::canAddChildTypes(int* typeList, int max)
{
  if(max < 3)
    return -1;
  int i = 0;
  typeList[i] = PT_FIELD;
  i++;
  typeList[i] = PT_LITERAL;
  i++;
  if(!has_constraints)
    {
      typeList[i] = PT_CONSTRAINTS2;
      i++;
    }
  return i;
}

int PalternatesS::makeCompleteNode()
{
  decl = new declS();
  
  if(modifiers->makeCompleteNode() != 0)
    return -1;
  if(decl->makeCompleteNode() != 0)
    return -1;
  return 0;
}

int PalternatesS::removeChildNode(PNodeP* removeThis)
{
  int vectorSize;
  int i, j;
  PNodeP* thisPNode;

  if(removeThis == NULL)
    return -1;

  field_literalT this_field_literal;
  vectorSize = field_literal.size();
  for(i = 0; i < vectorSize; i++)
    {
      this_field_literal = field_literal[i];
      if(this_field_literal->which == this_field_literal->FIELD)
	thisPNode = this_field_literal->field_literal.field;
      else if(this_field_literal->which == this_field_literal->LITERAL)
	thisPNode = this_field_literal->field_literal.literal;
      else
	thisPNode = NULL;
      if(thisPNode == removeThis)
	{
	  delete this_field_literal;
	  vectorSize--;
	  for(j = i; j < vectorSize; j++)
	    {
	      field_literal[j] = field_literal[j+1];
	    }
	  field_literal.pop_back();
	  return 0;
	}
    }

  if(removeThis == constraints && has_constraints)
    {
      has_constraints = false;
      constraints = NULL;
      return 0;
    }

  return -1;
}


PADSUniTreeNode* PalternatesS::makeUnifiedTree(PADSUniTreeNode* parent, PSTRING& varStr, PADSTypeMap* typeMap)
{
  DB_P("making composite node::%s\n", PT_TypeNames[nodeType]);
  PADSUniTreeNode* thisNode = new PADSUniTreeNode(this, varStr, parent);

  PADSUniTreeNode *newChild = NULL, *currentChild = NULL;

  PSTRING childNameStr;
  field_literalT this_field_literal;
  PNodeP* childNode;
  for(int i = 0; i < field_literal.size(); i++)
    {
      childNameStr = "";
      this_field_literal = field_literal[i];
      if(this_field_literal->which == this_field_literal->FIELD)
	{
	  this_field_literal->field_literal.field->getNameStr(childNameStr);
	  childNode = typeMap->find(childNameStr);
	  if(childNode != NULL)
	    {
	      newChild = childNode->makeUnifiedTree(thisNode, childNameStr, typeMap);
	      if(currentChild == NULL)
		{
		  thisNode->firstChild = newChild;
		}
	      else
		{
		  currentChild->nextSibling = newChild;
		  newChild->previousSibling = currentChild;
		}
	      currentChild = newChild;
	    }
	  else
	    {
	      newChild = this_field_literal->field_literal.field->makeUnifiedTree(thisNode, childNameStr, typeMap);
	      if(currentChild == NULL)
		{
		  thisNode->firstChild = newChild;
		}
	      else
		{
		  currentChild->nextSibling = newChild;
		  newChild->previousSibling = currentChild;
		}
	      currentChild = newChild;
	    }
	}
    }
  return thisNode;
}

// new_methods_def_point


// **********

PcharclassS::PcharclassS() : PNodeP(PT_PCHARCLASS)
{
  name = NULL;
  expr = NULL;
}

PNodeP* PcharclassS::addChild(PNodeP* newChild)
{
  switch(newChild->nodeType)
    {
    case PT_NAME:
      name = (nameT)newChild;
      break;
    case PT_EXPR:
      expr = (exprT)newChild;
      break;
    default:
      {
	throw PTException(PTE_CHILD_TYPE_ERR, nodeType, __LINE__);
	//throw except;
	return NULL;
      }
    }
  newChild->parent = (PNodeP*)this;
  return this;
}

void PcharclassS::serializeXMLContents(int spaceDepth, PSTRING& printStr)
{
  name->serializeXMLRepresentation(spaceDepth + 1, printStr);
  expr->serializeXMLRepresentation(spaceDepth + 1, printStr);
}

void PcharclassS::codifyContents(int spaceDepth, PSTRING& printStr)
{
  DB_P("Pcharclass ");
  printStr.append(PT_TypeNames[nodeType]);
  printStr.append(" ");
  name->codifyRepresentation(0, printStr);
  DB_P(" ");
  printStr.append(" ");
  expr->codifyRepresentation(0, printStr);
}

bool PcharclassS::canChangeToType(int newType)
{
  switch(newType)
    {
    default:
      break;
    }
  return false;
}

PNodeP* PcharclassS::convertAndCopy(int newType, PNodeP* copyTo)
{
  return NULL;
}

int PcharclassS::getTextualRepresentation(PSTRING& nameStr)
{
  nameStr.append(PT_TypeNames[nodeType]);
  nameStr.append(" ");
  name->getTextualRepresentation(nameStr);
  return 0;
}

void PcharclassS::getNameStr(PSTRING& nameStr)
{
  name->getNameStr(nameStr);
}

PADSPNodeTreeNode* PcharclassS::makeBasicTree(PADSPNodeTreeNode* parent)
{
  DB_P("making composite node::%s\n", PT_TypeNames[nodeType]);
  PADSPNodeTreeNode* thisNode = new PADSPNodeTreeNode(this, parent);

  PADSPNodeTreeNode *newChild = NULL, *currentChild = NULL;

  newChild = name->makeBasicTree(thisNode);
  newChild->setUpdateParentOnChange(true);
  newChild->setElementIsRequired(true);
  if(currentChild == NULL)
    {
      thisNode->firstChild = newChild;
    }
  else
    {
      currentChild->nextSibling = newChild;
      newChild->previousSibling = currentChild;
    }
  currentChild = newChild;

  newChild = expr->makeBasicTree(thisNode);
  newChild->setElementIsRequired(true);
  if(currentChild == NULL)
    {
      thisNode->firstChild = newChild;
    }
  else
    {
      currentChild->nextSibling = newChild;
      newChild->previousSibling = currentChild;
    }
  currentChild = newChild;
  
  return thisNode;
}

int PcharclassS::makeCompleteNode()
{
  name = new nameS();
  expr = new exprS();
  expr->nodeType = PT_EXPR2;
  if(name->makeCompleteNode() != 0)
    return -1;
  if(expr->makeCompleteNode() != 0)
    return -1;
  return 0;
}
// new_methods_def_point


// **********

enumFieldS::enumFieldS() : PNodeP(PT_ENUMFIELD)
{
  name = NULL;

  has_from = false;
  from = NULL;

  has_expr = false;
  expr = NULL;
}

PNodeP* enumFieldS::addChild(PNodeP* newChild)
{
  switch(newChild->nodeType)
    {
    case PT_NAME:
      name = (nameT)newChild;
      break;
    case PT_FROM:
      has_from = true;
      from = (fromT)newChild;
      break;
    case PT_EXPR:
      has_expr = true;
      expr = (exprT)newChild;
      break;
    default:
      {
	throw PTException(PTE_CHILD_TYPE_ERR, nodeType, __LINE__);
	//throw except;
	return NULL;
      }
    }
  newChild->parent = (PNodeP*)this;
  return this;
}

void enumFieldS::serializeXMLContents(int spaceDepth, PSTRING& printStr)
{
  name->serializeXMLRepresentation(spaceDepth + 1, printStr);
  if(has_from)
    from->serializeXMLRepresentation(spaceDepth + 1, printStr);
  if(has_expr)
    expr->serializeXMLRepresentation(spaceDepth + 1, printStr);
}

void enumFieldS::codifyRepresentation(int spaceDepth, PSTRING& printStr)
{
  DB_P(" ");
  printStr.append(spaceDepth * CODE_ADD_LEVEL_SPACE, ' ');
  name->codifyRepresentation(0, printStr);
  if(has_from)
    {
      DB_P(" ");
      printStr.append(" ");
      from->codifyRepresentation(0, printStr);
    }
  if(has_expr)
    {
      DB_P(" ");
      printStr.append(" ");
      expr->codifyRepresentation(0, printStr);
    }
}

bool enumFieldS::canChangeToType(int newType)
{
  switch(newType)
    {
    default:
      break;
    }
  return false;
}

PNodeP* enumFieldS::convertAndCopy(int newType, PNodeP* copyTo)
{
  return NULL;
}

int enumFieldS::getTextualRepresentation(PSTRING& nameStr)
{
  name->getTextualRepresentation(nameStr);
  if(has_expr)
    {
      nameStr.append(" ");
      expr->getTextualRepresentation(nameStr);
    }
  return 0;
}

void enumFieldS::getNameStr(PSTRING& nameStr)
{
  name->getNameStr(nameStr);
}

PADSPNodeTreeNode* enumFieldS::makeBasicTree(PADSPNodeTreeNode* parent)
{
  DB_P("making composite node::%s\n", PT_TypeNames[nodeType]);
  PADSPNodeTreeNode* thisNode = new PADSPNodeTreeNode(this, parent);

  PADSPNodeTreeNode *newChild = NULL, *currentChild = NULL;

  newChild = name->makeBasicTree(thisNode);
  newChild->setUpdateParentOnChange(true);
  newChild->setElementIsRequired(true);
  if(currentChild == NULL)
    {
      thisNode->firstChild = newChild;
    }
  else
    {
      currentChild->nextSibling = newChild;
      newChild->previousSibling = currentChild;
    }
  currentChild = newChild;

  if(has_from)
    {
      newChild = from->makeBasicTree(thisNode);
      if(currentChild == NULL)
	{
	  thisNode->firstChild = newChild;
	}
      else
	{
	  currentChild->nextSibling = newChild;
	  newChild->previousSibling = currentChild;
	}
      currentChild = newChild;
    }  

  if(has_expr)
    {
      newChild = expr->makeBasicTree(thisNode);
      if(currentChild == NULL)
	{
	  thisNode->firstChild = newChild;
	}
      else
	{
	  currentChild->nextSibling = newChild;
	  newChild->previousSibling = currentChild;
	}
      currentChild = newChild;
    }  

  return thisNode;
}

int enumFieldS::canAddChildTypes(int* typeList, int max)
{
  if(max < 2)
    return -1;
  int i = 0;
  if(!has_from)
    {
      typeList[i] = PT_FROM;
      i++;
    }
  if(!has_expr)
    {
      typeList[i] = PT_EXPR2;
      i++;
    }
  return i;
}

int enumFieldS::removeChildNode(PNodeP* removeThis)
{
  if(removeThis == from && has_from)
    {
      has_from = false;
      from = NULL;
      return 0;
    }

  if(removeThis == expr && has_expr)
    {
      has_expr = false;
      expr = NULL;
      return 0;
    }

  return -1;
}

int enumFieldS::makeCompleteNode()
{
  name = new nameS();
  return name->makeCompleteNode();
}
// new_methods_def_point


// **********

PenumS::PenumS() : PNodeP(PT_PENUM)
{
  modifiers = new modifiersS();
  decl = NULL;

  has_Pprefix = false;
  Pprefix = NULL;
}

PNodeP* PenumS::addChild(PNodeP* newChild)
{
  switch(newChild->nodeType)
    {
    case PT_PSOURCE:
    case PT_PRECORD:
    case PT_PLONGEST:
      modifiers->addChild(newChild);
      break;
    case PT_PPREFIX:
      Pprefix = (PprefixT)newChild;
      has_Pprefix = true;
      break;
    case PT_DECL:
      decl = (declT)newChild;
      break;
    case PT_ENUMFIELD:
      enumField.push_back((enumFieldT)newChild);
      break;
    default:
      {
	throw PTException(PTE_CHILD_TYPE_ERR, nodeType, __LINE__);
	//throw except;
	return NULL;
      }
    }
  newChild->parent = (PNodeP*)this;
  return this;
}

/*
PNodeP* PenumS::addValue(PSTRING& newVal)
{
  has_Pprefix = true;
  Pprefix = newVal;
  return this;
}
*/
void PenumS::serializeXMLContents(int spaceDepth, PSTRING& printStr)
{

  DB_P("\n");
  printStr.append("\n");
  int i;
  int numSpaces = (spaceDepth + 1) * XML_ADD_LEVEL_SPACE;
  for(i = 0; i < numSpaces; i++)
    printStr.append(" ");
    DB_P(" "); 

  modifiers->serializeXMLRepresentation(spaceDepth + 1, printStr);
  decl->serializeXMLRepresentation(spaceDepth + 1, printStr);
  if(has_Pprefix)
    {
      DB_P("<prefix>");
      printStr.append("<prefix>");
      Pprefix->serializeXMLRepresentation(0, printStr);
      DB_P("</prefix>");
      printStr.append("</prefix>");
    }
  int size = enumField.size();
  for(int i = 0; i < size; i++)
    {
      enumField.at(i)->serializeXMLRepresentation(spaceDepth + 1, printStr);
    }

  DB_P("\n");
  //for(i = 0; i < numSpaces - XML_ADD_LEVEL_SPACE; i++)
  //  printf(" ");
}

void PenumS::codifyContents(int spaceDepth, PSTRING& printStr)
{
  modifiers->codifyRepresentation(0, printStr);
  DB_P("%s\n", PT_TypeNames[nodeType]);
  printStr.append(PT_TypeNames[nodeType]);
  printStr.append(" ");
  decl->codifyRepresentation(0, printStr);
  DB_P(" ");
  printStr.append(" ");
  if(has_Pprefix)
    {
      DB_P("Pprefix(");
      printStr.append("Pprefix(");
      Pprefix->codifyRepresentation(0, printStr);
      DB_P(") ");
      printStr.append(") ");
    }
  DB_P("{\n");
  printStr.append("{\n");

  int size = enumField.size();
  for(int i = 0; i < size; i++)
    {
      if(i != 0)
	{
	  DB_P(",\n");
	  printStr.append(",\n");
	}

      enumField.at(i)->codifyRepresentation(spaceDepth + 1, printStr);
    }
  DB_P("\n}");
  printStr.append("\n");
  printStr.append(spaceDepth * CODE_ADD_LEVEL_SPACE, ' ');
  printStr.append("}");
}

bool PenumS::canChangeToType(int newType)
{
  switch(newType)
    {
    default:
      break;
    }
  return false;
}

PNodeP* PenumS::convertAndCopy(int newType, PNodeP* copyTo)
{
  return NULL;
}

int PenumS::getTextualRepresentation(PSTRING& nameStr)
{
  nameStr.append(PT_TypeNames[nodeType]);
  nameStr.append(" ");
  decl->getTextualRepresentation(nameStr);
  return 0;
}

void PenumS::getNameStr(PSTRING& nameStr)
{
  decl->getNameStr(nameStr);
}

PADSPNodeTreeNode* PenumS::makeBasicTree(PADSPNodeTreeNode* parent)
{
  DB_P("making composite node::%s\n", PT_TypeNames[nodeType]);
  PADSPNodeTreeNode* thisNode = new PADSPNodeTreeNode(this, parent);

  PADSPNodeTreeNode *newChild = NULL, *currentChild = NULL;

  newChild = modifiers->makeBasicTree(thisNode);
  newChild->setElementIsRequired(true);
  if(currentChild == NULL)
    {
      thisNode->firstChild = newChild;
    }
  else
    {
      currentChild->nextSibling = newChild;
      newChild->previousSibling = currentChild;
    }
  currentChild = newChild;

  newChild = decl->makeBasicTree(thisNode);
  newChild->setUpdateParentOnChange(true);
  newChild->setElementIsRequired(true);
  if(currentChild == NULL)
    {
      thisNode->firstChild = newChild;
    }
  else
    {
      currentChild->nextSibling = newChild;
      newChild->previousSibling = currentChild;
    }
  currentChild = newChild;

  if(has_Pprefix)
    {
      newChild = decl->makeBasicTree(thisNode);
      if(currentChild == NULL)
	{
	  thisNode->firstChild = newChild;
	}
      else
	{
	  currentChild->nextSibling = newChild;
	  newChild->previousSibling = currentChild;
	}
      currentChild = newChild;
    }

  enumFieldT this_enumField = NULL;
  int vectorSize = enumField.size();
  for(int i = 0; i < vectorSize; i++)
    {
      this_enumField = enumField.at(i);
      newChild = this_enumField->makeBasicTree(thisNode);
      if(currentChild == NULL)
	{
	  thisNode->firstChild = newChild;
	}
      else
	{
	  currentChild->nextSibling = newChild;
	  newChild->previousSibling = currentChild;
	}
      currentChild = newChild;
    }
  return thisNode;
}

int PenumS::canAddChildTypes(int* typeList, int max)
{
  if(max < 2)
    return -1;
  int i = 0;
  typeList[i] = PT_ENUMFIELD;
  i++;
  if(!has_Pprefix)
    {
      typeList[i] = PT_PPREFIX;
      i++;
    }
  return i;
}

int PenumS::makeCompleteNode()
{
  decl = new declS();
  if(modifiers->makeCompleteNode() != 0)
    return -1;
  if(decl->makeCompleteNode() != 0)
    return -1;
  return 0;
}

int PenumS::removeChildNode(PNodeP* removeThis)
{
  int vectorSize;
  int i, j;
  PNodeP* thisPNode;

  if(removeThis == NULL)
    return -1;

  if(removeThis == Pprefix && has_Pprefix)
    {
      has_Pprefix = false;
      Pprefix = NULL;
      return 0;
    }

  vectorSize = enumField.size();
  for(i = 0; i < vectorSize; i++)
    {
      thisPNode = enumField[i];
      if(thisPNode == removeThis)
	{
	  vectorSize--;
	  for(j = i; j < vectorSize; j++)
	    {
	      enumField[j] = enumField[j+1];
	    }
	  enumField.pop_back();
	  return 0;
	}
    }

  return -1;
}

// new_methods_def_point


// **********

PrecursiveS::PrecursiveS() : PNodeP(PT_PRECURSIVE)
{
  modifiers = new modifiersS();
  decl = NULL;
}

PNodeP* PrecursiveS::addChild(PNodeP* newChild)
{
  switch(newChild->nodeType)
    {
    case PT_PSOURCE:
    case PT_PRECORD:
    case PT_PLONGEST:
      modifiers->addChild(newChild);
      break;
    case PT_DECL:
      decl = (declT)newChild;
      break;
    default:
      {
	throw PTException(PTE_CHILD_TYPE_ERR, nodeType, __LINE__);
	//throw except;
	return NULL;
      }
    }
  newChild->parent = (PNodeP*)this;
  return this;
}

void PrecursiveS::serializeXMLContents(int spaceDepth, PSTRING& printStr)
{
  modifiers->serializeXMLRepresentation(spaceDepth + 1, printStr);
  decl->serializeXMLRepresentation(spaceDepth + 1, printStr);
}

void PrecursiveS::codifyContents(int spaceDepth, PSTRING& printStr)
{
  modifiers->codifyRepresentation(0, printStr);
  decl->codifyRepresentation(0, printStr);
}

bool PrecursiveS::canChangeToType(int newType)
{
  switch(newType)
    {
    default:
      break;
    }
  return false;
}

PNodeP* PrecursiveS::convertAndCopy(int newType, PNodeP* copyTo)
{
  return NULL;
}

int PrecursiveS::getTextualRepresentation(PSTRING& nameStr)
{
  nameStr.append(PT_TypeNames[nodeType]);
  nameStr.append(" ");
  decl->getTextualRepresentation(nameStr);
  return 0;
}

void PrecursiveS::getNameStr(PSTRING& nameStr)
{
  decl->getNameStr(nameStr);
}

PADSPNodeTreeNode* PrecursiveS::makeBasicTree(PADSPNodeTreeNode* parent)
{
  DB_P("making composite node::%s\n", PT_TypeNames[nodeType]);
  PADSPNodeTreeNode* thisNode = new PADSPNodeTreeNode(this, parent);

  PADSPNodeTreeNode *newChild = NULL, *currentChild = NULL;

  newChild = modifiers->makeBasicTree(thisNode);
  newChild->setElementIsRequired(true);
  if(currentChild == NULL)
    {
      thisNode->firstChild = newChild;
    }
  else
    {
      currentChild->nextSibling = newChild;
      newChild->previousSibling = currentChild;
    }
  currentChild = newChild;

  newChild = decl->makeBasicTree(thisNode);
  newChild->setUpdateParentOnChange(true);
  newChild->setElementIsRequired(true);
  if(currentChild == NULL)
    {
      thisNode->firstChild = newChild;
    }
  else
    {
      currentChild->nextSibling = newChild;
      newChild->previousSibling = currentChild;
    }
  currentChild = newChild;

  return thisNode;
}

int PrecursiveS::makeCompleteNode()
{
  decl = new declS();
  if(modifiers->makeCompleteNode() != 0)
    return -1;
  if(decl->makeCompleteNode() != 0)
    return -1;
  return 0;
}
// new_methods_def_point


// **********

PselectS::PselectS() : PNodeP(PT_PSELECT)
{
  name = NULL;
  typname = NULL;
  var = NULL;
  expr = NULL;
}

PNodeP* PselectS::addChild(PNodeP* newChild)
{
  switch(newChild->nodeType)
    {
    case PT_NAME:
      name = (nameT)newChild;
      break;
    case PT_TYPENAME:
      typname = (typenameT)newChild;
      break;
    case PT_VAR:
      var = (varT)newChild;
      break;
    case PT_EXPR:
      expr = (exprT)newChild;
      break;
    default:
      {
	throw PTException(PTE_CHILD_TYPE_ERR, nodeType, __LINE__);
	//throw except;
	return NULL;
      }
    }
  newChild->parent = (PNodeP*)this;
  return this;
}

void PselectS::serializeXMLContents(int spaceDepth, PSTRING& printStr)
{
  name->serializeXMLRepresentation(spaceDepth + 1, printStr);
  typname->serializeXMLRepresentation(spaceDepth + 1, printStr);
  var->serializeXMLRepresentation(spaceDepth + 1, printStr);
  expr->serializeXMLRepresentation(spaceDepth + 1, printStr);
}

void PselectS::codifyContents(int spaceDepth, PSTRING& printStr)
{
  // ambiguous - not in the manual!
  DB_P("Pselect ");
  printStr.append(PT_TypeNames[nodeType]);
  printStr.append(" ");
  typname->codifyRepresentation(0, printStr);
  DB_P(" ");
  printStr.append(" ");
  name->codifyRepresentation(0, printStr);
  DB_P(" : ");
  printStr.append(" : ");
  var->codifyRepresentation(0, printStr);
  DB_P(" => ");
  printStr.append(" => ");
  expr->codifyRepresentation(0, printStr);  
}

bool PselectS::canChangeToType(int newType)
{
  switch(newType)
    {
    default:
      break;
    }
  return false;
}

PNodeP* PselectS::convertAndCopy(int newType, PNodeP* copyTo)
{
  return NULL;
}

int PselectS::getTextualRepresentation(PSTRING& nameStr)
{
  nameStr.append(PT_TypeNames[nodeType]);
  nameStr.append(" ");
  name->getTextualRepresentation(nameStr);
  return 0;
}

void PselectS::getNameStr(PSTRING& nameStr)
{
  name->getNameStr(nameStr);
}

PADSPNodeTreeNode* PselectS::makeBasicTree(PADSPNodeTreeNode* parent)
{
  DB_P("making composite node::%s\n", PT_TypeNames[nodeType]);
  PADSPNodeTreeNode* thisNode = new PADSPNodeTreeNode(this, parent);

  PADSPNodeTreeNode *newChild = NULL, *currentChild = NULL;

  newChild = name->makeBasicTree(thisNode);
  newChild->setUpdateParentOnChange(true);
  newChild->setElementIsRequired(true);
  if(currentChild == NULL)
    {
      thisNode->firstChild = newChild;
    }
  else
    {
      currentChild->nextSibling = newChild;
      newChild->previousSibling = currentChild;
    }
  currentChild = newChild;

  newChild = typname->makeBasicTree(thisNode);
  newChild->setElementIsRequired(true);
  if(currentChild == NULL)
    {
      thisNode->firstChild = newChild;
    }
  else
    {
      currentChild->nextSibling = newChild;
      newChild->previousSibling = currentChild;
    }
  currentChild = newChild;

  newChild = var->makeBasicTree(thisNode);
  newChild->setElementIsRequired(true);
  if(currentChild == NULL)
    {
      thisNode->firstChild = newChild;
    }
  else
    {
      currentChild->nextSibling = newChild;
      newChild->previousSibling = currentChild;
    }
  currentChild = newChild;

  newChild = expr->makeBasicTree(thisNode);
  newChild->setElementIsRequired(true);
  if(currentChild == NULL)
    {
      thisNode->firstChild = newChild;
    }
  else
    {
      currentChild->nextSibling = newChild;
      newChild->previousSibling = currentChild;
    }
  currentChild = newChild;

  return thisNode;
}

int PselectS::makeCompleteNode()
{
  name = new nameS();
  typname = new typenameS();
  var = new varS();
  expr = new exprS();
  expr->nodeType = PT_EXPR2;
  if(name->makeCompleteNode() != 0)
    return -1;
  if(typname->makeCompleteNode() != 0)
    return -1;
  if(var->makeCompleteNode() != 0)
    return -1;
  if(expr->makeCompleteNode() != 0)
    return -1;
  return 0;
}

// new_methods_def_point


// **********

PstructS::PstructS() : PNodeP(PT_PSTRUCT)
{
  modifiers = new modifiersS();
  decl = NULL;

  has_constraints = false;
  constraints = NULL;
}

PNodeP* PstructS::addChild(PNodeP* newChild)
{
  switch(newChild->nodeType)
    {
    case PT_PSOURCE:
    case PT_PRECORD:
    case PT_PLONGEST:
      modifiers->addChild(newChild);
      break;
    case PT_DECL:
      decl = (declT)newChild;
      break;
    case PT_FIELD:
    case PT_LITERAL:
      {
	field_literalT fl = new field_literalS();
	fl->which = newChild->nodeType == PT_FIELD ? fl->FIELD : fl->LITERAL;
	if(fl->which == fl->FIELD)
	  fl->field_literal.field = (fieldT)newChild;
	else
	  fl->field_literal.literal = (literalT)newChild;
	field_literal.push_back(fl);
      }
      break;
    case PT_CONSTRAINTS:
    case PT_CONSTRAINTS2:
      has_constraints = true;
      constraints = (constraintsT)newChild;
      break;
    default:
      {
	throw PTException(PTE_CHILD_TYPE_ERR, nodeType, __LINE__);
	//throw except;
	return NULL;
      }
    }
  newChild->parent = (PNodeP*)this;
  return this;
}

void PstructS::serializeXMLContents(int spaceDepth, PSTRING& printStr)
{
  DB_P("\n");
  printStr.append("\n");
  int i;
  int numSpaces = (spaceDepth + 1) * XML_ADD_LEVEL_SPACE;
  for(i = 0; i < numSpaces; i++)
    printStr.append(" ");
    DB_P(" "); 

  modifiers->serializeXMLRepresentation(spaceDepth + 1, printStr);
  decl->serializeXMLRepresentation(spaceDepth + 1, printStr);

  int size = field_literal.size();
  field_literalT this_field_literal;
  for(int i = 0; i < size; i++)
    {
      this_field_literal = field_literal.at(i);
      if(this_field_literal->which == this_field_literal->FIELD)
	this_field_literal->field_literal.field->serializeXMLRepresentation(spaceDepth + 1, printStr);
      else if(this_field_literal->which == this_field_literal->LITERAL)
	this_field_literal->field_literal.literal->serializeXMLRepresentation(spaceDepth + 1, printStr);
      else
	{
	  throw PTException(PTE_CHILD_UNDEF_ERR, nodeType, __LINE__);
	  //throw except;
	}
    }

  if(has_constraints)
    constraints->serializeXMLRepresentation(spaceDepth + 1, printStr);
  
  // printf("\n");
  //for(i = 0; i < numSpaces - XML_ADD_LEVEL_SPACE; i++)
  //  printf(" ");
}

void PstructS::codifyContents(int spaceDepth, PSTRING& printStr)
{
  modifiers->codifyRepresentation(0, printStr);
  DB_P("Pstruct ");
  printStr.append(PT_TypeNames[nodeType]);
  printStr.append(" ");
  decl->codifyRepresentation(0, printStr);
  DB_P(" {\n");
  printStr.append(" {\n");

  int size = field_literal.size();
  field_literalT this_field_literal;
  for(int i = 0; i < size; i++)
    {
      this_field_literal = field_literal.at(i);
      //printStr.append("here!\n");
      if(this_field_literal->which == this_field_literal->FIELD)
	this_field_literal->field_literal.field->codifyRepresentation(spaceDepth + 1, printStr);
      else if(this_field_literal->which == this_field_literal->LITERAL)
	this_field_literal->field_literal.literal->codifyRepresentation(spaceDepth + 1, printStr);
      else
	{
	  throw PTException(PTE_CHILD_UNDEF_ERR, nodeType, __LINE__);
	  //throw except;
	}
    }
  DB_P("   ");
  printStr.append(spaceDepth * CODE_ADD_LEVEL_SPACE, ' ');
  if(has_constraints)
    {
      DB_P("} Pwhere {\n");
      printStr.append("} Pwhere {\n");
      constraints->codifyRepresentation(spaceDepth + 1, printStr);
    }
  DB_P("}");
  printStr.append("}");
}

bool PstructS::canChangeToType(int newType)
{
  switch(newType)
    {
    case PT_PUNION:
      return true;
    case PT_PARRAY:
      if(field_literal.size() > 0)
	return true;
      else
	return false;
    default:
      break;
    }
}

PNodeP* PstructS::convertAndCopy(int newType, PNodeP* copyTo)
{
  return NULL;
}

int PstructS::getTextualRepresentation(PSTRING& nameStr)
{
  nameStr.append(PT_TypeNames[nodeType]);
  nameStr.append(" ");
  decl->getTextualRepresentation(nameStr);
  return 0;
}

void PstructS::getNameStr(PSTRING& nameStr)
{
  decl->getNameStr(nameStr);
}

PADSPNodeTreeNode* PstructS::makeBasicTree(PADSPNodeTreeNode* parent)
{
  DB_P("making composite node::%s\n", PT_TypeNames[nodeType]);
  PADSPNodeTreeNode* thisNode = new PADSPNodeTreeNode(this, parent);

  PADSPNodeTreeNode *newChild = NULL, *currentChild = NULL;

  newChild = modifiers->makeBasicTree(thisNode);
  newChild->setElementIsRequired(true);
  if(currentChild == NULL)
    {
      thisNode->firstChild = newChild;
    }
  else
    {
      currentChild->nextSibling = newChild;
      newChild->previousSibling = currentChild;
    }
  currentChild = newChild;

  newChild = decl->makeBasicTree(thisNode);
  newChild->setUpdateParentOnChange(true);
  newChild->setElementIsRequired(true);
  if(currentChild == NULL)
    {
      thisNode->firstChild = newChild;
    }
  else
    {
      currentChild->nextSibling = newChild;
      newChild->previousSibling = currentChild;
    }
  currentChild = newChild;
  
  field_literalT this_field_literal = NULL;
  int vectorSize = field_literal.size();
  for(int i = 0; i < vectorSize; i++)
    {
      this_field_literal = field_literal.at(i);
      switch(this_field_literal->which)
	{
	case this_field_literal->FIELD:
	  newChild = this_field_literal->field_literal.field->makeBasicTree(thisNode);
	  break;
	case this_field_literal->LITERAL:
	  newChild = this_field_literal->field_literal.literal->makeBasicTree(thisNode);
	  break;
	default:
	  {
	    throw PTException(PTE_CHILD_UNDEF_ERR, nodeType, __LINE__);
	    //throw except;
	    return NULL;
	  }
	}
      if(currentChild == NULL)
	{
	  thisNode->firstChild = newChild;
	}
      else
	{
	  currentChild->nextSibling = newChild;
	  newChild->previousSibling = currentChild;
	}
      currentChild = newChild;
    }
  if(has_constraints)
    {
      newChild = constraints->makeBasicTree(thisNode);

      if(currentChild == NULL)
	{
	  thisNode->firstChild = newChild;
	}
      else
	{
	  currentChild->nextSibling = newChild;
	  newChild->previousSibling = currentChild;
	}
      currentChild = newChild;      
    }
  return thisNode;
}

int PstructS::canAddChildTypes(int* typeList, int max)
{
  if(max < 3)
    return -1;
  int i = 0;
  typeList[i] = PT_FIELD;
  i++;
  typeList[i] = PT_LITERAL;
  i++;
  if(!has_constraints)
    {
      typeList[i] = PT_CONSTRAINTS2;
      i++;
    }
  return i;
}

int PstructS::removeChildNode(PNodeP* removeThis)
{
  int vectorSize;
  int i, j;
  PNodeP* thisPNode;

  if(removeThis == NULL)
    return -1;

  field_literalT this_field_literal;
  vectorSize = field_literal.size();
  for(i = 0; i < vectorSize; i++)
    {
      this_field_literal = field_literal[i];
      if(this_field_literal->which == this_field_literal->FIELD)
	thisPNode = this_field_literal->field_literal.field;
      else if(this_field_literal->which == this_field_literal->LITERAL)
	thisPNode = this_field_literal->field_literal.literal;
      else
	thisPNode = NULL;
      if(thisPNode == removeThis)
	{
	  delete this_field_literal;
	  vectorSize--;
	  for(j = i; j < vectorSize; j++)
	    {
	      field_literal[j] = field_literal[j+1];
	    }
	  field_literal.pop_back();
	  return 0;
	}
    }

  if(removeThis == constraints && has_constraints)
    {
      has_constraints = false;
      constraints = NULL;
      return 0;
    }

  return -1;
}

int PstructS::makeCompleteNode()
{
  decl = new declS();
  if(modifiers->makeCompleteNode() != 0)
    return -1;
  if(decl->makeCompleteNode() != 0)
    return -1;
  return 0;
}

int PstructS::insertIntoNameMap(PADSTypeMap* nameMap)
{
  field_literalT this_field_literal;
  for(int i = 0; i < field_literal.size(); i++)
    {
      this_field_literal = field_literal[i];
      if(this_field_literal->which == this_field_literal->FIELD)
	this_field_literal->field_literal.field->insertIntoNameMap(nameMap);
    }

  PSTRING nameStr;
  getNameStr(nameStr);
  return nameMap->insert(nameStr, this);
}

int PstructS::insertIntoPathMap(PADSTypeMap* pathMap, PSTRING& pathStr)
{
  PSTRING nameStr;
  PSTRING fullPath;
  getNameStr(nameStr);
  fullPath = pathStr;
  fullPath.append(":");
  fullPath.append(nameStr);

  field_literalT this_field_literal;
  for(int i = 0; i < field_literal.size(); i++)
    {
      this_field_literal = field_literal[i];
      if(this_field_literal->which == this_field_literal->FIELD)
	this_field_literal->field_literal.field->insertIntoPathMap(pathMap, fullPath);
    }

  return pathMap->insert(fullPath, this);
}

PADSUniTreeNode* PstructS::makeUnifiedTree(PADSUniTreeNode* parent, PSTRING& varStr, PADSTypeMap* typeMap)
{
  DB_P("making Pstruct node %s::%s\n", varStr.c_str(), PT_TypeNames[nodeType]);
  PADSUniTreeNode* thisNode = new PADSUniTreeNode(this, varStr, parent);

  PADSUniTreeNode *newChild = NULL, *currentChild = NULL;

  PSTRING childTypeStr = "";
  PSTRING childNameStr = "";
  field_literalT this_field_literal;
  PNodeP* childNode;
  for(int i = 0; i < field_literal.size(); i++)
    {
      childNameStr = "";
      this_field_literal = field_literal[i];
      if(this_field_literal->which == this_field_literal->FIELD)
	{
	  this_field_literal->field_literal.field->getTypeStr(childTypeStr);
	  this_field_literal->field_literal.field->getNameStr(childNameStr);
	  childNode = typeMap->find(childTypeStr);
	  DB_P("  PStruct child => %s::%s\n", childNameStr.c_str(), childTypeStr.c_str());
	  DB_P("    child lookup val => %d\n", childNode);
	  if(childNode != NULL)
	    {
	      DB_P("## calling makeUnifiedTree on child node\n");
	      newChild = childNode->makeUnifiedTree(thisNode, childNameStr, typeMap);
	      if(newChild == NULL)
		{
		  newChild = this_field_literal->field_literal.field->makeUnifiedTree(thisNode, childNameStr, typeMap);
		  if(currentChild == NULL)
		    {
		      thisNode->firstChild = newChild;
		    }
		  else
		    {
		      currentChild->nextSibling = newChild;
		      newChild->previousSibling = currentChild;
		    }
		  currentChild = newChild;
		}
	      if(newChild->getLinkNode()->nodeType = PT_PTYPEDEF)
		{
		  newChild->setLinkNode(this_field_literal->field_literal.field);
		}

	      DB_P("## finished call of makeUnifiedTree on child node\n");
	      if(currentChild == NULL)
		{
		  thisNode->firstChild = newChild;
		}
	      else
		{
		  currentChild->nextSibling = newChild;
		  newChild->previousSibling = currentChild;
		}
	      currentChild = newChild;
	    }
	  else
	    {
	      newChild = this_field_literal->field_literal.field->makeUnifiedTree(thisNode, childNameStr, typeMap);
	      if(currentChild == NULL)
		{
		  thisNode->firstChild = newChild;
		}
	      else
		{
		  currentChild->nextSibling = newChild;
		  newChild->previousSibling = currentChild;
		}
	      currentChild = newChild;
	    }
	}
    }

  DB_P("@@Returning from Pstruct (%s) tree unification\n", varStr.c_str());
  return thisNode;
}

// new_methods_def_point


// **********

PtypedefS::PtypedefS() : PNodeP(PT_PTYPEDEF)
{
  modifiers = new modifiersS();
  decl = NULL;
  ptype = NULL;

  has_var = false;
  var = NULL;

  has_constraints = false;
  constraints = NULL;
}

PNodeP* PtypedefS::addChild(PNodeP* newChild)
{
  switch(newChild->nodeType)
    {
    case PT_PSOURCE:
    case PT_PRECORD:
    case PT_PLONGEST:
      modifiers->addChild(newChild);
      break;
    case PT_DECL:
      decl = (declT)newChild;
      break;
    case PT_PTYPE:
      ptype = (ptypeT)newChild;
      break;
    case PT_VAR:
      has_var = true;
      var = (varT)newChild;
      break;
    case PT_CONSTRAINTS:
    case PT_CONSTRAINTS2:
      has_constraints = true;
      constraints = (constraintsT)newChild;
      break;
    default:
      {
	throw PTException(PTE_CHILD_TYPE_ERR, nodeType, __LINE__);
	//throw except;
	return NULL;
      }
    }
  newChild->parent = (PNodeP*)this;
  return this;
}

void PtypedefS::serializeXMLContents(int spaceDepth, PSTRING& printStr)
{
  modifiers->serializeXMLRepresentation(spaceDepth + 1, printStr);
  decl->serializeXMLRepresentation(spaceDepth + 1, printStr);
  ptype->serializeXMLRepresentation(spaceDepth + 1, printStr);
  if(has_var)
    var->serializeXMLRepresentation(spaceDepth + 1, printStr);
  if(has_constraints)
    constraints->serializeXMLRepresentation(spaceDepth + 1, printStr);
}

void PtypedefS::codifyContents(int spaceDepth, PSTRING& printStr)
{
  DB_P("Ptypedef ");
  printStr.append(PT_TypeNames[nodeType]);
  printStr.append(" ");
  ptype->codifyRepresentation(0, printStr);
  DB_P(" ");
  printStr.append(" ");
  decl->codifyRepresentation(0, printStr);
  if(has_var || has_constraints)
    {
      DB_P(" : ");
      printStr.append(" : ");
    }
  if(has_var)
    {
      ptype->codifyRepresentation(0, printStr);
      DB_P(" ");
      printStr.append(" ");
      var->codifyRepresentation(0, printStr);
      DB_P(" => ");
      printStr.append(" => ");
    }
  if(has_constraints)
      constraints->codifyRepresentation(0, printStr);
}

bool PtypedefS::canChangeToType(int newType)
{
  switch(newType)
    {
    default:
      break;
    }
  return false;
}

PNodeP* PtypedefS::convertAndCopy(int newType, PNodeP* copyTo)
{
  return NULL;
}

int PtypedefS::getTextualRepresentation(PSTRING& nameStr)
{
  nameStr.append(PT_TypeNames[nodeType]);
  nameStr.append(" ");
  decl->getTextualRepresentation(nameStr);
  nameStr.append(" ");
  ptype->getTextualRepresentation(nameStr);
  return 0;
}

void PtypedefS::getNameStr(PSTRING& nameStr)
{
  decl->getNameStr(nameStr);
}

PADSPNodeTreeNode* PtypedefS::makeBasicTree(PADSPNodeTreeNode* parent)
{
  DB_P("making composite node::%s\n", PT_TypeNames[nodeType]);
  PADSPNodeTreeNode* thisNode = new PADSPNodeTreeNode(this, parent);

  PADSPNodeTreeNode *newChild = NULL, *currentChild = NULL;

  newChild = modifiers->makeBasicTree(thisNode);
  newChild->setElementIsRequired(true);
  if(currentChild == NULL)
    {
      thisNode->firstChild = newChild;
    }
  else
    {
      currentChild->nextSibling = newChild;
      newChild->previousSibling = currentChild;
    }
  currentChild = newChild;

  newChild = decl->makeBasicTree(thisNode);
  newChild->setElementIsRequired(true);
  if(currentChild == NULL)
    {
      thisNode->firstChild = newChild;
    }
  else
    {
      currentChild->nextSibling = newChild;
      newChild->previousSibling = currentChild;
    }
  currentChild = newChild;

  newChild = ptype->makeBasicTree(thisNode);
  newChild->setElementIsRequired(true);
  if(currentChild == NULL)
    {
      thisNode->firstChild = newChild;
    }
  else
    {
      currentChild->nextSibling = newChild;
      newChild->previousSibling = currentChild;
    }
  currentChild = newChild;

  if(has_var)
    {
      newChild = var->makeBasicTree(thisNode);
      if(currentChild == NULL)
	{
	  thisNode->firstChild = newChild;
	}
      else
	{
	  currentChild->nextSibling = newChild;
	  newChild->previousSibling = currentChild;
	}
      currentChild = newChild;
    }

  if(has_constraints)
    {
      newChild = constraints->makeBasicTree(thisNode);

      if(currentChild == NULL)
	{
	  thisNode->firstChild = newChild;
	}
      else
	{
	  currentChild->nextSibling = newChild;
	  newChild->previousSibling = currentChild;
	}
      currentChild = newChild;      
    }
  return thisNode;
}

int PtypedefS::canAddChildTypes(int* typeList, int max)
{
  if(max < 3)
    return -1;
  int i = 0;
  if(!has_var)
    {
      typeList[i] = PT_VAR;
      i++;
    }
  if(!has_constraints)
    {
      typeList[i] = PT_CONSTRAINTS2;
      i++;
    }
  return i;
}

int PtypedefS::removeChildNode(PNodeP* removeThis)
{
  if(removeThis == var && has_var)
    {
      has_var = false;
      var = NULL;
      return 0;
    }

  if(removeThis == constraints && has_constraints)
    {
      has_constraints = false;
      constraints = NULL;
      return 0;
    }

  return -1;
}

int PtypedefS::makeCompleteNode()
{
  decl = new declS();
  ptype = new ptypeS();
  if(modifiers->makeCompleteNode() != 0)
    return -1;
  if(decl->makeCompleteNode() != 0)
    return -1;
  if(ptype->makeCompleteNode() != 0)
    return -1;
  return 0;
}

PADSUniTreeNode* PtypedefS::makeUnifiedTree(PADSUniTreeNode* parent, PSTRING& varStr, PADSTypeMap* typeMap)
{
  DB_P("making composite node::%s\n", PT_TypeNames[nodeType]);
  //PADSUniTreeNode* thisNode = new PADSUniTreeNode(this, varStr, parent);

  PADSUniTreeNode *newChild = NULL, *currentChild = NULL;

  PSTRING typeStr = "";
  ptype->getNameStr(typeStr);
  DB_P("  Ptypedef type :: %s\n", typeStr.c_str());
  PNodeP* childNode = typeMap->find(typeStr);
  if(childNode != NULL)
    {
      newChild = childNode->makeUnifiedTree(parent, varStr, typeMap);
      return newChild;
    }
  DB_P("making base node::%s, name = %s \n", PT_TypeNames[nodeType], varStr.c_str());
  PADSUniTreeNode* thisNode = new PADSUniTreeNode(this, varStr, parent);
  thisNode->setTypeName(typeStr);
  return thisNode;
}


// new_methods_def_point


// **********

PcaseS::PcaseS() : PNodeP(PT_PCASE)
{
  expr = NULL;
  field = NULL;
}

PNodeP* PcaseS::addChild(PNodeP* newChild)
{
  switch(newChild->nodeType)
    {
    case PT_EXPR:
      expr = (exprT)newChild;
      break;
    case PT_FIELD:
      field = (fieldT)newChild;
      break;
    default:
      {
	throw PTException(PTE_CHILD_TYPE_ERR, nodeType, __LINE__);
	//throw except;
	return NULL;
      }
    }
  newChild->parent = (PNodeP*)this;
  return this;
}

void PcaseS::serializeXMLContents(int spaceDepth, PSTRING& printStr)
{
  expr->serializeXMLRepresentation(spaceDepth + 1, printStr);
  field->serializeXMLRepresentation(spaceDepth + 1, printStr);
}

void PcaseS::codifyContents(int spaceDepth, PSTRING& printStr)
{
  DB_P("Pcase ");
  printStr.append(PT_TypeNames[nodeType]);
  printStr.append(" ");
  expr->codifyRepresentation(0, printStr);
  DB_P(" : ");
  printStr.append(" : ");
  field->codifyRepresentation(0, printStr);
}

bool PcaseS::canChangeToType(int newType)
{
  switch(newType)
    {
    default:
      break;
    }
  return false;
}

PNodeP* PcaseS::convertAndCopy(int newType, PNodeP* copyTo)
{
  return NULL;
}

int PcaseS::getTextualRepresentation(PSTRING& nameStr)
{
  nameStr.append(PT_TypeNames[nodeType]);
  nameStr.append(" ");
  field->getTextualRepresentation(nameStr);
  return 0;
}

PADSPNodeTreeNode* PcaseS::makeBasicTree(PADSPNodeTreeNode* parent)
{
  DB_P("making composite node::%s\n", PT_TypeNames[nodeType]);
  PADSPNodeTreeNode* thisNode = new PADSPNodeTreeNode(this, parent);

  PADSPNodeTreeNode *newChild = NULL, *currentChild = NULL;

  newChild = expr->makeBasicTree(thisNode);
  newChild->setUpdateParentOnChange(true);
  newChild->setElementIsRequired(true);
  if(currentChild == NULL)
    {
      thisNode->firstChild = newChild;
    }
  else
    {
      currentChild->nextSibling = newChild;
      newChild->previousSibling = currentChild;
    }
  currentChild = newChild;

  newChild = field->makeBasicTree(thisNode);
  newChild->setElementIsRequired(true);
  if(currentChild == NULL)
    {
      thisNode->firstChild = newChild;
    }
  else
    {
      currentChild->nextSibling = newChild;
      newChild->previousSibling = currentChild;
    }
  currentChild = newChild;
    
  return thisNode;
}

int PcaseS::makeCompleteNode()
{
  expr = new exprS();
  field = new fieldS();
  expr->nodeType = PT_EXPR2;
  if(expr->makeCompleteNode() != 0)
    return -1;
  if(field->makeCompleteNode() != 0)
    return -1;
  return 0;
}

// new_methods_def_point


// **********

PdefaultS::PdefaultS() : PNodeP(PT_PDEFAULT)
{
  field = NULL;
}

PNodeP* PdefaultS::addChild(PNodeP* newChild)
{
  switch(newChild->nodeType)
    {
    case PT_FIELD:
      field = (fieldT)newChild;
      break;
    default:
      {
	throw PTException(PTE_CHILD_TYPE_ERR, nodeType, __LINE__);
	//throw except;
	return NULL;
      }
    }
  newChild->parent = (PNodeP*)this;
  return this;  
}

void PdefaultS::serializeXMLContents(int spaceDepth, PSTRING& printStr)
{
  field->serializeXMLRepresentation(spaceDepth + 1, printStr);
}

void PdefaultS::codifyContents(int spaceDepth, PSTRING& printStr)
{
  DB_P("Pdefault : ");
  printStr.append(PT_TypeNames[nodeType]);
  printStr.append(" : ");
  field->codifyRepresentation(0, printStr);
}

bool PdefaultS::canChangeToType(int newType)
{
  switch(newType)
    {
    default:
      break;
    }
  return false;
}

PNodeP* PdefaultS::convertAndCopy(int newType, PNodeP* copyTo)
{
  return NULL;
}

int PdefaultS::getTextualRepresentation(PSTRING& nameStr)
{
  nameStr.append(PT_TypeNames[nodeType]);
  nameStr.append(" ");
  field->getTextualRepresentation(nameStr);
  return 0;
}

PADSPNodeTreeNode* PdefaultS::makeBasicTree(PADSPNodeTreeNode* parent)
{
  DB_P("making composite node::%s\n", PT_TypeNames[nodeType]);
  PADSPNodeTreeNode* thisNode = new PADSPNodeTreeNode(this, parent);

  PADSPNodeTreeNode *newChild = NULL, *currentChild = NULL;

  newChild = field->makeBasicTree(thisNode);
  newChild->setElementIsRequired(true);
  if(currentChild == NULL)
    {
      thisNode->firstChild = newChild;
    }
  else
    {
      currentChild->nextSibling = newChild;
      newChild->previousSibling = currentChild;
    }
  currentChild = newChild;
    
  return thisNode;
}

int PdefaultS::makeCompleteNode()
{
  field = new fieldS();
  if(field->makeCompleteNode() != 0)
    return -1;
  return 0;
}

// new_methods_def_point


// **********

PswitchS::PswitchS() : PNodeP(PT_PSWITCH)
{
  expr = NULL;
  Pdefault = NULL;
}

PNodeP* PswitchS::addChild(PNodeP* newChild)
{
  switch(newChild->nodeType)
    {
    case PT_EXPR:
      expr = (exprT)newChild;
      break;
    case PT_PCASE:
      Pcase.push_back((PcaseT)newChild);
      break;
    case PT_PDEFAULT:
      Pdefault = (PdefaultT)newChild;
      break;
    default:
      {
	throw PTException(PTE_CHILD_TYPE_ERR, nodeType, __LINE__);
	//throw except;
	return NULL;
      }
    }
  newChild->parent = (PNodeP*)this;
  return this;
}

void PswitchS::serializeXMLContents(int spaceDepth, PSTRING& printStr)
{
  DB_P("\n");
  printStr.append("\n");
  int i;
  int numSpaces = (spaceDepth + 1) * XML_ADD_LEVEL_SPACE;
  for(i = 0; i < numSpaces; i++)
    printStr.append(" ");
    DB_P(" "); 

  expr->serializeXMLRepresentation(spaceDepth + 1, printStr);

  int size = Pcase.size();
  for(int i = 0; i < size; i++)
    {
      Pcase.at(i)->serializeXMLRepresentation(spaceDepth + 1, printStr);
    }

  Pdefault->serializeXMLRepresentation(spaceDepth + 1, printStr);

  DB_P("\n");
  //for(i = 0; i < numSpaces - XML_ADD_LEVEL_SPACE; i++)
  //  printf(" ");

}

void PswitchS::codifyContents(int spaceDepth, PSTRING& printStr)
{
  DB_P("Pswitch ");
  printStr.append(PT_TypeNames[nodeType]);
  printStr.append(" ");
  expr->codifyRepresentation(0, printStr);
  DB_P(" {\n");
  printStr.append(" {\n");
  int size = Pcase.size();
  for(int i = 0; i < size; i++)
    {
      Pcase.at(i)->codifyRepresentation(spaceDepth + 1, printStr);
    }

  DB_P("   ");
  printStr.append(" ", (spaceDepth + 1) * CODE_ADD_LEVEL_SPACE);
  Pdefault->codifyRepresentation(0, printStr);
  DB_P("   }\n");
  printStr.append(spaceDepth * CODE_ADD_LEVEL_SPACE, ' ');
  printStr.append("}\n");
}

bool PswitchS::canChangeToType(int newType)
{
  switch(newType)
    {
    default:
      break;
    }
  return false;
}

PNodeP* PswitchS::convertAndCopy(int newType, PNodeP* copyTo)
{
  return NULL;
}

int PswitchS::getTextualRepresentation(PSTRING& nameStr)
{
  nameStr.append(PT_TypeNames[nodeType]);
  nameStr.append(" (");
  expr->getTextualRepresentation(nameStr);
  nameStr.append(")");
  return 0;
}

PADSPNodeTreeNode* PswitchS::makeBasicTree(PADSPNodeTreeNode* parent)
{
  DB_P("making composite node::%s\n", PT_TypeNames[nodeType]);
  PADSPNodeTreeNode* thisNode = new PADSPNodeTreeNode(this, parent);    

  PADSPNodeTreeNode *newChild = NULL, *currentChild = NULL;

  newChild = expr->makeBasicTree(thisNode);
  newChild->setElementIsRequired(true);
  if(currentChild == NULL)
    {
      thisNode->firstChild = newChild;
    }
  else
    {
      currentChild->nextSibling = newChild;
      newChild->previousSibling = currentChild;
    }
  currentChild = newChild;
  
  PcaseT this_Pcase = NULL;
  int vectorSize = Pcase.size();
  for(int i = 0; i < vectorSize; i++)
    {
      this_Pcase = Pcase.at(i);
      newChild = this_Pcase->makeBasicTree(thisNode);
      if(currentChild == NULL)
	{
	  thisNode->firstChild = newChild;
	}
      else
	{
	  currentChild->nextSibling = newChild;
	  newChild->previousSibling = currentChild;
	}
      currentChild = newChild;
    }

  newChild = Pdefault->makeBasicTree(thisNode);
  newChild->setElementIsRequired(true);
  if(currentChild == NULL)
    {
      thisNode->firstChild = newChild;
    }
  else
    {
      currentChild->nextSibling = newChild;
      newChild->previousSibling = currentChild;
    }
  currentChild = newChild;

  return thisNode;
}

int PswitchS::canAddChildTypes(int* typeList, int max)
{
  if(max < 1)
    return -1;
  typeList[0] = PT_PCASE;
  return 1;
}

int PswitchS::removeChildNode(PNodeP* removeThis)
{
  int vectorSize;
  int i, j;
  PNodeP* thisPNode;

  if(removeThis == NULL)
    return -1;

  vectorSize = Pcase.size();
  for(i = 0; i < vectorSize; i++)
    {
      thisPNode = Pcase[i];
      if(thisPNode == removeThis)
	{
	  vectorSize--;
	  for(j = i; j < vectorSize; j++)
	    {
	      Pcase[j] = Pcase[j+1];
	    }
	  Pcase.pop_back();
	  return 0;
	}
    }
  return -1;
}

int PswitchS::makeCompleteNode()
{
  expr = new exprS();
  Pdefault = new PdefaultS();
  expr->nodeType = PT_EXPR2;
  if(expr->makeCompleteNode() != 0)
    return -1;
  if(Pdefault->makeCompleteNode() != 0)
    return -1;
  return 0;
}

// new_methods_def_point


// **********

PunionS::PunionS() : PNodeP(PT_PUNION)
{
  modifiers = new modifiersS();
  decl = NULL;

  which = UNDEFINED;
  Pswitch = NULL;

  has_constraints = false;
  constraints = NULL;
}

PNodeP* PunionS::addChild(PNodeP* newChild)
{
  switch(newChild->nodeType)
    {
    case PT_PSOURCE:
    case PT_PRECORD:
    case PT_PLONGEST:
      modifiers->addChild(newChild);
      break;
    case PT_DECL:
      decl = (declT)newChild;
      break;
    case PT_PSWITCH:
      Pswitch = (PswitchT)newChild;
      which = SWITCH;
      break;
    case PT_FIELD:
    case PT_LITERAL:
      {
	field_literalT fl = new field_literalS();
	fl->which = newChild->nodeType == PT_FIELD ? fl->FIELD : fl->LITERAL;
	if(fl->which == fl->FIELD)	  
	    fl->field_literal.field = (fieldT)newChild;
	else
	  fl->field_literal.literal = (literalT)newChild;
	field_literal.push_back(fl);
	which = FIELDLITERAL;
      }
      which = FIELDLITERAL;
      break;
    case PT_CONSTRAINTS:
    case PT_CONSTRAINTS2:
      has_constraints = true;
      constraints = (constraintsT)newChild;
      break;
    default:
      {
	throw PTException(PTE_CHILD_TYPE_ERR, nodeType, __LINE__);
	//throw except;
	return NULL;
      }
    }
  newChild->parent = (PNodeP*)this;
  return this;
}

void PunionS::serializeXMLContents(int spaceDepth, PSTRING& printStr)
{
  DB_P("\n");
  printStr.append("\n");
  int i;
  int numSpaces = (spaceDepth + 1) * XML_ADD_LEVEL_SPACE;
  for(i = 0; i < numSpaces; i++)
    printStr.append(" ");
    DB_P(" "); 

  modifiers->serializeXMLRepresentation(spaceDepth + 1, printStr);
  decl->serializeXMLRepresentation(spaceDepth + 1, printStr);

  if(which == SWITCH)
    {
      Pswitch->serializeXMLRepresentation(spaceDepth + 1, printStr);
    }
  else if(which == FIELDLITERAL)
    {
      int size = field_literal.size();
      field_literalT this_field_literal;
      for(int i = 0; i < size; i++)
	{
	  this_field_literal = field_literal.at(i);
	  if(this_field_literal->which == this_field_literal->FIELD)
	    this_field_literal->field_literal.field->serializeXMLRepresentation(spaceDepth + 1, printStr);
	  else if(this_field_literal->which == this_field_literal->LITERAL)
	    this_field_literal->field_literal.literal->serializeXMLRepresentation(spaceDepth + 1, printStr);
	  else
	    {
	      throw PTException(PTE_CHILD_UNDEF_ERR, nodeType, __LINE__);
	      //throw except;
	    }
	}
    }
  else
    {
      throw PTException(PTE_CHILD_UNDEF_ERR, nodeType, __LINE__);
      //throw except;
    }
  
  if(has_constraints)
    constraints->serializeXMLRepresentation(spaceDepth + 1, printStr);

  DB_P("\n");
  //for(i = 0; i < numSpaces - XML_ADD_LEVEL_SPACE; i++)
  //  printf(" ");
}

void PunionS::codifyContents(int spaceDepth, PSTRING& printStr)
{
  modifiers->codifyRepresentation(0, printStr);
  DB_P("Punion ");
  printStr.append(PT_TypeNames[nodeType]);
  printStr.append(" ");
  decl->codifyRepresentation(0, printStr);
  DB_P(" {\n");
  printStr.append(" {\n");

  if(which == SWITCH)
    {
      Pswitch->codifyRepresentation(spaceDepth + 1, printStr);
    }
  else if(which == FIELDLITERAL)
    {
      int size = field_literal.size();
      field_literalT this_field_literal;
      for(int i = 0; i < size; i++)
	{
	  this_field_literal = field_literal.at(i);
	  if(this_field_literal->which == this_field_literal->FIELD)
	    this_field_literal->field_literal.field->codifyRepresentation(spaceDepth + 1, printStr);
	  else if(this_field_literal->which == this_field_literal->LITERAL)
	    this_field_literal->field_literal.literal->codifyRepresentation(spaceDepth + 1, printStr);
	  else
	    {
	      throw PTException(PTE_CHILD_UNDEF_ERR, nodeType, __LINE__);
	      //throw except;
	    }
	}
    }
  else
    {
      throw PTException(PTE_CHILD_UNDEF_ERR, nodeType, __LINE__);
      //throw except;
    }
  DB_P("   ");
  printStr.append(spaceDepth * CODE_ADD_LEVEL_SPACE, ' ');
  if(has_constraints)
    {
      DB_P("} Pwhere {\n");
      printStr.append("} Pwhere {\n");
      constraints->codifyRepresentation(spaceDepth + 1, printStr);
    }
  DB_P("}");
  printStr.append("}");
}

bool PunionS::canChangeToType(int newType)
{
  switch(newType)
    {
    case PT_PSTRUCT:
      return true;
    case PT_PARRAY:
      if(which == FIELDLITERAL && field_literal.size() > 0)
	return true;
      else
	return false;
    default:
      break;
    }
  return false;
}

PNodeP* PunionS::convertAndCopy(int newType, PNodeP* copyTo)
{
  return NULL;
}

int PunionS::getTextualRepresentation(PSTRING& nameStr)
{
  nameStr.append(PT_TypeNames[nodeType]);
  nameStr.append(" ");
  decl->getTextualRepresentation(nameStr);
  return 0;
}

void PunionS::getNameStr(PSTRING& nameStr)
{
  decl->getNameStr(nameStr);
}

PADSPNodeTreeNode* PunionS::makeBasicTree(PADSPNodeTreeNode* parent)
{
  DB_P("making composite node::%s\n", PT_TypeNames[nodeType]);
  PADSPNodeTreeNode* thisNode = new PADSPNodeTreeNode(this, parent);    

  PADSPNodeTreeNode *newChild = NULL, *currentChild = NULL;

  newChild = modifiers->makeBasicTree(thisNode);
  newChild->setElementIsRequired(true);
  if(currentChild == NULL)
    {
      thisNode->firstChild = newChild;
    }
  else
    {
      currentChild->nextSibling = newChild;
      newChild->previousSibling = currentChild;
    }
  currentChild = newChild;

  newChild = decl->makeBasicTree(thisNode);
  newChild->setUpdateParentOnChange(true);
  newChild->setElementIsRequired(true);
  if(currentChild == NULL)
    {
      thisNode->firstChild = newChild;
    }
  else
    {
      currentChild->nextSibling = newChild;
      newChild->previousSibling = currentChild;
    }
  currentChild = newChild;

  if(which == SWITCH)
    {
      newChild = Pswitch->makeBasicTree(thisNode);
      newChild->setElementIsRequired(true);
      if(currentChild == NULL)
	{
	  thisNode->firstChild = newChild;
	}
      else
	{
	  currentChild->nextSibling = newChild;
	  newChild->previousSibling = currentChild;
	}
      currentChild = newChild;
    }
  else if(which == FIELDLITERAL)
    {
      field_literalT this_field_literal = NULL;
      int vectorSize = field_literal.size();
      for(int i = 0; i < vectorSize; i++)
	{
	  this_field_literal = field_literal.at(i);
	  switch(this_field_literal->which)
	    {
	    case this_field_literal->FIELD:
	      newChild = this_field_literal->field_literal.field->makeBasicTree(thisNode);
	      break;
	    case this_field_literal->LITERAL:
	      newChild = this_field_literal->field_literal.literal->makeBasicTree(thisNode);
	      break;
	    default:
	      {
		throw PTException(PTE_CHILD_UNDEF_ERR, nodeType, __LINE__);
		//throw except;
		return NULL;
	      }
	    }
	  if(currentChild == NULL)
	    {
	      thisNode->firstChild = newChild;
	    }
	  else
	    {
	      currentChild->nextSibling = newChild;
	      newChild->previousSibling = currentChild;
	    }
	  currentChild = newChild;
	}
    }

  if(has_constraints)
    {
      newChild = constraints->makeBasicTree(thisNode);   
      if(currentChild == NULL)
	{
	  thisNode->firstChild = newChild;
	}
      else
	{
	  currentChild->nextSibling = newChild;
	  newChild->previousSibling = currentChild;
	}
      currentChild = newChild;      
    }
  return thisNode;
}

int PunionS::canAddChildTypes(int* typeList, int max)
{
  if(max < 4)
    return -1;
  int i = 0;
  typeList[i] = PT_FIELD;
  i++;
  typeList[i] = PT_LITERAL;
  i++;
  if(which == FIELDLITERAL)
    {
      typeList[i] = PT_PSWITCH;
      i++;
    }
  if(!has_constraints)
    {
      typeList[i] = PT_CONSTRAINTS2;
      i++;
    }
  return i;
}

int PunionS::removeChildNode(PNodeP* removeThis)
{
  int vectorSize;
  int i, j;
  PNodeP* thisPNode;

  if(removeThis == NULL)
    return -1;

  if(which == FIELDLITERAL)
    {
      field_literalT this_field_literal;
      vectorSize = field_literal.size();
      for(i = 0; i < vectorSize; i++)
	{
	  this_field_literal = field_literal[i];
	  if(this_field_literal->which == this_field_literal->FIELD)
	    thisPNode = this_field_literal->field_literal.field;
	  else if(this_field_literal->which == this_field_literal->LITERAL)
	    thisPNode = this_field_literal->field_literal.literal;
	  else
	    thisPNode = NULL;
	  if(thisPNode == removeThis)
	    {
	      delete this_field_literal;
	      vectorSize--;
	      for(j = i; j < vectorSize; j++)
		{
		  field_literal[j] = field_literal[j+1];
		}
	      field_literal.pop_back();
	      return 0;
	    }
	}
    }

  if(removeThis == constraints && has_constraints)
    {
      has_constraints = false;
      constraints = NULL;
      return 0;
    }

  return -1;
}

int PunionS::makeCompleteNode()
{
  decl = new declS();
  if(modifiers->makeCompleteNode() != 0)
    return -1;
  if(decl->makeCompleteNode() != 0)
    return -1;
  return 0;
}

int PunionS::insertIntoNameMap(PADSTypeMap* nameMap)
{
  if(which == FIELDLITERAL)
    {
      field_literalT this_field_literal;
      for(int i = 0; i < field_literal.size(); i++)
	{
	  this_field_literal = field_literal[i];
	  if(this_field_literal->which == this_field_literal->FIELD)
	    this_field_literal->field_literal.field->insertIntoNameMap(nameMap);
	}
    }

  PSTRING nameStr;
  getNameStr(nameStr);
  return nameMap->insert(nameStr, this);
}

int PunionS::insertIntoPathMap(PADSTypeMap* pathMap, PSTRING& pathStr)
{
  PSTRING nameStr;
  PSTRING fullPath;
  getNameStr(nameStr);
  fullPath = pathStr;
  fullPath.append(":");
  fullPath.append(nameStr);

  if(which == FIELDLITERAL)
    {
      field_literalT this_field_literal;
      for(int i = 0; i < field_literal.size(); i++)
	{
	  this_field_literal = field_literal[i];
	  if(this_field_literal->which == this_field_literal->FIELD)
	    this_field_literal->field_literal.field->insertIntoPathMap(pathMap, fullPath);
	}
    }

  return pathMap->insert(fullPath, this);
}

PADSUniTreeNode* PunionS::makeUnifiedTree(PADSUniTreeNode* parent, PSTRING& varStr, PADSTypeMap* typeMap)
{
  DB_P("making composite node::%s\n", PT_TypeNames[nodeType]);
  PADSUniTreeNode* thisNode = new PADSUniTreeNode(this, varStr, parent);

  PADSUniTreeNode *newChild = NULL, *currentChild = NULL;

  PSTRING childNameStr = "";
  PSTRING childTypeStr = "";
  field_literalT this_field_literal;
  PNodeP* childNode;
  if(which == FIELDLITERAL)
    {
      for(int i = 0; i < field_literal.size(); i++)
	{
	  childNameStr = "";
	  this_field_literal = field_literal[i];
	  if(this_field_literal->which == this_field_literal->FIELD)
	    {
	      this_field_literal->field_literal.field->getTypeStr(childTypeStr);
	      this_field_literal->field_literal.field->getNameStr(childNameStr);
	      childNode = typeMap->find(childTypeStr);
	      DB_P("  PStruct child => %s::%s\n", childNameStr.c_str(), childTypeStr.c_str());
	      DB_P("    child lookup val => %d\n", childNode);
	      if(childNode != NULL)
		{
		  DB_P("## calling makeUnifiedTree on child node\n");
		  newChild = childNode->makeUnifiedTree(thisNode, childNameStr, typeMap);
		  if(newChild == NULL)
		    {
		      newChild = this_field_literal->field_literal.field->makeUnifiedTree(thisNode, childNameStr, typeMap);
		      if(currentChild == NULL)
			{
			  thisNode->firstChild = newChild;
			}
		      else
			{
			  currentChild->nextSibling = newChild;
			  newChild->previousSibling = currentChild;
			}
		      currentChild = newChild;
		    }
		  if(newChild->getLinkNode()->nodeType = PT_PTYPEDEF)
		    {
		      newChild->setLinkNode(this_field_literal->field_literal.field);
		    }
		  
		  DB_P("## finished call of makeUnifiedTree on child node\n");
		  if(currentChild == NULL)
		    {
		      thisNode->firstChild = newChild;
		    }
		  else
		    {
		      currentChild->nextSibling = newChild;
		      newChild->previousSibling = currentChild;
		    }
		  currentChild = newChild;
		}
	      else
		{
		  newChild = this_field_literal->field_literal.field->makeUnifiedTree(thisNode, childNameStr, typeMap);
		  if(currentChild == NULL)
		    {
		      thisNode->firstChild = newChild;
		    }
		  else
		    {
		      currentChild->nextSibling = newChild;
		      newChild->previousSibling = currentChild;
		    }
		  currentChild = newChild;
		}
	    }
	}
    }
  DB_P("@@Returning from Punion (%s) tree unification\n", varStr.c_str());
  return thisNode;
}

// new_methods_def_point

// **********

Pads::Pads() : PNodeP(PT_PADS)
{

}

PNodeP* Pads::addChild(PNodeP* newChild)
{
  PADStypeT pt = new PADStypeS();
  switch(newChild->nodeType)
    {
    case PT_PALTERNATES:
      pt->which = pt->PALTERNATES;
      pt->PADStype.Palternates = (PalternatesT)newChild;
      PADStype.push_back(pt);
      break;
    case PT_PARRAY:
      pt->which = pt->PARRAY;
      pt->PADStype.Parray = (ParrayT)newChild;
      PADStype.push_back(pt);
      break;
    case PT_PCHARCLASS:
      pt->which = pt->PCHARCLASS;
      pt->PADStype.Pcharclass = (PcharclassT)newChild;
      PADStype.push_back(pt);
      break;
    case PT_PENUM:
      pt->which = pt->PENUM;
      pt->PADStype.Penum = (PenumT)newChild;
      PADStype.push_back(pt);
      break;
    case PT_FUNCTION:
      pt->which = pt->FUNCTION;
      pt->PADStype.function = (functionT)newChild;
      PADStype.push_back(pt);
      break;
    case PT_PRECURSIVE:
      pt->which = pt->PRECURSIVE;
      pt->PADStype.Precursive = (PrecursiveT)newChild;
      PADStype.push_back(pt);
      break;
    case PT_PSELECT:
      pt->which = pt->PSELECT;
      pt->PADStype.Pselect = (PselectT)newChild;
      PADStype.push_back(pt);
      break;
    case PT_PSTRUCT:
      pt->which = pt->PSTRUCT;
      pt->PADStype.Pstruct = (PstructT)newChild;
      PADStype.push_back(pt);
      break;
    case PT_PTYPEDEF:
      pt->which = pt->PTYPEDEF;
      pt->PADStype.Ptypedef = (PtypedefT)newChild;
      PADStype.push_back(pt);
      break;
    case PT_PUNION:
      pt->which = pt->PUNION;
      pt->PADStype.Punion = (PunionT)newChild;
      PADStype.push_back(pt);
      break;
    default:
      {
	delete pt;
	throw PTException(PTE_CHILD_TYPE_ERR, nodeType, __LINE__);
	//throw except;
	return NULL;
      }
    }
  newChild->parent = (PNodeP*)this;
  return this;
}
 
void Pads::serializeXMLContents(int spaceDepth, PSTRING& printStr)
{
  int size = PADStype.size();
  PADStypeT this_PADStype;
  for(int i = 0; i < size; i++)
    {
      this_PADStype = PADStype.at(i);
      switch(this_PADStype->which)
	{
	case this_PADStype->PALTERNATES:
	  this_PADStype->PADStype.Palternates->serializeXMLRepresentation(spaceDepth + 1, printStr);
	  break;
	case this_PADStype->PARRAY:
	  this_PADStype->PADStype.Parray->serializeXMLRepresentation(spaceDepth + 1, printStr);
	  break;
	case this_PADStype->PCHARCLASS:
	  this_PADStype->PADStype.Pcharclass->serializeXMLRepresentation(spaceDepth + 1, printStr);
	  break;
	case this_PADStype->PENUM:
	  this_PADStype->PADStype.Penum->serializeXMLRepresentation(spaceDepth + 1, printStr);
	  break;
	case this_PADStype->FUNCTION:
	  this_PADStype->PADStype.function->serializeXMLRepresentation(spaceDepth + 1, printStr);
	  break;
	case this_PADStype->PRECURSIVE: 
	  this_PADStype->PADStype.Precursive->serializeXMLRepresentation(spaceDepth + 1, printStr);
	  break;
	case this_PADStype->PSELECT:
	  this_PADStype->PADStype.Pselect->serializeXMLRepresentation(spaceDepth + 1, printStr);
	  break;
	case this_PADStype->PSTRUCT:
	  this_PADStype->PADStype.Pstruct->serializeXMLRepresentation(spaceDepth + 1, printStr);
	  break;
	case this_PADStype->PTYPEDEF:
	  this_PADStype->PADStype.Ptypedef->serializeXMLRepresentation(spaceDepth + 1, printStr);
	  break;
	case this_PADStype->PUNION:
	  this_PADStype->PADStype.Punion->serializeXMLRepresentation(spaceDepth + 1, printStr);
	  break;
	default:
	  {
	    throw PTException(PTE_CHILD_UNDEF_ERR, nodeType, __LINE__);
	    //throw except;
	  }
	}
    }
}

void Pads::codifyRepresentation(int spaceDepth, PSTRING& printStr)
{
  int size = PADStype.size();
  PADStypeT this_PADStype;
  for(int i = 0; i < size; i++)
    {
      this_PADStype = PADStype.at(i);
      switch(this_PADStype->which)
	{
	case this_PADStype->PALTERNATES:
	  this_PADStype->PADStype.Palternates->codifyRepresentation(spaceDepth, printStr);
	  break;
	case this_PADStype->PARRAY:
	  this_PADStype->PADStype.Parray->codifyRepresentation(spaceDepth, printStr);
	  break;
	case this_PADStype->PCHARCLASS:
	  this_PADStype->PADStype.Pcharclass->codifyRepresentation(spaceDepth, printStr);
	  break;
	case this_PADStype->PENUM:
	  this_PADStype->PADStype.Penum->codifyRepresentation(spaceDepth, printStr);
	  break;
	case this_PADStype->FUNCTION:
	  this_PADStype->PADStype.function->codifyRepresentation(spaceDepth, printStr);
	  break;
	case this_PADStype->PRECURSIVE: 
	  this_PADStype->PADStype.Precursive->codifyRepresentation(spaceDepth, printStr);
	  break;
	case this_PADStype->PSELECT:
	  this_PADStype->PADStype.Pselect->codifyRepresentation(spaceDepth, printStr);
	  break;
	case this_PADStype->PSTRUCT:
	  this_PADStype->PADStype.Pstruct->codifyRepresentation(spaceDepth, printStr);
	  break;
	case this_PADStype->PTYPEDEF:
	  this_PADStype->PADStype.Ptypedef->codifyRepresentation(spaceDepth, printStr);
	  break;
	case this_PADStype->PUNION:
	  this_PADStype->PADStype.Punion->codifyRepresentation(spaceDepth, printStr);
	  break;
	default:
	  {
	    throw PTException(PTE_CHILD_UNDEF_ERR, nodeType, __LINE__);
	    //throw except;
	  }
	}
      DB_P("\n");
      printStr.append("\n");
    }
}

bool Pads::canChangeToType(int newType)
{
  switch(newType)
    {
    default:
      break;
    }
  return false;
}

PNodeP* Pads::convertAndCopy(int newType, PNodeP* copyTo)
{
  return NULL;
}

PADSPNodeTreeNode* Pads::makeBasicTree(PADSPNodeTreeNode* parent)
{
  DB_P("making composite node::%s\n", PT_TypeNames[nodeType]);
  PADSPNodeTreeNode* thisNode = new PADSPNodeTreeNode(this, parent);    

  PADSPNodeTreeNode *newChild = NULL, *currentChild = NULL;
  PADStypeT this_PADStype = NULL;
  int vectorSize = PADStype.size();
  for(int i = 0; i < vectorSize; i++)
    {
      this_PADStype = PADStype.at(i);
      switch(this_PADStype->which)
	{
	case this_PADStype->PALTERNATES:
	  newChild = this_PADStype->PADStype.Palternates->makeBasicTree(thisNode);
	  break;
	case this_PADStype->PARRAY:
	  newChild = this_PADStype->PADStype.Parray->makeBasicTree(thisNode);
	  break;
	case this_PADStype->PCHARCLASS:
	  newChild = this_PADStype->PADStype.Pcharclass->makeBasicTree(thisNode);
	  break;
	case this_PADStype->PENUM:
	  newChild = this_PADStype->PADStype.Penum->makeBasicTree(thisNode);
	  break;
	case this_PADStype->FUNCTION:
	  newChild = this_PADStype->PADStype.function->makeBasicTree(thisNode);
	  break;
	case this_PADStype->PRECURSIVE: 
	  newChild = this_PADStype->PADStype.Precursive->makeBasicTree(thisNode);
	  break;
	case this_PADStype->PSELECT:
	  newChild = this_PADStype->PADStype.Pselect->makeBasicTree(thisNode);
	  break;
	case this_PADStype->PSTRUCT:
	  newChild = this_PADStype->PADStype.Pstruct->makeBasicTree(thisNode);
	  break;
	case this_PADStype->PTYPEDEF:
	  newChild = this_PADStype->PADStype.Ptypedef->makeBasicTree(thisNode);
	  break;
	case this_PADStype->PUNION:
	  newChild = this_PADStype->PADStype.Punion->makeBasicTree(thisNode);
	  break;
	default:
	  {
	    throw PTException(PTE_CHILD_UNDEF_ERR, nodeType, __LINE__);
	    //throw except;
	    return thisNode;
	  }
	}
      if(currentChild == NULL)
	{
	  thisNode->firstChild = newChild;
	}
      else
	{
	  currentChild->nextSibling = newChild;
	  newChild->previousSibling = currentChild;
	}
      currentChild = newChild;
    }
  return thisNode;
}

int Pads::canAddChildTypes(int* typeList, int max)
{
  if(max < 10)
    return -1;
  typeList[0] = PT_PALTERNATES;
  typeList[1] = PT_PARRAY;
  typeList[2] = PT_PCHARCLASS;
  typeList[3] = PT_PENUM;
  typeList[4] = PT_FUNCTION;
  typeList[5] = PT_PRECURSIVE;
  typeList[6] = PT_PSELECT;
  typeList[7] = PT_PSTRUCT;
  typeList[8] = PT_PTYPEDEF;
  typeList[9] = PT_PUNION;
  return 10;
}

int Pads::removeChildNode(PNodeP* removeThis)
{
  int vectorSize;
  int i, j;
  PNodeP* thisPNode;

  if(removeThis == NULL)
    return -1;

  PADStypeT this_PADStype;
  vectorSize = PADStype.size();
  for(i = 0; i < vectorSize; i++)
    {
      this_PADStype = PADStype[i];
      switch(this_PADStype->which)
	{
	case this_PADStype->PALTERNATES:
	  thisPNode = this_PADStype->PADStype.Palternates;
	  break;
	case this_PADStype->PARRAY:
	  thisPNode = this_PADStype->PADStype.Parray;
	  break;
	case this_PADStype->PCHARCLASS:
	  thisPNode = this_PADStype->PADStype.Pcharclass;
	  break;
	case this_PADStype->PENUM:
	  thisPNode = this_PADStype->PADStype.Penum;
	  break;
	case this_PADStype->FUNCTION:
	  thisPNode = this_PADStype->PADStype.function;
	  break;
	case this_PADStype->PRECURSIVE:
	  thisPNode = this_PADStype->PADStype.Precursive;
	  break;
	case this_PADStype->PSELECT:
	  thisPNode = this_PADStype->PADStype.Pselect;
	  break;
	case this_PADStype->PSTRUCT:
	  thisPNode = this_PADStype->PADStype.Pstruct;
	  break;
	case this_PADStype->PTYPEDEF:
	  thisPNode = this_PADStype->PADStype.Ptypedef;
	  break;
	case this_PADStype->PUNION:
	  thisPNode = this_PADStype->PADStype.Punion;
	  break;
	default:
	  thisPNode = NULL;
	  break;
	}

      if(thisPNode == removeThis)
	{
	  delete this_PADStype;
	  vectorSize--;
	  for(j = i; j < vectorSize; j++)
	    {
	      PADStype[j] = PADStype[j+1];
	    }
	  PADStype.pop_back();
	  return 0;
	}
    }

  return -1;
}

int Pads::insertIntoNameMap(PADSTypeMap* nameMap)
{
  int vectorSize = PADStype.size();
  PADStypeT this_PADStype;
  for(int i = 0; i < vectorSize; i++)
    {
      this_PADStype = PADStype[i];
      switch(this_PADStype->which)
	{
	case this_PADStype->PALTERNATES:
	  this_PADStype->PADStype.Palternates->insertIntoNameMap(nameMap);
	  break;
	case this_PADStype->PARRAY:
	  this_PADStype->PADStype.Parray->insertIntoNameMap(nameMap);
	  break;
	case this_PADStype->PCHARCLASS:
	  this_PADStype->PADStype.Pcharclass->insertIntoNameMap(nameMap);
	  break;
	case this_PADStype->PENUM:
	  this_PADStype->PADStype.Penum->insertIntoNameMap(nameMap);
	  break;
	case this_PADStype->FUNCTION:
	  this_PADStype->PADStype.function->insertIntoNameMap(nameMap);
	  break;
	case this_PADStype->PRECURSIVE:
	  this_PADStype->PADStype.Precursive->insertIntoNameMap(nameMap);
	  break;
	case this_PADStype->PSELECT:
	  this_PADStype->PADStype.Pselect->insertIntoNameMap(nameMap);
	  break;
	case this_PADStype->PSTRUCT:
	  this_PADStype->PADStype.Pstruct->insertIntoNameMap(nameMap);
	  break;
	case this_PADStype->PTYPEDEF:
	  this_PADStype->PADStype.Ptypedef->insertIntoNameMap(nameMap);
	  break;
	case this_PADStype->PUNION:
	  this_PADStype->PADStype.Punion->insertIntoNameMap(nameMap);
	  break;
	default:
	  break;
	}
    }
}

int Pads::insertIntoPathMap(PADSTypeMap* pathMap, PSTRING& pathStr)
{
  PSTRING fullPath = "";
  int vectorSize = PADStype.size();
  PADStypeT this_PADStype;
  for(int i = 0; i < vectorSize; i++)
    {
      this_PADStype = PADStype[i];
      switch(this_PADStype->which)
	{
	case this_PADStype->PALTERNATES:
	  this_PADStype->PADStype.Palternates->insertIntoPathMap(pathMap, fullPath);
	  break;
	case this_PADStype->PARRAY:
	  this_PADStype->PADStype.Parray->insertIntoPathMap(pathMap, fullPath);
	  break;
	case this_PADStype->PCHARCLASS:
	  this_PADStype->PADStype.Pcharclass->insertIntoPathMap(pathMap, fullPath);
	  break;
	case this_PADStype->PENUM:
	  this_PADStype->PADStype.Penum->insertIntoPathMap(pathMap, fullPath);
	  break;
	case this_PADStype->FUNCTION:
	  this_PADStype->PADStype.function->insertIntoPathMap(pathMap, fullPath);
	  break;
	case this_PADStype->PRECURSIVE:
	  this_PADStype->PADStype.Precursive->insertIntoPathMap(pathMap, fullPath);
	  break;
	case this_PADStype->PSELECT:
	  this_PADStype->PADStype.Pselect->insertIntoPathMap(pathMap, fullPath);
	  break;
	case this_PADStype->PSTRUCT:
	  this_PADStype->PADStype.Pstruct->insertIntoPathMap(pathMap, fullPath);
	  break;
	case this_PADStype->PTYPEDEF:
	  this_PADStype->PADStype.Ptypedef->insertIntoPathMap(pathMap, fullPath);
	  break;
	case this_PADStype->PUNION:
	  this_PADStype->PADStype.Punion->insertIntoPathMap(pathMap, fullPath);
	  break;
	default:
	  break;
	}
    }
}

int Pads::insertIntoTypeMap(PADSTypeMap* typeMap)
{
  int vectorSize = PADStype.size();
  PADStypeT this_PADStype;
  for(int i = 0; i < vectorSize; i++)
    {
      this_PADStype = PADStype[i];
      switch(this_PADStype->which)
	{
	case this_PADStype->PALTERNATES:
	  this_PADStype->PADStype.Palternates->insertIntoTypeMap(typeMap);
	  break;
	case this_PADStype->PARRAY:
	  this_PADStype->PADStype.Parray->insertIntoTypeMap(typeMap);
	  break;
	case this_PADStype->PCHARCLASS:
	  this_PADStype->PADStype.Pcharclass->insertIntoTypeMap(typeMap);
	  break;
	case this_PADStype->PENUM:
	  this_PADStype->PADStype.Penum->insertIntoTypeMap(typeMap);
	  break;
	case this_PADStype->FUNCTION:
	  this_PADStype->PADStype.function->insertIntoTypeMap(typeMap);
	  break;
	case this_PADStype->PRECURSIVE:
	  this_PADStype->PADStype.Precursive->insertIntoTypeMap(typeMap);
	  break;
	case this_PADStype->PSELECT:
	  this_PADStype->PADStype.Pselect->insertIntoTypeMap(typeMap);
	  break;
	case this_PADStype->PSTRUCT:
	  this_PADStype->PADStype.Pstruct->insertIntoTypeMap(typeMap);
	  break;
	case this_PADStype->PTYPEDEF:
	  this_PADStype->PADStype.Ptypedef->insertIntoTypeMap(typeMap);
	  break;
	case this_PADStype->PUNION:
	  this_PADStype->PADStype.Punion->insertIntoTypeMap(typeMap);
	  break;
	default:
	  break;
	}
    }
}

PADSUniTreeNode* Pads::makeUnifiedTree(PADSUniTreeNode* parent, PSTRING& varStr, PADSTypeMap* typeMap)
{
  DB_P("making composite node::%s\n", PT_TypeNames[nodeType]);
  PADSUniTreeNode* thisNode = parent;//new PADSUniTreeNode(this, parent);
  
  PADSUniTreeNode *newChild = NULL, *currentChild = NULL;
  PADStypeT this_PADStype = NULL;
  PSTRING childNameStr = "";
  int vectorSize = PADStype.size();
  for(int i = 0; i < vectorSize; i++)
    {
      this_PADStype = PADStype.at(i);
      DB_P("next child has type %d\n", this_PADStype->which);
      switch(this_PADStype->which)
	{
	case this_PADStype->PALTERNATES:
	  //newChild = this_PADStype->PADStype.Palternates->makeUnifiedTree(thisNode, typeMap);
	  break;
	case this_PADStype->PARRAY:
	  if(this_PADStype->PADStype.Parray->modifiers->has_Psource)
	    {
	      DB_P("recurring from makeUnifiedTree on type Parray\n");
	      this_PADStype->PADStype.Parray->getNameStr(childNameStr);
	      newChild = this_PADStype->PADStype.Parray->makeUnifiedTree(thisNode, childNameStr, typeMap);
	    }
	  break;
	case this_PADStype->PCHARCLASS:
	  break;
	case this_PADStype->PENUM:
	  if(this_PADStype->PADStype.Penum->modifiers->has_Psource)
	    {
	      DB_P("recurring from makeUnifiedTree on type Penum\n");
	      this_PADStype->PADStype.Penum->getNameStr(childNameStr);
	      newChild = this_PADStype->PADStype.Penum->makeUnifiedTree(thisNode, childNameStr, typeMap);
	    }
	  break;
	case this_PADStype->FUNCTION:
	  break;
	case this_PADStype->PRECURSIVE: 
	  if(this_PADStype->PADStype.Precursive->modifiers->has_Psource)
	    {
	      DB_P("recurring from makeUnifiedTree on type Precursive\n");
	      this_PADStype->PADStype.Precursive->getNameStr(childNameStr);
	      newChild = this_PADStype->PADStype.Precursive->makeUnifiedTree(thisNode, childNameStr, typeMap);
	    }
	  break;
	case this_PADStype->PSELECT:
	  break;
	case this_PADStype->PSTRUCT:
	  if(this_PADStype->PADStype.Pstruct->modifiers->has_Psource)
	    {
	      DB_P("recurring from makeUnifiedTree on type Pstruct\n");
	      this_PADStype->PADStype.Pstruct->getNameStr(childNameStr);
	      newChild = this_PADStype->PADStype.Pstruct->makeUnifiedTree(thisNode, childNameStr, typeMap);
	    }
	  break;
	case this_PADStype->PTYPEDEF:
	  break;
	case this_PADStype->PUNION:
	  if(this_PADStype->PADStype.Punion->modifiers->has_Psource)
	    {
	      DB_P("recurring from makeUnifiedTree on type Punion\n");
	      this_PADStype->PADStype.Punion->getNameStr(childNameStr);
	      newChild = this_PADStype->PADStype.Punion->makeUnifiedTree(thisNode, childNameStr, typeMap);
	    }
	  break;
	default:
	  {
	    DB_P("Pads type throwing exception in makeUnifiedTree!\n");
	    throw PTException(PTE_CHILD_UNDEF_ERR, nodeType, __LINE__);
	    //throw except;
	    return thisNode;
	  }
	}
      if(newChild != NULL)
	return newChild;
    }
  return NULL;
}

// new_methods_def_point

// end PADS_lang.cpp
// *****************************************************************************

