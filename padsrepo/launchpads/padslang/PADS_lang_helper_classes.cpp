/* *****************************************************************************
 * Mark Daly
 * June 2006
 * PADS_lang_helper_classes - various classes to build an AST from the wxWidgets
 *  control and to create a path-map to each element in a PADS AST
 * ************************************************************************** */

#include "PADS_lang_helper_classes.h"
#include "../lp_PADS_constants.h"
#include "PADS_lang_factory.h"

using namespace std;

PADSGridNode::PADSGridNode()
{
   nodeType = PT_UNDEF;
   left  = -1;
   right = -1;
}

PADSGridNode::PADSGridNode(int type, int left_limit_inc, int right_limit_exc)
{
   nodeType = type;
   left = left_limit_inc;
   right = right_limit_exc;
}

PADSGridNode::~PADSGridNode()
{
   int size = children.size();
   PADSGridNode* child;
   for(int i = 0; i < size; i++)
   {
      child = children.at(i);
      if(child != NULL)
         delete child;
   }
}

PADSGridNode* PADSGridNode::addNewChildNode(PADSGridNode* newChild)
{
   children.push_back(newChild);
   return newChild;
}

PADSGridNode* PADSGridNode::makeAndAddNewChildNode(int type, int left_limit_inc, int right_limit_exc)
{
   PADSGridNode* newChild = new PADSGridNode(type, left_limit_inc, right_limit_exc);
   children.push_back(newChild);
   return newChild;
}

void PADSGridNode::getTextRepresentation(PSTRING& printStr)
{
  printStr.append(depth * PADS_GRID_NODE_ADD_LEVEL_SPACE, ' ');
  char tmpStr[64];
  DB_P("[%d] @ %d - %d # %d\n", nodeType, left, right, depth);
  sprintf(tmpStr, "[%d] @ %d - %d # %d\n", nodeType, left, right, depth);
  printStr.append(tmpStr);
  int vectorSize = children.size();
  for(int i = 0; i < vectorSize; i++)
    {
      children[i]->getTextRepresentation(printStr);
    }
}

void PADSGridNode::setDepth(int newDepth)
{
  depth = newDepth;
}

int PADSGridNode::getDepth()
{
  return depth;
}

int PADSGridNode::makeTypeNameFromPElementType(int oldType, PSTRING& typeStr)
{
  typeStr = "";
  switch(oldType)
    {
    case pChar:
      typeStr = PADS_formalized_names[oldType];
      return PT_FIELD;
      break;
    case pInt: 
      typeStr = PADS_formalized_names[oldType];
      typeStr.append("32");
      return PT_FIELD;
      break;
    case pFloat:
      typeStr = PADS_formalized_names[oldType];
      typeStr.append("32");
      return PT_FIELD;
      break;
    case pFpoint:
      typeStr = PADS_formalized_names[oldType];
      typeStr.append("32");
      return PT_FIELD;
      break;
    case pB:
      typeStr = PADS_formalized_names[oldType];
      typeStr.append("_int32");
      return PT_FIELD;
      break;
    case pString:
      typeStr = PADS_formalized_names[oldType];
      return PT_FIELD;
      break;
    case pCountX:
      typeStr = PADS_formalized_names[oldType];
      return PT_FIELD;
      break;
    case pCountXtoY:
      typeStr = PADS_formalized_names[oldType];
      return PT_FIELD;
      break;
    case pTimestamp_explicit:
      typeStr = PADS_formalized_names[oldType];
      return PT_FIELD;
      break;
    case pTimestamp:
      typeStr = PADS_formalized_names[oldType];
      return PT_FIELD;
      break;
    case pDate_explicit:
      typeStr = PADS_formalized_names[oldType];
      return PT_FIELD;
      break;
    case pDate:
      typeStr = PADS_formalized_names[oldType];
      return PT_FIELD;
      break;
    case pTime_explicit:
      typeStr = PADS_formalized_names[oldType];
      return PT_FIELD;
      break;
    case pTime:
      typeStr = PADS_formalized_names[oldType];
      return PT_FIELD;
      break;
    case pIp:
      typeStr = PADS_formalized_names[oldType];
      return PT_FIELD;
      break;
    case pEnumField:
      return PT_ENUMFIELD;
      break;
    case pCase:
      return PT_PCASE;
      break;
    case pFunction:
      return PT_FUNCTION;
      break;
    case pLit:
      return PT_LITERAL;
      break;
    case pStruct:
      typeStr = PADS_formalized_names[oldType];
      return -1;
      break;
    case pUnion: 
      typeStr = PADS_formalized_names[oldType];
      return -1;
      break;
    case pArray:
      typeStr = PADS_formalized_names[oldType];
      return -1;
      break;
    case pEnum:
      typeStr = PADS_formalized_names[oldType];
      return -1;
      break;
    default:
      return PT_UNDEF;
      break;
    }
  
}

int PADSGridNode::convertPElementTypeToPADSLangType(int oldType)
{
  switch(oldType)
    {
    case pChar:
    case pInt: 
    case pFloat:
    case pFpoint:
    case pB:
    case pString:
    case pCountX:
    case pCountXtoY:
    case pTimestamp_explicit:
    case pTimestamp:
    case pDate_explicit:
    case pDate:
    case pTime_explicit:
    case pTime:
    case pIp:
      return PT_FIELD;
      break;
    case pEnumField:
      return PT_ENUMFIELD;
      break;
    case pCase:
      return PT_PCASE;
      break;
    case pFunction:
      return PT_FUNCTION;
      break;
    case pLit:
      return PT_LITERAL;
      break;
    case pStruct:
      return PT_PSTRUCT;
      break;
    case pUnion: 
      return PT_PUNION;
      break;
    case pArray:
      return PT_PARRAY;
      break;
    case pOpt:
      return PT_POPT;
      break;
    case pEnum:
      return PT_PENUM;
      break;
    case pSwitch:
      return PT_PSWITCH;
      break;
    default:
      return PT_UNDEF;
      break;
    }
  return PT_UNDEF;
}

PNodeP* PADSGridNode::makeCorrectNewNodeFromTypes(PNodeP* rootNode, PNodeP* parentNode, int newParentType, int oldChildType, int& varCount, int left, int right, PSTRING& gridString)
{
  DB_P("makeCorrectNewNodeFromTypes called from %d to %d\n", newParentType, oldChildType);
  //int newParentType = convertPElementTypeToPADSLangType(oldParentType);
  int newChildType = convertPElementTypeToPADSLangType(oldChildType);

  PNodeP* newNode;
  PNodeFactory pFactory;

  char sprintBuf[32];
  sprintf(sprintBuf, "var_%d_%d_%d", varCount, left, right);
  varCount++;
  PSTRING str;
  PSTRING varNameStr;
  PSTRING typeNameStr;
  varNameStr = sprintBuf;
  if(makeTypeNameFromPElementType(oldChildType, typeNameStr) == -1)
    {
      typeNameStr = varNameStr;
      typeNameStr.append("_P");
    }

  DB_P("typename = %s ::: varname = %s\n", typeNameStr.c_str(), varNameStr.c_str());
  DB_P("rootNode == %s @ %d, parentNode == %d, newParentType = %d %s\n", 
       PT_TypeNames[rootNode->nodeType], rootNode, parentNode, newParentType, PT_TypeNames[newParentType]);
  if(parentNode != NULL)
    {
      switch(newParentType) // this only supports the five composite types used in the LP interface
	{
	case PT_PSTRUCT:
	  {
	    fieldT newFieldNode;
	    newFieldNode = (fieldT)pFactory.makeEmptyPNodeFromType(PT_FIELD);
	    newFieldNode->makeCompleteNode();
	    newFieldNode->name->addValue(varNameStr);
	    newFieldNode->ptype->name->addValue(typeNameStr);
	    newNode = newFieldNode;
	    try
	      {
		parentNode->addChild(newNode);
	      }
	    catch(...)
	      {
		DB_P("parent threw exception in makeCorrectNewNodeFromTypes=> %s -> %s\n", 
		     PT_TypeNames[parentNode->nodeType], PT_TypeNames[newChildType]);
		     
	      }
	  }
	    break;
	  case PT_PUNION:
	    {
	      fieldT newFieldNode;
	      newFieldNode = (fieldT)pFactory.makeEmptyPNodeFromType(PT_FIELD);
	      newFieldNode->makeCompleteNode();
	      newFieldNode->name->addValue(varNameStr);
	      newFieldNode->ptype->name->addValue(typeNameStr);
	      newNode = newFieldNode;
	      try
		{
		  parentNode->addChild(newNode);
		}
	      catch(...)
		{
		  DB_P("parent threw exception in makeCorrectNewNodeFromTypes=> %s -> %s\n", 
		       PT_TypeNames[parentNode->nodeType], PT_TypeNames[newChildType]);
		  
		}
	    }
	    break;
	  case PT_PARRAY:
	    {
	      ((ParrayT)parentNode)->ptype->name->addValue(typeNameStr);
	    }
	    break;
	  case PT_PENUM:
	    {
	      enumFieldT newFieldNode;
	      if(newChildType != PT_ENUMFIELD)
		{
		  DB_P("ERROR: trying to add child of type %s to PT_ENUM parent\n",
		       PT_TypeNames[newChildType]);
		}

	      newFieldNode = (enumFieldT)pFactory.makeEmptyPNodeFromType(PT_ENUMFIELD);
	      newFieldNode->makeCompleteNode();
	      varNameStr = gridString.substr(left, right-left);
	      newFieldNode->name->addValue(varNameStr);
	      newNode = newFieldNode;
	      try
		{
		  parentNode->addChild(newNode);
		}
	      catch(...)
		{
		  DB_P("parent threw exception in makeCorrectNewNodeFromTypes=> %s -> %s\n", 
		       PT_TypeNames[parentNode->nodeType], PT_TypeNames[newChildType]);
		  
		}
	    }
	    break;
	  default:
	    break;
	  
	}
    }

  // if this grid tree node can contain child nodes, make new parent-capable nodes
  switch(newChildType)
    {
    case PT_PSTRUCT:
      newNode = pFactory.makeEmptyPNodeFromType(newChildType);
      newNode->makeCompleteNode();
      DB_P("got newNode w/ type %s [%d]\n", PT_TypeNames[newNode->nodeType], newNode->nodeType);
      ((PstructT)newNode)->decl->name->addValue(typeNameStr);
      DB_P("typename added to decl->name\n");
      rootNode->addChild(newNode);
      return newNode;
      break;
    case PT_PUNION:
      newNode = pFactory.makeEmptyPNodeFromType(newChildType);
      newNode->makeCompleteNode();
      ((PunionT)newNode)->decl->name->addValue(typeNameStr);
      ((PunionT)newNode)->which = ((PunionT)newNode)->FIELDLITERAL;
      rootNode->addChild(newNode);
      return newNode;
      break;
    case PT_PARRAY:
      newNode = pFactory.makeEmptyPNodeFromType(newChildType);
      newNode->makeCompleteNode();
      ((ParrayT)newNode)->decl->name->addValue(typeNameStr);
      rootNode->addChild(newNode);
      return newNode;
      break;
    case PT_PENUM:
      newNode = pFactory.makeEmptyPNodeFromType(newChildType);
      newNode->makeCompleteNode();
      ((PenumT)newNode)->decl->name->addValue(typeNameStr);
      rootNode->addChild(newNode);
      return newNode;
      break;
    }
  if(parentNode == NULL)
    DB_P("**error:  parent node = NULL for child node of type %s\n", PADS_formalized_names[oldChildType]);

  return NULL;
}


void PADSGridNode::getPElementTypeAttritbutes(int type, 
					      bool &sign,
					      bool &enctype,
					      bool &bytes, 
					      bool &termtype)
{

  if(type < pChar || type >= pEndTerminals)
    {
      sign = false;
      enctype = false;
      bytes = false;
      termtype = false;
      return;
    }

  switch(type)
    {
    case pChar:
      sign = false;
      enctype = true;
      bytes = false;
      termtype = false;
      break;
    case pInt:
      sign = true;
      enctype = true;
      bytes = true;
      termtype = true;
      break;
    case pFloat:
      sign = false;
      enctype = true;
      bytes = true;
      termtype = false;
      break;
    case pFpoint:
      sign = true;
      enctype = true;
      bytes = true;
      termtype = false;
      break;
    case pB:
      sign = true;
      enctype = false;
      bytes = true;
      termtype = false;
      break;
    case pString:
      sign = false;
      enctype = true;
      bytes = false;
      termtype = true;
      break;
    case pTimestamp_explicit:
    case pTimestamp:
    case pDate_explicit:
    case pDate:
    case pTime_explicit:
    case pTime:
      sign = false;
      enctype = true;
      bytes = false;
      termtype = true;
      break;
    case pCountX:
    case pCountXtoY:
      sign = false;
      enctype = true;
      bytes = false;
      termtype = false;
      break;
    case pIp:
      sign = false;
      enctype = true;
      bytes = false;
      termtype = false;
      break;
    case pEnumField:
    case pCase:
    case pFunction:
    case pLit:
    case pUserDef: 
    case pTypeDefault: 
      sign = false;
      enctype = false;
      bytes = false;
      termtype = false;
      break;
    }

  return;
}

PNodeP* PADSGridNode::convertGridTreeToAST(PNodeP* PADSroot, PSTRING& gridString)
{
  int varCount = 0;
  PNodeP* rootStruct;
  PNodeFactory pFactory;
  rootStruct = makeCorrectNewNodeFromTypes(PADSroot, NULL, PT_PADS, pStruct, varCount, left, right, gridString);
  DB_P("first grid node with type %d\n", nodeType);
  DB_P("created new root struct with type %s\n", PT_TypeNames[rootStruct->nodeType]);
  ((PstructT)rootStruct)->modifiers = (modifiersT)pFactory.makeEmptyPNodeFromType(PT_MODIFIERS);
  ((PstructT)rootStruct)->modifiers->addChild(pFactory.makeEmptyPNodeFromType(PT_PSOURCE));
  DB_P("added mofifiers to struct with type %s\n", PT_TypeNames[((PstructT)rootStruct)->modifiers->nodeType]);
  for(int i = 0; i < children.size(); i++)
    {
      if(children[i] != NULL)
	children[i]->convertGridTreeToASTRecur(PADSroot, rootStruct, varCount, gridString);
      else
	DB_P("**error in conversion root:  child = NULL\n");
    }
  return PADSroot;
}

PNodeP* PADSGridNode::convertGridTreeToASTRecur(PNodeP* PADSroot, PNodeP* parentNode, int& varCount, PSTRING& gridString)
{
  int parentType; 
  if(parentNode == NULL)
    parentType = PT_UNDEF;
  else
    parentType = parentNode->nodeType;
  DB_P("this node data = %d %d %d %d\n", nodeType, left, right, depth);
  DB_P("recurring on new node of type %s\n", PT_TypeNames[convertPElementTypeToPADSLangType(nodeType)]);
  PNodeP* newNode = makeCorrectNewNodeFromTypes(PADSroot, parentNode, parentType, nodeType, varCount, left, right, gridString);
  if(newNode == NULL)
    {
      if(children.size() != 0)
	DB_P("***error: grid node %d @ %d-%d by %d got NULL new node but has children to parse!\n",
	     nodeType, left, right, depth);
      return NULL;
    }
  else
    {
      for(int i = 0; i < children.size(); i++)
	{
	  if(children[i] != NULL)
	    children[i]->convertGridTreeToASTRecur(PADSroot, newNode, varCount, gridString);
	  else
	    DB_P("**error in conversion recurrance:  child = NULL\n");
	}
    }
  return NULL;
}

/*
PADSGridNode* PADSGridNode::buildChildTree(int* grid[], int depth)
{
   int i, old_type, old_left;
   PADSGridNode* newChild;

   old_type = grid[depth][left];
   old_left = left;

   for(i = left; i < right; i++)
   {
      if(grid[depth][i] != old_type)
      {
         newChild = new PADSGridNode(old_type, old_left, i);
         if(!(PNodeP::isBaseType(old_type)))
         {
            newChild->buildChildTree(grid, depth+1);
         }
         old_type = grid[depth][i];
         old_left = i;
         children.push_back(newChild);
      }
   }
   return this;	
}
*/
// *********************************************

PADSMapElement::PADSMapElement(PSTRING newKey, PNodeP* newValue)
{
   key = newKey;
   value = newValue;
}

PADSMapElement::~PADSMapElement()
{
}


PADSTypeMap::PADSTypeMap()
{
}

PADSTypeMap::~PADSTypeMap()
{
   mapping.clear();
}

PADSMapElement* PADSTypeMap::findPair(PSTRING& key)
{
   if(key.compare("") == 0)
      return NULL;
   int i;
   PSTRING compStr = "";
   int size = mapping.size();
   PADSMapElement* thisPair;
   for(i = 0; i < size; i++)
   {
      thisPair = mapping.at(i);
      if(key.compare(thisPair->key) == 0)
         return thisPair;
   }
   return NULL;
}

PNodeP* PADSTypeMap::find(PSTRING& key)
{
   PADSMapElement* thisPair;
   thisPair = findPair(key);
   if(thisPair == NULL)
      return NULL;
   return thisPair->value;
}

int PADSTypeMap::findType(PSTRING& key)
{
   PADSMapElement* thisPair;
   thisPair = findPair(key);
   if(thisPair == NULL)
      return NULL;
   return thisPair->pairType;
}

PNodeP* PADSTypeMap::findNodeAndType(PSTRING& key, int *retType)
{
   PADSMapElement* thisPair;
   thisPair = findPair(key);
   if(thisPair == NULL)
     {
       *retType = PTMAP_NONE;
       return NULL;
     }
   *retType = thisPair->pairType;
   return thisPair->value;
}

PSTRING PADSTypeMap::splitStringAtSpace(PSTRING& key, PSTRING& retKey)
{
   int i, size;
   size = key.size();
   for(i = 0; i < size; i++)
   {
      if(key.at(i) == ' ')
         break;
   }
   retKey = key.substr(0, i);
   return retKey;
}

PNodeP* PADSTypeMap::findSplitSpace(PSTRING& key)
{
   PSTRING retKey;
   retKey = splitStringAtSpace(key, retKey);
   return find(retKey);
}

int PADSTypeMap::insert(PSTRING& key, PNodeP* value, int keyType)
{
   PADSMapElement* newElement;
   PNodeP* findKey;
   if(key.size() == 0)
     return -1;

   findKey = this->find(key);
   if(findKey != NULL)
     {
       if(value == findKey)
	 {
	   findKey->incRefCount();
	   return 0;
	 }
       else
	 {
	   return -1;
	 }
     }

   newElement = new PADSMapElement(key, value);
   newElement->pairType = keyType;
   mapping.push_back(newElement);
   return 0;
}

PNodeP* PADSTypeMap::remove(PSTRING& key)
{
   if(key.compare("") == 0)
      return NULL;
   int i;
   PSTRING compStr = "";
   int size = mapping.size();
   PADSMapElement* thisPair;
   PNodeP* retNode;
   for(i = 0; i < size; i++)
   {
      thisPair = mapping.at(i);
      if(key.compare(thisPair->key) == 0)
      {
         retNode = thisPair->value;
	 retNode->decRefCount();
	 if(retNode->getRefCount() == 0)
	 {
	   mapping.erase(mapping.begin()+i);
	 }
         return retNode;
      }
   }
   return NULL;
}

void PADSTypeMap::getTextualRepresentation(PSTRING& mapStr)
{
  PSTRING nameStr;
  int vectorSize = mapping.size();
  PADSMapElement* this_mapElement;
  char sbuf[1024];
  for(int i = 0; i < vectorSize; i++)
    {
      this_mapElement = mapping[i];
      DB_P("%s: %d [::%d] ", this_mapElement->key.c_str(), this_mapElement->value, this_mapElement->pairType);
      sprintf(sbuf, "%s: %d [::%d] ", this_mapElement->key.c_str(), this_mapElement->value, this_mapElement->pairType);
      mapStr.append(sbuf);

      if(this_mapElement->value != NULL)
	{
	  DB_P(" ::%s [%d]", PT_TypeNames[this_mapElement->value->nodeType], this_mapElement->value->nodeType);
	  sprintf(sbuf, " ::%s [%d]", PT_TypeNames[this_mapElement->value->nodeType], this_mapElement->value->nodeType);
	  mapStr.append(sbuf);
	}
      DB_P("\n");
      mapStr.append("\n");
    }
}

// *********************************************
