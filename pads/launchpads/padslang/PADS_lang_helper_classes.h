/* *****************************************************************************
 * Mark Daly
 * June 2006
 * PADS_lang_helper_classes - various classes to build an AST from the wxWidgets
 *  control and to create a path-map to each element in a PADS AST
 * ************************************************************************** */

#ifndef PADS_GRID_TRANS_H_INCLUDED
#define PADS_GRID_TRANS_H_INCLUDED

#include <string>
#include <stdlib.h>
#include <vector>

#include "PADS_lang.h"

#include "../lp_PADS_constants.h"
#include "PADS_lang_factory.h"

enum
  {
    PTMAP_NONE = 0,
    PTMAP_STD,
    PTMAP_STD_BASE,
    PTMAP_STD_COMP,
    PTMAP_UDEF,
  };

using namespace std;

#define PADS_GRID_NODE_ADD_LEVEL_SPACE 2

class PADSGridNode
{
 public:
  PADSGridNode();
  PADSGridNode(int type, int left_limit_inc, int right_limit_exc);
  
  ~PADSGridNode();
  
  PADSGridNode* addNewChildNode(PADSGridNode* newChild);
  PADSGridNode* makeAndAddNewChildNode(int type, int left_limit_inc, int right_limit_exc);
  
  void setDepth(int newDepth);
  int getDepth();
  
  int makeTypeNameFromPElementType(int oldType, PSTRING& typeStr);

  void getTextRepresentation(PSTRING& printStr);

  PNodeP* makeCorrectNewNodeFromTypes(PNodeP* rootNode, PNodeP* parentNode, int newParentType, int oldChildType, int& varCount, int left, int right, PSTRING& gridString);

  void getPElementTypeAttritbutes(int type, bool &sign, bool &enctype, bool &bytes, bool &termtype);

  static int convertPElementTypeToPADSLangType(int oldType);

  PNodeP* convertGridTreeToAST(PNodeP* PADSroot, PSTRING& gridString);
  PNodeP* convertGridTreeToASTRecur(PNodeP* PADSroot, PNodeP* parentNode, int& varCount, PSTRING& gridString);

  //PADSGridNode* buildChildTree(int* grid[], int depth);

  // protected:
  int nodeType;
  int left;
  int right;
  int depth;
   
  vector<PADSGridNode*> children;
  
};

class PADSMapElement
{
 public:
  PADSMapElement(PSTRING newKey, PNodeP* newValue);
  ~PADSMapElement();

  PSTRING key;
  PNodeP* value;
  int pairType;
};

class PADSTypeMap
{
public:
   PADSTypeMap();
   ~PADSTypeMap();

   int insert(PSTRING& key, PNodeP* value, int keyType = PTMAP_UDEF);
   PNodeP* remove(PSTRING& key);

   PNodeP* find(PSTRING& key);
   PNodeP* findSplitSpace(PSTRING& key);

   int findType(PSTRING& key);
   PNodeP* findNodeAndType(PSTRING& key, int* retType);

   static PSTRING splitStringAtSpace(PSTRING& key, PSTRING& retKey);

   void getTextualRepresentation(PSTRING& mapStr);

protected:
   PADSMapElement* findPair(PSTRING& key);
   vector<PADSMapElement*> mapping;
};

#endif
