
#include "PADS_lang_tree_obj.h"

PADSTreeObject::PADSTreeObject()
{
  PADSNode = NULL;
  updateParent = false;
}

PADSTreeObject::PADSTreeObject(PNodeP* pNode)
{
  PADSNode = pNode;
  updateParent = false;
}

void PADSTreeObject::setPADSNode(PNodeP* newNode)
{
  PADSNode = newNode;
}

PNodeP* PADSTreeObject::getPADSNode()
{
  return PADSNode;
}
