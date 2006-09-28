/* *****************************************************************************
 * Mark Daly
 * June 2006
 * PADS_lang_tree_obj - Intermediate tree type for use in mapping to
 *  wxWidgets tree view GUI elements
 * ************************************************************************** */

#ifndef PADS_TREE_OBJ_H_INCLUDED__
#define PADS_TREE_OBJ_H_INCLUDED__

#include "PADS_lang.h"

class PADSTreeObject : wxTreeItemData
{
 public:
  PADSTreeObject();
  PADSTreeObject(PNodeP* pNode);
  ~PADSTreeObject();

  void setPADSNode(PNodeP* newNode);
  PNodeP* getPADSNode();

  void setUpdateParentOnChange(boo newVal);
  bool getUpdateParentOnChange();

 protected:  
  PNodeP* PADSNode;
  bool updateParent;
};

#endif
