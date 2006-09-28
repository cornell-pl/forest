//#include "lp_includes.h"

#ifndef LP_PNODE_CONTAINER_H_INCLUDED__
#define LP_PNODE_CONTAINER_H_INCLUDED__

#include "wx/wx.h"
#include "wx/treectrl.h"
#include "wx/string.h"
#include "wx/arrstr.h"
#include "wx/dynarray.h"
#include "wx/regex.h"
#include "wx/textctrl.h"

#include "padslang/pads_lang_includes.h"

class PNodeWXTreeItem : public wxTreeItemData
{
 public:
  PNodeP* linkNode;
  bool updateParent;
  bool requiredElement;
  PNodeWXTreeItem();
  PNodeWXTreeItem(PNodeP* newLink);
  void setUpdateParentOnChange(bool newVal);
  bool getUpdateParentOnChange();
  void setElementIsRequired(bool newVal);
  bool getElementIsRequired();
};

#endif
