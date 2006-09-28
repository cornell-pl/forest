#include "lp_includes.h"

#include "wx/wx.h"
#include "wx/treectrl.h"
#include "wx/string.h"
#include "wx/arrstr.h"
#include "wx/dynarray.h"
#include "wx/regex.h"
#include "wx/textctrl.h"

#include "padslang/PADS_lang.h"

#include "lp_PNode_Container.h"


PNodeWXTreeItem::PNodeWXTreeItem() : wxTreeItemData()
{
  linkNode = NULL;
  updateParent = false;
  requiredElement = false;
}

PNodeWXTreeItem::PNodeWXTreeItem(PNodeP* newLink) : wxTreeItemData()
{
  linkNode = newLink;
  updateParent = false;
  requiredElement = false;
}

void PNodeWXTreeItem::setUpdateParentOnChange(bool newVal)
{
  updateParent = newVal;
}

bool PNodeWXTreeItem::getUpdateParentOnChange()
{
  return updateParent;
}

void PNodeWXTreeItem::setElementIsRequired(bool newVal)
{
  requiredElement = newVal;
}

bool PNodeWXTreeItem::getElementIsRequired()
{
  return requiredElement;
}
