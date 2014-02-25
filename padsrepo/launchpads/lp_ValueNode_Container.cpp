#include "lp_includes.h"

#include "wx/wx.h"
#include "wx/treectrl.h"
#include "wx/string.h"
#include "wx/arrstr.h"
#include "wx/dynarray.h"
#include "wx/regex.h"
#include "wx/textctrl.h"

#include "padslang/PADS_lang.h"

#include "lp_ValueNode_Container.h"

PValWXTreeItem::PValWXTreeItem() : wxTreeItemData()
{
  linkNode = NULL;
}

PValWXTreeItem::PValWXTreeItem(PADSUniTreeNode* newLink) : wxTreeItemData()
{
  linkNode = newLink;
}
