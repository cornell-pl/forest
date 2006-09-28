/* **************************************
 * PADS GUI project (preliminary tests)
 * Left frame related code
 * *********************************** */
#include "lp_includes.h"
#include "pads_gui.h"
#include "lp_PNode_Container.h"

void LaunchPADS::LeftMakeVisualTreeFromAST(PNodeP* rootASTNode)
{
  PADSPNodeTreeNode* rootSimpleNode;
  DB_P("  LeftMakeVisualTreeFromAST - making simple tree\n");
  rootSimpleNode = rootASTNode->makeBasicTree(NULL);
  DB_P("  LeftMakeVisualTreeFromAST - deleting all Nodes\n");
  leftTreeCtrl->DeleteAllItems();
  DB_P("  LeftMakeVisualTreeFromAST - making new link\n");
  PNodeWXTreeItem* newLink = new PNodeWXTreeItem(rootASTNode);
  wxString tempStr = rootSimpleNode->nodeName.c_str();
  DB_P("  LeftMakeVisualTreeFromAST - adding root to visual tree\n");
  wxTreeItemId visRoot = leftTreeCtrl->AddRoot(tempStr, -1, -1, newLink);
  newLink->SetId(visRoot);
  DB_P("  LeftMakeVisualTreeFromAST - recuring through simple tree\n");
  if(rootSimpleNode->firstChild != NULL)
    LeftRecurThroughSimpleTree((PADSPNodeTreeNode*)rootSimpleNode->firstChild, visRoot);
  DB_P("  LeftMakeVisualTreeFromAST - deleting rootSimpleNode @ %d\n", rootSimpleNode);
  delete rootSimpleNode;
}

void LaunchPADS::LeftSetTreeElementColourFromType(int type, wxColour& setMe)
{
  switch(type)
    {
    case PT_LITERAL:
     if(colourModeOn)
       {  
	 setMe.Set(MID_MODE_DELIM_COLOUR);
       }
     else
       {
	 setMe.Set(MID_MODE_DELIM_NO_COLOUR);
       }
     break;
    case PT_NAME:
    case PT_VAR:
    case PT_ID:
    case PT_EXPR:
    case PT_PRE:
    case PT_STRING:
    case PT_CHAR:
    case PT_EXPR2:
    case PT_PPARSECHECK:
    case PT_TYPENAME:
    case PT_ARGUMENT:
    case PT_PCOMPUTE:
    case PT_FROM:
    case PT_PPREFIX:
    case PT_SEP_LOCAL:
     if(colourModeOn)
       {  
	 setMe.Set(255, 172, 64);
       }
     else
       {
	 setMe.Set(128, 128, 128);
       }
      break;
    case PT_PSOURCE:
    case PT_PRECORD:
    case PT_PLONGEST:
    case PT_PARRAY:
    case PT_PSOME:
    case PT_PNONE:
    case PT_POPT:
    case PT_POMIT:
    case PT_PENDIAN:
    case PT_PALTERNATES:
    case PT_PCHARCLASS:
    case PT_PENUM:
    case PT_PRECURSIVE:
    case PT_PSELECT:
    case PT_PSTRUCT:
    case PT_PTYPEDEF:
    case PT_PSWITCH:
    case PT_PUNION:
     if(colourModeOn)
       {  
	 setMe.Set(255, 
		   ((192+ ((type % 32) * 51)) %224), 
		   ((192+ ((type % 8)  * 51)) %224));
       }
     else
       {
	 setMe.Set(152+ (((type % 8) * 51) %63), 
		   152+ (((type % 8) * 51) %63), 
		   152+ (((type % 8) * 51) %63));
       }
      break;
    default:
      if(colourModeOn)
	{  
	  setMe.Set((64 + ((96   * type) / 51)      % 64), 
			    (96 + (((255 * type  * 2) / 51) % 159)), 
			    255);
	}
      else
	{
	  setMe.Set(96 + (((255 * type * 2) / 51) % 63), 
			    96 + (((255 * type * 2) / 51) % 63), 
			    96 + (((255 * type * 2) / 51) % 63));
	}
      break;
    }
}

wxTreeItemId LaunchPADS::LeftRecurThroughSimpleTree(PADSPNodeTreeNode* thisNode, wxTreeItemId& parentID)
{
  if(thisNode == NULL)
    return parentID;
  wxColour bgColor;
  PNodeP* newLinkPNode = thisNode->linkNode;
  PNodeWXTreeItem* newLink = new PNodeWXTreeItem(thisNode->linkNode);
  newLink->setUpdateParentOnChange(thisNode->getUpdateParentOnChange());
  newLink->setElementIsRequired(thisNode->getElementIsRequired());
  wxString tempStr = thisNode->nodeName.c_str();
  wxTreeItemId thisID = leftTreeCtrl->AppendItem(parentID, tempStr, -1, -1, newLink);
  LeftSetTreeElementColourFromType(newLinkPNode->nodeType, bgColor);
  leftTreeCtrl->SetItemBackgroundColour(thisID, bgColor);
  newLink->SetId(thisID);
  if(thisNode->firstChild != NULL)
    LeftRecurThroughSimpleTree((PADSPNodeTreeNode*)thisNode->firstChild, thisID);
  if(thisNode->nextSibling != NULL)
    LeftRecurThroughSimpleTree((PADSPNodeTreeNode*)thisNode->nextSibling, parentID);
  return thisID;
}

int LaunchPADS::LoadPXMLFile(wxString& loadPath)
{
  DB_P("making new PADS_lang_def_parser\n");
  PADS_lang_def_parser* parser = new PADS_lang_def_parser();
  PSTRING URIstr = loadPath.c_str();
  DB_P("LoadPXMLFile: starting to parse %s\n", URIstr.c_str());
  parser->parseURI(URIstr);
  DB_P("Done parsing %s\n", URIstr.c_str());
  PSTRING testStr;
  DB_P("seralizing...\n");
  parser->codifyRepresentation(testStr);
  DB_P("done seralization:\n%s\n", testStr.c_str());
  leftDefTree = parser->getParsedAST();
  LeftMakeVisualTreeFromAST(leftDefTree);
  delete parser;
  return 0;
}

int LaunchPADS::SavePXMLFile(wxString& savePath)
{
  DB_P("save pxml file!\n");
  wxTreeItemId selectedId = leftTreeCtrl->GetRootItem();
  PNodeWXTreeItem* newLink = (PNodeWXTreeItem*)leftTreeCtrl->GetItemData(selectedId);
  PNodeP* selectedNode = newLink->linkNode;
  PSTRING xmlStr;
  selectedNode->serializeXMLRepresentation(0, xmlStr);
  wxString writeThisStr = xmlStr.c_str();

  wxFFile defFile(savePath, "w");
  DB_P("Opening file %s\n", savePath.c_str());

  if(!defFile.Write(writeThisStr))
    {
      wxLogError(_T("ERROR: could not write PADS code to file."));
    }

  defFile.Close();
  return;
}

void LaunchPADS::OnLeftTreeCtrlSelect(wxTreeEvent &event)
{
  DB_P("left tree select!\n");
  wxTreeItemId selectedId = leftTreeCtrl->GetSelection();
  PNodeWXTreeItem* newLink = (PNodeWXTreeItem*)leftTreeCtrl->GetItemData(selectedId);
  PNodeP* selectedNode = newLink->linkNode;
  leftInspector->setSelectedPNodePandId(selectedNode, selectedId);
  leftInspector->setCanDeleteSelectedElement(!newLink->getElementIsRequired());
  return;
}

void LaunchPADS::OnLeftTreeBeginDrag(wxTreeEvent &event)
{
  DB_P("left tree begin drag!\n");
  wxTreeItemId selectedId = event.GetItem();
  PNodeWXTreeItem* newLink = (PNodeWXTreeItem*)leftTreeCtrl->GetItemData(selectedId);
  PNodeP* selectedNode = newLink->linkNode;
  DB_P("begin node of type: %s\n", PT_TypeNames[selectedNode->nodeType]);
  //leftInspector->setSelectedPNodePandId(selectedNode, selectedId);
  return;
}

void LaunchPADS::OnLeftTreeEndDrag(wxTreeEvent &event)
{
  DB_P("left tree end drag!\n");
  wxTreeItemId selectedId = event.GetItem();
  PNodeWXTreeItem* newLink = (PNodeWXTreeItem*)leftTreeCtrl->GetItemData(selectedId);
  PNodeP* selectedNode = newLink->linkNode;
  DB_P("end node of type: %s\n", PT_TypeNames[selectedNode->nodeType]);
  //leftInspector->setSelectedPNodePandId(selectedNode, selectedId);
  return;
}

/* ************************************ */
