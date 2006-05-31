/* **************************************
 * PADS GUI project (preliminary tests)
 * XML state loading/saving code
 * *********************************** */
#include "lp_includes.h"


/* **************************************
 * Functions:
 *  --interface
 *  LoadState              --load XML file, with context dependent open file dialog paths
 *  LoadStateWrapped       --loads single XML state (actually does the work of loading)
 *  GetLoadStateNumSteps   --get number of loadable states in an XML file
 *  SaveStateAs            --save new XML file
 *  SaveState              --save XML file, with context dependent save file dialog paths
 *  SaveStateWrapped       --saves single XML state (actually does the work of saving)
 *  ISO_8859_1_process     --replace illegal characters with proper character entities - note needed when using TinyXML
 *  TreeLoadXMLRecur       --*DO NOT CALL DIRECTLY* auxiliary function for LoadStateWrapped (loads tree)
 *  TreeBuildXMLRecur      --*DO NOT CALL DIRECTLY* auxiliary function for SaveStateWrapped (saves tree)
 * *********************************** */

void LaunchPADS::LoadState(int stateNum, bool loadPlayback)
{
  if(loadPlayback == false)
    {
      if(wxMessageBox(_T("This will replace the current program state without saving the current state.\nContinue?"), 
		      _T("Confirm Load State"),
		      wxICON_QUESTION | wxYES_NO) == wxNO)
	{
	  return;
	}
    }

    if(!loadPlayback || !loadPathSet)
      {
	wxFileDialog openFileDialog(this, 
				    "Open XML state file",
				    "",
				    "",
				    STATE_FILETYPES,
				    wxOPEN | 
				    wxFILE_MUST_EXIST,	
				    wxDefaultPosition);
    
	if(openFileDialog.ShowModal() != wxID_OK)
	  {
	    return;
	  }
	if(!wxFile::Access(openFileDialog.GetPath(), wxFile::read))
	  {
	    wxMessageBox(_T("Selected XML state file could not be opened!"), 
			 _T("Could Not Open XML State"),
			 wxICON_ERROR | wxCANCEL);
	    return;
	  }
	loadPath = openFileDialog.GetPath();
	loadPathSet = true;
      } 

	// some tricks to keep the user occupied
	wxWindowDisabler *disableAll = new wxWindowDisabler();
	wxBusyInfo *bInfo = new wxBusyInfo(_T("Loading XML State..."), this);
	wxBeginBusyCursor();
	wxTheApp->Yield(); // global pointer to single instance of application... wow.

	LoadStateWrapped(stateNum, loadPlayback);

	bInfo->~wxBusyInfo();
	disableAll->~wxWindowDisabler();
	wxEndBusyCursor();
	wxLog::FlushActive();

	return;
}


void LaunchPADS::LoadStateWrapped(int stateNum, bool loadPlayback)
{

	int numWarnings = 0;
	int numErrors = 0;

	// do neat xml stuff
	DB_P("opened state file...\n");      

	TiXmlBase::SetCondenseWhiteSpace(false); // keep all the whitespace in the gui text

	DB_P("opening from file %s\n", loadPath.c_str());

	// convert this to a wxString in case we have to append a file type
	wxString xmlPath = loadPath;
	TiXmlDocument stateDoc;
	stateDoc.SetCondenseWhiteSpace(false);
	if(!stateDoc.LoadFile(xmlPath.c_str()))
	  {
	    wxMessageBox(_T("Selected XML state file did not load correctly!"), 
			 _T("Could Not Open XML State"),
			 wxICON_ERROR | wxCANCEL);
	    return;
	  }
	// TiXmlElement *xmlRoot = stateDoc.GetRootElement();

	const char *appName;
	const char *appVersion;
	const char *dateStamp;
	int   numSteps;

	TiXmlHandle docHandle(&stateDoc);
	TiXmlElement *xmlAppState;
	TiXmlElement *xmlChecklist;
	TiXmlElement *xmlGridCtrls;
	TiXmlElement *xmlGrid;
	TiXmlElement *xmlTree;
	TiXmlElement *xmlTreeCtrl;
	TiXmlElement *xmlCodeView;

	xmlAppState  = docHandle.FirstChild(XML_APPSTATE).Element();
	xmlChecklist = docHandle.FirstChild(XML_APPSTATE).ChildElement(XML_STEP, stateNum).FirstChild(XML_CHECKLIST).Element();
	xmlGridCtrls = docHandle.FirstChild(XML_APPSTATE).ChildElement(XML_STEP, stateNum).FirstChild(XML_GRIDCONTROLS).Element();
	xmlGrid      = docHandle.FirstChild(XML_APPSTATE).ChildElement(XML_STEP, stateNum).FirstChild(XML_GRID).Element();
	xmlTree      = docHandle.FirstChild(XML_APPSTATE).ChildElement(XML_STEP, stateNum).FirstChild(XML_TREE).Element();
	xmlTreeCtrl  = docHandle.FirstChild(XML_APPSTATE).ChildElement(XML_STEP, stateNum).FirstChild(XML_TREECTRL).Element();
	xmlCodeView  = docHandle.FirstChild(XML_APPSTATE).ChildElement(XML_STEP, stateNum).FirstChild(XML_CODEVIEW).Element();

	if(xmlAppState == NULL)
	  {
	    wxLogError(_T("Selected XML state file is missing main element AppState"));
	    return;
	  }

	if((appName    = xmlAppState->Attribute(XML_AS_APPNAME))    != NULL &&
	   (appVersion = xmlAppState->Attribute(XML_AS_APPVERSION)) != NULL &&
	   (dateStamp  = xmlAppState->Attribute(XML_AS_DATESTAMP))  != NULL)
	  {
	    if(loadPlayback == false)
	      wxLogMessage(_T("Loading state file with save info: %s v. %s @ %s"), appName, appVersion, dateStamp);
	  }
	else
	  {
	    wxLogMessage(_T("Could not read AppState attributes - app_state:%c; app_version:%c; date_stamp:%c"), 
			 appName    == NULL ? 'X' : '+', 
			 appVersion == NULL ? 'X' : '+',
			 dateStamp  == NULL ? 'X' : '+');
	  }

	if(xmlAppState->QueryIntAttribute(XML_NUM_STEPS, &numSteps) != TIXML_SUCCESS)
	  {
	    wxLogWarning(_T("Attribute AppState(\"num_steps\") missing - defaulting to 0 (initial state only)"));
	    numSteps = 0;
	  }
	xmlNumLoadSteps = numSteps;

	if(xmlChecklist == NULL || xmlGridCtrls == NULL || xmlGrid == NULL || xmlTree == NULL || xmlTreeCtrl == NULL || xmlCodeView == NULL)
	  {
	    DB_P("XML load: CL = %d, GC = %d, GR = %d, TR = %d, TC = %d\n", 
		 xmlChecklist, xmlGridCtrls, xmlGrid, xmlTree, xmlTreeCtrl);

	    wxLogError(_T("Selected XML state file is missing required elements - Checklist:%c; GridControls: %c; Grid:%c; Tree:%c; TreeCtrl:%c; Code:%c"),
		       xmlChecklist == NULL ? 'X' : '+', 
		       xmlGridCtrls == NULL ? 'X' : '+',
		       xmlGrid      == NULL ? 'X' : '+',
		       xmlTree      == NULL ? 'X' : '+',
		       xmlTreeCtrl  == NULL ? 'X' : '+', 
		       xmlCodeView  == NULL ? 'X' : '+');
	    return;
	  }

	// import window state
	int checked;
	if(xmlChecklist->QueryIntAttribute(XML_CL_CHECKED, &checked) != TIXML_SUCCESS)
	  {
	    checked = -1;
	    wxLogWarning(_T("Attribute Checklist(\"checked\") missing - defaulting to unchecked list"));
	    numWarnings++;
	  }

	topCheckList->Clear();

	TiXmlElement *checklistChild = xmlChecklist->FirstChild(XML_CL_KEY)->ToElement();
	TiXmlText *checklistText;

	for(int i = 0; 
	    checklistChild != NULL; 
	    checklistChild = checklistChild->NextSiblingElement(XML_CL_KEY), 
	      i++)
	  {
	    checklistText = checklistChild->FirstChild()->ToText();
	    if(checklistText != NULL)
	      {
		topCheckList->Append(checklistText->Value());
		if(i == checked) // crude error checking, rather than direct assignment
		  {
		    topCheckList->Check(i, true);
		  }
	      }
	  }

	// read grid controls state
	int ctrlsMode;
	int ctrlsTerm;
	int ctrlsNonterm;
	if(xmlGridCtrls->QueryIntAttribute(XML_GC_MODE, &ctrlsMode) != TIXML_SUCCESS)
	  {
	    ctrlsMode = MID_MODE_NONE;
	    wxLogWarning(_T("Attribute %s(\"%s\") missing - defaulting to \"None\""),
			 XML_GRIDCONTROLS, XML_GC_MODE);
	    numWarnings++;
	  }
	if(xmlGridCtrls->QueryIntAttribute(XML_GC_TERMTYPE, &ctrlsTerm) != TIXML_SUCCESS)
	  {
	    ctrlsTerm = 0;
	    wxLogWarning(_T("Attribute %s(\"%s\") missing - defaulting to \"%s\""),
			 XML_GRIDCONTROLS, XML_GC_NONTERMTYPE, 
			 PADS_labels[pChar]);
	    numWarnings++;
	  }
	if(xmlGridCtrls->QueryIntAttribute(XML_GC_NONTERMTYPE, &ctrlsNonterm) != TIXML_SUCCESS)
	  {
	    ctrlsNonterm = 0;
	    wxLogWarning(_T("Attribute %s(\"%s\") missing - defaulting to \"%s\""),
			 XML_GRIDCONTROLS, XML_GC_NONTERMTYPE, 
			 PADS_labels[pStruct]);
	    numWarnings++;
	  }

	DB_P("setting controls mode to %d\n", ctrlsMode);
	if(ctrlsMode >= MID_MODE_NONE &&
	   ctrlsMode <= MID_MODE_UDEF)
	  {
	    DB_P("selecting right mode: %d\n", ctrlsMode);
	    wxString stringSelection;
	    int selection;
	    selection = rightModeSelect->GetSelection();
	    stringSelection = rightModeSelect->GetStringSelection();
	    DB_P("current selection = %d\n", selection);
	    DB_P("selected string: %d\n", stringSelection.c_str());
	    rightModeSelect->SetSelection(ctrlsMode);
	    stringSelection = rightModeSelect->GetStringSelection();
	    selection = rightModeSelect->GetSelection();
	    DB_P("new selection = %d\n", selection);
	    DB_P("new string: %d\n", stringSelection.c_str());
	    rightModeSelect->SetStringSelection(stringSelection);
	    rightModeSelect->Update();

	    wxCommandEvent event1(wxEVT_COMMAND_RADIOBOX_SELECTED, RIGHT_MODE_SELECT);
	    event1.SetInt(MID_MODE_NONE);
	    wxPostEvent(rightModeSelect, event1);

	    wxCommandEvent event2(wxEVT_COMMAND_RADIOBOX_SELECTED, RIGHT_MODE_SELECT);
	    event2.SetInt(ctrlsMode);
	    wxPostEvent(rightModeSelect, event2);

	  }
	rightTypeSelect->SetSelection(ctrlsTerm);
	rightStructSelect->SetSelection(ctrlsNonterm);
	MidSetEditMode(MID_MODE_NONE); // set this to make sure we don't reset one of the menus by accident
	MidSetEditMode(ctrlsMode);

	// read grid state
	TiXmlElement *xmlGridRow;
	TiXmlElement *xmlGridCell;
	TiXmlText    *xmlGridText;
	wxString textStr = "";

	int numRows, numCols;

	// before we set these explicitly, let's see if we can "play back" input to recreate the state
	// xmlGrid->QueryIntAttribute(XML_G_CURRENTROW, &midCurrentRow);
	// xmlGrid->QueryIntAttribute(XML_G_CURRENTID,  &midCurrentID); // we'll do this later
	// xmlGrid->QueryIntAttribute(XML_G_MAXLEVELS,  &midGridMaxLevels);


	xmlGridRow = xmlGrid->FirstChild(XML_GRIDTEXT)->ToElement();
	if(xmlGridRow == NULL)
	  {
	    wxLogWarning(_T("Element Grid:GridText missing - skipping grid loading phase."));
	    numWarnings++;
	    MidGridUseAsInput(textStr);
	  }
	else
	  {

	    xmlGridText = xmlGridRow->FirstChild()->ToText();

	    if(xmlGridText != NULL)
	      textStr = xmlGridText->Value();
	    else
	      textStr = " ";

	    DB_P("text string = <%s>\n", textStr.c_str());

	    if(textStr.IsEmpty())
	      {
		wxLogMessage("Element GridText contains no text - grid creation phase skipped.");
		DB_P("skipping grid creation on empty string\n");
		MidGridUseAsInput(textStr);
	      }
	    else
	      {
		if(xmlGrid->QueryIntAttribute(XML_G_COLS, &numCols) != TIXML_SUCCESS)
		  {
		    numCols = textStr.Length();
		    wxLogWarning(_T("Attribute Grid(\"cols\") missing - defaulting to text length %d."), numCols);
		    numWarnings++;
		  }
		if(xmlGrid->QueryIntAttribute(XML_G_ROWS, &numRows) != TIXML_SUCCESS)
		  {
		    numRows = 0;
		    wxLogWarning(_T("Grid \"rows\" attribute missing - defaulting to 0 rows."));
		    numWarnings++;
		  }

		// remake the grid as if it were user input
		MidGridUseAsInput(textStr);
		int foundRow = -1;
		int thisCol, thisRow;
		int thisType, thisID, thisMode;
		wxString str;
		wxString cellStr;
		for(int i = 0; i < numRows && xmlGridRow != NULL; i++)
		  {
		    // allow for out-of-order row writing by searching through rows based
		    // on numerical order (allows for user alteration of saved states)
		    // looks for row number "i" in next sibling - if not found, starts
		    // from the first sibling and looks through all siblings until it finds the correct one

		    // give a slight performance boost if the rows are written in numerical order
		    xmlGridRow = xmlGridRow->NextSiblingElement(XML_ROW);
		    if(xmlGridRow != NULL)
		      {
			if(xmlGridRow->QueryIntAttribute(XML_R_ROW, &foundRow) != TIXML_SUCCESS)
			  wxLogWarning(_T("Attribute Row:\"row\" not found for row after %dth \"Row\" element in file."), i-1);
			numWarnings++;
		      }
		    if(foundRow != i)
		      {
			xmlGridRow = xmlGrid->FirstChild(XML_ROW)->ToElement();
			// find the next row based on numerical designation
			for( ;
			     i != foundRow &&  xmlGridRow != NULL;
			     xmlGridRow = xmlGridRow->NextSiblingElement(XML_ROW))
			  {
			    // this could go in the for loop, but it's better to do some error checking instead
			    if(xmlGridRow->QueryIntAttribute(XML_R_ROW, &foundRow) != TIXML_SUCCESS)
			      wxLogWarning(_T("Attribute Row:\"row\" not found for anonymous row."));
			    numWarnings++;
			  }
		      }
		    if(xmlGridRow == NULL)
		      {
			wxLogError(_T("(Critical) Missing grid row %d, skipping remainder of grid phase."), i);
			break;
		      }
		    else // if we've found the correct row, deal with the cells in that row
		      {
			for(xmlGridCell = xmlGridRow->FirstChild(XML_CELL)->ToElement(); 
			    xmlGridCell != NULL; 
			    xmlGridCell = xmlGridCell->NextSiblingElement(XML_CELL))
			  {
			    /* 
			    // the cell row attribute has been removed rather than make trouble for users trying to mismatch Cell row attributes 
			    // and Row row attributes
			    if(xmlGridCell->QueryIntAttribute(XML_R_ROW, &thisRow) != TIXML_SUCCESS)
			    {
			    wxLogWarning(_T("Attribute Cell(\"row\") not found for anonymous Cell - defaulting to parent Row(\"row\") attribute."));
			    thisRow = i;
			    }
			    */
			    thisRow = i;
			    if(xmlGridCell->QueryIntAttribute(XML_C_COL, &thisCol) != TIXML_SUCCESS)
			      {
				wxLogWarning(_T("Attribute Cell(\"col\") not found for anonymous Cell with functional row %d - \
terminating grid loading sequence."), 
					     thisRow);
				numWarnings++;
				break;
			      }
			    if(thisRow != i)
			      {
				wxLogError(_T("Row mismatch in element Grid:Row:Cell - cell row number %d != \"Row\" row number %d \
at (correct) row %d, col %d."), 
					   thisRow, i, i, thisCol);
				break;
			      }

			    xmlGridText = xmlGridCell->FirstChild()->ToText();
			    if(xmlGridText != NULL)
			      {
				cellStr = xmlGridText->Value();
				if(cellStr.GetChar(0) != '"' ||
				   cellStr.Last()  != '"')
				  {
				    wxLogWarning(_T("Element Grid:Row:Cell with text %s formatted incorrectly."), cellStr.c_str());
				    numWarnings++;
				  }
				else
				  {
				    cellStr.RemoveLast(); // remove last quote
				    cellStr.Replace(_T("\""), _T(""), false); // remove first quote
				  }

				midGrid->SetCellValue(thisRow, thisCol, cellStr);

				if(xmlGridCell->QueryIntAttribute(XML_C_CLASSID, &thisID) != TIXML_SUCCESS)
				  {
				    wxLogWarning(_T("Attribute Cell(\"class_id\") missing in cell at row:%d, col:%d."),
						 thisRow, thisCol);
				    numWarnings++;
				  }
				midGridUDefID[thisCol] = thisID;

				if(xmlGridCell->QueryIntAttribute(XML_TYPE, &thisType) != TIXML_SUCCESS)
				  {
				    wxLogWarning(_T("Attribute Cell(\"type\") missing in cell at row:%d, col:%d."),
						 thisRow, thisCol);
				    numWarnings++;
				  }
				midGridType[thisCol] = thisType;

				int newMode;
				if(thisType ==  pLit)
				  {
				    newMode = MID_MODE_DELIM;
				    midHighlightColour = midDelimColour;
				    midGrid->SetCellBackgroundColour(thisRow, thisCol, midHighlightColour);
				  }
				else if(PElement::IsPTerminal(thisType))
				  { 
				    newMode = MID_MODE_TERM;
				    MidGridNewTermColour(midGridUDefID[thisCol], MID_MODE_TERM_COL_STEPS);
				    midHighlightColour = midTermColour;
				    midGrid->SetCellBackgroundColour(thisRow, thisCol, midHighlightColour);
				  }
				else if(PElement::IsPNonTerminal(thisType))
				  {
				    newMode = MID_MODE_UDEF;
				    MidGridNewUDefColour(midGridUDefID[thisCol], MID_MODE_TERM_COL_STEPS);
				    midHighlightColour = midUDefColour;
				    midGrid->SetCellBackgroundColour(thisRow, thisCol, midHighlightColour);
				  }
				else
				  {
				    newMode = MID_MODE_NONE;
				    midHighlightColour = midNoneColour;
				    midGrid->SetCellBackgroundColour(thisRow, thisCol, midHighlightColour);
				  }

				midGridMode[thisCol] = newMode;
			      }
			  }
		      }

		    if(i != numRows - 1)
		      {
			MidGridNextLevel();
		      }
		    else
		      {
		   
			for(xmlGridCell = xmlGridRow->FirstChild(XML_CELL)->ToElement();
			    xmlGridCell != NULL;
			    xmlGridCell = xmlGridCell->NextSiblingElement(XML_CELL))
			  {
			    if(xmlGridCell->QueryIntAttribute(XML_C_COL, &thisCol) != TIXML_SUCCESS)
			      {
				wxLogWarning(_T("Attribute Cell(\"col\") not found for anonymous Cell with functional row %d."), midCurrentRow);
				numWarnings++;
				continue;
			      }
			    if(xmlGridCell->QueryIntAttribute(XML_C_MODE, &thisMode) != TIXML_SUCCESS)
			      {
				wxLogWarning(_T("Attribute Cell(\"mode\") not found for anonymous Cell at row:%d, col:%d."), midCurrentRow, thisCol);
				numWarnings++;
				continue;
			      }
			    midGridMode[thisCol] = thisMode;
			  }
		      }
		  }
		if(xmlGrid->QueryIntAttribute(XML_G_CURRENTID,  &midCurrentID) != TIXML_SUCCESS)
		  {
		    wxLogWarning(_T("Attribute Grid(\"current_id\") not found."));
		    numWarnings++;
		    midCurrentID = 0;
		  }

		int cursorRow, cursorCol;
		if(xmlGrid->QueryIntAttribute(XML_G_CURSOR_ROW,  &cursorRow) != TIXML_SUCCESS)
		  {
		    wxLogWarning(_T("Attribute Grid(\"cursor_row\") not found."));
		    numWarnings++;
		    cursorRow = 0;
		  }
		if(xmlGrid->QueryIntAttribute(XML_G_CURSOR_COL,  &cursorCol) != TIXML_SUCCESS)
		  {
		    wxLogWarning(_T("Attribute Grid(\"cursor_col\") not found."));
		    numWarnings++;
		    cursorCol = 0;
		  }
		midGrid->SetGridCursor(cursorRow, cursorCol);
		midGrid->MakeCellVisible(cursorRow, cursorCol);
	      }
	  }

	RightSetRadioMode(MID_MODE_NONE);
	midHighlightColour = midNoneColour;

	// read in tree state

	// coppied from MidGridMakeTreeFromParse
	//TiXmlElement *xmlTreeRoot;
	TiXmlElement *xmlTreeNode = NULL;
	PElement* elem;
	wxString str;
	wxTreeItemId root;

	leftTreeCtrl->DeleteAllItems();
	    
	str = "Root";
	elem = new PElement((int)pStruct, str, 0);
	root = leftTreeCtrl->AddRoot(str, -1, -1,elem);

	// get root element - we make a new fake one anyway, so we'll just move past it
	xmlTreeNode = xmlTree->FirstChild(XML_TREEITEM)->ToElement();
	/*	if(xmlTreeRoot != NULL)
	  {
	    xmlTreeNode = xmlTreeRoot->FirstChild(XML_TREEITEM)->ToElement();
	  }
	*/
	if(xmlTreeNode == NULL)
	  {
	    str.Printf("%s %s", PADS_generalized_names[pStruct], "Source_t");
	    elem = new PElement((int)pStruct, str, pSourceFlag);
	    assert(elem != NULL);
	    wxTreeItemId source = leftTreeCtrl->AppendItem(root, str, -1, -1,elem);
	    if(leftTreeBackgroundColourOn)
	      leftTreeCtrl->SetItemBackgroundColour(source, wxColour(MID_MODE_UDEF_COLOUR));
	    else if(leftTreeTextColourOn)
	      leftTreeCtrl->SetItemTextColour(source, wxColour(MID_MODE_UDEF_COLOUR));
	    else
	      leftTreeCtrl->SetItemBackgroundColour(source, wxColour(MID_MODE_UDEF_NO_COLOUR));
	  }
	else // if the tree has branches...
	  {
	    int tempWarnings = 0;
	    tempWarnings += TreeLoadXMLRecur(root, xmlTreeNode, 0);
	    // wxLogMessage(_T("TreeLoad returned %d warnings."), tempWarnings);
	    numWarnings += tempWarnings;
	  }
	leftTreeCtrl->Refresh();

	// read in tree control state
	int shown;
	int type;
	int sign, enctype, bytes, termtype;
	int tdef, compute, record, source;
	wxString name;
	wxString stopExpr;
	wxString predicate;
	wxString customText;


	if(xmlTreeCtrl->QueryIntAttribute(XML_TC_SHOWN, &shown) != TIXML_SUCCESS)
	  {
	    wxLogWarning(_T("Attribute TextCtrl(\"%s\") not found."), 
			 XML_TC_SHOWN);
	    numWarnings++;
	    shown = 0;
	  }

	if(xmlTreeCtrl->QueryIntAttribute(XML_TYPE, &type) != TIXML_SUCCESS)
	  {
	    wxLogWarning(_T("Attribute TextCtrl(\"%s\") not found."), 
			 XML_TYPE);
	    numWarnings++;
	    type = -1;
	  }

	if(xmlTreeCtrl->QueryIntAttribute(XML_SIGN, &sign) != TIXML_SUCCESS)
	  {
	    wxLogWarning(_T("Attribute TextCtrl(\"%s\") not found."), XML_SIGN);
	    numWarnings++;
	    sign = 0;
	  }
	if(xmlTreeCtrl->QueryIntAttribute(XML_T_ENCTYPE, &enctype) != TIXML_SUCCESS)
	  {
	    wxLogWarning(_T("Attribute TextCtrl(\"%s\") not found."), XML_T_ENCTYPE);
	    numWarnings++;
	    enctype = 0;
	  }
	if(xmlTreeCtrl->QueryIntAttribute(XML_BYTES, &bytes) != TIXML_SUCCESS)
	  {
	    wxLogWarning(_T("Attribute TextCtrl(\"%s\") not found."), XML_BYTES);
	    numWarnings++;
	    bytes = 0;
	  }
	if(xmlTreeCtrl->QueryIntAttribute(XML_TERMTYPE, &termtype) != TIXML_SUCCESS)
	  {
	    wxLogWarning(_T("Attribute TextCtrl(\"%s\") not found."), XML_TERMTYPE);
	    numWarnings++;
	    termtype = 0;
	  }

	if(xmlTreeCtrl->QueryIntAttribute(XML_TYPEDEF, &tdef) != TIXML_SUCCESS)
	  {
	    wxLogWarning(_T("Attribute TextCtrl(\"%s\") not found."), "tdef");
	    numWarnings++;
	    tdef = 0;
	  }
	if(xmlTreeCtrl->QueryIntAttribute(XML_TC_COMPUTE, &compute) != TIXML_SUCCESS)
	  {
	    wxLogWarning(_T("Attribute TextCtrl(\"%s\") not found."), XML_TC_COMPUTE);
	    numWarnings++;
	    compute = 0;
	  }
	if(xmlTreeCtrl->QueryIntAttribute(XML_TC_RECORD, &record) != TIXML_SUCCESS)
	  {
	    wxLogWarning(_T("Attribute TextCtrl(\"%s\") not found."), XML_TC_RECORD);
	    numWarnings++;
	    record = 0;
	  }
	if(xmlTreeCtrl->QueryIntAttribute(XML_TC_SOURCE, &source) != TIXML_SUCCESS)
	  {
	    wxLogWarning(_T("Attribute TextCtrl(\"%s\") not found."), XML_TC_SOURCE);
	    numWarnings++;
	    source = 0;
	  }

	TiXmlHandle  leftCtrlHandle(xmlTreeCtrl);
	TiXmlText    *getText;

	getText = leftCtrlHandle.FirstChild(XML_NAME).FirstChild().Text();
	if(getText != NULL)
	    name = getText->Value();
	else
	  {
	    // these can be empty, so there's no need for a warning
	    //wxLogWarning(_T("Element TextCtrl:%s not found."), XML_NAME);
	    name = "";
	  }

	getText = leftCtrlHandle.FirstChild(XML_STOPEXPR).FirstChild().Text();
	if(getText != NULL)
	  stopExpr = getText->Value();
	else
	  {
	    //wxLogWarning(_T("Element TextCtrl:%s not found."), XML_STOPEXPR);
	    stopExpr = "";
	  }

	getText = leftCtrlHandle.FirstChild(XML_PREDICATES).FirstChild().Text();
	if(getText != NULL)
	  predicate = getText->Value();
	else
	  {
	    //wxLogWarning(_T("Element TextCtrl:%s not found."), XML_PREDICATES);
	    predicate = "";
	  }

	getText = leftCtrlHandle.FirstChild(XML_CUSTOMTEXT).FirstChild().Text();
	if(getText != NULL)
	    customText = getText->Value();
	else
	  {
	    //wxLogWarning(_T("Element TextCtrl:%s not found."), XML_CUSTOMTEXT);
	    customText = "";
	  }

	LeftEnableControls(shown == 1 ? true : false);

	leftTypeSelect->SetSelection(type);
	leftNameInput->SetValue(name);

	leftSignSelect->SetSelection(sign);
	leftEncodingSelect->SetSelection(enctype);
	leftByteSelect->SetSelection(bytes);
	leftTerminationSelect->SetSelection(termtype);

	leftTypedef->SetValue(tdef    == 1 ? true : false);
	leftCompute->SetValue(compute == 1 ? true : false);
	leftRecord->SetValue(record   == 1 ? true : false);
	leftSource->SetValue(source   == 1 ? true : false);

	leftStopExpr->SetValue(stopExpr);
	leftArgInput_1->SetValue(predicate);
	leftTextSpecMain->SetValue(customText);

	// read in code state
	int codeMade, pos;
	if(xmlCodeView->QueryIntAttribute(XML_CV_GENERATED, &codeMade) != TIXML_SUCCESS)
	  {
	    wxLogWarning(_T("Attribute Code(\"generated\") not found - defaulting to 0 (false)."));
	    numWarnings++;
	    codeMade = 0;
	  }
	if(xmlCodeView->QueryIntAttribute(XML_CV_POS, &pos) != TIXML_SUCCESS)
	  {
	    wxLogWarning(_T("Attribute Code(\"pos\") not found - defaulting to 0."));
	    numWarnings++;
	    pos = 0;
	  }

	if(codeMade == 1)
	  {
	    BottomClearText();
	    LeftMakeCodeFromTree();
	    bottomCodeMade = true;
	    bottomCodeDisplay->ShowPosition(pos);
	  }
	else
	  {
	    BottomClearText();
	    bottomCodeMade = false;
	  }

	// print log messages
	//	wxLogMessage(_T("XML state load completed with %d warnings and %d errors."), numWarnings, numErrors);
	if(!loadPlayback)
	  {
	    wxLogMessage(_T("XML state load completed.")); // strange bug in error counting - don't use previous line

	  }

	xmlCurrentState = stateNum;

	stateDoc.Clear(); // I hope this works...

    return;
}

int  LaunchPADS::GetLoadStateNumSteps(void)
{
  if(!loadPathSet)
    {
      wxFileDialog openFileDialog(this, 
				  "Open XML state file",
				  "",
				  "",
				  STATE_FILETYPES,
				  wxOPEN | 
				  wxFILE_MUST_EXIST,	
				  wxDefaultPosition);
    
      if(openFileDialog.ShowModal() != wxID_OK)
	{
	  return 0;
	}
      if(!wxFile::Access(openFileDialog.GetPath(), wxFile::read))
	{
	  wxMessageBox(_T("Selected XML state file could not be opened!"), 
		       _T("Could Not Open XML State"),
		       wxICON_ERROR | wxCANCEL);
	  return 0;
	}
      loadPath = openFileDialog.GetPath();
      loadPathSet = true;
    }

  TiXmlDocument stateDoc;
  wxString xmlPath = loadPath;
  if(!stateDoc.LoadFile(xmlPath.c_str()))
    {
      wxMessageBox(_T("Selected XML state file did not load correctly!"), 
		   _T("Could Not Open XML State"),
		   wxICON_ERROR | wxCANCEL);
      return 0;
    }

  TiXmlHandle docHandle(&stateDoc);
  TiXmlElement *xmlAppState;
  xmlAppState  = docHandle.FirstChild(XML_APPSTATE).Element();

  if(xmlAppState == NULL)
    {
      wxLogError(_T("Selected XML state file is missing main element AppState"));
      return 0;
    }
  int numSteps;
  if(xmlAppState->QueryIntAttribute(XML_NUM_STEPS, &numSteps) != TIXML_SUCCESS)
    {
      wxLogWarning(_T("Attribute AppState(\"num_steps\") missing - defaulting to 0 (initial state only)"));
      numSteps = 0;
    }
  xmlNumLoadSteps = numSteps;

  return 1;
}


void LaunchPADS::SaveStateAs(int stateNum, bool catState)
{
  wxFileDialog saveFileDialog(this, 
			      "Save state to XML",			      
			      "",
			      "",
			      STATE_FILETYPES,
			      wxSAVE |
			      wxOVERWRITE_PROMPT,
			      wxDefaultPosition);

  if(saveFileDialog.ShowModal() == wxID_OK)
    {	  
      savePath = saveFileDialog.GetPath();
      savePathSet = true;
      SaveState(stateNum, catState);
    }

}
void LaunchPADS::SaveState(int stateNum, bool catState)
{

  if(!savePathSet)
    {
      SaveStateAs(stateNum, catState);
    }
  else
    {
      wxWindowDisabler *disableAll = new wxWindowDisabler();
      wxBusyInfo *bInfo = new wxBusyInfo(_T("Saving XML State..."), this);
      wxBeginBusyCursor();
      wxTheApp->Yield(); // global pointer to single instance of application... wow.

      SaveStateWrapped(stateNum, catState);

      bInfo->~wxBusyInfo();
      disableAll->~wxWindowDisabler();   
      wxEndBusyCursor();
      wxLog::FlushActive();
    }
  return;
}


void LaunchPADS::SaveStateWrapped(int stateNum, bool catState)
{

  DB_P("in save state\n");


  TiXmlBase::SetCondenseWhiteSpace(false); // keep all the whitespace in the gui text

  DB_P("saving to file %s\n", savePath.c_str());

  // convert this to a wxString in case we have to append a file type
  wxString xmlPath = savePath;
  TiXmlDocument stateDoc(xmlPath.c_str());
  stateDoc.SetCondenseWhiteSpace(false);

  stateDoc.SetTabSize(3);  // let's see if this works...

  TiXmlElement *xmlStateInfo;

  wxString timeNow = wxDateTime::Now().Format("%c", wxDateTime::UTC);
  TiXmlDeclaration xmlDec(XML_ENCODING_VER, XML_ENCTYPE, XML_STANDALONE);

  DB_P("here 28\n");

  if(catState == false || stateNum == 0)  // working on a new file
    {
      stateDoc.InsertEndChild(xmlDec);
      
      xmlStateInfo = new TiXmlElement(XML_APPSTATE);
      xmlStateInfo->SetAttribute(XML_AS_APPNAME, APP_NAME_STRING);
      xmlStateInfo->SetAttribute(XML_AS_APPVERSION, GUI_VERSION);
      xmlStateInfo->SetAttribute(XML_AS_DATESTAMP, timeNow.c_str());
      xmlStateInfo->SetAttribute(XML_NUM_STEPS, 0);
    }
  else
    {
      stateDoc.LoadFile();
      xmlStateInfo = stateDoc.FirstChildElement(XML_APPSTATE);
      xmlStateInfo->SetAttribute(XML_NUM_STEPS, stateNum);
    }

  DB_P("here 30\n");

  if(xmlMessagesOnSave == true && catState == false)
    wxLogMessage(_T("Saving state with save info: %s v. %s @ %s"), APP_NAME_STRING, GUI_VERSION, timeNow.c_str());

  // set initial state for replay functionality (to be added later)
  TiXmlElement xmlStep(XML_STEP);
  xmlStep.SetAttribute(XML_S_NUM, stateNum);
  xmlStep.SetAttribute(XML_S_TIMESTAMP, wxDateTime::Now().Format("%s", wxDateTime::UTC).c_str());

  // top frame state        
  int selectedItem = topCheckedEntry;
  int numItems = topCheckList->GetCount();

  TiXmlElement xmlChecklist(XML_CHECKLIST);
  TiXmlElement *xmlTopEntry;
  TiXmlText    *xmlTopEntryText;

  xmlChecklist.SetAttribute(XML_CL_CHECKED, selectedItem);
      
  for(int i = 0; i < numItems; i++)
    {
      xmlTopEntry = new TiXmlElement(XML_CL_KEY);
      xmlTopEntryText = new TiXmlText(topCheckList->GetString(i).c_str());
      xmlTopEntry->InsertEndChild(*xmlTopEntryText);
      xmlChecklist.InsertEndChild(*xmlTopEntry);
    }
  xmlStep.InsertEndChild(xmlChecklist);


      TiXmlElement xmlGridControls(XML_GRIDCONTROLS);
      int currentMode = rightModeSelect->GetSelection();
      int termSelection = rightTypeSelect->GetSelection();
      int nontermSelection = rightStructSelect->GetSelection();

      xmlGridControls.SetAttribute(XML_GC_MODE, currentMode);
      xmlGridControls.SetAttribute(XML_GC_TERMTYPE, termSelection);
      xmlGridControls.SetAttribute(XML_GC_NONTERMTYPE, nontermSelection);

      xmlStep.InsertEndChild(xmlGridControls);

      // mid frame state
      TiXmlElement xmlGrid(XML_GRID);
      if(midGridExists)
	{
	  int numCols = midGrid->GetNumberCols();
	  int numRows = midGrid->GetNumberRows();

	  DB_P("XML: num rows = %d, current row = %d\n", 
	       numRows, midCurrentRow);
	  DB_P("XML: num cols = %d, text length = %d\n", 
	       numCols, midGridTextLength);
	  
	  xmlGrid.SetAttribute(XML_G_ROWS, numRows);
	  xmlGrid.SetAttribute(XML_G_COLS, numCols);
	  xmlGrid.SetAttribute(XML_G_CURRENTID, midCurrentID);
	  xmlGrid.SetAttribute(XML_G_MAXLEVELS, midGridMaxLevels);
	  xmlGrid.SetAttribute(XML_G_CURRENTROW, midCurrentRow);
	  xmlGrid.SetAttribute(XML_G_CURSOR_ROW, midGrid->GetGridCursorRow());
	  xmlGrid.SetAttribute(XML_G_CURSOR_COL, midGrid->GetGridCursorCol());

	  TiXmlElement xmlGridStr(XML_GRIDTEXT);
	  xmlGridStr.SetAttribute(XML_GT_LENGTH, midGridTextLength);
	  TiXmlText xmlGridStrText(midGridText.c_str());
	  xmlGridStr.InsertEndChild(xmlGridStrText);
	  xmlGrid.InsertEndChild(xmlGridStr);

	  TiXmlElement *xmlGridRow;
	  TiXmlElement *xmlGridCell;
	  TiXmlText    *xmlGridCellText;
	  wxString cellStr;
	  
	  for(int i = 0; i < numRows; i++)
	    {
	      xmlGridRow = new TiXmlElement(XML_ROW);
	      xmlGridRow->SetAttribute(XML_R_ROW, i);
	      for(int j = 0; j < numCols; j++)
		{
		  cellStr = midGrid->GetCellValue(i, j);
		  if(cellStr.Length() == 0)
		    {
		      continue;
		    }
		  cellStr.Prepend('"').Append('"');

		  xmlGridCell = new TiXmlElement(XML_CELL);
		  // there's lots of redundancy here, but we'll keep it to be safe and/or for the playback feature
		  //xmlGridCell->SetAttribute(XML_R_ROW, i);
		  xmlGridCell->SetAttribute(XML_C_COL, j);
		  xmlGridCell->SetAttribute(XML_C_DEPTH,    midGridDepth[j]);
		  xmlGridCell->SetAttribute(XML_C_MODE,     midGridMode[j]);
		  xmlGridCell->SetAttribute(XML_TYPE,       midGridTypeLevels[i][j]);
		  xmlGridCell->SetAttribute(XML_C_CLASSID,  midGridUDefIDLevels[i][j]);

		  //ISO_8859_1_process(cellStr); // turns out this is done for us...
		  xmlGridCellText = new TiXmlText(cellStr.c_str());
		  xmlGridCell->InsertEndChild(*xmlGridCellText);

		  xmlGridRow->InsertEndChild(*xmlGridCell);
		}
	      xmlGrid.InsertEndChild(*xmlGridRow);
	    }
	  xmlStep.InsertEndChild(xmlGrid);
	}
      else
	{
	  xmlGrid.SetAttribute(XML_G_ROWS, 0);
	  xmlGrid.SetAttribute(XML_G_COLS, 0);
	  xmlGrid.SetAttribute(XML_G_CURRENTID, midCurrentID);

	  TiXmlElement xmlGridStr(XML_GRIDTEXT);
	  xmlGridStr.SetAttribute(XML_GT_LENGTH, 0);
	  TiXmlText xmlGridStrText("");
	  xmlGridStr.InsertEndChild(xmlGridStrText);
	  xmlGrid.InsertEndChild(xmlGridStr);

	  xmlStep.InsertEndChild(xmlGrid);
	}	

      // left tree view state
      TiXmlElement xmlTree(XML_TREE);
      
      DB_P("recurring on root...\n");
      wxTreeItemId treeRoot = leftTreeCtrl->GetRootItem();
      wxTreeItemIdValue cookie;
      wxTreeItemId child = leftTreeCtrl->GetFirstChild(treeRoot, cookie);
      if(child.IsOk())
	{
	  TreeBuildXMLRecur(child, &xmlTree, cookie);
	}

      xmlStep.InsertEndChild(xmlTree);

      TiXmlElement xmlTreeCtrl(XML_TREECTRL);
      xmlTreeCtrl.SetAttribute(XML_TC_SHOWN,  leftTreeControlState == true ? 1 : 0);

      xmlTreeCtrl.SetAttribute(XML_TYPE,     leftTypeSelect->GetSelection());
      xmlTreeCtrl.SetAttribute(XML_SIGN,     leftSignSelect->GetSelection());
      xmlTreeCtrl.SetAttribute(XML_T_ENCTYPE,  leftEncodingSelect->GetSelection());
      xmlTreeCtrl.SetAttribute(XML_BYTES,    leftByteSelect->GetSelection());
      xmlTreeCtrl.SetAttribute(XML_TERMTYPE, leftTerminationSelect->GetSelection());

      xmlTreeCtrl.SetAttribute(XML_TYPEDEF,  leftTypedef->GetValue() == true ? 1 : 0);
      xmlTreeCtrl.SetAttribute(XML_TC_COMPUTE,  leftCompute->GetValue() == true ? 1 : 0);
      xmlTreeCtrl.SetAttribute(XML_TC_RECORD,   leftRecord->GetValue()  == true ? 1 : 0);
      xmlTreeCtrl.SetAttribute(XML_TC_SOURCE,   leftSource->GetValue()  == true ? 1 : 0);

      // left tree controls state
      TiXmlElement *xmlCtrlWrapper;
      TiXmlText *xmlCtrlText;

      xmlCtrlWrapper = new TiXmlElement(XML_NAME);
      xmlCtrlText = new TiXmlText(leftNameInput->GetValue().c_str());
      xmlCtrlWrapper->InsertEndChild(*xmlCtrlText);
      xmlTreeCtrl.InsertEndChild(*xmlCtrlWrapper);

      xmlCtrlWrapper = new TiXmlElement(XML_STOPEXPR);
      xmlCtrlText = new TiXmlText(leftStopExpr->GetValue().c_str());
      xmlCtrlWrapper->InsertEndChild(*xmlCtrlText);
      xmlTreeCtrl.InsertEndChild(*xmlCtrlWrapper);

      xmlCtrlWrapper = new TiXmlElement(XML_PREDICATES);
      xmlCtrlText = new TiXmlText(leftArgInput_1->GetValue().c_str());
      xmlCtrlWrapper->InsertEndChild(*xmlCtrlText);
      xmlTreeCtrl.InsertEndChild(*xmlCtrlWrapper);

      xmlCtrlWrapper = new TiXmlElement(XML_CUSTOMTEXT);
      xmlCtrlText = new TiXmlText(leftTextSpecMain->GetValue().c_str());
      xmlCtrlWrapper->InsertEndChild(*xmlCtrlText);
      xmlTreeCtrl.InsertEndChild(*xmlCtrlWrapper);

      xmlStep.InsertEndChild(xmlTreeCtrl);


      // bottom code state - really simple
      TiXmlElement xmlCodeInfo(XML_CODEVIEW);
      xmlCodeInfo.SetAttribute(XML_CV_GENERATED, bottomCodeMade == true ? 1 : 0);
      xmlCodeInfo.SetAttribute(XML_CV_POS, (int)bottomCodeDisplay->GetInsertionPoint());
      xmlStep.InsertEndChild(xmlCodeInfo);

      // last steps...
      xmlStateInfo->InsertEndChild(xmlStep);
      if(stateNum == 0)
	stateDoc.InsertEndChild(*xmlStateInfo);
#ifdef DB_ON
      stateDoc.Print();
#endif
      bool stateSaved;
      stateSaved = stateDoc.SaveFile();
      
      if(stateDoc.Error())
	{
	  wxString str;
	  DB_P("XML: error occured %s\n", stateDoc.ErrorDesc());
	  wxLogError("ERROR:  XML generator returned error:\n%s", stateDoc.ErrorDesc());
	}

      // clean up all the dynamically allocated memory used for the leaf nodes
      xmlChecklist.Clear();
      xmlGrid.Clear();
      xmlTree.Clear();
      xmlTreeCtrl.Clear();
      stateDoc.Clear();

      if(stateSaved == true)
	{
	  if(xmlMessagesOnSave == true && catState == false)
	    wxLogMessage("XML state save completed.");
	}
      else
	wxLogWarning("XML state was not saved properly.");

}

int LaunchPADS::ISO_8859_1_process(wxString &str)
{
  int replace_num = 0;

  replace_num += str.Replace("&",  "&amp;",   true);
  replace_num += str.Replace("<",  "&gt;",    true);
  replace_num += str.Replace(">",  "&lt;",    true);
  replace_num += str.Replace("\'", "&apos;",  true);
  replace_num += str.Replace("\"", "&quot;",  true);

  return replace_num;

}

int LaunchPADS::TreeLoadXMLRecur(wxTreeItemId parent,
				  TiXmlElement *sibling, 
				  int depth)
{

  PElement* elem;
  wxString str;
  wxTreeItemId this_id;

  TiXmlElement *child;

  TiXmlElement *xmlLabel;
  TiXmlText    *xmlLabelText;
  TiXmlElement *xmlName;
  TiXmlText    *xmlNameText;

  TiXmlElement *xmlStopExpr;
  TiXmlText    *xmlStopExprText;
  TiXmlElement *xmlPredicate;
  TiXmlText    *xmlPredicateText;
  TiXmlElement *xmlCustom;
  TiXmlText    *xmlCustomText;

  wxString elemStr;

  int expand;
  int type, enctype, termtype, sign, bytes, tdef, flags, where;

  wxString name;
  wxString label;

  int numWarnings = 0;

  for( ;
       sibling != NULL;
       sibling = sibling->NextSiblingElement(XML_TREEITEM))
    {
      xmlLabel = sibling->FirstChild(XML_TREELABEL)->ToElement();
      if(xmlLabel == NULL)
	{
	  wxLogWarning(_T("Element TreeItem:TreeLabel missing \
for anonymous element at depth %d."), depth);
	  numWarnings++;
	  return numWarnings;
	}

      xmlName = sibling->FirstChild(XML_ELEMNAME)->ToElement();
      if(xmlName == NULL)
	{
	  wxLogWarning(_T("Element TreeItem:ElemName missing \
for anonymous element at depth %d."), depth);
	  numWarnings++;
	  return numWarnings;
	}

      xmlLabelText = xmlLabel->FirstChild()->ToText();
      if(xmlLabelText == NULL)
	{
	  wxLogWarning(_T("Element TreeItem:TreeLabel missing text at depth %d."), depth);
	  numWarnings++;
	  label = "";
	}
      else
	{ 
	  label = xmlLabelText->Value(); 
	  if(label.GetChar(0) != '"' ||
	     label.Last()  != '"')
	    {
	      wxLogWarning(_T("Element TreeItem:TreeLabel with label %s formatted incorrectly."), label.c_str());
	      numWarnings++;
	    }
	  else
	    {
	      label.RemoveLast(); // remove last quote
	      label.Replace(_T("\""), _T(""), false); // remove first quote
	    }
	}

      xmlNameText = xmlName->FirstChild()->ToText();      
      if(xmlNameText == NULL)
	{
	  wxLogWarning(_T("Element TreeItem:ElemName missing text at depth %d."), depth);
	  numWarnings++;
	  name = "";
	}
      else
	{ 
	  name = xmlNameText->Value(); 
	  if(name.GetChar(0) != '"' ||
	     name.Last()  != '"')
	    {
	      wxLogWarning(_T("Element TreeItem:ItemName with name %s formatted incorrectly."), name.c_str());
	      numWarnings++;
	    }
	  else
	    {
	      name.RemoveLast(); // remove last quote
	      name.Replace(_T("\""), _T(""), false); // remove first quote
	    }
	}


      if(sibling->QueryIntAttribute(XML_T_EXPANDED, &expand) != TIXML_SUCCESS)
	{
	  // missing the "expand" field isn't such a big deal, so we'll just ignore it
	  //wxLogWarning(_T("Attribute TreeItem(\"%s\") is missing for element %s at depth %d."), 
	  //	XML_TYPE, label.c_str(), depth);
	  expand = 0;
	}

      // A macro could make this more concise, but it if we formalize the XML terms it would break severely
      if(sibling->QueryIntAttribute(XML_TYPE, &type) != TIXML_SUCCESS)
	{
	  wxLogWarning(_T("Attribute TreeItem(\"%s\") is missing for element %s at depth %d."), 
			XML_TYPE, label.c_str(), depth);
	  numWarnings++;
	}
      if(sibling->QueryIntAttribute(XML_T_ENCTYPE, &enctype) != TIXML_SUCCESS)
	{
	  wxLogWarning(_T("Attribute TreeItem(\"%s\") is missing for element %s at depth %d."), 
			XML_T_ENCTYPE, label.c_str(), depth);
	  numWarnings++;
	}
      if(sibling->QueryIntAttribute(XML_TERMTYPE, &termtype) != TIXML_SUCCESS)
	{
	  wxLogWarning(_T("Attribute TreeItem(\"%s\") is missing for element %s at depth %d."), 
			XML_TERMTYPE, label.c_str(), depth);
	  numWarnings++;
	}
      if(sibling->QueryIntAttribute(XML_SIGN, &sign) != TIXML_SUCCESS)
	{
	  wxLogWarning(_T("Attribute TreeItem(\"%s\") is missing for element %s at depth %d."), 
			XML_SIGN, label.c_str(), depth);
	  numWarnings++;
	}
      if(sibling->QueryIntAttribute(XML_BYTES, &bytes) != TIXML_SUCCESS)
	{
	  wxLogWarning(_T("Attribute TreeItem(\"%s\") is missing for element %s at depth %d."), 
			XML_BYTES, label.c_str(), depth);
	  numWarnings++;
	}
      if(sibling->QueryIntAttribute(XML_TYPEDEF, &tdef) != TIXML_SUCCESS)
	{
	  wxLogWarning(_T("Attribute TreeItem(\"%s\") is missing for element %s at depth %d."), 
			XML_TYPEDEF, label.c_str(), depth);
	  numWarnings++;
	}
      if(sibling->QueryIntAttribute(XML_T_FLAGS, &flags) != TIXML_SUCCESS)
	{
	  wxLogWarning(_T("Attribute TreeItem(\"%s\") is missing for element %s at depth %d."), 
		     XML_T_FLAGS, label.c_str(), depth);
	  numWarnings++;
	}
      if(sibling->QueryIntAttribute(XML_T_WHERE, &where) != TIXML_SUCCESS)
	{
	  wxLogWarning(_T("Attribute TreeItem(\"%s\") is missing for element %s at depth %d."), 
		       XML_T_WHERE, label.c_str(), depth);
	  numWarnings++;
	}


      elem = new PElement(type, 
			  name,
			  flags,
			  enctype,
			  sign == 1 ? pSigned : pUnsigned,
			  termtype,
			  bytes, 
			  tdef == 1 ? true : false
			  );
      elem->SetLineNumber((long) where);
      

      xmlStopExpr = sibling->FirstChild(XML_STOPEXPR)->ToElement();
      if(xmlStopExpr != NULL)
	{
	  xmlStopExprText = xmlStopExpr->FirstChild()->ToText();
	  if(xmlStopExprText != NULL)
	    {
	      elemStr = xmlStopExprText->Value();
	      elem->SetStopExpr(elemStr);
	    }
	}

      for(xmlPredicate = sibling->FirstChild(XML_PREDICATE)->ToElement();
	  xmlPredicate != NULL;
	  xmlPredicate = xmlPredicate->NextSiblingElement(XML_PREDICATE))
	{
	  xmlPredicateText = xmlPredicate->FirstChild()->ToText();
	  if(xmlPredicateText != NULL)
	    {
	      elemStr = xmlPredicateText->Value();
	      elem->AddArg(elemStr);
	    }
	}


      xmlCustom = sibling->FirstChild(XML_CUSTOMTEXT)->ToElement();
      if(xmlCustom != NULL)
	{
	  xmlCustomText = xmlCustom->FirstChild()->ToText();
	  if(xmlCustomText != NULL)
	    {
	      elemStr = xmlCustomText->Value();
	      elem->AddClause(elemStr);
	    }
	}


      this_id = leftTreeCtrl->AppendItem(parent, label, -1, -1, elem);

      LeftSetItemBackgroundColour(this_id, type);

      if(PElement::IsPNonTerminal(type))
	{
	  child = sibling->FirstChild(XML_TREEITEM)->ToElement();
	  if(child != NULL)
	    numWarnings += TreeLoadXMLRecur(this_id, child, depth+1);
	}
      if(expand == 1)
	leftTreeCtrl->Expand(this_id);
    }   
  return numWarnings;
}

void LaunchPADS::TreeBuildXMLRecur(wxTreeItemId sibling, 
				   TiXmlElement *parent, 
				   wxTreeItemIdValue cookie)
{
  wxTreeItemId child;
  TiXmlElement *xmlSibling;
  TiXmlElement *xmlSubSibling;
  PElement *sibData;
  TiXmlText *sibText;
  wxString str;
  wxColour sibBGColour;
  wxColour sibTextColour;
  // TiXmlElement xmlColourSpec("NodeColor");

  DB_P("working on new node...  ", str.c_str());

  for( ; sibling.IsOk(); sibling = leftTreeCtrl->GetNextSibling(sibling))
    {
      sibData = (PElement*)leftTreeCtrl->GetItemData(sibling);
      xmlSibling = new TiXmlElement(XML_TREEITEM);

      xmlSibling->SetAttribute(XML_T_EXPANDED, leftTreeCtrl->IsExpanded(sibling) == true ? 1 : 0);

      xmlSibling->SetAttribute(XML_TYPE, sibData->GetType());
      xmlSibling->SetAttribute(XML_T_ENCTYPE, sibData->GetEnctype());
      xmlSibling->SetAttribute(XML_TERMTYPE, sibData->GetTermtype());
      xmlSibling->SetAttribute(XML_SIGN, sibData->GetSign() == true ? 1 : 0);
      xmlSibling->SetAttribute(XML_BYTES, sibData->GetByteSize());
      xmlSibling->SetAttribute(XML_TYPEDEF, sibData->GetIsTypedef() == true ? 1 : 0);
      xmlSibling->SetAttribute(XML_T_FLAGS, sibData->GetFlags());
      xmlSibling->SetAttribute(XML_T_WHERE, sibData->GetLineNumber());

      /*
      sibBGColour   = leftTreeCtrl->GetItemBackgroundColour(sibling);
      sibTextColour = leftTreeCtrl->GetItemTextColour(sibling);
      xmlColourSpec.SetAttribute("bgR",  (int)sibBGColour.Red());
      xmlColourSpec.SetAttribute("bgG",  (int)sibBGColour.Green());
      xmlColourSpec.SetAttribute("bgB",  (int)sibBGColour.Blue());
      xmlColourSpec.SetAttribute("txtR", (int)sibTextColour.Red());
      xmlColourSpec.SetAttribute("txtG", (int)sibTextColour.Green());
      xmlColourSpec.SetAttribute("txtB", (int)sibTextColour.Blue());
      xmlSibling->InsertEndChild(xmlColourSpec);
      */

      str = leftTreeCtrl->GetItemText(sibling);
      str.Prepend('"').Append('"');
      sibText = new TiXmlText(str.c_str());
      xmlSubSibling = new TiXmlElement(XML_TREELABEL);
      xmlSubSibling->InsertEndChild(*sibText);
      xmlSibling->InsertEndChild(*xmlSubSibling);

      //DB_P("working on node %s\n", str.c_str());

      sibData->GetName(str);
      str.Prepend('"').Append('"');
      sibText = new TiXmlText(str.c_str());
      DB_P("saving xml tree with name \"%s\"\n", str.c_str());
      xmlSubSibling = new TiXmlElement(XML_ELEMNAME);
      xmlSubSibling->InsertEndChild(*sibText);
      xmlSibling->InsertEndChild(*xmlSubSibling);

      if(sibData->HasStopExpr())
	{
	  sibData->GetStopExpr(str);
	  sibText = new TiXmlText(str.c_str());
	  xmlSubSibling = new TiXmlElement(XML_STOPEXPR);
	  xmlSubSibling->InsertEndChild(*sibText);
	  xmlSibling->InsertEndChild(*xmlSubSibling);
	}

      int numArgs = sibData->GetNumArgs();
      for(int i = 0; i < numArgs; i++)
	{
	  sibData->GetArgAtIndex(i, str);
	  sibText = new TiXmlText(str.c_str());
	  xmlSubSibling = new TiXmlElement(XML_PREDICATE);
	  xmlSubSibling->InsertEndChild(*sibText);
	  xmlSibling->InsertEndChild(*xmlSubSibling);
	}

      if(sibData->HasClause())
	{
	  sibData->GetClause(str);
	  sibText = new TiXmlText(str.c_str());
	  xmlSubSibling = new TiXmlElement(XML_CUSTOMTEXT);
	  xmlSubSibling->InsertEndChild(*sibText);
	  xmlSibling->InsertEndChild(*xmlSubSibling);
	}

      child = leftTreeCtrl->GetFirstChild(sibling, cookie);
      if(child.IsOk())
	{
	  TreeBuildXMLRecur(child, xmlSibling, cookie);
	}

      parent->InsertEndChild(*xmlSibling);
    }
  return;
}

/* ************************************ */
