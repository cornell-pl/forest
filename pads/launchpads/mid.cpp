/* **************************************
 * PADS GUI project (preliminary tests)
 * Middle frame related code
 * *********************************** */
#include "lp_includes.h"
#include "pads_gui.h"

/* **************************************
 * Functions:
 *  --init
 *  MidInitFrame
 *  MidGridTest
 *  MidGridRemake
 *
 *  --event handlers
 *  OnMidGridSelect
 *  OnMidGridDClick
 *  OnMidGridRightDClick
 *
 *  --interface
 *  MidGridApplySelectedStats  --get attributes of cell @ right d-click and send to controls
 *  MidSetEditMode             --change edit mode based (none, lit, term, nonterm)
 *  MidGridUseAsInput          --use string to remake grid
 *  MidGridNextLevel           --fragment (advance to next step, leaving terminal regions behind)
 *  MidGridLastLevel           --un-fragment
 *  MidGridStartOver           --undo all fragmentation, start with fresh grid
 *  MidGridLevelsResize        --resize the arrays holding cell info in case to prevent buffer overrun (*untested*, hopefully unnecessary)
 *  MidGridReadyForTree        --check for additional fragmentation opportunities, allow tree to be built if none
 *  MidGridMakeTreeFromParse   --construct tree according to grid layout (parse grid, build tree)
 *  MidGridRecur               --*DO NOT CALL DIRECTLY* auxiliary function for MidGridMakeTreeFromParse
 *  MidGridTermGuessMode       --attempt to infer terminal type based on simple analysis of content (could use extension)
 *  IsSign                     --auxiliary character class analysis functions for MidGridTermGuessMode
 *  IsFloat                    -- |
 *  IsHex                      -- |
 *  IsSymbol                   -- |
 *  IsSymbol                   -- |
 *  IsFloat                    -- |
 *  IsHex                      -- |
 *  IsSign                     -- |
 *  MidGridGetMode             --get current edit mode
 *  MidGridNewUDefColour       --set nonterminal grid color to next in cycle
 *  MidGridNewTermColour       --set terminal grid color to next in cycle
 * *********************************** */


void LaunchPADS::MidInitFrame(long panelflags, 
			     long gridflags)
{
  m_midWinPanel = new wxPanel(m_midWin, 
			      wxID_ANY, 
			      wxDefaultPosition,
			      wxDefaultSize,
			      //m_midWin->GetSize(),
			      panelflags);
  
  midCurrentRow = 0;
  midEditMode = MID_MODE_NONE;
  midGridExists = false;

  // see the header file for info about these
  midGridText     = "";
  midGridDepth    = NULL;
  midGridMode     = NULL;
  midGridType     = NULL;
  midGridUDefID   = NULL;


  // create, empty, set defaults, and make a grid
  midGridTypeLevels = new int*[MID_GRID_INIT_MAX_STEPS+1];   // fixes compilation problem under Carbon
  midGridUDefIDLevels   = new int*[MID_GRID_INIT_MAX_STEPS+1];
  midGridMaxLevels = MID_GRID_INIT_MAX_STEPS;

  for(int i = 0; i < midGridMaxLevels; i++) 
    {
      midGridTypeLevels[i] = NULL;
      midGridUDefIDLevels[i] = NULL;
    }


  if(midGridTypeLevels == NULL)
    {
      DB_P("Error: mid grid type levels == null\n");
      exit(1);
    }
  if(midGridUDefIDLevels == NULL)
    {
      DB_P("Error: mid udef id levels == null\n");
      exit(1);
    }


  midWinSizer = new wxBoxSizer(wxHORIZONTAL);

#ifdef DB_ON
  //  wxString test = "127.0.0.1:00";
  wxString test = "<<< LaunchPADS >>>";  
  MidGridUseAsInput(test);
#else
  wxString test = "<<< LaunchPADS >>>";
  //wxString test = "";
  MidGridUseAsInput(test);
#endif


  midHighlightColour.Set(255, 255, 255);
  midNormalColour.Set(255, 255, 255);


  if(colourModeOn)
    {
      midNoneColour.Set(MID_MODE_NONE_COLOUR);
      midDelimColour.Set(MID_MODE_DELIM_COLOUR);
      midTermColour.Set(MID_MODE_TERM_COLOUR);
      midUDefColour.Set(MID_MODE_UDEF_COLOUR);
    }
  else
    {
      leftTreeBackgroundColourOn = false;
      leftTreeTextColourOn = false;

      midNoneColour.Set(MID_MODE_NONE_NO_COLOUR);
      midDelimColour.Set(MID_MODE_DELIM_NO_COLOUR);
      midTermColour.Set(MID_MODE_TERM_NO_COLOUR);
      midUDefColour.Set(MID_MODE_UDEF_NO_COLOUR);
    }

  //  MidGridTest();

  //  This actually works!  Alright!
  wxAcceleratorEntry accels[5];
  accels[0].Set(wxACCEL_CTRL, (int)'N', MID_SELECT_NONE);
  accels[1].Set(wxACCEL_CTRL, (int)'D', MID_SELECT_DELIM);
  accels[2].Set(wxACCEL_CTRL, (int)'T', MID_SELECT_TERM);
  accels[3].Set(wxACCEL_CTRL, (int)'F', MID_SELECT_UDEF);
  accels[4].Set(wxACCEL_CTRL, (int)'R', MID_SELECT_GET);
  wxAcceleratorTable accel_table(5, accels);
  m_midWinPanel->SetAcceleratorTable(accel_table);
  

  m_midWinPanel->SetSizer(midWinSizer);
  m_midWinPanel->SetAutoLayout(true);
  midWinSizer->SetSizeHints(m_midWinPanel);
}

void LaunchPADS::MidGridTest(void)
{

  // another defunct widget test function, left here in case we want to run more test

  /* 
  midGrid->CreateGrid(1, 20);
  midGrid->EnableEditing(false);
  midGrid->EnableDragGridSize(false);
  midGrid->DisableDragColSize();
  midGrid->DisableDragRowSize();
  midGrid->HideCellEditControl();
  
  char mystring[] = "127.0.0.1:23";
  int mystrlength = strlen(mystring);

  wxString mywxstr;
  for(int i = 0; i < mystrlength; i++)
    {
      mywxstr = mystring[i];
      midGrid->SetCellValue(0, i, mywxstr);
    }
  
  
  midGrid->SetCellValue(0, 0, "1");
  midGrid->SetCellValue(0, 1, "2");
  midGrid->SetCellValue(0, 2, "7");
  midGrid->SetCellValue(0, 3, ".");
  midGrid->SetCellValue(0, 4, "0");
  midGrid->SetCellValue(0, 5, ".");
  midGrid->SetCellValue(0, 6, "0");
  midGrid->SetCellValue(0, 7, ".");
  midGrid->SetCellValue(0, 8, "1");
  midGrid->SetCellValue(0, 9, ":");
  midGrid->SetCellValue(0, 10, "2");
  midGrid->SetCellValue(0, 11, "2");
  
  midGridExists = true;
  midCurrentRow = 0;

  midGrid->SetDefaultCellAlignment(wxALIGN_CENTER, wxALIGN_CENTER);

  midGrid->AutoSize();
  midGrid->Refresh();
  */
}

// create a new initial highlighting grid from a string
void LaunchPADS::MidGridRemake(wxString& wxstr,
			      long gridflags = wxTAB_TRAVERSAL | mywxBORDER)
{
  int wxstrlen = wxstr.Length();

  // create the grid if we've never used it before
  if(midGridExists == false)
    {
      midGrid = new wxGrid(m_midWinPanel, 
			   MID_GRID_EVENT,
			   wxDefaultPosition,
			   wxDefaultSize,
			   //m_midWinPanel->GetSize(),
			   gridflags);

      midGrid->CreateGrid(1, wxstrlen);

      // lock the grid so the user can only highlight
      midGrid->EnableEditing(false);
      midGrid->EnableDragGridSize(false);
      midGrid->DisableDragColSize();
      midGrid->DisableDragRowSize();
      midGrid->HideCellEditControl();
      midGrid->SetDefaultCellAlignment(wxALIGN_CENTER, wxALIGN_CENTER);
      //midGrid->SetBackgroundColour(midNoneColour);
#ifdef __WXMAC__
      // NEVER EVER do this for anything but Carbon - breaks TERRIBLY under GTK
      midGrid->SetDefaultCellBackgroundColour(wxColour(255, 255, 255));
#endif

      midWinSizer->Add(midGrid, 1,  wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT, 5);
      midWinSizer->Layout();
      midGridExists = true;
    }

  if(midGridExists == true)
    {  
      // start over with a clean slate
      midGrid->ClearGrid();

      for( ; midCurrentRow >= 0; midCurrentRow--)
	{
	  DB_P("deleting row %d\n", midCurrentRow);
	  midGrid->DeleteRows(midCurrentRow);
	}
      midGrid->AppendRows();      
      midCurrentRow = 0;

      // fix the grid's length to match the new string
      int num_cols = midGrid->GetNumberCols();
      if(num_cols < wxstrlen)
      {
	midGrid->AppendCols(wxstrlen - num_cols);
      }

      if(num_cols > wxstrlen)
      {
	midGrid->DeleteCols(wxstrlen, num_cols - wxstrlen);
      }

      // fill the cells one at a time with characters from the string
      wxString sendstr(""); 
      for(int i = 0; i < wxstrlen; i++)
	{
	  sendstr = wxstr[i];
	  midGrid->SetCellValue(0, i, sendstr);
	}

      // reset the labels should the user desire that they be numbers
      if(midGridColNumberLabels)
	{
	  wxString num_str;
	  for(int i = 0; i < wxstrlen; i++)
	    {
	      num_str.Printf("%d", i);
	      midGrid->SetColLabelValue(i, num_str);
	    }
	}

      // hmm... this seems to be a bit buggy on MSW
      midGrid->AutoSize();
      midWinSizer->Layout();

      int col_width = midGrid->GetColSize(0) + 2;
      DB_P("col width = %d\n", col_width);
      for(int i = 0; i < wxstrlen; i++)
	{
	  midGrid->SetColSize(i, col_width);
	}

      m_midWinPanel->Refresh();

      wxLayoutAlgorithm layout;
      layout.LayoutFrame(this);
    }
}

/* ************************************ */


// update internal structurs and grid background color on user 
// grid region selection
void LaunchPADS::OnMidGridSelect(wxGridRangeSelectEvent &event)
{
  // allow selections which only include the current row
  if(event.Selecting() && event.GetTopRow() == midCurrentRow)
    {
      int left, right;

      left  = event.GetLeftCol();
      right = event.GetRightCol();

      // adjust element characteristics based on edit mode and right panel selections
      switch(midEditMode)
	{
	case MID_MODE_TERM:
	  {
	    int term_type = RightGetTermType();

	    midHighlightColour = midTermColour;
	    // cycle the terminal type color after we've used the current one
	    MidGridNewTermColour(midCurrentID, MID_MODE_TERM_COL_STEPS); 
	    DB_P("term color: %d: %d %d %d\n", term_type, MID_MODE_TERM_VARCOL(midCurrentID, MID_MODE_TERM_COL_STEPS));

	    // only bother with previous level checks if we've passed the first round of region selection
	    if(midCurrentRow > 0)
	      {	      
		// make sure enum fields don't show up outside of enums
		// and enums only contain enum fields
		if(term_type == pEnumField &&
		   midGridTypeLevels[midCurrentRow-1][left] != pEnum)
		  {
		    if(midTermErrorMBoxOn == true)
		      { 
			wxMessageBox(_T("Elements of Enum Field type may only be assigned inside of Enums."), 
				     _T("Illegal Enum Field Assignment"),
				     wxICON_ERROR | wxOK);
		      }
		    else
		      {
			wxBell();
		      }

		    break;
		  }
		else if(term_type != pEnumField &&
		   midGridTypeLevels[midCurrentRow-1][left] == pEnum)
		  {
		    if(midUDefErrorMBoxOn == true)
		      {
			if(midTermErrorMBoxOn == true)
			  {
			    wxMessageBox(_T("Nonterminal groups of type Enum may only contain elements of type Enum Field."), 
					 _T("Illegal Enum Contents Assignment"),
					 wxICON_ERROR | wxOK);
			  }
			else
			  {
			    wxBell();
			  }

			break;
		      }		
		  }

		// make sure we get only one child element per Popt
		if(term_type == pOpt && midGridTypeLevels[midCurrentRow-1][left] == pOpt)
		  {
		    if(midTermErrorMBoxOn == true)
		      {
			wxMessageBox(_T("Elements of type Popt may not have immediate children of type Popt."), 
				     _T("Illegal Popt Contents Assignment"),
				     wxICON_ERROR | wxOK);
		      }
		    else
		      {
			wxBell();
		      }
		    break;	    
		  }

		bool opt_warning = false;
		if(left != 0)
		  {
		    DB_P("check for pOpt: left = %d, right = %d\n", left, right);
		    if((midGridTypeLevels[midCurrentRow-1][left]    == pOpt &&
			midGridTypeLevels[midCurrentRow-1][left-1] == pOpt) || 
		       (midGridTypeLevels[midCurrentRow-1][left]    == pArray &&
			midGridTypeLevels[midCurrentRow-1][left-1] == pArray))
		      {
			opt_warning = true;
		      }
		  }
		if(right < midGridTextLength)
		  { // counting on a short circuit here...
		    if((midGridTypeLevels[midCurrentRow-1][right]   == pOpt && 
		       midGridTypeLevels[midCurrentRow-1][right+1] == pOpt) ||
		       (midGridTypeLevels[midCurrentRow-1][right]   == pArray && 
			midGridTypeLevels[midCurrentRow-1][right+1] == pArray))
		      {
			opt_warning = true; 
		      }
		  }
		if(opt_warning == true)
		  {
		    if(midTermErrorMBoxOn == true)
		      {
			wxMessageBox(_T("Only one element may be contained within nonterminal elements of type Popt or Parray."), 
				     _T("Illegal Popt Contents Assignment"),
				     wxICON_ERROR | wxOK);
		      }
		    else
		      {
			wxBell();
		      }
		    break;
		  }


		// this will be in bounds since the actual size of the string/array never shrinks
		int last_udefid = midGridUDefIDLevels[midCurrentRow-1][left];
		for(int i = left; i <= right; i++)
		  {
		    if(midGridDepth[i] == midCurrentRow)
		      {
			// if we find we're highlighting accross udef ids in the last level,
			// stop where we are and end the class here
			if(midGridUDefIDLevels[midCurrentRow-1][i] != last_udefid)
			  {
			    if(midTermErrorMBoxOn == true)
			      {
				wxMessageBox(_T("New terminal types may not cross nonterminal boundaries from the previous stage."),
					     _T("Selection Error"),
					     wxICON_ERROR | wxOK);
			      }
			    else
			      {
				wxBell();
			      }
			    break;
			  }
			last_udefid = midGridUDefIDLevels[midCurrentRow-1][i];

			midGrid->SetCellBackgroundColour(midCurrentRow, i, midHighlightColour);
			midGridMode[i]  = midEditMode;

			midGridType[i] = term_type;
			midGridUDefID[i]   = midCurrentID;
			if(term_type == pChar)
			  {
			    midCurrentID++;
			    midHighlightColour = midTermColour;
			    MidGridNewTermColour(midCurrentID, MID_MODE_TERM_COL_STEPS);
			  }
		      }
		  }
	      }
	    else
	      {
		// make sure we don't use enum fields before we can apply an enum
		if(term_type == pEnumField)
		  {
		    if(midTermErrorMBoxOn == true)
		      {
			wxMessageBox(_T("Elements of Enum Field type may only be assigned inside of Enums"), 
				     _T("Illegal Enum Field Assignment"),
				     wxICON_ERROR | wxOK);
		      }
		    else
		      {
			wxBell();
		      }

			break;
		  }

		for(int i = left; i <= right; i++)
		  {
		    if(midGridDepth[i] == midCurrentRow)
		      {
			midGrid->SetCellBackgroundColour(midCurrentRow, i, midHighlightColour);
			midGridMode[i]  = midEditMode;
			
			midGridType[i] = term_type;
			midGridUDefID[i]   = midCurrentID;
			if(term_type == pChar)
			  {
			    midCurrentID++;
			    midHighlightColour = midTermColour;
			    MidGridNewTermColour(midCurrentID, MID_MODE_TERM_COL_STEPS);
			  }
		      }
		  }
	      }
	    midCurrentID++;
	    break;
	  }
	case MID_MODE_UDEF:
	  {
	    int struct_type = RightGetNontermType();
	    
	    // use a new color for each udef group, just like above
	    midHighlightColour = midUDefColour;
	    MidGridNewUDefColour(midCurrentID, MID_MODE_UDEF_COL_STEPS);
	    DB_P("new udef color: %d %d %d\n", MID_MODE_UDEF_VARCOL(midCurrentID, MID_MODE_UDEF_COL_STEPS));

	    if(midCurrentRow > 0)
	      {	      
		if(midGridTypeLevels[midCurrentRow-1][left] == pEnum)
		  {
		    if(midUDefErrorMBoxOn == true)
		      { 
			wxMessageBox(_T("Nonterminal groups of type Enum may only contain elements of type Enum Field."), 
				     _T("Illegal Enum Contents Assignment"),
				     wxICON_ERROR | wxOK);
		      }
		    else
		      {
			wxBell();
		      }
		    break;
		  }


		// make sure we get only one child element per Popt
		if(struct_type == pOpt && midGridTypeLevels[midCurrentRow-1][left] == pOpt)
		  {
		    if(midUDefErrorMBoxOn == true)
		      {
			wxMessageBox(_T("Elements of type Popt may not have immediate children of type Popt."), 
				     _T("Illegal Popt Contents Assignment"),
				     wxICON_ERROR | wxOK);
		      }
		    else
		      {
			wxBell();
		      }
		    break;	    
		  }

		bool opt_warning = false;
		if(left != 0)
		  {
		    DB_P("check for pOpt: left = %d, right = %d\n", left, right);
		    if((midGridTypeLevels[midCurrentRow-1][left]    == pOpt &&
			midGridTypeLevels[midCurrentRow-1][left-1] == pOpt) || 
		       (midGridTypeLevels[midCurrentRow-1][left]    == pArray &&
			midGridTypeLevels[midCurrentRow-1][left-1] == pArray))				    
		      {
			opt_warning = true;
		      }
		  }
		if(right < midGridTextLength)
		  { // counting on a short circuit here...
		    if((midGridTypeLevels[midCurrentRow-1][right]   == pOpt && 
		       midGridTypeLevels[midCurrentRow-1][right+1] == pOpt) ||
		       (midGridTypeLevels[midCurrentRow-1][right]   == pArray && 
			midGridTypeLevels[midCurrentRow-1][right+1] == pArray))
		      {
			opt_warning = true; 
		      }
		  }
		if(opt_warning == true)
		  {
		    if(midUDefErrorMBoxOn == true)
		      {
			wxMessageBox(_T("Only one element may be contained within nonterminal elements of type Popt or Parray."), 
				     _T("Illegal Popt Contents Assignment"),
				     wxICON_ERROR | wxOK);
		      }
		    else
		      {
			wxBell();
		      }
		    break;
		  }

		// do udef region bounds checking for all rows that might have parent composite types
		// this prevents users from defining new structs/unions comprising a larger part of each 
		// entry than their parent structs/unions (which should be impossible to do in PADS code)
		
		// this will be in bounds since the actual size of the string/array never shrinks
		int last_udefid = midGridUDefIDLevels[midCurrentRow-1][left];
		for(int i = left; i <= right; i++)
		  {
		    if(midGridDepth[i] == midCurrentRow)
		      {
			// if we find we're highlighting accross udef ids in the last level,
			// stop where we are and end the class here
			if(midGridUDefIDLevels[midCurrentRow-1][i] != last_udefid)
			  {
			    if(midUDefErrorMBoxOn == true)
			      {
				wxMessageBox(_T("New nonterminal types may not cross nonterminal boundaries from the previous stage."),
					     _T("Selection Error"),
					     wxICON_ERROR | wxOK);
			      }
			    else
			      {
				wxBell();
			      }
			    break;
			  }
			// this probably isn't necessary, but we'll do it anyway just to be safe
			last_udefid = midGridUDefIDLevels[midCurrentRow-1][i];
			
			midGrid->SetCellBackgroundColour(midCurrentRow, i, midHighlightColour);
			midGridMode[i]  = midEditMode;
			
			midGridType[i] = struct_type;
			midGridUDefID[i]   = midCurrentID;
		      }
		  }
	      }
	    else
	      {
		for(int i = left; i <= right; i++)
		  {
		    //if(midGridDepth[i] == midCurrentRow) // this is the first level, so row values must all be 0
		      {			
			midGrid->SetCellBackgroundColour(midCurrentRow, i, midHighlightColour);
			midGridMode[i]  = midEditMode;
			
			midGridType[i] = struct_type;
			midGridUDefID[i]   = midCurrentID;
		      }
		  }
	      }
	    midCurrentID++;
	    break;
	  }
	case MID_MODE_DELIM:
	  {
	    if(midCurrentRow > 0)
	      {
		if(midGridTypeLevels[midCurrentRow-1][left] == pEnum)
		  {
		    if(midTermErrorMBoxOn == true)
		      { 
			wxMessageBox(_T("Nonterminal groups of type Enum may only contain elements of type Enum Field."), 
				     _T("Illegal Enum Contents Assignment"),
				     wxICON_ERROR | wxOK);
		      }
		    else
		      {
			wxBell();
		      }
		    break;
		  }

		// make sure we get only one child element per Popt
		bool opt_warning = false;
		if(left != 0)
		  {
		    DB_P("check for pOpt: left = %d, right = %d\n", left, right);
		    if((midGridTypeLevels[midCurrentRow-1][left]    == pOpt &&
			midGridTypeLevels[midCurrentRow-1][left-1] == pOpt) || 
		       (midGridTypeLevels[midCurrentRow-1][left]    == pArray &&
			midGridTypeLevels[midCurrentRow-1][left-1] == pArray))				    
		      {
			opt_warning = true;
		      }
		  }
		if(right < midGridTextLength)
		  { // counting on a short circuit here...
		    if((midGridTypeLevels[midCurrentRow-1][right]   == pOpt && 
		       midGridTypeLevels[midCurrentRow-1][right+1] == pOpt) ||
		       (midGridTypeLevels[midCurrentRow-1][right]   == pArray && 
			midGridTypeLevels[midCurrentRow-1][right+1] == pArray))
		      {
			opt_warning = true; 
		      }
		  }
		if(opt_warning == true)
		  {
		    if(midTermErrorMBoxOn == true)
		      {
			wxMessageBox(_T("Only one element may be contained within nonterminal elements of type Popt or Parray."), 
				     _T("Illegal Popt Contents Assignment"),
				     wxICON_ERROR | wxOK);
		      }
		    else
		      {
			wxBell();
		      }
		    break;
		  }
	      }

	    for(int i = left; i <= right; i++)
	      {
		if(midGridDepth[i] == midCurrentRow)
		  {
		    midGrid->SetCellBackgroundColour(midCurrentRow, i, midHighlightColour);
		    midGridMode[i]  = midEditMode;

		    midGridType[i] = pLit;
		    midGridUDefID[i]   = MID_NO_ID;
		  }
	      }
	    break;
	  }
	default: // mode == none
	  {
	    for(int i = left; i <= right; i++)
	      {
		if(midGridDepth[i] == midCurrentRow)
		  {
		    midGrid->SetCellBackgroundColour(midCurrentRow, i, midHighlightColour);
		    midGridMode[i]  = midEditMode;

		    midGridType[i] = pUndefined;
		    midGridUDefID[i]   = MID_NO_ID;
		  }
	      }
	    break;
	  }
	}
    }
  return;
}


// same as above, adapted for a single cell
void LaunchPADS::OnMidGridDClick(wxGridEvent &event)
{  
  int i = event.GetCol();

  if(midGridDepth[i] == midCurrentRow && event.GetRow() == midCurrentRow)
    {     
      switch(midEditMode)
	{
	case MID_MODE_TERM:
	  {
	    int term_type = RightGetTermType();
	    if(midCurrentRow == 0)
	      {
		if(term_type == pEnumField)
		  {
		    if(midTermErrorMBoxOn == true)
		      { 
			wxMessageBox(_T("Elements of Enum Field type may only be assigned inside of Enums"), 
				     _T("Illegal Enum Field Assignment"),
				     wxICON_ERROR | wxOK);
		      }
		    else
		      {
			wxBell();
		      }

		    break;
		  }
	      }
	    else
	      {
		if(term_type == pEnumField &&
		   midGridTypeLevels[midCurrentRow-1][i] != pEnum)
		  {
		    if(midTermErrorMBoxOn == true)
		      { 
			wxMessageBox(_T("Elements of Enum Field type may only be assigned inside of Enums."), 
				     _T("Illegal Enum Field Assignment"),
				     wxICON_ERROR | wxOK);
		      }
		    else
		      {
			wxBell();
		      }

		    break;
		  }
		else if(term_type != pEnumField &&
			midGridTypeLevels[midCurrentRow-1][i] == pEnum)
		  {
		    if(midTermErrorMBoxOn == true)
		      { 
			wxMessageBox(_T("Nonterminal groups of type Enum may only contain elements of type Enum Field."), 
				     _T("Illegal Enum Contents Assignment"),
				     wxICON_ERROR | wxOK);
		      }
		    else
		      {
			wxBell();
		      }
		    break;
		  }

		// make sure we get only one child element per Popt

		if(term_type == pOpt && midGridTypeLevels[midCurrentRow-1][i] == pOpt)
		  {
		    if(midUDefErrorMBoxOn == true)
		      {
			wxMessageBox(_T("Elements of type Popt may not have immediate children of type Popt."), 
				     _T("Illegal Popt Contents Assignment"),
				     wxICON_ERROR | wxOK);
		      }
		    else
		      {
			wxBell();
		      }
		    break;	    
		  }

		bool opt_warning = false;
		if(i != 0)
		  {
		    DB_P("check for pOpt: i = %d\n", i);
		    if((midGridTypeLevels[midCurrentRow-1][i]    == pOpt &&
			midGridTypeLevels[midCurrentRow-1][i-1] == pOpt) ||
		       (midGridTypeLevels[midCurrentRow-1][i]    == pArray &&
			midGridTypeLevels[midCurrentRow-1][i-1] == pArray))
		      {
			opt_warning = true;
		      }
		  }
		if(i < midGridTextLength)
		  { // counting on a short circuit here...
		    if((midGridTypeLevels[midCurrentRow-1][i]   == pOpt && 
			midGridTypeLevels[midCurrentRow-1][i+1] == pOpt) ||
		       (midGridTypeLevels[midCurrentRow-1][i]   == pArray && 
			midGridTypeLevels[midCurrentRow-1][i+1] == pArray))

		      {
			opt_warning = true; 
		      }
		  }
		if(opt_warning == true)
		  {
		    if(midTermErrorMBoxOn == true)
		      {
			wxMessageBox(_T("Only one element may be contained within nonterminal elements of type Popt or Parray."), 
				     _T("Illegal Popt Contents Assignment"),
				     wxICON_ERROR | wxOK);
		      }
		    else
		      {
			wxBell();
		      }
		    break;
		  }
	      }

	    midGridType[i] = term_type;
	    midGridUDefID[i]   = midCurrentID;

	    midHighlightColour = midTermColour; 
	    MidGridNewTermColour(midCurrentID, MID_MODE_TERM_COL_STEPS);
	    DB_P("term color: %d %d: %d %d %d\n", midCurrentID, MID_MODE_TERM_COL_STEPS, MID_MODE_TERM_VARCOL(term_type, MID_MODE_TERM_COL_STEPS));

	    midGrid->SetCellBackgroundColour(midCurrentRow, i, midHighlightColour);
	    midGridMode[i]  = midEditMode;

	    midCurrentID++;
	    break;
	  }
	case MID_MODE_UDEF:
	  {
	    DB_P("udef dclick!\n");

	    int struct_type = RightGetNontermType();

	    if(midCurrentRow > 0)
	      {
		if(midGridTypeLevels[midCurrentRow-1][i] == pEnum)
		  {
		    if(midUDefErrorMBoxOn == true)
		      { 
			wxMessageBox(_T("Nonterminal groups of type Enum may only contain elements of type Enum Field."), 
				     _T("Illegal Enum Contents Assignment"),
				     wxICON_ERROR | wxOK);
		      }
		    else
		      {
			wxBell();
		      }
		    break;
		  }
	    
		// make sure we get only one child element per Popt
		if(struct_type == pOpt && midGridTypeLevels[midCurrentRow-1][i] == pOpt)
		  {
		    if(midUDefErrorMBoxOn == true)
		      {
			wxMessageBox(_T("Elements of type Popt may not have immediate children of type Popt."), 
				     _T("Illegal Popt Contents Assignment"),
				     wxICON_ERROR | wxOK);
		      }
		    else
		      {
			wxBell();
		      }
		    break;	    
		  }

		bool opt_warning = false;
		if(i != 0)
		  {
		    DB_P("check for pOpt: i = %d\n", i);
		    if((midGridTypeLevels[midCurrentRow-1][i]    == pOpt &&
		       midGridTypeLevels[midCurrentRow-1][i-1] == pOpt)	||
		       (midGridTypeLevels[midCurrentRow-1][i]    == pArray &&
			midGridTypeLevels[midCurrentRow-1][i-1] == pArray))
		      {
			opt_warning = true;
		      }
		  }
		if(i < midGridTextLength)
		  { // counting on a short circuit here...
		    if((midGridTypeLevels[midCurrentRow-1][i]   == pOpt && 
		       midGridTypeLevels[midCurrentRow-1][i+1] == pOpt) ||
		       (midGridTypeLevels[midCurrentRow-1][i]   == pArray && 
			midGridTypeLevels[midCurrentRow-1][i+1] == pArray))
		      {
			opt_warning = true; 
		      }
		  }
		if(opt_warning == true)
		  {
		    if(midUDefErrorMBoxOn == true)
		      {
			wxMessageBox(_T("Only one element may be contained within nonterminal elements of type Popt or Parray."), 
				     _T("Illegal Popt Contents Assignment"),
				     wxICON_ERROR | wxOK);
		      }
		    else
		      {
			wxBell();
		      }
		    break;
		  }
	      }

	    midGridType[i] = struct_type;
	    midGridUDefID[i]   = midCurrentID;

	    midHighlightColour = midUDefColour;
	    MidGridNewUDefColour(midCurrentID, MID_MODE_UDEF_COL_STEPS);
	    DB_P("%d %d %d\n", MID_MODE_UDEF_VARCOL(midCurrentID, MID_MODE_UDEF_COL_STEPS));

	    midGrid->SetCellBackgroundColour(midCurrentRow, i, midHighlightColour);
	    midGridMode[i]  = midEditMode;

	    midCurrentID++;
	    break;
	  }
	case MID_MODE_DELIM:
	  {
	    if(midCurrentRow > 0)
	      {
		if(midGridTypeLevels[midCurrentRow-1][i] == pEnum)
		  {
		    if(midUDefErrorMBoxOn == true)
		      { 
			wxMessageBox(_T("Nonterminal groups of type Enum may only contain elements of type Enum Field."), 
				     _T("Illegal Enum Contents Assignment"),
				     wxICON_ERROR | wxOK);
		      }
		    else
		      {
			wxBell();
		      }
		    break;
		  }

		// make sure we get only one child element per Popt
		bool opt_warning = false;
		if(i != 0)
		  {
		    DB_P("check for pOpt: i = %d\n", i);
		    if((midGridTypeLevels[midCurrentRow-1][i]    == pOpt &&
			midGridTypeLevels[midCurrentRow-1][i-1] == pOpt) ||
		       (midGridTypeLevels[midCurrentRow-1][i]    == pArray &&
			midGridTypeLevels[midCurrentRow-1][i-1] == pArray))

		      {
			opt_warning = true;
		      }
		  }
		if(i < midGridTextLength)
		  { // counting on a short circuit here...
		    if((midGridTypeLevels[midCurrentRow-1][i]   == pOpt && 
		       midGridTypeLevels[midCurrentRow-1][i+1] == pOpt) ||
		       (midGridTypeLevels[midCurrentRow-1][i]   == pArray && 
			midGridTypeLevels[midCurrentRow-1][i+1] == pArray))
		      {
			opt_warning = true; 
		      }
		  }
		if(opt_warning == true)
		  {
		    if(midTermErrorMBoxOn == true)
		      {
			wxMessageBox(_T("Only one element may be contained within nonterminal elements of type Popt or Parray."), 
				     _T("Illegal Popt Contents Assignment"),
				     wxICON_ERROR | wxOK);
		      }
		    else
		      {
			wxBell();
		      }
		    break;
		  }
	      }

	    midGridType[i] = pLit;
	    midGridUDefID[i]   = MID_NO_ID;
	    midGrid->SetCellBackgroundColour(midCurrentRow, i, midHighlightColour);
	    midGridMode[i]  = midEditMode;
	    break;
	  }
	default:
	  {
	    midGridType[i] = pUndefined;
	    midGridUDefID[i]   = MID_NO_ID;
	    midGrid->SetCellBackgroundColour(midCurrentRow, i, midHighlightColour);
	    midGridMode[i]  = midEditMode;
	    break;
	  }
	}

      midGrid->Refresh();
    }
  return;
}

// set the current control/mode attributes to the right double clicked cell
void LaunchPADS::OnMidGridRightDClick(wxGridEvent &event)
{
  int col = event.GetCol();
  int row = event.GetRow();
  int mode;

  if(!midGridExists)
    return;

  DB_P("Setting %d %d from %d %d on rclick\n", 
       midGridMode[col],
       midGridType[col], row, col);
  
  if(midGridTypeLevels[row][col] == pUndefined)
    mode = MID_MODE_NONE;
  else if(midGridTypeLevels[row][col] == pLit)
    mode = MID_MODE_DELIM;
  else if(PElement::IsPTerminal(midGridTypeLevels[row][col]))
    mode = MID_MODE_TERM;
  else
    mode = MID_MODE_UDEF;

  RightSetAttr(mode,
	       PElement::IsPTerminal(midGridTypeLevels[row][col]) ? (midGridTypeLevels[row][col] - pStartTerminals - 1) : 0,
	       PElement::IsPTerminal(midGridTypeLevels[row][col]) ?  0 : (midGridTypeLevels[row][col] - pStartNonTerminals - 1));
}

// gets attributes from grid cells... I'm not sure that this is actually used yet
void LaunchPADS::MidGridApplySelectedStats(void)
{

  int col = midGrid->GetGridCursorCol();
  int row = midGrid->GetGridCursorRow();
  int mode;

  if(!midGridExists)
    return;

  DB_P("setting %d %d from %d %d on cursor ctrl-g\n", 
       midGridMode[col],
       midGridType[col], row, col);
  
  if(midGridTypeLevels[row][col] == pUndefined)
    mode = MID_MODE_NONE;
  else if(midGridTypeLevels[row][col] == pLit)
    mode = MID_MODE_DELIM;
  else if(PElement::IsPTerminal(midGridTypeLevels[row][col]))
    mode = MID_MODE_TERM;
  else
    mode = MID_MODE_UDEF;

  RightSetAttr(mode,
	       PElement::IsPTerminal(midGridTypeLevels[row][col]) ? (midGridTypeLevels[row][col] - pStartTerminals - 1) : 0,
	       PElement::IsPTerminal(midGridTypeLevels[row][col]) ?  0 : (midGridTypeLevels[row][col] - pStartNonTerminals - 1));


}

// change the type to assign to selected cells
void LaunchPADS::MidSetEditMode(int new_mode)
{  
  if(midGrid == NULL)
    return;

  if(!midGridExists)
      return;

  RightEnableComboBox_1(false);
  RightEnableComboBox_2(false);
  switch(new_mode)
    {
    case MID_MODE_NONE:
      {
	midEditMode = MID_MODE_NONE;
	DB_P("Mode: None %d\n", midEditMode);
	midHighlightColour = midNoneColour;
	break;
      }
    case MID_MODE_DELIM:
      {
	midEditMode = MID_MODE_DELIM;
	DB_P("Mode: delim %d\n", midEditMode);
	midHighlightColour = midDelimColour;
	break;
      }
    case MID_MODE_TERM:
      {
	midEditMode = MID_MODE_TERM;
	DB_P("Mode: term %d\n", midEditMode);
	// this is done in the selection/DClick handlers, so we don't need to change it here
	RightEnableComboBox_1(true);
	break;
      }
    case MID_MODE_UDEF:
      {
	midEditMode = MID_MODE_UDEF;	
	DB_P("Mode: udef %d\n", midEditMode);
	// this is also done in the selection/DClick handlers, so we don't need to change it here
	RightEnableComboBox_2(true);
	break;
      }
    default:
      midEditMode = MID_MODE_NONE;
      DB_P("Mode: def %d\n", midEditMode);
      midHighlightColour = midNoneColour;
      break;      
    }

  midGrid->Refresh();
  return;
}

// use the string passed as input as the text in the grid
// resize and reformat the grid to match the string
void LaunchPADS::MidGridUseAsInput(wxString& wxstr)
{
  // ADD COPY to struct (save string and create index for typing)
  midGridText = wxstr;

  midCurrentID = 0;

  int text_len = midGridText.Length();
  midGridTextLength = text_len;

  if(midGridDepth != NULL)
    delete midGridDepth;
  midGridDepth = new int[text_len];

  if(midGridMode != NULL)
    delete midGridMode;
  midGridMode = new int[text_len];


  /* // we don't need this any more since it's linked to the level array...
  if(midGridType != NULL)
    delete midGridType;
  */
  midGridType = new int[text_len];

  /*
  if(midGridUDefID != NULL)
    delete midGridUDefID;
  */
  midGridUDefID = new int[text_len];

  for(int i = 0; i <= midCurrentRow; i++) 
    {
      //DB_P("checking type/id at %d of %d\n", i, midGridMaxLevels);
      if(midGridTypeLevels[i] != NULL)
	{
	  DB_P("mid grid type levels delete != null at %d\n", i);
	  delete midGridTypeLevels[i];
	  midGridTypeLevels[i] = NULL;
	}
      if(midGridUDefIDLevels[i] != NULL)
	{
	  DB_P("mid grid udef levels delete != null at %d\n", i);
	  delete midGridUDefIDLevels[i];
	  midGridUDefIDLevels[i] = NULL;
	}
    }
  
#ifdef DB_ON
  for(int i = 0; i < midGridMaxLevels; i++)
    {
      if(midGridTypeLevels[i] != NULL)
	{
	  DB_P("mid grid type levels != null at %d\n", i);
	}
      if(midGridUDefIDLevels[i] != NULL)
	{
	  DB_P("mid grid udef levels != null at %d\n", i);
	}
    }
#endif

  midGridTypeLevels[0] = midGridType;
  midGridUDefIDLevels[0] = midGridUDefID;

  for(int i = 0; i < text_len; i++)
    {
      midGridDepth[i]    = 0;
      midGridMode[i]     = pUndefined;
      // midGridTermType[i] = pUndefined;
      midGridType[i]     = pUndefined;
      midGridUDefID[i]   = -1;
    }
  DB_P("done with use as text: starting remake\n");
  MidGridRemake(midGridText);
}

// "fragment" the grid and move to the next level
void LaunchPADS::MidGridNextLevel(void)
{
  int length = midGridText.Length();
  wxString sendstr(""); 

#ifdef DB_ON
  DB_P( "%8s%8s%8s%8s%8s\n",
	"text",
	"depth",
	"mode",
	"udefT",
	"udefID");
  for(int i = 0; i < length; i++)
    {
      DB_P( "%8c%8d%8d%8d%8d\n", 
	    midGridText[i],
	    midGridDepth[i],
	    midGridMode[i],
	    midGridType[i],
	    midGridUDefID[i]);
    }
#endif

  if(!midGridExists)
    return;

  bool can_continue = false;
  bool is_valid = true;
  //  int last_group = midGridUDefID[0];
  for(int i = 0; i < midGridTextLength; i++)
    {
      if(midGridMode[i] == MID_MODE_NONE)
	{
	  wxMessageBox( _T("No elements may have mode \"None\" for fragmentation to continue."), 
			_T("\"None\" mode elements found!"),
			wxICON_EXCLAMATION | wxOK);	  
	  return;
	}
      if(midGridMode[i] == MID_MODE_UDEF)
	{
	  can_continue = true;
	}
    }
  if(!can_continue)
    {
      wxMessageBox( _T("All elements have been set to terminal or delimiter mode.\nFragmentation is complete!\n\
Press \"Build Tree\" to create code tree from grid structure."), 
		    _T("No Undefined Elements Found!"),
		    wxICON_EXCLAMATION | wxOK);	
      return;
    }

  // new routine to change labels of already typed elements
  if(midGridWriteTypeOnNext)
    {
      for(int i = 0; i < midGridTextLength; i++)
	{
	  sendstr = PADS_short_labels[midGridType[i]];
	  midGrid->SetCellValue(midCurrentRow, i, sendstr);
	}
    }

  midCurrentRow++;
  midGrid->AppendRows();

  midCurrentID = 0;

  midGridType = new int[midGridTextLength];
  midGridUDefID = new int[midGridTextLength];
  
  midGridTypeLevels[midCurrentRow] = midGridType;
  midGridUDefIDLevels[midCurrentRow]   = midGridUDefID;

  for(int i = 0; i < midGridTextLength; i++)
    {
      if(midGridMode[i] == MID_MODE_UDEF)
	{
	  sendstr = midGridText[i];
	  midGridMode[i] = MID_MODE_NONE;
	  midGridType[i] = pUndefined; // this works now since UDefType is new for each level
	  midGridUDefID[i]   = MID_NO_ID;
	  midGridDepth[i] = midCurrentRow;
	}
      else
	{
	  sendstr = "";
	  // mode, termtype, and depth all stay the same for non-udef elements
	  midGridType[i] = pUndefined;
	  midGridUDefID[i]   = MID_NO_ID;
	}
      midGrid->SetCellValue(midCurrentRow, i, sendstr);
    }

  midGrid->Refresh();
  midGrid->AutoSize();
  midWinSizer->Layout();
  return;
}

// back up one level
void LaunchPADS::MidGridLastLevel(void)
{
  if(midCurrentRow == 0)
    return;

  int midLastRow = midCurrentRow - 1;
  wxString sendstr;

  delete midGridTypeLevels[midCurrentRow];
  delete midGridUDefIDLevels[midCurrentRow];
  midGridTypeLevels[midCurrentRow]   = NULL;
  midGridUDefIDLevels[midCurrentRow] = NULL;
  midGridType   = midGridTypeLevels[midCurrentRow - 1];
  midGridUDefID = midGridUDefIDLevels[midCurrentRow - 1];

#ifdef DB_ON
  for(int i = 0; i < midGridMaxLevels; i++)
    {
      if(midGridTypeLevels[i] != NULL)
	{
	  DB_P("last level: type levels at i %d == %d\n", 
	       i, midGridTypeLevels[i]);
	}

      if(midGridUDefIDLevels[i] != NULL)
	{
	  DB_P("last level: type levels at i %d == %d\n", 
	       i, midGridUDefIDLevels[i]);
	}


    }
#endif

  for(int i = 0; i < midGridTextLength; i++)
    {
      DB_P("backing up: i %d depth %d type %d %s mode %d id %d\n",
	   i, midGridDepth[i], midGridType[i], 
	   PADS_labels[midGridType[i]], midGridMode[i], 
	   midGridUDefID[i]);

      DB_P("is terminal: %s; is nonterminal: %s\n", 
	   PElement::IsPTerminal(midGridType[i]) ? "true" : "false", 
	   PElement::IsPNonTerminal(midGridType[i]) ? "true" : "false");

      // resets types to values from last level
      if(midGridDepth[i] == midCurrentRow)
	{
	  midGridDepth[i]--;
	  
	  if(midGridType[i] == pLit)
	    midGridMode[i] = MID_MODE_DELIM;
	  else if(PElement::IsPTerminal(midGridType[i]))
	    midGridMode[i] = MID_MODE_TERM;
	  else if(PElement::IsPNonTerminal(midGridType[i]))
	    midGridMode[i] = MID_MODE_UDEF;
	  else
	    midGridMode[i] = MID_MODE_NONE;
	}

      if(midGridUDefID[i] > midCurrentID)
	midCurrentID = midGridUDefID[i]+1;

      DB_P("after fix: i %d depth %d type %d %s mode %d id %d\n\n",
	   i, midGridDepth[i], midGridType[i], 
	   PADS_labels[midGridType[i]], midGridMode[i], 
	   midGridUDefID[i]);

      // replaces original characters at last level of fragmentation
      if(midGridDepth[i] == midLastRow)
	{
	  sendstr = midGridText[i];
	  midGrid->SetCellValue(midLastRow, i, sendstr);
	}

    }
  midGrid->DeleteRows(midCurrentRow);
  midGrid->Refresh();

  midCurrentRow--;
}

void LaunchPADS::MidGridStartOver(void)
{
  wxString resetstr(midGridText);
  MidGridUseAsInput(resetstr);
}

// expand the levels arrays in case we fragment too many times 
void LaunchPADS::MidGridLevelsResize(void)
{

  DB_P("-->got a call to resize??\n");
  if(midCurrentRow >= midGridMaxLevels-1)
    {
      int new_max_levels = midGridMaxLevels * MID_GRID_STEPS_MULTIPLIER;
      int **new_array1 = new int*[new_max_levels];  // fixes compilation problem under Carbon
      int **new_array2 = new int*[new_max_levels];

      for(int i = 0; i < new_max_levels; i++)
	{
	  new_array1[i] = NULL;
	  new_array2[i] = NULL;
	}

      for(int i = 0; i < midGridMaxLevels; i++)
	{
	  new_array1[i] = midGridTypeLevels[i];
	  new_array2[i] = midGridUDefIDLevels[i];
	}
      
      delete midGridTypeLevels;
      delete midGridUDefIDLevels;

      midGridMaxLevels = new_max_levels;
      midGridTypeLevels = new_array1;
      midGridUDefIDLevels   = new_array2;
    }
}

bool LaunchPADS::MidGridReadyForTree(void)
{
  int length = midGridText.Length();
  for(int i = 0; i < length; i++)
    {
      if(midGridDepth[i] == midCurrentRow)
	{
	  if(midGridMode[i] != MID_MODE_TERM &&
	     midGridMode[i] != MID_MODE_DELIM)
	    {
	      return false;
	    }
	}
    }
  return true;
}

// *** code gen routines ***

void LaunchPADS::MidGridMakeTreeFromParse(wxTreeCtrl* tree)
{
  int depth = 0;
  int left = 0;
  int right = midGridText.Length();
  
  int i = 0;
  int current_state = midGridTypeLevels[depth][left];
  
  PElement* elem;
  wxString str;
  wxTreeItemId root;
  // wxTreeItemId new_id;

  str.Printf("%s_%d_%d_%d_%d",
	     PADS_labels[current_state],
	     depth, left, i, current_state);
  elem = new PElement((int)pStruct, str, 0);

  str.Printf("%s %s_%d_%d_%d_%d[%s]", 
	     PADS_generalized_names[current_state],
	     "Root",
	     depth, left, i, pRoot,
	     midGridText.Mid(left, right-left).c_str());
  root = tree->AddRoot(str, -1, -1,elem);

#ifdef DB_ON
  DB_P("Starting recursion on %s\n", midGridText.c_str());
  DB_P("Adding root element %s\n", str.c_str());
#endif
    
  MidGridRecur(root, tree, pStruct, 0, left, right);

}

// recursive traversal function for grid to make code tree
// it's a bit of a mess, but it seems to work
void LaunchPADS::MidGridRecur(wxTreeItemId parent, wxTreeCtrl* tree,
			     int my_type,
			     int depth, int left, int right)
{
  int i = 0;
  int current_state = midGridTypeLevels[depth][left];
  int my_udef_id = midGridUDefIDLevels[depth][left];
  PElement* elem;
  wxString str;
  wxTreeItemId this_id;
  wxTreeItemId new_id;

  // make a new node for this UDef type
  str.Printf("%s_%d_%d_%d_%d", 
	     PADS_labels[my_type],
	     depth, left, right, my_type);
  elem = new PElement(my_type, str, (long)(depth == 0 ? pRecordFlag : 0));

  str.Printf("%s %s_%d_%d_%d_%d[%s]",
	     PADS_generalized_names[my_type],
	     PADS_labels[my_type],
	     depth, left, right, current_state,
	     midGridText.Mid(left, right-left).c_str());
  this_id = tree->AppendItem(parent, str, -1, -1, elem);
  elem->SetId(this_id);
  if(leftTreeBackgroundColourOn)
    {
      MidGridNewUDefColour((my_type - pStartNonTerminals)*2, MID_MODE_UDEF_COL_STEPS);
      tree->SetItemBackgroundColour(this_id, midUDefColour);
      tree->SetItemTextColour(this_id, DefaultTextColour);
    }
  else if(leftTreeTextColourOn)
    {
      MidGridNewUDefColour((my_type - pStartNonTerminals)*2, MID_MODE_UDEF_COL_STEPS);
      tree->SetItemBackgroundColour(this_id, DefaultBGColour);
      tree->SetItemTextColour(this_id, midUDefColour);
    }

  DB_P("recurrance type: %d\n", (my_type - pStartNonTerminals - 1)*2);
#ifdef DB_ON
  for(int j = 0; j < depth; j++)
    DB_P(" ");
#endif  
  DB_P("Recurring on  %s\n", str.c_str());

  elem = NULL;
  // then parse through sub-structures/terminals
  for(i = left; i < right; i++)
    {
#ifdef DB_ON
      for(int j = 0; j < depth*2; j++)
	DB_P(" ");
#endif  
      DB_P("l %d  r %d  i %d\n", left, right, i);

      if(midGridTypeLevels[depth][i] != current_state ||
	 midGridUDefIDLevels[depth][i] != my_udef_id)
	{
#ifdef DB_ON
	  for(int j = 0; j < depth*2; j++)
	    DB_P(" ");
#endif  
	  DB_P("state change: %d to %d\n", current_state, 
	       midGridTypeLevels[depth][i]);

	  if(my_type == pEnum && current_state != pEnumField)
	    {
	      DB_P("Enum-Enumfield error\n");
	      str.Printf("%s_%d_%d_%d_%d", 
			 "Error: pEnum may contain only pEnum_Field elements",
			 depth, left, i, pIllegalOp);
	      elem = new PElement(pIllegalOp, str, (long)0);
	      str.Printf("%s %d_%d_%d_%d[%s],", 
			 PADS_generalized_names[current_state],
			 "Error: pEnum may contain only pEnum_Field elements",
			 depth, left, i, pIllegalOp,
			 midGridText.Mid(left, i-left).c_str());
	      new_id = tree->AppendItem(this_id, str, -1, -1, elem);
	      elem->SetId(new_id);
	      if(leftTreeBackgroundColourOn)
		{
		  tree->SetItemBackgroundColour(new_id, midDelimColour);
		  tree->SetItemTextColour(new_id, DefaultTextColour);
		}
	      else if(leftTreeTextColourOn)
		{
		  tree->SetItemBackgroundColour(new_id, DefaultBGColour);
		  tree->SetItemTextColour(new_id, midDelimColour);
		}
	    }
	  else if(current_state == pLit)
	    {
	      str = midGridText.Mid(left, i-left);
	      str.Replace("\"", "\\\"");    
	      elem = new PElement((int)pLit, str, (long)0);
	      new_id = tree->AppendItem(this_id, str, -1, -1, elem);
	      if(leftTreeBackgroundColourOn)
		{
		  tree->SetItemBackgroundColour(new_id, midDelimColour);
		  tree->SetItemTextColour(new_id, DefaultTextColour);
		}
	      else if(leftTreeTextColourOn)
		{
		  tree->SetItemBackgroundColour(new_id, DefaultBGColour);
		  tree->SetItemTextColour(new_id, midDelimColour);
		}
	      DB_P("adding literal %s\n", str.c_str());
	    }
	  else if(PElement::IsPTerminal(current_state))
	    {
	      DB_P("adding terminal of type %s\n", PADS_labels[current_state]);

	      if(current_state == pGuess)
		{ current_state = MidGridTermGuessMode(left, i); }

	      if(current_state == pString)
		{
		  str.Printf("%s_%d_%d_%d_%d",
			     PADS_labels[current_state],
			     depth, left, i, current_state);
		  elem = new PElement((int)current_state, str, (long)0);

		  bool delim_in_text = false;
		  if(i < right)
		    {
		      char c = midGridText[i];
		      for(int j = left; j < i; j++)
			{
			  if(midGridText[j] == c)
			    delim_in_text = true;
			}
		    }
		  else
		    {
		      delim_in_text = true;
		    }

		  // don't need to check for i < right any more
		  if(midGridTypeLevels[depth][i] == pLit &&
		     delim_in_text == false)
		    {
		      elem->ChangeParams(0, -1, false, pendDefault);
		      str.Printf("\'%c\'", midGridText[i]);
		      elem->SetStopExpr(str);
		      
		      str.Printf("%s %s_%d_%d_%d_%d[%s](:\'%c\':)", 
				 PADS_generalized_names[current_state],
				 PADS_labels[current_state],
				 depth, left, i, current_state,
				 midGridText.Mid(left, i-left).c_str(), 
				 midGridText[i]);
		      
		    }
		  else
		    {
		      elem->ChangeParams(0, -1, false, pendFW);
		      str.Printf("%d", i-left);
		      elem->SetStopExpr(str);

		      str.Printf("%s %s_%d_%d_%d_%d[%s]_FW(:%d:)", 
				 PADS_generalized_names[current_state],
				 PADS_labels[current_state],
				 depth, left, i, current_state,
				 midGridText.Mid(left, i-left).c_str(), 
				 i-left);
		    }
		}
	      else if(current_state == pEnumField)
		{
		  if(my_type == pEnum)
		    {
		      /*
		      str.Printf("%s_%d_%d_%d_%d", 
				 PADS_labels[current_state],
				 depth, left, i, current_state);
		      */
		      str.Printf("%s", midGridText.Mid(left, i-left).c_str());
		      DB_P("making new pEnum Element with elem label %s\n", str.c_str());
		      elem = new PElement((int)current_state, str, (long)0);
		      str.Printf("%s %s,", 
				 PADS_generalized_names[current_state],
				 midGridText.Mid(left, i-left).c_str());
		      DB_P("making new pEnum Element with name %s\n", str.c_str());
		    }
		  else
		    {
		      DB_P("Critical post-terminal enum error\n");
		      str.Printf("%s_%d_%d_%d_%d", 
				 "Error: elements of type pEnumField may only be contained in pEnums",
				 depth, left, i, pIllegalOp);
		      elem = new PElement(pIllegalOp, str, (long)0);
		      str.Printf("%s %s_%d_%d_%d_%d[%s],", 
				 PADS_generalized_names[current_state],
				 "Error: pEnum may contain only pEnum_Field elements",
				 depth, left, i, pIllegalOp,
				 midGridText.Mid(left, i-left).c_str());
		      
		    }
		}
	      else
		{
		  DB_P("adding term elem of typd %d\n", current_state);
		  str.Printf("%s_%d_%d_%d_%d", 
			     PADS_labels[current_state],
			     depth, left, i, current_state);
		  elem = new PElement((int)current_state, str, (long)0);
		  str.Printf("%s %s_%d_%d_%d_%d[%s]", 
			     PADS_generalized_names[current_state],
			     PADS_labels[current_state],
			     depth, left, i, current_state,
			     midGridText.Mid(left, i-left).c_str());
		}
#ifdef DB_ON
	      for(int j = 0; j < depth*2; j++)
	        DB_P(" ");
#endif  
	      DB_P("adding terminal %s\n", str.c_str());
	      DB_P("adding item data %d\n", elem);
	      new_id = tree->AppendItem(this_id, str, -1, -1, elem);
	      elem->SetId(new_id);
	      //wxString testStr;
	      //elem->GetName(testStr);
	      //DB_P("orig item string = %s\n", testStr.c_str());
	      //PElement* testElem = (PElement*)tree->GetItemData(new_id);
	      //testElem->GetName(testStr);
	      //DB_P("new item string = %s\n", testStr.c_str());
	      //DB_P("new_id data = %d\n", tree->GetItemData(new_id));
	      if(leftTreeBackgroundColourOn)
		{
		  MidGridNewTermColour(current_state, MID_MODE_TERM_COL_STEPS);
		  tree->SetItemBackgroundColour(new_id, midTermColour);
		  tree->SetItemTextColour(new_id, DefaultTextColour);
		}
	      else if(leftTreeTextColourOn)
		{
		  MidGridNewTermColour(current_state, MID_MODE_TERM_COL_STEPS);
		  tree->SetItemBackgroundColour(new_id, DefaultBGColour);
		  tree->SetItemTextColour(new_id, midTermColour);
		}

	    }
	  else // current state is a composite type
	    { MidGridRecur(this_id, tree, current_state, depth+1, left, i); }

	  left = i;
	  current_state = midGridTypeLevels[depth][left];	  
	  my_udef_id = midGridUDefIDLevels[depth][left];

	  elem = NULL;
	  DB_P("this id = %d\n", this_id.m_pItem);
	  DB_P("new id = %d\n", new_id.m_pItem);
	}
    }

  // one last time without a right bound - goes to end of section
  if(current_state == pLit)
    {
      str = midGridText.Mid(left, i-left);
      elem = new PElement((int)pLit, str, (long)0);
      new_id = tree->AppendItem(this_id, str, -1, -1, elem);
      elem->SetId(new_id);
      if(leftTreeBackgroundColourOn)
	{
	  tree->SetItemBackgroundColour(new_id, midDelimColour);
	  tree->SetItemTextColour(new_id, DefaultTextColour);
	}
      else if(leftTreeTextColourOn)
	{
	  tree->SetItemBackgroundColour(new_id, DefaultBGColour);
	  tree->SetItemTextColour(new_id, midDelimColour);
	}
      DB_P("adding final literal %s\n", str.c_str());
    }
  else if(PElement::IsPTerminal(current_state))
    {
      if(current_state == pEnumField)
	{
	  if(my_type == pEnum)
	    {
	      DB_P("successful final enum field addition\n");
	      /*
		str.Printf("%s_%d_%d_%d_%d", 
		PADS_labels[current_state],
		depth, left, i, current_state);
	      */
	      str.Printf("%s", midGridText.Mid(left, i-left).c_str());
	      elem = new PElement((int)current_state, str, (long)0);
	      str.Printf("%s %s,",
			 PADS_generalized_names[current_state],
			 midGridText.Mid(left, i-left).c_str());
	      DB_P("final enum name: %s\n", str.c_str());
	    }
	  else
	    {
	      DB_P("Critical post-terminal enum error\n");
	      str.Printf("%s_%d_%d_%d_%d", 
			 "Error: elements of type pEnumField may only be contained in pEnums",
			 depth, left, i, pIllegalOp);
	      elem = new PElement(pIllegalOp, str, (long)0);
	      str.Printf("%s %d_%d_%d_%d[%s],", 
			 PADS_generalized_names[current_state],
			 "Error: pEnum may contain only pEnum_Field elements",
			 depth, left, i, pIllegalOp,
			 midGridText.Mid(left, i-left).c_str());
	    }
	}
      else
	{
	  if(current_state == pGuess)
	    { current_state = MidGridTermGuessMode(left, i); }
      
	  str.Printf("%s_%d_%d_%d_%d", 
		     PADS_labels[current_state],
		     depth, left, i, current_state);
	  elem = new PElement((int)current_state, str, (long)0);

	  if(current_state == pString)
	    {		 
	      DB_P("current_State = pstring!\n");
	      
	      elem->ChangeParams(0, -1, false, pendDefault);
	      if(right == midGridText.Length())
		{
		  str.Printf(_T(PADS_qualifiers[pEorQ]));		
		  elem->SetStopExpr(str);
		  str.Printf("%s %s_%d_%d_%d_%d[%s](:%s:)", 
			     PADS_generalized_names[current_state],
			     PADS_labels[current_state],
			     depth, left, i, current_state,
			     midGridText.Mid(left, i-left).c_str(), 
			     _T(PADS_qualifiers[pEorQ]));
		}
	      else
		{
		  str.Printf("\'%c\'", midGridText[i]);		
		  elem->SetStopExpr(str);
		  str.Printf("%s %s_%d_%d_%d_%d[%s](:\'%c\':)", 
			     PADS_generalized_names[current_state],
			     PADS_labels[current_state],
			     depth, left, i, current_state,
			     midGridText.Mid(left, i-left).c_str(), 
			     midGridText[i]);
		}
	    }
	  else
	    {
	      str.Printf("%s %s_%d_%d_%d_%d[%s]", 
			 PADS_generalized_names[current_state],
			 PADS_labels[current_state],
			 depth, left, i, current_state,
			 midGridText.Mid(left, i-left).c_str());
	    }
	}

      DB_P("adding final terminal %s\n", str.c_str());
      new_id = tree->AppendItem(this_id, str, -1, -1, elem);
      elem->SetId(new_id);
      if(leftTreeBackgroundColourOn)
	{
	  MidGridNewTermColour(current_state, MID_MODE_TERM_COL_STEPS);
	  tree->SetItemBackgroundColour(new_id, midTermColour);
	  tree->SetItemTextColour(new_id, DefaultTextColour);
	}
      else if(leftTreeTextColourOn)
	{
	  MidGridNewTermColour(current_state, MID_MODE_TERM_COL_STEPS);
	  tree->SetItemBackgroundColour(new_id, DefaultBGColour);
	  tree->SetItemTextColour(new_id, midTermColour);
	}
    }
  else if(PElement::IsPNonTerminal(current_state))
    { MidGridRecur(this_id, tree, current_state, depth+1, left, i); }  
  else
    {
      wxLogError(_T("Critical error in tree build at depth %d, state %d, left bound %d, right bound %d."),
		    depth, current_state, left, i);
      return;
    }
  DB_P("this id = %d\n", this_id.m_pItem);
  DB_P("new id = %d\n", new_id.m_pItem);
}

// simple guessing routine to determine types of grid regions
// ** need to be connected to PADS base type parsing functions **
int LaunchPADS::MidGridTermGuessMode(int left, int right)
{
  int i;
  int length;

  bool has_alpha, has_num, has_space, has_symbol, has_float, has_hex, has_sign;
  has_alpha = has_num = has_space = has_symbol = has_float = has_hex = has_sign = false;

  int num_float_point = 0;
  int num_float_exponent = 0;
  int num_sign = 0;
  int num_hex = 0;

  if(left > right)
    { // this should never happen
      int temp = right;
      right = left;
      left = temp;
    }

  wxString str = (wxChar*)midGridText.Mid(left, right-left).c_str();
  length = midGridText.Mid(left, right-left).Length();

  DB_P("guess! str: %s  length: %d\n", str.c_str(), length);

  if(length == 1)
    {
      if(!isdigit(str.GetChar(0)))
	return pChar;
      else
	return pInt;
    }
  DB_P("guess: length > 1\n");

  DB_P("str pre compile: \"%s\"\n", str.c_str());

#ifdef wxHAS_REGEX_ADVANCED
  DB_P("has regex advanced!\n");
#endif

  wxRegEx regex;
  // this looks ridiculous, but advanced regular expressions aren't available on this RH9 setup, so I'll have to specify quantities this way
  if(regex.Compile(
_T("[[:digit:]]?[[:digit:]]?[[:digit:]]\\.[[:digit:]][[:digit:]]?[[:digit:]]?\\.[[:digit:]][[:digit:]]?[[:digit:]]?\\.[[:digit:]][[:digit:]]?[[:digit:]]?"), 
wxRE_EXTENDED))
    {
      if(regex.Matches(str.c_str()))
	return pIp;
      DB_P("IP compiled but failed!\n");
    }

  for(i = 0; i < length; i++)
    {
      // str[i] = toascii((int)str[i]);

      DB_P("checking char %c at %d\n", str.GetChar(i), i);
      if(LaunchPADS::IsSign(str.GetChar(i)))
	{
	  has_sign   = true;
	  num_sign++;
	}
      else if(LaunchPADS::IsFloat(str.GetChar(i)))
	{
	  has_float  = true;
	  if(str.GetChar(i) == '.')
	    num_float_point++;
	  else if(str.GetChar(i) == 'e')
	    num_float_exponent++;
	}
      else if(LaunchPADS::IsHex(str.GetChar(i)))
	{
	  has_hex    = true;
	  if(str.GetChar(i) == 'x')
	    num_hex++;
	} 
      else if(isdigit(str.GetChar(i)))
	has_num    = true;
      else if(isspace(str.GetChar(i)))
	has_space  = true;
      else if(LaunchPADS::IsSymbol(str.GetChar(i)))
	has_symbol = true;
      else if(isalpha(str.GetChar(i)))
	has_alpha = true;
    }  

  if(has_num)
    DB_P("guess: has digits\n");
  if(has_space)
    DB_P("guess: has spaces\n");
  if(has_symbol)
    DB_P("guess: has sybols\n");
  if(has_float)
    DB_P("guess: has fp chars\n");

  DB_P("num fp: %d, num exp: %d, num sign: %d, num hex: %d\n", 
       num_float_point, num_float_exponent, num_sign, num_hex);


  if(num_float_point > 1 ||
     num_float_exponent > 1 ||
     num_sign > 1 ||
     num_hex > 1)
    {
      return pString;
    }

  if(has_alpha || has_symbol || has_space)
    return pString;
  else if(has_float && (num_float_point == 1))
    return pFloat;
  else if(has_hex)
    return pInt;
  else
    return pInt;  
  
}


// simple functions to help us identify type-specific characteristics
bool LaunchPADS::IsSymbol(char c)
{
  // encoding in ascii - I don't think we're interested in much else at 
  // this point...
  if(((c >= '!' && c <= '/')  ||
      (c >= ':' && c <= '@')  ||
      (c >= '[' && c <= '`')  ||
      (c >= '{' && c <= '~')) &&
     (c != '+' && c != '-' && c != '.'))
  {
    return true;
  }
  return false;
}

bool LaunchPADS::IsFloat(char c)
{
  if((c >= '0' && c <= '9') ||
     (c == '.' || c == 'e'))
    {
      return true;
    }
  return false;
}

bool LaunchPADS::IsHex(char c)
{
  if((c >= '0' && c <= '9') ||
     (c >= 'A' && c <= 'F') ||
     (c >= 'a' && c <= 'f') ||
     (c == 'x'))
    {
      return true;
    }
  return false;
}

bool LaunchPADS::IsSign(char c)
{
  if(c == '+' || c == '-')
    return true;

  return false;
}


int LaunchPADS::MidGridGetMode(void)
{
  DB_P("mid edit mode on get mode: %d\n", midEditMode);
  return midEditMode;
}

// cycle grid highlighting colors
void LaunchPADS::MidGridNewUDefColour(int a, int b)
{
  if(colourModeOn)
    {  
      midUDefColour.Set(255, 
			((192+ ((a % 32) * b)) %224), 
			((192+ ((a % 8)  * b)) %224));
    }
  else
    {
      midUDefColour.Set(152+ (((a % 8) * b) %63), 
			152+ (((a % 8) * b) %63), 
			152+ (((a % 8) * b) %63));
    }
  return;
}

void LaunchPADS::MidGridNewTermColour(int a, int b)
{
  if(colourModeOn)
    {  
      midTermColour.Set((64 + ((96   * a) / b)      % 64), 
			(96 + (((255 * a  * 2) / b) % 159)), 
			255);
    }
  else
    {
      midTermColour.Set(96 + (((255 * a * 2) / b) % 63), 
			96 + (((255 * a * 2) / b) % 63), 
			96 + (((255 * a * 2) / b) % 63));
    }
  return;
}

PADSGridNode* LaunchPADS::MidMakeGridNodeTreeFromGridData()
{
  int depth = 0;
  int left = 0;
  int right = midGridText.Length();
  PADSGridNode* treeRoot = new PADSGridNode(-1, left, right);
  gridTreeRoot = treeRoot;
  MidMakeGridNodeTreeRecur(treeRoot, left, right, 0);
  gridTreeRoot->setDepth(0);

  PSTRING testStr;
  gridTreeRoot->getTextRepresentation(testStr);
  DB_P("***text representation:***\n%s\n***", testStr.c_str());
  return treeRoot;
}

PADSGridNode* LaunchPADS::MidMakeGridNodeTreeRecur(PADSGridNode* parent, int left, int right, int depth)
{
  int i;
  int thisType = midGridTypeLevels[depth][left];
  int thisElementId = midGridUDefIDLevels[depth][left];
  PADSGridNode* newNode;
  for(i = left; i < right; i++)
    {
      if(midGridTypeLevels[depth][i] != thisType ||
	 midGridUDefIDLevels[depth][i] != thisElementId)
	{

	  if(PElement::IsPTerminal(thisType))
	    {
	      if(thisType == pGuess)
		{ thisType = MidGridTermGuessMode(left, i); }
	      newNode = parent->makeAndAddNewChildNode(thisType, left, i);
	      newNode->setDepth(depth);
	    }
	  else
	    {
	      newNode = parent->makeAndAddNewChildNode(thisType, left, i);
	      newNode->setDepth(depth);
	      MidMakeGridNodeTreeRecur(newNode, left, i, depth+1);
	    }
	  left = i;
	  thisType = midGridTypeLevels[depth][left];
	  thisElementId = midGridUDefIDLevels[depth][left];
	}      
    }
  // one more once i reaches the end of the grid
  if(PElement::IsPTerminal(thisType))
    {
      if(thisType == pGuess)
	{ thisType = MidGridTermGuessMode(left, i); }
      newNode = parent->makeAndAddNewChildNode(thisType, left, i);
      newNode->setDepth(depth);
    }
  else
    {
      newNode = parent->makeAndAddNewChildNode(thisType, left, i);
      newNode->setDepth(depth);
      MidMakeGridNodeTreeRecur(newNode, left, i, depth+1);
    }
  left = i;
  thisType = midGridTypeLevels[depth][left];
  thisElementId = midGridUDefIDLevels[depth][left];      
  
  return parent;
}

/* ************************************ */
