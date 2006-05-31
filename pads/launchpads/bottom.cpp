/* **************************************
 * PADS GUI project (preliminary tests)
 * Bottom frame related code
 * *********************************** */
#include "lp_includes.h"
#include "pads_gui.h"


/* **************************************
 * Functions:
 *  --init
 *  BottomInitFrame
 *
 *  --interface 
 *  BottomGetTextEnd      --get last line of text
 *  BottomSetTextVisible  --make selected region visible
 *  BottomAppendText      --add text at end of code
 *  BottomPrependText     --add text at begining of code
 *  BottomClearText       --clear code view
 *  BottomGetText         --get contents of code view
 *
 *  --customization
 *  BottomSetTermColour
 *  BottomSetNontermColour
 *  BottomSetLitColour
 *  BottomSetErrorColour
 *  BottomSetOtherColour
 *
 *  BottomGetTermColour
 *  BottomGetNontermColour
 *  BottomGetLitColour
 *  BottomGetErrorColour
 *  BottomGetOtherColour
 *
 *  BottomGetFont
 *  BottomSetFont
 * *********************************** */

void LaunchPADS::BottomInitFrame(long panelflags, 
				long textflags)
{
  
  m_bottomWinPanel = new wxPanel(m_bottomWin, 
				 wxID_ANY, 
				 wxDefaultPosition,
				 m_bottomWin->GetSize(),
				 panelflags);
  assert(m_bottomWinPanel != NULL);
  
  bottomCodeDisplay = new wxTextCtrl(m_bottomWinPanel, 
				     wxID_ANY,
				     _T(""),
				     wxDefaultPosition,
				     m_bottomWinPanel->GetSize(),
				     textflags);
  assert(bottomCodeDisplay != NULL);

  
#ifdef DB_ON
  bottomCodeDisplay->SetValue(_T("Fill me with code!\n"));

  wxString str;
  str = "term code\n";
  BottomAppendText(str, (int)pInt);
  str = "nonterm code\n";
  BottomAppendText(str, (int)pStruct);
  str = "normal code\n";
  BottomAppendText(str, (int)pUndefined);
  str = "error code\n";
  BottomAppendText(str, (int)pIllegalOp);
  str = "literal code\n";
  BottomAppendText(str, (int)pLit);
#endif
  

  bottomWinSizer = new wxBoxSizer(wxHORIZONTAL);
  assert(bottomWinSizer != NULL);
  bottomWinSizer->Add(bottomCodeDisplay, 1,  wxGROW | wxALL | wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT, 5);

  //BottomSetFont(bottomFont);

  m_bottomWinPanel->SetSizer(bottomWinSizer);
  m_bottomWinPanel->SetAutoLayout(true);
  bottomWinSizer->SetSizeHints(m_bottomWinPanel);

}

/* ************************************ */

long LaunchPADS::BottomGetTextEnd(void)
{
  return (long)bottomCodeDisplay->GetLastPosition();
}

// find element in code at position pos
void LaunchPADS::BottomSetTextVisible(long pos)
{
  long x, line, line_length;
  wxString str;
  int i;
  bottomCodeDisplay->ShowPosition(pos);
  bottomCodeDisplay->PositionToXY(pos, &x, &line);
  line_length = bottomCodeDisplay->GetLineLength(line);
  str = bottomCodeDisplay->GetLineText(line);
  for(i = x; i < line_length; i++)
    {
      if(str[i] == ';' || str[i] == '\n')
	break;
    }
  bottomCodeDisplay->SetSelection(pos, pos+(long)i);
  //  bottomCodeDisplay->SetSelection(pos, pos);
  DB_P("Making visible pos %d, pos+i %d, x %d, line %d, line_length %d\n",
       pos, pos+1, x, line, line_length);
  

}


void LaunchPADS::BottomAppendText(wxString &text, int mode)
{

  if(mode == pLit)
    bottomCodeDisplay->SetDefaultStyle(bottomLitColour);
  else if(PElement::IsPNonTerminal(mode))
    bottomCodeDisplay->SetDefaultStyle(bottomNontermColour);
  else if(mode == pIllegalOp)
    bottomCodeDisplay->SetDefaultStyle(bottomErrorColour);
  else if(PElement::IsPTerminal(mode))
    bottomCodeDisplay->SetDefaultStyle(bottomTermColour);
  else
    bottomCodeDisplay->SetDefaultStyle(bottomOtherColour);

  bottomCodeDisplay->AppendText(text);

  return;
}

void LaunchPADS::BottomPrependText(wxString &text, int mode)
{

  long position = bottomCodeDisplay->GetInsertionPoint();
  bottomCodeDisplay->SetInsertionPoint(0);


  if(mode == pLit)
    bottomCodeDisplay->SetDefaultStyle(bottomLitColour);
  else if(PElement::IsPNonTerminal(mode))
    bottomCodeDisplay->SetDefaultStyle(bottomNontermColour);
  else if(mode == pIllegalOp)
    bottomCodeDisplay->SetDefaultStyle(bottomErrorColour);
  else if(PElement::IsPTerminal(mode))
    bottomCodeDisplay->SetDefaultStyle(bottomTermColour);
  else
    bottomCodeDisplay->SetDefaultStyle(bottomOtherColour);

  bottomCodeDisplay->WriteText(text);
  bottomCodeDisplay->SetInsertionPoint(position);
  return;
}


void LaunchPADS::BottomClearText(void)
{
  bottomCodeDisplay->Clear();
}

wxString LaunchPADS::BottomGetText(void)
{
  return bottomCodeDisplay->GetValue();
}


void LaunchPADS::BottomSetTermColour(wxColour &TermColour_new)
{  bottomTermColour = TermColour_new; }

void LaunchPADS::BottomSetNontermColour(wxColour &NontermColour_new)
{  bottomNontermColour = NontermColour_new; }

void LaunchPADS::BottomSetLitColour(wxColour &LitColour_new)
{  bottomLitColour = LitColour_new; }

void LaunchPADS::BottomSetErrorColour(wxColour &ErrorColour_new)
{  bottomErrorColour = ErrorColour_new; }

void LaunchPADS::BottomSetOtherColour(wxColour &OtherColour_new)
{  bottomOtherColour = OtherColour_new; }


wxColour LaunchPADS::BottomGetTermColour()
{ return bottomTermColour; }

wxColour LaunchPADS::BottomGetNontermColour()
{ return bottomNontermColour; }

wxColour LaunchPADS::BottomGetLitColour()
{ return bottomLitColour; }

wxColour LaunchPADS::BottomGetErrorColour()
{ return bottomErrorColour; }

wxColour LaunchPADS::BottomGetOtherColour()
{ return bottomOtherColour; }


void LaunchPADS::BottomGetFont(wxFont &font)
{
  font = bottomCodeDisplay->GetFont();
}

void LaunchPADS::BottomSetFont(wxFont &font)
{
  DB_P("setting bottom checklist font\n");

  fConfig->Write(_T("/bottom/font_name"), font.GetFaceName());
  fConfig->Write(_T("/bottom/font_size"), font.GetPointSize());
      
  bottomCodeDisplay->SetFont(font);
  bottomCodeDisplay->Refresh();
}

/* ************************************ */
