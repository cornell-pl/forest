/* **************************************
 * PADS GUI project (preliminary tests)
 * Internal representation of PADS language (implementation)
 * *********************************** */

#include "lp_includes.h"

/* **************************************
 * Functions:
 *  --class constructor/destructor
 *  PElement
 *  ~PElement
 *
 *  --interface
 *  PCheckName        --make sure name is legal (doesn't do anything at the moment - should be left this way)
 *  ChangeName        --change element name
 *  ChangeParams      --change element attributes (overloaded: w/ name change)
 *  ChangeParams      --change element attributes (overloaded: w/o name change)
 *  ChangeType        --change element type (checked)
 *  SetStopExpr       --set element stop expression
 *  GetStopExpr       --get   |
 *  HasStopExpr       --has   |
 *  ClearStopExpr     --clear |
 *  AddArg            --add predicate (line at a time)
 *  RemoveArg         --remove predicate |
 *  GetNumArgs        --get number of predicates (lines)
 *  HasArgs           --has predicate
 *  GetArgAtIndex     --get predicate line at index #
 *  ClearArgs         --clear all predicates
 *  AddClause         --add custom text
 *  RemoveClause      --clear |
 *  HasClause         --has   |
 *  GetClause         --get   |
 *  ReplaceClause     --replace custom text with new
 *  ClearClause       --clear custom text
 *  GetType
 *  GetName
 *  GetEnctype
 *  GetTermtype
 *  GetSign
 *  GetByteSize
 *  GetIsTypedef      --is typedef flag set
 *  GetFlags          --get attribute flags
 *  SetFlags          --set |
 *  GetLineNumber     --get location in code
 *  SetLineNumber     --set |
 *  IsPTerminal       --type check functions (overloaded: element->type)
 *  IsPPrintableTerminal
 *  IsPNonTerminal
 *  IsPNumType
 *  GetPTypeAttributes--get legal attributes for current type
 *  IsPTerminal       --type check functions (overloaded: int type)
 *  IsPPrintableTerminal
 *  IsPNonTerminal
 *  IsPNumType
 *  GetPTypeAttributes
 * *********************************** */

bool PElement::PCheckName(wxString& name)
{

  return true;
  //  wxRegEx name_check(wxString("\s*P[(a_)|(e_)|(b_)|(sbl_)|(sbh_)|(ebc_)|(bcd_)]?u?[(float)|(int)|()]?[(8)|(16)|(32)|(64)]"));
  wxRegEx name_check(wxString("[:space:]*P[[:alnum:]_]+"));
  if(!name_check.IsValid())
    {
      return true; // if the expression is broken, don't risk false negatives
    }
  if(name_check.Matches(name.c_str()))
    {
      return false;
    }
  return true;
}


//#ifdef __NOT_DEFINED__

PElement::PElement(int type,
		   wxString &name_in, 
		   long flags,
		   int enct, 
		   bool sign, 
		   int termt, 
		   int bytesize, 
		   bool istypedef,
		   bool hasstop,
		   const wxString &stopexpr,
		   bool args,
		   const wxString &arg_list,
		   bool clause,
		   int clauset,
		   const wxString &clause_name)  
  : wxTreeItemData()
{

  PType = (type > pStartTerminals    && type < pEndTerminals ||
	   type > pStartNonTerminals && type < pEndNonTerminals )
    ? type : pUndefined;

  PTerminal =   (PType > pStartTerminals && PType < pEndTerminals)
		 ? true : false;

  PEnctype  = (enct >= pchrDefault && enct <= pnumBCD) ? enct : pUndefined;
  PSigned = sign;

  PTermType = (termt >= pendDefault && termt <= pendSE) ? termt : pUndefined;

  PBytesize = (bytesize == p8  ||
	       bytesize == p16 ||
	       bytesize == p32 ||
	       bytesize == p64) 
    ? bytesize : pUndefined;

  
  PIsTypedef = istypedef;
  if(istypedef == true)
    {
      flags |= pTypedefFlag;
    }
  else if((flags & pTypedefFlag) != 0)
    PIsTypedef = true;

  if(PType != pEnumField)
    {
      if(PCheckName(name_in))
	{
	  PName = name_in;
	  PNameLen = PName.Length();
	}
      else
	{
	  PName = "";
	  PNameLen = PName.Length();
	  PType = pIllegalOp;
	}
    }
  else // if the element is an EnumField, allow any name (since ENUM names are literal)
    {
      PName = name_in;
      PNameLen = PName.Length();
    }

  if(hasstop)
    {
      PHasStopExpr = hasstop;
      PStopExpr = stopexpr;
    }
  else
    {
      PHasStopExpr = false;
      PStopExpr = "";
    }


  PArgList = new wxArrayString();

  PHasArgs = args;
  if(PHasArgs)
    {
      PArgList->Add(arg_list);
      PNumArgs = 1;
    }
  else
    {
      PArgList->Empty();
      PNumArgs = 0;
    }

  PHasClause = clause;
  if(PHasClause)
      PClauseName = clause_name;
  else
      PClauseName = "";

  PFlags = flags;

  PLineNumber = 0;
}

PElement::~PElement()
{
  PArgList->~wxArrayString();
}


bool PElement::ChangeName(wxString &name_in)
{
  if(name_in.Cmp("") != 0)
    {
      if(PCheckName(name_in))
	{
	  PName = name_in;
	  PNameLen = PName.Length();
	  return true;
	}
      else
	{
	  return false;
	}
    }
  return false;
}

bool PElement::ChangeParams(wxString& name_in, 
			    int flags, 
			    int enct, bool sign, int termt, 
			    int bytesize, bool istypedef)
{

  if(name_in.Cmp("") != 0)
    {
      if(PCheckName(name_in))
	{
	  PName = name_in;
	  PNameLen = PName.Length();
	}
      else
	{
	  return false;
	}
    }

  PEnctype  = (enct >= pchrDefault && enct <= pnumBCD) ? enct : PEnctype;

  PSigned = sign;

  PTermType = (termt >= pendDefault && termt <= pendSE) ? termt : PTermType;

  PBytesize = (bytesize == p8  ||
	       bytesize == p16 ||
	       bytesize == p32 ||
	       bytesize == p64) 
    ? bytesize : PBytesize;

  PIsTypedef = istypedef;
  if(istypedef == true)
    {
      flags |= pTypedefFlag;
    }
  else if((flags & pTypedefFlag) != 0)
    PIsTypedef = true;
    
  
  PFlags = flags;
  return true;
}

bool PElement::ChangeParams(int flags,
			    int enct, bool sign, int termt, 
			    int bytesize, bool istypedef)
{

  PEnctype  = (enct >= pchrDefault && enct <= pnumBCD) ? enct : PEnctype;

  PSigned = sign;

  PTermType = (termt >= pendDefault && termt <= pendSE) ? termt : PTermType;

  PBytesize = (bytesize == p8  ||
	       bytesize == p16 ||
	       bytesize == p32 ||
	       bytesize == p64) 
    ? bytesize : PBytesize;

  PIsTypedef = istypedef;
  if(istypedef == true)
    {
      flags |= pTypedefFlag;
    }
  else if((flags & pTypedefFlag) != 0)
    PIsTypedef = true;

  PFlags = flags;
  return true;
}
/* ********************** */

bool PElement::ChangeType(int new_type)
{
  if(IsPTerminal() && !PElement::IsPTerminal(new_type))
    {
      return false;
    }
  if(IsPNonTerminal() && !PElement::IsPNonTerminal(new_type))
    {
      return false;
    }

  if(PElement::IsPTerminal(new_type) ||
     PElement::IsPNonTerminal(new_type))
    {
      if(new_type == pChar)
	{
	  PBytesize = p8;
	  PHasStopExpr = false;
	  PStopExpr = "";
	  PTermType = pchrDefault;
	  PSigned = false;
	  PTerminal = true;
	}
      else if(new_type == pString)
	{
	  PBytesize = p8;
	  PSigned = false;
	  PTerminal = true;
	}

      PType = new_type;
    }

  return true;
}

/* ********************** */

bool PElement::SetStopExpr(wxString& stop_e)
{
  if(stop_e.Cmp("") != 0)
    {
      PHasStopExpr = true;
      PStopExpr = stop_e;
      return true;
    }
  return false;
  
}

bool PElement::GetStopExpr(wxString& stop_e)
{
  if(PHasStopExpr)
    {
      stop_e = PStopExpr;
      return true;
    }
  return false;
}

bool PElement::HasStopExpr(void)
{
  return PHasStopExpr;
}

void PElement::ClearStopExpr(void)
{
  PHasStopExpr = false;
  PStopExpr.Clear();
}
/* ********************** */

bool PElement::AddArg(wxString& arg_in)
{
  if(arg_in.Cmp("") != 0)
    {
      PHasArgs = true;
      PNumArgs++;
      PArgList->Add(arg_in);
      return true;
    }
  return false;
}

bool PElement::RemoveArg(int index, wxString& arg_out)
{
  if(index < PArgList->GetCount() && PHasArgs == true)
    {
      arg_out = PArgList->Item(index);
      PArgList->RemoveAt(index);
      PNumArgs--;
      if(PNumArgs <= 0)
	{
	  PHasArgs = false;
	}
      return true;
    } 
  return false;
}

int PElement::GetNumArgs(void)
{
  return PNumArgs;
}

bool PElement::HasArgs(void)
{
  return PHasArgs;
}

bool PElement::GetArgAtIndex(int i, wxString& string)
{
  if(PHasArgs && i < PArgList->GetCount())
    {
      string = (*PArgList)[i];
      return true;
    }
  return false;
}

void PElement::ClearArgs(void)
{
  PNumArgs = 0;
  PHasArgs = false;
  PArgList->clear();
}



/* ********************** */

bool PElement::AddClause(wxString& clause_in)
{
  if(clause_in.Cmp("") != 0)
    {
      PHasClause = true;    
      PClauseName = clause_in;
      return true;
    }
  return false;
}

bool PElement::RemoveClause(wxString& clause_out)
{
  if(PHasClause)
    {
      clause_out = PClauseName;
      PClauseName = "";
      PHasClause = false;
      return true;
    } 
  return false;
}

bool PElement::HasClause(void)
{
  return PHasClause;
}


bool PElement::GetClause(wxString& string)
{
  if(PHasClause)
    {
      string = PClauseName;
      return true;
    }
  return false;
}

bool PElement::ReplaceClause(wxString& new_clause)
{
  PClauseName = new_clause;
  PHasClause = true;
  return true;
}

void PElement::ClearClause(void)
{
  PHasClause = false;
  PClauseName.Clear();
}


/* *********************** */


int PElement::GetType(void)
{
  return PType;
}

void PElement::GetName(wxString& name)
{
  name = PName; 
}

int PElement::GetEnctype(void)
{
  return PEnctype;
}

int PElement::GetTermtype(void)
{
  return PTermType;
}

bool PElement::GetSign(void)
{
  return PSigned;
}

int PElement::GetByteSize(void)
{
  return PBytesize;
}

bool PElement::GetIsTypedef(void)
{
  return PIsTypedef;
}

long PElement::GetFlags(void)
{
  return PFlags;
}

void PElement::SetFlags(long flags)
{
  
  if((flags & pTypedefFlag) != 0)
    {
      PIsTypedef = true;
    }
  else
    PIsTypedef = false;

  PFlags = flags;
}

long PElement::GetLineNumber(void)
{
  return PLineNumber;
}

void PElement::SetLineNumber(long num)
{
  PLineNumber = num;
}


bool PElement::IsPTerminal(int i)
{
  if(i > pStartTerminals &&
     i < pEndTerminals)
    return true;
  return false;
}

bool PElement::IsPPrintableTerminal(int i)
{
 if((i >  pStartTerminals &&
     i < pEnumField) ||
     i == pUserDef)
   return true;
 return false;

}

bool PElement::IsPNonTerminal(int i)
{
  if(i > pStartNonTerminals &&
     i < pEndNonTerminals)
    return true;
  return false;
}

bool PElement::IsPNumType(int i)
{
  if(i >= pInt &&
     i <= pFpoint)
      return true;
  return false;
}


void PElement::GetPTypeAttributes(int type, 
				  bool &sign,
				  bool &enctype,
				  bool &bytes, 
				  bool &termtype)
{


  if(type < pChar || type >= pEndTerminals)
    {
      sign = false;
      enctype = false;
      bytes = false;
      termtype = false;
      return;
    }

  switch(type)
    {
    case pChar:
      sign = false;
      enctype = true;
      bytes = false;
      termtype = false;
      break;
    case pInt:
      sign = true;
      enctype = true;
      bytes = true;
      termtype = true;
      break;
    case pFloat:
      sign = false;
      enctype = true;
      bytes = true;
      termtype = false;
      break;
    case pFpoint:
      sign = true;
      enctype = true;
      bytes = true;
      termtype = false;
      break;
    case pB:
      sign = true;
      enctype = false;
      bytes = true;
      termtype = false;
      break;
    case pString:
      sign = false;
      enctype = true;
      bytes = false;
      termtype = true;
      break;
    case pTimestamp_explicit:
    case pTimestamp:
    case pDate_explicit:
    case pDate:
    case pTime_explicit:
    case pTime:
      sign = false;
      enctype = true;
      bytes = false;
      termtype = true;
      break;
    case pCountX:
    case pCountXtoY:
      sign = false;
      enctype = true;
      bytes = false;
      termtype = false;
      break;
    case pIp:
      sign = false;
      enctype = true;
      bytes = false;
      termtype = false;
      break;
    case pEnumField:
    case pCase:
    case pFunction:
    case pLit:
    case pUserDef: 
    case pTypeDefault: 
      sign = false;
      enctype = false;
      bytes = false;
      termtype = false;
      break;
    }

  return;
}


bool PElement::IsPTerminal()
{
 if(PType > pStartTerminals &&
    PType < pEndTerminals)
    return true;
  return false;

}

bool PElement::IsPPrintableTerminal()
{
 if((PType >  pStartTerminals &&
     PType <  pEnumField) ||
     PType == pUserDef)
   return true;
  return false;

}

bool PElement::IsPNonTerminal()
{
  if(PType > pStartNonTerminals &&
     PType < pEndNonTerminals)
    return true;
  return false;
}

bool PElement::IsPNumType()
{
  if(PType >= pInt &&
     PType <= pFpoint)
      return true;
  return false;
}


void PElement::GetPTypeAttributes(bool &sign,
				  bool &enctype,
				  bool &bytes, 
				  bool &termtype)
{
  int type = PType;

  if(type < pChar || type >= pEndTerminals)
    {
      sign = false;
      enctype = false;
      bytes = false;
      termtype = false;
      return;
    }

  switch(type)
    {
    case pChar:
      sign = false;
      enctype = true;
      bytes = false;
      termtype = false;
      break;
    case pInt:
      sign = true;
      enctype = true;
      bytes = true;
      termtype = true;
      break;
    case pFloat:
      sign = false;
      enctype = true;
      bytes = true;
      termtype = false;
      break;
    case pFpoint:
      sign = true;
      enctype = true;
      bytes = true;
      termtype = false;
      break;
    case pB:
      sign = true;
      enctype = false;
      bytes = true;
      termtype = false;
      break;
    case pString:
      sign = false;
      enctype = true;
      bytes = false;
      termtype = true;
      break;
    case pTimestamp_explicit:
    case pTimestamp:
    case pDate_explicit:
    case pDate:
    case pTime_explicit:
    case pTime:
      sign = false;
      enctype = true;
      bytes = false;
      termtype = true;
      break;
    case pCountX:
    case pCountXtoY:
      sign = false;
      enctype = true;
      bytes = false;
      termtype = false;
      break;
    case pIp:
      sign = false;
      enctype = true;
      bytes = false;
      termtype = false;
      break;
    case pEnumField:
    case pCase:
    case pFunction:
    case pLit:
    case pUserDef: 
    case pTypeDefault: 
      sign = false;
      enctype = false;
      bytes = false;
      termtype = false;
      break;
    }

  return;
}

/* ************************************ */
