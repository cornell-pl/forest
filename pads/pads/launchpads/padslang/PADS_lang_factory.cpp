/* *****************************************************************************
 * Mark Daly
 * June 2006
 * PADS_lang_factory - (str -> PNode) allocation table for use in PXML
 *  interpretation
 * ************************************************************************** */

#include <stdio.h>
#include <stdlib.h>
#include <string>

#include "PADS_lang.h"
#include "PADS_lang_factory.h"


PNodeFactory::PNodeFactory()
{
}

PNodeP* PNodeFactory::makeEmptyPNodeFromType(int which)
{
  PNodeP* retVal;
  retVal = NULL;

  switch(which)
    {
    case PT_UNIT:
      retVal = (PNodeP*) new unitS();
      return retVal;
      break;
    case PT_NAME:
      retVal = (PNodeP*) new nameS();
      return retVal;
      break;
    case PT_VAR:
      retVal = (PNodeP*) new varS();
      return retVal;
      break;
    case PT_ID:
      retVal = (PNodeP*) new idS();
      return retVal;
      break;
/*  PT_LANGUAGE:
      retVal = (PNodeP*) new languageS;
      return retVal;
      break; */
    PT_EXPR:
      retVal = (PNodeP*) new ExprS();
      return retVal;
      break;
    case PT_PRE:
      retVal = (PNodeP*) new PreS;
      retVal->nodeType = PT_PRE;
      return retVal;
      break;
    case PT_STRING:
      retVal = (PNodeP*) new stringS();
      return retVal;
      break;
    case PT_CHAR:
      retVal = (PNodeP*) new charS();
      return retVal;
      break;
    case PT_REEXPR:
      retVal = (PNodeP*) new REExprS();
      return retVal;
      break;
    case PT_EXPR2:
      retVal = (PNodeP*) new exprS();
      retVal->nodeType = PT_EXPR2;
      return retVal;
      break;
    case PT_LITERAL:
      retVal = (PNodeP*) new literalS();
      retVal->nodeType = PT_LITERAL;
      return retVal;
      break;
    case PT_PPARSECHECK:
      retVal = (PNodeP*) new PparsecheckS();
      retVal->nodeType = PT_PPARSECHECK;
      return retVal;
      break;
    case PT_TYPENAME:
      retVal = (PNodeP*) new typenameS();
      return retVal;
      break;
    case PT_VARDECL:
      retVal = (PNodeP*) new vardeclS();
      return retVal;
      break;
    case PT_CONSTRAINTS:
      retVal = (PNodeP*) new ConstraintsS();
      return retVal;
      break;
    case PT_CONSTRAINTS2:
      retVal = (PNodeP*) new constraintsS();
      retVal->nodeType = PT_CONSTRAINTS2;
      return retVal;
      break;
    case PT_FUNCTION:
      retVal = (PNodeP*) new functionS();
      return retVal;
      break;
    case PT_PARAMETER:
      retVal = (PNodeP*) new parameterS();
      return retVal;
      break;
    case PT_ARGUMENT:
      retVal = (PNodeP*) new argumentS();
      retVal->nodeType = PT_ARGUMENT;
      return retVal;
      break;
    case PT_PSOURCE:
      retVal = (PNodeP*) new PsourceS();
      retVal->nodeType = PT_PSOURCE;
      return retVal;
      break;
    case PT_PRECORD:
      retVal = (PNodeP*) new PrecordS();
      retVal->nodeType = PT_PRECORD;
      return retVal;
      break;
    case PT_PLONGEST:
      retVal = (PNodeP*) new PlongestS();
      retVal->nodeType = PT_PLONGEST;
      return retVal;
      break;
    case PT_MODIFIERS:
      retVal = (PNodeP*) new modifiersS();
      return retVal;
      break;
    case PT_DECL:
      retVal = (PNodeP*) new declS();
      return retVal;
      break;
    case PT_PTYPE:
      retVal = (PNodeP*) new ptypeS();
      return retVal;
      break;
    case PT_MIN_LOCAL:
      retVal = (PNodeP*) new ExprS();
      retVal->nodeType = PT_MIN_LOCAL;
      return retVal;
      break;
    case PT_MAX_LOCAL:
      retVal = (PNodeP*) new ExprS();
      retVal->nodeType = PT_MAX_LOCAL;
      return retVal;
      break;
    case PT_FORALL:
      retVal = (PNodeP*) new forAllS();
      return retVal;
      break;
    case PT_SIZE:
      retVal = (PNodeP*) new sizeS();
      return retVal;
      break;
    case PT_ARRAYCONSTRAINTS:
      retVal = (PNodeP*) new arrayConstraintsS();
      return retVal;
      break;
    case PT_NOSEP_LOCAL:
      retVal = (PNodeP*) new unitS();
      retVal->nodeType = PT_NOSEP_LOCAL;
      return retVal;
      break;
    case PT_TERMEXPR:
      retVal = (PNodeP*) new TermExprS();
      return retVal;
      break;
    case PT_SEP_LOCAL:
      retVal = (PNodeP*) new REExprS();
      retVal->nodeType = PT_SEP_LOCAL;
      return retVal;
      break;
    case PT_TERM_LOCAL:
      retVal = (PNodeP*) new TermExprS();
      retVal->nodeType = PT_TERM_LOCAL;
      return retVal;
      break;
    case PT_LAST_LOCAL:
      retVal = (PNodeP*) new ConstraintsS();
      retVal->nodeType = PT_LAST_LOCAL;
      return retVal;
      break;
    case PT_ENDED_LOCAL:
      retVal = (PNodeP*) new ConstraintsS();
      retVal->nodeType = PT_ENDED_LOCAL;
      return retVal;
      break;
    case PT_OMIT_LOCAL:
      retVal = (PNodeP*) new ConstraintsS();
      retVal->nodeType = PT_OMIT_LOCAL;
      return retVal;
      break;
    case PT_DELIMITERCONSTRAINTS:
      retVal = (PNodeP*) new delimiterConstraintsS();
      return retVal;
      break;
    case PT_PARRAY:
      retVal = (PNodeP*) new ParrayS();
      return retVal;
      break;
    case PT_PSOME:
      retVal = (PNodeP*) new PsomeS();
      return retVal;
      break;
    case PT_PNONE:
      retVal = (PNodeP*) new PnoneS();
      return retVal;
      break;
    case PT_POPT:
      retVal = (PNodeP*) new PoptS();
      return retVal;
      break;
    case PT_POMIT:
      retVal = (PNodeP*) new PomitS();
      retVal->nodeType = PT_POMIT;
      return retVal;
      break;
    case PT_PENDIAN:
      retVal = (PNodeP*) new PendianS();
      retVal->nodeType = PT_PENDIAN;
      return retVal;
      break;
    case PT_PCOMPUTE:
      retVal = (PNodeP*) new PcomputeS();
      retVal->nodeType = PT_PCOMPUTE;
      return retVal;
      break;
    case PT_STRUCTMODIFIERS:
      retVal = (PNodeP*) new structModifiersS();
      return retVal;
      break;
    case PT_FROM:
      retVal = (PNodeP*) new fromS();
      retVal->nodeType = PT_FROM;
      return retVal;
      break;
    case PT_FIELD:
      retVal = (PNodeP*) new fieldS();
      return retVal;
      break;
    case PT_PALTERNATES:
      retVal = (PNodeP*) new PalternatesS();
      return retVal;
      break;
    case PT_PCHARCLASS:
      retVal = (PNodeP*) new PcharclassS();
      return retVal;
      break;
    case PT_PPREFIX:
      retVal = (PNodeP*) new PprefixS();
      retVal->nodeType = PT_PPREFIX;
      return retVal;
      break;
    case PT_ENUMFIELD:
      retVal = (PNodeP*) new enumFieldS();
      return retVal;
      break;
    case PT_PENUM:
      retVal = (PNodeP*) new PenumS();
      return retVal;
      break;
    case PT_PRECURSIVE:
      retVal = (PNodeP*) new PrecursiveS();
      return retVal;
      break;
    case PT_PSELECT:
      retVal = (PNodeP*) new PselectS();
      return retVal;
      break;
    case PT_PSTRUCT:
      retVal = (PNodeP*) new PstructS();
      return retVal;
      break;
    case PT_PTYPEDEF:
      retVal = (PNodeP*) new PtypedefS();
      return retVal;
      break;
    case PT_PCASE:
      retVal = (PNodeP*) new PcaseS();
      return retVal;
      break;
    case PT_PDEFAULT:
      retVal = (PNodeP*) new PdefaultS();
      return retVal;
      break;
    case PT_PSWITCH:
      retVal = (PNodeP*) new PswitchS();
      return retVal;
      break;
    case PT_PUNION:
      retVal = (PNodeP*) new PunionS();
      return retVal;
      break;
    case PT_PADS:
      retVal = (PNodeP*) new Pads();
      return retVal;
      break;
    default:
      throw PTException(PTE_FACTORY_UNDEF_ERR, which, __LINE__);
      //throw except;
      return NULL;
      break;
    }
  return retVal;
}

#ifdef PADS_NEW_XML_STR_STD
#define PADS_XML_NAMES PT_XMLNames
#endif
#ifndef PADS_NEW_XML_STR_STD
#define PADS_XML_NAMES PT_TypeNames
#endif

int PNodeFactory::getTypeFromString(char* str)
{
  int i;
  DB_P("PADS lang factory searching for string %s\n", str);
  for(i = PT_BEGIN_VALID_TYPES + 1; i < PT_LAST_TYPE; i++)
    {
      if(strcmp(str, PADS_XML_NAMES[i]) == 0)
	return i;
    }
  return PT_ERROR_TYPE;
}
