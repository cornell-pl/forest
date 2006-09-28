/* *****************************************************************************
 * Mark Daly
 * June 2006
 * PADS_lang_factory - (str -> PNode) allocation table for use in PXML
 *  interpretation
 * ************************************************************************** */

#ifndef PADS_FACTORY_CPP_INCLUDED__
#define PADS_FACTORY_CPP_INCLUDED__

class PNodeFactory
{
 public:
  PNodeFactory();

  PNodeP* makeEmptyPNodeFromType(int which);
  int     getTypeFromString(char* str);

};

#endif
