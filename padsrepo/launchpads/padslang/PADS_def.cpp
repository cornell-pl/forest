/* *****************************************************************************
 * Mark Daly
 * June 2006
 * PADS_def - container class for a PADS definition AST and related type map
 * ************************************************************************** */

#include "PADS_def.h"

PadsDefinition::PadsDefinition()
{
  rootNode = NULL;
  PSTRING str;
  for(int i = 0; i < PTYPES_num_types; i++)
    {
      str = PADS_reserved_type_names[i];
      typeMap.insert(str, NULL, PTMAP_STD);
    }

}

PadsDefinition::~PadsDefinition()
{
  delete rootNode;
}

