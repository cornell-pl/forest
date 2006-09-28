/* *****************************************************************************
 * Mark Daly
 * June 2006
 * PADS_def - container class for a PADS definition AST and related type map
 * ************************************************************************** */

#ifndef PADS_DEF_H_INCLUDED__
#define PADS_DEF_H_INCLUDED__

#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <vector>
#include <exception>

#include "PADS_lang.h"
#include "PADS_lang_helper_classes.h"
#include "PADS_base_types.h"
#include "PADS_lang_factory.h"

//using std::string;
//using std::vector;

using namespace std;


// **************************
// PADS Definition Container Class
// **************************

class PadsDefinition 
{
 public:
  PadsDefinition();
  ~PadsDefinition();

  PNodeP* rootNode;
  PADSTypeMap typeMap;
};

#endif
