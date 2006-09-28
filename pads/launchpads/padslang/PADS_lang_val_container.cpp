/* *****************************************************************************
 * Mark Daly
 * June 2006
 * PADS_lang_val_container - Container class for values from parsed data,
 *  intended to facilitate display of parsed data via GUI
 * ************************************************************************** */

#include "PADS_lang_val_container.h"

using namespace std;

void PADSValContainer::setLinkNode(PNodeP* newLink)
{
  linkNode = newLink;
}

PNodeP* PADSValContainer::getLinkNode()
{
  return linkNode;
}

void PADSValContainer::setHasError(bool newError)
{
  hasError = newError;
}

bool PADSValContainer::getHasError()
{
  return hasError;
}

int PADSValContainer::insertValue(char* strData)
{
  values.push_back(strData);
}

int PADSValContainer::insertValue(PSTRING& strData)
{
  values.push_back(strData);
}

int PADSValContainer::getVectorCount()
{
  return values.size();
}

int PADSValContainer::getVectorAt(int index, PSTRING& strData)
{
  if(index < values.size())
    {
      strData = values[index];
      return 0;
    }
}

// *********************************************
