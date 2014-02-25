/* *****************************************************************************
 * Mark Daly
 * June 2006
 * PADS_lang_val_container - Container class for values from parsed data,
 *  intended to facilitate display of parsed data via GUI
 * ************************************************************************** */

#ifndef PADS_LANG_VAL_CONTAINER_H_INCLUDED__
#define PADS_LANG_VAL_CONTAINER_H_INCLUDED__

#include <vector>
#include <string>

using namespace std;

class PNodeP;

class PADSValContainer
{
 public:
  PNodeP* linkNode;
  vector<string> values;

  bool hasError;

  void setLinkNode(PNodeP* newLink);
  PNodeP* getLinkNode();

  void setHasError(bool newError);
  bool getHasError();

  int insertValue(char* strData);
  int insertValue(PSTRING& strData);

  int getVectorCount();
  int getVectorAt(int index, PSTRING& strData);
};

#endif
