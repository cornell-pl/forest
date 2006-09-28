/* *****************************************************************************
 * Mark Daly
 * June 2006
 * LaunchPADS pxml parser - parser for pxml files using the Apache Xerces XML
 *  parser; parts of this have been borrowed from the Xerces example apps 
 *  (DOMCount.cpp in particular) in my attempt to understand the Xerces API
 * ************************************************************************** */

#ifndef PADS_PARSER_H_INCLUDED__
#define PADS_PARSER_H_INCLUDED__

#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/parsers/AbstractDOMParser.hpp>
#include <xercesc/dom/DOMImplementation.hpp>
#include <xercesc/dom/DOMImplementationLS.hpp>
#include <xercesc/dom/DOMImplementationRegistry.hpp>
#include <xercesc/dom/DOMBuilder.hpp>
#include <xercesc/dom/DOMException.hpp>
#include <xercesc/dom/DOMDocument.hpp>
#include <xercesc/dom/DOMNodeList.hpp>
#include <xercesc/dom/DOMError.hpp>
#include <xercesc/dom/DOMLocator.hpp>
#include <xercesc/dom/DOMNamedNodeMap.hpp>
#include <xercesc/dom/DOMAttr.hpp>

#include "PADS_lang.h"
#include "PADS_lang_factory.h"
#include "PADS_lang_helper_classes.h"
#include "PADS_lang_simple_tree.h"
#include "PADS_lang_pnode_tree.h"

XERCES_CPP_NAMESPACE_USE

// specify encoding options?
// apparently Xerces wants the following three (from DOM examples)
static XMLCh EncOptsDefault[] = { chLatin_L, chLatin_S, chNull };

class PADS_lang_def_parser
{
 public:
  PNodeFactory nodeFactory;

  PADS_lang_def_parser();
  PADS_lang_def_parser(XMLCh* EncOpts);
  ~PADS_lang_def_parser();

  PSTRING parseThisURI;

  PNodeP* rootPNode;
  DOMImplementation *domImplementation;

  XMLCh* encoding_options;

  DOMBuilder *xmlParser;
  XERCES_CPP_NAMESPACE_QUALIFIER DOMDocument *DOMdocument;

  static bool initComplete;
  static int instanceCount;

  PADSTypeMap treeNameMap;

  void instantiateParser();
  void setFileURI(PSTRING& URIstr);
  
  static void initParserSystem();
  static void terminateParserSystem();

  static bool stringContainsIllegalXMLEntities(const char* c);
  static bool isStringWhitespace(char* c);

  int parseURI();
  int parseURI(PSTRING& URIstr);
  int parseDataURI(PSTRING& URIstr);

  int serializeXMLRepresentation(PSTRING& xmlStr);
  int codifyRepresentation(PSTRING& codeStr);
  PNodeP* getParsedAST();
   void setASTRoot(PNodeP* newRootNode);

  PADSPNodeTreeNode* getSimpleTreeRepresentation();

 protected:
  PNodeP* walkXMLTree(DOMNode *n);
};

#endif
