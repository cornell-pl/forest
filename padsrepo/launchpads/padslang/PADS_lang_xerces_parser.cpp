/* *****************************************************************************
 * Mark Daly
 * June 2006
 * LaunchPADS pxml parser - parser for pxml files using the Apache Xerces XML
 *  parser; parts of this are based on the example apps included with Xerces 
 *  (DOMCount.cpp in particular) in my attempt to understand the Xerces API.
 *  (Specifically, the calls made to the Xerces API in the tree-walking function
 *  were decied upon by examining those of DOMCount.cpp.)
 * ************************************************************************** */

// necessary includes borrowed from xerces's own DOMCount.cpp
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

#include <string.h>
#include <stdlib.h>

#if defined(XERCES_NEW_IOSTREAMS)
#include <fstream>
#else
#include <fstream.h>
#endif

#include "PADS_lang.h"
#include "PADS_lang_factory.h"
#include "PADS_lang_helper_classes.h"
#include "PADS_lang_simple_tree.h"
#include "PADS_lang_pnode_tree.h"
#include "PADS_lang_xerces_parser.h"

XERCES_CPP_NAMESPACE_USE

bool PADS_lang_def_parser::initComplete = false;
int PADS_lang_def_parser::instanceCount = 0;

PADS_lang_def_parser::PADS_lang_def_parser()
{
  instanceCount++;
  if(!initComplete)
    initParserSystem();
  encoding_options = EncOptsDefault;
  domImplementation = DOMImplementationRegistry::getDOMImplementation(encoding_options);
  parseThisURI = "";
  rootPNode = NULL;
  instantiateParser();
}

PADS_lang_def_parser::PADS_lang_def_parser(XMLCh* EncOpts)
{
  instanceCount++;
  if(!initComplete)
    initParserSystem();
  if(EncOpts == NULL)
    encoding_options = EncOptsDefault;
  domImplementation = DOMImplementationRegistry::getDOMImplementation(encoding_options);
  parseThisURI = "";
  rootPNode = NULL;
  instantiateParser();
}

PADS_lang_def_parser::~PADS_lang_def_parser()
{
  xmlParser->release();
  instanceCount--;
  if(instanceCount == 0 && initComplete == true)
    terminateParserSystem();
}

void PADS_lang_def_parser::instantiateParser()
{
  xmlParser = ((DOMImplementationLS*)domImplementation)->
    createDOMBuilder(DOMImplementationLS::MODE_SYNCHRONOUS, 0);
  xmlParser->resetDocumentPool();
}

void PADS_lang_def_parser::setFileURI(PSTRING& URIstr)
{
  parseThisURI = URIstr;
}


void PADS_lang_def_parser::initParserSystem()
{
  if(!initComplete)
    {
      XMLPlatformUtils::Initialize();
      initComplete = true;
    }
}

void PADS_lang_def_parser::terminateParserSystem()
{
  if(initComplete && instanceCount == 0)
    {
      XMLPlatformUtils::Terminate();
      initComplete = false;
    }
}

int PADS_lang_def_parser::parseURI()
{
  DOMdocument = xmlParser->parseURI(parseThisURI.c_str());
  rootPNode = walkXMLTree((DOMNode*)DOMdocument->getDocumentElement());
  return 0;
}


int PADS_lang_def_parser::parseURI(PSTRING& URIstr)
{
  if(!initComplete)
    {
      DB_P("PADS_lang_def_parser: init not complete, returning with -1\n");
      return -1;
    }
  parseThisURI = URIstr;
  DB_P("parsing with init complete = %d\n", initComplete);
  DB_P("PADS_lang_def_parser: parsing %s\n", parseThisURI.c_str()); 
  DOMdocument = xmlParser->parseURI(parseThisURI.c_str());
  rootPNode = walkXMLTree((DOMNode*)DOMdocument->getDocumentElement());
  DB_P("root node acquired @ %d -> %s\n", 
       rootPNode, PT_TypeNames[rootPNode->nodeType]);
  return 0;
}

int PADS_lang_def_parser::parseDataURI(PSTRING& URIstr)
{
  // this has yet to be implemented in the interface as well...
}

int PADS_lang_def_parser::serializeXMLRepresentation(PSTRING& xmlStr)
{
  if(rootPNode == NULL)
    return -1;
  rootPNode->serializeXMLRepresentation(0, xmlStr);
  return 0;
}

int PADS_lang_def_parser::codifyRepresentation(PSTRING& codeStr)
{
  if(rootPNode == NULL)
    return -1;
  rootPNode->codifyRepresentation(0, codeStr);
  return 0;
}

PADSPNodeTreeNode* PADS_lang_def_parser::getSimpleTreeRepresentation()
{
  if(rootPNode == NULL)
    return NULL;
  return rootPNode->makeBasicTree(NULL);
}

// addapted from the Xerces DOMCount sample
PNodeP* PADS_lang_def_parser::walkXMLTree(DOMNode *thisDOMnode)
{
  DOMNode *childDOMnode;
  char *str, *str2, *elementName, *childName;
  PNodeP *thisPNode, *childPNode;
  int newType;

  if(thisDOMnode == NULL)
    return NULL;

  elementName = XMLString::transcode(thisDOMnode->getNodeName());
  if(thisDOMnode->getNodeType() == DOMNode::ELEMENT_NODE)
    {
      try
	{
	  newType = nodeFactory.getTypeFromString(elementName);
	  if(newType == PT_COMMENT)
	    {
	      thisPNode = NULL;
	    }
	  else
	    {
	      thisPNode = nodeFactory.makeEmptyPNodeFromType(newType);
	    }
	}
      catch (PTException e)
	{
	  char sbuf[1024];
	  printf("*@%d* %s\n", __LINE__, e.printReport(sbuf, 1024));
	}
      DB_P("created new node of type %s\n", PT_TypeNames[thisPNode->nodeType]);

      if(thisPNode != NULL)
	{
	  for(childDOMnode = thisDOMnode->getFirstChild();
	      childDOMnode != NULL;
	      childDOMnode = childDOMnode->getNextSibling())
	    {
	      childName = XMLString::transcode(childDOMnode->getNodeName());
	      if(childDOMnode->getNodeType() == DOMNode::TEXT_NODE ||
		 childDOMnode->getNodeType() == DOMNode::CDATA_SECTION_NODE)
		{
		  str = XMLString::transcode(childDOMnode->getTextContent());
		  if(!isStringWhitespace(str) || (strlen(str) == 1 && str[0] == ' '))
		    {
		      PSTRING pstr = str;
		      try
			{
			  DB_P("adding value to type %s: %s\n", 
			       PT_TypeNames[thisPNode->nodeType], pstr.c_str());
			  thisPNode->addValue(pstr);
			}
		      catch (PTException e)
			{
			  char sbuf[1024];
			  printf("*@%d* %s\n", __LINE__, e.printReport(sbuf, 1024));
			}
		    }
		  XMLString::release(&str);
		}
	      if(childDOMnode->getNodeType() == DOMNode::ELEMENT_NODE)
		{
		  childPNode = walkXMLTree(childDOMnode);
		  try
		    {
		      DB_P("adding child %s => %s\n", PT_TypeNames[thisPNode->nodeType], 
			   PT_TypeNames[childPNode->nodeType]);
		      thisPNode->addChild(childPNode);
		    }
		  catch (PTException e)
		    {
		      char sbuf[1024];
		      printf("*@%d* %s\n", __LINE__, e.printReport(sbuf, 1024));
		    }
		  XMLString::release(&childName);
		}
	    }
	}
    }
  XMLString::release(&elementName);
  return thisPNode;
}

bool PADS_lang_def_parser::isStringWhitespace(char* c)
{
  for(int i = 0; c[i] != '\0'; i++)
    {
      if(c[i] != ' ' &&
	 c[i] != '\n' &&
	 c[i] != '\t' &&
	 c[i] != '\r')
	return false;
    }
  return true;
}

bool PADS_lang_def_parser::stringContainsIllegalXMLEntities(const char* c)
{
  for(int i = 0; c[i] != '\0'; i++)
    {
      if(c[i] != '&'  &&
	 c[i] != '"'  &&
	 c[i] != '\'' &&
	 c[i] != '<'  &&
	 c[i] != '>'  )
	return true;
    }
  return false;
}

PNodeP* PADS_lang_def_parser::getParsedAST()
{
  return rootPNode;
}

void PADS_lang_def_parser::setASTRoot(PNodeP* newRootNode)
{
  rootPNode = newRootNode;
}

