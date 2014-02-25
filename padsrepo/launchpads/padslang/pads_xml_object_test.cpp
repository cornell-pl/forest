/* *****************************************************************************
 * Mark Daly
 * June 2006
 * PADS_lang xml object test
 *  short regression test of PADS_lang xml parsing/serialization facilities
 *  and various helper classes in the PADS_lang hierarchy
 * ************************************************************************** */

#include "PADS_lang.h"
#include "PADS_lang_factory.h"
#include "PADS_lang_helper_classes.h"
#include "PADS_lang_simple_tree.h"
#include "PADS_lang_pnode_tree.h"
#include "PADS_lang_unified_tree.h"
#include "PADS_lang_xerces_parser.h"

int main(int argc, char** argv)
{
  PADS_lang_def_parser* p = new PADS_lang_def_parser();
  PNodeP *rootPNode1, *rootPNode2;

  printf("init complete = %d\n", PADS_lang_def_parser::initComplete);

  PADS_lang_def_parser* p2 = new PADS_lang_def_parser();
  
  PSTRING fileName = "./sirius.p.xml";
  PSTRING fileName2 = "./ai.p.xml";//"./ai.p.xml";
  PSTRING xmlStr;
  PSTRING codeStr;
  PSTRING simpleTreeStr;
  PADSPNodeTreeNode* simpleTree;

  printf("parsing %s...\n", fileName.c_str());
  try
    {
      p->parseURI(fileName);
    }
  catch(...)
    {
      printf("p threw exception\n");
    }
  p->serializeXMLRepresentation(xmlStr);
  printf("p1 XML:\n%s\n**********\n", xmlStr.c_str());
  p->codifyRepresentation(codeStr);
  printf("p1 code:\n%s\n**********\n", codeStr.c_str());
  simpleTree = p->getSimpleTreeRepresentation();
  simpleTree->printTextualRepresentation(0, simpleTreeStr);
  rootPNode1 = p->getParsedAST();
  delete simpleTree;
  printf("p1 simpleTree:\n%s\n**********\n", simpleTreeStr.c_str());
  printf("\n##############################\n");
  printf("parsing %s...\n", fileName2.c_str());

  xmlStr.clear();
  codeStr.clear();
  simpleTreeStr.clear();

  try
    {
      p2->parseURI(fileName2);
    }
  catch(...)
    {
      printf("p2 threw exception\n");
    }
  p2->serializeXMLRepresentation(xmlStr);
  printf("p2 XML:\n%s\n**********\n", xmlStr.c_str());
  p2->codifyRepresentation(codeStr);
  printf("p2 code:\n%s\n**********\n", codeStr.c_str());
  simpleTree = p2->getSimpleTreeRepresentation();
  simpleTree->printTextualRepresentation(0, simpleTreeStr);
  rootPNode2 = p2->getParsedAST();
  delete simpleTree;
  printf("p2 simpleTree:\n%s\n**********\n", simpleTreeStr.c_str());

  delete p;
  delete p2;

  printf("init complete = %d\n", PADS_lang_def_parser::initComplete);

  PNodeP *testNode1;
  testNode1 = new PstructS();
  testNode1->makeCompleteNode();
  PNodeP *testNode2, *testNode3;
  testNode2 = new fieldS();
  testNode3 = new fieldS();
  PSTRING nameStr;
  nameStr = "field decl 1";
  testNode2->makeCompleteNode();
  ((fieldS*)testNode2)->name->addValue(nameStr);
  nameStr = "field decl 2";
  testNode3->makeCompleteNode();
  ((fieldS*)testNode3)->name->addValue(nameStr);


  testNode1->addChild(testNode2);
  testNode1->addChild(testNode3);
  int removeRetCode;
  simpleTree = testNode1->makeBasicTree(NULL);
  simpleTreeStr.clear();
  simpleTree->printTextualRepresentation(0, simpleTreeStr);
  printf("**********\ntestNode1 simple tree 1:\n%s", simpleTreeStr.c_str());
  removeRetCode = testNode1->removeChildNode(testNode3);
  simpleTree = testNode1->makeBasicTree(NULL);
  simpleTreeStr.clear();
  simpleTree->printTextualRepresentation(0, simpleTreeStr);
  printf("**********\ntestNode1 simple tree 2:\n%s", simpleTreeStr.c_str());  

  printf("**********\n");
  PADSTypeMap nameMap1, nameMap2;
  PSTRING mapData;
  rootPNode1->insertIntoNameMap(&nameMap1);
  nameMap1.getTextualRepresentation(mapData);
  printf("###mapData1:\n%s\n**********\n", mapData.c_str());
  mapData.clear();
  rootPNode2->insertIntoNameMap(&nameMap2);
  nameMap2.getTextualRepresentation(mapData);
  printf("###mapData2:\n%s\n**********\n", mapData.c_str());
  
  printf("**********\n");
  PADSTypeMap pathMap1, pathMap2;
  PSTRING pathData;
  PSTRING topPath = "";
  rootPNode1->insertIntoPathMap(&pathMap1, topPath);
  pathMap1.getTextualRepresentation(pathData);
  printf("###pathData1:\n%s\n**********\n", pathData.c_str());
  mapData.clear();
  topPath.clear();
  rootPNode2->insertIntoPathMap(&pathMap2, topPath);
  pathMap2.getTextualRepresentation(pathData);
  printf("###pathData2:\n%s\n**********\n", pathData.c_str());

  PADSTypeMap typeMap1, typeMap2;
  PADSUniTreeNode *uTree1, *uTree2;
  PSTRING typeData = "";
  PSTRING varStr = "";

  rootPNode1->insertIntoTypeMap(&typeMap1);
  typeMap1.getTextualRepresentation(typeData);
  printf("###typeData1:\n%s\n**********\n", typeData.c_str());
  printf("###making uniTree1\n");
  try
    {
      uTree1 = rootPNode1->makeUnifiedTree(NULL, varStr, &typeMap1);
    }
  catch (PTException e)
    {
      char sbuf[1024];
      // printf("*@%d* %s\n", __LINE__, e.printReport(sbuf, 1024));
    }
  DB_P("done make unified tree1 : %d\n", uTree1);
  typeData.clear();
  uTree1->printTextualRepresentation(0, typeData);
  printf("###uniTree1:\n%s\n**********\n", typeData.c_str());
  typeData.clear();
  varStr.clear();

  rootPNode2->insertIntoTypeMap(&typeMap2);
  typeMap2.getTextualRepresentation(typeData);
  printf("###typeData2:\n%s\n**********\n", typeData.c_str());
  printf("###making uniTree2\n");
  try
    {
      uTree2 = rootPNode2->makeUnifiedTree(NULL, varStr, &typeMap2);
    }
  catch (PTException e)
    {
      char sbuf[1024];
      printf("*@%d* %s\n", __LINE__, e.printReport(sbuf, 1024));
    }

  typeData.clear();
  uTree2->printTextualRepresentation(0, typeData);
  printf("###uniTree2:\n%s\n**********\n", typeData.c_str());

  
  PADSTypeMap typePathMap1, typePathMap2;
  PSTRING pathStr = "Psource:";
  PSTRING typePathStr = "";

  uTree1->insertIntoTypePathMap(&typePathMap1, pathStr, 0);
  typePathMap1.getTextualRepresentation(typePathStr);
  printf("***uTree1 path map:\n%s\n**********\n", typePathStr.c_str());
  typePathStr.clear();
  uTree2->insertIntoTypePathMap(&typePathMap2, pathStr, 0);
  typePathMap2.getTextualRepresentation(typePathStr);
  printf("***uTree2 path map:\n%s\n**********\n", typePathStr.c_str());

  PADSTypeMap namePathMap1, namePathMap2;
  //pathStr = "PadsAccum:Psource:";
  pathStr = "";
  PSTRING namePathStr = "";

  uTree1->insertIntoNamePathMap(&namePathMap1, pathStr, 0);
  namePathMap1.getTextualRepresentation(namePathStr);
  printf("***uTree1 path map:\n%s\n**********\n", namePathStr.c_str());
  namePathStr.clear();
  uTree2->insertIntoNamePathMap(&namePathMap2, pathStr, 0);
  namePathMap2.getTextualRepresentation(namePathStr);
  printf("***uTree2 path map:\n%s\n**********\n", namePathStr.c_str());

  return 0;
}
