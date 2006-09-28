
26 July 2006

LaunchPADS code review
Mark, Kathleen, Mary

Commentary by Mark
==================

o Currently, tools constructs type model of data top-down (horizontal decomposition)
  Also useful to construct type model bottom-up (vertical decomposition)

o Iterative process: Specify type, sample data, 
  find good records/find bad records, report result, repeat.

  Kathleen's idea: Do sampling in background

o No interactive way to define dependencies between types

External Libraries
==================

WX Widget library
-----------------
 - Download any version >= 2.6.2 from www.wxwidgets.org

 - Configuration/installation requires a "build" sub-directory for
   library.

   o Uncompressed source ends up in top-level WX-widget: 
      <path>/wx-GTK-<version> - Linux
      <path>/wxMac-<version>  - MacOS

 - In LaunchPADS/Makefile, 

   Set WX_HOME to top-level WX-widget directory. 
   Set WX_BUILD to build sub-directory.

Xerces 
------
 - Download from http://xml.apache.org 
 - Mac build for Xerces installs library as a Framework
 - Linux build, *** not sure ***
 - Used for parsing all XML data other than the playback log
 
TinyXML
-------
 - In LaunchPADS/src/tinyxml
 - Used for parsing playback log

LaunchPADS/
===========

 src/
 - Object files & executables get generated in this directory/

 - Makefiles/
   
 - GUI interface source (.h, .cpp)

    o Graphical components named according to location on screen
      {left,right,top,bottom,mid}

    o Event handling routines 
      main_event.cpp
      
    o Dialog boxes 
      padx_d*, shell_d*, prefs*, 
      
    o Demo play-back
      state_seq 
              
    o Main class 
      LaunchPADs.cpp

    o lp_* = LaunchPADS utility class

      lp_CodeGen : For each tool wizard, user is presented with
      appropriate parameters.  lp_CodeGen generates appropriate header
      files for target tool. 

      lp_PADS : Old PADS AST (this one should be obsolete)

      lp_PADS_constants : Enumerations for PADS base types (NOT obsolete)
 
      lp_PNode_Container : conduit object between internal AST nodes
        and graphical representation in the tree view (Isomorphism
        between AST and graphical rep)

      lp_PValueNode_Container : conduit object between
        PADS_lang_unified tree (which contains summary of accumulator
        output) and its graphical representation in the tree view

        (Many-to-one mapping from Accumulator output in unified tree
         and AST)

        Accumulator_output.xml 
          =>_parse DOM-Tree
            =>_map PADS_lang_unified_tree

       lp_XML : XML-state reading functions, works with TinyXML

       lp_constants : Associations between buttons/menus/events
       lp_event_ids : Designations for every control in interface 
 
       lp_includes : Uber include file

     o Configuration of tool process 
       
 bitmaps/
 - Artwork for buttons
 
 demos/
 - One playback sequence in *.xml
 - To load, [Tools]/[Playback] - then load .xml from demos/

 inspector/
 - PADS AST editor window
 
 padslang/
 - PADS AST classes & utilities, mappings from/to AST and other reps,
   like the grid view

   o PADS_lang.cpp : PADS AST

   o PADS_lang*     : helper class
     _factory       : mapping from XML element name to PADS type

     _helper_classes: 
       mapping from Grid rep to AST rep
       hash table for type lookup
   
     _pnode_tree    : mirror of PNode_Container that is independent
                      WX-widget, so that LaunchPADs is (kind of)
                      independent of WX-widget toolkit

     _unified_tree  : contains data from output of some PADS tool 

     _xerces_parser : DOM => AST 

  padslang/padslang_parser : regression test for parser
  - target is in padslang/Makefile

Design idea:

   Unified tree's structure is isomorphic to tool output. 

   Each path through unified tree corresponds to one path through AST 

   At each node in unified tree, tool-specific data annotates the node

   