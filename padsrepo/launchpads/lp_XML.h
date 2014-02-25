/* **************************************
 * PADS GUI project (preliminary tests)
 * XML state definition file
 * -- a study in doing XML the hard way
 * *********************************** */

#ifndef _LP_XML_H_INCLUDED
#define _LP_XML_H_INCLUDED

/* ***********************
 * Conventions:
 * -Element names start with caps.
 * -Attribute names start with lower case
 * -Extra _*_ qualifier included for sub-elements that need one
 * -Generalization of names done whenever possible 
 *  (i.e. "type," which is used frequently, has only one definition)
 * ******************** */

// main container element
#define XML_APPSTATE "AppState"
#define XML_AS_APPNAME "app_name"
#define XML_AS_APPVERSION "app_version"
#define XML_AS_DATESTAMP "date_stamp"
#define XML_NUM_STEPS "num_steps"

// playback step
#define XML_STEP "Step"
#define XML_S_NUM "num"
#define XML_S_TIMESTAMP "time_stamp"

// checklist container element
#define XML_CHECKLIST "Checklist"
#define XML_CL_CHECKED "checked"
// checklist key
#define XML_CL_KEY "Key"

#define XML_GRIDCONTROLS "GridControls"
#define XML_GC_MODE "mode"
#define XML_GC_TERMTYPE "term_type"
#define XML_GC_NONTERMTYPE "nonterm_type"

// grid element
#define XML_GRID "Grid"
#define XML_G_ROWS "rows"
#define XML_G_COLS "cols"
#define XML_G_CURRENTID "current_id"
#define XML_G_MAXLEVELS "max_levels"
#define XML_G_CURRENTROW "current_row"
#define XML_G_CURSOR_ROW "cursor_row"
#define XML_G_CURSOR_COL "cursor_column"
// grid text
#define XML_GRIDTEXT "GridText"
#define XML_GT_LENGTH "length"
// grid row
#define XML_ROW "Row"
#define XML_R_ROW "row"
// grid row cell
#define XML_CELL "Cell"
#define XML_C_COL "col"
#define XML_C_DEPTH "depth"
#define XML_C_MODE  "mode"
// generalized "type" - only need one for all components
#define XML_TYPE  "type"
#define XML_C_CLASSID "class_id"

// tree element
#define XML_TREE "Tree"
// node in def tree
#define XML_TREEITEM "TreeItem"
#define XML_T_EXPANDED "expand"
#define XML_T_ENCTYPE "enctype"
#define XML_TERMTYPE "termtype"
#define XML_SIGN "sign"
#define XML_BYTES "bytes"
#define XML_TYPEDEF "typedef"
#define XML_T_FLAGS "flags"
#define XML_T_WHERE "where"
// tree item label
#define XML_TREELABEL "TreeLabel"
// tree item code name
#define XML_ELEMNAME "ElemName"
// tree item stop expression
#define XML_STOPEXPR "StopExpr"
// tree item predicate
#define XML_PREDICATE "Predicate"
// tree item custom text
#define XML_CUSTOMTEXT "CustomText"

// tree control panel element
#define XML_TREECTRL "TreeCtrl"
#define XML_TC_SHOWN "shown"
#define XML_TC_COMPUTE "compute"
#define XML_TC_RECORD "record"
#define XML_TC_SOURCE "source"
// tree control panel name
#define XML_NAME "Name"
// tree control panel predicates (other controls already taken care of)
#define XML_PREDICATES "Predicates"

// code view element (not much - just says whether code was generated or not)
#define XML_CODEVIEW "Code"
#define XML_CV_GENERATED "generated"
#define XML_CV_POS "pos"

#endif
