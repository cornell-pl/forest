(* 
   Module: Pads_dm
   Description:
     This module implements the PADS datamodel. 
*)

open Dm
open Pads_c


(* PADS Items *)

class virtual padsItem :
  object
    inherit item
    method implementation : unit -> string

    method get_implid : Nodeid.implemid
    (*method get_docid : Nodeid.docid*)
    method get_nodeid_context: Nodeid_context.nodeid_context
  end


(* PADS Nodes *)

class virtual padsNode :
  Nodeid.docid ->  
  nodeRep -> 
  nodeRep option -> 
  object
    inherit node
    inherit padsItem 

    method get_nodeRep   : nodeRep
    method parent        : unit -> Dm.node option
    method update_parent : Dm.node -> unit

    method nodeid        : unit  -> Nodeid.nodeid
    method docorder      : unit  -> Nodeid.docorder

    method insert     	 : Dm.node Cursor.cursor -> Dm.node option -> unit
    method delete     	 : Dm.node -> unit
    method replace    	 : Dm.node Cursor.cursor -> Dm.node -> unit
    method replace_value : atomicValue Cursor.cursor -> unit

  end


(* PADS Document Nodes *)

class padsDocumentNode :
    Dm.atomicString option -> 
    Nodeid.docid ->  
    nodeRep -> 
  object
    inherit document
    inherit padsNode

    method children 	: unit -> Dm.node Cursor.cursor
    method document_uri : unit -> Dm.atomicString option

(* Added by Yitzhak *)
    method descendant_or_self   : unit -> Dm.node Cursor.cursor
    method descendant           : unit -> Dm.node Cursor.cursor
    method ancestor_or_self     : unit -> Dm.node Cursor.cursor
    method ancestor             : unit -> Dm.node Cursor.cursor
(* End *)

  end


(* PADS Element Nodes *)

class padsElementNode :
    Nodeid.docid ->  
    nodeRep ->
    nodeRep option ->
  object
    inherit element
    inherit padsNode

    method children    : unit -> Dm.node Cursor.cursor
    method node_name   : unit -> atomicQName option

    method node_type   : unit -> Dm.type_annotation option

    method attributes  : unit -> Dm.attribute Cursor.cursor
    method namespace_environment : unit -> Namespace_context.nsenv
    method elemName    : unit -> Dm._ElemName
    method typed_value : unit -> Dm.atomicValue Cursor.cursor
(* Added by Yitzhak *)
    method export_typed_value : 
	unit -> Dm.atomicValue Cursor.cursor

    method descendant_or_self   : unit -> Dm.node Cursor.cursor
    method descendant           : unit -> Dm.node Cursor.cursor
    method ancestor_or_self     : unit -> Dm.node Cursor.cursor
    method ancestor             : unit -> Dm.node Cursor.cursor
(* End *)

    method has_element_content : unit -> bool

    method nilled      : unit -> Dm.nilled_flag
  end


(* PADS Text Nodes *)
class padsTextNode :
    Nodeid.docid ->  
    nodeRep ->
    nodeRep option ->
  object
    inherit text
    inherit padsNode

    method string_value : unit -> Datatypes.xs_string
    method typed_value : unit -> Dm.atomicValue Cursor.cursor
(* Added by Yitzhak *)
    method export_typed_value : 
	unit -> Dm.atomicValue Cursor.cursor

    method descendant_or_self   : unit -> Dm.node Cursor.cursor
    method descendant           : unit -> Dm.node Cursor.cursor
    method ancestor_or_self     : unit -> Dm.node Cursor.cursor
    method ancestor             : unit -> Dm.node Cursor.cursor
(* End *)
  end

(* This is the "callback" function that is called from the PADS mainline *)
val pads_document : Processing_context.processing_context -> Dm.atomicString option -> nodeRep -> padsDocumentNode
