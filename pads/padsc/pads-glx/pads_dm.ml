(* Module: PADSDM
   Description:
     This module implements the PADS datamodel. 
*)

open Dm
open Conf
open Monitor
open Monitoring_context
open Namespace_context
open Namespace_names
(* open Pads_c *)

(* For now, we use one, global nodeid_context for the whole module.
   In the future, we should probably have PADS supply this for us. *)

let global_nodeid_ctxt = Nodeid_context.default_nodeid_context()
let global_impl = "pads"
let global_implid = Nodeid.new_implemid "pads"
let global_docid = Nodeid.new_docid (Nodeid.build_docid_gen())

(* PADS Items *)

class virtual padsItem =
  object (self)
    inherit item

    val impl = global_impl
    val implid = global_implid
    (*val docid = global_docid*)
    val nodeid_context = global_nodeid_ctxt

    method implementation() = impl
    method get_implid = implid
    (*method get_docid = docid*)
    method get_nodeid_context = nodeid_context
  end


(* PADS Nodes *)

class virtual padsNode docid' nr' pnr' =
  object
    inherit node
    inherit padsItem as super

    val docid = docid'

    val nr : Pads_c.nodeRep = nr'
    (* Parent's node rep: *)
    val pnr : Pads_c.nodeRep option = pnr'

    (* val mutable pre : NodeId.preorder option = None *)
    val mutable node_parent : (Dm.node option) = None

    method get_nodeRep = nr

    (* This is an ugly hack: it only works for one-level of the parent axis. *)	
    method parent() = 
      match pnr with 
      |	None -> None
      |	Some nr -> Some ((new padsElementNode docid nr None default_xml_nsenv) :> Dm.node)

    method nodeid() = 
      let printNidDebug nid = 
	let _ = print_string "Name: " in
	let _ = print_string (Pads_c.name nr) in
	let _ = print_string " nid: " in
	let _ = print_string (Int64.to_string nid.Nodeid.gId) in
	let _ = print_string ":" in
	let _ = print_string (Int64.to_string nid.Nodeid.mId) in
	let _ = print_string " implid: " in
	let _ = print_int (super#get_implid) in
	let _ = print_string " docid: " in
	let _ = print_int (docid) in
	print_newline ()
      in
      let nid = Pads_c.get_id nr in
      ((super#get_implid), Nodeid.IntPairId(docid, nid))

    method docorder() = 
      let printDocorderDebug nid =
	let _ = print_string "Name: " in
	let _ = print_string (Pads_c.name nr) in
	let _ = print_string " docorder: " in
	let _ = print_string (Int64.to_string nid.Nodeid.mId) in
	print_newline ()
      in
      let nid = Pads_c.get_id nr in
      ((super#get_implid), Nodeid.PreIntPair(docid, nid))

    (* PADS does not support update methods *)
    method update_parent n = node_parent <- (Some n)
    method insert     	 ns opt = ()
    method delete     	 n = ()
    method replace    	 ns n = ()
    method replace_value avl = ()
  end

(* PADS Document Nodes *)
(* padsDocumentNode is passed the nodeRep of the dummy root element
   node of the virtual XML document. *)
and padsDocumentNode (doc_uri_opt : atomicString option) docid psource_file (nr : Pads_c.nodeRep) =
  object (self)
    inherit document (doc_uri_opt)
    inherit padsNode docid nr (None)

    val mutable kidIdx = 0

    method children () =  
      let nsenv = add_all_ns default_xml_nsenv [(NSPrefix "padsns", NSUri ("file:"^psource_file))] in 
      Cursor.cursor_of_list[((new padsElementNode docid nr None nsenv) :> Dm.node)]

    method document_uri () =  doc_uri_opt

    (* descendant-or-self axis  *)

    (* Note:
         03/03/2004. Currently, descendant-or-self performs
         materialization of a list containing the context node and all
         of its descendants.
      - Jerome *)

    method descendant_or_self () =
      Cursor_descendant.cursor_descendant (Cursor.cursor_of_singleton (self :> node))

    (* descendant axis  *)

    (* Note:
         03/03/2004. descendant is now implemented using
         descendant-or-self.
         - Jerome
     *)
    method descendant () =
      Cursor.cursor_map_concat (fun n -> n#descendant_or_self()) (self#children())

    (* ancestor-or-self axis *)

    (* Note:
         03/03/2004. ancestor-or-self requires to accumulate all of
         the ancestors, as it should return things in document
         order. This should not be a big problem as the materialized
         list will be at most of the depth of the tree.
       - Jerome
     *)

    method ancestor_or_self () =
      let all_ancestors =
	let rec compute_ancestor_or_self current previous =
	  let new_previous = (current :> node) :: previous in
	  let parent = current#parent() in
	  match parent with
	  | None   -> new_previous
	  | Some p -> compute_ancestor_or_self p new_previous
	in
	compute_ancestor_or_self (self :> node) []
      in
      Cursor.cursor_of_list all_ancestors

    (* ancestor axis *)

    (* Note:
         03/03/2004. ancestor is now implemented using
         ancestor-or-self.
         - Jerome
     *)
    method ancestor () =
      let parent = self#parent() in
      match parent with
      | None -> Cursor.cursor_empty()
      | Some p -> p#ancestor_or_self()

  end


(* PADS Element Nodes *)

and padsElementNode docid (nr' : Pads_c.nodeRep) (pnr' : Pads_c.nodeRep option) (nsenv : nsenv) = 
  object (self)
    inherit element
    inherit padsNode docid (nr') (pnr')

    val rsym = Namespace_symbols.relem_symbol (NSDefaultElementPrefix, NSUri "", Pads_c.name(nr'))
    val qn = Some (new atomicQName (Namespace_symbols.anon_symbol(NSDefaultElementPrefix, NSUri "", Pads_c.name(nr'))))

    val mutable typed_content : Dm.atomicValue option = None

    method children () =  
      (* All children are indexed from 0, so start w/ -1 *)
      let k = ref (-1) in
      let f () = 
	incr k;
        match Pads_c.kth_child nr !k with
	  Some(c) -> 
	    if (Pads_c.kind(c) = "element") then
	      Some ((new padsElementNode docid c (Some nr) nsenv) :> Dm.node)
	    else
	      Some ((new padsTextNode docid c (Some nr)) :> Dm.node)
	| None -> None
      in Cursor.cursor_of_function f

    method node_name   () = qn (* atomicQName option *)
    method elemName    () = rsym 

    method node_type   () = Some Namespace_symbols.anytype (* Dm.type_annotation option *)

    method attributes  () = Cursor.cursor_empty ()
    method namespace_environment () = nsenv;
    method typed_value () = 
      match typed_content with
      |	None ->
	  let tv = (Pads_c.typed_value nr') in 
	  typed_content <- Some tv; Cursor.cursor_of_singleton tv
      |	Some tv -> Cursor.cursor_of_singleton tv 

    method export_typed_value =  self#typed_value
    method has_element_content () = true
    method nilled      () = false

    (* descendant-or-self axis  *)

    (* Note:
         03/03/2004. Currently, descendant-or-self performs
         materialization of a list containing the context node and all
         of its descendants.
      - Jerome *)

    method descendant_or_self () =
      Cursor_descendant.cursor_descendant (Cursor.cursor_of_singleton (self :> node))

    (* descendant axis  *)

    (* Note:
         03/03/2004. descendant is now implemented using
         descendant-or-self.
         - Jerome
     *)
    method descendant () =
      Cursor.cursor_map_concat (fun n -> n#descendant_or_self()) (self#children())

    (* ancestor-or-self axis *)

    (* Note:
         03/03/2004. ancestor-or-self requires to accumulate all of
         the ancestors, as it should return things in document
         order. This should not be a big problem as the materialized
         list will be at most of the depth of the tree.
       - Jerome
     *)

    method ancestor_or_self () =
      let all_ancestors =
	let rec compute_ancestor_or_self current previous =
	  let new_previous = (current :> node) :: previous in
	  let parent = current#parent() in
	  match parent with
	  | None   -> new_previous
	  | Some p -> compute_ancestor_or_self p new_previous
	in
	compute_ancestor_or_self (self :> node) []
      in
      Cursor.cursor_of_list all_ancestors

    (* ancestor axis *)

    (* Note:
         03/03/2004. ancestor is now implemented using
         ancestor-or-self.
         - Jerome
     *)
    method ancestor () =
      let parent = self#parent() in
      match parent with
      | None -> Cursor.cursor_empty()
      | Some p -> p#ancestor_or_self()

  end


(* PADS Text Nodes *)
and padsTextNode docid nr' pnr' = 
  object (self)
    inherit text
    inherit padsNode docid nr' pnr'

    val mutable typed_content : Dm.atomicValue option = None
    method string_value () = 
      begin
	let tv = 
	  match typed_content with
	  | None -> let tv = Pads_c.typed_value nr' in typed_content <- Some tv; tv 
	  | Some tv -> tv
	in tv#string_value()
      end
    method typed_value () = 
      match typed_content with
      |	None ->
	  let tv = Pads_c.typed_value nr' in 
	  typed_content <- Some tv; Cursor.cursor_of_singleton tv
      |	Some tv -> Cursor.cursor_of_singleton tv 
(*    method typed_value () = Cursor.cursor_of_singleton(new atomicString(typed_value nr')) *)
    method export_typed_value =  self#typed_value

    (* descendant-or-self axis  *)

    (* Note:
         03/03/2004. Currently, descendant-or-self performs
         materialization of a list containing the context node and all
         of its descendants.
      - Jerome *)

    method descendant_or_self () =
      Cursor_descendant.cursor_descendant (Cursor.cursor_of_singleton (self :> node))

    (* descendant axis  *)

    (* Note:
         03/03/2004. descendant is now implemented using
         descendant-or-self.
         - Jerome
     *)
    method descendant () =
      Cursor.cursor_map_concat (fun n -> n#descendant_or_self()) (self#children())

    (* ancestor-or-self axis *)

    (* Note:
         03/03/2004. ancestor-or-self requires to accumulate all of
         the ancestors, as it should return things in document
         order. This should not be a big problem as the materialized
         list will be at most of the depth of the tree.
       - Jerome
     *)

    method ancestor_or_self () =
      let all_ancestors =
	let rec compute_ancestor_or_self current previous =
	  let new_previous = (current :> node) :: previous in
	  let parent = current#parent() in
	  match parent with
	  | None   -> new_previous
	  | Some p -> compute_ancestor_or_self p new_previous
	in
	compute_ancestor_or_self (self :> node) []
      in
      Cursor.cursor_of_list all_ancestors

    (* ancestor axis *)

    (* Note:
         03/03/2004. ancestor is now implemented using
         ancestor-or-self.
         - Jerome
     *)
    method ancestor () =
      let parent = self#parent() in
      match parent with
      | None -> Cursor.cursor_empty()
      | Some p -> p#ancestor_or_self()

  end

let docid_gen = Nodeid.build_docid_gen()
let uri_docid_table = Hashtbl.create 101

(* Pass name of PADS-generated schema *)
let pads_document proc_ctxt uriopt psource_file nr = 
  start_monitor_call proc_ctxt Prolog "pads_document" ; 
  let docid =
    match uriopt with
    | Some uri ->
	(try 
	  Hashtbl.find uri_docid_table uri
	with Not_found -> 
	  let docid = (Nodeid.new_docid (docid_gen)) in
	  Hashtbl.add uri_docid_table uri docid;
	  docid)
    | None -> Nodeid.new_docid (docid_gen)  (* If there is no uri, then generate a unique docid for this call. *)
  in
  let pdnfn (docid, nr) = new padsDocumentNode uriopt docid psource_file nr in
  let pdn = 
    wrap_monitor 
      proc_ctxt 
      (Monitoring_context.Document_Toplevel_ParsingLoading_Phase "pads_document") 
      pdnfn
      (docid, nr)
  in end_monitor_call proc_ctxt; 
  pdn

let rec tree_walk nodes  = 
  match Cursor.cursor_peek nodes with
  | None -> ()
  | Some _ -> 
      let n = Cursor.cursor_next nodes in
      tree_walk (n#children ());
      tree_walk nodes

let walk_pads_document docitem = 
  tree_walk (Cursor.cursor_of_singleton (Dm_functions.get_node docitem))
  
let _ =
  begin
    (* Some default Galax options *)
    Callback.register "pads_document" pads_document;
    Callback.register "walk_pads_document" walk_pads_document
  end

