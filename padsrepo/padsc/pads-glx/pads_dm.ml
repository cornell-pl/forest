(* Module: PADSDM
   Description:
     This module implements the PADS datamodel. 
*)

(* open Conf *)
open Monitor
open Monitoring_context

open Namespace_context
open Namespace_names

open Datatypes
open Dm_atomic

(* Generic data model *)

open Dm_types
open Dm_step
open Dm
open Cursor

(* open Pads_c *)

(* For now, we use one, global nodeid_context for the whole module.
   In the future, we should probably have PADS supply this for us. *)

let global_nodeid_ctxt = Nodeid_context.default_nodeid_context()
let global_impl = "pads"
let global_implid = Nodeid.new_implemid "pads"
let global_docid = Nodeid.new_docid (Nodeid.build_docid_gen())
let element_prefix = NSDefaultElementPrefix
let element_uri = NSUri ""
let pads_prefix = NSPrefix "padsns"

(* PADS Nodes *)
class virtual padsNode docid' nr' pnr' =
  object(self)
    inherit node

    val impl = global_impl
    val implid = global_implid
    val nodeid_context = global_nodeid_ctxt
 
    method implementation () = impl
    method get_implid = implid
    method get_nodeid_context = nodeid_context

    val docid = docid'

    val nr : Pads_c.nodeRep = nr'
    (* Parent's node rep: *)
    val pnr : Pads_c.nodeRep option = pnr'

    (* val mutable pre : NodeId.preorder option = None *)
    val mutable node_parent : (Dm.node option) = None

    method get_nodeRep = nr

  (* Axes *)

    (* This is an ugly hack: it only works for one-level of the parent axis. *)	
    method parent nto = 
      let node_parent = 
	match pnr with 
	| None -> None
	| Some nr -> Some ((new padsElementNode docid nr (element_prefix, element_uri, Pads_c.name nr) None default_xml_nsenv) :> Dm.node)
      in
      match nto with
      | None ->
	  node_parent
      | Some (cx,nt) ->
	  let ntf x = eval_node_test_gen (self#get_access_ops_dm) cx Xquery_common_ast.Parent nt x in
	  Cursor.cursor_peek (Cursor.cursor_filter ntf (cursor_of_option node_parent))

  (* Node identity *)

    method nodeid() = 
      let printNidDebug nid = 
	let _ = print_string "Name: " in
	let _ = print_string (Pads_c.name nr) in
	let _ = print_string " nid: " in
	let _ = print_string (Int64.to_string nid.Nodeid.gId) in
	let _ = print_string ":" in
	let _ = print_string (Int64.to_string nid.Nodeid.mId) in
	let _ = print_string " implid: " in
	let _ = print_int (self#get_implid) in
	let _ = print_string " docid: " in
	let _ = print_int (docid) in
	print_newline ()
      in
      let nid = Pads_c.get_id nr in
      ((self#get_implid), Nodeid.IntPairId(docid, nid))

    method docorder() = 
      let printDocorderDebug nid =
	let _ = print_string "Name: " in
	let _ = print_string (Pads_c.name nr) in
	let _ = print_string " docorder: " in
	let _ = print_string (Int64.to_string nid.Nodeid.mId) in
	print_newline ()
      in
      let nid = Pads_c.get_id nr in
      ((self#get_implid), Nodeid.PreIntPair(docid, nid))

    method update_parent n = node_parent <- (Some n)
    method reset_parent () = raise(Error.Query(Error.Prototype("Reset_parent operation not implemented in PADX \n")))

    (* PADS does not support update methods *)
    method delete     	 n = raise(Error.Query(Error.Prototype("Delete operation not implemented in PADX \n")))
    method detach     	 n = raise(Error.Query(Error.Prototype("Detach operation not implemented in PADX \n")))
    method insert     	 ns opt = raise(Error.Query(Error.Prototype("Insert operation not implemented in PADX \n")))
    method replace    	 ns n = raise(Error.Query(Error.Prototype("Replace operation not implemented in PADX \n")))
    method replace_value avl = raise(Error.Query(Error.Prototype("Replace_value operation not implemented in PADX \n")))
    method rename        qn = raise(Error.Query(Error.Prototype("Rename operation not implemented in PADX \n")))
  end

(* PADS Document Nodes *)
(* padsDocumentNode is passed the nodeRep of the dummy root element
   node of the virtual XML document. *)
and padsDocumentNode (doc_uri_opt : atomicAnyURI option ref) docid psource_file (nr : Pads_c.nodeRep) =
  object (self)
    inherit document (doc_uri_opt)
    inherit padsNode docid nr (None)

    val mutable kidIdx = 0
    val pads_uri = NSUri (psource_file)

    method children nto =  
      let pads_lcname = Pads_c.name nr in
      let nsenv = add_all_ns default_xml_nsenv [(pads_prefix, pads_uri)] in 
      let kid = Cursor.cursor_of_list[((new padsElementNode docid nr (pads_prefix, pads_uri, pads_lcname) None nsenv) :> Dm.node)] in
      match nto with 
      |	None -> kid
      |	Some (_, nt) ->
	  begin
	    match nt with
	    | Xquery_algebra_ast.APNameTest anon_symbol -> 
		begin
		  let pads_symbol = Namespace_symbols.anon_symbol (pads_prefix, pads_uri, pads_lcname) in
		  if (Namespace_symbols.anon_subtag pads_symbol anon_symbol) then kid 
		  else Cursor.cursor_empty()
		end
  	    (* Currently, PADS_DM does not implement all the variants of node-kind tests *)
	    | Xquery_algebra_ast.APNodeKindTest(Xquery_algebra_ast.AElementKind(Xquery_algebra_ast.AElementTest None)) -> 
		kid
	    | _ -> Cursor.cursor_empty()
	  end

    method document_uri () =  (!doc_uri_opt)
  end

(* PADS Element Nodes *)

and padsElementNode docid (nr' : Pads_c.nodeRep) rqname (pnr' : Pads_c.nodeRep option) (nsenv : nsenv) = 
  object (self)
    inherit element (ref None)  (* Not sure what to do here -- should base URI be propgated from document? *)
    inherit padsNode docid (nr') (pnr')

(*    val rqname = (NSDefaultElementPrefix, NSUri "", Pads_c.name(nr')) *)

    val mutable typed_content : Dm_atomic.atomicValue option = None

    method children nto =  
      let k = ref (-1) in
      (* All children are indexed from 0, so start w/ -1 *)
      let kth_child match_node_kind = 
	let f () = 
	  incr k;
          match Pads_c.kth_child nr !k with
	    Some(c) -> 
	      let kind = Pads_c.kind(c) in
	      if (match_node_kind kind) then
		if (kind = "element") then
		  Some ((new padsElementNode docid c (element_prefix, element_uri, Pads_c.name c) (Some nr) nsenv) :> Dm.node)
		else 
		  Some ((new padsTextNode docid c (Some nr)) :> Dm.node)
	      else None
	  | None -> None
	in Cursor.cursor_of_function f
      in
      match nto with 
      |	None ->  kth_child (fun nk -> true)
      |	Some (_, nt) ->
	  begin
	    match nt with
	    | Xquery_algebra_ast.APNameTest anon_symbol -> 
		begin
		  let (prefix, uri, test_lcname) = (Namespace_symbols.anon_name anon_symbol) in
		  (* This is really ugly...and a horrid hack *)
		  if (uri = element_uri || uri = Namespace_names.NSWildcardUri) then
		      if (test_lcname = "*") then kth_child (fun nk -> true)
		      else
			let f () = 
			  incr k;
			  match Pads_c.kth_child_named nr !k test_lcname with
			  | Some c -> 
			      Some ((new padsElementNode docid c (element_prefix, element_uri, Pads_c.name c) (Some nr) nsenv) :> Dm.node)
			  | None -> None
			in Cursor.cursor_of_function f
		  else 
                    Cursor.cursor_empty() 
		end
  	    (* Currently, PADS_DM does not implement all the variants of node-kind tests *)
	    | Xquery_algebra_ast.APNodeKindTest(Xquery_algebra_ast.AElementKind(Xquery_algebra_ast.AElementTest None)) -> 
		kth_child (fun nk -> nk = "element")
	    | Xquery_algebra_ast.APNodeKindTest(Xquery_algebra_ast.ATextKind) ->
		kth_child (fun nk -> nk = "text")
	    | _ -> Cursor.cursor_empty ()
	  end

    method node_name   () = Some (new atomicQName (Namespace_symbols.anon_symbol(rqname))) (* atomicQName option *)
    method elemName    () = Namespace_symbols.relem_symbol rqname

    method node_type   () = Namespace_symbols.anytype 

    method attributes  nto = Cursor.cursor_empty ()
    method namespace_environment () = nsenv;
    method typed_value () = 
      match typed_content with
      |	None ->
          (* Strangely enough, we have to _unbox_ the typed value
             here, which is returning an item, not an atomicValue *)
	  (* Problem is here *)
	  let tv =  Dm_functions.get_atomicValue(Pads_c.typed_value nr') 
          (* new atomicString("element_typed_value") *)
	  in 
	  typed_content <- Some tv; Cursor.cursor_of_singleton tv
   |	Some tv -> Cursor.cursor_of_singleton tv 

    method export_typed_value () =  Cursor.list_of_cursor "Pads_dm.padsElementNode.export_typed_value" (self#typed_value())
    method has_element_content () = true
    method nilled      () = false
  end


(* PADS Text Nodes *)
and padsTextNode docid nr' pnr' = 
  object (self)
    inherit text
    inherit padsNode docid nr' pnr'

    val mutable typed_content : Dm_atomic.atomicValue option = None
    method string_value () = 
      begin
	let tv = 
	  match typed_content with
	  | None -> 
          (* Strangely enough, we have to _unbox_ the typed value
             here, which is returning an item, not an atomicValue *)
              let tv = Dm_functions.get_atomicValue(Pads_c.typed_value nr') in
	      let kind = Dm_util.string_of_atomic_value_kind(tv#getAtomicValueKind()) in
	      typed_content <- Some tv; tv 
	  | Some tv -> tv
	in tv#string_value()
      end
    method typed_value () = 
      match typed_content with
      |	None ->
          (* Strangely enough, we have to _unbox_ the typed value
             here, which is returning an item, not an atomicValue *)
          (* Dm_functions.get_atomicValue(Pads_c.typed_value nr')*)
	  let tv = Dm_functions.get_atomicValue(Pads_c.typed_value nr')
	  in 
	  typed_content <- Some tv; Cursor.cursor_of_singleton tv
      |	Some tv -> Cursor.cursor_of_singleton tv 

    method export_typed_value () =  Cursor.list_of_cursor "Pads_dm.padsTextNode.export_typed_value" (self#typed_value())
  end

let docid_gen = Nodeid.build_docid_gen()
let uri_docid_table = Hashtbl.create 101

(* Pass name of PADS-generated schema *)
let pads_document proc_ctxt uri psource_file nr = 
  (* Conf.print_physical_algebra := true; *)
  (* Normalization identity function, i.e., don't normalize the input query: 
  Processing_context.set_normalization_ident proc_ctxt true; *)
  start_monitor_call proc_ctxt Prolog "pads_document" ; 
  let docid =
    try 
      Hashtbl.find uri_docid_table uri
    with Not_found -> 
      let docid = (Nodeid.new_docid (docid_gen)) in
      Hashtbl.add uri_docid_table uri docid;
      docid
  in
  let pdnfn (docid, nr) = new padsDocumentNode (ref(Some(new atomicAnyURI (AnyURI._uri_of_string uri)))) docid psource_file nr in
  let pdn = 
    wrap_monitor 
      proc_ctxt 
      (Monitoring_context.Document_Toplevel_ParsingLoading_Phase "pads_document") 
      pdnfn
      (docid, nr)
  in end_monitor_call proc_ctxt; 
  Dm_functions.to_node_item pdn

let rec tree_walk nodes  = 
  match Cursor.cursor_peek nodes with
  | None -> ()
  | Some _ -> 
      let n = Cursor.cursor_next nodes in
      tree_walk (n#children None);
      tree_walk nodes

let walk_pads_document docitem = 
  tree_walk (Cursor.cursor_of_singleton (Dm_functions.get_node docitem))

let _ =
  begin
    (* Some default Galax options *)
    Callback.register "pads_document" pads_document;
    Callback.register "walk_pads_document" walk_pads_document
  end

