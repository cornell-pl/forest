structure Parse =

struct

  exception Unexpected
  open Common
  open Rep
  open TokenDefs
  structure RegExp = RegExpFn (structure P=AwkSyntax structure E=ThompsonEngine) : REGEXP
  structure MT = MatchTree
  structure SS = Substring
  val max_parses_per_line = 1

  structure ParseSet = SplaySetFn(struct
	type ord_key = Rep * metric_type * int  (* (rep, metric, pos) triplet, metric smaller => better *)
	val compare = 
		fn((r1, m1, j1), (r2, m2, j2)) =>
		if better_metric m1 m2 then LESS
		else if better_metric m2 m1 then GREATER
		else if j1 > j2 then LESS
		else if j1 < j2 then GREATER
		else Rep.compare(r1, r2)
  end)

  structure TokenMap = SplayMapFn(struct
                 type ord_key = Token
                 val compare = Tokens.compToken
        end)

  structure StringMap = SplayMapFn(struct
                 type ord_key = string
                 val compare = String.compare
        end)

  fun clean s = 
	let 
(*
	    val items = ParseSet.listItems s
	    fun f ((r1, m1, j1), (r2, m2, j2)) =
		if j1 > j2 the true
		else if j1 < j2 then false
		else if better_metric m2 m1 then true
		else false
	    val sorted_items = ListMergeSort.sort f items
*)
	    fun g ((r, m, j), map) =
		case IntMap.find (map, j) of
		  SOME nil => IntMap.insert (map, j, [(r, m, j)])
		| SOME ((r', m', j')::l) =>
			if better_metric m m' then IntMap.insert (map, j, [(r, m, j)])
			else if (equal_metric m m' andalso Rep.compare (r, r') <> EQUAL) 
			     then IntMap.insert (map, j, ((r, m, j)::(r', m', j')::l)) 
			else map
		| NONE =>  IntMap.insert (map, j, [(r, m, j)])
	    val map = ParseSet.foldl g IntMap.empty s
	    val mylist = List.concat (IntMap.listItems map)
	    val mylist = if length mylist > max_parses_per_line then 
			 List.take ((ListMergeSort.sort 
			  (fn ((_, m1, _), (_, m2, _)) => better_metric m2 m1) mylist), max_parses_per_line)
			 else mylist
	in
	    ParseSet.addList (ParseSet.empty, mylist)
	end
	    		
	
  val tmap = 
	let fun add ((t, re), map) = TokenMap.insert (map, t, re)
	in foldl add TokenMap.empty TokenDefs.tokenDefList
	end

  val tokenRegexMap = 
	let val m = TokenMap.insert(TokenMap.empty, Pint (0, ""), (RegExp.compileString "[\\-~]?([0-9]+)"))
	in (ref m)
	end

  val strRegexMap = 
	let val m = StringMap.insert(StringMap.empty, "[\\-~]?([0-9]+)", (RegExp.compileString "[\\-~]?([0-9]+)"))
	in (ref m)
	end

  fun parse_base (t : Token, start, input) =
    let val s = SS.extract (input, start, NONE) 
    in
      case TokenMap.find (tmap, t) of
	SOME re_str =>
	  let 
	      (* val _ = print ("matching " ^ (tokenTyToString t) ^ "\nagainst (" ^ SS.string s ^ ")\n") *)
	      val regex = 
			case TokenMap.find (!tokenRegexMap, t) of
			SOME r => r
			| NONE => 
				let val r = RegExp.compileString re_str
				     val _ = tokenRegexMap := TokenMap.insert (!tokenRegexMap, t, r) 
				in r
				end
	      (*
	      val _ = print ("regex compilation complete\n")
	      *)
	      val result_opt = (RegExp.prefix regex SS.getc s) 
	      (*
	      val _ = print ("regex match complete\n")
	      *)
	  in
	     case result_opt of
	       NONE =>  ((* print "match not found\n";*) (BaseR ErrorB, (1, 0, 0), start))
	     | SOME (match_tree, s') =>
		(
		  let 
		    val pair = MT.root match_tree
		    val len = #len pair
		    val matched_ss = SS.slice(#pos pair, 0, SOME len)
		    val outs = SS.string matched_ss
		    (* val _ = print ("found match (" ^ outs ^ ")\n") *)
		    val j = start + len
		    val tok = case t of
				   Ptime i     => Ptime outs
			        |  Pdate i     => Pdate outs
			        |  Pip i       => Pip outs
			        |  Phostname i => Phostname outs
			        |  Ppath i     => Ppath outs
			        |  Purl i      => Purl outs
			        |  Pemail i      => Pemail outs
			        |  Pmac i      => Pmac outs
			        |  PbXML (f,s) => PbXML(outs, "")
			        |  PeXML (f,s) => PeXML(outs, "")
			        |  Pint _    => 
				       (
					case (LargeInt.fromString outs) of
					  SOME i => Pint(i, outs)
					| _ => raise TyMismatch
				       )
			        |  Pfloat _    => 
				  let val (d,f) = SS.splitl (fn c => c <> #".") matched_ss
				  in Pfloat(SS.string d, SS.string f)
				  end
			        |  Pstring s => Pstring outs
			        |  Pwhite s  => Pwhite outs
			        |  Ptext s  => Ptext outs
			        |  Other c   => Other (SS.sub (matched_ss, 0)) 
			        |  Pempty    => Pempty
				|  _ => raise TyMismatch
		  (* val _ = print ("Token is " ^ (tokenTyToString tok) ^ "\n") *)
		  in (BaseR (GoodB tok), (0, 0, len), j)
		  end
		)
	  end

    	| _ => 
	  (
	    case t of
	      Pempty => (BaseR (GoodB Pempty), (0, 0, 0), start)
	    | Other c => (case SS.first s of
			   SOME c' => 
				if c = c' then (BaseR (GoodB t), (0, 0, 1), start+1)
				else (BaseR ErrorB, (1, 0, 0), start)
			 | NONE =>  (BaseR ErrorB, (1, 0, 0), start)
			 )
	    | _ => ((* print (tokenTyToString t); *) raise TyMismatch)
	  )
    end

  fun escapeRE s =
     let
        fun escapeChar (c:char) : string = case c of
          #"?" => "\\?"
        | #"*" => "\\*"
        | #"+" => "\\+"
        | #"|" => "\\|"
        | #"$" => "\\$"
        | #"^" => "\\^"
        | #";" => "\\;"
        | #"." => "\\."
        | #"=" => "\\="
        | #"-" => "\\-"
        | #"/" => "\\/"
        | #"\\" => "\\\\"
        | #"(" => "\\("
        | #")" => "\\)"
        | #"[" => "\\["
        | #"]" => "\\]"
        | #"<" => "\\<"
        | #">" => "\\>"
        | #"{" => "\\{"
        | #"}" => "\\}"
        | _ => Char.toString c
     in
        String.translate escapeChar s
     end

  (* return (recovered string, matched string, new index) *)
  fun parse_regex (re_str, start, input) =
    let
        val s = SS.extract (input, start, NONE) 
	(* val _ =  print ("against (" ^ (SS.string s) ^ ")\n") *)
        val regex = 
			case StringMap.find (!strRegexMap, re_str) of
			  SOME r => r
			| NONE => 
				let val r = RegExp.compileString re_str
	    			    val _ = strRegexMap := StringMap.insert (!strRegexMap, re_str, r) 
			        in r
				end
	fun reader (counter, ss) = 
		case SS.getc ss of
		  SOME(c, remainder) => SOME((c, (counter+1, remainder)))
		| NONE => NONE  
        val result_opt = (RegExp.find regex reader (0, s))
    in
   	case result_opt of
	  NONE => ((* print "no match!\n";*) (NONE, NONE, start))
	| SOME (matched, (counter, remainder)) =>
	  (
	      let 
		val pair = MT.root matched
		val (matched_index, s') = #pos pair
		val matched_len = #len pair
	    	val matched_ss = SS.slice(s', 0, SOME matched_len)
	    	val outs = SS.string matched_ss
		(* val _ = print ("count = " ^ (Int.toString counter) ^ "\n")*)
	    	val j = start + counter + matched_len
	      in
		if matched_index > 0 then (* there's recovered data *)
		  let val recovered_s = SS.string (SS.slice(s, 0, SOME matched_index))
		  in ((* print ("recovered (" ^ recovered_s ^")(" ^ outs ^")!\n"); *) 
			(SOME recovered_s, SOME outs, j))
		  end
		else ((* print ("matched ("^ outs ^ ")!\n"); *)
			(NONE, SOME outs, j))
	      end
	  )
    end

  fun parse_sync (refined, start, input) =
	(
	(*
	print ("Parsing: " ^ refinedToString refined ^ " at pos " ^ Int.toString start ^ "\n");  
	*)
	case refined of
	  StringME re =>
	    let 
		val re_str = String.substring (re, 1, (size re)-2) (*remove the / and / *)
	        val (recovered, matched, j) = parse_regex (re_str, start, input)
	    in
		case (recovered, matched) of
		  (NONE, SOME s) => [(SyncR(Good (s, StringConst s)), (0, 0, String.size s), j)]
		| (SOME r, SOME s) => [(SyncR(Recovered (r, s, StringConst s)), 
					(2, String.size r, String.size s), j)]
		| _ => [(SyncR Fail, (1, 0, 0), start)]
	    end
	| IntConst li => 
	    let val str = 
		  if li >= 0 then "0*" ^ (LargeInt.toString li)
		  else ("\\-0*" ^ LargeInt.toString (~li))
	        val (recovered, matched, j) = parse_regex (str, start, input)
	    in
		case (recovered, matched) of
		  (NONE, SOME s) => [(SyncR(Good (s, IntConst li)), (0, 0, String.size s), j)]
		| (SOME r, SOME s) => [(SyncR(Recovered (r, s, IntConst li)), 
					(2, String.size r, String.size s), j)]
		| _ => [(SyncR Fail, (1, 0, 0), start)]

		end
	| Int (min, max) => 
		let val s = "[\\-~]?([0-9]+)" 
		    val regex = 
				case StringMap.find (!strRegexMap, s) of
				  SOME r => r
				| NONE => 
					let val r = RegExp.compileString s
		    			    val _ = strRegexMap := StringMap.insert (!strRegexMap, s, r)
				        in r
					end
		    fun reader (counter, ss) = 
			case SS.getc ss of
		  	  SOME(c, remainder) => SOME((c, (counter+1, remainder)))
			| NONE => NONE  
        	    val mystring = SS.extract (input, start, NONE) 
		    fun find_next (s, index) =
		    (* index is the pos relative to start *)
		      let val result_opt = (RegExp.find regex reader (0, s))
		      (*
			  val _ = print ("matching " ^ SS.string s ^ " index = " ^ Int.toString index ^ "\n")
		      *)
		      in
		  	case result_opt of
		    	  NONE => [(SyncR Fail, (1, 0, 0), start)]
		  	| SOME (matched, (matched_index, remainder)) =>
			  (
			      let
				val pair = MT.root matched
				val (_, s') = #pos pair
				val matched_len = #len pair
	    			val matched_ss = SS.slice(s', 0, SOME matched_len)
	    			val outs = SS.string matched_ss
				val outint = some(LargeInt.fromString outs)
			     	val num = case (LargeInt.fromString outs) of
	   				    NONE => raise Unexpected
					  | SOME n => n
				val skipped_len = index + matched_index
			      in
				  if num < min orelse num > max then 
				     let val remaining = SS.slice(remainder, matched_len, NONE)
				     in
					find_next (remaining, skipped_len + matched_len)
				     end
				  else if skipped_len > 0 then (* there's recovered data *)
		  		    let val recovered_s = SS.string 
						(SS.slice(mystring, 0, SOME skipped_len))
		  		    in [(SyncR (Recovered (recovered_s, outs, IntConst outint)), 
					(2, skipped_len, matched_len), start + skipped_len + matched_len)]
		  		    end
				  else [(SyncR (Good (outs, IntConst outint)), (0, 0, matched_len), start+matched_len)] 
			      end
			  )
		      end
		in find_next (mystring, 0) 
		end
	| FloatConst (i, f) => 
	    let val (recovered, matched, j) = parse_regex (escapeRE ("0*" ^ i ^ "." ^ f ^ "0*"), start, input)
	    in
		case (recovered, matched) of
		  (NONE, SOME s) => [(SyncR(Good (s, FloatConst (i, f))), (0, 0, String.size s), j)]
		| (SOME r, SOME s) => [(SyncR(Recovered (r, s, FloatConst (i, f))), 
					(2, String.size r, String.size s), j)]
		| _ => [(SyncR Fail, (1, 0, 0), start)]
	    end
	| StringConst s => 
	    let val (recovered, matched, j) = parse_regex(escapeRE s, start, input)
	    in
		case (recovered, matched) of
		  (NONE, SOME s) => [(SyncR(Good (s, StringConst s)), (0, 0, String.size s), j)]
		| (SOME r, SOME s) => [(SyncR(Recovered (r, s, StringConst s)), 
					(2, String.size r, String.size s), j)]
		| _ => [(SyncR Fail, (1, 0, 0), start)]
	    end

	| Enum res => List.foldl (fn (r, l) => l@ (parse_sync(r, start, input))) nil res
	| Blob (str, patt) =>
	  (
	  case (str, patt) of
	    (SOME s, NONE) =>
	    	let val (recovered, matched, j) = parse_regex(escapeRE s, start, input)
		in
		  case (recovered, matched) of
		    (NONE, SOME s) => [(SyncR(Good ("", StringConst "")), (0, 0, 0), start)]
		  | (SOME r, SOME s) => [(SyncR(Good (r, StringConst r)), 
					(0, 0, String.size r), j - (String.size s))]
		  | _ => [(SyncR Fail, (1, 0, 0), start)]
		end
	  | (NONE, SOME re) => 
	    	let 
		  val re_str = String.substring (re, 1, (size re)-2) (*remove the / and / *)
	          val (recovered, matched, j) = parse_regex (re_str, start, input)
		in
		  case (recovered, matched) of
		    (NONE, SOME s) => [(SyncR(Good ("", StringConst "")), (0, 0, 0), start)]
		  | (SOME r, SOME s) => [(SyncR(Good (r, StringConst r)), 
					(0, 0, String.size r), j - (String.size s))]
		  | _ => [(SyncR Fail, (1, 0, 0), start)]
		end
	  | _ => (* blob to the end of line *)
		let 
		  val s = String.extract (input, start, NONE)
		  (* val _ = print ("Blob is " ^ s ^ "\n") *)
		in [(SyncR(Good (s, StringConst s)), (0, 0, String.size s), start + (String.size s))]
		end
	  )
	| _ => raise TyMismatch
	)		       	 
  
(* environment e is Label -> Rep map, currently the env stores rep for only three base types:
   Pint, Pstring and Other, and some refined base types *)
  fun parse_all (ty:Ty, e: Rep LabelMap.map, i: int, input: string) : ParseSet.set =
    case ty of
      Base (a, ts) => 
	(
	  case ts of
		nil => raise TyMismatch
	      | (t, l)::_ => ParseSet.singleton (parse_base (t, i, input))
	)
    | RefinedBase(a, r, ts) => 
	ParseSet.addList(ParseSet.empty, parse_sync (r, i, input))
    | Pstruct(a, tys) => 
	let fun parse_struct tys env start =
	    case tys of
	      nil => ParseSet.singleton((TupleR nil, (0, 0, 0), start))
	    | ty::tys =>
		let
		(* 
		  val _ = print "Parsing Ty:\n"
		  val _ = printTy ty
		*)
		  val this_set = parse_all(ty, env, start, input)
		  val idop = case ty of
				Base (a, ((Pint _), _)::_) => SOME (getLabel a)
			      | Base (a, ((Pstring _), _)::_) => SOME (getLabel a)
			      | Base (a, ((Other _), _)::_) => SOME (getLabel a)
			      | RefinedBase (a, Enum _, _) => SOME (getLabel a)
			      | RefinedBase (a, Int _, _) => SOME (getLabel a)
			      | _ => NONE
		  fun gg ((r, m, j), set) =
			let val newe = 
			  	case idop of
				  SOME id => 
				  (
					(*
					print ("Inserting " ^ Atom.toString id ^ ": " ^ 
						(repToString "" r) ^ "\n");
					*)
					LabelMap.insert(env, id, r)
				  )
				| _ => env
			    val news = parse_struct tys newe j
		  	    val newset = ParseSet.map
			      (fn (TupleR rlist, m', j') => (TupleR (r::rlist), add_metric m  m', j')) news
		  	in	
			   ParseSet.union (set, newset)
			end
		in 
		  ParseSet.foldl gg ParseSet.empty this_set
		end
	in 
	  parse_struct tys e i
	end
    | Punion (a, tys) =>
	(* branch number starts from 0 *)
	let fun g (ty, (parse_set, branchno)) =
		let val set = parse_all (ty, e, i, input) 
		    val newset = ParseSet.map (fn (r, m, j) => (UnionR (branchno, r), m, j)) set
		in (ParseSet.union (parse_set, newset), branchno+1)
		end
	in #1 (foldl g (ParseSet.empty, 0) tys)
	end
    | Switch (a, id, retys) => 
	let fun select retys re branchno =
		case retys of
		  nil => NONE
		| (r, t)::retys => 
		  (
		    case r of
		      Enum l =>
			  if List.exists (fn r => refine_equal(r, re)) l then SOME (branchno, Enum l, t)
			  else select retys re (branchno+1)
		    | _ => if refine_equal (r, re) then SOME (branchno, re, t)
			   else select retys re (branchno+1)
		  )
	     val re_to_search = 
		case LabelMap.find (e, id) of
		  SOME (BaseR (GoodB (Pint (i, _)))) => IntConst i
		| SOME (BaseR (GoodB (Pstring s))) => StringConst s
		| SOME (BaseR (GoodB (Other c))) => StringConst (String.str c)
		| SOME (SyncR (Good  (_, refined))) => refined
		| SOME (SyncR (Recovered (_, _, refined))) => refined
		| _ => StringConst "*" (* go with default branch if there's any *)
	     val search_result = select retys re_to_search 0
	     val newset =
		case search_result of
		  NONE => 
		    let val recovered_string = String.extract (input, i, NONE) 
		        val len = String.size recovered_string
		    in
			ParseSet.singleton (SwitchR (re_to_search, 
			SyncR (Recovered (recovered_string, "",  StringME ("/$/")))), (2, len, 0), i+len)
		    end
		| SOME (branchno, re, selected_ty) =>
		  let  
 	     		val set = parse_all (selected_ty, e, i, input) 
	     	  in ParseSet.map (fn (r, m, j) => (SwitchR (re, r), m, j)) set
		  end
	in newset
	end
    | RArray (a, sep, term, body, len, lengths) => 
     (
      let fun merge_s ((r, m, j), set, has_sep) =
		case r of
		  ArrayR(elems, seps, termop) =>
		    if has_sep then
		      ParseSet.map (fn (body_sep, m', j') =>
				   case body_sep of
				     TupleR ([elemR, sepR]) => 
					(ArrayR(elems@[elemR], seps@[sepR], termop), add_metric m  m', j')
				   | _ => raise TyMismatch) set
		    else 
		      ParseSet.map (fn (elemR, m', j') =>
				(ArrayR(elems@[elemR], seps, termop), add_metric m  m', j')) set
		| _ => raise TyMismatch

          fun merge_t ((r, m, j), set, has_term) = 
		case r of
		  ArrayR(elems, seps, termop) =>
		    if has_term then
		      ParseSet.map (fn (body_term, m', j') =>
				   case body_term of
				     TupleR ([elemR, termR]) => 
				       (* we compute the length of the terminator and substract that from j' - do not consume
					  terminator *)
				       let val termlen = 
					case termR of
					  SyncR(Good (s, _)) => String.size s
					| SyncR(Recovered (s1, s2, _)) => String.size (s1 ^ s2)
					| _ => 0
				       in
					  (ArrayR(elems@[elemR], seps, SOME termR), add_metric m  m', j'-termlen)
				       end
				   | _ => raise TyMismatch) set
		    else 
		      ParseSet.map (fn (elemR, m', j') =>
				(ArrayR(elems@[elemR], seps, termop), add_metric m  m', j')) set
		| _ => raise TyMismatch
          fun pair (b, r) = Pstruct (mkTyAux 0, [b, RefinedBase (mkTyAux 0, r, nil)])
      in
      case len of
        NONE =>	
	(
	 let
	   val (body_sep, has_sep) = case sep of
	     	        NONE => (body, false)
	     	     | SOME sep => (pair (body, sep), true)
	   val (body_term, has_term) = case term of
	     		NONE => (body, false)
	     	      | SOME term => (pair (body, term), true)
	   fun parse_array (e, parse_set) =
	     if ParseSet.numItems parse_set = 0 then parse_set
	     else
	       let 
		 val sepmap = IntMap.empty	
	         val termmap = IntMap.empty	
		 (*
		 val _ = print ("Size of input set is " ^ Int.toString (ParseSet.numItems parse_set) ^ "\n")
		 val _ = print "Parsing element:\n"
		 val _ = printTy body_sep
		 *)
	         fun f ((prev_r, m, start), (seprs, termrs, sepmap, termmap)) =
	     	   let
		    (*
		    val _ = ( print ("At pos " ^ (Int.toString start) ^ ":\n"); 
				 print (repToString "" prev_r))
		    *) 
		    val (sep_set, sepmap)  = 
	     		case IntMap.find(sepmap, start) of
	     		  SOME s => 
				((merge_s ((prev_r, m, start), s, has_sep), sepmap))
	     		| NONE =>
	     		  let 
			      val s = parse_all (body_sep, e, start, input)
			      (*
			      val _ = print "Before clean:\n"
			      val _ = ParseSet.app (fn (r, m, j) => print ((repToString "" r) ^ "Metric = " ^ (metricToString m) ^ "\n")) s
			      *)
			      val s = clean (ParseSet.filter 
					     (fn (r, m, j) => (j > start)) s)
			      (*
			      val _ = print "After clean:\n"
			      val _ = ParseSet.app (fn (r, m, j) => print ((repToString "" r) ^ "Metric = " ^ (metricToString m) ^ "\n")) s
			      *)
	     		      val sepmap = IntMap.insert(sepmap, start, s)
	     		  in (clean (merge_s ((prev_r, m, start), s, has_sep)), sepmap)
	     		  end
	     	    val (term_set, termmap) = 
	     		case IntMap.find(termmap, start) of
	     		  SOME s => (merge_t ((prev_r, m, start), s, has_term), termmap)
	     		| NONE =>
	     		  let val s = clean (ParseSet.filter (fn (r, m, j) => j > start)
	     				(parse_all (body_term, e, start, input)))
	     		      val termmap = IntMap.insert(termmap, start, s)
	     		  in (clean (merge_t ((prev_r, m, start), s, has_term)), termmap)
	     		  end
	     	   in (ParseSet.union(seprs, sep_set), ParseSet.union(termrs, term_set),
	     	       sepmap, termmap)
	     	   end
	         val (seps, terms, sepmap, termmap) = ParseSet.foldl f 
	     		(ParseSet.empty, ParseSet.empty, sepmap, termmap) parse_set
	         val sep' = parse_array (e, seps) 
	       in
	         ParseSet.union (sep', terms)
	       end
	   val non_empty_set = parse_array (e, ParseSet.singleton(ArrayR(nil, nil, NONE), (0, 0, 0), i))
	  in
		(* we have to add a parse that is an zero-length array *)
		ParseSet.add (non_empty_set, (ArrayR(nil, nil, NONE), (0, 0, 0), i))
	  end  
       )	
     | SOME x => 
	(* assume index starts from 0 *)
	let val len = 
		(
		case x of
		  IntConst i => Int.fromLarge i
		| LabelRef id => 
			(
			case LabelMap.find(e, id) of
			  SOME (BaseR (GoodB (Pint(i, _)))) => Int.fromLarge i
			| SOME (SyncR (Good (_, IntConst i))) => Int.fromLarge i
			| _ => raise TyMismatch
			)
		| _ => raise TyMismatch
		)
	fun parse_fixed_len_array (e, parse_set, index) =
	  if len = 0 then parse_set
	  else if index = len - 1 then
	    let fun f ((r, m, start), (set, map)) = 
	        let val (body_term, has_term) = 
			case term of 
			  NONE => (body, false)
			| SOME term => (pair (body, term), true)
		    val (term_set, termmap) = 
			case IntMap.find(map, start) of
			  SOME s => (merge_t ((r, m, start), s, has_term), map)
			| NONE =>
			  let val s = clean (ParseSet.filter (fn (r, m, j) => j > start)
					(parse_all (body_term, e, start, input)))
			      val map = IntMap.insert(map, start, s)
			  in (merge_t ((r, m, start), s, has_term), map)
			  end
		in (ParseSet.union(set, term_set), termmap)
		end
	    val (new_parse_set, newmap) =
		ParseSet.foldl f (ParseSet.empty, IntMap.empty) parse_set in
	    new_parse_set
	    end
	  else 
	    let fun f ((r, m, start), (set, map)) = 
	        let val (body_sep, has_sep) = 
			case sep of
			  NONE => (body, false)
			| SOME sep => (pair (body, sep), true)
		    val (sep_set, sepmap) = 
			case IntMap.find(map, start) of
			  SOME s => (merge_s ((r, m, start), s, has_sep), map)
			| NONE =>
			  let val s = clean(ParseSet.filter (fn (r, m, j) => j > start)
					(parse_all (body_sep, e, start, input)))
			      val map = IntMap.insert(map, start, s)
			  in (merge_s ((r, m, start), s, has_sep), map)
			  end
		in (ParseSet.union(set, sep_set), sepmap)
		end
	        val (new_parse_set, newmap) =
		   ParseSet.foldl f (ParseSet.empty, IntMap.empty) parse_set 
	    in parse_fixed_len_array (e, new_parse_set, index+1)
	    end
	in
	  parse_fixed_len_array (e, ParseSet.singleton(ArrayR(nil, nil, NONE), (0, 0, 0), i), 0)
	end
      end  
      )
    | Poption (a, ty) => 
	let val set = parse_all (ty, e, i, input) 
	    val newset = ParseSet.map (fn (r, m, j) => (OptionR(SOME r), m, j)) set
	in
	    ParseSet.add (newset, (OptionR NONE, (0, 0, 0), i))
	end
    | _ => raise TyMismatch
	
end 
