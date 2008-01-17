(*this program contains functions that populate a hand-written Ty structure
with data from a data file
Kenny Zhu 4/20/2007 *)
structure Populate =
struct
    open Structure
    open Types
    open Common
    open Model
    exception TyMismatch
    structure RegExp = RegExpFn (structure P=AwkSyntax structure E=DfaEngine) : REGEXP
    structure MT = MatchTree
    structure SS = Substring
    
    val recordNo = ref 0
    fun initTyList labelmap tys =
	case tys of 
	  nil => (labelmap, nil)
	  | ty::tail => 
	  let
	    val (labelmap', ty') = initializeTy labelmap ty
	    val (labelmap'', tail') = initTyList labelmap' tail
	  in (labelmap'', ty'::tail')
	  end
   (*function to assign labels to the nodes and clear the coverage
    and tokenlists in the tree, returns newlabelmap and newty *)
    and initializeTy labelmap ty =
      let
	val aux = getAuxInfo ty
   	val oldLabel = (#label aux)
	val newLabel = getLabel({coverage=0, label = NONE, tycomp=zeroComps})
	val labelmap' = case oldLabel of 
			NONE => labelmap
			| _ => LabelMap.insert(labelmap, some(oldLabel), newLabel)
	val newAux= {coverage = 0, label = SOME newLabel, tycomp=zeroComps}
      in
	case ty 
        of Base (a,t)                   => (labelmap', Base(newAux, List.take(t, 1)))
        |  TBD (a,i,cl)                 => raise TyMismatch
        |  Bottom (a, i, cl)              => raise TyMismatch
        |  Pstruct (a, tys)              => 
		let
		  val (labelmap'', tys') = (initTyList labelmap' tys)
		in (labelmap'', Pstruct(newAux, tys'))
		end
        |  Punion (a, tys)               => 
		let
		  val (labelmap'', tys') = initTyList labelmap' tys
		in (labelmap'', Punion(newAux, tys'))
		end
        |  Parray (a, {tokens, lengths, first, body, last}) =>  raise TyMismatch
        |  RefinedBase (a, r, tl)         => (labelmap', RefinedBase(newAux, r, nil))
        |  Switch(a, id, branches)        => 
		let
		  val newSwitchId = some(LabelMap.find(labelmap', id))
		  val (res, tys) = ListPair.unzip branches
		  val (labelmap'', tys') = initTyList labelmap' tys
		  val branches' = ListPair.zip (res, tys')
		in
		  (labelmap'', Switch(newAux, newSwitchId, branches'))
		end
        |  RArray (a,sep,term,body,len,lengths) => 
		let
		  val (labelmap'', body') = initializeTy labelmap' body
		in (labelmap'', RArray (newAux, sep, term, body', len, nil))
		end
        |  Poption (a, body)               => 
		let
		  val (labelmap'', body') = initializeTy labelmap' body
		in (labelmap'', Poption(newAux, body'))
		end
      end 
    (*function to clean up the first token in all the base types, because the golden IR were
	rewritten with one fake token in each base type *)
    fun cleanFirstToken ty =
        case ty 
        of Base (a,t)                   => Base(a, List.drop(t, 1))
        |  TBD (a,i,cl)                 => raise TyMismatch
        |  Bottom (a, i, cl)              => raise TyMismatch
        |  Pstruct (a, tys)              => Pstruct(a, map cleanFirstToken tys)
        |  Punion (a, tys)               => Punion(a, map cleanFirstToken tys)
        |  Parray (a, {tokens, lengths, first, body, last}) =>  raise TyMismatch
        |  RefinedBase (a, r, tl)         => RefinedBase(a, r, tl)
        |  Switch(a,id,branches)        => Switch(a, id, 
					map (fn (re, ty) => (re, cleanFirstToken ty)) branches)
        |  RArray (a,sep,term,body,len,lengths) => RArray (a, sep, term, cleanFirstToken body, len, lengths)
        |  Poption (a, ty)               => Poption(a, cleanFirstToken ty)

    (* This function checks if a string s matches a regex re *)
    type mi = {pos:substring, len:int}
    fun matchRegEx (s:string) (re:string) :string =
      let
        val reStr = String.substring (re, 1, (size re)-2) (*remove the / and / *)
(*
	val _ = print ("Match string (" ^ (String.toString s) ^") with regex ("^(String.toString reStr) ^ ")\n")
*)
        fun matchCvt mtOpt =  
	  let
		val (root:mi option) = MT.root mtOpt
  	  in
		case root of 
		NONE => SS.full ""
		| SOME m => SS.slice(#pos m, 0, SOME(#len m))
	  end
	val matchlist = [(reStr, matchCvt)]
	val matchOne = RegExp.match matchlist SS.getc (Substring.full s)
      in
	case matchOne of
	NONE => ((*print "Regex failed!\n";*) "")
	| SOME(matched, rest) => ((*print "Regex succeeded!\n";*) (Substring.string matched))
      end      			
   
    fun matchString (s:string) (matchedStr:string) loc (tokens: LToken list) =
      if s = matchedStr then (SOME((Pstring matchedStr, loc)), tokens)
      else if length tokens = 0 then (NONE, tokens)
      else
	let 
	  val (tok, nextloc) = hd tokens
	  val newloc = combLoc (loc, nextloc)
(*
	  val _ = print "In matchString ...\n";
	  val _ = print ("matching (" ^ (tokenToString tok) ^ ") with (" ^ s ^")\n");
*)
	in
	  case tok of
		Ptime(t) => if (String.isPrefix (matchedStr^t) s) then 
				matchString s (matchedStr^t) newloc (List.drop (tokens, 1))
			    else (NONE, tokens)
	|	Pdate(t) => if (String.isPrefix (matchedStr^t) s) then 
				matchString s (matchedStr^t) newloc (List.drop (tokens, 1))
			    else (NONE, tokens)
	|	Pip(t)  => if (String.isPrefix (matchedStr^t) s) then 
				matchString s (matchedStr^t) newloc (List.drop (tokens, 1))
			    else (NONE, tokens)
	|	Phostname(t)  => if (String.isPrefix (matchedStr^t) s) then 
				matchString s (matchedStr^t) newloc (List.drop (tokens, 1))
			    else (NONE, tokens)
	|	Ppath(t)  => if (String.isPrefix (matchedStr^t) s) then 
				matchString s (matchedStr^t) newloc (List.drop (tokens, 1))
			    else (NONE, tokens)
	|	Purl(t)  => if (String.isPrefix (matchedStr^t) s) then 
				matchString s (matchedStr^t) newloc (List.drop (tokens, 1))
			    else (NONE, tokens)
	|	Pemail(t)  => if (String.isPrefix (matchedStr^t) s) then  
				matchString s (matchedStr^t) newloc (List.drop (tokens, 1))
			    else (NONE, tokens)
	|	Pmac(t)  => if (String.isPrefix (matchedStr^(toLower t)) s) then 
				matchString s (matchedStr^(toLower t)) newloc (List.drop (tokens, 1))
			    else (NONE, tokens)
	|	Pstring(t)  =>  if (String.isPrefix (matchedStr^t) s) then 
				matchString s (matchedStr^t) newloc (List.drop (tokens, 1))
			        else (NONE, tokens)
	| 	Pint(i, t) => if (String.isPrefix (matchedStr^t) s) then 
				matchString s (matchedStr^t) newloc (List.drop (tokens, 1))
			        else (NONE, tokens)
	|	Pfloat (i, f) => if (String.isPrefix (matchedStr^i^"."^f) s) then 
				matchString s (matchedStr^i^"."^f) newloc (List.drop (tokens, 1))
			        else (NONE, tokens)
	|	Pwhite (t)  => if (String.isPrefix (matchedStr^t) s) then 
				matchString s (matchedStr^t) newloc (List.drop (tokens, 1))
			       else (NONE, tokens)
	|	Ptext(t)  => if (String.isPrefix (matchedStr^t) s) then  
				matchString s (matchedStr^t) newloc (List.drop (tokens, 1))
			    else (NONE, tokens)
	|	Other (c)  => let val t = str(c) 
(*
				val _ = print ("Trying to match char (" ^ matchedStr ^ t ^")\n")
*)
			      in if (String.isPrefix (matchedStr^t) s) then 
				((*print "success!\n"; *)
				matchString s (matchedStr^t) newloc (List.drop (tokens, 1)))
			        else ((*print "failed!\n"; *)
					(NONE, tokens))
			      end
	| 	_ => (NONE, tokens)
	end

    (* we want to have a greedy match that returns the longest match *)
    fun matchREString (s:string) (tokens:LToken list) =
      let
        val toMatch = String.concat (map tokenToRawString (map #1 tokens))
	(*
	val _ = print ("Trying to match " ^ toMatch ^ " with regex " ^ s ^ "\n")
	*)
	val matchedStr = (matchRegEx toMatch s)
(*
	val _ = print ("Matched str is ("^matchedStr ^ ")\n")
*)
      in
 	if matchedStr = "" then (NONE, tokens)
	else 
	  let
	    val (matchedTokenOp, restTokens) = matchString matchedStr "" (#2 (hd tokens)) tokens
(*
	    val _ = print ("Matched token is " ^ 
		    (case matchedTokenOp of 
			SOME t => ltokenToString t
			| NONE => "none!\n"
		    ))
*)
	  in
	    (matchedTokenOp, restTokens)
	  end
      end
    fun matchEnum res tokens =
      case res of 
          nil => (NONE, tokens)	
	| re::tail => 
	  let
	    val (ltokenop, tokens') = matchTokens re tokens
	  in
	    case ltokenop of
	    SOME ltoken => (ltokenop, tokens')
	    | NONE => matchEnum tail tokens
	  end
    (*returns (SOME token/NONE, remaining tokens)*)
    and matchTokens (re:Refined) (tokens:LToken list) =
      if length tokens = 0 then (NONE, tokens)
      else
        let 
          val tok = (#1 (hd tokens))
(*
       	  val _ = print ("Matching "^(tokenToString tok)^ " with " ^ (refinedToString re) ^ "\n")
*)
        in
  	case (re, tok) of 
  	  (Int(min,max), Pint (i, s)) =>  
  		if (i>=min andalso i<=max) then (SOME(hd tokens), List.drop(tokens, 1))
  		else (NONE, tokens)
  	| (IntConst x, Pint (i, s)) =>  
  		if (i = x) then (SOME(hd tokens), List.drop(tokens, 1))
  		else (NONE, tokens)
  (* No FloatConst for now
  	| (FloatConst (x, y), Pfloat(x1, y1)) =>  
  		if x = x1 andalso y = y1 then (SOME(hd tokens), List.drop(tokens, 1))
  		else (NONE, tokens)
  	| (FloatConst (x, y), Int(x1, s)) =>  
  		if (LargeInt.fromString x) = (LargeInt.fromString x1) andalso
  		   (LargeInt.fromString y) = 0 then (SOME(hd tokens), List.drop(tokens, 1))
  		else (NONE, tokens)
  *)
  	| (Enum res, tok) => matchEnum res tokens
  	| (StringConst s, tok) => matchString s "" (#2 (hd tokens)) tokens
	(* need to strip off the begining and ending "/" in the regex in StringME*)
  	| (StringME s, tok) => matchREString s tokens
  	| _ => (NONE, tokens)
        end 

    fun matchBranch(ltoken, branches, index) =
	case branches of
	nil => NONE
	| (re, ty)::tail => 
		let 
		  val (matched, tokens') = matchTokens re [ltoken]
		in
		  case matched of 
			SOME _ =>  SOME (index, ty)
		   	| NONE =>  matchBranch(ltoken, tail, index+1)
		end
    fun getDefaultBranch (branches, index) =
	case branches of
	nil => NONE
	| (StringConst "*", ty)::tail => SOME (index, ty)
	| _::tail => getDefaultBranch (tail, index+1)
    fun matchSwitchBranch (ltoken, branches) =
	case matchBranch(ltoken, branches, 0) of
		SOME (index, ty) => SOME(index, ty)
		| NONE => (case getDefaultBranch (branches, 0) of
				SOME (index, ty) => SOME (index, ty)
				| NONE => NONE
			  )
	
    fun updateBranches(branches, index, newTy) =
	let
	  val (re, ty) = List.nth (branches, index)
	  val head = List.take (branches, index)
	  val tail = List.drop (branches, index+1)
	in (head@[(re, newTy)]@tail)
	end

    (* returns (success', env', tokenlist', body', newlen) *)
    fun consumeArray (env, sep, term, body, fixedLen, len, ltokens) = 
      let 
	val (success, env', ltokens', body') = consume (true, env, ltokens, body)
(*
	val _ = print ("Consume array body: " ^ (Bool.toString success) ^ "\n")
	val _ = print ("remaining tokens : " ^ (LTokensToString ltokens'))
	val _ = print ("fixedLen = " ^ Int.toString(fixedLen) ^ " len = " ^ Int.toString(len) ^ "\n")
*)
      in
	if success then 
	  if len+1 = fixedLen then
	    case term of 
	      SOME termre => 
		let val (matched, remaining) = matchTokens termre ltokens'
		in
		  case matched of 
		  SOME _ => (success, env', ltokens', body', fixedLen)
		  | NONE => (false, env, ltokens, body, fixedLen)
		end
	     | NONE => (success, env', ltokens', body', fixedLen)
	  else (* haven't reached fixed len or no fixed len *)
	    case term of 
	      SOME termre =>
		let val (matched, remaining) = matchTokens termre ltokens'
		in
		  case matched of 
		  SOME _ => (success, env', ltokens', body', len+1)
		  | NONE => (*haven't reached the term yet, check sep and keep going *)
		    (case sep of 
			SOME sepre =>
			  let val (matched, remaining) = matchTokens sepre ltokens'
			  in
			    case matched of 
			    SOME _ => consumeArray(env', sep, term, body', fixedLen, len+1, remaining)
			    | NONE => (false, env, ltokens, body, len)
			  end
			| NONE => consumeArray(env', sep, term, body', fixedLen, len+1, ltokens')
		    )
		end
	     | NONE => (* no term, check sep, and keep going *)	
		if length ltokens' = 0 (*finished with the record *)
		then (true, env', ltokens', body', len+1)
		else
		    (case sep of 
			SOME sepre =>
			  let val (matched, remaining) = matchTokens sepre ltokens'
			  in
			    case matched of 
			    SOME _ => consumeArray(env', sep, term, body', fixedLen, len+1, remaining)
			    | NONE => (false, env, ltokens, body, len)
			  end
			| NONE => consumeArray(env', sep, term, body', fixedLen, len+1, ltokens')
		    )
	  else (*consume body failed*)
	    case term of
	      SOME _ => (false, env, ltokens, body, len)
	      | NONE => if len>0 then (true, env, ltokens, body, len)
			else (false, env, ltokens, body, len)
	end

    (* returns (success, env', tokenlist', ty'), env is a LabelMap of (id, ltoken) *)
    and consume (prevsuccess, env, tokenlist, ty) = 
      if not prevsuccess then 
	(false, env, tokenlist, ty)
      else if (length tokenlist) = 0 then
	case ty of 
	    (*TODO: the location of the new Pempty is not right here but may not matter *)
	    Base(a, tl as ((Pempty, loc)::l)) => 
		(true, env, tokenlist, Base(incCoverage a, tl@[(Pempty, loc)]))  
	  | Poption (a, ty') =>
		(true, env, tokenlist, Poption(incCoverage a, ty'))
	  | _ => (false, env, tokenlist, ty)
      else
      ((*print "Trying to consume ...\n"; printTy ty; *)
      case ty of
           Base (a, t) => (
(*
			  print ("Matching token " ^ (ltokenToString (hd tokenlist)) ^ " with " ^
					(ltokenTyToString (hd t)) ^ "\n");
*)
			  if compToken((#1 (hd tokenlist)), (#1 (hd t))) = EQUAL then 
			   let
				val tok = hd tokenlist
				val label = getLabel a
				val env' = LabelMap.insert(env, label, tok) 
				val newaux = incCoverage a
(*
				val _ = print "Tokens equal!\n"
				val _ = print ((ltokenToString (hd tokenlist)) ^ "\n")
*)
			   in (true, env', List.drop(tokenlist, 1), Base(newaux, (t@[tok])))
			   end
			  else (*Pempty is special case, matches anything*)
			    (case (hd t) of
				(Pempty, _) =>
				  let val loc = (#2 (hd tokenlist))
				  in (true, env, tokenlist, Base(incCoverage a, t@[(Pempty, loc)]))
				  end
				(* Pint can be negative so need to check for "-" *)
				| (Pint(_, _), _) =>
					if (length tokenlist<2) then (false, env, tokenlist, ty)
					else 
					  let val (minus, loc1) = (hd tokenlist)
					      val (num, loc2) = List.nth(tokenlist, 1)
					  in
					      if (compToken(minus, Other #"-") = EQUAL andalso
						  compToken(num, Pint (0, "0")) = EQUAL)
					      then 
						case num of 
						  Pint (i, s) => 
						    let
						      val label = getLabel a
						      val intTok = (Pint (~i, "-" ^ s), loc1)
						      val env' = LabelMap.insert(env, label, intTok)
						      val newaux = incCoverage a
						    in (true, env', List.drop(tokenlist, 2),
							Base(newaux, t@[intTok]))
						    end
						  | _ => raise TyMismatch
					      else (false, env, tokenlist, ty)
					  end
				(*Pfloat is also a special case, either int.int or int 
				  and also, need to check for negative number *)
				| (Pfloat(_, _), _) =>
				    (case (#1 (hd tokenlist)) of
					Pint (i, s) =>
						(*check if the second token and third are dot int*)
					  if (length tokenlist)>=3 then
						let
						  val [int1, dot, int2] = List.take (tokenlist, 3)
						in 
						  (case (dot, int2) of
						    ((Other #".", loc_dot), (Pint(i2, s2), loc_int)) =>
					   	    let
						      val label = getLabel a
						      val floattok = (Pfloat (s, s2), (#2 int1))
						      val env' = LabelMap.insert(env, label, floattok) 
						      val newaux = incCoverage a
					   	    in (true, env', List.drop(tokenlist, 3), 
							Base(newaux, t@[floattok]))
					   	    end
						  | _ => 
					   	    let
						      val label = getLabel a
						      val floattok = (Pfloat (s, "0"), (#2 int1))
						      val env' = LabelMap.insert(env, label, floattok) 
						      val newaux = incCoverage a
					   	    in (true, env', List.drop(tokenlist, 1), 
							Base(newaux, t@[floattok]))
					   	    end
						  )
						end
					  else 
					   	let
						  val tok = (hd tokenlist)
						  val label = getLabel a
						  val floattok = (Pfloat (s, "0"), (#2 tok))
						  val env' = LabelMap.insert(env, label, floattok) 
						  val newaux = incCoverage a
					   	in (true, env', List.drop(tokenlist, 1), Base(newaux, t@[floattok]))
					   	end
					| Other #"-" => (* a minus sign *)
					    if length tokenlist = 1 then (false, env, tokenlist, ty)
					    else 
						let
						  val tokenlist = List.drop (tokenlist, 1)
					        in	
					          (case (#1 (hd tokenlist)) of
						  	Pint (i, s) =>
							(*check if the second token and third are dot int*)
					  		if (length tokenlist)>=3 then
							  let
						  		val [int1, dot, int2] = List.take (tokenlist, 3)
							  in 
						  		(case (dot, int2) of
						    		  ((Other #".", loc_dot), (Pint(i2, s2), loc_int)) =>
					   	    		    let
						      			val label = getLabel a
						      			val floattok = (Pfloat (("-" ^ s), s2), (#2 int1))
						      			val env' = LabelMap.insert(env, label, floattok) 
						      			val newaux = incCoverage a
					   	    		    in (true, env', List.drop(tokenlist, 3), 
								        Base(newaux, t@[floattok]))
					   	    	  	    end
						  		  | _ => 
					   	    		    let
						      			val label = getLabel a
						      			val floattok = (Pfloat (s, "0"), (#2 int1))
						      			val env' = LabelMap.insert(env, label, floattok) 
						      			val newaux = incCoverage a
					   	    		    in (true, env', List.drop(tokenlist, 1), 
									Base(newaux, t@[floattok]))
					   	    		    end
						  		)
							  end
					  		else 
					   		  let
						  		val tok = (hd tokenlist)
								val label = getLabel a
								val floattok = (Pfloat (s, "0"), (#2 tok))
								val env' = LabelMap.insert(env, label, floattok) 
								val newaux = incCoverage a
							  in (true, env', List.drop(tokenlist, 1), Base(newaux, t@[floattok]))
					   		  end
						   	| _ =>  (false, env, tokenlist, ty)
				    		)
					      end
				       | _ => (false, env, tokenlist, ty)
			      )
			    | _ => (false, env, tokenlist, ty)
			    )
			)
	|  TBD (a,i,cl)     => raise TyMismatch
        |  Bottom (a, i, cl)  => raise TyMismatch
        |  Pstruct (a, tys)  => 
		let
		  fun consume_and (ty, (success, env, tokens, tylist)) =
		  let
			val (success', env', tokens', ty') = consume (success, env, tokens, ty)
		  in  (success', env', tokens', (tylist@[ty']))
		  end
		  val (success', env', tokens', tys') = foldl consume_and (true, env, tokenlist, nil) tys 
		in
			(success', env', tokens', Pstruct(incCoverage a, tys'))
		end
        |  Punion (a, tys) 	        => 
		let
			fun consume_or (success, env, tokens, nil, tys) = (false, env, tokens, tys)
			| consume_or (success, env, tokens, ty::rest, previousTys)  =
			  let
			    val (success', env', tokens', ty') = consume(success, env, tokens, ty)
			  in
			    if (success') then (success', env', tokens', previousTys@[ty']@rest)
			    else consume_or (success, env, tokens, rest, previousTys@[ty])
			  end
			val (success', env', tokens', tys') = consume_or(true, env, tokenlist, tys, nil) 
		in
			if success' then (success', env', tokens', Punion((incCoverage a), tys'))
			else (success', env, tokenlist, ty)
		end
        |  Parray (a, {tokens, lengths, first, body, last}) =>  raise TyMismatch
        |  RefinedBase (a, r, tl)     => 
	   let
	     val (matchedToken, tokenlist') = matchTokens r tokenlist
	   in
	     case matchedToken of
		SOME ltoken => 
		  let
	     		val label = getLabel a
			(*val _ = print ("inserting " ^ Atom.toString(label) ^ " with value " ^
				(ltokenToString ltoken) ^ "\n") *)
	     		val env' = LabelMap.insert(env, label, ltoken) 
	     		val newaux = incCoverage a
	   	  in
		    (true, env', tokenlist', RefinedBase (incCoverage a, r, (tl@[ltoken])))
	      	  end
		| NONE => (
			  (*print "matchTokens failed!\n"; *)
			  (false, env, tokenlist, ty))
	   end
        |  Switch(a, id, branches)     => 
	     (
	     case LabelMap.find (env, id) of
		SOME ltoken=> 
		(
		  case matchSwitchBranch(ltoken, branches) of 
		  (* index is the index in the branches list*)
		  SOME (index, matchedTy) =>
		    let
			val (success', env', tokenlist', matchedTy') = 
				consume (prevsuccess, env, tokenlist, matchedTy)
		    in
			if success' then (success', env', tokenlist', 
				Switch(incCoverage a, id, updateBranches(branches, index, matchedTy')))
			else (false, env, tokenlist, ty)
		    end
		  | NONE =>  raise TyMismatch
		)
		| NONE => raise TyMismatch
	     )
	(*TODO: RArray is a bit special in that it can be matched or not matched but consume or go on *)
        |  RArray (a, sep, term, body, len, lengths) => 
	     let
(*
                val _ =  (print "Trying to consume array ...\n"; printTy ty) 
		val _ = printLTokens tokenlist
*)
		val fixedLen = case len of 
				SOME (IntConst l) => (Int.fromLarge l) 
				| _ => (~1)
		val (success', env', tokenlist', body', newlen) = consumeArray (env, sep, term, body, 
										fixedLen, 0, tokenlist)
		val lengths' = lengths@[(newlen, (!recordNo))]
(*
		val _ = print ("Array consume success: " ^ (Bool.toString success') ^ "\n")
*)
	     in 
		if success' then (true, env', tokenlist', 
				RArray(incCoverage a, sep, term, body', len, lengths'))
		else (false , env, tokenlist, ty)
	     end
        |  Poption (a, ty)               => 
		let
			val (success', env', tokenlist', ty') = consume (prevsuccess, env, tokenlist, ty)
		in
			if success' then (success', env', tokenlist', Poption(incCoverage a, ty'))
			else (prevsuccess, env, tokenlist, Poption (incCoverage a, ty))
		end
	)
    fun populateOneRecord (ltokens:Context, ty:Ty) : Ty = 
      let
(*
	  val toMatch = String.concat (map tokenToRawString (map #1 ltokens))
	  val _ = print ("Record: " ^ toMatch ^ "\n")
*)
	val (success, env, ltokens', ty' ) = consume (true, LabelMap.empty, ltokens, ty)
      in
	if (success = false andalso length ltokens' > 0) then
		(print ("Failed at token: " ^ tokenToString (#1 (hd ltokens')) ^ "\n");
		print ("Record #" ^ (Int.toString (#lineNo (#2 (hd ltokens)))) ^ 
			" is not successfully populated!!!\n"); ty)	
	else if (success = false) then (
		print ("Record #" ^ (Int.toString (#lineNo (#2 (hd ltokens)))) ^ 
			" is not successfully populated!!!\n"); ty)	
	else ((*print "Recorded successfully populated!!!\n"; *)recordNo:=(!recordNo)+1; ty')
      end

    (* crack all group tokens in a given context list *)
    fun crackAllGroups (cl:Context list) : Context list =
	let fun clg pre [] = pre
	      | clg pre (hd::tail) = 
		(case hd of 
		  (Pgroup(g as {left, body, right}), loc) => clg (pre@(clg nil (groupToTokens g))) tail
		  | _ => clg (pre@[hd]) tail
		)
	in 
	  map (clg nil) cl
	end

    fun populateDataFile datafile ty =
      let val recordNumber = ref 0
          val _ = Tystamp := 0
          val records = loadFiles [datafile]
	  val rtokens : Context list = map (ltokenizeRecord recordNumber) records
	  val rtokens = crackAllGroups rtokens
	  val (newmap, cleanTy) = initializeTy LabelMap.empty ty
(*
	  val _ = print ("Populating data file: " ^ datafile ^ " into:\n")
	  val _ = printTy cleanTy
*)
	  val loadedTy = foldl populateOneRecord cleanTy rtokens
	  val finalTy = (measure (cleanFirstToken loadedTy))
(*
	  val _ = printTy finalTy
*)
      in finalTy
      end 
end
