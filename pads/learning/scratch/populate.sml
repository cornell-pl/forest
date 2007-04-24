(*this program contains functions that populate a hand-written Ty structure
with data from a data file*)
structure Populate =
struct
    open Structure
    open Types
    open Common
    exception TyMismatch
    structure RegExp = RegExpFn (structure P=AwkSyntax structure E=DfaEngine) : REGEXP
    structure MT = MatchTree
    structure SS = Substring
    
    val recordNo = ref 0
   (*function to assign labels to the nodes and clear the coverage
    and tokenlists in the tree *)
    fun initializeTy ty =
      let
	val newAux= {coverage = 0, label = SOME (getLabel({coverage=0, label = NONE, tycomp=zeroComps})),
	tycomp=zeroComps}
      in
	case ty 
        of Base (a,t)                   => Base(newAux, t)
        |  TBD (a,i,cl)                 => raise TyMismatch
        |  Bottom (a, i, cl)              => raise TyMismatch
        |  Pstruct (a, tys)              => Pstruct(newAux, map initializeTy tys)
        |  Punion (a, tys)               => Punion(newAux, map initializeTy tys)
        |  Parray (a, {tokens, lengths, first, body, last}) =>  raise TyMismatch
        |  RefinedBase (a, r, tl)         => RefinedBase(newAux, r, nil)
        |  Switch(a,id,branches)        => Switch(newAux, id, 
					map (fn (re, ty) => (re, initializeTy ty)) branches)
        |  RArray (a,sep,term,body,len,lengths) => RArray (newAux, sep, term, initializeTy body, len, nil)
        |  Poption (a, ty)               => Poption(a, initializeTy ty)
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
    fun matchRegEx (s:string) (re:string) :bool =
      let
        val reStr = String.substring (re, 1, (size re)-2) (*remove the / and / *)
        fun matchCvt mtOpt =  
	  let
		val (root:mi option) = MT.root mtOpt
  	  in
		case root of 
		NONE => false
		| SOME m => if (#len m) = (Substring.size (#pos m)) then true else false
	  end
	val matchlist = [(reStr, matchCvt)]
	val matchOne = RegExp.match matchlist SS.getc (Substring.full s)
      in
	case matchOne of
	NONE => false
	| SOME(matched, _) => matched
      end      			
   
    fun matchString (s:string) (matchedStr:string) loc (tokens: LToken list) =
      if s = matchedStr then (SOME((Pstring matchedStr, loc)), tokens)
      else if length tokens = 0 then (NONE, tokens)
      else
	let 
	  val (tok, nextloc) = hd tokens
	  val newloc = combLoc (loc, nextloc)
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
	|	Pwhite (t)  => if (String.isPrefix (matchedStr^t) s) then 
				matchString s (matchedStr^t) newloc (List.drop (tokens, 1))
			       else (NONE, tokens)
	|	Other (c)  => let val t = Char.toString(c) 
			      in if (String.isPrefix (matchedStr^t) s) then 
				matchString s (matchedStr^t) newloc (List.drop (tokens, 1))
			        else (NONE, tokens)
			      end
	| 	_ => (NONE, tokens)
	end

    fun matchREString (s:string) (matchedStr:string) loc (tokens:LToken list) =
      if matchedStr <> "" andalso (matchRegEx matchedStr s) then 
	(SOME((Pstring matchedStr, loc)), tokens)
      else if length tokens = 0 then (NONE, tokens)
      else
	let 
	  val (tok, nextloc) = hd tokens
	  val newloc = combLoc (loc, nextloc)
	in
	case (#1 (hd tokens)) of
		Ptime(t) => matchREString s (matchedStr^t) newloc (List.drop (tokens, 1))
	|	Pdate(t) => matchREString s (matchedStr^t) newloc (List.drop (tokens, 1))
	|	Pip(t)  => matchREString s (matchedStr^t) newloc (List.drop (tokens, 1))
	|	Phostname(t) => matchREString s (matchedStr^t) newloc (List.drop (tokens, 1))
	|	Ppath(t)  => matchREString s (matchedStr^t) newloc (List.drop (tokens, 1))
	|	Purl(t)  => matchREString s (matchedStr^t) newloc (List.drop (tokens, 1))
	|	Pemail(t)  => matchREString s (matchedStr^t) newloc (List.drop (tokens, 1))
	|	Pmac(t)  => matchREString s (matchedStr^(toLower t)) newloc (List.drop (tokens, 1))
	|	Pstring(t)  => matchREString s (matchedStr^t) newloc (List.drop (tokens, 1))
	|	Pwhite (t)  => matchREString s (matchedStr^t) newloc (List.drop (tokens, 1))
	|	Other (c)  => let val t = Char.toString(c) 
			      in matchREString s (matchedStr^t) newloc (List.drop (tokens, 1))
			      end
	| 	_ => (NONE, tokens)
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
      let 
        val tok = (#1 (hd tokens))
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
	| (StringME s, tok) => matchREString s "" (#2 (hd tokens)) tokens
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
      in
	if success then 
	  if len+1 = fixedLen then
	    case term of 
	      SOME termre => 
		let val (matched, remaining) = matchTokens termre ltokens'
		in
		  case matched of 
		  SOME _ => (success, env', remaining, body', fixedLen)
		  | NONE => (false, env, ltokens, body, fixedLen)
		end
	     | NONE => (success, env', ltokens', body', fixedLen)
	  else (* haven't reached fixed len or no fixed len *)
	    case term of 
	      SOME termre =>
		let val (matched, remaining) = matchTokens termre ltokens'
		in
		  case matched of 
		  SOME _ => (success, env', remaining, body', len+1)
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
	      | NONE => (true, env, ltokens, body, len)
	end

    (* returns (success, env', tokenlist', ty'), env is a LabelMap of (id, ltoken) *)
    and consume (prevsuccess, env, tokenlist, ty) = 
      if not prevsuccess orelse (length tokenlist) = 0 then 
	(false, env, tokenlist, ty)
      else
      case ty of
           Base (a, t) => if compToken((#1 (hd tokenlist)), (#1 (hd t))) = EQUAL then 
			   let
				val tok = (#1 (hd tokenlist))
				val label = getLabel a
				val env' = LabelMap.insert(env, label, hd tokenlist) 
				val newaux = incCoverage a
			   in (true, env', List.drop(tokenlist, 1), Base(newaux, t@[(hd t)]))
			   end
			  else (*Pempty is special case, matches anything*)
			    (case (hd t) of
				(Pempty, _) =>
				  let val loc = (#2 (hd tokenlist))
				  in (true, env, tokenlist, Base(incCoverage a, t@[(Pempty, loc)]))
				  end
				| _ => (false, env, tokenlist, ty)
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
		  val (success', env', tokens', tys') = foldr consume_and (true, env, tokenlist, nil) tys 
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
	     		val env' = LabelMap.insert(env, label, ltoken) 
	     		val newaux = incCoverage a
	   	  in
		    (true, env', tokenlist', RefinedBase (incCoverage a, r, (tl@[ltoken])))
	      	  end
		| NONE => (false, env, tokenlist, ty)
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
        |  RArray (a, sep, term, body, len, lengths) => 
	     let
		val fixedLen = case len of 
				SOME (IntConst l) => (Int.fromLarge l) 
				| _ => (~1)
		val (success', env', tokenlist', body', newlen) = consumeArray (env, sep, term, body, 
										fixedLen, 0, tokenlist)
		val lengths' = lengths@[(newlen, (!recordNo))]
	     in 
		if success' then (true, env', tokenlist', 
				RArray(incCoverage a, sep, term, body', len, lengths'))
		else (false, env, tokenlist, ty)
	     end
        |  Poption (a, ty)               => 
		let
			val (success', env', tokenlist', ty') = consume (prevsuccess, env, tokenlist, ty)
		in
			if success' then (success', env', tokenlist', Poption(incCoverage a, ty'))
			else (prevsuccess, env, tokenlist, Poption (incCoverage a, ty))
		end

    fun populateOneRecord (ltokens:Context, ty:Ty) : Ty = 
      let
	val (success, env, ltokens', ty' ) = consume (true, LabelMap.empty, ltokens, ty)
      in
	if (success = false orelse length ltokens' > 0) then
		(print "Record not successfully populated\n"; ty)	
	else (recordNo:=(!recordNo)+1; ty')
      end

    fun populateDataFile datafile ty =
      let 
          val recordNumber = ref 0
          val records = loadFiles [datafile]
	  val rtokens : Context list = map (ltokenizeRecord recordNumber) records
	  val cleanTy = initializeTy ty
	  val loadedTy = foldr populateOneRecord cleanTy rtokens
	  val finalTy = cleanFirstToken loadedTy
      in finalTy
      end 
end
