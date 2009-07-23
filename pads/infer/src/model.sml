structure Model = struct
    open Complexity
    open Types
    open Hosts

    (* Get the maximum from a list of integers *)
    fun maxInt ( l : int list ) : int = foldl Int.max 0 l

    (* Make a base complexity from a multiplier (often maximum length of token)
       and a number of choices. *)
    fun mkBaseComp numTokens ( avg : real ) ( tot : LargeInt.int ) ( choices : LargeInt.int ) : TyComp =
        { tc  = constructorComp
        , adc = multCompR (avg + 1.0) (int2Comp choices ) 
        , dc  = combine (multComp numTokens (int2Comp choices)) 
		(multComp tot ( int2Comp choices ))
        }

    fun mkBaseCompR numTokens ( avg : real ) ( tot : real ) ( choices : LargeInt.int ) : TyComp =
        { tc  = constructorComp
        , adc = multCompR (avg + 1.0) ( int2Comp choices )
        , dc  = combine (multComp numTokens (int2Comp choices)) 
		(multCompR tot ( int2Comp choices ))
        }

    (* TODO: this function is a hack to handle only a few special cases *)
    fun parseRegEx (s:string) : LargeInt.int*int  =
	case s of 
	  "/[^ ]+/" => (numStringChars-1, 0)
	| "/\\/[^ ]*/" => (numStringChars-1, 1)
	| "/[^\\]]+/" => (numStringChars-1, 0)
	| "/[^\\\\]+/" => (numStringChars-1, 0)
	| "/[^:]+/" => (numStringChars-1, 0)
	| "/no_ii[0-9]*/" => (10, 5)
	| "/[0-9a-f]+/" => (16, 0)
	| "/[0-9a-fx]+/" => (17, 0)
	| "/[0-9a-zA-Z]+/" => (62, 0)
	| "/[0-9a-zA-Z ]+/" => (63, 0)
	| "/[0-9a-zA-Z. ]+/" => (64, 0)
	| "/[0-9a-zA-Z.\\-_]+/" => (numStringChars+1, 0)
	| "/[0-9a-zA-Z_\\-]+/" => (numStringChars, 0)
	| "/(\\-|d)[\\-rwx]+/" => (4, 1)
	| "/[0-9][0-9]:[0-9][0-9]/" => (10, 0)
	| "/[0-9a-zA-Z\\-_ ~.]+/" => (numStringChars+3, 0)
	| "/[a-zA-Z ]+/" => (53, 0)
	| "/\\\"[^\"]+\\\"/" => (numStringChars-1, 2)
	| "/[^,]+/" => (numStringChars-1, 0)
	| "/[^.]+/" => (numStringChars-1, 0)
	| "/[^\"]+/" => (numStringChars-1, 0)
	| "/[A-Z][A-Z]/" => (26, 0)
	| "/[0-9]+/" => (10, 0)
	| "/-?[0-9]+/" => (10, 0) (* underestimate the size here *)
	| _ => (256, 0)

    (* Compute the type and data complexity of a refined type *)
    fun refinedComp ( avg : real )         (* Average length of tokens *)
                    ( tot : LargeInt.int ) (* Sum of length of tokens *)
                    ( num : LargeInt.int ) (* Number of tokens *)
                    ( r : Refined )        (* refined type *)
                     : TyComp =            (* Complexity numbers *)
        ( case r of
               StringME s     => 
			let
		     	  (* val _ = print ("Parsing " ^ s ^".\n") *)
			  val (choices, numConstChars) = parseRegEx s
			  (* val _ = print ("Choices = " ^ (LargeInt.toString choices) ^ "\n") *)
			  (*
			  fun f (len, c) = combine c 
					    (multCompS (len-numConstChars) 
						(int2Comp choices))
			  val totaldc = foldl f zeroComp lens
			  *)
			  val totaldc = multComp (tot - num * (Int.toLarge numConstChars)) 
					(int2Comp choices)
			  val avgdc = divComp num totaldc
			in
				{ tc = combine constructorComp
				( multCompS ((size s) - 2) (*exclude / / *)
				    (int2Comp numStringChars))
				 , adc = avgdc
				 , dc = totaldc
				} 
			end
             | Int (min, max) => { tc  = sumComps [ constructorComp
                                                  , int2Comp min
                                                  , int2Comp max
                                                  ]
                                 , adc = int2Comp ( max - min + 1 )
                                 , dc  = multComp num ( int2Comp ( max - min + 1 ) )
                                 }
             | IntConst n     => { tc  = sumComps [ constructorComp
                                                  , int2Comp 2
                                                  , int2Comp n
                                                  ]
                                 , adc = zeroComp
                                 , dc  = zeroComp
                                 }
             | FloatConst (m,n) => { tc  = sumComps [ constructorComp
                                                    , int2Comp 2
                                                    , (multCompS (size m) (int2Comp numDigits)) 
                                                    , (multCompS (size n) (int2Comp numDigits)) 
                                                    ]
                                 , adc = zeroComp
                                 , dc  = zeroComp
                                 }
             | StringConst s  => { tc  = combine constructorComp
                                                 ( multCompS (size s)
                                                   (int2Comp numStringChars)
                                                )
                                 , adc = zeroComp
                                 , dc  = zeroComp
                                 }
             | Enum rl        => { tc  = sumComps [ sumComps ( map (refinedTypeComp avg tot num) rl )
                                                  , constructorComp
                                                  , int2CompS ( length rl )
                                                  ]
                                 , adc = int2CompS ( length rl )
                                 , dc  = multCompS ( length rl )
                                                   ( int2CompS ( length rl ) )
                                 }
             | LabelRef i     => { tc =  unitComp, adc = unitComp, dc = unitComp }
	     | Blob (str, patt) =>
		let val strlen = 
		  (case (str, patt) of
		    (SOME s, _) => size s
		   | (_, SOME s) => size s - 2 (* exclusing / and / *)
		   | _ => 0 
		  )
		in
		{ tc = combine constructorComp
			(multCompS strlen (int2Comp numStringChars)),
		  adc = multCompR avg (Bits 8), (* all ascii characters *)
		  dc = multComp tot (Bits 8)
		} end
        )

    (* Get the type complexity of a refined type, assuming multiplier of 1 *)
    and refinedTypeComp ( avg : real )         (* Average length of tokens *)
                        ( tot : LargeInt.int ) (* Sum of length of tokens *)
                        ( num : LargeInt.int ) (* Number of tokens *)
                        ( r : Refined )        (* refined type *)
                         : Complexity =        (* Type complexity *)
           #tc (refinedComp avg tot num r)
    (* Get the type complexity of a refined type, assuming multiplier of 1 *)
    and refinedDataComp ( avg : real )         (* Average length of tokens *)
                        ( tot : LargeInt.int ) (* Sum of length of tokens *)
                        ( num : LargeInt.int ) (* Number of tokens *)
                        ( r : Refined )        (* refined type *)
                         : Complexity =        (* Data complexity *)
          #dc (refinedComp avg tot num r)

    (* Measure a refined base type *)
    exception NotRefinedBase (* Should be called only with refined base type *)
    fun measureRefined ( ty : Ty ) : Ty =
    ( case ty of
           RefinedBase ( a, r, ts ) =>
	     let 
		 val num =  #coverage a
		 val avg = if (#len a > 0.0) then #len a 
			   else avgTokenLength ts
		 val tot = if (#len a > 0.0) then 
				Real.toLargeInt IEEEReal.TO_NEAREST (avg * Real.fromInt num)
			   else sumTokenLength ts
             	 val comps = refinedComp avg tot (Int.toLarge num) r
		 val newty = RefinedBase (updateLenComps a avg comps, r, ts )
             in newty
             end
         | _ => raise NotRefinedBase
    )

    (* Complexity of refined option type, assuming multiplier 1 *)
    fun refinedOptionComp ( ro : Refined option ) : TyComp =
    ( case ro of
	   (*refined type doesn't exist, assume bigger value *)
           NONE   => zeroComps
         | SOME r => refinedComp 1.0 1 1 r (* Probably wrong ***** *)
    )

    (* Compute the complexity of a base type *)
    fun baseComp (t_opt : Token option, avglen, totlen, numTokens) : TyComp =
    ( case t_opt of
           NONE      => { tc = zeroComp, adc = zeroComp, dc = zeroComp }
         | SOME t =>
             let val mult : real           = Real.fromLargeInt numTokens * avglen
             in ( case t of
                    PbXML (s1, s2)    => mkBaseComp numTokens avglen totlen (numXMLChars+1)
                  | PeXML (s1, s2)    => mkBaseComp numTokens avglen totlen (numXMLChars+1)
                  | Ptime s           => mkBaseComp 0 1.0 numTokens numTime
                  | Pdate s           => mkBaseComp 0 1.0 numTokens numDate
                  | Ppath s           => mkBaseComp numTokens avglen totlen (numStringChars+1)
                  | Purl s            => mkBaseComp numTokens avglen totlen (numStringChars+1)
                  | Pemail s  	      => mkBaseComp numTokens avglen totlen (numStringChars+1)
                  | Pmac s            => mkBaseComp 0 avglen totlen numHexChars
                  | Pip s             => mkBaseComp 0 avglen totlen numIPTriplet
                  | Phostname s       => mkBaseComp numTokens avglen totlen (numStringChars+1)
		   (* assume max 64 bit integer, encode number of digits for each int to be
			transmitted, max 20 digits. *)
                  | Pint (l, s)            => mkBaseComp numTokens avglen totlen (numDigits+1)
                  | Pfloat (i,f)      => mkBaseComp numTokens avglen totlen (numDigits+2)
                  | Pstring s         => mkBaseComp numTokens avglen totlen (numStringChars+1)
                  | Pgroup x          => { tc  = constructorComp
                                         , adc = unitComp
                                         , dc  = unitComp
                                         }
                  | Pwhite s          => mkBaseComp numTokens avglen totlen (numWhiteChars+1)
                  | Ptext s           => mkBaseComp numTokens avglen totlen (numStringChars+1)
                  | Other c           => mkBaseComp 0 avglen totlen numOtherChars
                  | Pempty            => { tc  = constructorComp
                                         , adc = zeroComp
                                         , dc  = zeroComp
                                         }
                  | Error             => { tc  = constructorComp
                                         , adc = unitComp
                                         , dc  = unitComp
                                         }
                )
             end
    )

    fun mkBaseComplexity ( a : AuxInfo ) ( ts : LToken list ) : Ty =
    let val ty      = Base (a, ts)
        val numTokens = #coverage a
        val avglen : real         = if #len a > 0.0 then #len a else avgTokenLength ts
        val totlen : LargeInt.int = if #len a > 0.0 then 
					Real.toLargeInt IEEEReal.TO_NEAREST (avglen * Real.fromInt numTokens)
				    else sumTokenLength ts
        val comps   = case ts of
		t::_ => baseComp ((SOME (tokenOf t)), avglen, totlen, Int.toLarge numTokens)
		| nil => baseComp (NONE, avglen, totlen, Int.toLarge numTokens)
        fun updateCompBase ( ty : Ty ) ( comps : TyComp ) : Ty =
            Base ( updateLenComps a avglen comps, ts )
    in updateCompBase ty comps
    end

    fun maxContextComplexity ( cl : Context list ) : TyComp =
    let fun f ( ltl : LToken list, comps : TyComp ) : TyComp =
      let 
	val avglen : real         = avgTokenLength ltl
        val totlen : LargeInt.int = sumTokenLength ltl
        val numTokens : LargeInt.int = Int.toLarge (length ltl)
      in 
	case ltl of
	nil => combTyComp (baseComp (NONE, avglen, totlen, numTokens)) comps
	| t::_ => combTyComp (baseComp (SOME(tokenOf t), avglen, totlen, numTokens)) comps
      end
    in foldl f zeroComps cl
    end
    
    fun updateBaseAux (aux: AuxInfo) cov totlen token =
      let 
	  val totlen = (#len aux) * (Real.fromInt (#coverage aux)) + (Real.fromLargeInt totlen)
	  val newcov = (#coverage aux) + cov
	  val len = totlen / (Real.fromInt newcov) 
	  val comps = baseComp (SOME token, len, 
			Real.toLargeInt IEEEReal.TO_NEAREST totlen, Int.toLarge newcov)
      in
	  {coverage = newcov, label = #label aux, tycomp = comps, len = len}
      end

    fun updateRefinedBaseAux (aux:AuxInfo) cov totlen refined =
      let 
	  val totlen = (#len aux) * (Real.fromInt (#coverage aux)) + (Real.fromLargeInt totlen)
	  val newcov = (#coverage aux) + cov
	  val len = totlen / (Real.fromInt newcov) 
	  val comps = refinedComp len (Real.toLargeInt IEEEReal.TO_NEAREST totlen) 
			(Int.toLarge newcov) refined
      in
	  {coverage = newcov, label = #label aux, tycomp = comps, len = len}
      end

    (* Compute the type and data complexity of an inferred type *)    
    (* mode = 0 measures everything, mode = 1 measures only one level down *)
    (* we don't update the avglen for non-base types, because len don't make sense in those cases *)
    fun measure (mode: int)  (ty : Ty) : Ty =
    ( case ty of
           Base (a, ts )               => mkBaseComplexity a ts
         | TBD ( a, i, cl )             =>
             let val comps = maxContextComplexity cl
		 val lens = map sumTokenLength cl
		 val avg = (Real.fromLargeInt (sumLargeInts lens)) / (Real.fromInt (length cl))
             in TBD ( updateLenComps a avg comps, i, cl )
             end
         | Bottom ( a, i, cl )          =>
             let val comps = maxContextComplexity cl
		 val lens = map sumTokenLength cl
		 val avg = (Real.fromLargeInt (sumLargeInts lens)) / (Real.fromInt (length cl))
             in Bottom ( updateLenComps a avg comps, i, cl )
             end
         | RefinedBase ( a, r, ts ) =>
	(*
             let val avg = avgTokenLength ts
                 val tot = sumTokenLength ts
                 val num = LargeInt.fromInt ( length ts )
             in 
	*)
		measureRefined ty 
         (*    end  *)
         | Pstruct (a, tys)              =>
             let val measuredtys = if mode = 0 then map (measure 0) tys else tys
                 val comps = { tc  = sumComps [ constructorComp 
                                              , cardComp tys
                                              , sumTypeComps measuredtys
                                              ]
                             , adc = sumAtomicComps measuredtys
                             , dc  = sumDataComps measuredtys
                             }
		(*
		val lens = map (fn t => #len (getAuxInfo t)) measuredtys
		val avg = sumReals lens
		*)
             in Pstruct ( updateLenComps a 0.0 comps, measuredtys )
             end
         | Punion (a, tys)               =>
             let val measuredtys = if mode = 0 then map (measure 0) tys else tys
                 val comps = { tc  = sumComps [ constructorComp
                                              , cardComp tys
                                              , sumTypeComps measuredtys
                                              ]
                             , adc = (* combine ( cardComp tys ) *)
                                             ( weightedAtomic
                                                  ( sumCoverage measuredtys )
                                                  measuredtys
                                             )
                             , dc  = combine ( cardComp tys )
                                             ( sumDataComps measuredtys )
                             }
		(*
		val cov_len_pairs = map (fn ty => 
					  let val a = getAuxInfo ty 
					  in (#coverage a, #len a)
					  end) measuredtys
		val len = weightedSum cov_len_pairs
		*)
             in Punion ( updateLenComps a 0.0 comps, measuredtys )
             end
	 (*the complexity of Parray (first, body, last) should be at least the same
		as that of Pstruct (first, RArray body, last) *)
         | Parray ( a, { tokens  = ts
                       , lengths = ls
                       , first   = f
                       , body    = b
                       , last    = l
                       }
                  )                =>
	     (*TODO: we are not looking at lengths here now*)
             let val (f', b', l')     = if mode = 0 then (measure 0 f, measure 0 b, measure 0 l)
				        else (f, b, l)
                 val avglen : real = (Real.fromInt (getCoverage b)) / (Real.fromInt (#coverage a))
                 val tcomp  = sumComps [ constructorComp (*this is for the Pstruct *)
				       , constructorComp (*this is for PArray *)
				       , constructorComp (*this is for the sep *)
				       , constructorComp (*this is for the term *)
				       , Choices 3 (*card 3 for Pstruct(first, RArray body, last) *)	
				       , unitComp  (* for sep in RArray *)
				       , unitComp  (* for term in RArray *)
                                       , getTypeComp f'
                                       , getTypeComp l'
                                       , getTypeComp b'
                                       ]
                 val acomp  = sumComps [ getAtomicComp f'
                                       , multCompR avglen ( getAtomicComp b' )
                                       , getAtomicComp l'
                                       ]
                 val dcomp  = sumComps [ getDataComp f'
                                       , getDataComp l'
                                       , getDataComp b'
                                       ]
                 val comps  = { tc = tcomp, adc = acomp, dc = dcomp }
		(*
		 val len = #len (getAuxInfo f') + #len (getAuxInfo l') + 
				(Real.fromInt (getCoverage b') / Real.fromInt (getCoverage f')) 
				* (#len (getAuxInfo b'))
		*)
             in Parray ( updateLenComps a 0.0 comps
                       , { tokens  = ts
                         , lengths = ls
                         , first   = f'
                         , body    = b'
                         , last    = l'
                         }
                       )
             end
         | Switch ( a, id, bs)      =>
             let val switches         = map #1 bs
                 val branches         = map #2 bs
                 val cover            = #coverage a
                 val sumBranches      = sumCoverage branches
                 val measuredBranches = if mode = 0 then map (measure 0) branches else branches
                 val branchesTypeComp = sumTypeComps measuredBranches
                 val weightedBranches = weightedAtomic sumBranches measuredBranches
                 val branchesDataComp = sumDataComps branches
                 val comps            = { tc  = sumComps [ constructorComp
                                                         , cardComp bs
                                                         , branchesTypeComp
                                                         ]
                                        , adc = weightedBranches
                                        , dc  = sumDataComps measuredBranches
                                        }
		(*
		val cov_len_pairs = map (fn ty => 
					  let val a = getAuxInfo ty 
					  in (#coverage a, #len a)
					  end) measuredBranches
		val len = weightedSum cov_len_pairs
		*)
             in Switch ( updateLenComps a 0.0 comps
                       , id
                       , ListPair.zip ( switches, measuredBranches )
                       )
             end
         | RArray ( aux, osep, oterm, body, olen, ls ) =>
	     (*TODO: we are not looking at lengths here now*)
             let val maxlen           = maxInt (map #1 ls)
                 val avglen : real = (Real.fromInt (getCoverage body)) / (Real.fromInt (#coverage aux))
                 val mBody            = if mode = 0 then measure 0 body else body
                 val { tc = tbody, adc = abody, dc = dbody } = getComps mBody
		(* Do not count the complexity of len, term and sep as these are desirable info *)
		(*
                 val { tc = tlen, adc = alen, dc = dlen }    = refinedOptionComp olen
                 val { tc = tterm, adc = aterm, dc = dterm } = refinedOptionComp oterm
                 val { tc = tsep, adc = asep, dc = dsep }    = refinedOptionComp osep
                 val tcomp = sumComps [ constructorComp, tbody, tterm, tsep, unitComp, unitComp ]
                 val acomp = sumComps [ abody, alen, aterm, asep]
                 val dcomp = sumComps [ dbody, dlen, dterm, dsep]
		*)
		 val tlen = case olen of NONE => unitComp | _ => zeroComp
		 val tsep = case osep of NONE => unitComp | _ => zeroComp
		 val tterm = case oterm of NONE => unitComp | _ => zeroComp
                 val tcomp = sumComps [ constructorComp, tbody, tlen, tsep, tterm ]
                 val acomp = multCompR avglen abody
                 val dcomp = dbody
                 val comps = { tc = tcomp, adc = acomp, dc = dcomp }
		 (* val len = avglen * (#len (getAuxInfo mBody))*)
                 fun updateRArray ( comps : TyComp ) =
                   RArray ( updateLenComps aux 0.0 comps, osep, oterm, mBody, olen, ls )
             in updateRArray comps
             end
         | Poption ( aux, ty ) =>
             let val mBody   = if mode = 0 then measure 0 ty else ty
                 val tycomp  = getComps mBody
                 val tcomp   = sumComps [ constructorComp, #tc tycomp, unitComp ]
                 (* Half as complex, because sometimes not there ????? *)
                 val acomp   = (* combine unitComp *)  
                                       ( multCompR ( frac ( getCoverage ty )
                                                          ( #coverage aux )
                                                   )
                                                   ( #adc tycomp )
                                       )
                 val dcomp   = combine unitComp ( #dc tycomp )
                 val tycomp' = { tc = tcomp, adc = acomp, dc = dcomp }
		 (* val len = (frac (getCoverage ty) (#coverage aux)) * (#len (getAuxInfo mBody))*)
             in Poption ( updateLenComps aux 0.0 tycomp', mBody )
	     end
    )

(*
    (* Using this function will result in lots of computation on
       the type, which may have already been done
     *)
    fun typeMeasure ( ty : Ty ) : Complexity = getTypeComp ( measure ty )
    fun dataMeasure ( ty : Ty ) : Complexity = getDataComp ( measure ty )
*)
end
