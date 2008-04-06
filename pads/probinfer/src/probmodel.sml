structure Probmodel = struct
    open Complexity
    open Types
    open Hosts

    (* Get the maximum from a list of integers *)
    fun maxInt ( l : int list ) : int = foldl Int.max 0 l

    (* Make a base complexity from a multiplier (often maximum length of token)
       and a number of choices. *)
    fun mkBaseComp ( avg : real ) ( tot : LargeInt.int ) ( choices : LargeInt.int ) : TyComp =
        { tc  = constructorComp
        , adc = multCompR avg (int2Comp choices )
        , dc  = multComp tot ( int2Comp choices )
        }

    fun mkBaseCompR ( avg : real ) ( tot : real ) ( choices : LargeInt.int ) : TyComp =
        { tc  = constructorComp
        , adc = multCompR avg ( int2Comp choices )
        , dc  = multCompR tot ( int2Comp choices )
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
	| _ => (256, 0)

    (* Compute the type and data complexity of a refined type *)
    fun refinedComp ( avg : real )         (* Average length of tokens *)
                    ( tot : LargeInt.int ) (* Sum of length of tokens *)
                    ( num : LargeInt.int ) (* Number of tokens *)
		    ( lens : int list ) (* list of token lengths *)
                    ( r : Refined )        (* refined type *)
                     : TyComp =            (* Complexity numbers *)
        ( case r of
               StringME s     => 
			let
		     	  (* val _ = print ("Parsing " ^ s ^".\n") *)
			  val (choices, numConstChars) = parseRegEx s
			  (* val _ = print ("Choices = " ^ (LargeInt.toString choices) ^ "\n") *)
			  fun f (len, c) = combine c 
					    (multCompS (len-numConstChars) 
						(int2Comp choices))
			  val totaldc = foldl f zeroComp lens
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
             | Enum rl        => { tc  = sumComps [ sumComps ( map (refinedTypeComp avg tot num lens) rl )
                                                  , constructorComp
                                                  , int2CompS ( length rl )
                                                  ]
                                 , adc = int2CompS ( length rl )
                                 , dc  = multCompS ( length rl )
                                                   ( int2CompS ( length rl ) )
                                 }
             | LabelRef i     => { tc =  unitComp, adc = unitComp, dc = unitComp }
        )
    (* Get the type complexity of a refined type, assuming multiplier of 1 *)
    and refinedTypeComp ( avg : real )         (* Average length of tokens *)
                        ( tot : LargeInt.int ) (* Sum of length of tokens *)
                        ( num : LargeInt.int ) (* Number of tokens *)
			( lens : int list ) (* lenths of tokens *)
                        ( r : Refined )        (* refined type *)
                         : Complexity =        (* Type complexity *)
           #tc (refinedComp avg tot num lens r)
    (* Get the type complexity of a refined type, assuming multiplier of 1 *)
    and refinedDataComp ( avg : real )         (* Average length of tokens *)
                        ( tot : LargeInt.int ) (* Sum of length of tokens *)
                        ( num : LargeInt.int ) (* Number of tokens *)
			( lens : int list ) (* lenths of tokens *)
                        ( r : Refined )        (* refined type *)
                         : Complexity =        (* Data complexity *)
          #dc (refinedComp avg tot num lens r)

    (* Measure a refined base type *)
    exception NotRefinedBase (* Should be called only with refined base type *)

    fun newmeasureRefined ( ty : NewTy ) : NewTy =
    ( case ty of
           PPRefinedBase ( a, r, ts ) =>
	     let val avg = avgBSLTokenLength ts
		 val tot = sumBSLTokenLength ts
		 val num = length ts
             	 val comps = refinedComp avg tot (Int.toLarge num) 
				(map bslTokenLength ts) r
             in PPRefinedBase ( updateComps a comps, r, ts )
             end
         | _ => raise NotRefinedBase
    )

    (* Complexity of refined option type, assuming multiplier 1 *)
    fun refinedOptionComp ( ro : Refined option ) : TyComp =
    ( case ro of
	   (*refined type doesn't exist, assume bigger value *)
           NONE   => zeroComps
         | SOME r => refinedComp 1.0 1 1 [1] r (* Probably wrong ***** *)
    )

    (* Compute the complexity of a base type *)
    fun baseComp ( lts : BSLToken list ) : TyComp =
    ( case lts of
           []      => { tc = zeroComp, adc = zeroComp, dc = zeroComp }
         | (t::ts) =>
             let val avglen : real         = avgBSLTokenLength lts
                 val totlen : LargeInt.int = sumBSLTokenLength lts
                 val numTokens : LargeInt.int = Int.toLarge (length lts)
                 val mult : real           = Real.fromLargeInt numTokens * avglen
             in ( case btokenOf t of
                    PPbXML     => mkBaseComp avglen totlen numXMLChars
                  | PPeXML     => mkBaseComp avglen totlen numXMLChars
                  | PPtime            => mkBaseComp 1.0 numTokens numTime
                  | PPdate            => mkBaseComp 1.0 numTokens numDate
                  | PPpath            => mkBaseComp avglen totlen numStringChars
                  | PPurl             => mkBaseComp avglen totlen numStringChars
                  | PPurlbody             => mkBaseComp avglen totlen numStringChars
                  | PPemail             => mkBaseComp avglen totlen numStringChars
                  | PPword             => mkBaseComp avglen totlen numWordChars
                  | PPhstring             => mkBaseComp avglen totlen numHexChars
                  | PPid             => mkBaseComp avglen totlen numIdChars
                  | PPmessage             => mkBaseComp avglen totlen numStringChars
                  | PPtext             => mkBaseComp avglen totlen numTextChars
                  | PPpermission             => mkBaseComp avglen totlen numPermissionChars
                  | PPmac            => mkBaseComp avglen totlen numHexChars
                  | PPip              => mkBaseComp avglen totlen numIPTriplet
                  | PPhostname        => mkBaseComp avglen totlen numStringChars
                  | PPint             => { tc  = constructorComp
                                         , adc = combine ( int2Comp 2 )
                                                         ( multCompR avglen ( int2Comp numDigits ) )
                                         , dc  = combine ( int2Comp 2 )
                                                         ( multComp totlen ( int2Comp numDigits ) )
                                         }
                  | PPfloat       => mkBaseComp avglen totlen numDigits
                  | PPwhite          => mkBaseComp avglen totlen numWhiteChars
                  | PPempty            => { tc  = constructorComp
                                         , adc = zeroComp
                                         , dc  = zeroComp
                                         }
                  | PPblob             => mkBaseComp avglen totlen numStringChars
                  | PPpunc c             => mkBaseComp avglen totlen 0 (*numPuncChars*)
                  | PPError             => { tc  = constructorComp
                                         , adc = unitComp
                                         , dc  = unitComp
                                         }

                )
             end
    )

    fun mkBaseComplexity ( a : AuxInfo ) ( ts : BSLToken list ) : NewTy =
    let val ty      = PPBase (a, ts)
        val comps   = baseComp ts
        fun updateCompBase ( ty : NewTy ) ( comps : TyComp ) : NewTy =
            PPBase ( updateComps a comps, ts )
    in updateCompBase ty comps
    end

    fun maxContextComplexity ( cl : NewContext list ) : TyComp =
    let fun f ( ltl : BSLToken list, comps : TyComp ) : TyComp =
              combTyComp ( baseComp ltl ) comps
    in foldl f zeroComps cl
    end

    fun frac ( m : int ) ( n : int ) : real = Real.fromInt m / Real.fromInt n

    (* Compute the weighted sum of the data complexities of a list of types *)
    fun weightedData ( tot : int ) ( tys : NewTy list ) : Complexity =
    let fun f ( t : NewTy, c : Complexity ) : Complexity =
              combine c ( multCompR ( frac ( getNCoverage t ) tot )
                                    ( getNDataComp t )
                        )
    in foldl f zeroComp tys
    end

    fun weightedAtomic ( tot : int ) ( tys : NewTy list ) : Complexity =
    let fun f ( t : NewTy, c : Complexity ) : Complexity =
              combine c ( multCompR ( frac ( getNCoverage t ) tot )
                                    ( getNAtomicComp t )
                        )
    in foldl f zeroComp tys
    end

    (* Compute the type and data complexity of an inferred type *)    
    fun newmeasure ( ty : NewTy ) : NewTy =
    ( case ty of
           PPBase ( a, ts )               => mkBaseComplexity a ts
         | PPTBD ( a, i, cl )             =>
             let val comps = maxContextComplexity cl
             in PPTBD ( updateComps a comps, i, cl )
             end
         | PPBottom ( a, i, cl )          =>
             let val comps = maxContextComplexity cl
             in PPBottom ( updateComps a comps, i, cl )
             end
         | PPstruct (a,tys)              =>
             let val measuredtys = map newmeasure tys
                 val comps = { tc  = sumComps [ constructorComp 
                                              , cardComp tys
                                              , sumNTypeComps measuredtys
                                              ]
                             , adc = sumNAtomicComps measuredtys
                             , dc  = sumNDataComps measuredtys
                             }
             in PPstruct ( updateComps a comps, measuredtys )
             end
         | PPunion (a,tys)               =>
             let val measuredtys = map newmeasure tys
                 val comps = { tc  = sumComps [ constructorComp
                                              , cardComp tys
                                              , sumNTypeComps measuredtys
                                              ]
                             , adc = combine ( cardComp tys )
                                             ( weightedAtomic
                                                  ( sumNCoverage measuredtys )
                                                  measuredtys
                                             )
                             , dc  = combine ( cardComp tys )
                                             ( sumNDataComps measuredtys )
                             }
             in PPunion ( updateComps a comps, measuredtys )
             end
	 (*the complexity of Parray (first, body, last) should be at least the same
		as that of Pstruct (first, RArray body, last) *)
         | PParray ( a, { tokens  = ts
                       , lengths = ls
                       , first   = f
                       , body    = b
                       , last    = l
                       }
                  )                =>
	     (*TODO: we are not looking at lengths here now*)
             let val f'     = newmeasure f
                 val b'     = newmeasure b
                 val l'     = newmeasure l
                 val avglen : real = avgInts ( map #2 ls )
                 val tcomp  = sumComps [ constructorComp (*this is for the Pstruct *)
				       , constructorComp (*this is for PArray *)
				       , constructorComp (*this is for the sep *)
				       , constructorComp (*this is for the term *)
				       , Choices 3 (*card 3 for Pstruct(first, RArray body, last) *)	
				       , unitComp  (* for sep in RArray *)
				       , unitComp  (* for term in RArray *)
                                       , getNTypeComp f'
                                       , getNTypeComp l'
                                       , getNTypeComp b'
                                       ]
                 val acomp  = sumComps [ getNAtomicComp f'
                                       , multCompR avglen ( getNAtomicComp l' )
                                       , getNAtomicComp b'
                                       ]
                 val dcomp  = sumComps [ getNDataComp f'
                                       , getNDataComp l'
                                       , getNDataComp b'
                                       ]
                 val comps  = { tc = tcomp, adc = acomp, dc = dcomp }
             in PParray ( updateComps a comps
                       , { tokens  = ts
                         , lengths = ls
                         , first   = f'
                         , body    = b'
                         , last    = l'
                         }
                       )
             end
         | rb as PPRefinedBase ( a, r, ts ) =>
             let val avg = avgBSLTokenLength ts
                 val tot = sumBSLTokenLength ts
                 val num = LargeInt.fromInt ( length ts )
             in newmeasureRefined rb
             end
         | PPSwitch ( a, id, bs)      =>
             let val switches         = map #1 bs
                 val branches         = map #2 bs
                 val cover            = #coverage a
                 val sumBranches      = sumNCoverage branches
                 val measuredBranches = map newmeasure branches
                 val branchesTypeComp = sumNTypeComps measuredBranches
                 val weightedBranches = weightedAtomic sumBranches measuredBranches
                 val branchesDataComp = sumNDataComps branches
                 val comps            = { tc  = sumComps [ constructorComp
                                                         , cardComp bs
                                                         , branchesTypeComp
                                                         ]
                                        , adc = weightedBranches
                                        , dc  = sumNDataComps measuredBranches
                                        }
             in PPSwitch ( updateComps a comps
                       , id
                       , ListPair.zip ( switches, measuredBranches )
                       )
             end
         | PPRArray ( aux, osep, oterm, body, olen, ls ) =>
	     (*TODO: we are not looking at lengths here now*)
             let val maxlen           = maxInt (map #1 ls)
                 val avglen : real    = avgInts (map #1 ls)
                 val mBody            = newmeasure body
                 val { tc = tbody, adc = abody, dc = dbody } = getNComps mBody
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
                 fun updateRArray ( comps : TyComp ) =
                   PPRArray ( updateComps aux comps, osep, oterm, mBody, olen, ls )
             in updateRArray comps
             end
         | PPoption ( aux, ty ) =>
             let val mBody   = newmeasure ty
                 val tycomp  = getNComps mBody
                 val tcomp   = sumComps [ constructorComp, #tc tycomp, unitComp ]
                 (* Half as complex, because sometimes not there ????? *)
                 val acomp   = combine unitComp
                                       ( multCompR ( frac ( getNCoverage ty )
                                                          ( #coverage aux )
                                                   )
                                                   ( #adc tycomp )
                                       )
                 val dcomp   = combine unitComp ( #dc tycomp )
                 val tycomp' = { tc = tcomp, adc = acomp, dc = dcomp }
             in PPoption ( updateComps aux tycomp', mBody )
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
