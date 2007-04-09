structure Model = struct
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

    (* Compute the type and data complexity of a refined type *)
    fun refinedComp ( avg : real )         (* Average length of tokens *)
                    ( tot : LargeInt.int ) (* Sum of length of tokens *)
                    ( num : LargeInt.int ) (* Number of tokens *)
                    ( r : Refined )        (* refined type *)
                     : TyComp =            (* Complexity numbers *)
        ( case r of
               StringME s     => mkBaseComp avg tot numStringChars
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
    fun measureRefined ( avg : real )         (* Average length of tokens *)
                       ( tot : LargeInt.int ) (* Sum of length of tokens *)
                       ( num : LargeInt.int ) (* Number of tokens *)
                       ( ty : Ty ) : Ty =
    ( case ty of
           RefinedBase ( a, r, ts ) =>
             let val comps = refinedComp avg tot num r
             in RefinedBase ( updateComps a comps, r, ts )
             end
         | _ => raise NotRefinedBase
    )

    (* Complexity of refined option type, assuming multiplier 1 *)
    fun refinedOptionComp ( ro : Refined option ) : TyComp =
    ( case ro of
           NONE   => zeroComps
         | SOME r => refinedComp 1.0 1 1 r (* Probably wrong ***** *)
    )

    (* Compute the complexity of a base type *)
    fun baseComp ( lts : LToken list ) : TyComp =
    ( case lts of
           []      => { tc = zeroComp, adc = zeroComp, dc = zeroComp }
         | (t::ts) =>
             let val avglen : real         = avgTokenLength lts
                 val totlen : LargeInt.int = sumTokenLength lts
                 val numTokens : int       = length lts
                 val mult : real           = Real.fromInt numTokens * avglen
             in ( case tokenOf t of
                    PbXML (s1, s2)    => mkBaseComp avglen totlen numXMLChars
                  | PeXML (s1, s2)    => mkBaseComp avglen totlen numXMLChars
                  | Ptime s           => mkBaseComp avglen totlen numTime
                  | Pdate s           => mkBaseComp avglen totlen numDate
                  | Ppath s           => mkBaseComp avglen totlen numStringChars
                  | Purl s            => mkBaseComp avglen totlen numStringChars
                  | Pip s             => mkBaseComp avglen totlen numIP
                  | Phostname s       => mkBaseComp avglen totlen numStringChars
                  | Pint (l, s)            => { tc  = constructorComp
                                         , adc = combine ( int2Comp 2 )
                                                         ( multCompR avglen ( int2Comp numDigits ) )
                                         , dc  = combine ( int2Comp 2 )
                                                         ( multComp totlen ( int2Comp numDigits ) )
                                         }
		  (*TODO: what to do for float *)
                  | Pfloat (i,f)      => mkBaseComp avglen totlen numDigits
                  | Pstring s         => mkBaseComp avglen totlen numStringChars
                  | Pgroup x          => { tc  = constructorComp
                                         , adc = unitComp
                                         , dc  = unitComp
                                         }
                  | Pwhite s          => mkBaseComp avglen totlen numWhiteChars
                  | Other c           => mkBaseComp avglen totlen numStringChars
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
        val comps   = baseComp ts
        fun updateCompBase ( ty : Ty ) ( comps : TyComp ) : Ty =
            Base ( updateComps a comps, ts )
    in updateCompBase ty comps
    end

    fun maxContextComplexity ( cl : Context list ) : TyComp =
    let fun f ( ltl : LToken list, comps : TyComp ) : TyComp =
              combTyComp ( baseComp ltl ) comps
    in foldl f zeroComps cl
    end

    fun frac ( m : int ) ( n : int ) : real = Real.fromInt m / Real.fromInt n

    (* Compute the weighted sum of the data complexities of a list of types *)
    fun weightedData ( tot : int ) ( tys : Ty list ) : Complexity =
    let fun f ( t : Ty, c : Complexity ) : Complexity =
              combine c ( multCompR ( frac ( getCoverage t ) tot )
                                    ( getDataComp t )
                        )
    in foldl f zeroComp tys
    end

    fun weightedAtomic ( tot : int ) ( tys : Ty list ) : Complexity =
    let fun f ( t : Ty, c : Complexity ) : Complexity =
              combine c ( multCompR ( frac ( getCoverage t ) tot )
                                    ( getAtomicComp t )
                        )
    in foldl f zeroComp tys
    end

    (* Compute the type and data complexity of an inferred type *)    
    fun measure ( ty : Ty ) : Ty =
    ( case ty of
           Base ( a, ts )               => mkBaseComplexity a ts
         | TBD ( a, i, cl )             =>
             let val comps = maxContextComplexity cl
             in TBD ( updateComps a comps, i, cl )
             end
         | Bottom ( a, i, cl )          =>
             let val comps = maxContextComplexity cl
             in Bottom ( updateComps a comps, i, cl )
             end
         | Pstruct (a,tys)              =>
             let val measuredtys = map measure tys
                 val comps = { tc  = sumComps [ constructorComp 
                                              , cardComp tys
                                              , sumTypeComps measuredtys
                                              ]
                             , adc = sumAtomicComps measuredtys
                             , dc  = sumDataComps measuredtys
                             }
             in Pstruct ( updateComps a comps, measuredtys )
             end
         | Punion (a,tys)               =>
             let val measuredtys = map measure tys
                 val comps = { tc  = sumComps [ constructorComp
                                              , cardComp tys
                                              , sumTypeComps measuredtys
                                              ]
                             , adc = combine ( cardComp tys )
                                             ( weightedAtomic
                                                  ( sumCoverage measuredtys )
                                                  measuredtys
                                             )
                             , dc  = combine ( cardComp tys )
                                             ( sumDataComps measuredtys )
                             }
             in Punion ( updateComps a comps, measuredtys )
             end
         | Parray ( a, { tokens  = ts
                       , lengths = ls
                       , first   = f
                       , body    = b
                       , last    = l
                       }
                  )                =>
             let val f'     = measure f
                 val b'     = measure b
                 val l'     = measure l
                 val tcomp  = sumComps [ constructorComp
                                       , getTypeComp f'
                                       , getTypeComp l'
                                       , getTypeComp b'
                                       ]
                 val acomp  = sumComps [ getAtomicComp f'
                                       , getAtomicComp l'
                                       , getAtomicComp b'
                                       ]
                 val dcomp  = sumComps [ getDataComp f'
                                       , getDataComp l'
                                       , getDataComp b'
                                       ]
                 val comps  = { tc = tcomp, adc = acomp, dc = dcomp }
             in Parray ( updateComps a comps
                       , { tokens  = ts
                         , lengths = ls
                         , first   = f'
                         , body    = b'
                         , last    = l'
                         }
                       )
             end
         | rb as RefinedBase ( a, r, ts ) =>
             let val avg = avgTokenLength ts
                 val tot = sumTokenLength ts
                 val num = LargeInt.fromInt ( length ts )
             in measureRefined avg tot num rb
             end
         | Switch ( a, id, bs)      =>
             let val switches         = map #1 bs
                 val branches         = map #2 bs
                 val cover            = #coverage a
                 val sumBranches      = sumCoverage branches
                 val measuredBranches = map measure branches
                 val branchesTypeComp = sumTypeComps measuredBranches
                 val weightedBranches = weightedAtomic sumBranches measuredBranches
                 val branchesDataComp = sumDataComps branches
                 val comps            = { tc  = sumComps [ constructorComp
                                                         , cardComp bs
                                                         , branchesTypeComp
                                                         ]
                                        , adc = weightedBranches
                                        , dc  = combine ( cardComp branches )
                                                        ( sumDataComps measuredBranches )
                                        }
             in Switch ( updateComps a comps
                       , id
                       , ListPair.zip ( switches, measuredBranches )
                       )
             end
         | RArray ( aux, osep, oterm, body, olen, ls ) =>
             let val maxlen           = maxInt (map #1 ls)
                 val mBody            = measure body
                 val { tc = tbody, adc = abody, dc = dbody } = getComps mBody
                 val { tc = tlen, adc = alen, dc = dlen }    = refinedOptionComp olen
                 val { tc = tterm, adc = aterm, dc = dterm } = refinedOptionComp oterm
                 val { tc = tsep, adc = asep, dc = dsep }    = refinedOptionComp osep
                 val tcomp = sumComps [ constructorComp, tbody, tterm, tsep, unitComp, unitComp ]
                 val acomp = sumComps [ abody, alen, aterm, asep]
                 val dcomp = sumComps [ dbody, dlen, dterm, dsep]
                 val comps = { tc = tcomp, adc = acomp, dc = dcomp }
                 fun updateRArray ( comps : TyComp ) =
                   RArray ( updateComps aux comps, osep, oterm, mBody, olen, ls )
             in updateRArray comps
             end
         | Poption ( aux, ty ) =>
             let val mBody   = measure ty
                 val tycomp  = getComps mBody
                 val tcomp   = sumComps [ constructorComp, #tc tycomp, unitComp ]
                 (* Half as complex, because sometimes not there ????? *)
                 val acomp   = combine unitComp
                                       ( multCompR ( frac ( getCoverage ty )
                                                          ( #coverage aux )
                                                   )
                                                   ( #adc tycomp )
                                       )
                 val dcomp   = combine unitComp
                                       ( multCompR ( frac ( getCoverage ty )
                                                          ( #coverage aux )
                                                   )
                                                   ( #dc tycomp )
                                       )
                 val tycomp' = { tc = tcomp, adc = acomp, dc = dcomp }
             in Poption ( updateComps aux tycomp', mBody )
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
