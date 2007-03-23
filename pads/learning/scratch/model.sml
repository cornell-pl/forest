structure Model = struct
    open Complexity
    open Types
    open Hosts

    (* Get the maximum from a list of integers *)
    fun maxInt ( l : int list ) : int = foldl Int.max 0 l

    (* Make a base complexity from a multiplier (often maximum length of token)
       and a number of choices. *)
    fun mkBaseComp ( mult : LargeInt.int ) ( choices : LargeInt.int ) : Complexity  * Complexity =
        ( constructorComp, multComp mult ( int2Comp choices ) )
    fun mkBaseCompR ( mult : real ) ( choices : LargeInt.int ) : Complexity  * Complexity =
        ( constructorComp, multCompR mult ( int2Comp choices ) )

    (* Compute the type and data complexity of a refined type *)
    fun refinedComp ( mult : LargeInt.int ) ( r : Refined ) : Complexity * Complexity =
        ( case r of
               StringME s     => mkBaseComp mult numStringChars
             | Int (min, max) => ( sumComps [ constructorComp
                                            , int2Comp min
                                            , int2Comp max
                                            ]
                                 , multComp mult ( int2Comp ( max - min + 1 ) )
                                 )
             | IntConst n     => ( sumComps [ constructorComp, int2Comp 2, int2Comp n ]
                                 , zeroComp
                                 )
             | StringConst s  => ( combine constructorComp
                                           ( multCompS (size s) (int2Comp numStringChars) )
                                 , zeroComp
                                 )
             | Enum rl        => ( combine ( maxComps ( map (refinedTypeComp mult) rl ) )
                                           ( sumComps [ constructorComp
                                                      , int2CompS ( length rl )
                                                      ]
                                           )
                                  (* Assumes all brances constants *)
                                 , multComp mult ( int2CompS ( length rl ) )
                                 )
             | LabelRef i     => ( unitComp, unitComp )
        )
    (* Get the type complexity of a refined type, assuming multiplier of 1 *)
    and refinedTypeComp ( mult : LargeInt.int ) ( r : Refined ) : Complexity = #1 (refinedComp mult r)
    (* Get the type complexity of a refined type, assuming multiplier of 1 *)
    and refinedDataComp ( mult : LargeInt.int ) ( r : Refined ) : Complexity = #2 (refinedComp mult r)

    (* Measure a refined base type *)
    exception NotRefinedBase (* Function should be called only with refined base type *)
    fun measureRefined ( m : LargeInt.int ) ( ty : Ty ) : Ty =
    ( case ty of
           RefinedBase ( a, r, ts ) =>
             let val ( tcomp, dcomp ) = refinedComp m r
             in RefinedBase ( updateComps a tcomp dcomp, r, ts )
             end
         | _ => raise NotRefinedBase
    )

    (* Complexity of refined option type, assuming multiplier 1 *)
    fun refinedOptionComp ( ro : Refined option ) : Complexity * Complexity =
    ( case ro of
           NONE   => ( zeroComp, zeroComp )
         | SOME r => refinedComp 1 r
    )

    (* Compute the complexity of a base type *)
    fun baseComp ( lts : LToken list ) : Complexity * Complexity =
    ( case lts of
           []      => ( zeroComp, zeroComp )
         | (t::ts) =>
             let val avglen : real         = avgTokenLength lts
                 val totlen : LargeInt.int = sumTokenLength lts
                 val numTokens : int       = length lts
                 val mult : real           = Real.fromInt numTokens * avglen
             in ( case tokenOf t of
                    PbXML (s1, s2)    => mkBaseComp totlen numXMLChars
                  | PeXML (s1, s2)    => mkBaseComp totlen numXMLChars
                  | Ptime s           => mkBaseComp totlen numTime
                  | Pdate s           => mkBaseComp totlen numDate
                  | Ppath s           => mkBaseComp totlen numStringChars
                  | Purl s            => mkBaseComp totlen numStringChars
                  | Pip s             => mkBaseComp totlen numIP
                  | Phostname s       => mkBaseComp totlen numStringChars
                  | Pint l            => ( constructorComp
                                         , combine ( int2Comp 2 )
                                                   ( multCompR avglen ( int2Comp numDigits ) )
                                         )
                  | Pstring s         => ( constructorComp
                                         , multComp totlen ( int2Comp numStringChars )
                                         )
                  | Pgroup x          => ( constructorComp, unitComp ) (* ???? *)
                  | Pwhite s          => ( constructorComp
                                         , multComp totlen ( int2Comp numWhiteChars )
                                         )
                  | Other c           => mkBaseComp totlen numStringChars
                  | Pempty            => ( constructorComp, unitComp )
                  | Error             => ( constructorComp, unitComp )
                )
             end
    )

    fun mkBaseComplexity ( a : AuxInfo ) ( ts : LToken list ) : Ty =
    let val ty      = Base (a, ts)
        val ( tcomp, dcomp ) = baseComp ts
        fun updateCompBase (ty:Ty) (t:Complexity) (d:Complexity) : Ty =
            Base ( updateComps a t d, ts )
    in updateCompBase ty tcomp dcomp
    end

    fun maxContextComplexity ( cl : Context list ) : Complexity * Complexity =
    let fun f (ltl:LToken list,comps:Complexity * Complexity ):Complexity*Complexity =
        let val ( t1, d1 ) = baseComp ltl
            val ( t2, d2 ) = comps
        in ( combine t1 t2, combine d1 d2 )
        end
    in foldl f ( zeroComp, zeroComp ) cl
    end

    (* Compute the weighted sum of the data complexities of a list of types *)
    fun weighted ( tot : int ) ( tys : Ty list ) : Complexity =
    let fun frac ( m : int ) ( n : int ) : real = Real.fromInt m / Real.fromInt n
        fun f ( t : Ty, c : Complexity ) : Complexity =
              combine c ( multCompR ( frac ( getCoverage t ) tot ) ( getDataComp t ) )
    in foldl f zeroComp tys
    end

    (* Compute the type and data complexity of an inferred type *)    
    fun measure ( ty : Ty ) : Ty =
    ( case ty of
           Base ( a, ts )               => mkBaseComplexity a ts
         | TBD ( a, i, cl )             =>
             let val (t, d) = maxContextComplexity cl
             in TBD ( updateComps a t d, i, cl )
             end
         | Bottom ( a, i, cl )          =>
             let val (t, d) = maxContextComplexity cl
             in Bottom ( updateComps a t d, i, cl )
             end
         | Pstruct (a,tys)              =>
             let val measuredtys = map measure tys
             in Pstruct ( updateComps a ( sumComps [ constructorComp 
                                                   , cardComp tys
                                                   , sumTypeComps measuredtys
                                                   ]
                                        )
                                        ( sumDataComps measuredtys )
                        , measuredtys
                        )
             end
         | Punion (a,tys)               =>
             let val measuredtys = map measure tys
             in Punion ( updateComps a
                           ( sumComps [ constructorComp
                                      , cardComp tys
                                      , sumTypeComps measuredtys
                                      ]
                           )
                           ( combine (cardComp tys)  ( sumDataComps measuredtys ) )
(*                           ( combine (cardComp tys)  ( weighted ( sumCoverage measuredtys ) measuredtys ) ) *)
                       , measuredtys
                       )
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
                 val dcomp  = sumComps [ getDataComp f'
                                       , getDataComp l'
                                       , getDataComp b'
                                       ]
             in Parray ( updateComps a tcomp dcomp
                       , { tokens  = ts
                         , lengths = ls
                         , first   = f'
                         , body    = b'
                         , last    = l'
                         }
                       )
             end
         | rb as RefinedBase ( a, r, ts ) => measureRefined (Int.toLarge (length ts)) rb
         | Switch ( a, id, bs)      =>
             let val switches         = map #1 bs
                 val branches         = map #2 bs
                 val sumBranches      = sumCoverage branches
                 val measuredBranches = map measure branches
                 val branchesTypeComp = sumTypeComps measuredBranches
                 val branchesDataComp = multCompS ( #coverage a ) ( weighted sumBranches measuredBranches )
             in Switch ( updateComps a
                          ( sumComps [ constructorComp, cardComp bs, branchesTypeComp ] )
                          branchesDataComp
                       , id
                       , ListPair.zip ( switches, measuredBranches )
                       )
             end
         | RArray ( a, osep, oterm, body, olen, ls ) =>
             let val maxlen           = maxInt (map #1 ls)
                 val mBody            = measure body
                 val ( tbody, dbody ) = getComps mBody
                 val ( tlen, dlen )   = refinedOptionComp olen
                 val ( tterm, dterm ) = refinedOptionComp oterm
                 val ( tsep, dsep )   = refinedOptionComp osep
                 val tcomp = sumComps [ constructorComp, tbody, tterm, tsep, unitComp, unitComp ]
                 val dcomp = sumComps [ dbody, dlen, dterm, dsep]
                 fun updateRArray ( t : Complexity ) ( d : Complexity ) =
                   RArray ( updateComps a t d, osep, oterm, mBody, olen, ls )
             in updateRArray tcomp dcomp
             end
    )

    (* Using this function will result in lots of computation on
       the type, which may have already been done
     *)
    fun typeMeasure ( ty : Ty ) : Complexity = getTypeComp ( measure ty )
    fun dataMeasure ( ty : Ty ) : Complexity = getDataComp ( measure ty )

end
