(*------------------------------------------------------------------------------
--
-- Revision history
--
-- version 1: March 1, 2007: first draft
-- version 2: March 8, 2007:
--   Fixed elementary coding bugs
--   Changed type complexity to codebook scheme
--   Ran against some of the PADS test cases
-- version 3: March 8, 2007
--   Tested and fixed Refined base type computations.
--
------------------------------------------------------------------------------*)

structure Model = struct
    open Complexity
    open Types

    (* Get the maximum from a list of integers *)
    fun maxInt ( l : int list ) : int = foldl Int.max 0 l

    (* Make a base complexity from a multiplier (often maximum length of token)
       and a number of choices. *)
    fun mkBaseComp ( mult : int ) ( choices : int ) : Complexity  * Complexity =
        ( unitComplexity, multComp mult ( int2Complexity choices ) )
    (* Same thing with a large integer *)
    fun mkBaseCompL ( mult : int ) ( choices : LargeInt.int ) : Complexity  * Complexity =
        ( unitComplexity, multComp mult ( int2ComplexityL choices ) )

    (* Compute the type and data complexity of a refined type *)
    fun refinedComp ( multiplier:int ) ( r:Refined ) : Complexity * Complexity =
        ( case r of
               StringME s     => mkBaseComp multiplier numStringChars
             | Int (min, max) => mkBaseCompL multiplier ( max - min + 1 )
             | IntConst n     => ( unitComplexity, int2ComplexityL n )
             | StringConst s  => ( unitComplexity, int2Complexity (size s) )
               (* Need term here for number of choices???? *)
             | Enum rl        => ( unitComplexity, sumComps ( map refinedDataComp rl))
             | LabelRef i     => ( unitComplexity, unitComplexity )
        )
    (* Get the type complexity of a refined type, assuming multiplier of 1 *)
    and refinedTypeComp ( r : Refined ) : Complexity = #1 (refinedComp 1 r)
    (* Get the type complexity of a refined type, assuming multiplier of 1 *)
    and refinedDataComp ( r : Refined ) : Complexity = #2 (refinedComp 1 r)


    (* Measure a refined base type *)
    exception NotRefinedBase
    fun measureRefined (m:int) (ty:Ty) : Ty =
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
           NONE =>   ( zeroComplexity, zeroComplexity )
         | SOME r => refinedComp 1 r
    )

    (* Compute the complexity of a base type (e.g. Pint) *)
    fun baseComp ( lts : LToken list ) : Complexity * Complexity =
    ( case lts of
           []      => ( zeroComplexity, zeroComplexity )
         | (t::ts) =>
             let val maxlen  = maxTokenLength lts
                 val nTimes  = 60 * 60 * 24
             in ( case tokenOf t of
                    PbXML (s1, s2)    => mkBaseComp maxlen numXMLChars
                  | PeXML (s1, s2)    => mkBaseComp maxlen numXMLChars
                  | Ptime s           => mkBaseComp maxlen nTimes
                  | Pdate s           => mkBaseComp maxlen 365
                  | Ppath s           => mkBaseComp maxlen 256
                  | Purl s            => mkBaseComp maxlen 256
                  | Pip s             => mkBaseComp maxlen 256
                  | Pint l            => mkBaseComp maxlen 10
                  | Pstring s         => mkBaseComp maxlen numStringChars
                  | Pgroup x          => ( unitComplexity, unitComplexity )
                  | Pwhite s          => mkBaseComp maxlen numWhiteChars
                  | Other c           => mkBaseComp 1 256
                  | Pempty            => ( unitComplexity, unitComplexity )
                  | Error             => ( unitComplexity, unitComplexity )
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
    in foldl f ( zeroComplexity, zeroComplexity ) cl
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
             in Pstruct ( updateComps a (sumTypeComps measuredtys)
                                        (sumDataComps measuredtys)
                        , measuredtys
                        )
             end
         | Punion (a,tys)               =>
             let val measuredtys = map measure tys
             in Punion ( updateComps a
                           ( combine (cardComp tys) (sumTypeComps measuredtys) )
                           ( combine (cardComp tys) (sumDataComps measuredtys) )
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
                 val maxlen = maxInt (map #1 ls)
                 val tcomp  = sumComps [ getTypeComp f'
                                       , getTypeComp l'
                                       , getTypeComp b'
                                       ]
                 val dcomp  = sumComps [ getDataComp f'
                                       , getDataComp l'
                                       , multComp maxlen (getDataComp b')
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
         | rb as RefinedBase ( a, r, ts ) => measureRefined (maxTokenLength ts) rb
         | Switch ( a, id, bs)      =>
             let val switches         = map #1 bs
                 val branches         = map #2 bs
                 val switchesComps    = map (refinedComp 1) switches
                 val switchesTypeComp = sumComps (map #1 switchesComps)
                 val switchesDataComp = sumComps (map #2 switchesComps)
                 val measuredBranches = map measure branches
                 val branchesTypeComp = sumTypeComps measuredBranches
                 val branchesDataComp = sumDataComps measuredBranches
             in Switch ( updateComps a
                          (combine switchesTypeComp branchesTypeComp)
                          (combine switchesDataComp branchesDataComp)
                       , id
                       , ListPair.zip ( switches, measuredBranches )
                       )
             end
         | RArray ( a, osep, oterm, body, olen, ls ) =>
             let val maxlen           = maxInt (map #1 ls)
                 val mBody            = measure body
                 val (tbody, dbody)   = getComps mBody
                 val ( tlen, dlen )   = refinedOptionComp olen
                 val ( tterm, dterm ) = refinedOptionComp oterm
                 val ( tsep, dsep )   = refinedOptionComp osep
                 val tcomp = sumComps [ tbody, tlen, tterm, tsep ]
                 val dcomp = sumComps [ multComp maxlen dbody, dlen, dterm, dsep]
                 fun updateRArray (t:Complexity) (d:Complexity) =
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
