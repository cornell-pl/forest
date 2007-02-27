structure Model = struct
    open Complexity
    open Distribution
    open Types

    exception GoFigure
    exception NoDensity
    exception BadModel

    type TokenDensity = Token Density
    fun junkTokenDensity ( t : Token ) : TokenDensity =
       fn ( u : Token ) => if u = t then 1.0 else 0.0

    type TokenListDensity = ( Token list ) Density
    fun junkTokenListDensity ( tl : Token list ) : TokenListDensity =
      fn ( ul : Token list ) => if ul = tl then 1.0 else 0.0

    fun maxInt ( l : int list ) : int = foldl Int.max 0 l

    fun refinedComp ( multiplier:int ) ( r:Refined ) : Complexity * Complexity =
        let val rmaxlen = Real.fromInt multiplier
        in ( case r of
                  StringME s     => ( prob2Complexity probStringChar
                                    , prob2Complexity ( power probStringChar rmaxlen )
                                    )
                | Int (min, max) => ( int2ComplexityL ( max - min + 1 )
                                    , multComp multiplier
                                               ( int2ComplexityL ( max - min + 1 ) )
                                    )
                | IntConst n     => ( int2ComplexityL n, int2ComplexityL n )
                | StringConst s  => ( int2Complexity (size s)
                                    , int2Complexity (size s)
                                    )
                | Enum rl        =>
                    let val comps     = map (refinedComp 1) rl
                        val typeComps = map #1 comps
                        val dataComps = map #2 comps
                    in ( sumComplexities typeComps
                        , sumComplexities dataComps
                        )
                    end
                | LabelRef i     => ( zeroComplexity, zeroComplexity ) (* ????? *)
           )
        end

    fun baseComplexity ( ts : LToken list ) : Complexity * Complexity =
    let val tok     = tokenOf (hd ts)
        val maxlen  = maxTokenLength ts
        val rmaxlen = Real.fromInt maxlen
    in ( case tok of
              PbXML (s1, s2)    => ( prob2Complexity probXMLChar
                                   , prob2Complexity ( power probXMLChar rmaxlen )
                                   )
            | PeXML (s1, s2)    => ( prob2Complexity probXMLChar
                                   , prob2Complexity ( power probXMLChar rmaxlen )
                                   )
            | Ptime s           =>
               let val timeProb = (1.0 / 60.0) * (1.0 / 60.0) * (1.0 / 24.0)
               in ( prob2Complexity timeProb, prob2Complexity timeProb )
               end
            | Pmonth s          => ( prob2Complexity ( 1.0 / 12.0 )
                                   , prob2Complexity ( 1.0 / 12.0 )
                                   )
            (* Assumes IP addresses are made up of 3 digit pieces
               Note: this is a case where type complexity differs
               from data complexity *)
            | Pip s             => ( prob2Complexity ( 1.0 / 100.0 )
                                   , prob2Complexity ( power ( 1.0 / 100.0 ) rmaxlen )
                                   )
            (* Assumes ints are digits 0 through 10 *)
            | Pint l            => ( prob2Complexity ( 1.0 / 10.0 )
                                   , prob2Complexity ( power ( 1.0 / 10.0 ) rmaxlen )
                                   )
            | Pstring s         => ( prob2Complexity probStringChar
                                   , prob2Complexity ( power probStringChar rmaxlen )
                                   )
            | Pgroup x          => ( zeroComplexity,  zeroComplexity )
            | Pwhite s          => ( prob2Complexity probWhiteChar
                                   , prob2Complexity ( power probWhiteChar rmaxlen )
                                   )
            | Other c           => ( prob2Complexity ( 1.0 / 256.0 )
                                   , prob2Complexity ( 1.0 / 256.0 )
                                   )
            | Pempty            => ( zeroComplexity,  zeroComplexity )
            | Error             => ( impossible, impossible )
       )
    end

    fun mkBaseComplexity ( a : AuxInfo ) ( ts : LToken list ) : Ty =
    let val ty      = Base (a, ts)
        val ( tcomp, dcomp ) = baseComplexity ts
        fun updateCompBase (ty:Ty) (t:Complexity) (d:Complexity) : Ty =
            Base ( updateComplexities a t d, ts )
    in updateCompBase ty tcomp dcomp
    end

    fun maxContextComplexity ( cl : Context list ) : Complexity * Complexity =
    let fun f (ltl:LToken list,comps:Complexity * Complexity ) : Complexity * Complexity =
        let val ( t1, d1 ) = baseComplexity ltl
            val ( t2, d2 ) = comps
        in ( combine t1 t2, combine d1 d2 )
        end
    in foldl f ( zeroComplexity, zeroComplexity ) cl
    end
    
    fun measure ( ty : Ty ) : Ty =
    ( case ty of
           Base ( a, ts )               => mkBaseComplexity a ts
         | TBD ( a, i, cl )             =>
             let val (t, d) = maxContextComplexity cl
             in TBD ( updateComplexities a t d, i, cl )
             end
         | Bottom ( a, i, cl )          =>
             let val (t, d) = maxContextComplexity cl
             in Bottom ( updateComplexities a t d, i, cl )
             end
         | Pstruct (a,tys)              =>
             Pstruct ( updateComplexities a (sumTypeComplexities tys)
                                            (sumDataComplexities tys)
                     , tys
                     )
           (* We will need information about the frequency of each branch
              of the union to do a better job here *)
         | Punion (a,tys)               =>
             let val l = length tys
             in Punion ( updateComplexities a
                           ( combine (Choices l) (sumTypeComplexities tys) )
                           ( combine (Choices l) (sumDataComplexities tys) )
                       , tys
                       )
             end
           (* Don't really want a complexity for a Parray, want to
              wait until we have a refined array (see below)
            *)
         | Parray ( a, x as { tokens  = ts
                            , lengths = ls
                            , first   = f
                            , body    = b
                            , last    = l
                            }
                  )                =>
             let val maxlen    = maxInt (map #1 ls)
                 val firstLastType = combine (getTypeComplexity f) (getTypeComplexity l)
                 val firstLastData = combine (getDataComplexity f) (getDataComplexity l)
                 val totalType = combine firstLastType
                                         ( multComp maxlen (getTypeComplexity b) )
                 val totalData = combine firstLastData
                                         ( multComp maxlen (getDataComplexity b) )
             in Parray ( updateComplexities a totalType totalData
                       , x
                       )
             end
         | RefinedBase ( a, r, ts ) =>
             let fun updateCompRefinedBase (ty:Ty) (t:Complexity) (d:Complexity) : Ty =
                     RefinedBase ( updateComplexities a t d, r, ts )
                 val maxlen = maxTokenLength ts
                 val ( typeComp, dataComp ) = refinedComp maxlen r
             in updateCompRefinedBase ty typeComp dataComp
             end
         | Switch ( a, id, bs)      =>
             let val switches         = map #1 bs
                 val branches         = map #2 bs
                 val switchComps      = map (refinedComp 1) switches
                 val switchTypeComps  = sumComplexities (map #1 switchComps)
                 val measuredBranches = map measure branches
                 val branchesTypeComp = sumTypeComplexities measuredBranches
                 val branchesDataComp = sumDataComplexities measuredBranches
             in Switch ( updateComplexities a zeroComplexity zeroComplexity
                       , id
                       , ListPair.zip ( switches, measuredBranches )
                       )
             end
         | RArray ( a, osep, oterm, body, olen ) =>
             let val rlen         = getLengthRArray ty
                 val measuredBody = measure body
                 val tbody        = getTypeComplexity measuredBody
                 val dbody        = getDataComplexity measuredBody
                 fun updateRArray (t:Complexity) (d:Complexity) =
                     RArray ( updateComplexities a t d, osep, oterm, measuredBody, olen )
                 val ( tlen, dlen ) = ( case olen of
                                             NONE     => ( zeroComplexity, zeroComplexity )
                                           | SOME len => refinedComp 1 len
                                      )
                 val ( tterm, dterm ) = ( case oterm of
                                               NONE      => ( zeroComplexity, zeroComplexity )
                                             | SOME term => refinedComp 1 term
                                        )
                 val ( tsep, dsep ) = ( case osep of
                                               NONE     => ( zeroComplexity, zeroComplexity )
                                             | SOME sep => refinedComp 1 sep
                                        )
                 val tcomp = sumComplexities [tbody, tlen, tterm, tsep]
                 val dcomp = sumComplexities [dbody, dlen, dterm, dsep]
             in updateRArray tcomp dcomp
             end
    )

end
