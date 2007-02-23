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

    fun measure ( ty : Ty ) : Ty =
    ( case ty of
           Base (a, ts)               =>
             let val tok     = tokenOf (hd ts)
                 val maxlen  = maxTokenLength ts
                 val rmaxlen = Real.fromInt maxlen
             in ( case tok of
                    PbXML (s1, s2)    => raise GoFigure
                  | PeXML (s1, s2)    => raise GoFigure
                  | Ptime s           =>
                       let val timeProb = (1.0 / 60.0) * (1.0 / 60.0) * (1.0 / 24.0)
                           val timeComp = prob2Complexity timeProb
                       in Base ( updateComplexities a timeComp timeComp
                               , ts
                               )
                       end
                  | Pmonth s          =>
                       let val monthProb = 1.0 / 12.0
                           val monthComp = prob2Complexity monthProb
                       in Base ( updateComplexities a monthComp monthComp
                               , ts
                               )
                       end
                  (* Assumes IP addresses are made up of 3 digit pieces
                     Note: this is a case where type complexity differs
                     from data complexity
                   *)
                  | Pip s             =>
                       Base ( updateComplexities a
                                ( prob2Complexity ( 1.0 / 100.0 ) )
                                ( prob2Complexity ( power ( 1.0 / 100.0 ) rmaxlen ) )
                            , ts
                            )
                  (* Assumes ints are digits 0 through 10 *)
                  | Pint l            =>
                       Base ( updateComplexities a
                                ( prob2Complexity ( 1.0 / 10.0 ) )
                                ( prob2Complexity ( power ( 1.0 / 10.0 ) rmaxlen ) )
                            , ts
                            )
                  | Pstring s         =>
                       Base ( updateComplexities a
                                ( prob2Complexity probStringChar )
                                ( prob2Complexity ( power probStringChar rmaxlen ) )
                            , ts
                            )
                  | Pgroup x          => raise GoFigure
                  | Pwhite s          =>
                       Base ( updateComplexities a
                                ( prob2Complexity probWhiteChar )
                                ( prob2Complexity ( power probWhiteChar rmaxlen ) )
                            , ts
                            )
                  | Other c           =>
                       let val otherProb = 1.0 / 256.0
                           val otherComp = prob2Complexity otherProb
                       in Base ( updateComplexities a otherComp otherComp
                               , ts
                               )
                       end
                  | Pempty            =>
                       Base ( updateComplexities a zeroComplexity zeroComplexity
                            , ts
                            )
                  | Error             =>
                       Base ( updateComplexities a impossible impossible
                            , ts
                            )
                )
             end
         | TBD (a,i,cl)                 => raise GoFigure
         | Bottom (a,i,cl)              => raise GoFigure
           (* How to get entropy of data with respect to model? *)
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
         | RefinedBase (a,r,tl)         => raise GoFigure
         | Switch(a,id,branches)        => raise GoFigure
         | RArray (a,sep,term,body,len) => raise GoFigure
    )

end
