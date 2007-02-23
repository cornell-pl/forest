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

    (* Density function for a stream, based on the length of the
       string, and a uniform distribution over values
     *)
    fun stringDensity ( max : int ) : TokenDensity =
      let val rmax : real = Real.fromInt max
      in fn ( t : Token ) =>
            ( case t of
                    Pstring s      => power probStringChar rmax
                  | Pwhite s       => power probWhiteChar rmax
                  | PbXML (s1, s2) => raise XMLToken (* Fix this up ******)
                  | PeXML (s1, s2) => raise XMLToken (* Fix this up ******)
                    (* Assumes IP addresses are made up of 3 digit pieces *)
                  | Pip s          => power ( 1.0 / 100.0 ) rmax
                    (* Assumes ints are digits 0 through 10 *)
                  | Pint l         => power ( 1.0 / 10.0 ) rmax
                  | _              => raise BadToken
            )
      end

    val fixedWidthDensity : TokenDensity =
      fn ( t : Token ) =>
         ( case t of
                (* Somewhat brain-dead density for times, assuming
                   a ss:mm:hh format, and with each ss having probability
                   1/60, each mm having probability 1/60, and each hh
                   having probability 1/24. Does not deal with erroneous
                   times
                 *)
                 Ptime s  => ( 1.0 / 60.0 ) * ( 1.0 / 60.0 ) * ( 1.0 / 24.0 )
               | Pmonth s => 1.0 / 12.0
               | Other c  => 1.0 / 256.0
               | Pempty   => 1.0 (* ?????? *)
               | Error    => 0.0 (* ?????? *)
               | _        => raise BadToken
         )

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
                       let val timeProb = ( 1.0 / 60.0 ) * ( 1.0 / 60.0 ) * ( 1.0 / 24.0 )
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
                    (* Assumes IP addresses are made up of 3 digit pieces *)
                  | Pip s             =>
                       Base ( updateComplexities a
                                ( prob2Complexity ( 1.0 / 100.0 ) )
                                ( prob2Complexity ( power ( 1.0 / 100.0 ) rmaxlen ) )
                            , ts
                            )
                    (* Assumes ints are digits 0 through 10 *)
                  | Pint l            =>
                       let val intProb = power ( 1.0 / 10.0 ) rmaxlen
                           val intComp = prob2Complexity intProb
                       in Base ( updateComplexities a intComp intComp
                               , ts
                               )
                       end
                  | Pstring s         =>
                       let val strProb = power probStringChar rmaxlen
                           val strComp = prob2Complexity strProb
                       in Base ( updateComplexities a strComp strComp
                               , ts
                               )
                       end
                  | Pgroup x          => raise GoFigure
                  | Pwhite s          =>
                       let val whiteProb = power probWhiteChar rmaxlen
                           val whiteComp = prob2Complexity whiteProb
                       in Base ( updateComplexities a whiteComp whiteComp
                               , ts
                               )
                       end
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
         | Pstruct (a,tys)              => raise GoFigure
         | Punion (a,tys)               => raise GoFigure
         | Parray (a, _)                => raise GoFigure
         | RefinedBase (a,r,tl)         => raise GoFigure
         | Switch(a,id,branches)        => raise GoFigure
         | RArray (a,sep,term,body,len) => raise GoFigure
    )

end
