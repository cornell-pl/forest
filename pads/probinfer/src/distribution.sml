structure Distribution = struct
    open Complexity

    (* A mapping having integers used as ordinals as a domain.
       This occurs in a Punion or an enumerated type. We use the
       ordinal (position in the list of variants) as the key of the
       map, and the range of the map will be the frquency with which
       that variant occurs in the observed data.
     *)
    structure OrdinalMap =  RedBlackMapFn ( struct type ord_key = int
                                                   val  compare = Int.compare
                                            end
                                          )
    (* Mapping ordinals to frequency *)
    type OrdinalFreq = int OrdinalMap.map
    val emptyOrdinalFreq : OrdinalFreq = OrdinalMap.empty
    fun addOrdinalFreq (n:int, f:OrdinalFreq):OrdinalFreq =
        ( case OrdinalMap.find (f,n) of
               NONE   => f
             | SOME m => OrdinalMap.insert (f,n,m+1)
        )

    (* Lurking in the background are some laws that a discrete
       density must obey:
       1. forall x. f(x) \geq 0
       2. {x : f(x) \neq 0} is at most countable. Let {x_i} be this set:
       3. \sigma_i f(x_i) = 1
     *)
    type 'a Density = 'a -> real

    (* When the density depends on another parameter (e.g. functional
       dependencies in PADS), we can often get a better estimate of entropy
     *)
    type ('a,'b) FDDensity = 'b -> 'a Density

    (* When the probability depends on the past history of values (e.g.
       when the next value is related to the previous value), we get
       another signature:
     *)
    type 'a HDDensity = 'a list -> 'a Density

    (* Finally, combining functional dependency with historical dependency *)
    type ('a, 'b) HDFDDensity = 'b -> 'a HDDensity

    datatype ('a, 'b) Densities = Independent          of 'a Density
                                | FunctionalDependence of ('a, 'b) FDDensity
                                | HistoricalDependence of 'a HDDensity
                                | FunctionalHistoricalDependence of ('a, 'b) HDFDDensity

    (* Now for some particular densities
     *
     * UniformStar: One model for a regular expression with a star
     *              is to use the max length of the observed data
     *              to give a finite sample space. For now, the star
     *              is over ASCII characters, all 256 values. We will
     *              refine this in the future.
     * FixedWidth:  Modeling a data type such as an integer or a date
     *              format, with a uniform distribution. Very similar
     *              to uniform star.
     * Frequency:   Use the frequency of the observed data of a (usually
     *              small sample space. The map from ordinal to frequency
     *              is represented as a list of integers
     *)

    datatype DataModel = NoModel                 (* Null value, temp only *)
                       | UniformStar of int      (* the maximum length *)
                       | FixedWidth  of int      (* the fixed width    *)
                       | Frequency   of int list (* Frequency per ordinal *)

    (* Styles of distribution

       1. ReStar: For a regular expression with a star (no limit to the
                  length, e.g string). For now we will use the max length
                  of the observed data to reduce this to case 2.
       2. ReFW:   For a fixed width regular expression (e.g. int or date)
       3. Bucket: For an enumeration, with values lumped into buckets
     *)

    datatype Distribution = ReStar of int (* max length of data *)
                          | ReFW   of int (* fixed width of data *)
                          | Bucket of OrdinalFreq

end (* structure Distribution *)
