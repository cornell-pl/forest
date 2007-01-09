(* Zach DeVito
   interp.sml
   
   A simple interpretor for parsing formats described by an IR
   
*)
structure Interp = struct
	structure SIO = TextIO.StreamIO
	structure RE = RegExpFn(
						structure P = AwkSyntax
						structure E = BackTrackEngine
						)
						
	(* data types are in ir.sml *)
	open Common
	
	exception PARSE_ERROR (* raised only when a bug causes the parser to get into 
							an unexpected state *)
	exception ParseFailure (* Raised and handled during parsing to back up on failure *)
	
	
	(* reads a token from an input stream, using basedata_generation's mapping from
	regex to base data type *)
	fun readToken basedata_generation = 
	let 
	  fun getString operation mt = case MatchTree.root mt of 
						SOME{pos,len} => let val (a,strm) = (SIO.inputN(pos,len))
											in operation a
										end
					  | NONE => raise PARSE_ERROR
	  val basedata_generation' = map (fn (x,y) => (x,getString y)) basedata_generation
	in
	  RE.match basedata_generation' SIO.input1
	end
		
	
	(* debugging function, change to true to get messages *)
	fun printdb x = if Options.print_interp_debug then print x else ()
	(* debugging function, prints the whole stream of tokens *)
(*	fun readTokens istream = case readToken istream of
			SOME (x,istream') => (print (bdtos x ^ "\n"); readTokens istream')
		|	NONE => print "\n" *)
	(* previews the next 50 characters *)
	fun preview istream = printdb (#1(SIO.inputN(istream,50)))
	
	(* tries to read base type b from stream; raises ParseFailure if it cannot *)
	fun readTokenForBase b stream =
	let
		val gen_func = 
		case b of
		             (* regex format, function to create base from string x *)
		  IntBase => [("-?[0-9]+",    	fn x => Int(x,some(Int.fromString x)) )]
		| ConstBase c =>[(escapeRegex(c),	fn x => Const x)]
		| LettersBase =>  [("[A-Za-z]+", 	fn x => Letters x )]
		| REBase s   => [(s, fn x => RegEx x)]
		
		(* don't try to read a nil base *)
		val tok = case b of 
				  ConstBase "" => SOME(Const(""),stream)
				| _ => readToken gen_func stream
	in
		case tok of 
		  SOME(bd,istream') => (printdb ("read:" ^ bdtos bd ^ "\n"); (bd, istream'))
		| NONE => ((printdb  "didn't read\n");preview stream; raise ParseFailure)
	end
	
	
	(*use the internal representation ir to interpret istream, returns the data it parsed
	and the pointer to the remaining istream data *)
	fun interp ir istream = (printdb ("INTERP: " ^ (irtos ir) ^"\n"); case ir of
		Base b =>
			let
				val (ird,str') = (readTokenForBase b istream)
			in
				(BaseD(ird),str')
			end
	|	Tuple irlist => 
			let 
				val (lst,str') = foldl 
				  (fn (item,(res,str)) => 
				    let 
				      val (ir,str') = interp item str 
				    in 
				      (res @ [ir],str') 
				    end ) 
				  (nil,istream) irlist
			in
				(TupleD lst,str')
			end
	|	Sum irlist => 
			let 
				fun sum irl n = case irl of
					(h :: t) => ((interp h istream,n)
								 handle ParseFailure => sum t (n+1) )
				|	nil => raise ParseFailure
				val ((ir, str'),n) = (sum irlist 0)
			in
				(SumD(ir,n), str')
			end
	|	Array ir =>
			let (* try to parse ir until we fail *)
				fun arr str = 
					let val (ret, str') = interp ir str
					
						val (lst, str'') = arr str' handle ParseFailure => (nil,str')
						(*val _ = printdb ( (irdatatos ret) ^ " " ^  Bool.toString (lst = nil) ^ "\n" ) *)
					in
						(ret :: lst, str'')
					end
					handle ParseFailure => ((nil,str))
				val (list, str') = arr istream
				val _ = printdb (Int.toString (length list))	
			in
				(* if we parsed no items then we couldn't parse the array *)
				if length list = 0 then raise ParseFailure
				else (ArrayD list, str')
			end
	|  Label(l,ir') => interp ir' istream)
	(* call interp but check that it gets the whole stream; report where it
	stopped on error *)
	fun interp' ir istream =
	let
		val (ret,istream') = interp ir istream
		val _ = case (SIO.input1 istream') of 
		  SOME (x,_) => (print (#1(SIO.inputN(istream',50))); raise ParseFailure) 
		| NONE => ()
	(*	val _ = print (irdatatos ret) *)
	in
		ret
	end
	(* reads fileName with ir *)
	fun interpFile(ir, fileName) = interp' ir (TextIO.getInstream(TextIO.openIn fileName))
end