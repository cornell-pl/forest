(* Zach DeVito
	Example IRs for the test formats
*)
structure Desc = struct
	open Common
	(*Example data and descriptions*)
	val ls_desc = Array( Tuple [
								Sum [
									Tuple [
										Sum [
											Tuple [
												Base LettersBase, Base IntBase
												], 
												Base LettersBase
											], 
										Base (ConstBase "."), 
										Base LettersBase
									],
									Sum [
											Tuple [
												Base LettersBase, Base IntBase
											],
											Base LettersBase
										]
								],
								Base(ConstBase "\n")
							 ]
						)

	val ls = TextIO.getInstream( TextIO.openIn "lsd" )
(*	val ls_data = interp' ls_desc (ls) *)
	
	val test2_desc = Array( Tuple[ Sum [ Base LettersBase, Base IntBase], Sum [Base LettersBase, Base IntBase],Base (ConstBase "\n")] )	
(*	val test2_data = interp' test2_desc (TextIO.getInstream(TextIO.openIn "test2")) *)
	
	val	test1_desc = Array( 
						Tuple[ 
							Base IntBase,  
							Base (ConstBase " "), 
							Base IntBase, 
							Base (ConstBase " "),
							Base IntBase, 
							Base (ConstBase " "), 
							Base IntBase, 
							Base(ConstBase "\n") 
						] 
					)
	(*val _ = readTokens (TextIO.getInstream(TextIO.openIn "test1")) *)
(*	val test1_data = interp' test1_desc (TextIO.getInstream(TextIO.openIn "test1")) *)
	
(*	
	val Array(x) = test1_desc
	val ArrayD(y) = test1_data
	
	val Array(x2) = test2_desc
	val ArrayD(y2) = test2_data
	(*val moo2 = openAndInterp stuff "lsd"*)	
*)

	val test3_desc = Array( Tuple [ Base (ConstBase "("), Array(Tuple [Base IntBase, Base (ConstBase ",")]), Base (ConstBase ")"), Base (ConstBase "\n")] )
	
(*	val test3_data = interp' test3_desc (TextIO.getInstream(TextIO.openIn "test3")) *)
	
	val apache_desc = 
	Array(
		Tuple [
			Sum [
				Base (REBase "[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+"),
				Base (REBase "[a-zA-Z0-9\\.\\-]+")
			],
			Base (ConstBase " - "),
			Sum [
				Base (REBase "[a-z]+\\@[a-z]+\\.[a-z]+"),
				Base LettersBase,
				Base (ConstBase "-"),
				Base (ConstBase "\"\"")
			],
			Base (ConstBase " ["),
			Base IntBase,
			Base (ConstBase "/"),
			Base LettersBase,
			Base (ConstBase "/"),
			Base IntBase,
			Base (ConstBase ":"),
			Base IntBase,
			Base (ConstBase ":"),
			Base IntBase,
			Base (ConstBase ":"),
			Base IntBase,
			Base (ConstBase " -"),
			Base IntBase,
			Base (ConstBase "] \""),
			Base LettersBase,
			Base (ConstBase " "),
			Base (REBase "[a-zA-Z0-9/\\.@_\\-&\\*%=';:\\\\]+"),
			Base (ConstBase " HTTP/1."),
			Base IntBase,
			Base (ConstBase "\" "),
			Base IntBase,
			Base (ConstBase " "),
			Sum [
				Base IntBase,
				Base (ConstBase "-")
			],
			Base (ConstBase "\n")
		]
	)
(*	val apache_data = interp' apache_desc (TextIO.getInstream(TextIO.openIn "apache")) *)
	
	val tag_desc = Tuple [ 
					Sum [
						Base (ConstBase "</"),
						Base (ConstBase "<")
					],
					Sum [
						Base (ConstBase "key"),
						Base (ConstBase "string"),
						Base (ConstBase "real"),
						Base (ConstBase "data"),
						Base (ConstBase "integer")
					],
					Base (ConstBase ">")
				]
	val dict_nest = Tuple [ Base (ConstBase "\t<dict>\n"),
						Array ( Tuple [
									Base (ConstBase "\t\t"),
									tag_desc,
									Sum [
										Base (REBase "0\\.[0-9\\.]+"),
										Base IntBase,
										Base (REBase "\\n\\t[A-Z0-9a-z\\n\\t=]+"),
										Base (REBase "[A-Z0-9a-z:;~\\[\\$\\^\\/\\- ]+"),
										Base (ConstBase "")
									],
									tag_desc,
									Base (ConstBase "\n")
								]
						),
						Base (ConstBase "\t</dict>\n")
					]
	val dict_desc = Tuple [ Base (ConstBase "<dict>\n"),
						Array ( 
							Sum [
								Tuple [
									Base (ConstBase "\t"),
									tag_desc,
									Sum [
										Array(Base (REBase "[0-9\\.]+ ")),
										Base (REBase "0\\.[0-9\\.]+"),
										Base IntBase,
										Base (REBase "\\n\\t[A-Z0-9a-z\\n\\t=]+"),
										Base (REBase "[A-Z0-9a-z:;\\~\\[\\$\\^\\/\\- ]+"),
										Base (ConstBase "")
									],
									tag_desc,
									Base (ConstBase "\n")
								],
								dict_nest
							]
						),
						Base (ConstBase "</dict>\n")
					]
		
	val plist_desc = Tuple [ 
					Base (ConstBase "<plist version=\"1.0\">\n"),
					dict_desc,
					Base (ConstBase "</plist>\n")
				]
	val NL = Base (ConstBase "\n")
	val SP = Base (ConstBase " ")
	val eg = 
	  Tuple [
	    Base IntBase,
	    NL,
	    Array(
	      Tuple [
	        Base LettersBase,
	        Base (ConstBase "."),
	        SP,
	        Base LettersBase,
	        SP,
	        Base LettersBase,
	        SP,
	        Base IntBase,
	        SP,
	        Base LettersBase,
	        SP,
	        Array(
	          Tuple [
	            Base (ConstBase "{"),
	            Base IntBase,
	            SP,
	            Base IntBase,
	            SP,
	            Base IntBase,
	            SP,
	            Sum [
	              Base IntBase,
	              Base (ConstBase "")
	            ],
	            Base (ConstBase "}")
	          ]
	        ),
	        NL
	      ]
	    )
	  ]
	
(*	val plist_data = interp' plist_desc (TextIO.getInstream(TextIO.openIn "com.apple.Terminal.xml")) *)
end