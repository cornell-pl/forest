%name PxmlLex;

%states INITIAL CDATA LIT;

%let word = [a-zA-Z][a-zA-Z0-9_]*;
%let begintag = \<[^!/][^>]*[^/]\>;
%let endtag = \<\/{word}\>;
%let dtag = \<{word}\/\>;
(* %let literal = \<char\>[^<]+\<\/char\> | \<string\>[^<]+\<\/string\>; *)
%let cddata = \"[^\"]*\" | \'[^']*\' | [^<> \t\r\n]+;
%let white = [ \t\r\n]+;
%let begincdata = \<!\[CDATA\[;
%let endcdata = \]\]\>;
(* %let beginlit = \<char\> | \<string\>;
%let endlit = \<\/char\> | \<\/string\>;
*)

%defs (
structure T = PxmlTokens
type lex_result = T.token
fun eof() = T.EOF
exception UnknownToken
);

<INITIAL> {begincdata} => ( YYBEGIN (CDATA); continue() );
<CDATA>   {endcdata}   => ( YYBEGIN (INITIAL); continue() );
<CDATA>   .    => ( T.CData (yytext ) );
(*     
<INITIAL> {beginlit} => ( YYBEGIN (LIT); 
			    T.BeginTag (String.substring(yytext, 1, size(yytext)-2)) );
<LIT>     {endlit}   => ( YYBEGIN (INITIAL);
			    T.EndTag (String.substring(yytext, 2, size(yytext)-3)) );
<LIT>	  .    => ( T.CData (yytext) );
*)
<INITIAL> {begintag}   => (
		let val s = (String.substring(yytext, 1, size(yytext)-2))
		    val newsub = Substring.takel 
		        (fn c => c <> #" ") (Substring.full s)
		    val news = Substring.string newsub
		in (T.BeginTag news) 
		end
	      );
<INITIAL> {endtag}     => ( T.EndTag (String.substring(yytext, 2, size(yytext)-3)) );
<INITIAL> {dtag}       => ( T.DTag (String.substring (yytext, 1, size(yytext)-3)) );
(*
<INITIAL> {literal}    => ( let 
		      val sub = Substring.full yytext
		      val dl = Substring.dropl (fn c => c <> #">") sub
		      val dr = Substring.string 
			(Substring.dropr (fn c => c <> #"<") dl)
		      val s = String.substring (dr, 1, size(dr) - 2)
		      (* val _ = print s *)
			
		  in
		    T.Data (s)
		  end
		);
*)
<INITIAL> {cddata}     => ( T.Data (yytext) );
<INITIAL> {white}      => ( continue() );
<INITIAL> .          =>  ( print ("error char >>" ^ yytext ^ "<< (" ^ Int.toString (!yylineno) ^
		", " ^ Int.toString yypos ^ ")\n"); raise UnknownToken );
