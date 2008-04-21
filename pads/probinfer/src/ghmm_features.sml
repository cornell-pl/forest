structure Ghmm_features = 
struct
  open Basetokens

   structure FeatureMap = RedBlackMapFn(
     struct 
       type ord_key = string
	   val compare = String.compare
	 end
   ) 

  fun token_length s = Real.fromInt(String.size s)

  fun punc_num s punc = 
    let
      fun is_punc (c, i) = if Char.compare(c, punc)=EQUAL then i+1 else i 
      val num = List.foldl is_punc 0 (String.explode s)
    in
      Real.fromInt num
    end

  fun dot_num s = punc_num s #"." 

  fun slash_num s = punc_num s #"/"
  
  fun bslash_num s = punc_num s #"\\"

  fun semicolon_num s = punc_num s #";"

  fun bar_num s = punc_num s #"|"

  fun less_num s = punc_num s #"<"

  fun tilde_num s = punc_num s #"~"

  fun bquote_num s = punc_num s #"`"
  
  fun bang_num s = punc_num s #"!"

  fun at_num s = punc_num s #"@"

  fun hash_num s = punc_num s #"#"

  fun dollar_num s = punc_num s #"$"

  fun percent_num s = punc_num s #"%"

  fun caret_num s = punc_num s #"^" 

  fun and_num s = punc_num s #"&"

  fun star_num s = punc_num s #"*" 

  fun lpar_num s = punc_num s #"("

  fun rpar_num s = punc_num s #")"

  fun hyphen_num s = punc_num s #"-"

  fun underscore_num s = punc_num s #"_" 

  fun add_num s = punc_num s #"+" 

  fun equal_num s = punc_num s #"="

  fun lbrac_num s = punc_num s #"{"

  fun rbrac_num s = punc_num s #"}"

  fun lsqubrac_num s = punc_num s #"["

  fun rsqubrac_num s = punc_num s #"]"

  fun colon_num s = punc_num s #":"

  fun dquote_num s = punc_num s #"\""

  fun quote_num s = punc_num s #"'"

  fun greater_num s = punc_num s #">"

  fun comma_num s = punc_num s #","

  fun question_num s = punc_num s #"?" 

  fun white_num s = punc_num s #" "

  fun tab_num s = punc_num s #"\t"

  fun isdigit c = if Char.isDigit c then 1.0 else 0.0
  fun isalpha c = if Char.isAlpha c then 1.0 else 0.0
  fun ispunct c = if Char.isPunct c then 1.0 else 0.0

  fun first_digit s = if String.size s = 0 then 0.0 else isdigit (String.sub(s, 0))

  fun first_alpha s = if String.size s = 0 then 0.0 else isalpha (String.sub(s, 0))

  fun first_punct s = if String.size s = 0 then 0.0 else ispunct (String.sub(s, 0))

  fun first_quote s = if String.size s = 0 then 0.0 else 
    let
      fun isquote c = if Char.compare(c, #"\"")=EQUAL then 1.0 else 0.0
    in
      isquote (String.sub(s, 0))
    end

  fun first_colon s = if String.size s = 0 then 0.0 else 
    let
      fun iscolon c = if Char.compare(c, #":")=EQUAL then 1.0 else 0.0
    in
      iscolon (String.sub(s, 0))
    end

  fun first_slash s = if String.size s = 0 then 0.0 else 
    let
      fun isslash c = if Char.compare(c, #"/")=EQUAL then 1.0 else 0.0
    in
      isslash (String.sub(s, 0))
    end

  fun first_bslash s = if String.size s = 0 then 0.0 else 
    let
      fun isbslash c = if Char.compare(c, #"\\")=EQUAL then 1.0 else 0.0
    in
      isbslash (String.sub(s, 0))
    end

  fun first_http s = if String.size s < 7 then 0.0 
                     else if String.compare("http://", String.substring(s, 0, 7))=EQUAL then 1.0 else 0.0

  fun last_digit s = if String.size s = 0 then 0.0 else isdigit (String.sub(s, (String.size s)-1))

  fun last_alpha s = if String.size s = 0 then 0.0 else isalpha (String.sub(s, (String.size s)-1))

  fun last_punct s = if String.size s = 0 then 0.0 else ispunct (String.sub(s, (String.size s)-1))

  fun only_hex s = if List.all Char.isHexDigit (String.explode s) then 1.0 else 0.0

  fun num_digit s = 
    let
      fun is_digit (c, i) = if Char.isDigit c then i+1 else i 
      val num = List.foldl is_digit 0 (String.explode s)
    in
      Real.fromInt num
    end

  fun num_alpha s = 
    let
      fun is_alpha (c, i) = if Char.isAlpha c then i+1 else i 
      val num = List.foldl is_alpha 0 (String.explode s)
    in
      Real.fromInt num
    end

  fun num_punc s = 
    let
      fun is_punc (c, i) = if Char.isPunct c then i+1 else i 
      val num = List.foldl is_punc 0 (String.explode s)
    in
      Real.fromInt num
    end

  val featureList = [
    ("token_length", token_length),
    ("dot_num", dot_num), 
    ("slash_num", slash_num),
    ("bslash_num", bslash_num),
    ("semicolon_num", semicolon_num),
    ("bar_num", bar_num),
    ("less_num", less_num),
    ("tilde_num", tilde_num),
    ("bquote_num", bquote_num),
    ("bang_num", bang_num),
    ("at_num", at_num),
    ("hash_num", hash_num),
    ("dollar_num", dollar_num),
    ("percent_num", percent_num),
    ("caret_num", caret_num),
    ("and_num", add_num),
    ("star_num", star_num),
    ("lpar_num", lpar_num),
    ("rpar_num", rpar_num),
    ("hyphen_num", hyphen_num),
    ("underscore_num", underscore_num),
    ("add_num", add_num),
    ("equal_num", equal_num),
    ("lbrac_num", lbrac_num),
    ("rbrac_num", rbrac_num),
    ("lsqubrac_num", lsqubrac_num),
    ("rsqubrac_num", rsqubrac_num),
    ("colon_num", colon_num),
    ("dquote_num", dquote_num),
    ("quote_num", quote_num),
    ("greater_num", greater_num),
    ("comma_num", comma_num),
    ("question_num", question_num),
    ("white_num", white_num),
    ("tab_num", tab_num),
    ("first_digit", first_digit),
    ("first_alpha", first_alpha),
    ("first_punct", first_punct),
    ("first_quote", first_quote),
    ("first_colon", first_colon),
    ("first_slash", first_slash),
    ("first_bslash", first_bslash),
    ("first_http", first_http),
    ("last_digit", last_digit),
    ("last_alpha", last_alpha),
    ("only_hex", only_hex),
    ("num_digit", num_digit),
    ("num_alpha", num_alpha),
    ("num_punc", num_punc)
  ]

  fun is_first s sbegin send =  if sbegin = 0 then 1.0 else 0.0

  fun is_last s sbegin send =  if send = ~1 then 0.0 else if send = (String.size s)-1 then 1.0 else 0.0

  fun pre_digit s sbegin send = if sbegin>0 then isdigit(List.nth((String.explode s), sbegin-1))
                                else 0.0   

  fun pre_alpha s sbegin send = if sbegin>0 then isalpha(List.nth((String.explode s), sbegin-1))
                                else 0.0   

  fun pre_punct s sbegin send = if sbegin>0 then ispunct(List.nth((String.explode s), sbegin-1))
                                else 0.0   

  fun next_digit s sbegin send = if send < (String.size s)-1 then isdigit(List.nth((String.explode s), send+1))
                                 else 0.0   

  fun next_alpha s sbegin send = if send < (String.size s)-1 then isalpha(List.nth((String.explode s), send+1))
                                 else 0.0   

  fun next_punct s sbegin send = if send < (String.size s)-1 then ispunct(List.nth((String.explode s), send+1))
                                 else 0.0   

  val otherFeatures = 
  [
    ("is_first", is_first),
    ("is_last", is_last),
    ("pre_digit", pre_digit),
    ("pre_alpha", pre_alpha),
    ("pre_punct", pre_punct),
    ("next_digit", next_digit),
    ("next_alpha", next_alpha),
    ("next_punct", next_punct)
  ]

  fun BSToken2FeatureStrs (b, str, sbegin, send) : string = 
    let
(*val _ = print (str^" "^(Int.toString sbegin)^" "^(Int.toString send)^"\n")*)
      val s = if sbegin = ~1 orelse send = ~1 then "" else String.substring(str, sbegin, send-sbegin+1)
(*val _ = print (s^"\n")*)
      fun mymap1 ((fname, f), rets) = (rets^" "^fname^" "^Real.toString(f s))
      fun mymap2 ((fname, f), rets) = (rets^" "^fname^" "^Real.toString(f str sbegin send))
      val featureStr = (List.foldl mymap1 "" featureList)^" "^(List.foldl mymap2 "" otherFeatures)
    in
      Int.toString(BTokenCompleteEnum(b)-1)^"\t"^featureStr^"\n"
    end

  fun BSToken2Features (b, str, sbegin, send) : (string*real) list =
    let
      val s = if sbegin = ~1 orelse send = ~1 then "" else String.substring(str, sbegin, send-sbegin+1)
      fun mymap1 ((fname, f), rets) = rets@[(fname, (f s))]
      fun mymap2 ((fname, f), rets) = rets@[(fname, (f str sbegin send))]
      val features = (List.foldl mymap1 [] featureList)@(List.foldl mymap2 [] otherFeatures)
    in
      features
    end

  fun f2prob features tindex ghmmmodel = 
    let
      val bias = case FeatureMap.find(ghmmmodel, "**BIAS**") of
                     NONE => 0.0
                   | SOME b => List.nth(b, tindex)
      fun addOne ((fname, v), ret) = 
        let
          val this = case FeatureMap.find(ghmmmodel, fname) of
                         NONE => 0.0
                       | SOME b => List.nth(b, tindex)*v
        in
          this + ret
        end
      val prob = bias + (List.foldl addOne 0.0 features)
    in
      prob
    end

  fun tokenProb (b, str, sbegin, send, ghmmmodel) : real = 
    let
      val s = String.substring(str, sbegin, send-sbegin+1)
      val features = BSToken2Features (b, str, sbegin, send)
      val tindex = BTokenCompleteEnum(b)-1
      fun recFn i = if i = 0 then [f2prob features i ghmmmodel]
                    else recFn(i-1)@[f2prob features i ghmmmodel]
      val problist = recFn(BTokenCompleteEnum(PPempty)-1)
      val problist = List.map Math.exp problist
      fun addAll (p, mysum) = p+mysum
      val sum = List.foldl addAll 0.0 problist
      val prob = List.nth(problist, tindex)/sum
    in
      prob
    end

end
