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

  fun token_length_5 s = if (String.size s)<=5 then 1.0 else 0.0

  fun token_length_10 s = 
    let
      val l = String.size s
    in
      if l>5 andalso l<=10 then 1.0 else 0.0
    end

  fun token_length_20 s = 
    let
      val l = String.size s
    in
      if l>10 andalso l<=20 then 1.0 else 0.0
    end

  fun token_length_30 s = 
    let
      val l = String.size s
    in
      if l>20 then 1.0 else 0.0
    end

  fun punc_num s punc = 
    let
      fun is_punc (c, i) = if Char.compare(c, punc)=EQUAL then i+1 else i 
      val num = List.foldl is_punc 0 (String.explode s)
    in
      Real.fromInt num
    end

  fun dot_num s = punc_num s #"." 

  fun punc_num_int s punc = 
    let
      fun is_punc (c, i) = if Char.compare(c, punc)=EQUAL then i+1 else i 
      val num = List.foldl is_punc 0 (String.explode s)
    in
      num
    end

  fun punc_num_no s punc = 
    let 
      val n = punc_num_int s punc
    in
      if n=0 then 1.0 else 0.0
    end

  fun punc_num_1 s punc = 
    let 
      val n = punc_num_int s punc
    in
      if n=1 then 1.0 else 0.0
    end

  fun punc_num_2 s punc = 
    let 
      val n = punc_num_int s punc
    in
      if n=2 then 1.0 else 0.0
    end

  fun punc_num_3 s punc = 
    let 
      val n = punc_num_int s punc
    in
      if n=3 then 1.0 else 0.0
    end

  fun punc_num_5 s punc = 
    let 
      val n = punc_num_int s punc
    in
      if n>1 andalso n<=5 then 1.0 else 0.0
    end

  fun punc_num_5_2 s punc = 
    let 
      val n = punc_num_int s punc
    in
      if n>3 andalso n<=5 then 1.0 else 0.0
    end

  fun punc_num_10 s punc = 
    let 
      val n = punc_num_int s punc
    in
      if n>5 andalso n<=10 then 1.0 else 0.0
    end

  fun punc_num_20 s punc = 
    let 
      val n = punc_num_int s punc
    in
      if n>10 then 1.0 else 0.0
    end

  fun dot_num_no s = punc_num_no s #"."
  fun dot_num_1 s = punc_num_1 s #"."
  fun dot_num_2 s = punc_num_2 s #"."
  fun dot_num_3 s = punc_num_3 s #"."
  fun dot_num_5 s = punc_num_5_2 s #"."
  fun dot_num_10 s = punc_num_10 s #"."
  fun dot_num_20 s = punc_num_20 s #"."

  fun slash_num s = punc_num s #"/"
  fun slash_num_no s = punc_num_no s #"/"
  fun slash_num_1 s = punc_num_1 s #"/"
  fun slash_num_5 s = punc_num_5 s #"/"
  fun slash_num_10 s = punc_num_10 s #"/"
  fun slash_num_20 s = punc_num_20 s #"/"
  
  fun bslash_num s = punc_num s #"\\"
  fun bslash_num_no s = punc_num_no s #"\\"
  fun bslash_num_1 s = punc_num_1 s #"\\"
  fun bslash_num_5 s = punc_num_5 s #"\\"
  fun bslash_num_10 s = punc_num_10 s #"\\"
  fun bslash_num_20 s = punc_num_20 s #"\\"

  fun semicolon_num s = punc_num s #";"
  fun semicolon_num_no s = punc_num_no s #";"
  fun semicolon_num_1 s = punc_num_1 s #";"
  fun semicolon_num_2 s = punc_num_2 s #";"
  fun semicolon_num_3 s = punc_num_3 s #";"
  fun semicolon_num_5 s = punc_num_5_2 s #";"
  fun semicolon_num_10 s = punc_num_10 s #";"
  fun semicolon_num_20 s = punc_num_20 s #";"

  fun bar_num s = punc_num s #"|"
  fun bar_num_no s = punc_num_no s #"|"
  fun bar_num_1 s = punc_num_1 s #"|"
  fun bar_num_5 s = punc_num_5 s #"|"
  fun bar_num_10 s = punc_num_10 s #"|"
  fun bar_num_20 s = punc_num_20 s #"|"

  fun less_num s = punc_num s #"<"
  fun less_num_no s = punc_num_no s #"<"
  fun less_num_1 s = punc_num_1 s #"<"
  fun less_num_5 s = punc_num_5 s #"<"
  fun less_num_10 s = punc_num_10 s #"<"
  fun less_num_20 s = punc_num_20 s #"<"

  fun tilde_num s = punc_num s #"~"
  fun tilde_num_no s = punc_num_no s #"~"
  fun tilde_num_1 s = punc_num_1 s #"~"
  fun tilde_num_5 s = punc_num_5 s #"~"
  fun tilde_num_10 s = punc_num_10 s #"~"
  fun tilde_num_20 s = punc_num_20 s #"~"

  fun bquote_num s = punc_num s #"`"
  fun bquote_num_no s = punc_num_no s #"`"
  fun bquote_num_1 s = punc_num_1 s #"`"
  fun bquote_num_5 s = punc_num_5 s #"`"
  fun bquote_num_10 s = punc_num_10 s #"`"
  fun bquote_num_20 s = punc_num_20 s #"`"
  
  fun bang_num s = punc_num s #"!"
  fun bang_num_no s = punc_num_no s #"!"
  fun bang_num_1 s = punc_num_1 s #"!"
  fun bang_num_5 s = punc_num_5 s #"!"
  fun bang_num_10 s = punc_num_10 s #"!"
  fun bang_num_20 s = punc_num_20 s #"!"

  fun at_num s = punc_num s #"@"
  fun at_num_no s = punc_num_no s #"@"
  fun at_num_1 s = punc_num_1 s #"@"
  fun at_num_5 s = punc_num_5 s #"@"
  fun at_num_10 s = punc_num_10 s #"@"
  fun at_num_20 s = punc_num_20 s #"@"

  fun hash_num s = punc_num s #"#"
  fun hash_num_no s = punc_num_no s #"#"
  fun hash_num_1 s = punc_num_1 s #"#"
  fun hash_num_5 s = punc_num_5 s #"#"
  fun hash_num_10 s = punc_num_10 s #"#"
  fun hash_num_20 s = punc_num_20 s #"#"

  fun dollar_num s = punc_num s #"$"
  fun dollar_num_no s = punc_num_no s #"$"
  fun dollar_num_1 s = punc_num_1 s #"$"
  fun dollar_num_5 s = punc_num_5 s #"$"
  fun dollar_num_10 s = punc_num_10 s #"$"
  fun dollar_num_20 s = punc_num_20 s #"$"

  fun percent_num s = punc_num s #"%"
  fun percent_num_no s = punc_num_no s #"%"
  fun percent_num_1 s = punc_num_1 s #"%"
  fun percent_num_5 s = punc_num_5 s #"%"
  fun percent_num_10 s = punc_num_10 s #"%"
  fun percent_num_20 s = punc_num_20 s #"%"

  fun caret_num s = punc_num s #"^" 
  fun caret_num_no s = punc_num_no s #"^"
  fun caret_num_1 s = punc_num_1 s #"^"
  fun caret_num_5 s = punc_num_5 s #"^"
  fun caret_num_10 s = punc_num_10 s #"^"
  fun caret_num_20 s = punc_num_20 s #"^"

  fun and_num s = punc_num s #"&"
  fun and_num_no s = punc_num_no s #"&"
  fun and_num_1 s = punc_num_1 s #"&"
  fun and_num_5 s = punc_num_5 s #"&"
  fun and_num_10 s = punc_num_10 s #"&"
  fun and_num_20 s = punc_num_20 s #"&"

  fun star_num s = punc_num s #"*" 
  fun star_num_no s = punc_num_no s #"*"
  fun star_num_1 s = punc_num_1 s #"*"
  fun star_num_5 s = punc_num_5 s #"*"
  fun star_num_10 s = punc_num_10 s #"*"
  fun star_num_20 s = punc_num_20 s #"*"

  fun lpar_num s = punc_num s #"("
  fun lpar_num_no s = punc_num_no s #"("
  fun lpar_num_1 s = punc_num_1 s #"("
  fun lpar_num_5 s = punc_num_5 s #"("
  fun lpar_num_10 s = punc_num_10 s #"("
  fun lpar_num_20 s = punc_num_20 s #"("

  fun rpar_num s = punc_num s #")"
  fun rpar_num_no s = punc_num_no s #")"
  fun rpar_num_1 s = punc_num_1 s #")"
  fun rpar_num_5 s = punc_num_5 s #")"
  fun rpar_num_10 s = punc_num_10 s #")"
  fun rpar_num_20 s = punc_num_20 s #")"

  fun hyphen_num s = punc_num s #"-"
  fun hyphen_num_no s = punc_num_no s #"-"
  fun hyphen_num_1 s = punc_num_1 s #"-"
  fun hyphen_num_2 s = punc_num_2 s #"-"
  fun hyphen_num_3 s = punc_num_3 s #"-"
  fun hyphen_num_5 s = punc_num_5_2 s #"-"
  fun hyphen_num_10 s = punc_num_10 s #"-"
  fun hyphen_num_20 s = punc_num_20 s #"-"

  fun underscore_num s = punc_num s #"_" 
  fun underscore_num_no s = punc_num_no s #"_"
  fun underscore_num_1 s = punc_num_1 s #"_"
  fun underscore_num_2 s = punc_num_2 s #"_"
  fun underscore_num_3 s = punc_num_3 s #"_"
  fun underscore_num_5 s = punc_num_5_2 s #"_"
  fun underscore_num_10 s = punc_num_10 s #"_"
  fun underscore_num_20 s = punc_num_20 s #"_"

  fun add_num s = punc_num s #"+" 
  fun add_num_no s = punc_num_no s #"+"
  fun add_num_1 s = punc_num_1 s #"+"
  fun add_num_5 s = punc_num_5 s #"+"
  fun add_num_10 s = punc_num_10 s #"+"
  fun add_num_20 s = punc_num_20 s #"+"

  fun equal_num s = punc_num s #"="
  fun equal_num_no s = punc_num_no s #"="
  fun equal_num_1 s = punc_num_1 s #"="
  fun equal_num_5 s = punc_num_5 s #"="
  fun equal_num_10 s = punc_num_10 s #"="
  fun equal_num_20 s = punc_num_20 s #"="

  fun lbrac_num s = punc_num s #"{"
  fun lbrac_num_no s = punc_num_no s #"{"
  fun lbrac_num_1 s = punc_num_1 s #"{"
  fun lbrac_num_5 s = punc_num_5 s #"{"
  fun lbrac_num_10 s = punc_num_10 s #"{"
  fun lbrac_num_20 s = punc_num_20 s #"{"

  fun rbrac_num s = punc_num s #"}"
  fun rbrac_num_no s = punc_num_no s #"}"
  fun rbrac_num_1 s = punc_num_1 s #"}"
  fun rbrac_num_5 s = punc_num_5 s #"}"
  fun rbrac_num_10 s = punc_num_10 s #"}"
  fun rbrac_num_20 s = punc_num_20 s #"}"

  fun lsqubrac_num s = punc_num s #"["
  fun lsqubrac_num_no s = punc_num_no s #"["
  fun lsqubrac_num_1 s = punc_num_1 s #"["
  fun lsqubrac_num_5 s = punc_num_5 s #"["
  fun lsqubrac_num_10 s = punc_num_10 s #"["
  fun lsqubrac_num_20 s = punc_num_20 s #"["

  fun rsqubrac_num s = punc_num s #"]"
  fun rsqubrac_num_no s = punc_num_no s #"]"
  fun rsqubrac_num_1 s = punc_num_1 s #"]"
  fun rsqubrac_num_5 s = punc_num_5 s #"]"
  fun rsqubrac_num_10 s = punc_num_10 s #"]"
  fun rsqubrac_num_20 s = punc_num_20 s #"]"

  fun colon_num s = punc_num s #":"
  fun colon_num_no s = punc_num_no s #":"
  fun colon_num_1 s = punc_num_1 s #":"
  fun colon_num_2 s = punc_num_2 s #":"
  fun colon_num_3 s = punc_num_3 s #":"
  fun colon_num_5 s = punc_num_5_2 s #":"
  fun colon_num_10 s = punc_num_10 s #":"
  fun colon_num_20 s = punc_num_20 s #":"

  fun dquote_num s = punc_num s #"\""
  fun dquote_num_no s = punc_num_no s #"\""
  fun dquote_num_1 s = punc_num_1 s #"\""
  fun dquote_num_5 s = punc_num_5 s #"\""
  fun dquote_num_10 s = punc_num_10 s #"\""
  fun dquote_num_20 s = punc_num_20 s #"\""

  fun quote_num s = punc_num s #"'"
  fun quote_num_no s = punc_num_no s #"'"
  fun quote_num_1 s = punc_num_1 s #"'"
  fun quote_num_5 s = punc_num_5 s #"'"
  fun quote_num_10 s = punc_num_10 s #"'"
  fun quote_num_20 s = punc_num_20 s #"'"

  fun greater_num s = punc_num s #">"
  fun greater_num_no s = punc_num_no s #">"
  fun greater_num_1 s = punc_num_1 s #">"
  fun greater_num_5 s = punc_num_5 s #">"
  fun greater_num_10 s = punc_num_10 s #">"
  fun greater_num_20 s = punc_num_20 s #">"

  fun comma_num s = punc_num s #","
  fun comma_num_no s = punc_num_no s #","
  fun comma_num_1 s = punc_num_1 s #","
  fun comma_num_2 s = punc_num_2 s #","
  fun comma_num_3 s = punc_num_3 s #","
  fun comma_num_5 s = punc_num_5_2 s #","
  fun comma_num_10 s = punc_num_10 s #","
  fun comma_num_20 s = punc_num_20 s #","

  fun question_num s = punc_num s #"?" 
  fun question_num_no s = punc_num_no s #"?"
  fun question_num_1 s = punc_num_1 s #"?"
  fun question_num_5 s = punc_num_5 s #"?"
  fun question_num_10 s = punc_num_10 s #"?"
  fun question_num_20 s = punc_num_20 s #"?"

  fun white_num s = punc_num s #" "
  fun white_num_no s = punc_num_no s #" "
  fun white_num_1 s = punc_num_1 s #" "
  fun white_num_2 s = punc_num_2 s #" "
  fun white_num_3 s = punc_num_3 s #" "
  fun white_num_5 s = punc_num_5_2 s #" "
  fun white_num_10 s = punc_num_10 s #" "
  fun white_num_20 s = punc_num_20 s #" "

  fun tab_num s = punc_num s #"\t"
  fun tab_num_no s = punc_num_no s #"\t"
  fun tab_num_1 s = punc_num_1 s #"\t"
  fun tab_num_5 s = punc_num_5 s #"\t"
  fun tab_num_10 s = punc_num_10 s #"\t"
  fun tab_num_20 s = punc_num_20 s #"\t"

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

  fun num_digit_int s = 
    let
      fun is_digit (c, i) = if Char.isDigit c then i+1 else i 
      val num = List.foldl is_digit 0 (String.explode s)
    in
      num
    end

  fun num_digit_no s =
    let 
      val n = num_digit_int s
    in
      if n=0 then 1.0 else 0.0
    end

  fun num_digit_1 s =
    let 
      val n = num_digit_int s
    in
      if n=1 then 1.0 else 0.0
    end

  fun num_digit_2 s =
    let 
      val n = num_digit_int s
    in
      if n=2 then 1.0 else 0.0
    end

  fun num_digit_3 s =
    let 
      val n = num_digit_int s
    in
      if n=3 then 1.0 else 0.0
    end

  fun num_digit_5 s =
    let 
      val n = num_digit_int s
    in
      if n>3 andalso n<=5 then 1.0 else 0.0
    end

  fun num_digit_10 s =
    let 
      val n = num_digit_int s
    in
      if n>5 andalso n<=10 then 1.0 else 0.0
    end

  fun num_digit_20 s =
    let 
      val n = num_digit_int s
    in
      if n>10 then 1.0 else 0.0
    end

  fun num_alpha s = 
    let
      fun is_alpha (c, i) = if Char.isAlpha c then i+1 else i 
      val num = List.foldl is_alpha 0 (String.explode s)
    in
      Real.fromInt num
    end

  fun num_alpha_int s = 
    let
      fun is_alpha (c, i) = if Char.isAlpha c then i+1 else i 
      val num = List.foldl is_alpha 0 (String.explode s)
    in
      num
    end

  fun num_alpha_no s =
    let 
      val n = num_alpha_int s
    in
      if n=0 then 1.0 else 0.0
    end

  fun num_alpha_1 s =
    let 
      val n = num_alpha_int s
    in
      if n=1 then 1.0 else 0.0
    end

  fun num_alpha_2 s =
    let 
      val n = num_alpha_int s
    in
      if n=2 then 1.0 else 0.0
    end

  fun num_alpha_3 s =
    let 
      val n = num_alpha_int s
    in
      if n=3 then 1.0 else 0.0
    end

  fun num_alpha_5 s =
    let 
      val n = num_alpha_int s
    in
      if n>3 andalso n<=5 then 1.0 else 0.0
    end

  fun num_alpha_10 s =
    let 
      val n = num_alpha_int s
    in
      if n>5 andalso n<=10 then 1.0 else 0.0
    end

  fun num_alpha_20 s =
    let 
      val n = num_alpha_int s
    in
      if n>10 then 1.0 else 0.0
    end

  fun num_punc s = 
    let
      fun is_punc (c, i) = if Char.isPunct c then i+1 else i 
      val num = List.foldl is_punc 0 (String.explode s)
    in
      Real.fromInt num
    end

  fun num_punc_int s = 
    let
      fun is_punc (c, i) = if Char.isPunct c then i+1 else i 
      val num = List.foldl is_punc 0 (String.explode s)
    in
      num
    end

  fun num_punc_no s =
    let 
      val n = num_punc_int s
    in
      if n=0 then 1.0 else 0.0
    end

  fun num_punc_1 s =
    let 
      val n = num_punc_int s
    in
      if n=1 then 1.0 else 0.0
    end

  fun num_punc_2 s =
    let 
      val n = num_punc_int s
    in
      if n=2 then 1.0 else 0.0
    end

  fun num_punc_3 s =
    let 
      val n = num_punc_int s
    in
      if n=3 then 1.0 else 0.0
    end

  fun num_punc_5 s =
    let 
      val n = num_punc_int s
    in
      if n>3 andalso n<=5 then 1.0 else 0.0
    end

  fun num_punc_10 s =
    let 
      val n = num_punc_int s
    in
      if n>5 andalso n<=10 then 1.0 else 0.0
    end

  fun num_punc_20 s =
    let 
      val n = num_punc_int s
    in
      if n>10 then 1.0 else 0.0
    end

  fun has_ampm s = 
    if ( String.isSubstring "am" s orelse String.isSubstring "AM" s orelse String.isSubstring "pm" s orelse String.isSubstring "PM" s ) then 1.0 else 0.0  

  fun has_month s = 
    if ( String.isSubstring "Jan" s orelse 
         String.isSubstring "jan" s orelse 
         String.isSubstring "Feb" s orelse 
         String.isSubstring "feb" s orelse
         String.isSubstring "Mar" s orelse
         String.isSubstring "mar" s orelse
         String.isSubstring "Apr" s orelse
         String.isSubstring "apr" s orelse
         String.isSubstring "May" s orelse
         String.isSubstring "may" s orelse
         String.isSubstring "Jun" s orelse
         String.isSubstring "jun" s orelse
         String.isSubstring "Jul" s orelse
         String.isSubstring "jul" s orelse
         String.isSubstring "Aug" s orelse
         String.isSubstring "aug" s orelse
         String.isSubstring "Sep" s orelse
         String.isSubstring "sep" s orelse
         String.isSubstring "Oct" s orelse
         String.isSubstring "oct" s orelse
         String.isSubstring "Nov" s orelse
         String.isSubstring "nov" s orelse
         String.isSubstring "Dec" s orelse
         String.isSubstring "dec" s orelse
         String.isSubstring "January" s orelse
         String.isSubstring "February" s orelse
         String.isSubstring "March" s orelse
         String.isSubstring "April" s orelse
         String.isSubstring "May" s orelse
         String.isSubstring "June" s orelse
         String.isSubstring "July" s orelse
         String.isSubstring "August" s orelse
         String.isSubstring "September" s orelse
         String.isSubstring "October" s orelse
         String.isSubstring "November" s orelse
         String.isSubstring "December" s
       ) 
      then 1.0 else 0.0  

  fun has_year s = 
    if ( String.isSubstring "2000" s orelse 
         String.isSubstring "2001" s orelse 
         String.isSubstring "2002" s orelse 
         String.isSubstring "2003" s orelse
         String.isSubstring "2004" s orelse
         String.isSubstring "2005" s orelse
         String.isSubstring "2006" s orelse
         String.isSubstring "2007" s orelse
         String.isSubstring "2008" s orelse
         String.isSubstring "2009" s orelse
         String.isSubstring "2010" s orelse
         String.isSubstring "1999" s orelse
         String.isSubstring "1998" s orelse
         String.isSubstring "1997" s 
       )
      then 1.0 else 0.0

  fun has_date s =
    if ( String.isSubstring "Mon" s orelse
         String.isSubstring "Monday" s orelse
         String.isSubstring "Tue" s orelse
         String.isSubstring "Tuesday" s orelse
         String.isSubstring "Wed" s orelse
         String.isSubstring "Wednesday" s orelse
         String.isSubstring "Thu" s orelse
         String.isSubstring "Thursday" s orelse
         String.isSubstring "Fri" s orelse
         String.isSubstring "Friday" s orelse
         String.isSubstring "Sat" s orelse
         String.isSubstring "Saturday" s orelse
         String.isSubstring "Sun" s orelse
         String.isSubstring "Sunday" s orelse
         String.isSubstring "mon" s orelse
         String.isSubstring "tue" s orelse
         String.isSubstring "wed" s orelse
         String.isSubstring "thu" s orelse
         String.isSubstring "fri" s orelse
         String.isSubstring "sat" s orelse
         String.isSubstring "sun" s
       )
      then 1.0 else 0.0

  fun no_alpha s = 
    let 
      val has = List.exists Char.isAlpha (String.explode s)
    in
      if has then 0.0 else 1.0
    end

  fun no_colon s = 
    let 
      fun is_colon c = if Char.compare(c, #",")=EQUAL then true else false
      val has = List.exists is_colon (String.explode s)
    in
      if has then 0.0 else 1.0
    end

  fun all_digit s = 
    let 
      fun digit_dot c = Char.isDigit c orelse Char.compare(c, #".")=EQUAL orelse Char.compare(c, #"-")=EQUAL orelse Char.compare(c, #"~")=EQUAL
      val has = List.all digit_dot (String.explode s)
    in
      if has then 1.0 else 0.0
    end

  fun digit_all_below24 s =
    let
      fun collect_digit (c, (thisint, list)) = 
        if Char.isDigit c then (thisint@[c], list) 
        else 
          if List.length thisint = 0 then (thisint, list)
          else ([], list@[String.implode thisint])
      val (lastint, intlist) = List.foldl collect_digit ([], []) (String.explode s)
      val intlist = if List.length lastint = 0 then intlist else intlist@[String.implode lastint]
      fun str2int str = ((*print (str^" ");*) Option.valOf(Int.fromString str)) handle Overflow => 25 
      val intlist = List.map str2int intlist 
      fun check i = if i>=0 andalso i<25 then true else false handle Overflow => false
    in
      if (List.all check intlist) then 1.0 else 0.0
    end

  fun digit_all_below31 s =
    let
      fun collect_digit (c, (thisint, list)) = 
        if Char.isDigit c then (thisint@[c], list) 
        else 
          if List.length thisint = 0 then (thisint, list)
          else ([], list@[String.implode thisint])
      val (lastint, intlist) = List.foldl collect_digit ([], []) (String.explode s)
      val intlist = if List.length lastint = 0 then intlist else intlist@[String.implode lastint]
      fun str2int str = ((*print (str^" ");*) Option.valOf(Int.fromString str)) handle Overflow => 32 
      val intlist = List.map str2int intlist 
      fun check i = if i>=0 andalso i<32 then true else false handle Overflow => false
    in
      if (List.all check intlist) then 1.0 else 0.0
    end

  fun all_alpha_space s = 
    let 
      fun alpha_space c = Char.isAlpha c orelse Char.compare(c, #" ")=EQUAL
      val has = List.all alpha_space (String.explode s)
    in
      if has then 1.0 else 0.0
    end

  fun not_all_digit s = 
    let 
      fun digit_dot c = Char.isDigit c orelse Char.compare(c, #".")=EQUAL orelse Char.compare(c, #"-")=EQUAL orelse Char.compare(c, #"~")=EQUAL
      val has = List.all digit_dot (String.explode s)
    in
      if has then 0.0 else 1.0
    end

  fun has_hostsuffix s =
    if ( String.isSuffix "com" s orelse
         String.isSuffix "net" s orelse
         String.isSuffix "edu" s orelse
         String.isSuffix "org" s orelse
         String.isSuffix "us" s orelse
         String.isSuffix "cn" s orelse
         String.isSuffix "jp" s orelse
         String.isSuffix "fr" s
       ) then 1.0 else 0.0

  fun has_docname_suffix s =
    if ( String.isSuffix "rpm" s orelse
         String.isSuffix "doc" s orelse
         String.isSuffix "pdf" s orelse
         String.isSuffix "jpg" s orelse
         String.isSuffix "jpeg" s orelse
         String.isSuffix "txt" s orelse
         String.isSuffix "tar" s orelse
         String.isSuffix "zip" s orelse
         String.isSuffix "xls" s orelse
         String.isSuffix "ppt" s orelse
         String.isSuffix "omf" s orelse
         String.isSuffix "tex" s orelse
         String.isSuffix "eps" s orelse 
         String.isSuffix "div" s orelse
         String.isSuffix "rar" s
       ) then 1.0 else 0.0

  fun int_num_0 s =
    let
      fun collect_digit (c, (thisint, list)) = 
        if Char.isDigit c then (thisint@[c], list) 
        else 
          if List.length thisint = 0 then (thisint, list)
          else ([], list@[String.implode thisint])
      val (lastint, intlist) = List.foldl collect_digit ([], []) (String.explode s)
      val intlist = if List.length lastint = 0 then intlist else intlist@[String.implode lastint]
(*
      fun str2int str =  Option.valOf(Int.fromString str)) handle Overflow => 1 
      val intlist = List.map str2int intlist 
*)
      val intnum = List.length intlist
    in
      if intnum = 0 then 1.0 else 0.0
    end

  fun int_num_1 s =
    let
      fun collect_digit (c, (thisint, list)) = 
        if Char.isDigit c then (thisint@[c], list) 
        else 
          if List.length thisint = 0 then (thisint, list)
          else ([], list@[String.implode thisint])
      val (lastint, intlist) = List.foldl collect_digit ([], []) (String.explode s)
      val intlist = if List.length lastint = 0 then intlist else intlist@[String.implode lastint]
(*
      fun str2int str =  Option.valOf(Int.fromString str)) handle Overflow => 1 
      val intlist = List.map str2int intlist 
*)
      val intnum = List.length intlist
    in
      if intnum = 1 then 1.0 else 0.0
    end

  fun int_num_2 s =
    let
      fun collect_digit (c, (thisint, list)) = 
        if Char.isDigit c then (thisint@[c], list) 
        else 
          if List.length thisint = 0 then (thisint, list)
          else ([], list@[String.implode thisint])
      val (lastint, intlist) = List.foldl collect_digit ([], []) (String.explode s)
      val intlist = if List.length lastint = 0 then intlist else intlist@[String.implode lastint]
(*
      fun str2int str =  Option.valOf(Int.fromString str)) handle Overflow => 1 
      val intlist = List.map str2int intlist 
*)
      val intnum = List.length intlist
    in
      if intnum = 2 then 1.0 else 0.0
    end

  fun int_num_3 s =
    let
      fun collect_digit (c, (thisint, list)) = 
        if Char.isDigit c then (thisint@[c], list) 
        else 
          if List.length thisint = 0 then (thisint, list)
          else ([], list@[String.implode thisint])
      val (lastint, intlist) = List.foldl collect_digit ([], []) (String.explode s)
      val intlist = if List.length lastint = 0 then intlist else intlist@[String.implode lastint]
(*
      fun str2int str =  Option.valOf(Int.fromString str)) handle Overflow => 1 
      val intlist = List.map str2int intlist 
*)
      val intnum = List.length intlist
    in
      if intnum = 3 then 1.0 else 0.0
    end

  fun int_num_2_3_4 s =
    let
      fun collect_digit (c, (thisint, list)) = 
        if Char.isDigit c then (thisint@[c], list) 
        else 
          if List.length thisint = 0 then (thisint, list)
          else ([], list@[String.implode thisint])
      val (lastint, intlist) = List.foldl collect_digit ([], []) (String.explode s)
      val intlist = if List.length lastint = 0 then intlist else intlist@[String.implode lastint]
(*
      fun str2int str =  Option.valOf(Int.fromString str)) handle Overflow => 1 
      val intlist = List.map str2int intlist 
*)
      val intnum = List.length intlist
    in
      if intnum = 2 orelse intnum = 3 then 1.0 else 0.0
    end

  fun int_num_4 s =
    let
      fun collect_digit (c, (thisint, list)) = 
        if Char.isDigit c then (thisint@[c], list) 
        else 
          if List.length thisint = 0 then (thisint, list)
          else ([], list@[String.implode thisint])
      val (lastint, intlist) = List.foldl collect_digit ([], []) (String.explode s)
      val intlist = if List.length lastint = 0 then intlist else intlist@[String.implode lastint]
(*
      fun str2int str =  Option.valOf(Int.fromString str)) handle Overflow => 1 
      val intlist = List.map str2int intlist 
*)
      val intnum = List.length intlist
    in
      if intnum = 4 then 1.0 else 0.0
    end

  fun int_num_5 s =
    let
      fun collect_digit (c, (thisint, list)) = 
        if Char.isDigit c then (thisint@[c], list) 
        else 
          if List.length thisint = 0 then (thisint, list)
          else ([], list@[String.implode thisint])
      val (lastint, intlist) = List.foldl collect_digit ([], []) (String.explode s)
      val intlist = if List.length lastint = 0 then intlist else intlist@[String.implode lastint]
(*
      fun str2int str =  Option.valOf(Int.fromString str)) handle Overflow => 1 
      val intlist = List.map str2int intlist 
*)
      val intnum = List.length intlist
    in
      if intnum > 4 then 1.0 else 0.0
    end

  fun int_less_colon_1 s = (*for time *)
    let
      fun collect_digit (c, (thisint, list)) = 
        if Char.isDigit c then (thisint@[c], list) 
        else 
          if List.length thisint = 0 then (thisint, list)
          else (
            if List.length thisint = 2 then ([], list@[String.implode thisint])
            else ([], list)
          )
      val (lastint, intlist) = List.foldl collect_digit ([], []) (String.explode s)
      val intlist = if List.length lastint = 2 then intlist@[String.implode lastint] else intlist
(*
      fun str2int str =  Option.valOf(Int.fromString str)) handle Overflow => 1 
      val intlist = List.map str2int intlist 
*)
      val intnum = List.length intlist
      fun is_colon c = if Char.compare(c, #":")=EQUAL then true else false
      fun count (c, num) = if is_colon c then num+1 else num
      val colonnum = List.foldl count 0 (String.explode s)
    in
      if (intnum = 3 andalso colonnum = 2) orelse (intnum = 2 andalso colonnum = 1) then 1.0 else 0.0
    end

  fun int_less_dot_1 s = (*for ip *)
    let
      fun collect_digit (c, (thisint, list)) = 
        if Char.isDigit c then (thisint@[c], list) 
        else 
          if List.length thisint = 0 then (thisint, list)
          else ([], list@[String.implode thisint])
      val (lastint, intlist) = List.foldl collect_digit ([], []) (String.explode s)
      val intlist = if List.length lastint = 0 then intlist else intlist@[String.implode lastint]
(*
      fun str2int str =  Option.valOf(Int.fromString str)) handle Overflow => 1 
      val intlist = List.map str2int intlist 
*)
      val intnum = List.length intlist
      fun is_dot c = if Char.compare(c, #".")=EQUAL then true else false
      fun count (c, num) = if is_dot c then num+1 else num
      val dotnum = List.foldl count 0 (String.explode s)
    in
      if (intnum = 4 andalso dotnum = 3) then 1.0 else 0.0
    end

  fun int_less_dot_2 s = (*for float *)
    let
      fun collect_digit (c, (thisint, list)) = 
        if Char.isDigit c then (thisint@[c], list) 
        else 
          if List.length thisint = 0 then (thisint, list)
          else ([], list@[String.implode thisint])
      val (lastint, intlist) = List.foldl collect_digit ([], []) (String.explode s)
      val intlist = if List.length lastint = 0 then intlist else intlist@[String.implode lastint]
(*
      fun str2int str =  Option.valOf(Int.fromString str)) handle Overflow => 1 
      val intlist = List.map str2int intlist 
*)
      val intnum = List.length intlist
      fun is_dot c = if Char.compare(c, #".")=EQUAL then true else false
      fun count (c, num) = if is_dot c then num+1 else num
      val dotnum = List.foldl count 0 (String.explode s)
    in
      if (intnum = 2 andalso dotnum = 1) then 1.0 else 0.0
    end

  fun timezone_suffix s =
    if String.size s < 5 then 0.0
    else if not(String.isSuffix "00" s) then 0.0 
    else if not(Char.compare(#"-", String.sub(s, (String.size s)-5))=EQUAL orelse Char.compare(#"+", String.sub(s, (String.size s)-5))=EQUAL) then 0.0
    else if not(Char.compare(#"0", String.sub(s, (String.size s)-4))=EQUAL orelse Char.compare(#"1", String.sub(s, (String.size s)-4))=EQUAL) then 0.0
    else if not(Char.isDigit (String.sub(s, (String.size s)-3))) then 0.0
    else 1.0

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
    ("and_num", and_num),
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
    ("last_punct", last_punct),
    ("only_hex", only_hex),
    ("num_digit", num_digit),
    ("num_alpha", num_alpha),
    ("num_punc", num_punc),
    ("has_ampm", has_ampm),
    ("has_month", has_month),
    ("has_date", has_date),
    ("has_hostsuffix", has_hostsuffix)
  ]

  val binaryfeatureList = [
    ("token_length_5", token_length_5),
    ("token_length_10", token_length_10),
    ("token_length_20", token_length_20),
    ("token_length_30", token_length_30),
    ("dot_num_no", dot_num_no), 
    ("dot_num_1", dot_num_1),
    ("dot_num_2", dot_num_2),
    ("dot_num_3", dot_num_3),
    ("dot_num_5", dot_num_5),
    ("dot_num_10", dot_num_10),
    ("dot_num_20", dot_num_20),
    ("slash_num_no", slash_num_no),
    ("slash_num_1", slash_num_1),
    ("slash_num_5", slash_num_5),
    ("slash_num_10", slash_num_10),
    ("slash_num_20", slash_num_20),
    ("bslash_num_no", bslash_num_no),
    ("bslash_num_1", bslash_num_1),
    ("bslash_num_5", bslash_num_5),
    ("bslash_num_10", bslash_num_10),
    ("bslash_num_20", bslash_num_20),
    ("semicolon_num_no", semicolon_num_no),
    ("semicolon_num_1", semicolon_num_1),
    ("semicolon_num_2", semicolon_num_2),
    ("semicolon_num_3", semicolon_num_3),
    ("semicolon_num_5", semicolon_num_5),
    ("semicolon_num_10", semicolon_num_10),
    ("semicolon_num_20", semicolon_num_20),
    ("bar_num_no", bar_num_no),
    ("bar_num_1", bar_num_1),
    ("bar_num_5", bar_num_5),
    ("bar_num_10", bar_num_10),
    ("bar_num_20", bar_num_20),
    ("less_num_no", less_num_no),
    ("less_num_1", less_num_1),
    ("less_num_5", less_num_5),
    ("less_num_10", less_num_10),
    ("less_num_20", less_num_20),
    ("tilde_num_no", tilde_num_no),
    ("tilde_num_1", tilde_num_1),
    ("tilde_num_5", tilde_num_5),
    ("tilde_num_10", tilde_num_10),
    ("tilde_num_20", tilde_num_20),
    ("bquote_num_no", bquote_num_no),
    ("bquote_num_1", bquote_num_1),
    ("bquote_num_5", bquote_num_5),
    ("bquote_num_10", bquote_num_10),
    ("bquote_num_20", bquote_num_20),
    ("bang_num_no", bang_num_no),
    ("bang_num_1", bang_num_1),
    ("bang_num_5", bang_num_5),
    ("bang_num_10", bang_num_10),
    ("bang_num_20", bang_num_20),
    ("at_num_no", at_num_no),
    ("at_num_1", at_num_1),
    ("at_num_5", at_num_5),
    ("at_num_10", at_num_10),
    ("at_num_20", at_num_20),
    ("hash_num_no", hash_num_no),
    ("hash_num_1", hash_num_1),
    ("hash_num_5", hash_num_5),
    ("hash_num_10", hash_num_10),
    ("hash_num_20", hash_num_20),
    ("dollar_num_no", dollar_num_no),
    ("dollar_num_1", dollar_num_1),
    ("dollar_num_5", dollar_num_5),
    ("dollar_num_10", dollar_num_10),
    ("dollar_num_20", dollar_num_20),
    ("percent_num_no", percent_num_no),
    ("percent_num_1", percent_num_1),
    ("percent_num_5", percent_num_5),
    ("percent_num_10", percent_num_10),
    ("percent_num_20", percent_num_20),
    ("caret_num_no", caret_num_no),
    ("caret_num_1", caret_num_1),
    ("caret_num_5", caret_num_5),
    ("caret_num_10", caret_num_10),
    ("caret_num_20", caret_num_20),
    ("and_num_no", and_num_no),
    ("and_num_1", and_num_1),
    ("and_num_5", and_num_5),
    ("and_num_10", and_num_10),
    ("and_num_20", and_num_20),
    ("star_num_no", star_num_no),
    ("star_num_1", star_num_1),
    ("star_num_5", star_num_5),
    ("star_num_10", star_num_10),
    ("star_num_20", star_num_20),
    ("lpar_num_no", lpar_num_no),
    ("lpar_num_1", lpar_num_1),
    ("lpar_num_5", lpar_num_5),
    ("lpar_num_10", lpar_num_10),
    ("lpar_num_20", lpar_num_20),
    ("rpar_num_no", rpar_num_no),
    ("rpar_num_1", rpar_num_1),
    ("rpar_num_5", rpar_num_5),
    ("rpar_num_10", rpar_num_10),
    ("rpar_num_20", rpar_num_20),
    ("hyphen_num_no", hyphen_num_no),
    ("hyphen_num_1", hyphen_num_1),
    ("hyphen_num_2", hyphen_num_2),
    ("hyphen_num_3", hyphen_num_3),
    ("hyphen_num_5", hyphen_num_5),
    ("hyphen_num_10", hyphen_num_10),
    ("hyphen_num_20", hyphen_num_20),
    ("underscore_num_no", underscore_num_no),
    ("underscore_num_1", underscore_num_1),
    ("underscore_num_2", underscore_num_2),
    ("underscore_num_3", underscore_num_3),
    ("underscore_num_5", underscore_num_5),
    ("underscore_num_10", underscore_num_10),
    ("underscore_num_20", underscore_num_20),
    ("add_num_no", add_num_no),
    ("add_num_1", add_num_1),
    ("add_num_5", add_num_5),
    ("add_num_10", add_num_10),
    ("add_num_20", add_num_20),
    ("equal_num_no", equal_num_no),
    ("equal_num_1", equal_num_1),
    ("equal_num_5", equal_num_5),
    ("equal_num_10", equal_num_10),
    ("equal_num_20", equal_num_20),
    ("lbrac_num_no", lbrac_num_no),
    ("lbrac_num_1", lbrac_num_1),
    ("lbrac_num_5", lbrac_num_5),
    ("lbrac_num_10", lbrac_num_10),
    ("lbrac_num_20", lbrac_num_20),
    ("rbrac_num_no", rbrac_num_no),
    ("rbrac_num_1", rbrac_num_1),
    ("rbrac_num_5", rbrac_num_5),
    ("rbrac_num_10", rbrac_num_10),
    ("rbrac_num_20", rbrac_num_20),
    ("lsqubrac_num_no", lsqubrac_num_no),
    ("lsqubrac_num_1", lsqubrac_num_1),
    ("lsqubrac_num_5", lsqubrac_num_5),
    ("lsqubrac_num_10", lsqubrac_num_10),
    ("lsqubrac_num_20", lsqubrac_num_20),
    ("rsqubrac_num_no", rsqubrac_num_no),
    ("rsqubrac_num_1", rsqubrac_num_1),
    ("rsqubrac_num_5", rsqubrac_num_5),
    ("rsqubrac_num_10", rsqubrac_num_10),
    ("rsqubrac_num_20", rsqubrac_num_20),
    ("colon_num_no", colon_num_no),
    ("colon_num_1", colon_num_1),
    ("colon_num_2", colon_num_2),
    ("colon_num_3", colon_num_3),
    ("colon_num_5", colon_num_5),
    ("colon_num_10", colon_num_10),
    ("colon_num_20", colon_num_20),
    ("dquote_num_no", dquote_num_no),
    ("dquote_num_1", dquote_num_1),
    ("dquote_num_5", dquote_num_5),
    ("dquote_num_10", dquote_num_10),
    ("dquote_num_20", dquote_num_20),
    ("quote_num_no", quote_num_no),
    ("quote_num_1", quote_num_1),
    ("quote_num_5", quote_num_5),
    ("quote_num_10", quote_num_10),
    ("quote_num_20", quote_num_20),
    ("greater_num_no", greater_num_no),
    ("greater_num_1", greater_num_1),
    ("greater_num_5", greater_num_5),
    ("greater_num_10", greater_num_10),
    ("greater_num_20", greater_num_20),
    ("comma_num_no", comma_num_no),
    ("comma_num_1", comma_num_1),
    ("comma_num_2", comma_num_2),
    ("comma_num_3", comma_num_3),
    ("comma_num_5", comma_num_5),
    ("comma_num_10", comma_num_10),
    ("comma_num_20", comma_num_20),
    ("question_num_no", question_num_no),
    ("question_num_1", question_num_1),
    ("question_num_5", question_num_5),
    ("question_num_10", question_num_10),
    ("question_num_20", question_num_20),
    ("white_num_no", white_num_no),
    ("white_num_1", white_num_1),
    ("white_num_2", white_num_2),
    ("white_num_3", white_num_3),
    ("white_num_5", white_num_5),
    ("white_num_10", white_num_10),
    ("white_num_20", white_num_20),
    ("tab_num_no", tab_num_no),
    ("tab_num_1", tab_num_1),
    ("tab_num_5", tab_num_5),
    ("tab_num_10", tab_num_10),
    ("tab_num_20", tab_num_20),
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
    ("last_punct", last_punct),
    ("only_hex", only_hex),
    ("num_digit_no", num_digit_no),
    ("num_digit_1", num_digit_1),
    ("num_digit_2", num_digit_2),
    ("num_digit_3", num_digit_3),
    ("num_digit_5", num_digit_5),
    ("num_digit_10", num_digit_10),
    ("num_digit_20", num_digit_20),
    ("num_alpha_no", num_alpha_no),
    ("num_alpha_1", num_alpha_1),
    ("num_alpha_2", num_alpha_2),
    ("num_alpha_3", num_alpha_3),
    ("num_alpha_5", num_alpha_5),
    ("num_alpha_10", num_alpha_10),
    ("num_alpha_20", num_alpha_20),
    ("num_punc_no", num_punc_no),
    ("num_punc_1", num_punc_1),
    ("num_punc_2", num_punc_2),
    ("num_punc_3", num_punc_3),
    ("num_punc_5", num_punc_5),
    ("num_punc_10", num_punc_10),
    ("num_punc_20", num_punc_20),
    ("has_ampm", has_ampm),
    ("has_month", has_month),
    ("has_date", has_date),
(*
    ("no_alpha", no_alpha),
    ("no_colon", no_colon),
*)
    ("has_hostsuffix", has_hostsuffix),
    ("has_docname_suffix", has_docname_suffix),
    ("all_digit", all_digit),
    ("not_all_digit", not_all_digit),
    ("all_alpha_space", all_alpha_space),
    ("digit_all_below24", digit_all_below24),
    ("digit_all_below31", digit_all_below31),
    ("int_num_0", int_num_0),
    ("int_num_1", int_num_1),
    ("int_num_2", int_num_2),
    ("int_num_3", int_num_3),
    ("int_num_2_3_4", int_num_2_3_4),
    ("int_num_4", int_num_4),
    ("int_num_5", int_num_5),
    ("int_less_colon_1", int_less_colon_1),
    ("int_less_dot_1", int_less_dot_1),
    ("int_less_dot_2", int_less_dot_2),
    ("timezone_suffix", timezone_suffix)
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

  fun no_dot_around s sbegin send = 
    let 
      val pre = if sbegin>0 then (
                                      if Char.compare(String.sub(s, sbegin-1), #".") = EQUAL then true else false 
                                 )
                else false
      val next = if send<(String.size s)-1 then (
                   if Char.compare(String.sub(s, send+1), #".")=EQUAL then true else false
                 )
                 else false
     in
       if pre orelse next then 0.0 else 1.0
     end

  fun no_colon_around s sbegin send = 
    let 
      val pre = if sbegin>0 then (
                                      if Char.compare(String.sub(s, sbegin-1), #":") = EQUAL then true else false 
                                 )
                else false
      val next = if send<(String.size s)-1 then (
                   if Char.compare(String.sub(s, send+1), #":")=EQUAL then true else false
                 )
                 else false
     in
       if pre orelse next then 0.0 else 1.0
     end

  val otherFeatures = 
  [
    ("is_first", is_first),
    ("is_last", is_last),
    ("pre_digit", pre_digit),
    ("pre_alpha", pre_alpha),
    ("pre_punct", pre_punct),
    ("next_digit", next_digit),
    ("next_alpha", next_alpha),
    ("next_punct", next_punct),
    ("no_dot_around", no_dot_around),
    ("no_colon_around", no_colon_around)
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

  fun BSToken2FeatureStrs2 (b, str, sbegin, send) : string = 
    let
(*val _ = print (str^" "^(Int.toString sbegin)^" "^(Int.toString send)^"\n")*)
      val s = if sbegin = ~1 orelse send = ~1 then "" else String.substring(str, sbegin, send-sbegin+1)
(*val _ = print (s^"\n")*)
      fun mymap1 ((fname, f), rets) = if Real.compare((f s), 0.0)=EQUAL then rets else (rets^" "^fname)
      fun mymap2 ((fname, f), rets) =  if Real.compare((f str sbegin send), 0.0)=EQUAL then rets else (rets^" "^fname)
      val featureStr = (List.foldl mymap1 "" binaryfeatureList)^" "^(List.foldl mymap2 "" otherFeatures)
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
                         NONE => 0.0    (* if this feature is not trained, it has no weights *)
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
val i = ref 0
fun printproblist thisprob = (print (BTokenToName(indexToBToken (!i))^" "^(Real.toString thisprob)^" "); i:= !i + 1)
val _ = if compBToken(b, PPfloat)=EQUAL orelse compBToken(b, PPtext)=EQUAL then (print ((BTokenToName b)^" ----------- "^s^" ------------- "^(Real.toString prob)^"\n"); List.app printproblist problist; print "\n") else ()
    in
      prob
    end

(*
  fun tokenProb (b, str, sbegin, send, ghmmmodel) : real = 
    let
      val s = String.substring(str, sbegin, send-sbegin+1)
      val features = BSToken2Features (b, str, sbegin, send)
      val tindex = BTokenCompleteEnum(b)-1
      val prob = f2prob features tindex ghmmmodel
      val prob = Math.exp prob
    in
      prob
    end
*)
end
