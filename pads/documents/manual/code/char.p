/*@FILE char-decl.tex char-constraint-decl.tex countX.tex stringFW.tex string-stop.tex string-match.tex string-stope.tex timestamp-explicit.tex*/

/*@BEGIN char-decl.tex */
Pa_char c;
/*@END char-decl.tex */

Pstruct foo{
/*@BEGIN char-constraint-decl.tex */
 Pe_char  c : c == 'A' || c == 'B';
/*@END char-constraint-decl.tex */
/*@BEGIN countX.tex */
 Pa_countX(: '=', 0, 0 :) my_count;
/*@END countX.tex */
/*@BEGIN stringFW.tex */
 Pstring_FW(: 10 :) my_string;
/*@END stringFW.tex */
/*@BEGIN string-stop.tex */
 Pstring_FW(: 10 :) my_string;
/*@END string-stop.tex */
/*@BEGIN string-match.tex */
 Pa_string_ME(: "/\\S\*/" :) my_string;
/*@END string-match.tex */
/*@BEGIN timestamp-explicit.tex */
Pa_timestamp_explicit(: '|' , "%Y-%m-%d+%H:%M" :) my_timestamp;
/*@END timestamp-explicit.tex */

};

Pstruct foo2 {
/*@BEGIN string-stope.tex */
 Pa_string_SE(: "/\\s|$/" :) my_string;
/*@END string-stope.tex */
};


