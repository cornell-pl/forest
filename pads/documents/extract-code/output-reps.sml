(* output-reps.sml
 *
 * COPYRIGHT (c) 2002 John Reppy (jhr@cs.uchicago.edu)
 *)

structure OutputReps =
  struct

    datatype text
      = RAW of string
      | COMMENT of string	(* EOL comment; newline not included *)
      | LN of Tokens.line
      | HLIGHT_ON | HLIGHT_OFF

    datatype outfile = OF of {
	name : Atom.atom,	(* the output file name *)
	codeEnv : string,	(* type of code environment (code, centercode, *)
				(* or tightcode) *)
	closed : bool ref,	(* false, while the file is open *)
	enabled : bool ref,	(* true, if output is enabled *)
	text : text list ref	(* the output to the file *)
      }

  end
