(*
 Trials.sml
 Zach DeVito
 
 Runs time trials for different formats
*)
structure Trials = struct
open Common


(* reads in an r column tuple of ints from trial.txt and runs the analysis on it
*)
fun trial(r) = 
let
  val tups = List.tabulate(r,(fn _ => Tuple[ Base IntBase, Base (ConstBase " ")]))
  val desc = Array (Tuple (tups @ [ Base (ConstBase "\n") ]))
  val desc = Reduce.reduce NONE desc
  val data = Interp.interpFile(desc,"trial.txt")
in
	Main.run((desc,"trial.txt"), data)
end
(* times trial(r), printing the time to stderr *)
fun go(c,a) = 
let
	val (_,time) = TimeIt.time (fn () => trial(a) ) 
in
	TextIO.output(TextIO.stdErr,(Time.toString time) ^ "\n")
end


(* tests the apache log format in apache_test *)
fun apache_test() = 
let
  val desc = Reduce.reduce NONE Desc.apache_desc
  val data = Interp.interpFile(desc,"apache_test.txt")
in
	Main.run((desc,"apache_test.txt"),data)
end

fun apache() = 
let
	val (_,time) = TimeIt.time apache_test 
in
	TextIO.output(TextIO.stdErr,(Time.toString time) ^ "\n")
end

(* val _ = OS.Process.exit(OS.Process.success); *)
end