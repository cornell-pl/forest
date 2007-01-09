(* 
Zach DeVito
sets up sml for profiling 
*)
SMLofNJ.Internals.GC.messages false;
Compiler.Profile.setProfMode true;
Compiler.Profile.setTimingMode true;
CM.make();

