(*
Zach DeVito
Provides easy timing of programs
*)
structure TimeIt = struct

(* takes a function f : unit -> 'a and times it *)
fun time f = 
let
	val timer = Timer.startCPUTimer()
	val ret = f()
	val { 
		  nongc = { usr = utime, sys = stime }, 
	      gc = { usr = gutime, sys = gstime }
	    } = Timer.checkCPUTimes timer
	val result = 	"Usr: "^(Time.toString utime)^
					" Sys: "^(Time.toString stime)^
					" GC_usr: "^(Time.toString gutime)^
					" GC_Sys:"^(Time.toString gstime)^"\n"
	val tms = [stime,gutime,gstime]
	val total = foldr Time.+ utime tms
in
	(print result; (ret,total) )
end

end