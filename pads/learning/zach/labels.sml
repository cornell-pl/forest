(* provides unique labels for data and sets/maps for the data *)
structure Label = struct
type id = string
val nextID = ref 0
fun freshID () = let
				val _ = nextID := !nextID + 1
			  in
			   	"r" ^ (Int.toString (!nextID))
			  end
			  
fun makeID (x:string) = x
structure Map = SplayMapFn(struct
							type ord_key = id
							val compare = String.compare
							end)
structure Set = SplaySetFn(struct
							type ord_key = id
							val compare = String.compare
							end)
end
