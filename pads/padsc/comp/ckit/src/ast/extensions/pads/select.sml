structure Select =

struct
   structure AtomMap = PBaseTys.PBST
   datatype selectInfo = Select of {tyName : string, offset : int, size : int}

   fun selectToString (Select {tyName : string, offset : int, size : int}) =
       tyName^"("^(Int.toString offset)^","^(Int.toString size)^")"

   fun selectListToString sl = 
       let fun h sl = 
	   case sl 
           of [] => ""
           |  [s] => (selectToString s)
           |  (s::sl) => ((selectToString s)^","^(h sl))
       in
	   "{"^(h sl)^"}"
       end

   fun cmp (Select {tyName=tyName1, offset=offset1, size=size1}, Select {tyName=tyName2, offset=offset2, size=size2})  = 
       if offset1 < offset2 then LESS
       else if offset1 > offset2 then GREATER
       else if size1 < size2 then LESS
       else if size1 > size2 then GREATER
       else EQUAL

   fun itemLeq ((offset1, size1), (offset2, size2))  = 
       if offset1 < offset2 then true
       else if offset1 > offset2 then false
       else if size1 < size2 then true
       else if size1 > size2 then false
       else true

   type selectMapTy = (int * int) list AtomMap.map
   val selectMap : selectMapTy ref = ref AtomMap.empty

   fun insert (Select s : selectInfo) = 
       let val key = Atom.atom (#tyName s)
	   val newitem = (#offset s, #size s)
	   val prevEntryOpt = AtomMap.find(!selectMap, key)
	   fun ins [] = [newitem] 
             | ins (l as (e::es)) = 
                 if itemLeq(newitem, e) then newitem :: l
                 else (e :: (ins es))
	   val newentry = case prevEntryOpt of NONE => [newitem] | SOME l => ins l
       in
	   selectMap := AtomMap.insert(!selectMap, key, newentry) 
       end
  
   fun listSelections () = []

(* Atom.listKeys (!selectMap) *)

   fun isSelection () = not (AtomMap.isEmpty (!selectMap))


end