structure Select =
struct
   structure PT = ParseTree

   datatype selectInfo = Select of {tyName : string, offset : IntInf.int, size : IntInf.int}

   datatype pathNode = Id of string | Dot of string | Sub of IntInf.int 
   type path = pathNode list

   fun sexprToPath (p) : path = 
       let fun cnvExp (exp, accumP) = 
	       case exp
	       of PT.Id s => (Id s):: accumP
	       |  PT.Binop (PT.Dot, e1, PT.Id f) => cnvExp(e1, (Dot f) :: accumP)
	       |  PT.Binop (PT.Sub, e1, PT.IntConst i)  => cnvExp(e1, (Sub  i) :: accumP)
	       |  p => raise Fail "Ill-formed path expression."
       in
	   cnvExp(p, [])
       end


   fun selectToString (Select {tyName, offset, size}) =
       tyName^"("^(IntInf.toString offset)^","^(IntInf.toString size)^")"

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
       if IntInf.< (offset1, offset2) then LESS
       else if IntInf.>(offset1, offset2) then GREATER
       else if IntInf.<(size1, size2) then LESS
       else if IntInf.>(size1, size2) then GREATER
       else EQUAL

   structure SelectMap = RedBlackMapFn(
			     struct type ord_key = selectInfo
			     val compare = cmp
		         end) 


   type selectMapTy = unit SelectMap.map
   val selectMap : selectMapTy ref = ref SelectMap.empty

   fun insert (s : selectInfo) = 
       selectMap := SelectMap.insert(!selectMap, s, ())
   fun listSelections () = SelectMap.listKeys (!selectMap)

   fun isSelection () = not (SelectMap.isEmpty (!selectMap))


end