structure TyProps =

struct
   structure P = ParseTreeUtil
   structure PT = ParseTree

   datatype diskSize =  Size of IntInf.int * IntInf.int  (* number of bytes, number of EOR markers *)
                      | Param of string list * string option * PT.expression * PT.expression
                      | Variable

   type argList = string list * PT.expression list
   type labelInfo = string * string * argList (* label name, label type, supplied arguments *)

   datatype compoundSize =  Base of diskSize | Typedef of diskSize 
                          | Struct of (labelInfo option * diskSize) list 
                          | Union of (labelInfo option * diskSize) list 
                          | Array of {baseTy : string, args : argList,
				      elem : diskSize, sep : diskSize, term : diskSize, length : diskSize}
                          | Enum of diskSize

   datatype memChar = Static | Dynamic

   fun mkSize (n1,n2) = Size(IntInf.fromInt n1, IntInf.fromInt n2)

   fun printStrList [] = ""
     | printStrList (x::xs) = (x^", "^( printStrList xs))

   fun printSize Variable = print "Variable\n"
     | printSize (Param(params, _, exp1, exp2)) = 
          print ("Parameterized:\n Vars: "^(printStrList params)^
		 "\nNumber of bytes expression: "^(P.expToString exp1)^
		 "\nNumber of records expresion: "^(P.expToString exp2)^".\n")
     | printSize (Size(n1,n2)) = print ("Fixed size: bytes = "^(IntInf.toString n1)^" records = "^(IntInf.toString n2)^".\n")

   fun add (Variable, _ ) = Variable 
     | add (_, Variable ) = Variable 
     | add (Size(x1,y1), Size(x2,y2)) = Size(IntInf.+(x1, x2), IntInf.+(y1, y2))
     | add (Size(x1,y1), Param(ps, s, ebytes, erecs)) = Param(ps, s, P.plusX(PT.IntConst x1, ebytes),
							             P.plusX(PT.IntConst y1, erecs))
     | add (Param(ps, s, ebytes, erecs), Size(x2,y2)) = Param(ps, s, P.plusX(ebytes, PT.IntConst x2),
							             P.plusX(erecs, PT.IntConst y2))
     | add (Param(ps1,s1,ebytes1, erecs1), Param(ps2,s2,ebytes2,erecs2)) = 
             Param(ps1, NONE, P.plusX(ebytes1, ebytes2), P.plusX(erecs1, erecs2))


   val dynamicValue = PLib.strLen(PT.String "Non-static-length")
   fun merge (e1,e2) =  P.condX(P.eqX(e1, e2), e2, dynamicValue)

   fun overlay (Variable, _ ) = Variable 
     | overlay (_, Variable ) = Variable 
     | overlay (r as Size(x1,y1), Size(x2,y2)) = 
       if x1 = x2 andalso y1 = y2 then  r else Variable
     | overlay (Size(x1,y1), Param(ps, s, ebytes, erecs)) = 
	   Param(ps, s, merge(PT.IntConst x1, ebytes), merge(PT.IntConst y1, erecs))
     | overlay (Param(ps, s, ebytes, erecs), Size(x2,y2)) = 
	   Param(ps, s, merge(ebytes, PT.IntConst x2), merge(erecs, PT.IntConst y2))
     | overlay (Param(ps1,s1,ebytes1, erecs1), Param(ps2,s2,ebytes2,erecs2)) = 
             Param(ps1, NONE, merge(ebytes1, ebytes2), merge(erecs1, erecs2))

   (* scale first argument by first first component of second;
      using byte size to represent array repetition. *)
   fun scale (Variable, _ ) = Variable 
     | scale (_, Variable ) = Variable 
     | scale (Size(x1,y1), Size(rep,_)) = Size(IntInf.*(x1, rep), IntInf.*(y1, rep))
     | scale (Size(x1,y1), Param(ps, s, rep, _)) = Param(ps, s, P.timesX(PT.IntConst x1, rep),
							        P.timesX(PT.IntConst y1, rep))
     | scale (Param(ps, s, rep, _), Size(x2,y2)) = Param(ps, s, P.timesX(rep, PT.IntConst x2),
							          P.timesX(rep, PT.IntConst y2))
     | scale (Param(ps1,s1,ebytes1, erecs1), Param(ps2,s2,rep,_)) = 
             Param(ps1, NONE, P.timesX(ebytes1, rep), P.plusX(erecs1, rep))



   fun mergeDiskSize f (x,y) = case (x,y) 
    of (Variable, _ ) => Variable
     | (_, Variable ) => Variable
     | (Param a, _) => Param a
     | (_, Param b) => Param b
     | (Size n, Size m) => f(n,m)

   fun mergeMemChar (Dynamic, _) = Dynamic
     | mergeMemChar (_, Dynamic) = Dynamic
     | mergeMemChar (Static, Static) = Static


end