structure TyProps =

struct
   structure P = ParseTreeUtil

   datatype diskSize =  Size of int * int  (* number of bytes, number of EOR markers *)
                      | Param of string list * string option * ParseTree.expression * ParseTree.expression
                      | Variable

   datatype compoundSize =  Base of diskSize | Typedef of diskSize | Struct of diskSize list | Union of diskSize list 
                          | Array of {elem : diskSize, sep : diskSize, term : diskSize, length : diskSize}
                          | Enum of diskSize

   datatype memChar = Static | Dynamic

   fun printStrList [] = ""
     | printStrList (x::xs) = (x^", "^( printStrList xs))

   fun printSize Variable = print "Variable\n"
     | printSize (Param(params, _, exp1, exp2)) = 
          print ("Parameterized:\n Vars: "^(printStrList params)^
		 "\nNumber of bytes expression: "^(P.expToString exp1)^
		 "\nNumber of records expresion: "^(P.expToString exp2)^".\n")
     | printSize (Size(n1,n2)) = print ("Fixed size: bytes = "^(Int.toString n1)^" records = "^(Int.toString n2)^".\n")

   fun add (Variable, _ ) = Variable 
     | add (_, Variable ) = Variable 
     | add (Size(x1,y1), Size(x2,y2)) = Size(x1 + x2, y1 + y2)
     | add (Size(x1,y1), Param(ps, s, ebytes, erecs)) = Param(ps, s, P.plusX(P.intX x1, ebytes),
							             P.plusX(P.intX y1, erecs))
     | add (Param(ps, s, ebytes, erecs), Size(x2,y2)) = Param(ps, s, P.plusX(ebytes, P.intX x2),
							             P.plusX(erecs, P.intX y2))
     | add (Param(ps1,s1,ebytes1, erecs1), Param(ps2,s2,ebytes2,erecs2)) = 
             Param(ps1, NONE, P.plusX(ebytes1, ebytes2), P.plusX(erecs1, erecs2))


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