structure TyProps =

struct
   datatype diskSize = Size of int * int  (* number of bytes, number of EOR markers *)
                      | Param of int * string * ParseTree.expression | Variable
   datatype memChar = Static | Dynamic

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