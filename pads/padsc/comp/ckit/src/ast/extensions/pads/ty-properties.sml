structure TyProps =

struct
   datatype diskSize = Size of int | Param | Variable
   datatype memChar = Static | Dynamic

   fun mergeDiskSize f (x,y) = case (x,y) 
    of (Variable, _ ) => Variable
     | (_, Variable ) => Variable
     | (Param, _) => Param
     | (_, Param) => Param
     | (Size n, Size m) => f(n,m)

   fun mergeMemChar (Dynamic, _) = Dynamic
     | mergeMemChar (_, Dynamic) = Dynamic
     | mergeMemChar (Static, Static) = Static


end