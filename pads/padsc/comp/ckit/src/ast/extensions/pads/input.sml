structure PInput = 
struct

type PinputTy = {outputWrites : bool}

val defaults : PinputTy = 
    { outputWrites = true } 

val inputs : PinputTy ref = ref defaults

(* eventually generalize to allow partial customization *)
fun init (ins : PinputTy) = inputs := ins

end