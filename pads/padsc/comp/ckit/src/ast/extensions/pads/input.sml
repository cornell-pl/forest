structure PInput = 
struct

type PinputTy = {outputWrites : bool ref,
		 outputXML    : bool ref}


val inputs : PinputTy = {outputWrites = ref true,
			 outputXML    = ref false}


fun emitWrites (status) = (#outputWrites inputs) := status
fun emitXML (status) = (#outputXML inputs) := status
end