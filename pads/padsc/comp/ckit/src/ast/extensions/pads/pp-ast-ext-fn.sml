(* Copyright (c) 1998 by Lucent Technologies *)

functor PPAstExtFn (type aidinfo):PPASTEXT = struct
  type aidinfo = aidinfo
  fun ppUnopExt aidinfo pair pps unopExt = ()
  fun ppBinopExt aidinfo pair pps binopExt = ()
  fun ppExpressionExt quad aidinfo pair pps expExt = ()
  fun ppStatementExt quad aidinfo pair pps (AstExt.SComment s) =        
      (PPLib.addStr pps "/* ";
	PPLib.addStr pps s;
        PPLib.addStr pps " */")

  fun ppExternalDeclExt stringOpt quad aidinfo pair pps (AstExt.EComment s) = 
       (PPLib.addStr pps "/* ";
	PPLib.addStr pps s;
        PPLib.addStr pps " */";
        PPLib.newline pps)
end

