module Drive where
import Language.Pads.Syntax
import Language.Pads.Parser
import Language.Pads.Pretty


result = runLex padsDecl "Foo = (Pint,'|',Pint)"