structure Tokens = 

struct
    type offset = {offset: int, span:int}
    datatype Token = Pint of LargeInt.int | Pstring of string | 
                     Pip of string | Ptime of string |
	             Pwhite of string | Other of char | Error
end