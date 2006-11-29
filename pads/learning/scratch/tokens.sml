structure Tokens = 

struct
    type offset = {offset: int, span:int}

    datatype Token = Ptime of string | Pmonth of string | Pip of string | 
                     Pint of LargeInt.int | Pstring of string | 
                     Pgroup of {left : Token, body : LToken list, right : Token} | 
	             Pwhite of string | Other of char | Pempty | Error
    withtype LToken = Token * offset

end