structure Tokens = 

struct
    type location = {lineNo: int, beginloc: int, endloc:int}

    datatype Token = PbXML of string * string |
                     PeXML of string * string |
	             Ptime of string | 
		     Pmonth of string | 
		     Pip of string | 
                     Pint of LargeInt.int | 
		     Pstring of string | 
                     Pgroup of {left : LToken, body : LToken list, right : LToken} | 
	             Pwhite of string | 
		     Other of char | 
		     Pempty | 
		     Error
    withtype LToken = Token * location

end