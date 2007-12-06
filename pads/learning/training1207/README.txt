How to define a training description:

    1)  Only use base types listed below & literal punctuation.
             -- eg: no Pstring
    2)  Define the description that you would like the tokenizer to infer 
          (not necessarily the description that you hope the system will
           eventually output)

question:  what happens to unseparated literals?  eg: " ["
question: do we need Pignore when you can't figure out what to do?
question: what happens to array separators (do they get printed?)
q: should time & date tokens be separated?

Token Definitions 
from Qian's Email re 11/16/2007 12:56 AM:

Meta-Rule:
-- Tokenization shouldn't break nesting of well-nested parens or quotes. 
-- ie: ( ) [ ] { } " "  ' ' < >

Pint		|	numbers									
Pfloat		|	numbers "." numbers 
Ptimez		|	2 digits ":" 2 digits ":" 2 digits, followed by an option of "am" or "pm", and an option of timezone
Pip		|	1-3 digits "." 1-3 digits "." 1-3 digits "." 1-3 digits	
Phostname	|	a series of labels, with each label being separated by "." (restrictions on valid host names can be found at http://en.wikipedia.org/wiki/Hostname)
Pemail		|	any strings that may have numbers, letters, "_" and "-", but doesn't begin with "_" or "-", followed by "@", followed by hostname
Pmac		|	6 2-digit hex numbers separated by ":" or "-"
Pdate		|	any strings containing numbers and letters that represent a date
Ppath		|	a series of filename separated by "/" or "\" (filename limitations can be found at http://en.wikipedia.org/wiki/Filename)
Purl		|	protocal string, hostname, filename or ip separated by various punctuations
Pword		|	strict English world containing letters and "'"
Pid		|	any strings that are composed of numbers, letters, "-", "_" and "."
PbXML		|	XML data token
PeXML		|	another XML data token
Pwhite		|	white spaces
Pmessage        |       any characters to end of line or delimiting scope like ""?  Takes
                        terminating character as argument.

Pdot		|	"."
Pslash		|	"/"
Psemicolon	|	";"
Pbar		|	"|"
Pless		|	"<"
.
.
.
(This is a group of punctuation tokens, with each punctuation being a unique token)

Pblob		|	any strings that can't be represented by the above tokens, but must have a terminated character, like "\n" or "\r" 