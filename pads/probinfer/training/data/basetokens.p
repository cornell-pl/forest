Ptypedef Pstring_ME(:"/[-~]?([0-9]+)/":) PPint;

Ptypedef Pstring_ME(:"/([-~]?([0-9]+))|[-~]?([0-9]+)\\.([0-9]+)|[-~]?[1-9]\\.([0-9]+)(E|e)[-~]?([0-9]+)/":) PPfloat;

Ptypedef Pstring_ME(:"/([0-9]{2}):([0-9]{2}):([0-9]{2})([ ]*(am|AM|pm|PM))?([ \\t]+([+-][0-1][0-9]00))?|([0-9]{2}):([0-9]{2})/":) PPtime;

Ptypedef Pstring_ME(:"/((Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)|(0?[1-9]|1[0-2]))\\/([1-9]|[1-2][0-9]|0[1-9]|3[0-1])\\/([0-2][0-9]{3})|([1-9]|[1-2][0-9]|0[1-9]|3[0-1])\\/((Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)|(0?[1-9]|1[0-2]))\\/([0-2][0-9]{3})|([0-2][0-9]{3})\\/((Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)|(0?[1-9]|1[0-2]))\\/([1-9]|[1-2][0-9]|0[1-9]|3[0-1])|((Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)|(0?[1-9]|1[0-2]))\\-([1-9]|[1-2][0-9]|0[1-9]|3[0-1])\\-([0-2][0-9]{3})|([1-9]|[1-2][0-9]|0[1-9]|3[0-1])\\-((Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)|(0?[1-9]|1[0-2]))\\-([0-2][0-9]{3})|([0-2][0-9]{3})\\-((Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)|(0?[1-9]|1[0-2]))\\-([1-9]|[1-2][0-9]|0[1-9]|3[0-1])|((Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)|(0?[1-9]|1[0-2]))\\.([1-9]|[1-2][0-9]|0[1-9]|3[0-1])\\.([0-2][0-9]{3})|([1-9]|[1-2][0-9]|0[1-9]|3[0-1])\\.((Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)|(0?[1-9]|1[0-2]))\\.([0-2][0-9]{3})|([0-2][0-9]{3})\\.((Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)|(0?[1-9]|1[0-2]))\\.([1-9]|[1-2][0-9]|0[1-9]|3[0-1])|((Mon|Monday|Tue|Tuesday|Wed|Wednesday|Thu|Thursday|Fri|Friday|Sat|Saturday|Sun|Sunday|mon|tue|wed|thu|fri|sat|sun),?[ \\t]+)?(Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)[ \\t]+([1-9]|[1-2][0-9]|0[1-9]|3[0-1])(,[ \\t]+([0-2][0-9]{3}))?|((Mon|Monday|Tue|Tuesday|Wed|Wednesday|Thu|Thursday|Fri|Friday|Sat|Saturday|Sun|Sunday|mon|tue|wed|thu|fri|sat|sun),?[ \\t]+)?([1-9]|[1-2][0-9]|0[1-9]|3[0-1])[ \\t]+(Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)(,[ \\t]+([0-2][0-9]{3}))?/":) PPdate;

Ptypedef Pstring_ME(:"/([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3})/":) PPip;

Ptypedef Pstring_ME(:"/((([0-9A-Za-z_-]{1,63})\\.)+((com|net|edu|org|gov)|(com|net|edu|org|gov)\\.[a-z][a-z]))/":) PPhostname;

Ptypedef Pstring_ME(:"/(([0-9A-Za-z!#$%&'*+/=?\\^_`{|}~]|(\\-))([.]?([a-zA-Z0-9!#$%&'*+/=?\\^_`{|}~]|(\\-))){1,61}([a-zA-Z0-9!#$%&'*+/=?\\^_`{|}~]|(\\-))|([a-zA-Z0-9!#$%&'*+/=?\\^_`{|}~]|(\\-))|([a-zA-Z0-9!#$%&'*+/=?\\^_`{|}~]|(\\-))([a-zA-Z0-9!#$%&'*+/=?\\^_`{|}~]|(\\-)))@((([0-9A-Za-z_-]{1,63})\\.)+([0-9A-Za-z_-]{1,63}))/":) PPemail;

Ptypedef Pstring_ME(:"/(([0-9a-fA-F]{2})(:|\\-)){5}([0-9a-fA-F]{2})/":) PPmac;

Ptypedef Pstring_ME(:"/(\\/([^][\\/\\\\?*:<>+\"']+))(\\/([^][\\/\\\\?*:<>+\"']+))+\\/?|\\\\?(\\\\([^][\\/\\\\?*:<>+\"']+))(\\\\([^][\\/\\\\?*:<>+\"']+))+\\\\?/":) PPpath;

Ptypedef Pstring_ME(:"/(http|ftp|https):\\/\\/((([0-9A-Za-z_-]{1,63})\\.)+([0-9A-Za-z_-]{1,63}))(:([1-9][0-9]*))?\\/?(\\/([^][\\/\\\\?*:<>+\"']+))*\\/?(\\?)?\\&?([^&=]+=[^&]*(\\&[^&=]+=[^&]*)*\\&?)?(#([0-9A-Za-z][A-Za-z0-9_-]*))?|(http|ftp|https):\\/\\/(([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3}))(:([1-9][0-9]*))?\\/?(\\/([^][\\/\\\\?*:<>+\"']+))*\\/?(\\?)?\\&?([^&=]+=[^&]*(\\&[^&=]+=[^&]*)*\\&?)?(#([0-9A-Za-z][A-Za-z0-9_-]*))?/":) PPurl;

Ptypedef Pstring_ME(:"/((([0-9A-Za-z_-]{1,63})\\.)+([0-9A-Za-z_-]{1,63}))(:([1-9][0-9]*))?\\/?(\\/([^][\\/\\\\?*:<>+\"']+))*\\/?(\\?)?\\&?([^&=]+=[^&]*(\\&[^&=]+=[^&]*)*\\&?)?(#([0-9A-Za-z][A-Za-z0-9_-]*))?|\\/\\/(([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3}))(:([1-9][0-9]*))?\\/?(\\/([^][\\/\\\\?*:<>+\"']+))*\\/?(\\?)?\\&?([^&=]+=[^&]*(\\&[^&=]+=[^&]*)*\\&?)?(#([0-9A-Za-z][A-Za-z0-9_-]*))?/":) PPurlbody;

Ptypedef Pstring_ME(:"/[-A-Za-z']+/":) PPword;

Ptypedef Pstring_ME(:"/[-0-9A-Za-z_.]+/":) PPid;

Ptypedef Pstring_ME(:"/\\<([a-zA-Z])+\\>/":) PPbXML;

Ptypedef Pstring_ME(:"/\\<\\/[^>]+\\>/":) PPeXML;

Ptypedef Pstring_ME(:"/[ \\t\\r\\n]+/":) PPwhite;

Ptypedef Pstring_ME(:"/\\\"[^\"]+\\\"|[[][^]]+[]]|[(][^)]+[)]|[{][^}]+[}]|[^?]+\\?|:[^:]+/":) PPmessage;

Ptypedef Pstring_ME(:"/[-dl]([-r][-w][-xsStT]){3}/":) PPpermission;

Ptypedef Pstring_ME(:"/[\\\"'][-A-Za-z0-9_,;. ]+[\\\"']|[-A-Za-z0-9_,;. ]/":) PPtext;

Ptypedef Pstring_ME(:"/[0-9a-f]+/":) PPhstring;

Ptypedef Pstring_ME(:"/[.\\/\\\\;|<>~`!@#$%\\^&*()-_+={}[]:\"',?]/":) PPpunc;

Ptypedef Pstring_ME(:"/[.]/":) PPpunc_dot;

Ptypedef Pstring_ME(:"/[\\/]/":) PPpunc_slash;

Ptypedef Pstring_ME(:"/[\\\\]/":) PPpunc_bslash;

Ptypedef Pstring_ME(:"/[;]/":) PPpunc_scolon;

Ptypedef Pstring_ME(:"/[|]/":) PPpunc_bar;

Ptypedef Pstring_ME(:"/[<]/":) PPpunc_less;

Ptypedef Pstring_ME(:"/[~]/":) PPpunc_tilde;

Ptypedef Pstring_ME(:"/[`]/":) PPpunc_bquote;

Ptypedef Pstring_ME(:"/[!]/":) PPpunc_bang;

Ptypedef Pstring_ME(:"/[@]/":) PPpunc_at;

Ptypedef Pstring_ME(:"/[#]/":) PPpunc_hash;

Ptypedef Pstring_ME(:"/[$]/":) PPpunc_dollar;

Ptypedef Pstring_ME(:"/[%]/":) PPpunc_percent;

Ptypedef Pstring_ME(:"/[\\^]/":) PPpunc_caret;

Ptypedef Pstring_ME(:"/[&]/":) PPpunc_and;

Ptypedef Pstring_ME(:"/[*]/":) PPpunc_star;

Ptypedef Pstring_ME(:"/[(]/":) PPpunc_lpar;

Ptypedef Pstring_ME(:"/[)]/":) PPpunc_rpar;

Ptypedef Pstring_ME(:"/[-]/":) PPpunc_hyphen;

Ptypedef Pstring_ME(:"/[_]/":) PPpunc_underscore;

Ptypedef Pstring_ME(:"/[+]/":) PPpunc_plus;

Ptypedef Pstring_ME(:"/[=]/":) PPpunc_equa;

Ptypedef Pstring_ME(:"/[{]/":) PPpunc_lbrac;

Ptypedef Pstring_ME(:"/[}]/":) PPpunc_rbrac;

Ptypedef Pstring_ME(:"/[[]/":) PPpunc_lsqubrac;

Ptypedef Pstring_ME(:"/[]]/":) PPpunc_rsqubrac;

Ptypedef Pstring_ME(:"/[:]/":) PPpunc_colon;

Ptypedef Pstring_ME(:"/[\"]/":) PPpunc_dquote;

Ptypedef Pstring_ME(:"/[']/":) PPpunc_quote;

Ptypedef Pstring_ME(:"/[>]/":) PPpunc_greater;

Ptypedef Pstring_ME(:"/[,]/":) PPpunc_comma;

Ptypedef Pstring_ME(:"/[?]/":) PPpunc_question;

Ptypedef Pstring_ME(:"/[^0-9A-Za-z \\t\\r\\n]/":) PPchar;

