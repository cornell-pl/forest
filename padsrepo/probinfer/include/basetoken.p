Ptypedef Pstring_ME(:"/[0-9]+/":) PPint;

Ptypedef Pstring_ME(:"/[0-9]+\\.[0-9]+/":) PPfloat;

Ptypedef Pstring_ME(:"/([0-9]{2}):([0-9]{2}):([0-9]{2})([ ]*(am|AM|pm|PM))?([#\\t]+([+-][0-1][0-9]00))?/":) PPtime;

Ptypedef Pstring_ME(:"/((Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)|(0?[1-9]|1[0-2]))\\/([1-9]|[1-2][0-9]|0[1-9]|3[0-1])\\/([0-2][0-9]{3})|([1-9]|[1-2][0-9]|0[1-9]|3[0-1])\\/((Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)|(0?[1-9]|1[0-2]))\\/([0-2][0-9]{3})|([0-2][0-9]{3})\\/((Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)|(0?[1-9]|1[0-2]))\\/([1-9]|[1-2][0-9]|0[1-9]|3[0-1])|((Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)|(0?[1-9]|1[0-2]))\\-([1-9]|[1-2][0-9]|0[1-9]|3[0-1])\\-([0-2][0-9]{3})|([1-9]|[1-2][0-9]|0[1-9]|3[0-1])\\-((Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)|(0?[1-9]|1[0-2]))\\-([0-2][0-9]{3})|([0-2][0-9]{3})\\-((Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)|(0?[1-9]|1[0-2]))\\-([1-9]|[1-2][0-9]|0[1-9]|3[0-1])|((Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)|(0?[1-9]|1[0-2]))\\.([1-9]|[1-2][0-9]|0[1-9]|3[0-1])\\.([0-2][0-9]{3})|([1-9]|[1-2][0-9]|0[1-9]|3[0-1])\\.((Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)|(0?[1-9]|1[0-2]))\\.([0-2][0-9]{3})|([0-2][0-9]{3})\\.((Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)|(0?[1-9]|1[0-2]))\\.([1-9]|[1-2][0-9]|0[1-9]|3[0-1])|((Mon|Monday|Tue|Tuesday|Wed|Wednesday|Thu|Thursday|Fri|Friday|Sat|Saturday|Sun|Sunday|mon|tue|wed|thu|fri|sat|sun),?[ \\t]+)?(Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)[ \\t]+([1-9]|[1-2][0-9]|0[1-9]|3[0-1])(,[ \\t]+([0-2][0-9]{3}))?|((Mon|Monday|Tue|Tuesday|Wed|Wednesday|Thu|Thursday|Fri|Friday|Sat|Saturday|Sun|Sunday|mon|tue|wed|thu|fri|sat|sun),?[ \\t]+)?([1-9]|[1-2][0-9]|0[1-9]|3[0-1])[ \\t]+(Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)(,[ \\t]+([0-2][0-9]{3}))?/":) PPdate;

Ptypedef Pstring_ME(:"/([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3})/":) PPip;

Ptypedef Pstring_ME(:"/((([0-9A-Za-z]|[0-9A-Za-z][0-9A-Za-z]|[0-9A-Za-z][A-Za-z0-9_\\-]{1,61}[0-9A-Za-z])\\.)+(com|net|edu|org|gov)(\\.[a-z][a-z])?)/":) PPhostname;

Ptypedef Pstring_ME(:"/([0-9A-Za-z!#$%&'*+\\-/=?\\^_`{|}~]([.]?[a-zA-Z0-9!#$%&'*+\\-/=?\\^_`{|}~]){1,61}[a-zA-Z0-9!#$%&'*+\\-/=?\\^_`{|}~]|[a-zA-Z0-9!#$%&'*+\\-/=?\\^_`{|}~]|[a-zA-Z0-9!#$%&'*+\\-/=?\\^_`{|}~][a-zA-Z0-9!#$%&'*+\\-/=?\\^_`{|}~])@((([0-9A-Za-z]|[0-9A-Za-z][0-9A-Za-z]|[0-9A-Za-z][A-Za-z0-9_\\-]{1,61}[0-9A-Za-z])\\.)+([0-9A-Za-z]|[0-9A-Za-z][0-9A-Za-z]|[0-9A-Za-z][A-Za-z0-9_\\-]{1,61}[0-9A-Za-z]))/":) PPemail;

Ptypedef Pstring_ME(:"/(([0-9a-fA-F]{2})(:|\\-)){5}([0-9a-fA-F]{2})/":) PPmac;

Ptypedef Pstring_ME(:"/(\\/([^\\/\\\\?*:<>\"\\[\\] ]+)){2}(\\/([^\\/\\\\?*:<>\"\\[\\] ]+))*\\/?|(([^\\/\\\\?*:<>\"\\[\\] ]+)\\/){2}(([^\\/\\\\?*:<>\"\\[\\] ]+)\\/)*([^\\/\\\\?*:<>\"\\[\\] ]+)?|\\\\?(\\\\([^\\/\\\\?*:<>\"\\[\\] ]+)){2}(\\\\([^\\/\\\\?*:<>\"\\[\\] ]+))*\\\\?|(([^\\/\\\\?*:<>\"\\[\\] ]+)\\\\){2}(([^\\/\\\\?*:<>\"\\[\\] ]+)\\\\)*([^\\/\\\\?*:<>\"\\[\\] ]+)?/":) PPpath;

Ptypedef Pstring_ME(:"/(http|ftp|https):\\/\\/((([0-9A-Za-z]|[0-9A-Za-z][0-9A-Za-z]|[0-9A-Za-z][A-Za-z0-9_\\-]{1,61}[0-9A-Za-z])\\.)+([0-9A-Za-z]|[0-9A-Za-z][0-9A-Za-z]|[0-9A-Za-z][A-Za-z0-9_\\-]{1,61}[0-9A-Za-z]))(:([1-9][0-9]*))?\\/?(\\/([^\\/\\\\?*:<>\"\\[\\] ]+))*\\/?(\\?)?\\&?([^&=]+=[^&]*(\\&[^&=]+=[^&]*)*\\&?)?(#([0-9A-Za-z][A-Za-z0-9_\\-]*))?|(http|ftp|https):\\/\\/(([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3}))(:([1-9][0-9]*))?\\/?(\\/([^\\/\\\\?*:<>\"\\[\\] ]+))*\\/?(\\?)?\\&?([^&=]+=[^&]*(\\&[^&=]+=[^&]*)*\\&?)?(#([0-9A-Za-z][A-Za-z0-9_\\-]*))?/":) PPurl;

Ptypedef Pstring_ME(:"/((([0-9A-Za-z]|[0-9A-Za-z][0-9A-Za-z]|[0-9A-Za-z][A-Za-z0-9_\\-]{1,61}[0-9A-Za-z])\\.)+([0-9A-Za-z]|[0-9A-Za-z][0-9A-Za-z]|[0-9A-Za-z][A-Za-z0-9_\\-]{1,61}[0-9A-Za-z]))(:([1-9][0-9]*))?\\/?(\\/([^\\/\\\\?*:<>\"\\[\\] ]+))*\\/?(\\?)?\\&?([^&=]+=[^&]*(\\&[^&=]+=[^&]*)*\\&?)?(#([0-9A-Za-z][A-Za-z0-9_\\-]*))?|(http|ftp|https):\\/\\/(([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3}))(:([1-9][0-9]*))?\\/?(\\/([^\\/\\\\?*:<>\"\\[\\] ]+))*\\/?(\\?)?\\&?([^&=]+=[^&]*(\\&[^&=]+=[^&]*)*\\&?)?(#([0-9A-Za-z][A-Za-z0-9_\\-]*))?/":) PPurlbody;

Ptypedef Pstring_ME(:"/[A-Za-z'\\-]+/":) PPword;

Ptypedef Pstring_ME(:"/[0-9A-Za-z\\-_\\.]+/":) PPid;

Ptypedef Pstring_ME(:"/\\<([a-zA-Z])+\\>/":) PPbXML;

Ptypedef Pstring_ME(:"/\\<\\/[^>]+\\>/":) PPeXML;

Ptypedef Pstring_ME(:"/[ \\t\\r\\n]+/":) PPwhite;

Ptypedef Pstring_ME(:"/\".+\"|[[].+[]]|[(].+[)]|[{].+[}]|.+\\?$/":) PPmessage;

Ptypedef Pstring_ME(:"/(d|\\-)((r|\\-)(w|\\-)(x|-))[3]/":) PPpermission;

Ptypedef Pstring_ME(:"/[.]/":) PPdot;

Ptypedef Pstring_ME(:"/[/]/":) PPslash;

Ptypedef Pstring_ME(:"/[\\]/":) PPbackslash;

Ptypedef Pstring_ME(:"/[;]/":) PPsemicolon;

Ptypedef Pstring_ME(:"/[|]/":) PPbar;

Ptypedef Pstring_ME(:"/[<]/":) PPless;

Ptypedef Pstring_ME(:"/[~]/":) PPtilde;

Ptypedef Pstring_ME(:"/[`]/":) PPbquote;

Ptypedef Pstring_ME(:"/[!]/":) PPbang;

Ptypedef Pstring_ME(:"/[@]/":) PPat;

Ptypedef Pstring_ME(:"/[#]/":) PPhash;

Ptypedef Pstring_ME(:"/[$]/":) PPdollar;

Ptypedef Pstring_ME(:"/[%]/":) PPpercent;

Ptypedef Pstring_ME(:"/[^]/":) PPcaret;

Ptypedef Pstring_ME(:"/[&]/":) PPand;

Ptypedef Pstring_ME(:"/[*]/":) PPstar;

Ptypedef Pstring_ME(:"/[(]/":) PPlpar;

Ptypedef Pstring_ME(:"/[)]/":) PPrpar;

Ptypedef Pstring_ME(:"/[-]/":) PPdash;

Ptypedef Pstring_ME(:"/[_]/":) PPunderscore;

Ptypedef Pstring_ME(:"/[+]/":) PPplus;

Ptypedef Pstring_ME(:"/[=]/":) PPequa;

Ptypedef Pstring_ME(:"/[{]/":) PPlbrac;

Ptypedef Pstring_ME(:"/[}]/":) PPrbrac;

Ptypedef Pstring_ME(:"/[[]/":) PPlsqubrac;

Ptypedef Pstring_ME(:"/[]]/":) PPrsqubrac;

Ptypedef Pstring_ME(:"/[:]/":) PPcolon;

Ptypedef Pstring_ME(:"/[\"]/":) PPdquote;

Ptypedef Pstring_ME(:"/[']/":) PPquote;

Ptypedef Pstring_ME(:"/[>]/":) PPgreater;

Ptypedef Pstring_ME(:"/[,]/":) PPcomma;

Ptypedef Pstring_ME(:"/[?]/":) PPquestion;

Ptypedef Pstring_ME(:"/^([0-9]+)([0-9]+\\.[0-9]+)(([0-9]{2}):([0-9]{2}):([0-9]{2})([ ]*(am|AM|pm|PM))?([#\\t]+([+-][0-1][0-9]00))?)(((Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)|(0?[1-9]|1[0-2]))\\/([1-9]|[1-2][0-9]|0[1-9]|3[0-1])\\/([0-2][0-9]{3})|([1-9]|[1-2][0-9]|0[1-9]|3[0-1])\\/((Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)|(0?[1-9]|1[0-2]))\\/([0-2][0-9]{3})|([0-2][0-9]{3})\\/((Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)|(0?[1-9]|1[0-2]))\\/([1-9]|[1-2][0-9]|0[1-9]|3[0-1])|((Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)|(0?[1-9]|1[0-2]))\\-([1-9]|[1-2][0-9]|0[1-9]|3[0-1])\\-([0-2][0-9]{3})|([1-9]|[1-2][0-9]|0[1-9]|3[0-1])\\-((Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)|(0?[1-9]|1[0-2]))\\-([0-2][0-9]{3})|([0-2][0-9]{3})\\-((Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)|(0?[1-9]|1[0-2]))\\-([1-9]|[1-2][0-9]|0[1-9]|3[0-1])|((Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)|(0?[1-9]|1[0-2]))\\.([1-9]|[1-2][0-9]|0[1-9]|3[0-1])\\.([0-2][0-9]{3})|([1-9]|[1-2][0-9]|0[1-9]|3[0-1])\\.((Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)|(0?[1-9]|1[0-2]))\\.([0-2][0-9]{3})|([0-2][0-9]{3})\\.((Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)|(0?[1-9]|1[0-2]))\\.([1-9]|[1-2][0-9]|0[1-9]|3[0-1])|((Mon|Monday|Tue|Tuesday|Wed|Wednesday|Thu|Thursday|Fri|Friday|Sat|Saturday|Sun|Sunday|mon|tue|wed|thu|fri|sat|sun),?[ \\t]+)?(Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)[ \\t]+([1-9]|[1-2][0-9]|0[1-9]|3[0-1])(,[ \\t]+([0-2][0-9]{3}))?|((Mon|Monday|Tue|Tuesday|Wed|Wednesday|Thu|Thursday|Fri|Friday|Sat|Saturday|Sun|Sunday|mon|tue|wed|thu|fri|sat|sun),?[ \\t]+)?([1-9]|[1-2][0-9]|0[1-9]|3[0-1])[ \\t]+(Jan|jan|Feb|feb|Mar|mar|Apr|apr|May|may|Jun|jun|Jul|jul|Aug|aug|Sep|sep|Oct|oct|Nov|nov|Dec|dec|January|February|March|April|May|June|July|August|September|October|November|December)(,[ \\t]+([0-2][0-9]{3}))?)(([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3}))(((([0-9A-Za-z]|[0-9A-Za-z][0-9A-Za-z]|[0-9A-Za-z][A-Za-z0-9_\\-]{1,61}[0-9A-Za-z])\\.)+(com|net|edu|org|gov)(\\.[a-z][a-z])?))(([0-9A-Za-z!#$%&'*+\\-/=?\\^_`{|}~]([.]?[a-zA-Z0-9!#$%&'*+\\-/=?\\^_`{|}~]){1,61}[a-zA-Z0-9!#$%&'*+\\-/=?\\^_`{|}~]|[a-zA-Z0-9!#$%&'*+\\-/=?\\^_`{|}~]|[a-zA-Z0-9!#$%&'*+\\-/=?\\^_`{|}~][a-zA-Z0-9!#$%&'*+\\-/=?\\^_`{|}~])@((([0-9A-Za-z]|[0-9A-Za-z][0-9A-Za-z]|[0-9A-Za-z][A-Za-z0-9_\\-]{1,61}[0-9A-Za-z])\\.)+([0-9A-Za-z]|[0-9A-Za-z][0-9A-Za-z]|[0-9A-Za-z][A-Za-z0-9_\\-]{1,61}[0-9A-Za-z])))((([0-9a-fA-F]{2})(:|\\-)){5}([0-9a-fA-F]{2}))((\\/([^\\/\\\\?*:<>\"\\[\\] ]+)){2}(\\/([^\\/\\\\?*:<>\"\\[\\] ]+))*\\/?|(([^\\/\\\\?*:<>\"\\[\\] ]+)\\/){2}(([^\\/\\\\?*:<>\"\\[\\] ]+)\\/)*([^\\/\\\\?*:<>\"\\[\\] ]+)?|\\\\?(\\\\([^\\/\\\\?*:<>\"\\[\\] ]+)){2}(\\\\([^\\/\\\\?*:<>\"\\[\\] ]+))*\\\\?|(([^\\/\\\\?*:<>\"\\[\\] ]+)\\\\){2}(([^\\/\\\\?*:<>\"\\[\\] ]+)\\\\)*([^\\/\\\\?*:<>\"\\[\\] ]+)?)((http|ftp|https):\\/\\/((([0-9A-Za-z]|[0-9A-Za-z][0-9A-Za-z]|[0-9A-Za-z][A-Za-z0-9_\\-]{1,61}[0-9A-Za-z])\\.)+([0-9A-Za-z]|[0-9A-Za-z][0-9A-Za-z]|[0-9A-Za-z][A-Za-z0-9_\\-]{1,61}[0-9A-Za-z]))(:([1-9][0-9]*))?\\/?(\\/([^\\/\\\\?*:<>\"\\[\\] ]+))*\\/?(\\?)?\\&?([^&=]+=[^&]*(\\&[^&=]+=[^&]*)*\\&?)?(#([0-9A-Za-z][A-Za-z0-9_\\-]*))?|(http|ftp|https):\\/\\/(([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3}))(:([1-9][0-9]*))?\\/?(\\/([^\\/\\\\?*:<>\"\\[\\] ]+))*\\/?(\\?)?\\&?([^&=]+=[^&]*(\\&[^&=]+=[^&]*)*\\&?)?(#([0-9A-Za-z][A-Za-z0-9_\\-]*))?)(((([0-9A-Za-z]|[0-9A-Za-z][0-9A-Za-z]|[0-9A-Za-z][A-Za-z0-9_\\-]{1,61}[0-9A-Za-z])\\.)+([0-9A-Za-z]|[0-9A-Za-z][0-9A-Za-z]|[0-9A-Za-z][A-Za-z0-9_\\-]{1,61}[0-9A-Za-z]))(:([1-9][0-9]*))?\\/?(\\/([^\\/\\\\?*:<>\"\\[\\] ]+))*\\/?(\\?)?\\&?([^&=]+=[^&]*(\\&[^&=]+=[^&]*)*\\&?)?(#([0-9A-Za-z][A-Za-z0-9_\\-]*))?|(http|ftp|https):\\/\\/(([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3}))(:([1-9][0-9]*))?\\/?(\\/([^\\/\\\\?*:<>\"\\[\\] ]+))*\\/?(\\?)?\\&?([^&=]+=[^&]*(\\&[^&=]+=[^&]*)*\\&?)?(#([0-9A-Za-z][A-Za-z0-9_\\-]*))?)([A-Za-z'\\-]+)([0-9A-Za-z\\-_\\.]+)(\\<\\/[^>]+\\>)(\\<([a-zA-Z])+\\>)([ \\t\\r\\n]+)(\".+\"|[[].+[]]|[(].+[)]|[{].+[}]|.+\\?$)((d|\\-)((r|\\-)(w|\\-)(x|-))[3])([.])([/])([\\])([;])([|])([<])([~])([`])([!])([@])([#])([$])([%])([^])([&])([*])([(])([)])([-])([_])([+])([=])([{])([}])([[])([]])([:])([\"])(['])([>])([,])([?])/":) PPblob;

