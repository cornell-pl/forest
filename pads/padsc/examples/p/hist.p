Punion last {
	    Pchar c;
	    Pint8 i;
};

Precord Pstruct entry_t {
			Pdate(:'|':) date;
			'|';
			Pip ip;
			'|';
			Pstring(:'|':) str;
			'|';
			Pint16 foo;
			'|';
			last l;			
} 
 
