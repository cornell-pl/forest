:PACKAGE: ast

.SOURCE : ../include

CCFLAGS = $(CC.DEBUG) $(CC.DLL)

libpadsc-gen.c : libpadsc-gen-src.c
	if [ -f libpadsc-gen.c ]; then $(RM) libpadsc-gen.c; fi
	$(CC) -E libpadsc-gen-src.c | ./addnl.pl | cat > libpadsc-gen.c
