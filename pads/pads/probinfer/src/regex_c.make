FILES = regex_c.c
H = LibH.libh
D = FFI
HF = ../libh.sml
CF = regex_c.cm
CPPO = -D__builtin_va_list=int

$(D)/$(CF): $(FILES)
	$(SMLNJ_BINDIR)/ml-nlffigen $(CPPO) -include $(HF) -libhandle $(H) -dir $(D) -cmfile $(CF) $^
