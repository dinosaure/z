BUILDDIR=../_build/test
$(shell mkdir -p $(BUILDDIR))
OCAMLDIR=$(shell ocamlopt -where)

CC:= $(shell ocamlfind ocamlc -config | awk '/^bytecomp_c_compiler/ {for(i=2;i<=NF;i++) printf "%s " ,$$i}')
LD=$(CC)

OSTYPE:=$(shell ocamlfind ocamlc -config | awk '/^os_type:/ {print $$2}')

ifeq ($(OSTYPE),$(filter $(OSTYPE),Win32 Cygwin))
EXTEXE=.exe
else
EXTEXE=
endif

all: $(BUILDDIR)/test$(EXTEXE)

clean:
	rm -rf $(BUILDDIR)

$(BUILDDIR)/test$(EXTEXE): $(BUILDDIR)/test.o
	$(LD) -o $@ $< -L$(BUILDDIR)/.. -lminiz

$(BUILDDIR)/test.o: test.c
	$(CC) -g -c -o $@ -I $(OCAMLDIR) -I $(BUILDDIR)/../generated  $<

.PHONY: all clean
