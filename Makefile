BUILDDIR=_build
VPATH=$(BUILDDIR)
OCAMLDIR=$(shell ocamlopt -where)
$(shell mkdir -p $(BUILDDIR) $(BUILDDIR)/lib $(BUILDDIR)/stub $(BUILDDIR)/rev $(BUILDDIR)/stub_generator $(BUILDDIR)/test $(BUILDDIR)/generated)
PACKAGES=bigstringaf,optint,checkseum.c,fmt,ctypes.stubs,ctypes.foreign

# The files used to build the stub generator.
GENERATOR_FILES=$(BUILDDIR)/lib/dd.cmx \
		$(BUILDDIR)/lib/zz.cmx \
                $(BUILDDIR)/rev/bindings.cmx \
                $(BUILDDIR)/stub_generator/generate.cmx

# The files from which we'll build a shared library.
INTFILES=$(BUILDDIR)/lib/dd.cmi \
         $(BUILDDIR)/lib/zz.cmi

LIBFILES=$(BUILDDIR)/lib/dd.cmx \
	 $(BUILDDIR)/lib/zz.cmx \
         $(BUILDDIR)/rev/bindings.cmx \
         $(BUILDDIR)/generated/miniz_bindings.cmx \
         $(BUILDDIR)/rev/apply_bindings.cmx \
         $(BUILDDIR)/generated/miniz.o

CAML_INIT=$(BUILDDIR)/stub/init.o

# The files that we'll generate
GENERATED=$(BUILDDIR)/generated/miniz.h \
          $(BUILDDIR)/generated/miniz.c \
          $(BUILDDIR)/generated/miniz_bindings.ml

OSTYPE:=$(shell ocamlfind ocamlc -config | awk '/^os_type:/ {print $$2}')
SYSTEM:=$(shell ocamlfind ocamlc -config | awk '/^system:/ {print $$2}')
EXTDLL:=$(shell ocamlfind ocamlc -config | awk '/^ext_dll:/ {print $$2}')
CC:= $(shell ocamlfind ocamlc -config | awk '/^bytecomp_c_compiler/ {for(i=2;i<=NF;i++) printf "%s " ,$$i}')

ifeq ($(OSTYPE),$(filter $(OSTYPE),Win32 Cygwin))
EXTEXE=.exe
else
EXTEXE=
endif

GENERATOR=$(BUILDDIR)/generate$(EXTEXE)

all: sharedlib

sharedlib: $(BUILDDIR)/libminiz$(EXTDLL)

ifeq ($(OSTYPE),$(filter $(OSTYPE),Win32 Cygwin))
$(BUILDDIR)/libminiz$(EXTDLL): $(CAML_INIT) $(LIBFILES)
	ocamlfind opt -o $@ -linkpkg -output-obj -verbose -package $(PACKAGES) $^
else ifeq ($(SYSTEM),$(filter $(SYSTEM),macosx))
$(BUILDDIR)/libminiz$(EXTDLL): $(CAML_INIT) $(LIBFILES)
	ocamlfind opt -o $@ -linkpkg -runtime-variant _pic -verbose -ccopt -dynamiclib -package $(PACKAGES) $^
else
$(BUILDDIR)/libminiz$(EXTDLL): $(CAML_INIT) $(LIBFILES)
	ocamlfind opt -o $@ -linkpkg -output-obj -runtime-variant _pic -verbose -package $(PACKAGES) $^
endif

stubs: $(GENERATED)

$(BUILDDIR)/stub/%.o:
	ocamlc -g -c stub/init.c
	mv init.o $@

$(GENERATED): $(GENERATOR)
	$(GENERATOR) $(BUILDDIR)/generated

$(BUILDDIR)/%.o: %.c
	$(CC) -g -c -o $@ -fPIC -I $(shell ocamlfind query ctypes) -I $(OCAMLDIR) -I $(OCAMLDIR)/../ctypes $<

$(BUILDDIR)/%.cmi: %.mli
	ocamlfind opt -I $(BUILDDIR)/lib -I $(BUILDDIR)/generated -I $(BUILDDIR)/rev -package $(PACKAGES) -c $< -o $@

$(BUILDDIR)/rev/%.cmx: rev/%.ml
	ocamlfind opt -O3 -unbox-closures -unbox-closures-factor 20 -c -o $@ -I $(BUILDDIR)/lib -I $(BUILDDIR)/generated -I $(BUILDDIR)/rev -package $(PACKAGES) $<

$(BUILDDIR)/generated/%.cmx: generated/%.ml
	ocamlfind opt -O3 -unbox-closures -unbox-closures-factor 20 -c -o $@ -I $(BUILDDIR)/lib -I $(BUILDDIR)/generated -I $(BUILDDIR)/rev -package $(PACKAGES) $<

$(BUILDDIR)/stub_generator/%.cmx: stub_generator/%.ml
	ocamlfind opt -O3 -unbox-closures -unbox-closures-factor 20 -c -o $@ -I $(BUILDDIR)/lib -I $(BUILDDIR)/generated -I $(BUILDDIR)/rev -package $(PACKAGES) $<

$(BUILDDIR)/%.cmx: %.ml $(BUILDDIR)/%.cmi
	ocamlfind opt -O3 -unbox-closures -unbox-closures-factor 20 -c -o $@ -I $(BUILDDIR)/lib -I $(BUILDDIR)/generated -I $(BUILDDIR)/rev -package $(PACKAGES) $<

$(GENERATOR): $(GENERATOR_FILES)
	ocamlfind opt -o $@ -linkpkg -package $(PACKAGES) -I $(BUILDDIR)/lib $^

clean:
	rm -rf $(BUILDDIR)

test: all
	$(MAKE) -C $@
ifeq ($(OSTYPE),Win32)
	PATH="$(BUILDDIR):$(PATH)" _build/test/test$(EXTEXE) test/file > test/file.z
else
	LD_LIBRARY_PATH=$(BUILDDIR) _build/test/test$(EXTEXE) -i test/file.z > test/file.o && \
  diff test/file.o test/file
endif

.PHONY: test
