top_srcdir = $(shell pwd)
builddir = _build
INSTALL_SUDO =
OCAMLBUILD = ocamlbuild
OCAMLFIND = ocamlfind
OCAML_INSTALL = $(INSTALL_SUDO) $(OCAMLFIND) install
OCAML_REMOVE = $(INSTALL_SUDO) $(OCAMLFIND) remove
OCAML_INSTALL_OPTIONS =
exe = byte

-include Makefile.config

scan_mlpack = $(wildcard $(patsubst %,%.$(2),$(shell cat $(1))))
scansub_mlpacks = $(strip \
    $(foreach pk, $(call scan_mlpack,$(1),mlpack), \
	$(pk) $(call scansub_mlpacks,$(pk))))
scanrec_mlpacks = $(1) $(call scansub_mlpacks, $(1))

ocaml_source_exts = ml mli mll mly
ocaml_PACKS = $(call scanrec_mlpacks,$(wildcard *.mlpack))
ocaml_SOURCES = $(strip \
    $(foreach pk, $(ocaml_PACKS), \
	$(foreach ext, $(ocaml_source_exts), $(call scan_mlpack,$(pk),$(ext)))))

ocamlbuild_stamp = _build/ocamlbuild.stamp

ocaml_native = $(ocaml_PACKS:%.mlpack=%.cmxa)
ocaml_byte = $(ocaml_PACKS:%.mlpack=%.cma)

INSTFILES = META \
	$(filter %.mli, $(ocaml_SOURCES)) \
	$(foreach ext, ml mll mly,\
	    $(patsubst %.$(ext),_build/%.cmi,\
		$(filter %.$(ext),$(ocaml_SOURCES)))) \
	$(ocaml_PACKS:%.mlpack=_build/%.cma) \
	$(ocaml_PACKS:%.mlpack=_build/%.cmxa)

TESTS = $(wildcard tests/test_*.ml)

.PHONY: all check clean install uninstall

all: $(ocamlbuild_stamp)

clean:
	$(OCAMLBUILD) -clean

install: all
	$(OCAML_REMOVE) iplogic $(OCAML_INSTALL_OPTIONS) 2>/dev/null || :
	$(OCAML_INSTALL) iplogic $(OCAML_INSTALL_OPTIONS) $(INSTFILES)
uninstall:
	$(OCAML_REMOVE) iplogic $(OCAML_INSTALL_OPTIONS)

$(ocamlbuild_stamp): $(ocaml_SOURCES) $(ocaml_PACKS)
	$(OCAMLBUILD) -use-ocamlfind $(ocaml_byte) $(ocaml_native)
	touch $@

check:
	$(OCAMLBUILD) $(TESTS:%.ml=%.$(exe))
	@error_count=0; \
	for test in $(TESTS:%.ml=%.$(exe)); do \
	    if _build/$$test; then \
		echo "OK $$test"; \
	    else \
		echo "!! $$test exited with error $$?"; \
		error_count=`expr $$error_count + 1`; \
	    fi; \
	done; \
	test $$error_count -eq 0 || { \
	    echo "$$error_count tests failed"; \
	    exit 1; \
	}
