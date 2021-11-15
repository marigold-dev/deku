include Makefile.coq

HIDE := $(if $(VERBOSE),,@)

Makefile.coq:
	coq_makefile -f _CoqProject -o Makefile.coq

clean::
	$(HIDE)find . -name .\*.aux -type f -delete
