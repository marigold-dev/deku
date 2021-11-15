# last refactor: 2021-05-05
.ONESHELL:

all: test

# Use install-deps instead of 'install' because usually 'make install' adds a
# binary to the system path and we don't want to confuse users
install-deps:
#	Install ligo/tezos specific system-level dependencies
	sudo scripts/install_native_dependencies.sh
	scripts/install_build_environment.sh # TODO: or scripts/install_opam.sh ?

build-deps:
	export PATH="/usr/local/bin$${PATH:+:}$${PATH:-}"
#	Create opam dev switch locally for use with Ligo, add merlin/etc
	if [ ! -d "./_opam" ];
	then scripts/setup_switch.sh;
	fi
	eval $$(opam config env)
# NEW-PROTOCOL-TEMPORARY
	git submodule init
	git pull --recurse
# NEW-PROTOCOL-TEMPORARY
#	Install OCaml build dependencies for Ligo
	scripts/install_vendors_deps.sh

build: build-deps
	export PATH="/usr/local/bin$${PATH:+:}$${PATH:-}"
	eval $$(opam config env)
#	Build Ligo for local dev use
	scripts/build_ligo_local.sh

test: build
	export PATH="/usr/local/bin$${PATH:+:}$${PATH:-}"
	eval $$(opam config env)
	scripts/test_ligo.sh

clean:
	dune clean
	rm -fr _coverage_all _coverage_cli _coverage_ligo

coverage:
	eval $$(opam config env)
	find . -name '*.coverage' | xargs rm -f
	dune runtest --instrument-with bisect_ppx --force
	bisect-ppx-report html -o ./_coverage_all --title="LIGO overall test coverage"
	bisect-ppx-report summary --per-file
