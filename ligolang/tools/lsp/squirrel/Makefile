PACKAGE := squirrel

STACK_DEV_OPTIONS = --fast --ghc-options '-Werror -freverse-errors' --file-watch

.DEFAULT_GOAL := all

all: build

FORCE:

grammar: ; $(MAKE) -C $@

clean:
	$(RM) -r grammar/*/src
	$(RM) grammar/*/binding.gyp
	stack clean

build: grammar
	stack build $(STACK_DEV_OPTIONS)

.PHONY: all clean grammar build FORCE
