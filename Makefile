EMACS ?= emacs
ELDEV ?= eldev

FILES = $(wildcard *.el)
ELCFILES = $(FILES:.el=.elc)
TESTFILES = $(foreach file,$(wildcard test/*.el),-l $(file))
TESTSELECTORS =

ifneq ($(TAGS),)
comma := ,
TESTSELECTORS := $(foreach tag,$(subst $(comma), ,$(TAGS)),"(tag $(tag))")
endif

ifneq ($(PATTERN),)
TESTSELECTORS := $(TESTSELECTORS) \"$(PATTERN)\"
endif

# ifneq ($(TESTSELECTORS),)
# TESTSELECTORS := (quote (or $(TESTSELECTORS)))
# endif

.PHONY: all
all: compile

.PHONY: eldev
eldev:
ifeq ($(ELDEV),)
	$(error "Install eldev (https://github.com/doublep/eldev)")
endif

.PHONY: test
test:
	$(ELDEV) test $(TESTSELECTORS)

.PHONY: clean
clean:
	make -C js clean
	@rm $(ELCFILES) 2>/dev/null || true

.PHONY: clean-eldev
clean-eldev:
	@rm -rf .eldev/ 2>/dev/null || true

.PHONY: widgets
widgets:
	make -C js

.PHONY: compile
compile:
	$(ELDEV) compile
