EMACS ?= emacs
CASK ?= $(shell command -v cask)

FILES = $(wildcard *.el)
ELCFILES = $(FILES:.el=.elc)
TESTFILES = $(foreach file,$(wildcard test/*.el),-l $(file))
TESTSELECTORS =

ifneq ($(TAGS),)
comma := ,
TESTSELECTORS := $(foreach tag,$(subst $(comma), ,$(TAGS)),(tag $(tag)))
endif

ifneq ($(PATTERN),)
TESTSELECTORS := $(TESTSELECTORS) \"$(PATTERN)\"
endif

ifneq ($(TESTSELECTORS),)
TESTSELECTORS := (quote (or $(TESTSELECTORS)))
endif

.PHONY: all
all: compile

.PHONY: cask
cask:
ifeq ($(CASK),)
	$(error "Install cask (https://github.com/cask/cask)")
endif

.PHONY: dev
dev: cask
	$(CASK) install
	$(CASK) update

test: export EMACSLOADPATH = $(shell $(CASK) load-path)

.PHONY: test
test:
	$(EMACS) -nw -Q -batch -l ert $(TESTFILES) \
	    --eval "(ert-run-tests-batch-and-exit $(TESTSELECTORS))"

.PHONY: clean
clean:
	make -C js clean
	@rm $(ELCFILES) 2>/dev/null || true

.PHONY: clean-cask
clean-cask:
	@rm -rf .cask/ 2>/dev/null || true

.PHONY: widgets
widgets:
	make -C js

.PHONY: compile
compile:
	$(CASK) build
