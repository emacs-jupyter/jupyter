EMACS ?= emacs
SHELL ?= bash
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

# Build the zmq module.
.PHONY: zmq
ifneq ($(APPVEYOR),)
# There are issues with string quoting in Appveyor which causes the normal
# evaluated command to give a parsing error so we just make this a no-op in
# Appveyor. The pre-built binaries are used when building in Appveyor anyways.
zmq:
else
zmq: cask
	$(CASK) emacs -Q -batch --eval "(progn (fset 'read-string (lambda (&rest _) \"y\")) (require 'zmq))"
endif

.PHONY: dev
dev: cask
	$(CASK) install
	$(CASK) update

test: export EMACSLOADPATH = $(shell $(CASK) load-path)

.PHONY: test
test: zmq
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
compile: zmq
	$(CASK) build
