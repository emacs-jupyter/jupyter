EMACS ?= emacs
SHELL = bash
CASK ?= $(shell command -v cask)

FILES = $(wildcard *.el)
ELCFILES = $(FILES:.el=.elc)

ifneq ($(TAGS),)
override TAGS := -t $(TAGS)
endif

ifneq ($(PATTERN),)
override PATTERN := -p $(PATTERN)
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

.PHONY: init
init: cask
	$(CASK) install
	$(CASK) update

.PHONY: dev
dev: cask
	$(CASK) --dev install
	$(CASK) --dev update

.PHONY: test
test: zmq
	$(CASK) exec ert-runner --script $(TAGS) $(PATTERN)

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
