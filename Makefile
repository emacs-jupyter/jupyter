EMACS ?= emacs
SHELL = bash
CASK ?= $(shell command -v cask)
ifeq ($(CASK),)
    $(error "Install cask (https://github.com/cask/cask)")
endif

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

# Build the zmq module.
.PHONY: zmq
zmq:
	$(CASK) eval "(cl-letf (((symbol-function 'read-string) (lambda (&rest _) \"y\"))) (require 'zmq))"

.PHONY: init
init:
	$(CASK) install
	$(CASK) update

.PHONY: dev
dev:
	$(CASK) --dev install
	$(CASK) --dev update

.PHONY: test
test: zmq compile
	$(CASK) exec ert-runner --no-win $(TAGS) $(PATTERN)

.PHONY: clean
clean:
	make -C js clean
	rm $(ELCFILES)

.PHONY: clean-cask
clean-cask:
	rm -rf .cask/

.PHONY: widgets
widgets:
	make -C js

.PHONY: compile
compile:
	$(CASK) build
