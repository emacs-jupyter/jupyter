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

.PHONY: dev
dev: cask
	$(CASK) install
	$(CASK) update

.PHONY: test
test:
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
compile:
	$(CASK) build
