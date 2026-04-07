SHELL = bash

NPM ?= $(shell command -v npm)
YARN ?= $(shell command -v yarn)
# If yarn isn't already installed, it is built locally
ifeq ($(YARN),)
	YARN = ./node_modules/.bin/yarn
endif

.PHONY: all build clean really-clean

all: build

clean:
	rm -rf built/ 2>/dev/null || true

really-clean: clean
	rm -rf node_modules/ 2>/dev/null || true

ifeq ($(NPM),)
	build:
		@echo 'Node not installed (https://nodejs.org/en/)' && false
else
	build: built/index.built.js
		$(NPM) install
		$(YARN) run build --progress
endif
