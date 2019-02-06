SHELL = bash

NPM ?= $(shell command -v npm)
ifeq ($(NPM),)
	$(error "Node not installed (https://nodejs.org/en/)")
endif

YARN ?= $(shell command -v yarn)
ifeq ($(YARN),)
# If yarn isn't already installed, it is built locally
	YARN = ./node_modules/.bin/yarn
endif

.PHONY: all build clean

all: build

clean:
	@rm -rf built/ 2>/dev/null || true

really-clean: clean
	@rm -rf node_modules 2>/dev/null || true

build: built/index.built.js

built/index.built.js:
	$(NPM) install
	$(YARN) run build --progress
