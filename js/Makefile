SHELL = bash

NPM ?= $(shell command -v npm)
ifeq ($(NPM),)
	$(error "Node not installed (https://nodejs.org/en/)")
endif

.PHONY: all build clean

all: build

clean:
	make -C widget clean
  make -C web-renderer clean

build:
  make -C widget build
  make -C web-renderer build
