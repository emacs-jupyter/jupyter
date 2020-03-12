SHELL = bash
NPM ?= $(shell command -v npm)

.PHONY: all build clean

all: build

clean:
	@rm -rf node_modules 2>/dev/null || true

build:
	$(NPM) install
