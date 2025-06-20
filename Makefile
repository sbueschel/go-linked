SHELL := /bin/bash

TEST_TIMEOUT ?= 30s
MODULE = $(shell grep -E -e '^module ' go.mod | cut -d ' ' -f 2)
COVERAGE = $(shell grep -E -o -e 'coverage: [0-9]{1,3}[^%]+' build/cover.stdout | cut -d ' ' -f 2)

GO ?= go

GO_TEST = $(GO) test -timeout=$(TEST_TIMEOUT) -count=1

# TODO(sbueschel): CI/CD badge

cover: build
	$(GO_TEST) -coverprofile=build/cover.out $(MODULE) | tee build/cover.stdout

badge: cover
	curl -sS 'https://img.shields.io/badge/Coverage-$(COVERAGE)%25-green' -o cover.svg

test:
	$(GO_TEST) -v $(MODULE)

build:
	mkdir build/

clean:
	rm -rvf build/
