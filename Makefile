# Makefile for guile-roulette
# Use GNU Make (gmake on FreeBSD)

GUILE := guile
GUILD := guild
PREFIX ?= /usr/local
SITE_DIR := $(shell $(GUILE) -c "(display (%site-dir))")

SOURCES := roulette/core.scm \
           roulette/inference.scm \
           roulette.scm

COMPILED := $(SOURCES:.scm=.go)

.PHONY: all clean install test check

all: $(COMPILED)

# Compile submodules first, then main module
roulette/core.go: roulette/core.scm
	$(GUILD) compile -L . -o $@ $<

roulette/inference.go: roulette/inference.scm roulette/core.go
	$(GUILD) compile -L . -o $@ $<

roulette.go: roulette.scm roulette/core.go roulette/inference.go
	$(GUILD) compile -L . -o $@ $<

clean:
	rm -f $(COMPILED)
	rm -f tests/*.log
	find . -name "*.go" -delete

install: all
	install -d $(DESTDIR)$(SITE_DIR)/roulette
	install -m 644 roulette.scm $(DESTDIR)$(SITE_DIR)/
	install -m 644 roulette/*.scm $(DESTDIR)$(SITE_DIR)/roulette/

test: all
	$(GUILE) -L . tests/test-core.scm
	$(GUILE) -L . tests/test-inference.scm

check: test

.PHONY: repl
repl: all
	$(GUILE) -L .
