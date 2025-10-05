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

.PHONY: all clean install test check docs pdf

all: $(COMPILED)

# Compile submodules first, then main module
roulette/core.go: roulette/core.scm
	$(GUILD) compile -L . -o $@ $<

roulette/inference.go: roulette/inference.scm roulette/core.go
	$(GUILD) compile -L . -o $@ $<

roulette.go: roulette.scm roulette/core.go roulette/inference.go
	$(GUILD) compile -L . -o $@ $<

clean: clean-docs
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

# Documentation targets
.PHONY: docs pdf presentation
docs: pdf

pdf: PRESENTATION.pdf

presentation: PRESENTATION.pdf

PRESENTATION.pdf: PRESENTATION.org
	@echo "Exporting presentation to PDF with Emacs..."
	emacs --batch \
		--eval "(require 'org)" \
		--eval "(require 'ox-beamer)" \
		--visit=PRESENTATION.org \
		--funcall org-beamer-export-to-pdf

clean-docs:
	rm -f PRESENTATION.pdf PRESENTATION.tex
	rm -f *.aux *.log *.nav *.out *.snm *.toc *.vrb
