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

.PHONY: all clean install test check docs pdf vale lint-docs check-docs

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
.PHONY: docs pdf presentation vale vale-sync lint-docs check-docs
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

# Vale linting targets
vale-sync:
	@echo "Syncing Vale packages..."
	vale sync

vale: vale-sync
	@echo "Linting documentation with Vale..."
	vale README.org TUTORIAL.org API.org CONFERENCES.org roulette.org

lint-docs: vale

check-docs: vale
	@echo "✓ Documentation checks complete"

# Fetch reference papers
.PHONY: fetch-papers docs-papers
fetch-papers: docs/papers/rosette-pldi2014.pdf

docs/papers/rosette-pldi2014.pdf:
	@echo "Fetching Rosette paper..."
	@mkdir -p docs/papers
	curl -L -o $@ https://homes.cs.washington.edu/~bodik/ucb/Files/2014/rosette-pldi2014.pdf

docs-papers: fetch-papers

clean-docs:
	rm -f PRESENTATION.pdf PRESENTATION.tex
	rm -f *.aux *.log *.nav *.out *.snm *.toc *.vrb

# Help target
.PHONY: help
help:
	@echo "Guile Roulette Makefile"
	@echo ""
	@echo "Build targets:"
	@echo "  all          - Build all Scheme modules (default)"
	@echo "  clean        - Remove compiled files"
	@echo "  install      - Install to Guile site directory"
	@echo ""
	@echo "Testing:"
	@echo "  test         - Run test suite"
	@echo "  check        - Alias for test"
	@echo "  repl         - Start Guile REPL with library loaded"
	@echo ""
	@echo "Documentation:"
	@echo "  vale-sync    - Sync Vale linting packages"
	@echo "  vale         - Lint documentation with Vale"
	@echo "  lint-docs    - Alias for vale"
	@echo "  check-docs   - Run all documentation checks"
	@echo "  pdf          - Build PRESENTATION.pdf"
	@echo "  docs-papers  - Fetch reference papers"
	@echo ""
	@echo "Cleaning:"
	@echo "  clean-docs   - Remove generated documentation files"
