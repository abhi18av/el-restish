EMACS ?= emacs
CASK ?= cask

# Source files
SOURCES = el-restish.el \
          el-restish-config.el \
          el-restish-core.el \
          el-restish-buffer.el \
          el-restish-format.el \
          el-restish-client.el

# Test files
TEST_SOURCES = test/el-restish-tests.el

# All elisp files
ALL_SOURCES = $(SOURCES) $(TEST_SOURCES)

# Compiled files
OBJECTS = $(SOURCES:.el=.elc)

.PHONY: all test compile clean lint checkdoc install deps

all: compile

# Install dependencies
deps:
	$(CASK) install

# Compile elisp files
compile: $(OBJECTS)

%.elc: %.el
	$(CASK) exec $(EMACS) -batch -L . -f batch-byte-compile $<

# Run tests
test: compile
	$(CASK) exec $(EMACS) -batch -L . -L test \
		-l ert \
		-l test/el-restish-tests.el \
		-f ert-run-tests-batch-and-exit

# Run tests with coverage (if available)
test-coverage: compile
	$(CASK) exec $(EMACS) -batch -L . -L test \
		-l undercover \
		-l test/el-restish-tests.el \
		-f ert-run-tests-batch-and-exit

# Lint elisp files
lint: $(ALL_SOURCES)
	$(CASK) exec $(EMACS) -batch \
		--eval "(require 'package-lint)" \
		--eval "(setq package-lint-main-file \"el-restish.el\")" \
		-f package-lint-batch-and-exit $(SOURCES)

# Check documentation
checkdoc: $(ALL_SOURCES)
	$(CASK) exec $(EMACS) -batch \
		--eval "(checkdoc-file \"el-restish.el\")" \
		--eval "(checkdoc-file \"el-restish-config.el\")" \
		--eval "(checkdoc-file \"el-restish-core.el\")" \
		--eval "(checkdoc-file \"el-restish-buffer.el\")" \
		--eval "(checkdoc-file \"el-restish-format.el\")" \
		--eval "(checkdoc-file \"el-restish-client.el\")"

# Install package locally
install: compile
	$(CASK) exec $(EMACS) -batch \
		--eval "(package-initialize)" \
		--eval "(package-install-file \"el-restish.el\")"

# Clean compiled files
clean:
	rm -f $(OBJECTS)
	rm -f *.elc test/*.elc

# Development helpers
dev-deps:
	$(CASK) install --dev

format:
	$(CASK) exec $(EMACS) -batch \
		--eval "(require 'elisp-format)" \
		--eval "(elisp-format-file \"el-restish.el\")"

# Quality checks
quality: lint checkdoc compile test

# Release preparations
release: clean quality
	@echo "Ready for release!"

# Watch for changes and run tests (requires inotify-tools)
watch:
	while inotifywait -e modify $(ALL_SOURCES); do make test; done

# Interactive development
repl:
	$(CASK) exec $(EMACS) -L . --eval "(require 'el-restish)"

# Help
help:
	@echo "Available targets:"
	@echo "  all       - Compile all source files (default)"
	@echo "  deps      - Install dependencies"
	@echo "  compile   - Compile elisp files"
	@echo "  test      - Run unit tests"
	@echo "  lint      - Run package-lint"
	@echo "  checkdoc  - Check documentation"
	@echo "  install   - Install package locally"
	@echo "  clean     - Remove compiled files"
	@echo "  quality   - Run all quality checks"
	@echo "  release   - Prepare for release"
	@echo "  help      - Show this help"