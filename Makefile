DOCS_DIR ?= docs
BROWSER ?= firefox

all:
all: build test lint ## Build everything

.PHONY: build
build: ## Build the library
	cabal new-build

.PHONY: lint
lint: ## Lint source files
	hlint lint src
	hlint lint test
	find src test -name '*.hs' | xargs ormolu --mode=check

.PHONY: test
test: ## Run tests
	cabal new-run test

.PHONY: format
format: ## Format source files
	find src test -name '*.hs' | xargs ormolu --mode=inplace

.PHONY: docs
docs: ## Generate haddocks in DOCS_DIR (default: docs)
	rm -rf $(DOCS_DIR)
	cp -r $(shell dirname `cabal new-haddock 2>/dev/null | tail -n1`) $(DOCS_DIR)

.PHONY: open-docs
open-docs: docs ## Generate haddocks and open them in BROWSER (default: firefox)
	$(BROWSER) docs/index.html

.PHONY: ghcid-lib
ghcid-lib: ## Run ghcid for the library target
	-ghcid --command="cabal new-repl"

.PHONY: ghcid-test
ghcid-test: ## Run ghcid for the test target
	-ghcid --command="cabal new-repl test" --setup=":load Main" --test=":main --hide-successes"

# https://marmelab.com/blog/2016/02/29/auto-documented-makefile.html
.PHONY: help
help: ## Show this help message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
