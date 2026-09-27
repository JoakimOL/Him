.PHONY: build run test watch ghci fmt lint clean

HS_DIRS := src app test

build:
	stack build

# Usage: make run ARGS=path/to/file
run:
	stack run him -- $(ARGS)

test:
	stack test

# Rebuild on every save (fast, unoptimised).
watch:
	stack build --fast --file-watch

ghci:
	stack ghci

fmt:
	@command -v fourmolu >/dev/null || { echo "fourmolu not found: stack install fourmolu"; exit 1; }
	fourmolu -i $$(find $(HS_DIRS) -name '*.hs')

lint:
	@command -v hlint >/dev/null || { echo "hlint not found: stack install hlint"; exit 1; }
	hlint $(HS_DIRS)

clean:
	stack clean
