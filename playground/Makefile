.PHONY: test
all: build run

clean:
	stack clean

build: clean
	stack build

run: build
	stack run

run-crypto: build
	stack run playground-crypto -- $$(find ./src -type f)

dev: 
	stack build --file-watch


test: clean
	stack test

test-watch:
	stack test --file-watch