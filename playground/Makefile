.PHONY: test
all: build run

clean:
	stack clean

build: clean
	stack build

run: build
	stack exec playground

dev: 
	stack build --file-watch


test: clean
	stack test

test-watch:
	stack test --file-watch