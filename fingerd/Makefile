.PHONY: test
all: build run

clean:
	stack clean

build: clean
	stack build

run: build
	stack exec fingerd

debug: build
	stack exec fingerd-debug