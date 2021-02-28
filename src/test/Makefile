MLKIT_ROOT ?= ~/gits/mlkit
TESTFILES=./fib37.sml ./life.sml
TESTFILES_IT=./msort.mlb

.PHONY: all
all: test

.PHONY: test
test: test.json test-it.json

test.json:
	MLKIT_ROOT=$(MLKIT_ROOT) ../mlkit-bench -r 4 -o $@ -mlkit -mlkit:-no_gc: -mlkit:-gengc: -mlton $(TESTFILES)

test-it.json:
	MLKIT_ROOT=$(MLKIT_ROOT) ../mlkit-bench -it -r 4 -o $@ -mlkit -mlkit:-no_gc: -mlkit:-gengc: -mlton $(TESTFILES_IT)

.PHONY: clean
clean:
	rm -rf MLB *~ *.exe.out.* *.exe time.out test.json test-it.json
