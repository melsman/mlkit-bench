MLKIT_ROOT ?= ~/gits/mlkit
TESTFILES=fib37.sml life.sml

.PHONY: all
all: test

.PHONY: test
test: test.json

test.json:
	MLKIT_ROOT=$(MLKIT_ROOT) ../mlkit-bench -o $@ -mlkit -mlkit:-no_gc: -mlkit:-gengc: -mlton $(TESTFILES)

.PHONY: clean
clean:
	rm -rf MLB *~ *.exe.out.* *.exe time.out test.json
