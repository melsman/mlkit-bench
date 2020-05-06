MLCOMP ?= mlkit

.PHONY: all
all: mlkit-bench

mlkit-bench: Bench.mlb Makefile *.sml
	$(MLCOMP) -output $@ $<

.PHONY: clean
clean:
	rm -rf MLB *~ mlkit-bench
