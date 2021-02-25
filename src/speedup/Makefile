MLCOMP ?= mlkit

.PHONY: all
all: mlkit-bench-speedup

mlkit-bench-speedup: Speedup.mlb Makefile *.sml
	$(MLCOMP) -output $@ $<

.PHONY: clean
clean:
	rm -rf MLB *~ mlkit-bench-speedup
