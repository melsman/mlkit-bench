MLCOMP ?= mlkit

.PHONY: all
all: mlkit-bench-mem

mlkit-bench-mem: Mem.mlb Makefile *.sml
	$(MLCOMP) -output $@ $<

.PHONY: clean
clean:
	rm -rf MLB *~ mlkit-bench-mem
