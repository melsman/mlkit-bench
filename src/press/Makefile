MLCOMP ?= mlkit

.PHONY: all
all: mlkit-bench-press

mlkit-bench-press: Press.mlb Makefile *.sml
	$(MLCOMP) -output $@ $<

.PHONY: clean
clean:
	rm -rf MLB *~ mlkit-bench-press

../charting/lib:
	$(MAKE) -C ../charting lib
