MLCOMP ?= mlkit

.PHONY: all
all: mlkit-bench-abstime

mlkit-bench-abstime: Abstime.mlb Makefile *.sml
	$(MLCOMP) -output $@ $<

.PHONY: clean
clean:
	rm -rf MLB *~ mlkit-bench-abstime
