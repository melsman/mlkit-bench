MLCOMP ?= mlkit

.PHONY: all
all: mlkit-bench smlloc
	$(MAKE) -C press all

mlkit-bench: Bench.mlb Makefile *.sml
	$(MLCOMP) -output $@ $<

smlloc: smlloc.mlb smlloc.sml FileUtil.sml Makefile
	$(MLCOMP) -output $@ $<

.PHONY: clean
clean:
	rm -rf MLB *~ mlkit-bench smlloc
	$(MAKE) -C press clean
	$(MAKE) -C charting clean
	$(MAKE) -C speedup clean
