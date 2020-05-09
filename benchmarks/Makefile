MLKIT_ROOT ?= ~/gits/mlkit
BENCHFILES=badlife.sml barnes-hut.mlb DLX.sml fib37.sml kbc.sml life.sml logic.mlb mlyacc.mlb msort.sml msort-rf.sml professor.sml ratio.sml ray.mlb simple.sml tak.sml tsp.sml tsp_tp.sml zebra.sml zern.sml
MLKIT_BENCH=../src/mlkit-bench

MLKIT_VERSION=$(shell $(MLKIT_ROOT)/bin/mlkit --version | sed -n 's/.*\(v[0-9\.a-z-]*\).*/\1/p')
TODAY=$(shell date "+%Y-%m-%d")

.PHONY: all
all: report.json
	cp $< report-$(TODAY)-$(MLKIT_VERSION).json

report.json:
	MLKIT_ROOT=$(MLKIT_ROOT) $(MLKIT_BENCH) -o $@ -mlkit -mlkit:-no_gc: -mlton $(BENCHFILES)

.PHONY: clean
clean:
	rm -rf MLB *~ *.exe.out.* *.exe time.out report.json
	rm -rf raybench/MLB raybench/*~

.PHONY: version
version:
	@echo $(MLKIT_VERSION)
	@echo $(TODAY)
