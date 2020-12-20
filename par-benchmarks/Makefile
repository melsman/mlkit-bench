MLKIT_ROOT ?= ~/gits/mlkit
BENCHFILES=fib/fib.mlb pmsort/pmsort.mlb ray/ray.mlb nqueens/nqueens.mlb
MLKIT_BENCH=../src/mlkit-bench

MLKIT_VERSION=$(shell $(MLKIT_ROOT)/bin/mlkit --version | sed -n 's/.*\([mv][0-9\.a-z-]*\).*/\1/p')
TODAY=$(shell date "+%Y-%m-%d")
MACHINE_TAG ?= MacBookPro2016

.PHONY: all
all: report-par.json
	cp $< report-par_$(MLKIT_VERSION)_$(MACHINE_TAG)_$(TODAY).json

report-par.json:
	MLKIT_ROOT=$(MLKIT_ROOT) $(MLKIT_BENCH) -n 10 -o $@ \
          -mlkit:MLCOMP=mlkit-seq -no_gc: \
          -mlkit:MLCOMP=mlkit-seq -no_gc -par -mlb-subdir C1: \
          -mlkit:MLCOMP=mlkit-par -no_gc -par: $(BENCHFILES)

.PHONY: clean
clean:
	rm -rf MLB *~ *.exe.out.* *.exe time.out report-par.json *.auto.mlb
	rm -rf */MLB */*~ */run */*.ppm */*.exe*

.PHONY: version
version:
	@echo $(MLKIT_VERSION)
	@echo $(TODAY)

.PHONY: press
press: report-par.json
	@../src/press/mlkit-bench-press -d real -skip1 \
           -merge_rows cname -c pname \
           -c 'real # MLKIT [MLCOMP=mlkit-seq -no_gc]' \
           -c 'real # MLKIT [MLCOMP=mlkit-seq -no_gc -par -mlb-subdir C1]' \
           -c 'real # MLKIT [MLCOMP=mlkit-par -no_gc -par]' $<