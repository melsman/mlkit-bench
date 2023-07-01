MLKIT_ROOT ?= ~/gits/mlkit
BENCHFILES=seam-carve/seam-carve.mlb ray/ray.mlb ray-orig/ray-orig.mlb nqueens/nqueens.mlb soac/sgm_scan.mlb soac/filter.mlb soac/scan.mlb fib/fib.mlb primes/primes.mlb mandelbrot/mandelbrot.mlb sobol/soboloption.mlb sobol/sobolpi.mlb vpmsort/vpmsort.mlb pmsort/pmsort.mlb vpmsort/vpmsort2.mlb
MLKIT_BENCH=../src/mlkit-bench

MLKIT_VERSION=$(shell $(MLKIT_ROOT)/bin/mlkit --version | sed -n 's/.*\([mv][0-9\.a-z-]*\).*/\1/p')
TODAY=$(shell date "+%Y-%m-%d")
MACHINE_TAG ?= MacBookPro2016
MPL_PROCS=8

ARGOBOTS_ROOT?=`pwd`/../../argobots

.PHONY: all
all: report-par.json
	cp $< report-par_$(MLKIT_VERSION)_$(MACHINE_TAG)_$(TODAY).json

#          -mpl:-mlb-path-var \'MLCOMP mpl\' -runtime procs -runtime $(MPL_PROCS): \

report-par.json:
	MLKIT_ROOT=$(MLKIT_ROOT) $(MLKIT_BENCH) -it -r 4 -o $@ \
          -mlton:-mlb-path-var \'MLCOMP mlton\': \
          -mlkit:MLCOMP=mlkit-seq -no_gc: \
          -mlkit:MLCOMP=mlkit-seq -no_gc -par -mlb-subdir C1: \
          -mlkit:MLCOMP=mlkit-par -no_gc -par: $(BENCHFILES)

report-arpar.json:
	MLKIT_ROOT=$(MLKIT_ROOT) $(MLKIT_BENCH) -it -r 4 -o $@ \
          -mlkit:MLCOMP=mlkit-par -no_gc -par -argo -libs 'm,c,dl,abt' -libdirs $(ARGOBOTS_ROOT)/src/.libs: \
          -mlkit:MLCOMP=mlkit-seq -no_gc: \
          -mlkit:MLCOMP=mlkit-par -no_gc -par: $(BENCHFILES)

.PHONY: clean
clean:
	rm -rf MLB *~ *.exe.out.* *.exe time.out report-par.json report-arpar.json *.auto.mlb
	rm -rf */MLB */*~ */run ray/*.ppm ray-orig/*.ppm */*.exe*

.PHONY: version
version:
	@echo $(MLKIT_VERSION)
	@echo $(TODAY)

.PHONY: press
press: report-par.json
	@../src/press/mlkit-bench-press -sn -d real -d rss -skip1 \
           -merge_rows cname -c pname \
           -c 'real # mlton' \
           -c 'real # mpl' \
           -c 'real # seq' \
           -c 'real # par1' \
           -c 'real # par' \
           -c 'rss # par' \
           -c plen $<

.PHONY: arpress
arpress: report-arpar.json
	@../src/press/mlkit-bench-press -sn -d real -d rss -skip1 -merge_rows cname $<
