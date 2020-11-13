MLKIT_ROOT ?= ~/gits/mlkit
BENCHFILES=vliw.sml lexgen.sml badlife.sml barnes-hut.mlb DLX.sml fft.sml fib37.sml kbc.sml life.sml logic.mlb mandelbrot.sml mlyacc.mlb mpuz.sml msort.sml msort-rf.sml nucleic.mlb professor.sml ratio.sml ray.mlb simple.sml tak.sml tsp.sml tsp_tp.sml zebra.sml zern.sml
#BENCHFILES=vliw.sml lexgen.sml
MLKIT_BENCH=../src/mlkit-bench

MLKIT_VERSION=$(shell $(MLKIT_ROOT)/bin/mlkit --version | sed -n 's/.*\([mv][0-9\.a-z-]*\).*/\1/p')
TODAY=$(shell date "+%Y-%m-%d")
MACHINE_TAG ?= MacBookPro2016


.PHONY: all
all: report.json
	cp $< report_$(MLKIT_VERSION)_$(MACHINE_TAG)_$(TODAY).json

report.json:
	MLKIT_ROOT=$(MLKIT_ROOT) $(MLKIT_BENCH) -n 30 -o $@ -mlkit:-no_ri: -mlkit -mlkit:-gengc: -mlkit:-no_ri -gengc: -mlkit:-no_gc: -mlton $(BENCHFILES)

.PHONY: clean
clean:
	rm -rf MLB *~ *.exe.out.* *.exe time.out report.json *.auto.mlb
	rm -rf raybench/MLB raybench/*~ nucleic/MLB nucleic/*~
	rm -rf mlyacc/MLB mlyacc/*~ logic/MLB logic/*~ barnes-hut/MLB barnes-hut/*~

KEYS=DLX barnes-hut fft fib37 kbc lexgen life logic mandelbrot mlyacc mpuz msort-rf msort nucleic professor ratio ray simple tak tsp vliw zebra zern
PCTFILES_rg=$(KEYS:%=%_rg.pct)
PCTFILES_rGG=$(KEYS:%=%_rGG.pct)

%_rg.pct: %_mlkit.exe
	/bin/echo -n "$*: " > $@
	./$< -verbose_gc 2>&1 | tail -n 1 | sed -e 's/.*RI:[ ]*\([0-9]*\)%, GC:[ ]*\([0-9]*\)%.*/\2/g' >> $@

%_rGG.pct: %_mlkit-gengc.exe
	/bin/echo -n "$*: " > $@
	./$< -verbose_gc 2>&1 | tail -n 1 | sed -e 's/.*RI:[ ]*\([0-9]*\)%, GC:[ ]*\([0-9]*\)%.*/\2/g' >> $@

.PHONY: pct
pct: $(PCTFILES_rg) $(PCTFILES_rGG)
	cat $(PCTFILES_rg)
	cat $(PCTFILES_rGG)

.PHONY: version
version:
	@echo $(MLKIT_VERSION)
	@echo $(TODAY)
