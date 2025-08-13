MLKIT_ROOT ?= ~/gits/mlkit
#BENCHFILES=vliw.sml lexgen.sml badlife.sml barnes-hut.mlb DLX.sml fft.sml fib37.sml kbc.sml life.sml logic.mlb logic2.mlb mandelbrot.sml mlyacc.mlb mpuz.sml msort.sml msort-rf.sml nucleic.mlb professor.sml ratio.sml ray.mlb simple.sml tak.sml tsp.sml tsp_tp.sml zebra.sml zern.sml
BENCHFILES=vliw.sml lexgen.sml badlife.sml barnes-hut.mlb fft.sml fib37.sml kbc.sml life.sml logic.mlb logic2.mlb mandelbrot.sml mlyacc.mlb mpuz.sml msort.sml msort-rf.sml nucleic.mlb professor.sml ratio.sml ray.mlb simple.sml tak.sml tsp.sml tsp_tp.sml zebra.sml zern.sml
#BENCHFILES=vliw.sml lexgen.sml

BENCHFILES_SORTED=DLX.sml barnes-hut.mlb fft.sml fib37.sml kbc.sml lexgen.sml life.sml logic.mlb mandelbrot.sml mlyacc.mlb mpuz.sml msort-rf.sml msort.sml nucleic.mlb professor.sml ratio.sml ray.mlb simple.sml tak.sml tsp.sml vliw.sml zebra.sml zern.sml
#BENCHFILES_SORTED=life.sml

MLKIT_BENCH=../src/mlkit-bench

MLKIT_VERSION=$(shell $(MLKIT_ROOT)/bin/mlkit --version | sed -n 's/.*\([mv][0-9\.a-z-]*\).*/\1/p')
TODAY=$(shell date "+%Y-%m-%d")
MACHINE_TAG ?= MacBookPro2023


.PHONY: all
all: report.json
	cp $< report_$(MLKIT_VERSION)_$(MACHINE_TAG)_$(TODAY).json

report.json:
	MLKIT_ROOT=$(MLKIT_ROOT) $(MLKIT_BENCH) -r 5 -o $@ -mlkit -mlkit:-no_gc: -mlton $(BENCHFILES)
#	MLKIT_ROOT=$(MLKIT_ROOT) $(MLKIT_BENCH) -r 10 -o $@ -mlkit:-no_ri: -mlkit -mlkit:-gengc: -mlkit:-no_ri -gengc: -mlkit:-no_gc: -mlton $(BENCHFILES)
#	MLKIT_ROOT=$(MLKIT_ROOT) $(MLKIT_BENCH) -r 10 -o $@ -mlkit -mlkit:-disable_spurious_type_variables -scratch: -mlkit:-no_gc: -mlton $(BENCHFILES_SORTED)

report-flattening.json:
#	MLKIT_ROOT=$(MLKIT_ROOT) $(MLKIT_BENCH) -r 5 -o $@ -mlkit -mlkit:-no_gc: -mlkit:-no_uncurry -no_unbox_funargs -mlb-subdir x0: -mlkit:-no_uncurry -mlb-subdir x1: -mlkit:-no_unbox_real_funargs -mlb-subdir x2: -mlkit:-no_unbox_funargs -mlb-subdir x3: -mlton $(BENCHFILES)
	MLKIT_ROOT=$(MLKIT_ROOT) $(MLKIT_BENCH) -r 5 -o $@ -mlkit -mlkit:-no_gc: -mlkit:-no_uncurry -no_unbox_funargs -mlb-subdir x0: -mlkit:-no_uncurry -mlb-subdir x1: -mlkit:-no_unbox_real_funargs -mlb-subdir x2: -mlkit:-no_unbox_funargs -mlb-subdir x3: -mlton $(BENCHFILES)

.PHONY: clean
clean:
	rm -rf MLB *~ *.exe.out.* *.exe time.out report.json *.auto.mlb
	rm -rf raybench/MLB raybench/*~ nucleic/MLB nucleic/*~
	rm -rf mlyacc/MLB mlyacc/*~ logic/MLB logic/*~ logic2/MLB logic2/*~ barnes-hut/MLB barnes-hut/*~
	rm -rf *.spurious *.spurious.log *.spurious.no.log *.spurious.yes.log

# ------------------------------
# GC / Region collection ratios
# ------------------------------

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

MLKIT=SML_LIB=$(MLKIT_ROOT) $(MLKIT_ROOT)/bin/mlkit

# -----------------
# Region profiles
# -----------------

mlyacc_GG_prof.exe: mlyacc.mlb
	$(MLKIT) -no_ri -gengc -prof -o $@ $<

mlyacc_g_prof.exe: mlyacc.mlb
	$(MLKIT) -no_ri -prof -o $@ $<

mlyacc_r_prof.exe: mlyacc.mlb
	$(MLKIT) -no_gc -prof -o $@ $<

mlyacc_rg_prof.exe: mlyacc.mlb
	$(MLKIT) -prof -o $@ $<

mlyacc_rGG_prof.exe: mlyacc.mlb
	$(MLKIT) -gengc -prof -o $@ $<

mlyacc_r_prof.pdf: mlyacc_r_prof.exe
	./$< -microsec 10000 && $(MLKIT_ROOT)/bin/rp2ps -region -name "conf: r" && ps2pdf region.ps $@
	@echo Wrote file: $@

mlyacc_GG_prof.pdf: mlyacc_GG_prof.exe
	./$< -microsec 1000 && $(MLKIT_ROOT)/bin/rp2ps -region -name "conf: G" && ps2pdf region.ps $@
	@echo Wrote file: $@

mlyacc_rGG_prof.pdf: mlyacc_rGG_prof.exe
	./$< -microsec 1000 && $(MLKIT_ROOT)/bin/rp2ps -region -name "conf: rG" && ps2pdf region.ps $@
	@echo Wrote file: $@

mlyacc_%_prof.pdf: mlyacc_%_prof.exe
	./$< -microsec 1000 && $(MLKIT_ROOT)/bin/rp2ps -region -name "conf: $*" && ps2pdf region.ps $@
	@echo Wrote file: $@

.PHONY: mlyacc_pdfs
mlyacc_pdfs: mlyacc_g_prof.pdf mlyacc_GG_prof.pdf mlyacc_rg_prof.pdf mlyacc_rGG_prof.pdf mlyacc_r_prof.pdf

# ----------------------------------------
# Fragmentation functionality
# ----------------------------------------
%.gc.exe: %
	SML_LIB=$(MLKIT_ROOT) $(MLKIT) -o $*.gc.exe $<

%.gengc.exe: %
	SML_LIB=$(MLKIT_ROOT) $(MLKIT) -gengc -o $*.gengc.exe $<

%.norigc.exe: %
	SML_LIB=$(MLKIT_ROOT) $(MLKIT) -no_ri -o $*.norigc.exe $<

%.norigengc.exe: %
	SML_LIB=$(MLKIT_ROOT) $(MLKIT) -no_ri -gengc -o $*.norigengc.exe $<

%.frag: %.exe
	/bin/echo -n "$*:" > $@
	./$< -verbose_gc 2>&1 > /dev/null | grep -i Frag >> $@

FRAG_GC_FILES=$(BENCHFILES_SORTED:%=%.gc.frag)
FRAG_GENGC_FILES=$(BENCHFILES_SORTED:%=%.gengc.frag)
FRAG_NORIGC_FILES=$(BENCHFILES_SORTED:%=%.norigc.frag)
FRAG_NORIGENGC_FILES=$(BENCHFILES_SORTED:%=%.norigengc.frag)

frag.txt: $(FRAG_GC_FILES) $(FRAG_GENGC_FILES) $(FRAG_NORIGC_FILES) $(FRAG_NORIGENGC_FILES)
	cat $(FRAG_GC_FILES) $(FRAG_GENGC_FILES) $(FRAG_NORIGC_FILES) $(FRAG_NORIGENGC_FILES) > $@
	cat $@

EXTRACT_FRAG=sed -e 's/avg:nan/avg:0/g' | sed -e 's/\([0-9A-Za-z-]*\).*Frag avg:[ ]*\([0-9]*\).*/\2/g'
EXTRACT_NAMES=sed -e 's/avg:nan/avg:0/g' | sed -e 's/\([0-9A-Za-z-]*\).*Frag avg:[ ]*\([0-9]*\).*/\1/g'

frag_names.txt: $(FRAG_GC_FILES)
	cat $^ | $(EXTRACT_NAMES) > $@

frag_rg.txt: $(FRAG_GC_FILES)
	cat $^ | $(EXTRACT_FRAG) > $@

frag_rGG.txt: $(FRAG_GENGC_FILES)
	cat $^ | $(EXTRACT_FRAG) > $@

frag_g.txt: $(FRAG_NORIGC_FILES)
	cat $^ | $(EXTRACT_FRAG) > $@

frag_GG.txt: $(FRAG_NORIGENGC_FILES)
	cat $^ | $(EXTRACT_FRAG) > $@

frags.txt: frag_names.txt frag_rg.txt frag_rGG.txt frag_g.txt frag_GG.txt
	echo '%rg (%) \t rG (%) \t g (%) \t G (%)' > $@
	paste $^ >> $@

.PHONY: frags
frags: frag_rg.txt frag_rGG.txt frag_g.txt frag_GG.txt

# ------------------
# Spurious functions
# ------------------

%.spurious.log: %
	$(MLKIT) -stats_spurious -o $*.spurious.exe $< > $@

%.spurious.no.log: %
	(rm -rf MLB */MLB; $(MLKIT) -disable_spurious_type_variables -Pcee -o $*.spurious.dummy.exe $< > $@)

%.spurious.yes.log: %
	(rm -rf MLB */MLB; $(MLKIT) -Pcee -o $*.spurious.dummy.exe $< > $@)

%.spurious: %.spurious.log %.spurious.no.log %.spurious.yes.log
	@/bin/echo -n "$* & " | sed 's/.sml//' | sed 's/.mlb//' > $@
	../src/smlloc $* | tr -d '\n' >> $@
	@/bin/echo -n ' & ' >> $@
	@grep 'Spurious functions' $< | sed -n -E -e 's/.*: ([0-9]+) \/ ([0-9]+)/\1/p' | awk '{s+=$$1} END {print s}' | tr -d '\n' >> $@
	@/bin/echo -n ' / ' >> $@
	@grep 'Spurious functions' $< | sed -n -E -e 's/.*: ([0-9]+) \/ ([0-9]+)/\2/p' | awk '{s+=$$1} END {print s}' | tr -d '\n' >> $@
#	@/bin/echo -n ' (' >> $@
#	/bin/echo -n "scale=1; $$(grep 'Spurious functions:' $< | sed -n -E -e 's/.*: ([0-9]+) \/ ([0-9]+)/\1/p' | awk '{s+=$$1} END {print s}') * 100.0 / $$(grep 'Spurious functions:' $< | sed -n -E -e 's/.*: ([0-9]+) \/ ([0-9]+)/\2/p' | awk '{s+=$$1} END {print s}')" | bc -l | tr -d '\n' >> $@
#	@/bin/echo -n '%)' >> $@
	@/bin/echo -n ' & ' >> $@
	@grep 'Spurious instantiations' $< | sed -n -E -e 's/.*: ([0-9]+) \/ ([0-9]+)/\1/p' | awk '{s+=$$1} END {print s}' | tr -d '\n' >> $@
	@/bin/echo -n ' / ' >> $@
	@grep 'Spurious instantiations' $< | sed -n -E -e 's/.*: ([0-9]+) \/ ([0-9]+)/\2/p' | awk '{s+=$$1} END {print s}' | tr -d '\n' >> $@
#	@/bin/echo -n ' (' >> $@
#	/bin/echo -n "scale=1; $$(grep 'Spurious instantiations:' $< | sed -n -E -e 's/.*: ([0-9]+) \/ ([0-9]+)/\1/p' | awk '{s+=$$1} END {print s}') * 100.0 / $$(grep 'Spurious instantiations:' $< | sed -n -E -e 's/.*: ([0-9]+) \/ ([0-9]+)/\2/p' | awk '{s+=$$1} END {print s}')" | bc -l | tr -d '\n' >> $@
#	@/bin/echo -n '%)' >> $@
	@/bin/echo -n ' & ' >> $@
	@(diff -q $*.spurious.no.log $*.spurious.yes.log; /bin/echo -n $$? | sed 's/1/x/' | sed 's/0//' >> $@)
	@/bin/echo ' \\' >> $@

SPURIOUS_FILES=$(BENCHFILES_SORTED:%=%.spurious)

spurious.report: $(SPURIOUS_FILES)
	/bin/echo 'Program & LoC & Spur. funs. & Spur. insts. & Code diff \\ \hline' > $@
	cat $^ >> $@

.PHONY: spurious
spurious: spurious.report
	cat $<

.PHONY: version
version:
	@echo $(MLKIT_VERSION)
	@echo $(TODAY)
	@echo $(BENCHFILES_SORTED)

REPORT ?= report.json

.PHONY: press
press:
	@../src/press/mlkit-bench-press -sn -d real -d rss -skip1 \
           -merge_rows cname \
           -c pname \
           -c plen \
           -c 'real # rg' \
           -c 'real # r' \
           -c 'real # mlton' \
           -c 'rss # rg' \
           -c 'rss # r' \
           -c 'rss # mlton' \
           $(REPORT)

.PHONY: press-flattening
press-flattening:
	@../src/press/mlkit-bench-press -sn -d real -d rss -skip1 -merge_rows cname -c pname -c plen \
          -rcn 'MLKIT [-no_uncurry -no_unbox_funargs -mlb-subdir x0]@rg0' \
          -rcn 'MLKIT [-no_unbox_funargs -mlb-subdir x3]@rg0uc' \
          -rcn 'MLKIT [-no_unbox_real_funargs -mlb-subdir x2]@rg0uc_uba' \
          -c 'real # rg0' \
          -c 'real # rg0uc' \
          -c 'real # rg0uc_uba' \
          -c 'real # rg' \
          -c 'real # r' \
          -c 'real # mlton' \
          report-flattening.json
