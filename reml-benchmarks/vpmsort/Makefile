MLTON?=mlton
FLAGS?=-Pcee
MLKIT?=SML_LIB=~/gits/mlkit ~/gits/mlkit/bin/mlkit $(FLAGS)

.PHONY: all
all: vpmsort2-par.exe vpmsort-par.exe slmsort-par.exe

%-reml.exe: %.mlb %.sml
	MLCOMP=mlkit-reml $(MLKIT) -er -par -no_gc -mlb-subdir REML -o $@ $<

%-par.exe: %.mlb %.sml ../util/soac.sml
	MLCOMP=mlkit-par $(MLKIT) -par -no_gc -o $@ $<

%-par0.exe: %.mlb %.sml ../util/soac.sml
	MLCOMP=mlkit-par $(MLKIT) -par -par0 -no_gc -o $@ $<

%-seq.exe: %.mlb %.sml ../util/soac.sml
	MLCOMP=mlkit-seq $(MLKIT) -par -no_gc -mlb-subdir C1 -o $@ $<

%.exe: %.mlb %.sml ../util/soac.sml
	MLCOMP=mlkit-seq $(MLKIT) -no_gc -o $@ $<

%-mlton.exe: %.mlb %.sml ../util/soac.sml
	$(MLTON) -mlb-path-var 'MLCOMP mlton' -output $@ $<

.PHONY: clean
clean:
	rm -rf MLB ../util/MLB *.exe *.exe.out.* *~
