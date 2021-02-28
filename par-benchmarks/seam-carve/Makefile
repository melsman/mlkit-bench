MLTON?=mlton
FLAGS?=
MLKIT?=SML_LIB=~/gits/mlkit ~/gits/mlkit/bin/mlkit

%-par.exe: %.mlb *.sml ../util/*.sml
	MLCOMP=mlkit-par $(MLKIT) -par -no_gc $(FLAGS) -o $@ $<

%-seq.exe: %.mlb *.sml ../util/*.sml
	MLCOMP=mlkit-seq $(MLKIT) -par -no_gc -mlb-subdir C1 $(FLAGS) -o $@ $<

%.exe: %.mlb *.sml ../util/*.sml
	MLCOMP=mlkit-seq $(MLKIT) -no_gc $(FLAGS) -o $@ $<

%-mlton.exe: %.mlb *.sml ../util/*.sml
	$(MLTON) -mlb-path-var 'MLCOMP mlton' -output $@ $<

.PHONY: clean
clean:
	rm -rf MLB ../util/MLB *.exe *.exe.out.* *~ run *.png *.gif
