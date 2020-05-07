
MLKIT=SML_LIB=~/gits/mlkit ~/gits/mlkit/bin/mlkit

.PHONY: all
#all: output-mlkit.png output-mlton.png
all: output-mlkit.png

ray-mlkit.exe:
#	$(MLKIT) --statistics_after_optimisation --comments_in_x64_asmcode --no_delete_target_files --maximum_inline_size 1000 -output $@ ray-mlkit.mlb
	$(MLKIT) --no_delete_target_files --maximum_inline_size 1000 -output $@ ray-mlkit.mlb

ray-mlton.exe:
	mlton -keep g -output $@ ray.mlb

output-%.png: output-%.ppm
	convert $< $@

output-%.ppm: ray-%.exe
	./$< -n 400 -m 400 -f $@

.PHONY: clean
clean:
	rm -rf *~ MLB ray-*.exe run output-*.ppm output-*.png *.s *.c
