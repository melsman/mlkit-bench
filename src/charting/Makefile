##
##   $ make deploy
##   $ cd dist
##   $ python -m SimpleHTTPServer 8000
##

SMLPKG?=smlpkg
SML_LIB= ~/gits/mlkit/js
JS_PATH_FLAGS=-js_path_compress -js_path_relative_to $(SML_LIB)
SMLTOJS=SML_LIB=$(SML_LIB) ~/gits/mlkit/bin/smltojs

.PHONY: all
all: index.html

page.html: main.mlb lib
	$(SMLTOJS) $(JS_PATH_FLAGS) -o page $<

index.html: static/page_start.html page.html static/page_end.html
	cat static/page_start.html > $@
	cat page.html >> $@
	cat static/page_end.html >> $@

.PHONY: highchart
highchart: highchart.mlb lib
	SML_LIB=$(SML_LIB) $(SMLTOJS) -c $<

DEPLOYDIR=dist

.PHONY: realclean
realclean: clean
	rm -rf lib

.PHONY: clean
clean: cleandeploy
	rm -rf MLB *~ index.html page.html prims.js basis static/*~
	rm -rf isodate/*~ isodate/MLB
	rm -rf listutil/*~ listutil/MLB

.PHONY: cleandeploy
cleandeploy:
	rm -rf $(DEPLOYDIR)/MLB $(DEPLOYDIR)/basis $(DEPLOYDIR)/favicon.ico $(DEPLOYDIR)/index.html
	rm -rf $(DEPLOYDIR)/isodate $(DEPLOYDIR)/lib $(DEPLOYDIR)/listutil $(DEPLOYDIR)/prims.js $(DEPLOYDIR)/custom.css

.PHONY: deploy
deploy: cleandeploy index.html
	cp -pa $(SML_LIB)/prims.js $(DEPLOYDIR)
	mkdir -p $(DEPLOYDIR)/basis/MLB/Js
	cp -pa $(SML_LIB)/basis/MLB/Js/*-sml*.js $(DEPLOYDIR)/basis/MLB/Js
	cp -pa static/favicon.ico index.html $(DEPLOYDIR)
	cp -pa static/custom.css index.html $(DEPLOYDIR)
	mkdir -p $(DEPLOYDIR)/MLB/Js
	cp -pa MLB/Js/*.js $(DEPLOYDIR)/MLB/Js
	mkdir -p $(DEPLOYDIR)/isodate/MLB/Js
	cp -pa isodate/MLB/Js/*.js $(DEPLOYDIR)/isodate/MLB/Js
	mkdir -p $(DEPLOYDIR)/listutil/MLB/Js
	cp -pa listutil/MLB/Js/*.js $(DEPLOYDIR)/listutil/MLB/Js
#
	mkdir -p $(DEPLOYDIR)/lib/github.com/diku-dk/sml-sort/MLB/Js
	cp -pa lib/github.com/diku-dk/sml-sort/MLB/Js/*.js $(DEPLOYDIR)/lib/github.com/diku-dk/sml-sort/MLB/Js
#
	mkdir -p $(DEPLOYDIR)/lib/github.com/diku-dk/sml-json/MLB/Js
	cp -pa lib/github.com/diku-dk/sml-json/MLB/Js/*.js $(DEPLOYDIR)/lib/github.com/diku-dk/sml-json/MLB/Js
#
	mkdir -p $(DEPLOYDIR)/lib/github.com/diku-dk/sml-setmap/map/MLB/Js
	cp -pa lib/github.com/diku-dk/sml-setmap/map/MLB/Js/*.js $(DEPLOYDIR)/lib/github.com/diku-dk/sml-setmap/map/MLB/Js

lib:
	$(SMLPKG) sync
