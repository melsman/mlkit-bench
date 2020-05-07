SMLPKG=smlpkg

.PHONY: all
all: lib
	$(MAKE) -C src all

lib:
	$(SMLPKG) sync

.PHONY: test
test:
	$(MAKE) -C src/test test

.PHONY: clean
clean:
	$(MAKE) -C src clean
	$(MAKE) -C src/test clean
	rm -rf *~

.PHONY: realclean
realclean: clean
	rm -rf lib
