MLCOMP ?= mlkit

.PHONY: all
all:
	$(MLCOMP) matrix.mlb

.PHONY: test
test:
	$(MAKE) -C test test

.PHONY: clean
clean:
	$(MAKE) -C test clean
	rm -rf MLB *~ run
