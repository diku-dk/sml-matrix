
.PHONY: all
all:
	$(MAKE) -C lib/github.com/diku-dk/sml-matrix all

.PHONY: test
test:
	$(MAKE) -C lib/github.com/diku-dk/sml-matrix test

.PHONY: clean
clean:
	$(MAKE) -C lib/github.com/diku-dk/sml-matrix clean
	rm -rf MLB *~ .*~
