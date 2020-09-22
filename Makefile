.PHONY: all test fmt clean

export HOME = $(shell pwd)

all:

test:
	$(MAKE) -C golang $@
	$(MAKE) -C tests -j1

fmt:
	$(MAKE) -C golang $@

clean:
	$(MAKE) -C golang $@
	$(MAKE) -C tests $@
