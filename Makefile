.PHONY: all test fmt clean

all:

test:
	$(MAKE) -C golang $@
	$(MAKE) -C tests

fmt:
	$(MAKE) -C golang $@

clean:
	$(MAKE) -C golang $@
	$(MAKE) -C tests $@
