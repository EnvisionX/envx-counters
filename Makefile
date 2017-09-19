.PHONY: all test fmt clean

all:

test:
	$(MAKE) -C golang $@

fmt:
	$(MAKE) -C golang $@

clean:
	$(MAKE) -C golang $@
