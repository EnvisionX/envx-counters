.PHONY: all doc test fmt clean

all:
	$(MAKE) -C erlang compile

doc:
	$(MAKE) -C erlang html

test:
	$(MAKE) -C erlang eunit
	$(MAKE) -C golang $@

fmt:
	$(MAKE) -C golang $@

clean:
	$(MAKE) -C erlang $@
	$(MAKE) -C golang $@
	rm -f debian/envx-erlang-counters.install
