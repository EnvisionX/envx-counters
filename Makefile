.PHONY: all doc test fmt clean

all:
	$(MAKE) -C erlang compile

doc:
	$(MAKE) -C erlang html

test:
	$(MAKE) -C erlang eunit

fmt:

clean:
	$(MAKE) -C erlang $@
	rm -f debian/envx-erlang-counters.install
