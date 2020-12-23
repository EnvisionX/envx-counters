.PHONY: all test cover clean

ifndef ($(HOME))
    export HOME = /tmp
endif

DIR = src/envx_counters

all:

test:
	cd $(DIR) && go test -v -race -coverprofile=../.cover.out
	$(MAKE) -C tests -j1

cover: test
	cd $(DIR) && go tool cover -html=../.cover.out

clean:
	rm -f src/.cover.out
	$(MAKE) -C tests $@
