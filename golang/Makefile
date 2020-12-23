.PHONY: all test cover clean

DIR = src/envx_counters

all:

test:
	cd $(DIR) && go test -v -race -coverprofile=../.cover.out

cover: test
	cd $(DIR) && go tool cover -html=../.cover.out

clean:
	rm -f src/.cover.out
