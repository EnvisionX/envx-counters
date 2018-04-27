.PHONY: all test fmt clean

all:

test:
	GOPATH=/usr/share/gocode:`pwd` go test -v envx_counters

fmt:
	find . -type f -name \*.go -exec gofmt -w '{}' ';'

clean:
