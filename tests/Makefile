.PHONY: all server clean

CLI = ../bin/envx-counters-cli
export GOPATH = $(shell pwd)/../golang

all: server

server:
	$(MAKE) clean
	mkdir -p run
	nohup go run server.go > run/log 2>&1 &
	sleep 1s
	test "`$(CLI) list`" = ""
	test "`$(CLI) dump`" = ""
	test "`$(CLI) get myName`" = "myName 0"
	test "`$(CLI) hit myName`" = ""
	test "`$(CLI) list`" = "myName"
	test "`$(CLI) get myName`" = "myName 1"
	test "`$(CLI) hit myName`" = ""
	test "`$(CLI) get myName`" = "myName 2"
	test "`$(CLI) hit myName 10`" = ""
	test "`$(CLI) get myName`" = "myName 12"
	test "`$(CLI) dump`" = "myName 12"
	test "`$(CLI) reset`" = ""
	test "`$(CLI) list`" = ""
	test "`$(CLI) dump`" = ""
	test "`$(CLI) get myName`" = "myName 0"

clean:
	rm -rf run
