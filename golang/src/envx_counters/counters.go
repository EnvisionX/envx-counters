/*
Provides an interface for gathering statistics like counters
and gauges.

Author: Aleksey Morarash <aleksey.morarash@envisionx.co>
Since: 19 Aug 2016
Copyright: 2016, ENVISIONX <info@envisionx.co>
*/

package envx_counters

import (
	"bufio"
	"fmt"
	"net"
	"os"
	"strconv"
	"strings"
	"time"
)

const (
	defaultPort = 8907

	envDisabled   = "ENVX_COUNTERS_DISABLED"
	envPortNumber = "ENVX_COUNTERS_PORT"

	opHit   = 1
	opSet   = 2
	opReset = 3
)

// type for communication between clients and counters server.
type eventType struct {
	op    uint8
	name  string
	value int64
}

// storage for counter and gauge values
type storageType map[string]int64

var disabled bool
var queue chan eventType
var storage storageType

// Package initialization.
// Start all daemons.
func init() {
	disabled = len(os.Getenv(envDisabled)) > 0
	storage = make(storageType)
	if !disabled {
		queue = make(chan eventType, 10)
		go srv()
		go tcp_srv()
		go udp_srv()
	}
}

// Increment counter value with 1.
func Hit(name string) {
	if disabled {
		return
	}
	queue <- eventType{
		op:    opHit,
		name:  name,
		value: 1,
	}
}

// Increment counter value with 1.
func Hitf(format string, arg ...interface{}) {
	if disabled {
		return
	}
	queue <- eventType{
		op:    opHit,
		name:  fmt.Sprintf(format, arg...),
		value: 1,
	}
}

// Set new value for gauge (or counter).
func Set(name string, value int64) {
	if disabled {
		return
	}
	queue <- eventType{
		op:    opSet,
		name:  name,
		value: value,
	}
}

// Delete all collected counters.
func Reset() {
	if disabled {
		return
	}
	queue <- eventType{
		op: opReset,
	}
}

// Dump all collected counters to the stdout.
func Print() {
	if disabled {
		return
	}
	for name, value := range storage {
		fmt.Printf("%s %d\n", name, value)
	}
}

// Get value for the counter.
func Get(name string) int64 { return storage[name] }

// Server thread which actually increments values of the counters.
func srv() {
	for event := range queue {
		switch event.op {
		case opHit:
			storage[event.name] += event.value
		case opSet:
			storage[event.name] = event.value
		case opReset:
			storage = make(storageType)
		}
	}
}

// This thread accepts TCP connections from the network
func tcp_srv() {
	port := getPort()
	bindAddr := fmt.Sprintf(":%d", port)
	var ss net.Listener
	var err error
	// loop until success
	for {
		ss, err = net.Listen("tcp", bindAddr)
		if err == nil {
			break
		}
		time.Sleep(5 * time.Second)
	}
	defer ss.Close()
	for {
		socket, err := ss.Accept()
		if err != nil {
			continue
		}
		go handleTcpConnection(socket)
	}
}

// TCP connection handler thread
func handleTcpConnection(socket net.Conn) {
	defer socket.Close()
	scanner := bufio.NewScanner(socket)
	for scanner.Scan() {
		reply, err := processExtRequest(scanner.Text())
		if err {
			return
		}
		if 0 < len(reply) {
			socket.Write([]byte(reply))
		}
	}
}

// This thread serves requests via UDP/IP from the network
func udp_srv() {
	port := getPort()
	bindAddr := fmt.Sprintf(":%d", port)
	var ss net.PacketConn
	var err error
	// loop until success
	for {
		ss, err = net.ListenPacket("udp", bindAddr)
		if err == nil {
			break
		}
		time.Sleep(5 * time.Second)
	}
	defer ss.Close()
	ss.SetReadDeadline(time.Time{})
	buff := make([]byte, 1024)
	for {
		n, from, err := ss.ReadFrom(buff)
		if err == nil {
			reply, is_err := processExtRequest(string(buff[0:n]))
			if !is_err && 0 < len(reply) {
				ss.WriteTo([]byte(reply), from)
			}
		}
	}
}

// Process request from external client.
func processExtRequest(request string) (reply string, error bool) {
	tokens := strings.Fields(request)
	if len(tokens) < 1 {
		return "", true
	}
	switch strings.ToUpper(tokens[0]) {
	case "GET":
		if len(tokens) != 2 {
			return "", true
		}
		reply := fmt.Sprintln(tokens[1], storage[tokens[1]])
		return reply, false
	case "HIT":
		if len(tokens) != 2 {
			return "", true
		}
		Hit(tokens[1])
		return "", false
	case "SET":
		if len(tokens) != 3 {
			return "", true
		}
		value, err := strconv.ParseInt(tokens[2], 10, 64)
		if err != nil {
			return "", true
		}
		Set(tokens[1], value)
		return "", false
	case "LIST":
		if len(tokens) != 1 {
			return "", true
		}
		reply := ""
		for name, _ := range storage {
			if 0 < len(reply) {
				reply += " "
			}
			reply += name
		}
		return reply, false
	case "DUMP":
		if len(tokens) != 1 {
			return "", true
		}
		reply := ""
		for name, value := range storage {
			reply += fmt.Sprintln(name, value)
		}
		return reply, false
	}
	return "", true
}

// Return TCP/UDP port number to be listened by servers.
func getPort() uint32 {
	strPort := os.Getenv(envPortNumber)
	port, err := strconv.ParseUint(strPort, 10, 32)
	if err != nil {
		port = defaultPort
	}
	return uint32(port)
}
