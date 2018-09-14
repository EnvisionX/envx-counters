/**
Implements network interface to counters storage.
*/

package envx_counters

import (
	"bufio"
	"fmt"
	"log"
	"net"
	"os"
	"strconv"
	"strings"
	"time"
)

const DEFAULT_PORT = 8907

// Goroutine.
// Accepts TCP connections from the network
func startServer() {
	port := DEFAULT_PORT
	if s := os.Getenv(ENV_PORT); s != "" {
		n, err := strconv.ParseUint(s, 10, 16)
		if err != nil {
			log.Fatalf("Invalid value %#v for %s "+
				"environment variable: %s",
				s, ENV_PORT, err)
		}
		port = int(n)
	}
	bindAddr := fmt.Sprintf(":%d", port)
	for {
		ss, err := net.Listen("tcp", bindAddr)
		if err != nil {
			// loop until success
			time.Sleep(5 * time.Second)
			continue
		}
		for {
			socket, err := ss.Accept()
			if err != nil {
				time.Sleep(100 * time.Millisecond)
				continue
			}
			go handleTcpConnection(socket)
		}
	}
}

// TCP connection handler thread
func handleTcpConnection(socket net.Conn) {
	scanner := bufio.NewScanner(socket)
	for scanner.Scan() {
		reply, err := processExtRequest(scanner.Text())
		if err {
			socket.Close()
			return
		}
		if 0 < len(reply) {
			socket.Write([]byte(reply))
		}
	}
	socket.Close()
}

// Process request from external client.
func processExtRequest(request string) (reply string, error bool) {
	tokens := strings.Fields(request)
	if len(tokens) < 1 {
		return "", true
	}
	var (
		command = strings.ToUpper(tokens[0])
		args    = tokens[1:]
	)
	switch command {
	case "GET":
		if len(args) != 1 {
			return "", true
		}
		reply := fmt.Sprintln(args[0], Get(args[0]))
		return reply, false
	case "HIT":
		switch len(args) {
		case 1:
			Hit(args[0])
			return "", false
		case 2:
			if n, err := strconv.ParseInt(args[1], 10, 64); err == nil {
				HitDelta(args[0], n)
				return "", false
			}
		}
		return "", true
	case "SET":
		if len(args) != 2 {
			return "", true
		}
		value, err := strconv.ParseInt(args[1], 10, 64)
		if err != nil {
			return "", true
		}
		Set(args[0], value)
		return "", false
	case "LIST":
		if len(args) != 0 {
			return "", true
		}
		return strings.Join(List(), " ") + "\n", false
	case "DUMP":
		if len(args) != 0 {
			return "", true
		}
		reply := ""
		for _, name := range List() {
			reply += fmt.Sprintf("%s %d\n", name, Get(name))
		}
		if len(reply) == 0 {
			reply = "\n"
		}
		return reply, false
	case "RESET":
		if len(args) != 0 {
			return "", true
		}
		Reset()
		return "\n", false
	}
	return "", true
}
