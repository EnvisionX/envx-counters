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
	"sync"
	"time"
)

const (
	DEFAULT_PORT = 8907
	ENV_DISABLED = "ENVX_COUNTERS_DISABLED"
	ENV_PORT     = "ENVX_COUNTERS_PORT"
)

var (
	gDisabled         bool
	gStorage          = map[string]int64{}
	gStorageMu        = sync.Mutex{}
	gStorageCallbacks = map[string]func() int64{}
)

// Package initialization.
// Start all daemons.
func init() {
	gDisabled = len(os.Getenv(ENV_DISABLED)) > 0
	if !gDisabled {
		go tcp_srv()
		go udp_srv()
	}
}

// Register callback function.
func Register(name string, callback func() int64) {
	if gDisabled {
		return
	}
	gStorageMu.Lock()
	gStorageCallbacks[name] = callback
	gStorageMu.Unlock()
}

// Unregister callback function.
func Unregister(name string) {
	if gDisabled {
		return
	}
	gStorageMu.Lock()
	delete(gStorageCallbacks, name)
	gStorageMu.Unlock()
}

// Increment counter value with 1.
func Hit(name string) {
	HitDelta(name, 1)
}

// Increment counter value with delta.
func HitDelta(name string, delta int64) {
	if gDisabled {
		return
	}
	gStorageMu.Lock()
	v := gStorage[name]
	gStorage[name] = v + delta
	gStorageMu.Unlock()
}

// Increment counter value with 1.
func Hitf(format string, arg ...interface{}) {
	HitDeltaf(format, 1, arg...)
}

// Increment counter value with delta.
func HitDeltaf(format string, delta int64, arg ...interface{}) {
	if gDisabled {
		return
	}
	name := fmt.Sprintf(format, arg...)
	gStorageMu.Lock()
	v := gStorage[name]
	gStorage[name] = v + delta
	gStorageMu.Unlock()
}

// Set new value for gauge (or counter).
func Set(name string, value int64) {
	if gDisabled {
		return
	}
	gStorageMu.Lock()
	gStorage[name] = value
	gStorageMu.Unlock()
}

// Set new value for gauge (or counter).
func Setf(format string, value int64, arg ...interface{}) {
	if gDisabled {
		return
	}
	name := fmt.Sprintf(format, arg...)
	gStorageMu.Lock()
	gStorage[name] = value
	gStorageMu.Unlock()
}

// Delete all collected counters.
func Reset() {
	if gDisabled {
		return
	}
	gStorageMu.Lock()
	gStorage = map[string]int64{}
	gStorageMu.Unlock()
}

// Dump all collected counters to the stdout.
func Print() {
	if gDisabled {
		return
	}
	gStorageMu.Lock()
	for name, value := range gStorage {
		fmt.Printf("%s %d\n", name, value)
	}
	gStorageMu.Unlock()
}

// Get value for the counter.
func Get(name string) int64 {
	if gDisabled {
		return 0
	}
	gStorageMu.Lock()
	if value, ok := gStorage[name]; ok {
		gStorageMu.Unlock()
		return value
	}
	if callback, ok := gStorageCallbacks[name]; ok {
		gStorageMu.Unlock()
		return callback()
	}
	gStorageMu.Unlock()
	return 0
}

// List all counter names registered.
func List() []string {
	if gDisabled {
		return []string{}
	}
	gStorageMu.Lock()
	sorter := map[string]bool{}
	for k, _ := range gStorage {
		sorter[k] = true
	}
	for k, _ := range gStorageCallbacks {
		sorter[k] = true
	}
	res := make([]string, len(sorter))
	i := 0
	for k, _ := range sorter {
		res[i] = k
		i++
	}
	gStorageMu.Unlock()
	return res
}

// This thread accepts TCP connections from the network
func tcp_srv() {
	bindAddr := fmt.Sprintf(":%d", getPort())
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
		reply := fmt.Sprintln(tokens[1], Get(tokens[1]))
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
		return strings.Join(List(), " ") + "\n", false
	case "DUMP":
		if len(tokens) != 1 {
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
		if len(tokens) != 1 {
			return "", true
		}
		Reset()
		return "\n", false
	}
	return "", true
}

// Return TCP/UDP port number to be listened by servers.
func getPort() uint16 {
	strPort := os.Getenv(ENV_PORT)
	port, err := strconv.ParseUint(strPort, 10, 16)
	if err != nil {
		port = DEFAULT_PORT
	}
	return uint16(port)
}
