/*
Provides an interface for gathering statistics like counters
and gauges.

Author: Aleksey Morarash <aleksey.morarash@envisionx.co>
Since: 19 Aug 2016
Copyright: 2016, EnvisionX <info@envisionx.co>
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
	"sync/atomic"
	"time"
)

const (
	DEFAULT_PORT = 8907
	ENV_DISABLED = "ENVX_COUNTERS_DISABLED"
	ENV_PORT     = "ENVX_COUNTERS_PORT"
)

var (
	gDisabled  bool
	gStorage   map[string]*int64
	gCallbacks map[string]func() int64
	gLock      = sync.RWMutex{}
)

// Package initialization.
// Start all daemons.
func init() {
	gDisabled = len(os.Getenv(ENV_DISABLED)) > 0
	if !gDisabled {
		Initialize()
		go tcp_srv()
	}
}

// Drop all existing counters and all registered callbacks.
func Initialize() {
	gStorage = map[string]*int64{}
	gCallbacks = map[string]func() int64{}
}

// Register callback function.
func Register(name string, callback func() int64) {
	if gDisabled {
		return
	}
	gLock.Lock()
	gCallbacks[name] = callback
	gLock.Unlock()
}

// Unregister callback function.
func Unregister(name string) {
	if gDisabled {
		return
	}
	gLock.Lock()
	delete(gCallbacks, name)
	gLock.Unlock()
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

	gLock.RLock()
	if v := gStorage[name]; v != nil {
		gLock.RUnlock()
		atomic.AddInt64(v, delta)
		return
	}
	gLock.RUnlock()
	i := delta
	gLock.Lock()
	gStorage[name] = &i
	gLock.Unlock()
}

// Increment counter value with 1.
func Hitf(format string, args ...interface{}) {
	if gDisabled {
		return
	}
	HitDelta(fmt.Sprintf(format, args...), 1)
}

// Increment counter value with delta.
func HitDeltaf(format string, delta int64, args ...interface{}) {
	if gDisabled {
		return
	}
	HitDelta(fmt.Sprintf(format, args...), delta)
}

// Set new value for gauge (or counter).
func Set(name string, value int64) {
	if gDisabled {
		return
	}

	gLock.RLock()
	if v := gStorage[name]; v != nil {
		gLock.RUnlock()
		atomic.StoreInt64(v, value)
		return
	}
	gLock.RUnlock()
	i := value
	gLock.Lock()
	gStorage[name] = &i
	gLock.Unlock()
}

// Set new value for gauge (or counter).
func Setf(format string, value int64, args ...interface{}) {
	if gDisabled {
		return
	}
	Set(fmt.Sprintf(format, args...), value)
}

// Drop counter from the storage.
func Unset(name string) {
	gLock.Lock()
	delete(gStorage, name)
	gLock.Unlock()
}

// Drop counter from the storage.
func Unsetf(format string, args ...interface{}) {
	if gDisabled {
		return
	}
	Unset(fmt.Sprintf(format, args...))
}

// Delete all collected counters, except registered callbacks.
func Reset() {
	if gDisabled {
		return
	}
	gLock.Lock()
	gStorage = map[string]*int64{}
	gLock.Unlock()
}

// Dump all collected counters to the stdout.
func Print() {
	if gDisabled {
		return
	}
	for _, name := range List() {
		fmt.Printf("%s %d\n", name, Get(name))
	}
}

// Get value for the counter.
func Get(name string) int64 {
	if gDisabled {
		return 0
	}
	gLock.RLock()
	if value, ok := gStorage[name]; ok {
		gLock.RUnlock()
		return atomic.LoadInt64(value)
	}
	if callback, ok := gCallbacks[name]; ok {
		gLock.RUnlock()
		return callback()
	}
	gLock.RUnlock()
	return 0
}

// List all counter names registered.
func List() []string {
	if gDisabled {
		return []string{}
	}
	uniqer := map[string]bool{}
	gLock.RLock()
	for k := range gStorage {
		uniqer[k] = true
	}
	for k := range gCallbacks {
		uniqer[k] = true
	}
	gLock.RUnlock()
	res := make([]string, len(uniqer))
	i := 0
	for k := range uniqer {
		res[i] = k
		i++
	}
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

// Return TCP port number to be listened by servers.
func getPort() uint16 {
	strPort := os.Getenv(ENV_PORT)
	port, err := strconv.ParseUint(strPort, 10, 16)
	if err != nil {
		port = DEFAULT_PORT
	}
	return uint16(port)
}
