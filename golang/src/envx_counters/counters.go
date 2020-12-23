/*
Provides an interface for gathering statistics like counters
and gauges.

Author: Aleksey Morarash <aleksey.morarash@envisionx.co>
Since: 19 Aug 2016
Copyright: 2016, EnvisionX <info@envisionx.co>
*/

package envx_counters

import (
	"fmt"
	"os"
	"sync"
)

const (
	ENV_DISABLED = "ENVX_COUNTERS_DISABLED"
	ENV_PORT     = "ENVX_COUNTERS_PORT"
)

var (
	gDisabled  bool
	gStorage   map[string]int64
	gCallbacks map[string]func() int64
	gLock      sync.RWMutex
)

// Package initialization.
// Start all daemons.
func init() {
	gDisabled = len(os.Getenv(ENV_DISABLED)) > 0
	if !gDisabled {
		Initialize()
		go startServer()
	}
}

// Drop all existing counters and all registered callbacks.
func Initialize() {
	gStorage = map[string]int64{}
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
	gLock.Lock()
	v := gStorage[name]
	gStorage[name] = v + delta
	gLock.Unlock()
}

// Increment counter value with 1.
func Hitf(format string, args ...interface{}) {
	HitDeltaf(format, 1, args...)
}

// Increment counter value with delta.
func HitDeltaf(format string, delta int64, args ...interface{}) {
	if gDisabled {
		return
	}
	name := fmt.Sprintf(format, args...)
	gLock.Lock()
	v := gStorage[name]
	gStorage[name] = v + delta
	gLock.Unlock()
}

// Set new value for gauge (or counter).
func Set(name string, value int64) {
	if gDisabled {
		return
	}
	gLock.Lock()
	gStorage[name] = value
	gLock.Unlock()
}

// Set new value for gauge (or counter).
func Setf(format string, value int64, args ...interface{}) {
	if gDisabled {
		return
	}
	name := fmt.Sprintf(format, args...)
	gLock.Lock()
	gStorage[name] = value
	gLock.Unlock()
}

// Drop counter from the storage.
func Unset(name string) {
	gLock.Lock()
	delete(gStorage, name)
	gLock.Unlock()
}

// Drop counter from the storage.
func Unsetf(format string, args ...interface{}) {
	Unset(fmt.Sprintf(format, args...))
}

// Delete all collected counters, except registered callbacks.
func Reset() {
	if gDisabled {
		return
	}
	gLock.Lock()
	gStorage = map[string]int64{}
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
		return value
	}
	if callback, ok := gCallbacks[name]; ok {
		gLock.RUnlock()
		return callback()
	}
	gLock.RUnlock()
	return 0
}

// Getf fetches value for the counter.
func Getf(format string, args ...interface{}) int64 {
	if gDisabled {
		return 0
	}
	return Get(fmt.Sprintf(format, args...))
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
