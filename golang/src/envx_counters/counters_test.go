package envx_counters

import (
	"testing"
)

func TestMain(t *testing.T) {
	if Get("counter1") != 0 {
		t.Error()
	}
}
