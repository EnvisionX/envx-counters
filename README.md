# EnvisionX Counters library for Go

## Summary

Provides an API for target Golang application to update various
counters and an API for external processes to fetch current values
for the counters.

## Examples

```
import "envx_counters"

...
// increment "my_counter1" counter for 1
envx_counters.Hit("my_counter1")
...
// increment "my_counter2" counter for 5
envx_counters.HitDelta("my_counter2", 5)
...
// increment "my_counter3_4" for 1
envx_counters.Hitf("my_counter%d_%d", 3, 4)
...
// increment "my_counter4_5" for 3
envx_counters.HitDeltaf("my_counter%d", 3, 4, 5)
```
