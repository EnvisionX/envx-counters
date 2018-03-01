package envx_counters

import (
	"strconv"
	"sync"
	"testing"
)

func TestRegister(t *testing.T) {
	Initialize()
	testset := []struct {
		Name     string
		RetValue int64
	}{
		{"c1", 45},
		{"c2", 53},
		{"c1", 64},
		{"c3", 4},
	}
	for n, test := range testset {
		Register(test.Name, func() int64 { return test.RetValue })
		assert(t, n, test.Name, test.RetValue)
	}
}

func TestUnregister(t *testing.T) {
	Initialize()
	c := "c1"
	assert(t, 0, c, 0)
	Register(c, func() int64 { return 3 })
	assert(t, 0, c, 3)
	Unregister(c)
	assert(t, 0, c, 0)
}

func TestHit(t *testing.T) {
	Initialize()
	testset := []struct {
		PreExpected  int64
		HitName      string
		PostExpected int64
	}{
		{0, "c1", 1},
		{1, "c1", 2},
		{0, "c2", 1},
		{2, "c1", 3},
		{3, "c1", 4},
		{1, "c2", 2},
		{0, "c3", 1},
	}
	for n, test := range testset {
		assert(t, n, test.HitName, test.PreExpected)
		Hit(test.HitName)
		assert(t, n, test.HitName, test.PostExpected)
	}
}

func TestHitDelta(t *testing.T) {
	Initialize()
	testset := []struct {
		PreExpected  int64
		HitName      string
		Delta        int64
		PostExpected int64
	}{
		{0, "c1", 2, 2},
		{2, "c1", 3, 5},
		{0, "c2", 4, 4},
		{5, "c1", 5, 10},
		{10, "c1", 6, 16},
		{4, "c2", 7, 11},
		{0, "c3", 8, 8},
	}
	for n, test := range testset {
		assert(t, n, test.HitName, test.PreExpected)
		HitDelta(test.HitName, test.Delta)
		assert(t, n, test.HitName, test.PostExpected)
	}
}

func TestHitf(t *testing.T) {
	Initialize()
	testset := []struct {
		PreExpected   int64
		HitNameFormat string
		HitNameArgs   []interface{}
		HitName       string
		PostExpected  int64
	}{
		{0, "c%d", []interface{}{1}, "c1", 1},
		{1, "c%d", []interface{}{1}, "c1", 2},
		{0, "c%d_%d", []interface{}{2, 3}, "c2_3", 1},
		{2, "c%d", []interface{}{1}, "c1", 3},
		{3, "c%d", []interface{}{1}, "c1", 4},
		{1, "c%d_%d", []interface{}{2, 3}, "c2_3", 2},
		{0, "c%d", []interface{}{3}, "c3", 1},
	}
	for n, test := range testset {
		assert(t, n, test.HitName, test.PreExpected)
		Hitf(test.HitNameFormat, test.HitNameArgs...)
		assert(t, n, test.HitName, test.PostExpected)
	}
}

func TestHitDeltaf(t *testing.T) {
	Initialize()
	testset := []struct {
		PreExpected   int64
		Delta         int64
		HitNameFormat string
		HitNameArgs   []interface{}
		HitName       string
		PostExpected  int64
	}{
		{0, 2, "c%d", []interface{}{1}, "c1", 2},
		{2, 3, "c%d", []interface{}{1}, "c1", 5},
		{0, 4, "c%d_%d", []interface{}{2, 3}, "c2_3", 4},
		{5, 5, "c%d", []interface{}{1}, "c1", 10},
		{10, 6, "c%d", []interface{}{1}, "c1", 16},
		{4, 7, "c%d_%d", []interface{}{2, 3}, "c2_3", 11},
		{0, 8, "c%d", []interface{}{3}, "c3", 8},
	}
	for n, test := range testset {
		assert(t, n, test.HitName, test.PreExpected)
		HitDeltaf(test.HitNameFormat, test.Delta, test.HitNameArgs...)
		assert(t, n, test.HitName, test.PostExpected)
	}
}

func TestSet(t *testing.T) {
	Initialize()
	testset := []struct {
		PreExpected  int64
		SetName      string
		NewValue     int64
		PostExpected int64
	}{
		{0, "c1", 2, 2},
		{2, "c1", 3, 3},
		{0, "c2", 4, 4},
		{3, "c1", 5, 5},
		{5, "c1", 6, 6},
		{4, "c2", 7, 7},
		{0, "c3", 8, 8},
	}
	for n, test := range testset {
		assert(t, n, test.SetName, test.PreExpected)
		Set(test.SetName, test.NewValue)
		assert(t, n, test.SetName, test.PostExpected)
	}
}

func TestSetf(t *testing.T) {
	Initialize()
	testset := []struct {
		PreExpected   int64
		SetNameFormat string
		SetNameArgs   []interface{}
		SetName       string
		NewValue      int64
		PostExpected  int64
	}{
		{0, "c%d", []interface{}{1}, "c1", 2, 2},
		{2, "c%d", []interface{}{1}, "c1", 3, 3},
		{0, "c%d_%d", []interface{}{2, 3}, "c2_3", 4, 4},
		{3, "c%d", []interface{}{1}, "c1", 5, 5},
		{5, "c%d", []interface{}{1}, "c1", 6, 6},
		{4, "c%d_%d", []interface{}{2, 3}, "c2_3", 7, 7},
		{0, "c%d", []interface{}{3}, "c3", 8, 8},
	}
	for n, test := range testset {
		assert(t, n, test.SetName, test.PreExpected)
		Setf(test.SetNameFormat, test.NewValue, test.SetNameArgs...)
		assert(t, n, test.SetName, test.PostExpected)
	}
}

func TestUnset(t *testing.T) {
	Initialize()
	c1 := "c1"
	c2 := "c2"
	assert(t, 0, c1, 0)
	assert(t, 0, c2, 0)
	Hit(c1)
	Hit(c2)
	assert(t, 1, c1, 1)
	assert(t, 1, c2, 1)
	Unset(c1)
	assert(t, 2, c1, 0)
	assert(t, 2, c2, 1)
	a := List()
	if len(a) != 1 {
		t.Fatalf("expected list len 1 but %d found", len(a))
	}
	if a[0] != c2 {
		t.Fatalf("expected %#v but %#v found", c2, a[0])
	}
}

func TestUnsetf(t *testing.T) {
	Initialize()
	c1 := "c1"
	c2 := "c2"
	assert(t, 0, c1, 0)
	assert(t, 0, c2, 0)
	Hit(c1)
	Hit(c2)
	assert(t, 1, c1, 1)
	assert(t, 1, c2, 1)
	Unsetf("c%d", 1)
	assert(t, 2, c1, 0)
	assert(t, 2, c2, 1)
	a := List()
	if len(a) != 1 {
		t.Fatalf("expected list len 1 but %d found", len(a))
	}
	if a[0] != c2 {
		t.Fatalf("expected %#v but %#v found", c2, a[0])
	}
}

func TestReset(t *testing.T) {
	Initialize()
	c1 := "c1"
	c2 := "c2"
	assert(t, 0, c1, 0)
	Hit(c1)
	assert(t, 0, c1, 1)
	assert(t, 0, c2, 0)
	Hit(c2)
	Hit(c2)
	assert(t, 0, c2, 2)
	Reset()
	assert(t, 0, c1, 0)
	assert(t, 0, c2, 0)
}

func TestList(t *testing.T) {
	Initialize()
	c1 := "c1"
	c2 := "c2"
	c3 := "c3"
	if a := List(); len(a) != 0 {
		t.Fatalf("expected len %d but %d found", 0, len(a))
	}
	Hit(c1)
	Register(c2, func() int64 { return 2 })
	Register(c3, func() int64 { return 3 })
	Hit(c3)
	a := List()
	if len(a) != 3 {
		t.Fatalf("expected len %d but %d found", 3, len(a))
	}
	m := map[string]bool{}
	for _, name := range a {
		m[name] = true
	}
	for _, name := range []string{c1, c2, c3} {
		if !m[name] {
			t.Fatalf("elem %#v not found", name)
		}
	}
}

func TestHitRace(t *testing.T) {
	Initialize()
	wg := sync.WaitGroup{}
	times := 20
	wg.Add(times)
	for i := 0; i < times; i++ {
		go func() {
			Hit("test")
			wg.Done()
		}()
	}
	wg.Wait()
	if Get("test") != int64(times) {
		t.Fatal("should be", times, "but is", Get("test"))
	}
}

func TestOnlyRaceHitRemove(t *testing.T) {
	Initialize()
	wg := sync.WaitGroup{}
	times := 20
	wg.Add(times)
	for i := 0; i < times; i++ {
		go func() {
			Hit("test")
			wg.Done()
		}()
	}
	Unset("test")
	wg.Wait()
}

func TestOnlyRaceGetHit(t *testing.T) {
	Initialize()
	wg := sync.WaitGroup{}
	times := 20
	wg.Add(times)
	for i := 0; i < times; i++ {
		go func() {
			Hit("test")
			wg.Done()
		}()
	}
	Get("test")
	wg.Wait()
}

func TestOnlyRaceAll(t *testing.T) {
	Initialize()
	wg := sync.WaitGroup{}
	times := 20
	wg.Add(times * 4)
	for i := 0; i < times; i++ {
		go func() {
			Hit("test")
			wg.Done()
		}()
	}
	for i := 0; i < times; i++ {
		go func() {
			Get("test")
			wg.Done()
		}()
	}
	for i := 0; i < times; i++ {
		go func() {
			Set("test", 0)
			wg.Done()
		}()
	}
	for i := 0; i < times; i++ {
		go func() {
			Reset()
			wg.Done()
		}()
	}
	wg.Wait()
}

func BenchmarkHit(b *testing.B) {
	Initialize()
	for i := 0; i < b.N; i++ {
		Hit("test")
	}
	if Get("test") != int64(b.N) {
		b.Fatal("should be", b.N, "but is", Get("test"))
	}
}

func BenchmarkSetNew(b *testing.B) {
	Initialize()
	for i := 0; i < b.N; i++ {
		Set("test"+strconv.Itoa(i), int64(i))
	}

	for i := 0; i < b.N; i++ {
		if Get("test"+strconv.Itoa(i)) != int64(i) {
			b.Fatal("test"+strconv.Itoa(i), "should be", i,
				"but is", Get("test"+strconv.Itoa(i)))
		}
	}
}

func BenchmarkSetExisting(b *testing.B) {
	Initialize()
	for i := 0; i <= b.N; i++ {
		Set("test", int64(i))
	}

	if Get("test") != int64(b.N) {
		b.Fatal("test", "should be", b.N,
			"but is", Get("test"))
	}
}

func BenchmarkGet(b *testing.B) {
	Initialize()
	Set("test", 44421421)

	var n int64
	for i := 0; i < b.N; i++ {
		n = Get("test")
		_ = n
	}
}

func assert(t *testing.T, testN int, name string, expectedValue int64) {
	if value := Get(name); value != expectedValue {
		t.Fatalf("#%d> expected %d but %d found",
			testN, expectedValue, value)
	}
}
