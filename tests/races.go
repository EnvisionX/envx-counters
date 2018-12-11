package main

import (
	"envx_counters"
	"log"
)

func main() {
	var (
		hits    = 100000
		workers = 50
		c       = make(chan bool, workers)
	)
	for i := 0; i < workers; i++ {
		go worker(c, hits)
	}
	for i := 0; i < workers; i++ {
		<-c
	}
	total := int(envx_counters.Get("counter"))
	if expected := hits * workers; total != expected {
		log.Fatalf("expected %d but found: %d", expected, total)
	}
	log.Println("OK")
}

func worker(c chan bool, count int) {
	for 0 < count {
		envx_counters.Hit("counter")
		count--
	}
	c <- true
}
