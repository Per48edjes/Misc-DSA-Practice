package main

import (
	"fmt"
	"sync"
	"time"
)

var mu sync.Mutex

func increment(container []int, pos int) {
	mu.Lock()
	defer mu.Unlock()

	if pos >= 0 && pos < len(container) {
		current := container[pos]
		time.Sleep(100 * time.Microsecond)
		container[pos] = current + 1
	} else {
		panic("Index out of bounds")
	}
}

func main() {
	shared := make([]int, 1)
	shared[0] = 0

	var wg sync.WaitGroup
	for range 10_000 {
		wg.Add(1)
		go func() {
			defer wg.Done()
			increment(shared, 0)
		}()
	}
	wg.Wait()
	fmt.Println(shared[0])
}
