
package boundedbuffer

import (
    "testing"
    "sync"
)

func AddSpam(b *Buffer, iterations int, wg *sync.WaitGroup) {
    defer wg.Done()
    i := 0
    for i < iterations {
        b.Add(1)
        i++
    }
}

func RemoveSpam(b *Buffer, iterations int, wg *sync.WaitGroup) {
    defer wg.Done()
    i := 0
    for i < iterations {
        b.Remove()
        i++
    }
}

func TestGlobalDeadlock(t *testing.T) {
    var wg sync.WaitGroup
    wg.Add(2)

    var buffer *Buffer
    buffer = New(1000)
    
    go AddSpam(buffer, 1001, &wg)
    go RemoveSpam(buffer, 0, &wg)
    
    wg.Wait()
}

func TestBuffer(t *testing.T) {
    
    var wg sync.WaitGroup
    wg.Add(2)

    var buffer *Buffer
    buffer = New(1000)
    
    go AddSpam(buffer, 1000, &wg)
    go RemoveSpam(buffer, 1000, &wg)
    
    wg.Wait()
}

func TestRace(t *testing.T) {
    
    var wg sync.WaitGroup
    wg.Add(2)

    var buffer *Buffer
    buffer = New(20)
    
    go AddSpam(buffer, 20, &wg)
    go RemoveSpam(buffer, 20, &wg)
    
    wg.Wait()
}

