
package boundedbuffer

import (
    "fmt"
)

/************************Bounded Buffer***********************/
type BufferElem interface {}

type AddRequest struct {
    elem BufferElem
    rec chan bool
}

type BoundedBuffer interface {
    New(capacity int) *BoundedBuffer
    Add(elem BufferElem)
    Remove() BufferElem
}


type Buffer struct {
    b []BufferElem
    in chan AddRequest
    out chan chan BufferElem
}


func New(capacity int) *Buffer {
    bb := &Buffer{
        make([]BufferElem, 0, capacity), 
        make(chan AddRequest), 
        make(chan chan BufferElem),
    }

    go bb.insertHandler()
    go bb.insertHandler()
    go bb.removeHandler()
    go bb.removeHandler()
    return bb
}


func (buffer *Buffer) Add(elem BufferElem) {
    rec := make(chan bool)
    request := AddRequest{elem, rec}
    buffer.in <- request
    <-rec
}

func (buffer *Buffer) Remove() BufferElem {
    rec := make(chan BufferElem)
    buffer.out <- rec
    var r BufferElem
    r = -1
    select {
    case x := <-rec:
        r = x
    }
    return r
}

func (buffer *Buffer) insertHandler() {
    for elem := range buffer.in {
        if len(buffer.b) == cap(buffer.b) {
            //blocking
            continue
        }
        buffer.b = append(buffer.b, elem.elem)
        fmt.Println("added element", len(buffer.b), "/", cap(buffer.b))
        elem.rec <- true
    }
}

func (buffer *Buffer) removeHandler() {
    for rec := range buffer.out {
        for {
            if len(buffer.b) == 0 {
                //blocking
                continue
            }
            var element BufferElem
            element = buffer.b[0]
            nBuffer := make([]BufferElem, 0, cap(buffer.b))
            for _ , num := range buffer.b[1:] {
                nBuffer = append(nBuffer, num)
            }
            buffer.b = nBuffer
            rec <- element
            fmt.Println("removed element", len(buffer.b), "/", cap(buffer.b))
            break
        }
    }
}

