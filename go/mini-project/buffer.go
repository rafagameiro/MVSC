
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

//MUTUAL EXCLUSION: In our implementation, we achieve mutual exlusion through
//the use of a channel shared between all worker threads (access). When the 
//buffer is initialised, a signal is sent through the channel, indicating that
//it is open, for a worker to access the critical region it has to receive a 
//signal through the channel, and because once one worker receives it the others
//can't, this guarantees one worker accessing the critical region at a time. 
//After accessing, the worker will send another signal through the channel to
//indicate it is open again.

type Buffer struct {
    b []BufferElem
    in chan AddRequest
    out chan chan BufferElem
    access chan  bool
}


func New(capacity int) *Buffer {
    bb := &Buffer{
        make([]BufferElem, 0, capacity), 
        make(chan AddRequest), 
        make(chan chan BufferElem),
        make(chan bool),
    }

    go bb.insertHandler()
    go bb.insertHandler()
    go bb.removeHandler()
    go bb.removeHandler()
    go func () {
        bb.access <- true
    }()
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
        for {
            <- buffer.access
            if len(buffer.b) == cap(buffer.b) {
                //blocking
                buffer.access <- true
                continue
            }
            buffer.b = append(buffer.b, elem.elem)
            fmt.Println("added element", len(buffer.b), "/", cap(buffer.b))
            go func () {
                buffer.access <- true
            }()
            elem.rec <- true
            break
        }
    }
}

func (buffer *Buffer) removeHandler() {
    for rec := range buffer.out {
        for {
            <- buffer.access
            if len(buffer.b) == 0 {
                //blocking
                buffer.access <- true
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
            go func () {
                buffer.access <- true
            }()
            break
        }
    }
}

