package main

import (
	"fmt"
	"log"
	"os"
	"strconv"
	"bitcoin_miner/message"
        "bitcoin_miner/hash"
        "context"
	"net"
        "time"
        "math"
)

const MIN_JOB_PER_WORKER = 40
const FUNC_CHANGER = 300
const DUMMY_SIZE = 4
const LOW_DEFAULT = uint64(0)

//Struct used to send results through the pipeline
type Result struct {
    hash uint64
    nonce uint64
}

//Function that will between an interval of values, compute the hash
//and compare with the previous ones and check which hash is bigger
//The biggest will be sent to the next stage of pipeline
func worker(msg string, low uint64, upper uint64, result chan<- Result){

    cHash := uint64(0)
    nonce := uint64(0)
    for i := low; i <= upper; i++ {
        nHash := hash.Hash(msg, i)
        if nHash > cHash {
            cHash = nHash
            nonce = i
        }

    }
    result <- Result{cHash, nonce}
}

//Function that receives results send by each worker and stores the one with the biggest hash
//After receiving a result from all workers sends the stored result
//If the connection was closed, the function must terminate, ignoring all results sent and to send
func assembler(ctxt context.Context, conn net.Conn, result <-chan Result, nWorkers int, low uint64, upper uint64) {

    finalResult := Result{uint64(0), uint64(0)}
    completeWorkers := 0
    terminate := false
    for {
        select {
            case <-ctxt.Done():
                terminate = true

            case ans := <-result:
                if ans.hash > finalResult.hash {
                    finalResult = ans
                }
                completeWorkers++

        }
        if completeWorkers == nWorkers {
            break
        }
    }
    if !terminate {
        msg, err := message.NewResult(finalResult.hash, finalResult.nonce, low, upper).ToJSON()
        if err != nil {
            fmt.Println("Failed to create result.")
        }

        conn.Write(msg)
        fmt.Println("Request complete.")

    }
    conn.Close()
}

//Function that defines the number of workers needed to split the computation
func setNumWorkers(upper uint64) int {
    count := 1
    if upper > MIN_JOB_PER_WORKER {
        var countTmp float64
        if upper < FUNC_CHANGER {
            //Computes de division between the upper value and the minimum amount of jobs per worker
            countTmp = float64(upper) / float64(MIN_JOB_PER_WORKER)
        } else {
            //Computes logarithm base 2 of upper value
            countTmp = math.Log2(float64(upper))
        }
        //Ceils the value obtain during the calc
        count = int(math.Ceil(countTmp))
    }
    return count
}

//Function that initiates the pipeline, by starting each worker and the assembler
//After starting the go routines, it will stay put while listening to the channel
//between the server and client
//If the connection is suddenly lost, the function must alert the assembler
func workPool(args *message.Message, conn net.Conn) {
    ctxt := context.Background()
    ctxt, cancel := context.WithCancel(ctxt)
    result := make(chan Result, 10)

    nWorkers := setNumWorkers(args.Upper)
    interval := args.Upper / uint64(nWorkers)
    low := LOW_DEFAULT
    for i := 0; i < nWorkers; i++ {
        if low+interval > args.Upper {
            interval = args.Upper - low
        }
        go worker(args.Data, low, low+interval, result)
        low += (interval + 1)
    }
    go assembler(ctxt, conn, result, nWorkers, args.Lower, args.Upper)

    dummy := make([]byte, DUMMY_SIZE)
    conn.Read(dummy)
    cancel()
    fmt.Println("Disconnected from client.")
}


//Function that handles each connection established with the server
//and creates a workpool to process the client request
func handleConnection(conn net.Conn) {
	msg := make([]byte, 0)
	data := make([]byte, 256)
	for {
                conn.SetReadDeadline(time.Now().Add(time.Second*2))
                size, err := conn.Read(data)
                conn.SetReadDeadline(time.Time{})
		if err != nil {
			break
		}
		msg = append(msg, data[:size]...)
	}
        args, err := message.FromJSON(msg)
	if err != nil {
		fmt.Println("Error parsing message.")
                conn.Close()
		return
	}
        go workPool(args, conn)
}


func startServer(port int) (bool, net.Listener) {
	ln, err := net.Listen("tcp", ":" + strconv.Itoa(port))
	if err != nil {
		return false, nil
	}

	return true, ln
}

var LOGF *log.Logger

func main() {
	// You may need a logger for debugging
	const (
		name = "log.txt"
		flag = os.O_RDWR | os.O_CREATE
		perm = os.FileMode(0666)
	)

	file, err := os.OpenFile(name, flag, perm)
	if err != nil {
		return
	}
	defer file.Close()

	LOGF = log.New(file, "", log.Lshortfile|log.Lmicroseconds)
	// Usage: LOGF.Println() or LOGF.Printf()

	const numArgs = 2
	if len(os.Args) != numArgs {
		fmt.Printf("Usage: ./%s <port>\n", os.Args[0])
		return
	}

	port, err := strconv.Atoi(os.Args[1])
	if err != nil {
		fmt.Println("Port must be a number:", err)
		return
	}

	connected, ln := startServer(port)
	if !connected {
		fmt.Println("Failed to start the server.")
		return
	}
	fmt.Println("Server listening on port", port)

	for {
		conn, err := ln.Accept()
		if err != nil {
			fmt.Println("Error connecting to a client!")
		} else {
			fmt.Println("Connected to a client!")
			go handleConnection(conn)
		}
	}
}
