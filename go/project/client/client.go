package main

import (
	"fmt"
	"os"
	"strconv"
	"bitcoin_miner/message"
	"net"
)

func main() {
	const numArgs = 4
	if len(os.Args) != numArgs {
		fmt.Printf("Usage: ./%s <host:port> <message> <max>\n", os.Args[0])
		return
	}
	host := os.Args[1]
	payload := os.Args[2]
	max, err := strconv.ParseUint(os.Args[3], 10, 64)
	if err != nil {
		fmt.Printf("%s is not a number.\n", os.Args[3])
		return
	}

	conn, err := net.Dial("tcp", host)
	if err != nil {
		fmt.Println("Failed to connect to the server.")
		return
	}
	fmt.Println("Connected to a server.")
	data, err := message.NewRequest(payload, 0, max).ToJSON()
	if err != nil {
		fmt.Println("Failed to create Json.")
		return
	}
	conn.Write(data)
        awaitResult(conn)
}

//Function that waits for the answer from the server
//After receiving it, prints it
//If the server got disconnected, print an error message and terminates
func awaitResult(conn net.Conn) {
    msg := make([]byte, 0)
    data := make([]byte, 256)
    for {
        size, err := conn.Read(data)
        if err != nil {
            break
        }

        msg = append(msg, data[:size]...)
    }
    if len(msg) == 0 {
        printDisconnected()
        return
    }

    args, err := message.FromJSON(msg)
    if err != nil {
	fmt.Println("Error parsing message.")
	return
    }

    printResult(args.Hash, args.Nonce)
    conn.Close()
}

//printResult prints the final result to stdout.
func printResult(hash, nonce uint64) {
	fmt.Println("Result", hash, nonce)
}

//printDisconnected prints a disconnected message to stdout.
func printDisconnected() {
	fmt.Println("Disconnected from the server.")
        return
}
