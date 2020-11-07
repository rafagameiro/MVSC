package main

import (
	"io"
	"log"
	"net"
	"time"
        "os"
)

func main() {
        port := os.Args[1]
        listener, err := net.Listen("tcp", "localhost:" + port)
	if err != nil {
		log.Fatal(err)
	}
	for {
		conn, err := listener.Accept()
		if err != nil {
			log.Print(err)
			continue
		}
		go handleConn(conn)
	}
}

func handleConn(conn net.Conn) {
	defer conn.Close()
	for {
		_, err := io.WriteString(conn, time.Now().Format(time.RFC1123)+"\n")
		if err != nil {
			return
		}
		time.Sleep(1 * time.Second)
	}
}
