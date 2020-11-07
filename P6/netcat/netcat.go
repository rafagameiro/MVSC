package main

import (
	"io"
	"log"
	"net"
	"os"
)

func main() {
        port := os.Args[1] 
        conn, err := net.Dial("tcp", "localhost:" + port)
	if err != nil {
		log.Fatal(err)
	}
	defer conn.Close()
	mustCopy(os.Stdout, conn)
}

func mustCopy(dst io.Writer, src io.Reader) {
	if _, err := io.Copy(dst, src); err != nil {
		log.Fatal(err)
	}
}
