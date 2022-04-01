package deku_interop

import (
	"encoding/binary"
	"encoding/json"
	"fmt"
	"os"
)

var chain_to_machine *os.File
var machine_to_chain *os.File

type chain_message struct {
	Type    string
	Payload interface{}
}

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func read() []byte {
	// read the number of bytes being sent
	b := make([]byte, 8)
	_, err := chain_to_machine.Read(b)
	check(err)
	n := binary.LittleEndian.Uint16(b)
	value := make([]byte, n)
	bytes_read, err := chain_to_machine.Read(value)
	fmt.Printf("read %d bytes\n", bytes_read)
	return value
}

func write(message []byte) {
	message_length := make([]byte, 8)
	binary.LittleEndian.PutUint16(message_length, uint16(len(message)))
	machine_to_chain.Write(message_length)
	// send the message
	machine_to_chain.Write(message)
}

// Get(key) synchronously gets the bytes associated with the given
// key from the Deku state.
func Get(key string) []byte {
	message := []byte(fmt.Sprintf("[\"Get\", \"%s\"]", key))
	write(message)
	return read()
}

// Set(key,value) synchronously sets a key in the Deku state
// to the given value. That value must be serializable
// with json.Marshal.
func Set(key string, value interface{}) {
	b, err := json.Marshal(value)
	check(err)
	value_str := string(b)
	message := []byte(fmt.Sprintf("[\"Set\",{\"key\":\"%s\",\"value\":%s}]", key, value_str))
	write(message)
}

func Main(state_transition func(input []byte)) {
	println("Opening read")
	fifo_path := os.Args[1]
	fmt.Printf("fifo path: %s\n", fifo_path)
	machine_to_chain, _ = os.OpenFile(fifo_path+"_read", os.O_WRONLY, 0666)
	println("Opening write")
	chain_to_machine, _ = os.OpenFile(fifo_path+"_write", os.O_RDONLY, 0666)
	println("done")
	for {
		input := read()
		fmt.Printf("Read start message: %s\n", string(input))
		state_transition(input)
		end_message := []byte("[\"Stop\"]")
		write(end_message)
	}
}
