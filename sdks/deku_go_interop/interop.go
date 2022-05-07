package deku_go_interop

import (
	"encoding/binary"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"strings"
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

func log(message string) {
	// TODO: implement debug mode logging.
	if false {
		colored := fmt.Sprintf("\x1b[%dm%s\x1b[0m", 31, message)
		fmt.Fprintln(os.Stderr, colored)
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
	log(fmt.Sprintf("read %d bytes", bytes_read))
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

func Main(state_transition func(sender string, tx_hash string, input []byte) (err error)) {
	log("Opening read")
	fifo_path := os.Args[1]
	log(fmt.Sprintf("fifo path: %s", fifo_path))
	machine_to_chain, _ = os.OpenFile(fifo_path+"_read", os.O_WRONLY, 0666)
	log("Opening write")
	chain_to_machine, _ = os.OpenFile(fifo_path+"_write", os.O_RDONLY, 0666)
	log("done")
	for {
		// TODO: replace this with a control pipe
		control := read()
		if string(control) == "\"close\"" {
			break
		}
		sender := strings.Trim(string(read()), "\"")
		tx_hash := strings.Trim(string(read()), "\"")
		input := read()
		log(fmt.Sprintf("Read start message: %s", string(input)))
		err := state_transition(sender, tx_hash, input)
		var end_message []byte
		if err != nil {
			end_message = []byte(fmt.Sprintf("[\"Error\", \"%s\"]", err.Error()))

		} else {
			end_message = []byte("[\"Stop\"]")
		}
		write(end_message)
	}
}
