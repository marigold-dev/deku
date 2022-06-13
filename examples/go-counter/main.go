package main

import (
	"encoding/json"
	"errors"
	"fmt"
	"os"
	
	"github.com/marigold-dev/deku/sdks/deku_go_interop"
)

type Action int

const (
	Decrement Action = iota
	Increment 
)

type message struct {
	Action Action
}

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func log(message string) {
	colored := fmt.Sprintf("\x1b[%dm%s\x1b[0m", 31, message)
	fmt.Fprintln(os.Stderr, colored)
}

func main() {
	state_transition := func(transaction deku_go_interop.Transaction) (return_err error) {
		var message message
		log(fmt.Sprintf(
				"State transition received. Sender: %s, Tx hash: %s, Op hash: %s, Payload: %s,",
				transaction.Source,
				transaction.Tx_hash,
				transaction.Op_hash,
				string(transaction.Operation)))
		err := json.Unmarshal(transaction.Operation, &message)
		check(err)
		counter_bytes := deku_go_interop.Get("counter")
		var counter *int
		json.Unmarshal(counter_bytes, &counter)
		var new_counter int
		switch message.Action {
		case Increment:
			new_counter = *counter + 1
			deku_go_interop.Set("counter", new_counter)
			log(fmt.Sprintf("Incremented counter %d", new_counter))
		case Decrement:
			if *counter > 0 {
				new_counter = *counter - 1
				deku_go_interop.Set("counter", new_counter)
				log(fmt.Sprintf("Decremented counter %d", new_counter))
			} else {
				log("Skipping counter increase")
			}
		default:
			// Signal an invalid transaction by returning an error
			return errors.New(fmt.Sprintf("Unrecognized action: %s", message.Action))
		}
		// Valid transactions produce nil results.
		return
	}
	var initial_state = make(map[string]interface{})
	initial_state["counter"] = 4;
	deku_go_interop.Main(initial_state, state_transition)
}
