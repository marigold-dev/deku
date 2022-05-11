open Prometheus
open Config

let network_received_message_size =
  let help = "Size of received message in bytes" in
  Gauge.v ~help ~namespace ~subsystem "network_received_message_size"

let measure_network_received_message_size size =
  Gauge.set network_received_message_size (Float.of_int size)

let network_received_message_count =
  let help = "Count of total messages received" in
  Counter.v ~help ~namespace ~subsystem "network_received_message_count"

let inc_network_messages_received () =
  Counter.inc_one network_received_message_count
