open Prometheus
open Config

let network_received_message_size =
  let help = "Size of received message in bytes" in
  Gauge.v_label ~label_name:"route" ~help ~namespace ~subsystem
    "network_received_message_size"

let measure_network_received_message_size route size =
  Gauge.set (network_received_message_size route) (Float.of_int size)

let network_response_message_size =
  let help = "Size of response message in bytes" in
  Gauge.v ~help ~namespace ~subsystem "network_response_message_size"

let measure_network_response_message_size size =
  Gauge.set network_response_message_size (Float.of_int size)

let network_received_message_count =
  let help = "Count of total messages received" in
  Counter.v_label ~label_name:"route" ~help ~namespace ~subsystem
    "network_received_message_count"

let inc_network_messages_received route =
  Counter.inc_one (network_received_message_count route)

let network_response_message_count =
  let help = "Count of total response messages sent" in
  Counter.v_label ~label_name:"route" ~help ~namespace ~subsystem
    "network_response_message_count"

let inc_network_messages_response route =
  Counter.inc_one (network_response_message_count route)
