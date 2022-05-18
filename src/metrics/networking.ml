open Prometheus
open Config

(****************************************************************)
(* Metrics for messages received. Call in deku_node *)
let network_received_message_size =
  let help = "Size of received message in bytes" in
  Gauge.v_label ~label_name:"route" ~help ~namespace ~subsystem
    "network_received_message_size"

let measure_network_received_message_size route size =
  Gauge.set (network_received_message_size route) (Float.of_int size)

let network_received_message_count =
  let help = "Count of total message received" in
  Counter.v_label ~label_name:"route" ~help ~namespace ~subsystem
    "network_received_message_count"

let inc_network_messages_received route =
  Counter.inc_one (network_received_message_count route)

(****************************************************************)
(* Metrics for messages sent/response *)
let network_response_message_size =
  let help = "Size of respone message in bytes" in
  Gauge.v ~help ~namespace ~subsystem "network_respose_message_size"

let measure_network_response_message_size size =
  Gauge.set network_response_message_size (Float.of_int size)

let network_response_message_count =
  let help = "Count of total respond message sent" in
  Counter.v_label ~label_name:"route" ~help ~namespace ~subsystem
    "network_response_message_count"

let inc_network_messages_response route =
  Counter.inc_one (network_response_message_count route)

(****************************************************************)
(* Metrics for protocol snapshot. Call in deku_node *)
let network_snapshot_size =
  let help = "Size of snapshot in bytes" in
  Gauge.v ~help ~namespace ~subsystem "network_snapshot_ize"

let measure_network_snapshot_size size =
  Gauge.set network_snapshot_size (Float.of_int size)

let network_snapshot_count =
  let help = "Count of total snapshot" in
  Counter.v_label ~label_name:"route" ~help ~namespace ~subsystem
    "network_snapshot_count"

let inc_network_snapshot_size route =
  Counter.inc_one (network_snapshot_count route)
