open Prometheus
open Prometheus_reporter

module Unix_runtime = struct
  let start_time = Unix.gettimeofday ()

  let simple_metric ~metric_type ~help name fn =
    let info =
      {
        MetricInfo.name = MetricName.v name;
        help;
        metric_type;
        label_names = [];
      }
    in
    let collect () = LabelSetMap.singleton [] [ Sample_set.sample (fn ()) ] in
    (info, collect)

  let process_start_time_seconds =
    simple_metric ~metric_type:Counter "process_start_time_seconds"
      (fun () -> start_time)
      ~help:"Start time of the process since unix epoch in seconds."

  let metrics = [ process_start_time_seconds ]
end

let () =
  let add (info, collector) =
    CollectorRegistry.(register default) info collector
  in
  List.iter add Unix_runtime.metrics

let handler (_request : Piaf.Request.t) =
  let data = Prometheus.CollectorRegistry.(collect default) in
  let body =
    Fmt.to_to_string TextFormat_0_0_4.output data |> Piaf.Body.of_string
  in
  let headers =
    Piaf.Headers.of_list [ ("Content-Type", "text/plain; version=0.0.4") ]
  in
  Piaf.Response.create ~headers ~body `OK

let path () = Routes.((s "metrics" /? nil) @--> handler)
