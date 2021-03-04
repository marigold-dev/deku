// open Protocol;
// open Helpers;
// open Cohttp_lwt;
// open Cohttp_lwt_unix;
// open Node_communication;
// module HTTP = Cohttp_lwt_unix.Client;
// let operation_request_to_body = req =>
//   req |> operation_request_to_yojson |> Yojson.Safe.to_string |> Body.of_string;
// let post_operation = (~uri, op_request) => {
//   let.await (response, _body) =
//     HTTP.post(
//       ~body=operation_request_to_body(op_request),
//       Uri.with_path(uri, "/operation"),
//     );
//   // TODO: what if connection errors
//   let status = Response.status(response);
//   await(status == `OK ? Ok() : Error(`Status_error(status)));
// };
// // POST /broadcast-transaction
// let broadcast = (state, endpoint) =>
//   state.Node_state.validators
//   |> filter_p_limited_concurrency(uri => {
//        let.await response = endpoint(~uri);
//        await(
//          switch (response) {
//          | Ok(v) => Some(v)
//          // TODO: log this error, should it be a noop?
//          | Error(_err) => None
//          },
//        );
//      });
// let broadcast_side_ops = (state, ~signature, operation) =>
//   broadcast(state, post_operation({signature, operation}))
//   |> Lwt.map(_ => ());
// let broadcast_block = (state, ~signature, operation) =>
//   broadcast(state, post_operation({signature, operation}))
//   |> Lwt.map(_ => ());
