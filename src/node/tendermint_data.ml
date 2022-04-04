open Crypto
open State
open Tendermint_helpers
open Tendermint_internals

type elmt = value * round

module MySet = Set.Make (struct
  type t = elmt

  let compare (v1, r1) (v2, r2) = compare (v1, r1) (v2, r2)
end)

type node_identifier = Key_hash.t

type index_step = Tendermint_internals.consensus_step

type index = height * index_step

type proposal_content = {
  process_round : round;
  proposal : value;
  process_valid_round : round;
  sender : node_identifier;
}

type prevote_content = {
  process_round : round;
  repr_value : value;
  sender : node_identifier;
}

type precommit_content = prevote_content

type content =
  | ProposalContent  of proposal_content
  | PrevoteContent   of prevote_content
  | PrecommitContent of precommit_content
  | Timeout

(* TODO: ensure PrevoteOP and PrecommitOP contains repr_values and not values. *)
let content_of_op sender = function
  | Tendermint_internals.ProposalOP
      (_, process_round, proposal, process_valid_round) ->
    ProposalContent { process_round; proposal; process_valid_round; sender }
  | Tendermint_internals.PrevoteOP (_, process_round, repr_value) ->
    PrevoteContent { process_round; repr_value; sender }
  | Tendermint_internals.PrecommitOP (_, process_round, repr_value) ->
    PrecommitContent { process_round; repr_value; sender }

(* TODO: ensure there is no data duplication in input_log *)
type input_log = {
  msg_log : (index, content list) Hashtbl.t;
  timeouts : (index, content) Hashtbl.t;
      (* Making timeouts a separate queue because it's easier at the moment... *)
}
let empty () : input_log =
  { msg_log = Hashtbl.create 0; timeouts = Hashtbl.create 0 }

let add (input_log : input_log) (index : index) (c : content) =
  match c with
  | Timeout ->
    Hashtbl.replace input_log.timeouts index c;
    input_log
  | c -> (
    let previous =
      match Hashtbl.find_opt input_log.msg_log index with
      | Some ls -> ls
      | None -> [] in
    match List.find_opt (fun elem -> elem = c) previous with
    | Some _ -> input_log (* Do not add same content twice *)
    | None ->
      Hashtbl.replace input_log.msg_log index (c :: previous);
      input_log)

let remove (input_log : input_log) (index : index) =
  Hashtbl.remove input_log.msg_log index;
  Hashtbl.remove input_log.timeouts index

let prune (input_log : input_log) (height : height) =
  let _ =
    [Proposal; Prevote; Precommit]
    (* Should we make sure all h < height are emptied here? *)
    |> List.map (fun step -> (height, step))
    |> List.map (fun index -> remove input_log index) in
  ()

let map_option f ls =
  let rec aux = function
    | [] -> []
    | x :: xs ->
    match f x with
    | Some y -> y :: aux xs
    | None -> aux xs in
  aux ls

let contains_timeout input_log height step =
  match Hashtbl.find_opt input_log.timeouts (height, step) with
  | None -> false
  | Some _ ->
    Hashtbl.remove input_log.timeouts (height, step);
    true

(** Tendermint's output_log AKA decision log*)
module OutputLog = struct
  type t = (height, value * round) Hashtbl.t

  let empty () : t = Hashtbl.create 0

  let contains_nil (table : t) (height : height) =
    Hashtbl.find_opt table height |> Option.is_some |> not

  let set t height value round =
    assert (contains_nil t height);
    Hashtbl.add t height (value, round)

  let get (table : t) (height : height) = Hashtbl.find_opt table height

  let contains_block (t : t) height block =
    match Hashtbl.find_opt t height with
    | None -> false
    | Some (block', _) -> block = block'
end

(************************************ Selection on input_log ************************************)

let on_proposal (f : proposal_content -> 'a option) = function
  | ProposalContent x -> f x
  | _ -> raise (Invalid_argument "Must be called on ProposalContent only")

let select_matching_step (msg_log : input_log) (i : index) (s : consensus_step)
    (p : 'b -> 'a option) =
  match i with
  | _, step when step <> s -> raise (Invalid_argument "Bad step")
  | _h, _step ->
    let found =
      try Hashtbl.find msg_log.msg_log i |> map_option p with
      | Not_found -> [] in
    found

let select_matching_prevote (msg_log : input_log) (i : index)
    (p : content -> 'a option) =
  select_matching_step msg_log i Prevote p
let select_matching_proposal (msg_log : input_log) (i : index)
    (p : content -> 'a option) =
  select_matching_step msg_log i Proposal p

let select_matching_precommit (msg_log : input_log) (i : index)
    (p : content -> 'a option) =
  select_matching_step msg_log i Precommit p

let select_proposal_process_round_matching_proposer (msg_log : input_log)
    (consensus_state : consensus_state) (global_state : State.t) =
  let index = (consensus_state.height, Proposal) in
  let selected =
    select_matching_proposal msg_log index
      (on_proposal (fun c ->
           if
             is_allowed_proposer global_state consensus_state.height
               consensus_state.round c.sender
           then
             Some (c.proposal, c.process_round)
           else
             None)) in
  MySet.of_list selected

let select_proposal_valid_round_matching_proposer (msg_log : input_log)
    (consensus_state : consensus_state) (global_state : State.t) : MySet.t =
  let index = (consensus_state.height, Proposal) in
  let selected =
    select_matching_proposal msg_log index
      (on_proposal (fun c ->
           if
             is_allowed_proposer global_state consensus_state.height
               consensus_state.round c.sender
           then
             Some (c.proposal, c.process_valid_round)
           else
             None)) in
  MySet.of_list selected

let select_proposal_matching_several_rounds (msg_log : input_log)
    (consensus_state : consensus_state) (global_state : State.t) : MySet.t =
  let index = (consensus_state.height, Proposal) in
  let selected =
    select_matching_proposal msg_log index
      (on_proposal (fun c ->
           if
             is_allowed_proposer global_state consensus_state.height
               consensus_state.round c.sender
           then
             Some (c.proposal, c.process_round)
           else
             None)) in
  MySet.of_list selected

(** Helper function to compute the required weight threshold *)
let compute_threshold ?(f = 2.) global_state =
  let open Protocol.Validators in
  let validators = length global_state.protocol.validators |> float_of_int in
  (f *. (validators -. 1.) /. 3.) +. 1.

let count_prevotes ?prevote_selection (msg_log : input_log)
    (consensus_state : consensus_state) (global_state : State.t) : MySet.t =
  let prevote_selection =
    match prevote_selection with
    | None -> (
      function
      | PrevoteContent content
        when content.process_round = consensus_state.round ->
        Some
          ( (content.repr_value, content.process_round),
            get_weight global_state content.sender )
      | PrevoteContent _ -> None
      | _ -> failwith "This should never happen, it's prevotes")
    | Some p -> p in
  let threshold = compute_threshold global_state in
  let index = (consensus_state.height, Prevote) in
  let all_prevotes = select_matching_prevote msg_log index prevote_selection in
  let filtered = Counter.filter_threshold all_prevotes ~threshold in
  MySet.of_list filtered

(** Selects (repr_value, process_round) from Precommit data if the pair has
   enough cumulated weight. *)
let count_precommits (msg_log : input_log) (consensus_state : consensus_state)
    (global_state : State.t) =
  let threshold = compute_threshold global_state in
  let index = (consensus_state.height, Precommit) in
  let all_precommits =
    select_matching_precommit msg_log index (fun x -> Option.some @@ x) in
  let precommits_with_weights =
    List.map
      (function
        | PrecommitContent content ->
          ( (content.repr_value, content.process_round),
            get_weight global_state content.sender )
        | _ -> failwith "This should never happen, it's precommits")
      all_precommits in
  let filtered = Counter.filter_threshold precommits_with_weights ~threshold in
  MySet.of_list filtered

let count_all_actions ?(threshold_f = 2.) (msg_log : input_log)
    (consensus_state : consensus_state) (global_state : State.t) =
  let threshold = compute_threshold ~f:threshold_f global_state in
  let index_proposal = (consensus_state.height, Proposal) in
  let index_prevote = (consensus_state.height, Prevote) in
  let index_precommit = (consensus_state.height, Precommit) in
  let proposals =
    select_matching_proposal msg_log index_proposal (fun x -> Option.some x)
  in
  let prevotes =
    select_matching_prevote msg_log index_prevote (fun x -> Option.some x) in
  let precommits =
    select_matching_precommit msg_log index_precommit (fun x -> Option.some x)
  in
  let proposals_with_weights =
    List.map
      (function
        | ProposalContent content ->
          ((nil, content.process_round), get_weight global_state content.sender)
        | _ -> failwith "This should never happen, it's proposals")
      proposals in
  let prevotes_with_weights =
    List.map
      (function
        | PrevoteContent content ->
          ((nil, content.process_round), get_weight global_state content.sender)
        | _ -> failwith "This should never happen, it's prevotes")
      prevotes in
  let precommits_with_weights =
    List.map
      (function
        | PrecommitContent content ->
          ((nil, content.process_round), get_weight global_state content.sender)
        | _ -> failwith "This should never happen, it's precommits")
      precommits in
  let filtered =
    Counter.filter_threshold
      (List.concat
         [
           proposals_with_weights; prevotes_with_weights; precommits_with_weights;
         ])
      ~threshold in
  MySet.of_list filtered
