open Crypto
open State

open Tendermint_helpers

open Tendermint_internals
(** Tendermint input_log and output_log.
    Holds all the querying function on the input_log required by Tendermint subprocesses. *)

module CI = Tendermint_internals

type round = CI.round

type height = CI.height

module MySet = Set.Make (struct
  type t = CI.value * CI.round

  let compare (v1, r1) (v2, r2) = compare (v1, r1) (v2, r2)
end)

(* FIXME: Tendermint we could do better *)
type node_identifier = Key_hash.t

type index_step = CI.consensus_step

type index = height * index_step
(** Tendermint input is indexed by height and consensus step. *)

type proposal_content = {
  process_round : round;  (** Round of the process that sent the PROPOSAL *)
  proposal : CI.value;
  process_valid_round : round;
      (** "Locked round" (see paper) of the PRECOMMIT of this value; -1 when the value has notbeen locked before *)
  sender : node_identifier;
}
(** Proposal messages carry more relevant information: the process_round, the
   proposed value, the last known valid round and, the sender *)

type prevote_content = {
  process_round : round;  (** Round of the process that sent the PREVOTE *)
  repr_value : CI.value;
  sender : node_identifier;
}
(** Prevote and Precommit messages only carry the process_round, a representation
   of the value being dealt with, and the sender. *)

type precommit_content = prevote_content

(* FIXME: we can do better than this now that we know the requirements:
    - find a way for "tendermint processes" to register to the input log
    - find a different way of handling clocks and timeouts? *)
type content =
  | ProposalContent  of proposal_content
  | PrevoteContent   of prevote_content
  | PrecommitContent of precommit_content
  | Timeout  (** Used to trigger actions on timeouts *)

(* TODO: ensure PrevoteOP and PrecommitOP contains repr_values and not values. *)
let content_of_op sender = function
  | CI.ProposalOP (_, process_round, proposal, process_valid_round) ->
    ProposalContent { process_round; proposal; process_valid_round; sender }
  | CI.PrevoteOP (_, process_round, repr_value) ->
    PrevoteContent { process_round; repr_value; sender }
  | CI.PrecommitOP (_, process_round, repr_value) ->
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
  type t = (CI.height, CI.value * CI.round) Hashtbl.t

  let empty () : t = Hashtbl.create 0

  let contains_nil t height = Hashtbl.find_opt t height |> Option.is_some |> not

  let set t height value round =
    assert (contains_nil t height);
    Hashtbl.add t height (value, round)

  let get t height = Hashtbl.find_opt t height

  let contains_block t height block =
    match Hashtbl.find_opt t height with
    | None -> false
    | Some (block', _) -> block = block'
end

type output_log = OutputLog.t

(************************************ Selection on input_log ************************************)

let on_proposal (f : proposal_content -> 'a option) = function
  | ProposalContent x -> f x
  | _ -> raise (Invalid_argument "Must be called on ProposalContent only")

let select_matching_step (msg_log : input_log) (i : index)
    (s : CI.consensus_step) (p : 'b -> 'a option) =
  match i with
  | _, step when step <> s -> raise (Invalid_argument "Bad step")
  | _h, _step ->
    let found =
      try Hashtbl.find msg_log.msg_log i |> map_option p with
      | Not_found -> [] in
    found

let select_matching_prevote (msg_log : input_log) (i : index)
    (p : content -> 'a option) =
  select_matching_step msg_log i CI.Prevote p
let select_matching_proposal (msg_log : input_log) (i : index)
    (p : content -> 'a option) =
  select_matching_step msg_log i CI.Proposal p

let select_matching_precommit (msg_log : input_log) (i : index)
    (p : content -> 'a option) =
  select_matching_step msg_log i CI.Precommit p

(** Selects (proposal_value, process_round) from Proposal data if the sender
   matches authorized proposer of current (height, round). *)
let select_proposal_process_round_matching_proposer (msg_log : input_log)
    (consensus_state : CI.consensus_state) (global_state : State.t) : MySet.t =
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

(** Selects (proposal_value, process_valid_round) from Proposal data if the
   sender matches authorized poposer of current (height, round). *)
let select_proposal_valid_round_matching_proposer (msg_log : input_log)
    (consensus_state : CI.consensus_state) (global_state : State.t) : MySet.t =
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

(** Selects (proposal_value, process_round) from Proposal data if the sender
   matches authorized proposer of current (height, round). *)
let select_proposal_matching_several_rounds (msg_log : input_log)
    (consensus_state : CI.consensus_state) (global_state : State.t) : MySet.t =
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
let compute_threshold global_state =
  let open Protocol.Validators in
  let validators = length global_state.protocol.validators |> float_of_int in
  (2. *. (validators -. 1.) /. 3.) +. 1.

(** Selects (repr_value, process_round) from Prevote data if the pair has enough
   cumulated weight.*)
let count_prevotes ?prevote_selection (msg_log : input_log)
    (consensus_state : CI.consensus_state) (global_state : State.t) : MySet.t =
  let open CI in
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
let count_precommits (msg_log : input_log)
    (consensus_state : CI.consensus_state) (global_state : State.t) =
  let open CI in
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

(* Selects (Value.nil, process_round) from Proposal, Prevote, and Precommit data
   if the pair has enough cumulated weight.
   We are not interested in getting a real value here, just checking the weight
   of all actions as this is failure detection. *)
let count_all_actions (msg_log : input_log)
    (consensus_state : CI.consensus_state) (global_state : State.t) =
  let open CI in
  let threshold = compute_threshold global_state in
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
