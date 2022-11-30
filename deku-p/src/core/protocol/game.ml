open Deku_gameboy
open Deku_stdlib
open Deku_ledger
open Deku_crypto

type governance_mode = Anarchy | Democracy [@@deriving yojson, show, eq]

let governance_mode_encoding =
  let open Data_encoding in
  union ~tag_size:`Uint8
    [
      case ~title:"Anarchy" (Tag 0)
        (Data_encoding.constant "Anarchy")
        (function Anarchy -> Some () | _ -> None)
        (fun () -> Anarchy);
      case ~title:"Democracy" (Tag 1)
        (Data_encoding.constant "Democracy")
        (function Democracy -> Some () | _ -> None)
        (fun () -> Democracy);
    ]

module Vote = struct
  type t = Governance of governance_mode | Input of Joypad.t
  [@@deriving yojson, show, eq]

  let encoding =
    let open Data_encoding in
    union ~tag_size:`Uint8
      [
        case ~title:"Governance" (Tag 0)
          (Data_encoding.dynamic_size governance_mode_encoding)
          (function
            | Governance governance_mode -> Some governance_mode | _ -> None)
          (fun governance_mode -> Governance governance_mode);
        case ~title:"Input" (Tag 1)
          (Data_encoding.dynamic_size Joypad.encoding)
          (function Input input -> Some input | _ -> None)
          (fun input -> Input input);
      ]

  module Map = Deku_stdlib.Map.Make (struct
    type nonrec t = t

    let compare = compare
    let t_of_yojson = t_of_yojson
    let yojson_of_t = yojson_of_t
  end)
end

module Votes = struct
  type t = Vote.t Address.Map.t [@@deriving yojson]

  let empty = Address.Map.empty
  let add_vote voter vote t = Address.Map.add voter vote t

  (* TODO: tests for this function *)
  let get_majority t =
    let votes =
      Address.Map.fold
        (fun _address button votes ->
          Vote.Map.update button
            (function
              | Some votes_for_button -> Some (votes_for_button + 1)
              | None -> Some 1)
            votes)
        t Vote.Map.empty
    in
    Vote.Map.fold
      (fun button votes majority ->
        match majority with
        | Some (_majority_button, majority) when votes > majority ->
            Some (button, votes)
        | Some (majority_button, majority) -> Some (majority_button, majority)
        | None when votes > 0 -> Some (button, votes)
        | None -> None)
      votes None
    |> Option.map fst

  module Test = struct
    type outcome = Vote.t option [@@deriving show]

    open Joypad
    open Vote

    let address_1 =
      Key_hash.of_b58 "tz1Nm5D1MDerXaTMdc4kz2HWeVuJ6RHCF4WB"
      |> Option.get |> Address.of_key_hash

    let address_2 =
      Key_hash.of_b58 "tz1Uc11Ds3xwG4Hw1ragWLf3KLj4v4xu9BY2"
      |> Option.get |> Address.of_key_hash

    let address_3 =
      Key_hash.of_b58 "tz1hAGxu4tEDEoFQNwPFZf1U4gVrQcmstnnp"
      |> Option.get |> Address.of_key_hash

    let%expect_test "voting: no votes" =
      let t = empty in
      let outcome = get_majority t in
      Format.printf "%a" pp_outcome outcome;
      [%expect {| None |}]

    let%expect_test "voting: clear majority" =
      let t =
        add_vote address_1 (Input A) empty
        |> add_vote address_2 (Input A)
        |> add_vote address_3 (Input B)
      in
      let outcome = get_majority t in
      Format.printf "%a" pp_outcome outcome;
      [%expect {| (Some (Game.Vote.Input Joypad.A)) |}]

    let%expect_test "voting: ties are broken" =
      let t =
        add_vote address_1 (Input A) empty |> add_vote address_2 (Input Start)
      in
      let outcome = get_majority t in
      Format.printf "%a" pp_outcome outcome;
      [%expect {| (Some (Game.Vote.Input Joypad.Start)) |}]

    let%expect_test "voting: voting is idempotent" =
      let t =
        add_vote address_1 (Input Start) empty
        |> add_vote address_1 (Input Start)
        |> add_vote address_2 (Input A)
        |> add_vote address_3 (Input A)
      in
      let outcome = get_majority t in
      Format.printf "%a" pp_outcome outcome;
      [%expect {| (Some (Game.Vote.Input Joypad.A)) |}]
  end
end

let twitch_oracle_address =
  Key_hash.of_b58 "tz1gwS6Z98uXXXTKiMwn2Hccb7HUA2EMpCZk"
  |> Option.get |> Address.of_key_hash

module Twitch_handle = struct
  type t = string [@@deriving yojson, eq, show]

  let of_string s =
    match String.length s <= 26 with
    | true -> Ok s
    | false -> Error "twitch handle two long"

  let compare = String.compare
  let yojson_of_t s = `String s

  let t_of_yojson = function
    | `String s -> of_string s |> Result.get_ok
    | _ -> failwith "Cannot convert json to Twitch handle"
end

module Twitch_handle_map = Deku_stdlib.Map.Make (Twitch_handle)

type t = {
  governance_mode : governance_mode;
  votes : Votes.t;
  address_to_twitch_attestations : Twitch_handle.t Address.Map.t;
  twitch_to_address_attestations : Address.t Twitch_handle_map.t;
  first_vote : Joypad.t option;
}
[@@deriving yojson]

let empty =
  {
    governance_mode = Democracy;
    votes = Votes.empty;
    address_to_twitch_attestations = Address.Map.empty;
    twitch_to_address_attestations = Twitch_handle_map.empty;
    first_vote = None;
  }

let attest_twitch_handle ~sender ~twitch_handle t =
  let {
    governance_mode;
    votes;
    address_to_twitch_attestations;
    twitch_to_address_attestations;
    first_vote;
  } =
    t
  in
  let address_to_twitch_attestations =
    Address.Map.add sender twitch_handle address_to_twitch_attestations
  in
  {
    governance_mode;
    votes;
    address_to_twitch_attestations;
    twitch_to_address_attestations;
    first_vote;
  }

let attest_deku_address ~sender ~deku_address ~twitch_handle t =
  let {
    governance_mode;
    votes;
    address_to_twitch_attestations;
    twitch_to_address_attestations;
    first_vote;
  } =
    t
  in
  match Address.equal sender twitch_oracle_address with
  | true ->
      let twitch_to_address_attestations =
        Twitch_handle_map.add twitch_handle deku_address
          twitch_to_address_attestations
      in
      Some
        {
          governance_mode;
          votes;
          address_to_twitch_attestations;
          twitch_to_address_attestations;
          first_vote;
        }
  | _ -> None

let update_first_vote ~vote t =
  let { first_vote; _ } = t in
  match (vote, first_vote) with
  | Vote.Input input, None -> { t with first_vote = Some input }
  | _ -> t

let vote ~sender ~vote t =
  let {
    governance_mode;
    votes;
    address_to_twitch_attestations;
    twitch_to_address_attestations;
    first_vote;
  } =
    t
  in
  let votes = Votes.add_vote sender vote votes in
  let t =
    {
      governance_mode;
      votes;
      address_to_twitch_attestations;
      twitch_to_address_attestations;
      first_vote;
    }
  in
  update_first_vote ~vote t

let delegated_vote ~sender ~twitch_handle ~vote t =
  match Address.equal sender twitch_oracle_address with
  | true -> (
      let {
        governance_mode;
        votes;
        address_to_twitch_attestations;
        twitch_to_address_attestations;
        first_vote;
      } =
        t
      in
      match
        Twitch_handle_map.find_opt twitch_handle twitch_to_address_attestations
      with
      | Some voter_address -> (
          match
            Address.Map.find_opt voter_address address_to_twitch_attestations
          with
          | Some attested_handle when String.equal twitch_handle attested_handle
            ->
              let votes = Votes.add_vote voter_address vote votes in
              let t =
                {
                  governance_mode;
                  votes;
                  address_to_twitch_attestations;
                  twitch_to_address_attestations;
                  first_vote;
                }
              in
              Some (update_first_vote ~vote t)
          | _ -> None)
      | None -> None)
  | false -> None

let new_round t =
  let {
    governance_mode;
    votes = _;
    address_to_twitch_attestations;
    twitch_to_address_attestations;
    first_vote = _;
  } =
    t
  in
  {
    governance_mode;
    votes = Votes.empty;
    address_to_twitch_attestations;
    twitch_to_address_attestations;
    first_vote = None;
  }

let execute_decision t =
  let {
    governance_mode;
    votes;
    address_to_twitch_attestations = _;
    twitch_to_address_attestations = _;
    first_vote;
  } =
    t
  in
  let open Vote in
  let majority = Votes.get_majority votes in
  let input, t =
    match (governance_mode, majority, first_vote) with
    | Anarchy, Some (Governance Democracy), _ ->
        (None, { t with governance_mode = Democracy })
    | Anarchy, _, Some input -> (Some input, t)
    | Anarchy, _, _ -> (None, t)
    | Democracy, Some (Governance Anarchy), _ ->
        (None, { t with governance_mode = Anarchy })
    | Democracy, Some (Input input), _ -> (Some input, t)
    | Democracy, Some (Governance Democracy), _ | Democracy, None, _ -> (None, t)
  in
  (input, new_round t)
