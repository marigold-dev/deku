open Deku_gameboy
open Deku_ledger
open Deku_crypto

type governance_mode = Anarchy | Democracy [@@deriving show, eq]

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
  [@@deriving show, eq]

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

  let%expect_test "Data_encoding round_trip" =
    let vote = Input Joypad.A in
    let serialized = Data_encoding.Binary.to_bytes_exn encoding vote in
    let deserialized = Data_encoding.Binary.of_bytes_exn encoding serialized in
    Format.printf "%a <-> %a\n" Hex.pp (Hex.of_bytes serialized) pp deserialized;
    [%expect
      {|
      010000000106 <-> (Game.Vote.Input Deku_gameboy.Joypad.A) |}]

  module Map = Deku_stdlib.Map.Make (struct
    type nonrec t = t

    let compare = compare
    let encoding = encoding
  end)
end

module Votes = struct
  type t = Vote.t Address.Map.t

  let encoding = Address.Map.encoding Vote.encoding
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
      [%expect {| (Some (Game.Vote.Input Deku_gameboy.Joypad.A)) |}]

    let%expect_test "voting: ties are broken" =
      let t =
        add_vote address_1 (Input A) empty |> add_vote address_2 (Input Start)
      in
      let outcome = get_majority t in
      Format.printf "%a" pp_outcome outcome;
      [%expect {| (Some (Game.Vote.Input Deku_gameboy.Joypad.Start)) |}]

    let%expect_test "voting: voting is idempotent" =
      let t =
        add_vote address_1 (Input Start) empty
        |> add_vote address_1 (Input Start)
        |> add_vote address_2 (Input A)
        |> add_vote address_3 (Input A)
      in
      let outcome = get_majority t in
      Format.printf "%a" pp_outcome outcome;
      [%expect {| (Some (Game.Vote.Input Deku_gameboy.Joypad.A)) |}]
  end
end

module Twitch_handle = struct
  type t = string [@@deriving eq, show]

  let of_string s =
    match String.length s <= 26 with
    | true -> Ok s
    | false -> Error "twitch handle two long"

  let compare = String.compare
  let encoding = Data_encoding.string
end

module Twitch_handle_map = Deku_stdlib.Map.Make (Twitch_handle)

type t = {
  twitch_oracle_address : Address.t;
  governance_mode : governance_mode;
  votes : Votes.t;
  address_to_twitch_attestations : Twitch_handle.t Address.Map.t;
  twitch_to_address_attestations : Address.t Twitch_handle_map.t;
  first_vote : Joypad.t option;
  prev_round_decision : Joypad.t option;
}

let encoding =
  let open Data_encoding in
  conv
    (fun {
           twitch_oracle_address;
           governance_mode;
           votes;
           address_to_twitch_attestations;
           twitch_to_address_attestations;
           first_vote;
           prev_round_decision;
         } ->
      ( twitch_oracle_address,
        governance_mode,
        votes,
        address_to_twitch_attestations,
        twitch_to_address_attestations,
        first_vote,
        prev_round_decision ))
    (fun ( twitch_oracle_address,
           governance_mode,
           votes,
           address_to_twitch_attestations,
           twitch_to_address_attestations,
           first_vote,
           prev_round_decision ) ->
      {
        twitch_oracle_address;
        governance_mode;
        votes;
        address_to_twitch_attestations;
        twitch_to_address_attestations;
        first_vote;
        prev_round_decision;
      })
    (tup7
       (dynamic_size Address.encoding)
       governance_mode_encoding Votes.encoding
       (Address.Map.encoding (dynamic_size Twitch_handle.encoding))
       (Twitch_handle_map.encoding (dynamic_size Address.encoding))
       (option Joypad.encoding) (option Joypad.encoding))

let empty ?twitch_oracle_address () =
  let twitch_oracle_address =
    match twitch_oracle_address with
    | Some address -> address
    | None ->
        Key_hash.of_b58 "tz1cTyRNTn3c83gKkrGXKtYWTeVfKaxxt8s5"
        |> Option.get |> Address.of_key_hash
  in
  {
    twitch_oracle_address;
    governance_mode = Democracy;
    votes = Votes.empty;
    address_to_twitch_attestations = Address.Map.empty;
    twitch_to_address_attestations = Twitch_handle_map.empty;
    first_vote = None;
    prev_round_decision = None;
  }

let attest_twitch_handle ~sender ~twitch_handle t =
  let {
    twitch_oracle_address;
    governance_mode;
    votes;
    address_to_twitch_attestations;
    twitch_to_address_attestations;
    first_vote;
    prev_round_decision;
  } =
    t
  in
  let address_to_twitch_attestations =
    Address.Map.add sender twitch_handle address_to_twitch_attestations
  in
  {
    twitch_oracle_address;
    governance_mode;
    votes;
    address_to_twitch_attestations;
    twitch_to_address_attestations;
    first_vote;
    prev_round_decision;
  }

let attest_deku_address ~sender ~deku_address ~twitch_handle t =
  let {
    twitch_oracle_address;
    governance_mode;
    votes;
    address_to_twitch_attestations;
    twitch_to_address_attestations;
    first_vote;
    prev_round_decision;
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
          twitch_oracle_address;
          governance_mode;
          votes;
          address_to_twitch_attestations;
          twitch_to_address_attestations;
          first_vote;
          prev_round_decision;
        }
  | _ -> None

let update_first_vote ~vote t =
  let { first_vote; _ } = t in
  match (vote, first_vote) with
  | Vote.Input input, None -> { t with first_vote = Some input }
  | _ -> t

let vote ~sender ~vote t =
  let {
    twitch_oracle_address;
    governance_mode;
    votes;
    address_to_twitch_attestations;
    twitch_to_address_attestations;
    first_vote;
    prev_round_decision;
  } =
    t
  in
  let votes = Votes.add_vote sender vote votes in
  let t =
    {
      twitch_oracle_address;
      governance_mode;
      votes;
      address_to_twitch_attestations;
      twitch_to_address_attestations;
      first_vote;
      prev_round_decision;
    }
  in
  update_first_vote ~vote t

let delegated_vote ~sender ~twitch_handle ~vote t =
  let {
    twitch_oracle_address;
    governance_mode;
    votes;
    address_to_twitch_attestations;
    twitch_to_address_attestations;
    first_vote;
    prev_round_decision;
  } =
    t
  in
  match Address.equal sender twitch_oracle_address with
  | true -> (
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
                  twitch_oracle_address;
                  governance_mode;
                  votes;
                  address_to_twitch_attestations;
                  twitch_to_address_attestations;
                  first_vote;
                  prev_round_decision;
                }
              in
              Some (update_first_vote ~vote t)
          | _ -> None)
      | None -> None)
  | false -> None

let new_round t =
  let {
    twitch_oracle_address = _;
    governance_mode;
    votes;
    address_to_twitch_attestations = _;
    twitch_to_address_attestations = _;
    first_vote;
    prev_round_decision = _;
  } =
    t
  in
  let majority = Votes.get_majority votes in
  let t = { t with prev_round_decision = None } in
  let t =
    match (governance_mode, majority, first_vote) with
    | Anarchy, Some (Governance Democracy), _ ->
        { t with governance_mode = Democracy }
    | Anarchy, _, Some input -> { t with prev_round_decision = Some input }
    | Anarchy, _, _ -> t
    | Democracy, Some (Governance Anarchy), _ ->
        { t with governance_mode = Anarchy }
    | Democracy, Some (Input input), _ ->
        { t with prev_round_decision = Some input }
    | Democracy, Some (Governance Democracy), _ | Democracy, None, _ -> t
  in
  { t with votes = Votes.empty; first_vote = None }

let get_decision t = t.prev_round_decision
