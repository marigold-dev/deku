open Helpers;
open Protocol;

[@deriving (yojson, ord)]
type signature = {
  key: Address.t,
  signature: string,
};

module Signature_set =
  Set_with_yojson_make({
    type t = signature;
    let compare = compare_signature;
    let to_yojson = signature_to_yojson;
    let of_yojson = signature_of_yojson;
  });

// TODO: this is clearly multisig for dumb people
// TODO: please kill this file

[@deriving (yojson, ord)]
type t('a) = {
  signatures: Signature_set.t,
  data: 'a,
};

let data = t => t.data;
let signatures = t => t.signatures |> Signature_set.elements;

let of_signed = (Signed.{key, signature, data}) => {
  open Signature_set;
  let signature = {key, signature};
  {signatures: empty |> add(signature), data};
};
let sign = (~key, data) => {
  open Signature_set;
  let signature = {
    let Signed.{signature, _} = Signed.sign(~key, data);
    let key = Address.of_key(key);
    {key, signature};
  };
  {signatures: empty |> add(signature), data};
};
let append_sign = (~key, t) => {
  open Signature_set;
  let signature = {
    let Signed.{signature, _} = Signed.sign(~key, t.data);
    let key = Address.of_key(key);
    {key, signature};
  };
  {...t, signatures: t.signatures |> add(signature)};
};
let verify_signature = ({key, signature}, t) => {
  open Signature_set;
  let.ok _ = Signed.verify(~key, ~signature, t.data);
  let signature = {key, signature};
  Ok({...t, signatures: t.signatures |> add(signature)});
};
// TODO: should we ignore invalid signatures on a list of signatures? Maybe this should be a flag?
let of_yojson = (f, json) => {
  let.ok {signatures, data} = of_yojson(f, json);
  let t =
    signatures
    |> Signature_set.elements
    |> List.fold_left(
         (t, {key, signature}) =>
           switch (Signed.verify(~key, ~signature, data)) {
           | Ok(_) => {
               ...t,
               signatures:
                 t.signatures |> Signature_set.add({key, signature}),
             }
           | Error(_) => t
           },
         {signatures: Signature_set.empty, data},
       );
  t.signatures == Signature_set.empty
    ? Error("no valid signature present") : Ok(t);
};
