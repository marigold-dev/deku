const add_tez : tez = 21mutez + 0.000_021tez

const sub_tez : tez = 21mutez - 20mutez

(* This is not enough. *)

const not_enough_tez : tez = 461_168_601_842_738_7903mutez

const nat_mul_tez : tez = 1n * 100mutez
const tez_mul_nat : tez = 100mutez * 10n

const tez_div_tez1 : nat = 100mutez / 1mutez
const tez_div_tez2 : nat = 100mutez / 90mutez
const tez_div_tez3 : nat = 100mutez / 110mutez

const tez_mod_tez1 : tez = 100mutez mod 1mutez
const tez_mod_tez2 : tez = 100mutez mod 90mutez
const tez_mod_tez3 : tez = 100mutez mod 110mutez
