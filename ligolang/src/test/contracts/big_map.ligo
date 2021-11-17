type parameter is unit
type storage is big_map (int, int) * unit
type return is list (operation) * storage

function main (const _ : parameter; var s : storage) : return is
  block {
    var _toto : option (int) := Some (0);
    _toto := s.0[23];
    s.0[2] := 444
  }
  with ((nil : list (operation)), s)

type foo is big_map (int, int)

function set_ (var n : int; var m : foo) : foo is block {
  m[23] := n
} with m

function add (var n : int ; var m : foo) : foo is set_ (n,m)

function rm (var m : foo) : foo is block {
  remove 42 from map m
} with m

function get (const m : foo) : option (int) is m[42]

const empty_big_map : big_map (int,int) = big_map []

const big_map1 : big_map (int,int) = big_map [23 -> 0; 42 -> 0]

function mutimaps (const m : foo; var n : foo) : foo is block {
  var bar : foo := m;
  bar[42] := 0;
  n[42] := get_force (42, bar)
} with n
