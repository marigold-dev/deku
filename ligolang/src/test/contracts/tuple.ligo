type abc is int * int * int

function projection_abc (const tpl : abc) : int is tpl.1

function modify_abc (var tpl : abc) : abc is
  block {
    tpl.1 := 2048
  } with tpl

type foobar is int * int

const fb : foobar = (0,0)

function projection (const tpl : foobar) : int is tpl.0 + tpl.1

type big_tuple is int * int * int * int * int * int * int * int * int * int * int * int

const br : big_tuple = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)

function update (var tpl : big_tuple) : big_tuple is
  block {
    tpl.11 := 2048
  } with tpl
