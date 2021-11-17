let y (_ : unit) : nat =
  let _x : nat = 1n in
  begin
    (let _x : nat = 2n in unit) ;
    (let _x : nat = 23n in unit) ;
    (let _x : nat = 42n in unit) ;
    _x
  end
