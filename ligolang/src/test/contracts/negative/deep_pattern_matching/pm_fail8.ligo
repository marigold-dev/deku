type myt is Nil | Cons of (int * int)

function t (const x: myt ; const y: myt) is
  case x of
  | Nil -> (
    case y of
    | Nil -> 1
    | Cons (a,b) ->
      block {
        const a = "a" ;
      } with int(String.length (a)) + b
    end
  )
  | Cons (a,b) ->
    block {
      const old_b = b ;
      const b =
        case y of
        | Nil ->
          block {
            const f = function (const b:int) is b + a ;
          } with f (b+1)
        | Cons (a,b) -> "invalid"
        end ;
    } with a + old_b + b
    end