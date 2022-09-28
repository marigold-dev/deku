[%%let
("let.ok" : ('a, 'err) result -> ('a -> ('b, 'err) result) -> ('b, 'err) result)]

[%%let ("let.some" : 'a option -> ('a -> 'b option) -> 'b option)]
