open List

type usage =
| Drop
| Keep

val filter_keeps : usage list -> usage list

val select : usage list -> 'a1 list -> 'a1 list

type side =
| Left
| Right
| Both

type splitting = side list

val keep_right : usage -> side

val keep_rights : usage list -> side list

val left_usage : side -> usage

val right_usage : side -> usage

val left_usages : splitting -> usage list

val right_usages : splitting -> usage list

val split : splitting -> 'a1 list -> 'a1 list * 'a1 list

val assoc_splitting1 : splitting -> splitting -> splitting

val assoc_splitting2 : splitting -> splitting -> splitting

val assoc_splitting : splitting -> splitting -> splitting * splitting

val flip_side : side -> side

val flip_splitting : splitting -> splitting

val union : usage list -> usage list -> splitting * usage list
