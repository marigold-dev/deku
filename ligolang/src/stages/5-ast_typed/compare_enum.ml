open Stage_common.Enums

let constant'_tag = constant'_to_enum

let constant' a b = Int.compare (constant'_tag a) (constant'_tag b)

let literal_tag = literal_to_enum

let literal a b = Int.compare (literal_tag a) (literal_tag b)
