type t = int * int * int [@@deriving sexp, hash, compare]

val distance : t -> t -> t
val to_abs : origin:t -> t -> t
val zero : t
val manhattan : t -> t -> int
