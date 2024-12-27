open Base

type t = {x: int; y: int}

module Direction : sig
  val up : t
  val down : t
  val left : t
  val right : t
  val up_right: t
  val down_right: t
  val up_left : t
  val down_left : t
end

val x : t -> int
val y : t -> int
val zero : t
val create : int -> int -> t
val move : t -> t -> t
val move_n: t -> n:int -> direction:t -> t
val negate : t -> t
val sub: t -> t -> t
val element_at : 'a array array -> t -> 'a option
val to_string : t -> string

val hash_fold_t : Hash.state -> t -> Hash.state

include Comparable.S with type t := t
include Hashable.Key with type t := t
