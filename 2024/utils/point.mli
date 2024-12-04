open Base

type t

module Direction : sig
  val up : t
  val down : t
  val left : t
  val right : t
end

val x : t -> int
val y : t -> int
val zero : t
val create : int -> int -> t
val move : t -> t -> t
val element_at : 'a array array -> t -> 'a option

include Comparable.S with type t := t
include Hashable.Key with type t := t
