open Base

module Point : sig
  type t

  module Direction : sig
    val up : t
    val down : t
    val left : t
    val right : t
  end

  val x : t -> int
  val y : t -> int
  val create : int -> int -> t
  val move : t -> t -> t

  include Comparable.S with type t := t
  include Hashable.Key with type t := t
end
