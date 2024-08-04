open Base

type point = { x : int; y : int } [@@deriving compare, sexp_of, sexp, hash]

module Point = struct
  module T = struct
    type t = point [@@deriving compare, sexp_of, sexp, hash]
  end

  include T

  module Direction = struct
    let up = { x = 0; y = -1 }
    let down = { x = 0; y = 1 }
    let left = { x = -1; y = 0 }
    let right = { x = 1; y = 0 }
  end

  let create x y = { x; y }
  let x { x; _ } = x
  let y { y; _ } = y
  let move { x = x1; y = y1 } { x = x2; y = y2 } = { x = x1 + x2; y = y1 + y2 }

  include Comparable.Make (T)
  include Core.Hashable.Make (T)
end
