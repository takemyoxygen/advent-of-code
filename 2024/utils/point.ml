open Base
open Core

module T = struct
  type t = { x : int; y : int }
  [@@deriving compare, sexp_of, sexp, hash, fields]
end

include T

module Direction = struct
  let up = { x = 0; y = -1 }
  let down = { x = 0; y = 1 }
  let left = { x = -1; y = 0 }
  let right = { x = 1; y = 0 }
  let up_right = { x = 1; y = -1 }
  let down_right = { x = 1; y = 1 }
  let up_left = { x = -1; y = -1 }
  let down_left = { x = -1; y = 1 }
end

let zero = { x = 0; y = 0 }
let create x y = { x; y }

let move_n { x = x1; y = y1 } ~n ~direction:{ x = dir_x; y = dir_y } =
  { x = x1 + (n * dir_x); y = y1 + (n * dir_y) }

let move p1 p2 = move_n p1 ~n:1 ~direction:p2
let negate { x; y } = { x = -x; y = -y }
let sub p1 p2 = move p1 (negate p2)
let to_string { x; y } = sprintf "(%d, %d)" x y

let element_at grid point =
  let row =
    if point.y >= 0 && point.y < Array.length grid then Some grid.(point.y)
    else None
  in
  Option.bind row ~f:(fun row ->
      if point.x >= 0 && point.x < Array.length row then Some row.(point.x)
      else None)

include Comparable.Make (T)
include Core.Hashable.Make (T)
