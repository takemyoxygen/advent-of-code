open Base

type t = int * int * int [@@deriving sexp, hash, compare]

let distance (x1, y1, z1) (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)
let to_abs ~origin:(x, y, z) (dx, dy, dz) = (x + dx, y + dy, z + dz)
let zero = (0, 0, 0)

let manhattan (x1, y1, z1) (x2, y2, z2) =
  abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)
