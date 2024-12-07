open! Core

let index_of arr ~equal x =
  Array.findi arr ~f:(fun _ x' -> equal x' x) |> Option.map ~f:fst
