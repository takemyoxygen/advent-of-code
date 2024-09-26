open! Base

let minmax xs ~compare =
  let min = List.min_elt xs ~compare in
  let max = List.max_elt xs ~compare in
  Option.both min max
