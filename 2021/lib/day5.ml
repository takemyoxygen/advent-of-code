open Base
open Core

type point = int * int
type segment = point * point

(*A line represented as x = a or y = ax + b *)
type line = Vertical of int | Other of int * int
type line_intersection = NoIntersection | Point of point | Overlap

module Point = struct
  type t = point

  include Tuple.Comparable (Int) (Int)
end

let input : segment list =
  Stdio.In_channel.read_lines "./input/day5.txt"
  |> List.map ~f:(fun line ->
         String.split_on_chars ~on:[ ','; ' '; '-'; '>' ] line
         |> List.filter ~f:(fun t -> not (String.is_empty t))
         |> List.map ~f:int_of_string)
  |> List.map ~f:(function
       | [ x1; y1; x2; y2 ] -> ((x1, y1), (x2, y2))
       | _ -> failwith "Invalid input format")

let derive_line ((x1, y1), (x2, y2)) =
  if x1 = x2 then Vertical x1
  else
    let a = (y1 - y2) / (x1 - x2) in
    let b = y1 - (a * x1) in
    Other (a, b)

let line_intersection l1 l2 =
  match (l1, l2) with
  | Vertical x1, Vertical x2 -> if x1 = x2 then Overlap else NoIntersection
  | Vertical vx, Other (a, b) | Other (a, b), Vertical vx ->
      let y = (a * vx) + b in
      Point (vx, y)
  | Other (a1, b1), Other (a2, b2) when a1 = a2 && b1 = b2 -> Overlap
  | Other (a1, _), Other (a2, _) when a1 = a2 -> NoIntersection
  | Other (a1, b1), Other (a2, b2) ->
      let x = (b2 - b1) / (a1 - a2) in
      let y = (a1 * x) + b1 in
      if (a1 * x) + b1 = y && (a2 * x) + b2 = y then Point (x, y)
      else NoIntersection

let minmax a b = if a > b then (b, a) else (a, b)

let within_segment ((x1, y1), (x2, y2)) (x, y) =
  let x1, x2 = minmax x1 x2 in
  let y1, y2 = minmax y1 y2 in
  x1 <= x && x <= x2 && y1 <= y && y <= y2

let segment_intersection (seg1 : segment) (seg2 : segment) =
  match line_intersection (derive_line seg1) (derive_line seg2) with
  | NoIntersection -> None
  | Point p ->
      if within_segment seg1 p && within_segment seg2 p then Some (p, p)
      else None
  | Overlap ->
      let open Point in
      let (l11, l12), (l21, l22) = (seg1, seg2) in
      if max l21 l22 < min l11 l12 || min l21 l22 > max l11 l12 then None
      else
        let sorted =
          Array.sorted_copy [| l11; l12; l21; l22 |] ~compare:Point.compare
        in
        Some (sorted.(1), sorted.(2))

let pairs items =
  let open Sequence.Generator in
  let rec loop = function
    | hd :: tail ->
        let gen =
          tail |> List.map ~f:(fun another -> yield (hd, another)) |> all_unit
        in
        gen >>= fun () -> loop tail
    | _ -> return ()
  in
  loop items |> run

let segment_points ((x1, y1), (x2, y2)) =
  let dx, dy = (Int.compare x2 x1, Int.compare y2 y1) in
  let move (x, y) = (x + dx, y + dy) in
  Sequence.unfold
    ~init:(Some (x1, y1))
    ~f:(function
      | None -> None
      | Some p when Point.equal p (x2, y2) -> Some (p, None)
      | Some p -> Some (p, Some (move p)))

let incl set segment =
  segment_points segment |> Sequence.fold ~init:set ~f:Set.add

let solve segments =
  pairs segments
  |> Sequence.map ~f:(fun (a, b) -> segment_intersection a b)
  |> Sequence.filter_map ~f:Fn.id
  |> Sequence.fold ~init:(Set.empty (module Point)) ~f:incl
  |> Set.length

let part1 =
  input
  |> List.filter ~f:(fun ((x1, y1), (x2, y2)) -> x1 = x2 || y1 = y2)
  |> solve

let part2 = solve input
