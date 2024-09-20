open Base
open Core
open Utils
module P = Point

type segment = P.t * P.t

(*A line represented as x = a or y = ax + b *)
type line = Vertical of int | Other of int * int
type line_intersection = NoIntersection | Point of P.t | Overlap

let derive_line (p1, p2) =
  let x1, y1, x2, y2 = (P.x p1, P.y p1, P.x p2, P.y p2) in
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
      Point (P.create vx y)
  | Other (a1, b1), Other (a2, b2) when a1 = a2 && b1 = b2 -> Overlap
  | Other (a1, _), Other (a2, _) when a1 = a2 -> NoIntersection
  | Other (a1, b1), Other (a2, b2) ->
      let x = (b2 - b1) / (a1 - a2) in
      let y = (a1 * x) + b1 in
      if (a1 * x) + b1 = y && (a2 * x) + b2 = y then Point (P.create x y)
      else NoIntersection

let minmax a b = if a > b then (b, a) else (a, b)

let within_segment (st, fin) p =
  let x1, y1, x2, y2, x, y = (P.x st, P.y st, P.x fin, P.y fin, P.x p, P.y p) in
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
      let (l11, l12), (l21, l22) = (seg1, seg2) in
      let open P in
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

let segment_points (p1, p2) =
  let d =
    P.create (Int.compare (P.x p2) (P.x p1)) (Int.compare (P.y p2) (P.y p1))
  in
  Sequence.unfold ~init:(Some p1) ~f:(function
    | None -> None
    | Some p when P.equal p p2 -> Some (p, None)
    | Some p -> Some (p, Some (P.move p d)))

let incl set segment =
  segment_points segment |> Sequence.fold ~init:set ~f:Set.add

let solve segments =
  pairs segments
  |> Sequence.map ~f:(fun (a, b) -> segment_intersection a b)
  |> Sequence.filter_map ~f:Fn.id
  |> Sequence.fold ~init:(Set.empty (module Point)) ~f:incl
  |> Set.length

let solve filename =
  let input : segment list =
    Stdio.In_channel.read_lines filename
    |> List.map ~f:(fun line ->
           String.split_on_chars ~on:[ ','; ' '; '-'; '>' ] line
           |> List.filter ~f:(fun t -> not (String.is_empty t))
           |> List.map ~f:int_of_string)
    |> List.map ~f:(function
         | [ x1; y1; x2; y2 ] -> (P.create x1 y1, P.create x2 y2)
         | _ -> failwith "Invalid input format")
  in
  let part1 =
    input
    |> List.filter ~f:(fun (p1, p2) -> P.x p1 = P.x p2 || P.y p1 = P.y p2)
    |> solve
  in

  let part2 = solve input in
  (Some (string_of_int part1), Some (string_of_int part2))
