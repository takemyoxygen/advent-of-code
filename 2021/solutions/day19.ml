open Base
open Core
open Utils

module Vec3 = struct
  type t = int * int * int [@@deriving sexp, hash, compare]

  let distance (x1, y1, z1) (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)
  let to_abs ~origin:(x, y, z) (dx, dy, dz) = (x + dx, y + dy, z + dz)
  let zero = (0, 0, 0)

  let manhattan (x1, y1, z1) (x2, y2, z2) =
    abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)
end

module Orientation = struct
  type axis = X | Y | Z [@@deriving compare]
  type dir = Pos | Neg [@@deriving compare]
  type axis_dir = dir * axis [@@deriving compare]

  type t = { right : axis_dir; up : axis_dir; fwd : axis_dir }
  [@@deriving compare]

  let axis_dir_of_string s =
    let dir = if String.is_prefix s ~prefix:"-" then Neg else Pos in
    let axis =
      match String.chop_prefix_if_exists s ~prefix:"-" with
      | "x" -> X
      | "y" -> Y
      | "z" -> Z
      | _ -> assert false
    in
    (dir, axis)

  let neg_axis (dir, axis) =
    let neg_dir = match dir with Pos -> Neg | Neg -> Pos in
    (neg_dir, axis)

  let create ~up ~right ~fwd = { up; right; fwd }
  let canonical = { right = (Pos, X); up = (Pos, Y); fwd = (Pos, Z) }

  let to_canonical { right; up; fwd } (x, y, z) =
    let coords =
      List.map2_exn [ right; up; fwd ] [ x; y; z ] ~f:(fun (dir, axis) coord ->
          let coord' = match dir with Pos -> coord | Neg -> -coord in
          (axis, coord'))
    in
    let find axis =
      List.Assoc.find_exn coords
        ~equal:(fun a1 a2 -> compare_axis a1 a2 = 0)
        axis
    in
    (find X, find Y, find Z)

  let convert ~src ~target point =
    let x, y, z = to_canonical src point in
    let by_axis = function X -> x | Y -> y | Z -> z in
    let by_dir_axis (dir, axis) =
      by_axis axis * match dir with Pos -> 1 | Neg -> -1
    in
    (by_dir_axis target.right, by_dir_axis target.up, by_dir_axis target.fwd)

  let variations ({ up; right; fwd } as canonical : t) =
    [
      canonical;
      { up = right; right = neg_axis up; fwd };
      { up = neg_axis right; right = up; fwd };
      { up = neg_axis up; right = neg_axis right; fwd };
    ]

  let all =
    let variants =
      [
        ("y", "x", "z");
        ("y", "z", "-x");
        ("y", "-x", "-z");
        ("y", "-z", "x");
        ("z", "x", "-y");
        ("-z", "x", "y");
      ]
      |> List.map ~f:(fun (up, right, fwd) ->
             create ~up:(axis_dir_of_string up)
               ~right:(axis_dir_of_string right) ~fwd:(axis_dir_of_string fwd))
      |> List.bind ~f:variations
      |> List.dedup_and_sort ~compare
    in
    variants
end

module Normalized_scanner = struct
  type t = { id : int; origin : Vec3.t; points : Vec3.t list }
  [@@deriving sexp_of]

  let hash x = x.id
  let compare x1 x2 = Int.compare x1.id x2.id
end

type projection = { origin : Vec3.t; points : Vec3.t list }

let read_input filename =
  Stdio.In_channel.read_all filename
  |> Str.split (Str.regexp "\n\n")
  |> List.mapi ~f:(fun idx scaner_text ->
         let scanner_lines = String.split_lines scaner_text in
         let points =
           List.drop scanner_lines 1
           |> List.map ~f:(fun line ->
                  let parts =
                    String.split ~on:',' line |> List.map ~f:Int.of_string
                  in
                  match parts with
                  | [ x; y; z ] -> (x, y, z)
                  | _ -> assert false)
         in
         (idx, points))

let projection_overlap orientation projection1 projection2 =
  let scan2_set = Hash_set.of_list (module Vec3) projection2.points in
  projection1.points
  |> List.filter ~f:(fun point ->
         Orientation.convert ~src:Orientation.canonical ~target:orientation
           point
         |> Hash_set.mem scan2_set)

let make_view_projection points origin =
  { origin; points = List.map points ~f:(fun p -> Vec3.distance p origin) }

let make_all_view_projections points =
  List.map points ~f:(make_view_projection points)

let scanner_overlap scanner1 scanner2 =
  let projections1 = make_all_view_projections scanner1
  and projections2 = make_all_view_projections scanner2 in
  List.cartesian_product projections1 projections2
  |> Sequence.of_list
  |> Sequence.bind ~f:(fun (p1, p2) ->
         Orientation.all |> Sequence.of_list
         |> Sequence.filter_map ~f:(fun orient ->
                match projection_overlap orient p1 p2 with
                | xs when List.length xs < 12 -> None
                | xs ->
                    let p1' = { p1 with points = xs } in
                    let p2' = { p2 with points = xs } in
                    Some (orient, p1', p2')))
  |> Sequence.hd

let normalize_origin orientation ~normalized ~to_normalize =
  let origin2 =
    Orientation.convert ~src:orientation ~target:Orientation.canonical
      to_normalize.origin
  in
  Vec3.distance normalized.origin origin2

let normalize_if_overlaps (normalized : Normalized_scanner.t)
    ((id, scanner) : int * Vec3.t list) =
  match scanner_overlap normalized.points scanner with
  | None -> None
  | Some (orientation, projection1, projection2) ->
      let origin =
        normalize_origin orientation ~normalized:projection1
          ~to_normalize:projection2
      in
      let oriented_origin =
        Orientation.convert ~src:Orientation.canonical ~target:orientation
          origin
      in
      let points =
        scanner
        |> List.map ~f:(Vec3.to_abs ~origin:oriented_origin)
        |> List.map
             ~f:
               (Orientation.convert ~src:orientation
                  ~target:Orientation.canonical)
      in
      Some Normalized_scanner.{ id; origin; points }

let normalize numbered_scanners =
  let start =
    List.hd_exn numbered_scanners |> fun (id, points) ->
    Normalized_scanner.{ id; origin = Vec3.zero; points }
  in
  Graph.bfs
    (module Normalized_scanner)
    ~start
    ~adjacent:(fun norm visited ->
      numbered_scanners
      |> List.filter ~f:(fun (id, _) ->
             id <> norm.id
             && Hash_set.for_all visited ~f:(fun vis -> vis.id <> id))
      |> List.filter_map ~f:(normalize_if_overlaps norm))

let solve filename =
  let numbered_scanners = read_input filename in
  let normalized = normalize numbered_scanners in
  let part1 =
    List.fold normalized
      ~init:(Hash_set.create (module Vec3))
      ~f:(fun set norm ->
        Hash_set.of_list (module Vec3) norm.points |> Hash_set.union set)
    |> Hash_set.length
  in
  let part2 =
    List.cartesian_product normalized normalized
    |> List.map ~f:(fun (n1, n2) -> Vec3.manhattan n1.origin n2.origin)
    |> List.max_elt ~compare:Int.compare
    |> Option.value_exn
  in
  (Some (Int.to_string part1), Some (Int.to_string part2))
