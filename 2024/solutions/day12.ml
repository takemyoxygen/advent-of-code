open! Core
open Utils

let parse str =
  str |> String.strip |> String.split_lines
  |> List.map ~f:String.to_array
  |> Array.of_list

let directions =
  [
    Point.Direction.up;
    Point.Direction.down;
    Point.Direction.left;
    Point.Direction.right;
  ]

let scan_region grid start =
  let symbol = Point.element_at grid start |> Option.value_exn in
  Graph.bfs
    (module Point)
    ~start
    ~adjacent:(fun pos _ ->
      directions
      |> List.map ~f:(Point.move pos)
      |> List.filter ~f:(fun adj ->
             match Point.element_at grid adj with
             | Some s when Char.equal s symbol -> true
             | _ -> false))
  |> Set.of_list (module Point)

let scan_all_regions grid =
  let rec loop acc remaining =
    match Set.min_elt remaining with
    | None -> acc
    | Some next ->
        let region = scan_region grid next in
        let remaining' = Set.diff remaining region in
        loop (region :: acc) remaining'
  in
  Array.concat_mapi grid ~f:(fun y ->
      Array.mapi ~f:(fun x _ -> Point.create x y))
  |> Set.of_array (module Point)
  |> loop []

let perimeter grid region =
  let symbol =
    region |> Set.min_elt_exn |> Point.element_at grid |> Option.value_exn
  in
  region |> Set.to_list
  |> List.sum
       (module Int)
       ~f:(fun pos ->
         directions
         |> List.count ~f:(fun dir ->
                match Point.element_at grid (Point.move pos dir) with
                | Some s when Char.(s <> symbol) -> true
                | None -> true
                | _ -> false))

let part1 grid =
  scan_all_regions grid
  |> List.sum (module Int) ~f:(fun reg -> perimeter grid reg * Set.length reg)

let corners =
  [
    [ Point.Direction.up; Point.Direction.right ];
    [ Point.Direction.right; Point.Direction.down ];
    [ Point.Direction.down; Point.Direction.left ];
    [ Point.Direction.left; Point.Direction.up ];
  ]

let count_corners grid region =
  let symbol =
    region |> Set.min_elt_exn |> Point.element_at grid |> Option.value_exn
  in
  let is_symbol_at pos =
    match Point.element_at grid pos with
    | Some x when Char.equal x symbol -> true
    | _ -> false
  in
  let is_outward_corner pos corner =
    List.for_all corner ~f:(fun corner_dir ->
        not (is_symbol_at (Point.move corner_dir pos)))
  in
  let is_inward_corner pos corner =
    let diag = List.fold corner ~init:Point.zero ~f:Point.move in
    (not (is_symbol_at (Point.move diag pos)))
    && List.for_all corner ~f:(fun corner_dir ->
           is_symbol_at (Point.move corner_dir pos))
  in
  region |> Set.to_list
  |> List.sum
       (module Int)
       ~f:(fun pos ->
         List.count corners ~f:(fun corner ->
             is_outward_corner pos corner || is_inward_corner pos corner))

let part2 grid =
  scan_all_regions grid
  |> List.sum
       (module Int)
       ~f:(fun region ->
         let corners = count_corners grid region in
         corners * Set.length region)

let solve filename =
  let grid = filename |> Stdio.In_channel.read_all |> parse in
  let p1 = part1 grid and p2 = part2 grid in
  (Some (Int.to_string p1), Some (Int.to_string p2))
