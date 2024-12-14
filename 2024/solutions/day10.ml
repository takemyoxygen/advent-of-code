open! Core
open Utils

let parse input =
  input |> String.split_lines
  |> List.map ~f:(fun line ->
         line |> String.to_array |> Array.map ~f:Char.get_digit_exn)
  |> Array.of_list

let directions =
  [
    Point.Direction.up;
    Point.Direction.down;
    Point.Direction.left;
    Point.Direction.right;
  ]

let score grid start =
  let visited =
    Graph.bfs
      (module Point)
      ~start
      ~adjacent:(fun pos _ ->
        let current = Point.element_at grid pos |> Option.value_exn in
        directions
        |> List.map ~f:(Point.move pos)
        |> List.filter ~f:(fun adj ->
               match Point.element_at grid adj with
               | Some x when x = current + 1 -> true
               | _ -> false))
  in
  visited
  |> List.count ~f:(fun p ->
         match Point.element_at grid p with Some 9 -> true | _ -> false)

let part2 grid =
  let iter_on n ~f =
    Array.iteri grid ~f:(fun y row ->
        Array.iteri row ~f:(fun x i -> if i = n then f x y))
  in
  let scores =
    Array.init (Array.length grid) ~f:(fun y ->
        Array.create ~len:(Array.length grid.(y)) 0)
  in
  List.range 0 10 |> List.rev
  |> List.iter ~f:(fun n ->
         iter_on n ~f:(fun x y ->
             if n = 9 then scores.(y).(x) <- 1
             else
               let pos = Point.create x y in
               let adjs_scores =
                 directions
                 |> List.map ~f:(Point.move pos)
                 |> List.sum
                      (module Int)
                      ~f:(fun adj ->
                        match Point.element_at grid adj with
                        | Some x when x = n + 1 ->
                            Point.element_at scores adj |> Option.value_exn
                        | _ -> 0)
               in
               scores.(y).(x) <- adjs_scores));
  let total = ref 0 in
  iter_on 0 ~f:(fun x y ->
      let v = scores.(y).(x) in
      total := !total + v);
  !total

let part1 grid =
  Array.concat_mapi grid ~f:(fun y row ->
      Array.mapi row ~f:(fun x v ->
          if v = 0 then score grid (Point.create x y) else 0))
  |> Array.sum (module Int) ~f:Fn.id

let solve filename =
  let input = filename |> Stdio.In_channel.read_all |> String.strip |> parse in
  let p1 = part1 input and p2 = part2 input in
  (Some (Int.to_string p1), Some (Int.to_string p2))
