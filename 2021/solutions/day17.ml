open! Base
open Core

let xs, ys = ((70, 125), (-159, -121))

let quadratic_eq ~a ~b ~c =
  let open Float in
  let d = (b * b) - (4.0 * a * c) in
  let x1 = (-b - sqrt d) / (2.0 * a) in
  let x2 = (-b + sqrt d) / (2.0 * a) in
  (x1, x2)

let progression_sum ~initial ~delta ~n =
  ((2 * initial) + (delta * (n - 1))) * n / 2

let num_of_progression_elements ~initial ~delta ~target =
  let a = Float.of_int delta in
  let b = Float.of_int ((2 * initial) - delta) in
  let c = Float.of_int (-2 * target) in
  let x1, x2 = quadratic_eq ~a ~b ~c in
  match List.filter [ x1; x2 ] ~f:(fun x -> Float.(x > 0.0)) with
  | [ x ] -> x
  | xs ->
      let xs_str = List.map xs ~f:string_of_float |> String.concat ~sep:", " in
      failwith (sprintf "Expected 1 positive root, got %s" xs_str)

let y_time ~vy ~y_min ~y_max =
  let t_min =
    num_of_progression_elements ~initial:vy ~delta:(-1) ~target:y_max
    |> Float.round_up |> int_of_float
  in
  let t_max =
    num_of_progression_elements ~initial:vy ~delta:(-1) ~target:y_min
    |> Float.round_down |> int_of_float
  in
  let at_t_min = progression_sum ~initial:vy ~delta:(-1) ~n:t_min in
  let at_t_max = progression_sum ~initial:vy ~delta:(-1) ~n:t_max in
  let within x = y_min <= x && x <= y_max in
  if t_min <= t_max && within at_t_min && within at_t_max then
    Some (t_min, t_max)
  else None

let x_dist ~vx ~t = progression_sum ~initial:vx ~delta:(-1) ~n:(min t vx)

let part1 () =
  let min_y, _ = ys in
  let vy = -min_y - 1 in
  progression_sum ~initial:1 ~delta:1 ~n:vy

let part2 () =
  let x_min, x_max = xs in
  let y_min, y_max = ys in
  let vy_min = y_min in
  let vy_max = -y_min in
  let vx_min = 1 in
  let vx_max = x_max in
  let within_x x = x_min <= x && x <= x_max in
  let vxs = List.range ~stop:`inclusive vx_min vx_max in
  let vys = List.range ~stop:`inclusive vy_min vy_max in
  List.bind vys ~f:(fun vy ->
      match y_time ~vy ~y_min ~y_max with
      | None -> []
      | Some (t_min, t_max) ->
          let times = Sequence.range ~stop:`inclusive t_min t_max in
          vxs
          |> List.filter ~f:(fun vx ->
                 Sequence.exists times ~f:(fun t -> x_dist ~vx ~t |> within_x))
          |> List.map ~f:(fun vx -> (vx, vy)))
  |> List.length
