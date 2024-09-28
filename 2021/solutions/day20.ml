open Base
open Core
open Utils

type pixel = Light | Dark [@@deriving compare]

module Image = struct
  type t = { base : pixel; pixels : (Point.t, pixel) Hashtbl.t }

  let get_px t p = Hashtbl.find t.pixels p |> Option.value ~default:t.base

  let compress t =
    {
      t with
      pixels = Hashtbl.filter t.pixels ~f:(fun v -> compare_pixel v t.base <> 0);
    }
end

let read_input filename =
  let lines = Stdio.In_channel.read_lines filename in
  match lines with
  | algo :: _ :: img ->
      let algo_arr =
        String.to_array algo
        |> Array.map ~f:(function '#' -> Light | _ -> Dark)
      in
      let pixels = Hashtbl.create (module Point) in
      List.iteri img ~f:(fun y row ->
          String.iteri row ~f:(fun x ch ->
              let px = if Char.(ch = '#') then Light else Dark in
              Hashtbl.set pixels ~key:(Point.create x y) ~data:px));
      (algo_arr, Image.{ base = Dark; pixels })
  | _ -> assert false

let related_pixel_offsets =
  [
    (-1, -1); (0, -1); (1, -1); (-1, 0); (0, 0); (1, 0); (-1, 1); (0, 1); (1, 1);
  ]
  |> List.map ~f:(fun (dx, dy) -> Point.create dx dy)

let related_pixelds p img =
  related_pixel_offsets
  |> List.map ~f:(Point.move p)
  |> List.map ~f:(Image.get_px img)

let pixels_to_number pixels =
  List.foldi pixels ~init:0 ~f:(fun i acc px ->
      let bit = match px with Light -> 1 | Dark -> 0 in
      (Int.(2 ** (9 - i - 1)) * bit) + acc)

let next_pixel algo pixels =
  let algo_idx = pixels_to_number pixels in
  algo.(algo_idx)

let next_base algo (img : Image.t) =
  List.init 9 ~f:(fun _ -> img.base) |> next_pixel algo

let boundaries_of points =
  let xs = List.map points ~f:Point.x and ys = List.map points ~f:Point.y in
  let x_bound = List_ex.minmax xs ~compare:Int.compare |> Option.value_exn
  and y_bound = List_ex.minmax ys ~compare:Int.compare |> Option.value_exn in
  (x_bound, y_bound)

let apply algo (img : Image.t) =
  let (x_lo, x_hi), (y_lo, y_hi) = boundaries_of (Hashtbl.keys img.pixels) in
  let offset = 2 in
  let x_min, x_max, y_min, y_max =
    (x_lo - offset, x_hi + offset, y_lo - offset, y_hi + offset)
  in
  let xs = List.range ~stop:`inclusive x_min x_max in
  let ys = List.range ~stop:`inclusive y_min y_max in
  let pixels' =
    List.cartesian_product xs ys
    |> List.map ~f:(fun (x, y) ->
           let pos = Point.create x y in
           let px = related_pixelds pos img |> next_pixel algo in
           (pos, px))
    |> Hashtbl.of_alist_exn (module Point)
  in
  let base' = next_base algo img in
  Image.{ base = base'; pixels = pixels' } |> Image.compress

let solve filename =
  let algo, img = read_input filename in
  let apply = apply algo in
  let lit_pxs (img : Image.t) =
    img.pixels |> Hashtbl.to_alist
    |> List.filter ~f:(fun (_, v) -> match v with Light -> true | _ -> false)
    |> List.length
  in
  let part1 = Fn.apply_n_times ~n:2 apply img |> lit_pxs in
  let part2 = Fn.apply_n_times ~n:50 apply img |> lit_pxs in
  (Some (Int.to_string part1), Some (Int.to_string part2))
