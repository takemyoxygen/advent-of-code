open! Core

let concat a b =
  Int.to_string a ^ Int.to_string b
  |> Int.of_string_opt
  |> Option.value ~default:Int.max_value

let can_be_combined ?(with_concat = false) target nums =
  let opt_apply opt f value =
    match opt with Some x -> Some (f x value) | None -> Some value
  in
  let rec loop result left =
    match (result, left) with
    | Some res, _ when res = target -> true
    | _, [] -> false
    | Some res, _ when res > target -> false
    | res, next :: rest ->
        let add = lazy (loop (opt_apply res ( + ) next) rest)
        and mult = lazy (loop (opt_apply res ( * ) next) rest)
        and concat =
          lazy
            (if with_concat then loop (opt_apply res concat next) rest
             else false)
        in
        [ add; mult; concat ] |> List.exists ~f:force
  in
  loop None nums

let parse_input input =
  input |> String.split_lines
  |> List.map ~f:(fun line ->
         line
         |> String.substr_replace_first ~pattern:":" ~with_:""
         |> String.split ~on:' ' |> List.map ~f:Int.of_string
         |> fun l -> (List.hd_exn l, List.tl_exn l))

let part1 input =
  input
  |> List.filter ~f:(fun (tgt, nums) -> can_be_combined tgt nums)
  |> List.sum (module Int) ~f:fst

let part2 input =
  input
  |> List.filter ~f:(fun (tgt, nums) ->
         can_be_combined ~with_concat:true tgt nums)
  |> List.sum (module Int) ~f:fst

let solve filename =
  let input = filename |> Stdio.In_channel.read_all |> parse_input in
  let p1 = part1 input and p2 = part2 input in
  (Some (Int.to_string p1), Some (Int.to_string p2))
