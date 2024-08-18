let input = Stdio.In_channel.read_lines "./input/day3.txt"
let invert ch = if ch = '0' then '1' else '0'

let most_common nums pos =
  let zeros =
    nums |> List.to_seq
    |> Seq.map (fun n -> n.[pos])
    |> Seq.filter (( = ) '0')
    |> Seq.length
  in
  if zeros > List.length nums / 2 then '0' else '1'

let int_of_binary_string s = Printf.sprintf "0b%s" s |> int_of_string

let int_of_binary_chars bits =
  bits |> List.to_seq |> String.of_seq |> int_of_binary_string

let part1 () =
  let gamma =
    Core.List.range 0 (List.hd input |> String.length)
    |> List.map (most_common input)
  in
  let epsilon = List.map invert gamma in
  int_of_binary_chars gamma * int_of_binary_chars epsilon

let part2 () =
  let find_rating adjust_bit nums =
    let rec loop pos = function
      | [ x ] -> x
      | nums ->
          let bit_val = most_common nums pos |> adjust_bit in
          nums |> List.filter (fun n -> n.[pos] = bit_val) |> loop (pos + 1)
    in
    loop 0 nums
  in
  let oxyg = find_rating (fun x -> x) input in
  let co2 = find_rating invert input in
  int_of_binary_string oxyg * int_of_binary_string co2
