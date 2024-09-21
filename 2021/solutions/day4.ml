let read_input filename =
  let input =
    Stdio.In_channel.read_all filename |> Str.split (Str.regexp "\n\n")
  in
  match input with
  | draw_text :: boards_text ->
      let draw = String.split_on_char ',' draw_text |> List.map int_of_string in
      let boards =
        boards_text
        |> List.map (fun board ->
               board |> String.split_on_char '\n'
               |> List.map (fun line ->
                      String.split_on_char ' ' line
                      |> List.filter (( <> ) "")
                      |> List.map int_of_string))
      in
      (draw, boards)
  | _ -> failwith "Invalid input"

module IntSet = Set.Make (Int)

let has_won drawn board =
  let was_drawn x = IntSet.find_opt x drawn |> Option.is_some in
  let crossed_row board =
    board |> List.exists (fun row -> List.for_all was_drawn row)
  in
  let rec crossed_col board =
    board
    |> List.map (function hd :: tl -> Some (hd, tl) | _ -> None)
    |> List.fold_left
         (fun acc el ->
           match (acc, el) with
           | Some (hds, tls), Some (hd, tl) -> Some (hd :: hds, tl :: tls)
           | _ -> None)
         (Some ([], []))
    |> function
    | None -> false
    | Some (col, rest) -> List.for_all was_drawn col || crossed_col rest
  in
  crossed_row board || crossed_col board

let find_winner_opt drawn boards = List.find_opt (has_won drawn) boards
let get_winners drawn boards = List.partition (has_won drawn) boards

let score drawn board last_drawn =
  board |> List.flatten
  |> List.filter (fun x -> IntSet.find_opt x drawn |> Option.is_none)
  |> List.fold_left ( + ) 0
  |> fun sum -> sum * last_drawn

let solve filename =
  let draw, boards = read_input filename in
  let part1 =
    let rec play_until_first_winner prev to_draw drawn =
      match (get_winners drawn boards, to_draw, prev) with
      | ([], _), pick :: rest, _ ->
          play_until_first_winner (Some pick) rest (IntSet.add pick drawn)
      | (winner :: _, _), _, Some prev -> score drawn winner prev
      | _ -> failwith "Game over"
    in
    play_until_first_winner None draw IntSet.empty
  in
  let part2 =
    let rec play_until_last_winner prev to_draw drawn boards =
      match (get_winners drawn boards, to_draw, prev) with
      | (_, losers), pick :: rest, _ when not (List.is_empty losers) ->
          play_until_last_winner (Some pick) rest (IntSet.add pick drawn) losers
      | ([ winner ], []), _, Some prev -> score drawn winner prev
      | _ -> failwith "Game over"
    in
    play_until_last_winner None draw IntSet.empty boards
  in
  (Some (Int.to_string part1), Some (Int.to_string part2))
