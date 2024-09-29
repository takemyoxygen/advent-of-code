open Base
open Core

module Player = struct
  type t = { id : int; pos : int; score : int }

  let size = 10
  let create ~id ~init_pos = { id; pos = init_pos - 1; score = 0 }

  let advance p v =
    let pos = (p.pos + v) % size in
    { p with pos; score = p.score + pos + 1 }
end

let dice =
  let vals = Sequence.range ~stop:`inclusive 1 100 in
  Sequence.repeat vals |> Sequence.concat
  |> (fun seq -> Sequence.chunks_exn seq 3)
  |> Sequence.map ~f:(List.reduce_exn ~f:( + ))

let round player dice =
  match Sequence.next dice with
  | Some (v, rest) -> (Player.advance player v, rest)
  | _ -> assert false

let simple_play player1 player2 =
  let rec loop (current : Player.t) (another : Player.t) dice consumed_dices =
    if current.score >= 1000 then (current, another, consumed_dices)
    else if another.score >= 1000 then (another, current, consumed_dices)
    else
      let current', dice' = round current dice in
      loop another current' dice' (consumed_dices + 3)
  in
  loop player1 player2 dice 0

let quantum_dice =
  let open List.Let_syntax in
  let xs = [ 1; 2; 3 ] in
  let all =
    let%bind a = xs in
    let%bind b = xs in
    let%map c = xs in
    a + b + c
  in
  all
  |> List.sort_and_group ~compare:Int.compare
  |> List.map ~f:(fun gr ->
         let el = List.hd_exn gr in
         let cnt = List.length gr in
         (el, cnt))

let quantum_play player1 player2 =
  let rec loop (current : Player.t) (another : Player.t) universes =
    if current.score >= 21 then [ (current.id, universes) ]
    else if another.score >= 21 then [ (another.id, universes) ]
    else
      quantum_dice
      |> List.map ~f:(fun (dice_val, dice_univ) ->
             let current' = Player.advance current dice_val in
             loop another current' (universes * dice_univ))
      |> List.concat
      |> List.sort_and_group ~compare:(fun (id1, _) (id2, _) ->
             Int.compare id1 id2)
      |> List.map ~f:(fun gr ->
             let id = gr |> List.hd_exn |> fst in
             let univ = gr |> List.map ~f:snd |> List.reduce_exn ~f:( + ) in
             (id, univ))
  in
  loop player1 player2 1

let solve _ =
  let p1 = Player.create ~id:1 ~init_pos:7 in
  let p2 = Player.create ~id:2 ~init_pos:9 in

  let part1 =
    let _, loser, consumed = simple_play p1 p2 in
    loser.score * consumed
  in

  let part2 =
    quantum_play p1 p2 |> List.map ~f:snd
    |> List.max_elt ~compare:Int.compare
    |> Option.value_exn
  in

  (Some (Int.to_string part1), Some (Int.to_string part2))
