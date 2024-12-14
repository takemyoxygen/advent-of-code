open! Core

module Block = struct
  type t = File of { size : int; id : int } | Free of int [@@deriving sexp_of]

  let of_nums nums =
    List.mapi nums ~f:(fun idx size ->
        if idx % 2 = 0 then File { size; id = idx / 2 } else Free size)

  let size = function File { size; _ } -> size | Free size -> size
end

let compact blocks =
  let dequeue = blocks |> List.to_array |> Deque.of_array in
  let rec loop acc =
    match (Deque.peek_front dequeue, Deque.peek_back dequeue) with
    | None, _ | _, None -> acc
    | Some (Block.File _ as f), _ | Some (Block.Free 0 as f), _ ->
        Deque.dequeue_front dequeue |> ignore;
        loop (f :: acc)
    | _, Some (Block.Free _) ->
        Deque.dequeue_back dequeue |> ignore;
        loop acc
    | ( Some (Block.Free free_size),
        Some (Block.File { size = file_size; _ } as file) )
      when free_size >= file_size ->
        let free' = Block.Free (free_size - file_size) in
        Deque.dequeue_front dequeue |> ignore;
        Deque.enqueue_front dequeue free';
        Deque.dequeue_back dequeue |> ignore;
        loop (file :: acc)
    | Some (Block.Free free_size), Some (Block.File { size = file_size; id }) ->
        Deque.dequeue_front dequeue |> ignore;
        Deque.dequeue_back dequeue |> ignore;
        let file_back = Block.File { id; size = file_size - free_size } in
        let file_compacted = Block.File { id; size = free_size } in
        Deque.enqueue_back dequeue file_back;
        loop (file_compacted :: acc)
  in
  loop [] |> List.rev

let compact_2 blocks =
  let blocks_with_positions =
    List.folding_map blocks ~init:0 ~f:(fun acc bl ->
        let next_pos = acc + Block.size bl in
        (next_pos, (acc, bl)))
  in
  let free_blocks =
    blocks_with_positions
    |> List.filter_map ~f:(fun (pos, bl) ->
           match bl with Block.Free size -> Some (pos, size) | _ -> None)
    |> Map.of_alist_exn (module Int)
  in
  let files =
    blocks_with_positions
    |> List.filter ~f:(fun (_, bl) ->
           match bl with Block.File _ -> true | _ -> false)
    |> List.rev
  in
  let compacted_files = Hashtbl.create (module Int) in
  let rec loop free_blocks = function
    | [] -> ()
    | ((file_pos, Block.File { size = file_size; _ }) as f) :: rest -> (
        let free_space =
          Map.to_sequence ~order:`Increasing_key free_blocks
            ~keys_less_or_equal_to:file_pos
          |> Sequence.find ~f:(fun (_, sz) -> sz >= file_size)
        in
        match free_space with
        | None ->
            Hashtbl.set compacted_files ~key:file_pos ~data:f;
            loop free_blocks rest
        | Some (start, free_size) ->
            Hashtbl.set compacted_files ~key:start ~data:f;
            let free_blocks' =
              Map.remove free_blocks start |> fun m ->
              if free_size > file_size then
                Map.add_exn m ~key:(start + file_size)
                  ~data:(free_size - file_size)
              else m
            in
            loop free_blocks' rest)
    | _ -> assert false
  in
  loop free_blocks files;
  compacted_files |> Hashtbl.to_alist
  |> List.sort ~compare:(Comparable.lift Int.compare ~f:fst)

let part1 blocks =
  blocks |> compact
  |> List.fold ~init:(0, 0) ~f:(fun (pos, acc) block ->
         match block with
         | Block.Free size -> (pos + size, acc)
         | Block.File { size; id } ->
             let file_check = size * ((2 * pos) + (size - 1)) / 2 * id in
             (pos + size, acc + file_check))
  |> snd

let part2 blocks =
  blocks |> compact_2
  |> List.fold ~init:0 ~f:(fun acc (pos, (_, file)) ->
         match file with
         | Block.File { size; id } ->
             let file_check = size * ((2 * pos) + (size - 1)) / 2 * id in
             acc + file_check
         | _ -> assert false)

let solve filename =
  let blocks =
    filename |> Stdio.In_channel.read_all |> String.strip |> String.to_list
    |> List.map ~f:Char.get_digit_exn
    |> Block.of_nums
  in
  let p1 = part1 blocks and p2 = part2 blocks in
  (Some (Int.to_string p1), Some (Int.to_string p2))
