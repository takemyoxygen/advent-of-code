open Base

type number_part = Open | Close | Literal of int

type node = {
  mutable prev : node option;
  mutable next : node option;
  mutable value : number_part;
}

let lines = Stdio.In_channel.read_lines "./input/day18.txt"

let line_to_linked_list line =
  String.to_list line
  |> List.filter ~f:(Char.( <> ) ',')
  |> List.map ~f:(function
       | '[' -> Open
       | ']' -> Close
       | x -> Literal (Char.get_digit_exn x))
  |> List.fold ~init:(None, None) ~f:(fun (fst, lst) item ->
         let node = { value = item; prev = lst; next = None } in
         let () = match lst with Some l -> l.next <- Some node | _ -> () in
         let fst' = Option.value fst ~default:node in
         (Some fst', Some node))
  |> fun (fst, lst) -> (Option.value_exn fst, Option.value_exn lst)

let find_in_linked_list ~start ~dir ~pred =
  let rec loop = function
    | None -> None
    | Some x when pred x.value -> Some x
    | Some x -> loop (dir x)
  in
  loop (Some start)

let find_next = find_in_linked_list ~dir:(fun n -> n.next)
let find_prev = find_in_linked_list ~dir:(fun n -> n.prev)
let is_literal = function Literal _ -> true | _ -> false

let add_num num = function
  | Some ({ value = Literal x; _ } as n) -> n.value <- Literal (x + num)
  | _ -> ()

let connect l r =
  l.next <- Some r;
  r.prev <- Some l;
  ()

let create_node value = { value; next = None; prev = None }

let split_node num =
  let left = num / 2 in
  let right = num - left in
  let op = create_node Open in
  let cl = create_node Close in
  let left_node = create_node (Literal left) in
  let right_node = create_node (Literal right) in
  connect op left_node;
  connect left_node right_node;
  connect right_node cl;
  (op, cl)

let explode_first start =
  let rec loop node depth =
    let next = Option.bind node ~f:(fun n -> n.next) in
    match (node, next) with
    | None, _ -> None
    | Some { value = Open; _ }, _ -> loop next (depth + 1)
    | Some { value = Close; _ }, _ -> loop next (depth - 1)
    | ( Some { value = Literal l; prev = Some op; _ },
        Some { value = Literal r; next = Some cl; _ } )
      when depth >= 5 ->
        find_prev ~start:op ~pred:is_literal |> add_num l;
        find_next ~start:cl ~pred:is_literal |> add_num r;
        let zero = create_node (Literal 0) in
        Option.map op.prev ~f:(fun prev -> connect prev zero) |> ignore;
        Option.map cl.next ~f:(fun next -> connect zero next) |> ignore;
        Some ()
    | _ -> loop next depth
  in
  loop (Some start) 0

let split_first start =
  let rec loop node =
    match node with
    | None -> None
    | Some { value = Literal x; next = Some n; prev = Some p } when x >= 10 ->
        let op, cl = split_node x in
        connect p op;
        connect cl n;
        Some ()
    | Some n -> loop n.next
  in
  loop (Some start)

let reduce_once start =
  match explode_first start with None -> split_first start | x -> x

let reduce start =
  let rec loop () =
    match reduce_once start with Some _ -> loop () | None -> ()
  in
  loop ()

let add (fst1, lst1) (fst2, lst2) =
  let op = create_node Open in
  let cl = create_node Close in
  connect lst1 fst2;
  connect op fst1;
  connect lst2 cl;
  (op, cl)

let linked_list_to_tree start =
  let rec read_elem node =
    match node with
    | None -> assert false
    | Some { value = Open; next; _ } -> (
        let item1, next' = read_elem next in
        let item2, next'' = read_elem next' in
        match next'' with
        | Some { value = Close; next = after_pair; _ } ->
            (`pair (item1, item2), after_pair)
        | _ -> assert false)
    | Some { value = Literal x; next; _ } -> (`literal x, next)
    | _ -> assert false
  in
  read_elem (Some start) |> fst

let rec magnitude = function
  | `literal x -> x
  | `pair (left, right) -> (3 * magnitude left) + (2 * magnitude right)

let sum_all nums =
  List.reduce_exn nums ~f:(fun l1 l2 ->
      let sum_fst, sum_lst = add l1 l2 in
      reduce sum_fst;
      (sum_fst, sum_lst))

let part1 () =
  let numbers = List.map lines ~f:line_to_linked_list in
  let total, _ = sum_all numbers in
  let tree = linked_list_to_tree total in
  magnitude tree

let part2 () =
  let magnitude_of_sum line1 line2 =
    let sum, _ = add (line_to_linked_list line1) (line_to_linked_list line2) in
    reduce sum;
    linked_list_to_tree sum |> magnitude
  in
  List.cartesian_product lines lines
  |> List.filter ~f:(fun (l1, l2) -> String.(l1 <> l2))
  |> List.map ~f:(fun (l1, l2) -> magnitude_of_sum l1 l2)
  |> List.max_elt ~compare:Int.compare
  |> Option.value_exn
