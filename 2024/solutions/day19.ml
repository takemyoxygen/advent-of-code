open Core

let read_input filename =
  match
    filename |> In_channel.read_all |> String.strip
    |> Str.split (Str.regexp "\n\n")
  with
  | [ towels; patterns ] ->
      let towels = Str.split (Str.regexp ", ") towels in
      let patterns = String.split_lines patterns in
      (towels, patterns)
  | _ -> failwith "invalid input"

module Trie = struct
  module Memo_key = struct
    module T = struct
      type t = char list [@@deriving hash, sexp, compare]
    end

    include T
    include Hashable.Make (T)
  end

  type t = {
    children : t Char.Table.t;
    mutable terminal : bool;
    memo : int Memo_key.Table.t;
  }

  let create () =
    {
      children = Hashtbl.create (module Char) ~size:26;
      terminal = false;
      memo = Memo_key.Table.create ();
    }

  let add_word t word =
    let rec add_word_chars t chars =
      match chars with
      | [] -> t.terminal <- true
      | ch :: rest ->
          let child = Hashtbl.find_or_add t.children ch ~default:create in
          add_word_chars child rest
    in
    add_word_chars t (String.to_list word)

  let count_combinations trie_root =
    let rec memo_loop node chars =
      match Hashtbl.find node.memo chars with
      | Some result -> result
      | None ->
          let result = loop node chars in
          Hashtbl.set node.memo ~key:chars ~data:result;
          result
    and loop node chars =
      match chars with
      | [] -> if node.terminal then 1 else 0
      | ch :: rest ->
          Hashtbl.find node.children ch
          |> Option.value_map
               ~f:(fun child ->
                 if child.terminal then [ child; trie_root ] else [ child ])
               ~default:[]
          |> List.sum (module Int) ~f:(fun child -> memo_loop child rest)
    in
    fun word -> memo_loop trie_root (String.to_list word)
end

let solve filename =
  let towels, patterns = read_input filename in
  let trie = Trie.create () in
  List.iter towels ~f:(Trie.add_word trie);

  let match_combinations =
    List.map patterns ~f:(fun p -> Trie.count_combinations trie p)
  in
  let p1 = List.count match_combinations ~f:Int.is_positive
  and p2 = List.sum (module Int) match_combinations ~f:Fn.id in
  (Some (Int.to_string p1), Some (Int.to_string p2))
