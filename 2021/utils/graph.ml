open! Base
open Core

let bfs m ~start ~adjacent =
  let visited = Hash_set.create m in
  let queue = Queue.create () in
  let () = Queue.enqueue queue start in
  let rec loop () =
    if Queue.is_empty queue then visited |> Hash_set.to_list
    else
      let p = Queue.dequeue_exn queue in
      if Hash_set.mem visited p then loop ()
      else
        let () = Hash_set.add visited p in
        let () =
          adjacent p visited
          |> List.filter ~f:(fun adj -> not (Hash_set.mem visited adj))
          |> List.iter ~f:(Queue.enqueue queue)
        in
        loop ()
  in
  loop ()
