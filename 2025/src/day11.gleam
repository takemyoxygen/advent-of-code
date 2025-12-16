import files
import gleam/deque
import gleam/dict
import gleam/int
import gleam/list
import gleam/option
import gleam/pair
import gleam/result
import gleam/set
import gleam/string

fn parse_input(file) {
  files.read_file(file)
  |> string.split("\n")
  |> list.map(fn(line) {
    let assert [device, ..outputs] =
      string.replace(line, each: ":", with: "") |> string.split(" ")
    #(device, outputs)
  })
  |> dict.from_list
}

fn bfs_count_paths(adj, paths, queue, pred) {
  case deque.pop_front(queue) {
    Error(Nil) -> paths
    Ok(#(node, queue)) -> {
      let next =
        dict.get(adj, node)
        |> result.unwrap([])
        |> list.filter(pred)
      let queue = list.fold(next, queue, deque.push_back)
      let paths =
        dict.upsert(paths, node, fn(existing) {
          case existing {
            option.None -> 1
            option.Some(num) -> num + 1
          }
        })
      bfs_count_paths(adj, paths, queue, pred)
    }
  }
}

fn bfs_visited(adj, visited, queue) {
  case deque.pop_front(queue) {
    Error(Nil) -> visited
    Ok(#(node, queue)) -> {
      case set.contains(visited, node) {
        True -> bfs_visited(adj, visited, queue)
        False -> {
          let next =
            dict.get(adj, node)
            |> result.unwrap([])
            |> list.filter(fn(n) { !set.contains(visited, n) })
          let queue = list.fold(next, queue, deque.push_back)
          bfs_visited(adj, set.insert(visited, node), queue)
        }
      }
    }
  }
}

fn part1(input) {
  let paths =
    bfs_count_paths(input, dict.new(), deque.from_list(["you"]), fn(_) { True })
  let assert Ok(result) = dict.get(paths, "out")
  result
}

fn revert(adj) {
  dict.to_list(adj)
  |> list.flat_map(fn(edges) {
    let #(node, adjacents) = edges
    list.map(adjacents, fn(adj) { #(adj, node) })
  })
  |> list.group(pair.first)
  |> dict.map_values(fn(_, pairs) { list.map(pairs, pair.second) })
}

fn unique_paths(adj, from, to) {
  let reverted = revert(adj)
  let nodes_to_use =
    bfs_visited(reverted, set.new(), deque.from_list([to])) |> set.insert(to)
  let paths =
    bfs_count_paths(adj, dict.new(), deque.from_list([from]), fn(node) {
      set.contains(nodes_to_use, node)
    })
  let assert Ok(result) = dict.get(paths, to)
  result
}

fn part2(input) {
  // splitting into 3 segments: paths svr->fft, fft->dac, dac->out
  // paths dac-fft don't exist
  let seg1 = unique_paths(input, "svr", "fft")
  let seg2 = unique_paths(input, "fft", "dac")
  let seg3 = unique_paths(input, "dac", "out")

  seg1 * seg2 * seg3
}

pub fn solve(file) {
  let input = parse_input(file)
  let part1 = part1(input) |> int.to_string |> option.Some
  let part2 = part2(input) |> int.to_string |> option.Some
  #(part1, part2)
}
