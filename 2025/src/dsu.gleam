import gleam/dict
import gleam/list
import gleam/result

pub opaque type Dsu(node) {
  Dsu(
    parents: dict.Dict(node, node),
    sizes: dict.Dict(node, Int),
    roots_count: Int,
  )
}

pub fn of_list(nodes) {
  let parents = list.fold(nodes, dict.new(), fn(d, x) { dict.insert(d, x, x) })
  let sizes = list.fold(nodes, dict.new(), fn(d, x) { dict.insert(d, x, 1) })
  Dsu(parents:, sizes:, roots_count: list.length(nodes))
}

pub fn find(dsu: Dsu(node), node: node) {
  case dict.get(dsu.parents, node) {
    Ok(parent) if parent == node -> Ok(node)
    Ok(parent) -> find(dsu, parent)
    err -> err
  }
}

pub fn union(dsu: Dsu(node), n1: node, n2: node) {
  use root1 <- result.try(find(dsu, n1))
  use root2 <- result.try(find(dsu, n2))
  case root1 == root2 {
    True -> Ok(dsu)
    False -> {
      use size1 <- result.try(dict.get(dsu.sizes, root1))
      use size2 <- result.try(dict.get(dsu.sizes, root2))
      let #(child, parent) = case size1 >= size2 {
        True -> #(root2, root1)
        False -> #(root1, root2)
      }
      let parents = dict.insert(dsu.parents, child, parent)
      let sizes = dict.insert(dsu.sizes, parent, size1 + size2)
      Ok(Dsu(parents:, sizes:, roots_count: dsu.roots_count - 1))
    }
  }
}

pub fn roots_count(dsu: Dsu(node)) {
  dsu.roots_count
}

pub fn roots(dsu: Dsu(node)) {
  dsu.parents
  |> dict.to_list
  |> list.filter_map(fn(pair) {
    case pair {
      #(n1, n2) if n1 == n2 ->
        dict.get(dsu.sizes, n1) |> result.map(fn(size) { #(n1, size) })
      _ -> Error(Nil)
    }
  })
}
