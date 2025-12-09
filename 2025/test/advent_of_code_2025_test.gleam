import day3
import dsu
import gleam/list
import gleeunit

pub fn main() -> Nil {
  gleeunit.main()
}

pub fn day3_test() {
  assert day3.max_in_list(
      [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5],
      how_many_to_keep: 0,
    )
    == #(9, [2, 6, 5, 3, 5])

  assert day3.max_in_list([1, 3, 2, 7], how_many_to_keep: 0) == #(7, [])

  assert day3.max_in_list([1, 3, 2, 7], how_many_to_keep: 3) == #(1, [3, 2, 7])

  assert day3.max_in_list(
      [9, 8, 7, 6, 5, 4, 3, 2, 1, 1, 1, 1, 1, 1, 1],
      how_many_to_keep: 1,
    )
    == #(9, [8, 7, 6, 5, 4, 3, 2, 1, 1, 1, 1, 1, 1, 1])
}

pub fn dsu_test() {
  let dsu = dsu.of_list(list.range(0, 9))
  assert dsu.find(dsu, 3) == Ok(3)

  let assert Ok(dsu) = dsu.union(dsu, 1, 3)
  assert dsu.find(dsu, 1) == Ok(1)
  assert dsu.find(dsu, 3) == Ok(1)

  let assert Ok(dsu) = dsu.union(dsu, 4, 3)
  let assert Ok(dsu) = dsu.union(dsu, 5, 3)
  assert dsu.find(dsu, 5) == Ok(1)
}
