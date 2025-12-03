import day2
import gleeunit

pub fn main() -> Nil {
  gleeunit.main()
}

// gleeunit test functions end in `_test`
pub fn day2_test() {
  assert day2.find_invalid_ids(#("82", "95")) == [88]
  assert day2.find_invalid_ids(#("82", "83")) == []
  assert day2.find_invalid_ids(#("11", "22")) == [11, 22]
  assert day2.find_invalid_ids(#("1698522", "1698528")) == []
  assert day2.find_invalid_ids(#("998", "1012")) == [1010]
}
