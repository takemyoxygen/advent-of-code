import gleam/int

pub fn parse_or_panic(str: String) {
  let assert Ok(result) = int.parse(str) as { "failed to parse: " <> str }
  result
}
