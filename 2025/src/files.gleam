import simplifile
import gleam/string

pub fn read_file(path: String) {
  case simplifile.is_file(path) {
    Ok(True) -> {
      let assert Ok(content) = simplifile.read(path)
        as { "unable to read file: " <> path }
      string.trim(content)
    }
    _ -> panic as { path <> " is not a proper file" }
  }
}
