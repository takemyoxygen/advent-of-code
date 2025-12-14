import gleam/string
import simplifile

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

pub fn write_file(path: String, content: String) {
  case simplifile.write(to: path, contents: content) {
    Ok(_) -> Nil
    Error(err) ->
      panic as {
        "Failed to write to " <> path <> " : " <> simplifile.describe_error(err)
      }
  }
}
