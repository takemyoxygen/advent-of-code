open Base

val bfs :
  'a Hash_set.Key.t ->
  start:'a ->
  adjacent:('a -> 'a Hash_set.t -> 'a list) ->
  'a list
