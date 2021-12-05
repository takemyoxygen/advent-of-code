def run_machine(blueprint, total_steps, initial_state):
  position = 0
  state = initial_state
  steps = 0
  tape = {}
  while steps < total_steps:
    write, move, state = blueprint[(state, tape.get(position, 0))]
    tape[position] = write
    position += move
    steps += 1
    if steps % 1000000 == 0:
      print('Step', steps)
  return sum(map(lambda k: tape[k], tape))


test_blueprint = {
  ('A', 0): (1, 1, 'B'),
  ('A', 1): (0, -1, 'B'),
  ('B', 0): (1, -1, 'A'),
  ('B', 1): (1, 1, 'A'),
}

blueprint = {
  ('A', 0): (1, 1, 'B'),
  ('A', 1): (0, -1, 'D'),
  ('B', 0): (1, 1, 'C'),
  ('B', 1): (0, 1, 'F'),
  ('C', 0): (1, -1, 'C'),
  ('C', 1): (1, -1, 'A'),
  ('D', 0): (0, -1, 'E'),
  ('D', 1): (1, 1, 'A'),
  ('E', 0): (1, -1, 'A'),
  ('E', 1): (0, 1, 'B'),
  ('F', 0): (0, 1, 'C'),
  ('F', 1): (0, 1, 'E')
}

print(run_machine(blueprint, 12302209, 'A'))