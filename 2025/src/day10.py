import sys
import z3
import json

def clause(buttons_with_vars, idx, target):
  buttons = [var for (idxs, var) in buttons_with_vars if idx in idxs]
  if not buttons:
    print(list(buttons_with_vars))
    raise Exception(f"No buttons for tgt: {target} at {idx}")
  return z3.Sum(buttons) == target


def solve(buttons, targets):
  vars = list(map(lambda idx: z3.Int(f"button{idx + 1}"), range(0, len(buttons))))
  opt = z3.Optimize()
  buttons = list(zip(map(set, buttons), vars))
  clauses = [clause(buttons, idx, tgt) for (idx,tgt) in enumerate(targets)] 
  constraints = [var >= 0 for var in vars]
  opt.add(*(clauses + constraints))
  opt.minimize(z3.Sum(vars))
  opt.check()
  model = opt.model()
  return sum([model.eval(var, model_completion=True).as_long() for var in vars])
  
  
def main():
  file = open(sys.argv[1])
  inputs = json.load(file)
  answers = [solve(row["buttons"], row["target"]) for row in inputs]
  print(sum(answers))
  
if __name__ == "__main__":
  main()