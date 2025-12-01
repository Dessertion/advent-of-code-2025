import Qq

def solve {α β} (parser : String → α) (work : α → β) (input : String) : β :=
  input |> parser |> work

unsafe def eval_string (s : String) : Lean.Elab.TermElabM String := do
  -- this make the string into an expression which is of type string
  let s := s!"(toString ({s}) : String)"

  -- parse the string using the current environment
  let env: Lean.Environment ← Lean.getEnv
  let stx?: Except String Lean.Syntax := Lean.Parser.runParserCategory env `term s
  let stx : Lean.Syntax ← Lean.ofExcept stx?

  -- elaborate the type of the expression
  let tp : Lean.Expr ← Lean.Elab.Term.elabTypeOf stx none

  -- evaluate the expression and return result
  let x ← Lean.Elab.Term.evalTerm String tp stx (safety := Lean.DefinitionSafety.unsafe)
  return x
