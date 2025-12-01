import AdventOfCode2025


unsafe def main : IO Unit := do
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout
  let inputFile := (← stdin.getLine).trim
  -- let (inputFile, parser, work) := (input[0]!, input[1]!, input[2]!)

  let problemInput := (← IO.FS.readFile s!"./Inputs/{inputFile}.in").trim


  stdout.putStrLn s!"{solve D1.parser D1.PartTwo.work problemInput}"
-- #eval Id.run (do ← IO.FS.readFile ".")
