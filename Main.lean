import AdventOfCode2025


def main : IO Unit := do
  let input := (← IO.FS.readFile "./Inputs/d1.in").trim
  println! "{solve D1.parser D1.PartTwo.work input}"
-- #eval Id.run (do ← IO.FS.readFile ".")
