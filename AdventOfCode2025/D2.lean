import AdventOfCode2025.Util

namespace D2

-- input: things of the form `a₁-b₁,a₂-b₂,...`, all in one line
def parser (input : String) : List (Nat × Nat) :=
  let pairs := input.splitToList (· == ',')
  let f (s : String) : Nat × Nat :=
    if let [x, y] := s.splitToList (· == '-') |> .map String.toNat! then
      ⟨x, y⟩
    else
      panic! "input malformed"
  pairs.map f

private def testString :=
"11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

#eval parser testString

-- part one problem:
-- for each range of values a-b, record all values x ∈ [a, b]
-- such that x = yy as strings, without leading zeroes
-- eg:
--  - 11-22 has two invalid IDs, 11 and 22
--  - 95-115 has 1 invalid ID, 99
--  - 998-1012 has 1 invalid ID, 1010
-- then sum up all such values

def testInvalid₁ (n : Nat) : Bool :=
    let s := n.repr
    let len := s.length
    if Odd len then false else
    let s₁ := Substring.Raw.mk s 0 ⟨len/2⟩
    let s₂ := Substring.Raw.mk s ⟨len/2⟩ ⟨len⟩
    s₁ == s₂

def work (testInvalid : ℕ → Bool) (input : List (Nat × Nat)) : Nat :=
  let getInvalid (p : Nat × Nat) : List Nat :=
    List.range' p.1 (p.2 - p.1 + 1) |> .filter testInvalid
  input.map getInvalid |> List.flatten |> List.sum

#eval solve parser (work testInvalid₁) testString


def print₁ : IO Unit := do
  let input := (← IO.FS.readFile s!"./Inputs/d2.in").trim
  println! "{solve parser (work testInvalid₁) input}"

#eval print₁
-- 16793817782

-- part two problem:
-- now, invalid check tests if x = yy+, ie. if x is some sequence of digits repeated at least twice,
-- so 123123123 = (123) three times
-- again, add up all invalids

-- m is the length of the prefix
def testInvalid₂_helper (s : String) (m : ℕ) : Bool :=
  let pre : Substring.Raw := ⟨s, 0, ⟨m⟩⟩
  let n := s.length / m
  -- Substring.Raw.extract : Substring.Raw → String.Pos.Raw → String.Pos.Raw → Substring.Raw
  List.range' 0 n m |>
    List.map (fun i ↦ Substring.Raw.mk s ⟨i⟩ ⟨i + m⟩ == pre) |>
    List.and

#eval testInvalid₂_helper "123123123" 3
#eval testInvalid₂_helper "123123122" 3
#eval testInvalid₂_helper "11" 2

def testInvalid₂ (n : Nat) : Bool :=
  -- let's run through all factors of x.length
  -- for each factor m | x.length, take the substring x[0:m]
  -- `Substring.Raw.dropPrefix? : (s pre : Substring.Raw) → Option Substring.Raw` looks useful
  let s := n.repr
  let len := s.length
  len.divisorsAntidiagonalList |>
    List.map (fun (a, _) ↦ testInvalid₂_helper s a) |>
    List.dropLast |>
    List.or

#eval List.range' 0 9 3
#eval (9).divisorsAntidiagonalList.map (fun p : ℕ × ℕ ↦ testInvalid₂_helper "123123122" p.1)
#eval testInvalid₂ 123123123
#eval testInvalid₂ 123123122


def print₂ : IO Unit := do
  let input := (← IO.FS.readFile s!"./Inputs/d2.in").trim
  println! "{solve parser (work testInvalid₂) input}"

#time #eval print₂
-- time: 16891ms
-- 27469417404
