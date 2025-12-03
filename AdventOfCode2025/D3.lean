import AdventOfCode2025.Util

namespace D3

-- input: lines of the form x₁x₂x₃...xₙ, with xᵢ ∈ [1, 9]
-- assumption: each line is at least length 2.
def parser (input : String) : List (List ℕ) :=
  input.splitToList (·.isWhitespace) |>
    List.map (fun s ↦ s.toList.map (· |> Char.toString |> String.toNat!))

private
def testString :=
"987654321111111
811111111111119
234234234234278
818181911112111"

#eval parser testString
private def testList := parser testString

def work_helper (w : List ℕ → ℕ) (input : List (List ℕ)) : ℕ :=
  input.map w |> List.sum

def printAnswer (w : List ℕ → ℕ) : IO Unit := do
  let input := (← IO.FS.readFile s!"./Inputs/d3.in").trim
  println! "{solve parser (work_helper w) input}"

-- Part One:
-- for each line, find the length-two subsequence which the maximum value when concatted
-- as a string and read as a nat, then sum it all up

def work₁ (l : List ℕ) : ℕ :=
  -- get the largest digit which occurs (and is not the last digit),
  -- then get the largest digit which occurs after the index where the largest digit occurs.
  -- for each digit, get the first occurrence in the list (if it exists)
  let inner : Option ℕ := do
    let x₁ ← l.dropLast.max?
    let x₂ ← l.drop (l.idxOf x₁ + 1) |> List.max?
    x₁ * 10 + x₂
  inner.get!

#eval solve parser (work_helper work₁) testString
#eval printAnswer work₁
-- 16973

-- Part Two:
-- instead of length-two subsequence, we need a length-12 subsequence
#eval List.range' 1 9 |>.reverse

def work₂ (num_needed : ℕ) (l : List ℕ) :=
  -- note, each line is length 100
  -- probably want a recursive solution

  let rec helper (num_needed : ℕ) (l : List ℕ) : Option (List ℕ) :=
    if num_needed == 0 then some [] else
    if num_needed > l.length then none else
    if num_needed == l.length then some l else
    -- find the largest digit d ∈ l such that (helper (num_needed - 1) l[d + 1:]) is some,
    -- append d to (helper (num_needed - 1) l [d + 1:])
    let f (i : ℕ) : Option (List ℕ) := do
      i :: (← helper (num_needed - 1) $ l.drop ((← l.idxOf? i) + 1))
    List.range' 1 9 |>.reverse |>.firstM f
  termination_by l.length
  decreasing_by simp_all; omega

  let rec reify : List ℕ → ℕ
    | [] => 0
    | x :: xs => x + 10 * reify xs

  helper 12 l |>.get! |>.reverse |> reify
  -- Id.run do
  --   let some answer := helper 12 l | unreachable!
  --   answer.reverse |> reify

#eval work₂ 12 testList[0]!
#eval solve parser (work_helper $ work₂ 12) testString
#eval printAnswer $ work₂ 12
-- 168027167146027

#eval List.replicate 5 [1,2,3] |>.flatten
