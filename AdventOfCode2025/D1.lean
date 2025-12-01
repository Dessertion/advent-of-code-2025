import Mathlib.Data.List.Defs

import AdventOfCode2025.Util

namespace D1

private
def test_list : List Int64 := [-82, 14, -99, -1, -55, 60, -5, 48, -30, -68]

/--
take the input and convert it into a list of +s and -s; e.g.:
```
L68
L30
R48
L5
R60
L55
L1
L99
R14
L82
```
should be converted into:
-82 :: 14 :: -99 :: -1 :: -55 :: 60 :: -5 :: 48 :: -30 :: -68 :: []
-/
def parser (input : String) := Id.run do
  -- first, split the input by '\n'
  let inputList : List String := input.splitToList (. == '\n')
  let parse (s : String) : Int64 :=
    let num := (s.drop 1).toNat! |> Int64.ofNat
    if s.toList[0]! == 'L' then
      -num
    else
      num
  inputList.map parse |> List.reverse

private
def test_string : String :=
"L68
L30
R48
L5
R60
L55
L1
L99
R14
L82"
#eval parser test_string



namespace PartOne

def work (l : List Int64) :=
  let l' : List Int64 :=
    (l.mapAccumr (fun a b ↦ ⟨a + b, a + b⟩) (Int64.ofNat 50)).2
  let l'' := l'.map (fun a ↦ a % 100)
  (l''.count ↑0)
#eval work test_list


#eval solve parser work test_string

/-
- dial starts at 50
- work in mod 100 (left rotation is -, right rotation is +)
- count the number of 0 that appear

use `List.mapAccumRM`.

final answer for part 1: 1078
-/

end PartOne

namespace PartTwo

/-
Part 2:
Now, need to check how many times it rolls over to 0, regardless of whether it happens during a rotation
or at the end of one.
```
Following the same rotations as in the above example, the dial points at zero a few extra times during its rotations:

The dial starts by pointing at 50.
The dial is rotated L68 to point at 82; during this rotation, it points at 0 once.
The dial is rotated L30 to point at 52.
The dial is rotated R48 to point at 0.
The dial is rotated L5 to point at 95.
The dial is rotated R60 to point at 55; during this rotation, it points at 0 once.
The dial is rotated L55 to point at 0.
The dial is rotated L1 to point at 99.
The dial is rotated L99 to point at 0.
The dial is rotated R14 to point at 14.
The dial is rotated L82 to point at 32; during this rotation, it points at 0 once.
In this example, the dial points at 0 three times at the end of a rotation, plus three more times during a rotation.
So, in this example, the new password would be 6.

Be careful: if the dial were pointing at 50, a single rotation like R1000 would cause the dial to point at 0 ten times
before returning back to 50!
```
-/


#eval
  let hmm := fun n ↦ -(-n % 100)
  hmm (-105)

/-
Solution to Part 2:
- need to mod at every step, instead of all at once at the end
- for positive rotation of size n:
    add n/100 directly to counter, let n ← n % 100
- for negative rotation of size n:
    add |n/100| directly to counter, let n ← -(-n % 100)
- if (cur + n) ≥ 100 or (cur + n) ≤ 0, then add 1 to counter...
    problem!! if n = 0 after modding, then you add 1 unnecessarily.
    just special case it out for now, lol.
-/

def work (l : List Int64) :=
  -- List.mapAccumr (f : α → γ → γ × β) : List α → γ → γ × List β
  -- f : (next : α) → (cur : γ) → (newCur : γ) × (mapResult : β)
  let diag {α} : α → α × α := fun a ↦ (a, a)

  let red : Int64 → Int64 × Int64 → (Int64 × Int64) × (Int64 × Int64)
    | next, (cur, ret) =>
      let ret := ret + (Int64.abs next)/100
      let next := next % 100
      if next == 0 then
        diag (cur, ret)
      else
        let sum := cur + next
        let add :=
          if sum ≥ 100 || (cur != 0 && sum ≤ 0) then 1 else 0
        diag ((sum + 100) % 100, ret + add)
  let ((_, ret), _) := l.mapAccumr red (Int64.ofNat 50, Int64.ofNat 0)
  ret

#eval solve parser work test_string
-- #eval test_list.reverse
-- #eval ((Int64.ofInt (-105)) % 100)
-- #eval (-64/100)

-- final answer: 6412

end PartTwo
