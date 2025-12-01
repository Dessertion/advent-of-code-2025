

def solve {α β} (parser : String → α) (work : α → β) (input : String) : β :=
  input |> parser |> work
