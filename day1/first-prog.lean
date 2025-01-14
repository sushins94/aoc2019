def readIntsFromFile (filename : String) : IO (List Int) := do
  let contents ← IO.FS.readFile filename
  let lines := contents.splitOn "\n"
  let nums := lines.filterMap String.toInt?
  return nums


def total_fuel (fuels: List Int) : Int :=
  List.foldl (fun acc x => acc + (x/3 - 2) ) 0 fuels

def calc_fuel (mass : Int): Int := mass / 3 - 2

partial def recursive_fuel (mass : Int) : Int :=
  let fuel := calc_fuel mass;
  if fuel ≤ 0 then 0
  else
      fuel + recursive_fuel  fuel

def total_fuel_func (f: Int -> Int) (fuels: List Int) : Int :=
  List.foldl (fun acc x => acc + f x ) 0 fuels

def main : IO Unit := do
  let numbers ← readIntsFromFile "input.txt"
  --IO.print s!"{total_fuel numbers}\n"
  IO.print s!"{total_fuel_func calc_fuel numbers}\n"
  IO.print s!"{total_fuel_func recursive_fuel numbers}\n"
