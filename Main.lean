import Std.Data.List.Basic
import Cli
import Oleanparser

open IO System Process
open Cli

def String.leftpad (n : Nat) (s : String) : String :=
  String.mk (.replicate (n - s.length) ' ') ++ s

-- easily add thousands separators to number
def ppNum (n : Nat) : String :=
  toString n |>.toList.reverse.toChunks 3 |>.map (·.reverse |> .mk) |>.intersperse "." |>.reverse |> .join

unsafe def runCmd (p : Parsed) : IO UInt32 := do
  let mut cumStats := {}
  for file in p.variableArgsAs! String do
    let cnt ← IO.FS.readBinFile file
    let ((obj, st), _) ← ofExcept <| parseOLean.run cnt cumStats
    if p.hasFlag "dump" then
      println <| repr obj
    cumStats := st.stats
  if p.hasFlag "stats" then
    let cumBytes := cumStats.bytesPerTag.toArray.foldl (· + ·.2) 0
    let cumObjs := cumStats.objsPerTag.toArray.foldl (· + ·.2) 0
    let ppRelAbs n m (suffix := "") :=
      s!"{ppNum n |>.leftpad 12}{suffix} ({(n * 100 + 50) / m |> ppNum |>.leftpad 4}%)"
    let printStats name objs bytes := do
      if objs > 0 then
        println s!"{name}\t{ppRelAbs objs cumObjs}\t{ppRelAbs bytes cumBytes "B"}"
    let printTagStats tag name := do
      let objs := cumStats.objsPerTag.findD tag.toUInt8 0
      let bytes := cumStats.bytesPerTag.findD tag.toUInt8 0
      printStats name objs bytes
    for (tag, name) in ["array", "struct array", "scalar array", "string", "bignum", "thunk", "task", "ref"].enumFrom 246 do
      printTagStats tag name
    let cumCtorBytes := cumStats.bytesPerTag.toArray.filter (·.1 < 245) |>.foldl (· + ·.2) 0
    let cumCtorObjs := cumStats.objsPerTag.toArray.filter (·.1 < 245) |>.foldl (· + ·.2) 0
    printStats "ctor" cumCtorObjs cumCtorBytes
    for t in [0:245] do
      printTagStats t s!"\ttag {t}"
    printStats "total" cumObjs cumBytes
    println ""
    let sharedPointers := cumStats.numPointers - cumObjs
    println s!"shared pointers\t{ppRelAbs sharedPointers cumStats.numPointers}"
  return 0

unsafe def cmd : Cmd := `[Cli|
  oleanparser VIA runCmd;
  "Parses .olean Lean object files and prints various information."

  FLAGS:
    dump;  "Prints pretty-printed object trees."
    stats; "Prints object metadata statistics."

  ARGS:
    ...inputs : String; "The .olean files to parse."
]

unsafe def main (args : List String) : IO UInt32 :=
  cmd.validate args
