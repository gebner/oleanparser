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

partial def reifyName : Obj → Lean.Name
  | .scalar 0 => .anonymous
  | .ctor 1 #[n, .string s] _ => .str (reifyName n) s
  | .ctor 2 #[n, .scalar p] _ => .num (reifyName n) p
  | _ => unreachable!

def Lean.HashMap.inc (m : Lean.HashMap String Nat) (s : String) (amount := 1) :=
  m.insert s (m.findD s 0 + amount)

def Obj.toArray! : Obj → Array Obj
  | .array a => a
  | _ => unreachable!

def Lean.Name.getString? : Name → Option String
  | .str _ s => s
  | _ => none

unsafe def runCmd (p : Parsed) : IO UInt32 := do
  let mut cumStats := {}
  let mut reachableObjsPerCat : Lean.HashMap String Nat := ∅
  for file in p.variableArgsAs! String do
    let cnt ← IO.FS.readBinFile file
    let ((obj, st), _) ← ofExcept <| parseOLean.run cnt cumStats
    if p.hasFlag "dump" then
      println <| repr obj
    cumStats := st.stats
    if p.hasFlag "env-stats" then
      if let .ctor 0 #[_imports, constNames, constants, _extraConstNames, entries] _ := obj then
        reachableObjsPerCat := Id.run <| StateT.run' (s := ∅) do
          let mut m := reachableObjsPerCat
          for (name, c) in constNames.toArray!.zip constants.toArray! do
            let name := reifyName name
            let .ctor i #[.ctor _ args _] _ := c | unreachable!
            -- count `ConstVal` signature objects, then body, then anything else
            let sigObjs ← if i == 2 || i == 1 then args[0]!.countNewObjs else pure 0
            if name.getString?.any (·.startsWith "_cstage") || name.components.any (·.getString?.any (·.startsWith "_spec")) then
              m := m.inc "old compiler constants" (← c.countNewObjs)
            else if i == 2 then
              m := m.inc "proofs" (← args[1]!.countNewObjs)
            else if i == 1 then
              let reducibilityHints := args[2]!
              if !(reducibilityHints matches .ctor 1 ..) then
                m := m.inc "non-abbrev def bodies" (← args[1]!.countNewObjs)
            let cat := #["axioms", "defs", "theorems", "opaques", "quots", "inductives", "inductives", "inductives"][i]!
            m := m.inc cat <| sigObjs + (← c.countNewObjs)
          for e in entries.toArray! do
            let .ctor 0 #[extName, extEntries] _ := e | unreachable!
            let crefs := (← extEntries.countNewObjs) - 1  -- ignore containing array
            if crefs > 1 then
              m := m.inc (reifyName extName |> toString) crefs
          pure m
  if p.hasFlag "stats" then
    let cumBytes := cumStats.bytesPerTag.toArray.foldl (· + ·.2) 0
    let cumObjs := cumStats.objsPerTag.toArray.foldl (· + ·.2) 0
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
    println ""
  unless reachableObjsPerCat.isEmpty do
    let cumObjs := cumStats.objsPerTag.toArray.foldl (· + ·.2) 0
    println "objects reachable from"
    for (name, num) in reachableObjsPerCat.toArray.qsort (·.2 > ·.2) do
      println s!"{ppRelAbs num cumObjs} {name}"
  return 0
where
  ppRelAbs n m (suffix := "") :=
    s!"{ppNum n |>.leftpad 12}{suffix} ({(n * 100 + 50) / m |> ppNum |>.leftpad 4}%)"

unsafe def cmd : Cmd := `[Cli|
  oleanparser VIA runCmd;
  "Parses .olean Lean object files and prints various information."

  FLAGS:
    dump;  "Prints pretty-printed object trees."
    stats; "Prints object metadata statistics."
    "env-stats"; "Prints statistics about `Lean.Environment` object roots."

  ARGS:
    ...inputs : String; "The .olean files to parse."
]

unsafe def main (args : List String) : IO UInt32 :=
  cmd.validate args
