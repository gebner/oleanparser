import Cli
import Oleanparser

open IO System Process
open Cli

unsafe def runCmd (p : Parsed) : IO UInt32 := do
  for file in p.variableArgsAs! String do
    let cnt ← IO.FS.readBinFile file
    let (obj, _) ← ofExcept <| parseOLean.run cnt
    if p.hasFlag "dump" then
      println <| repr obj
  return 0

unsafe def cmd : Cmd := `[Cli|
  oleanparser VIA runCmd;
  "Parses .olean Lean object files and prints various information."

  FLAGS:
    dump;  "Prints pretty-printed object trees."

  ARGS:
    ...inputs : String; "The .olean files to parse."
]

unsafe def main (args : List String) : IO UInt32 :=
  cmd.validate args
