import Oleanparser

open IO System Process

unsafe def main (args : List String) : IO Unit :=
  if h : args = [] then do
    println "Usage: ... filename"
    exit 1
  else do
    let cnt ← IO.FS.readBinFile (args.head ‹_›)
    let (obj, last) ← ofExcept <| parseOLean.run cnt
    println <| repr obj
