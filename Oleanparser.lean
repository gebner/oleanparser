import Oleanparser.Object
import Oleanparser.ByteArrayParser
open IO
open ByteArrayParser

structure Stats where
  bytesPerTag : Lean.HashMap UInt8 Nat := ∅
  objsPerTag : Lean.HashMap UInt8 Nat := ∅
  numPointers : Nat := 0

structure State where
  stats : Stats := {}

abbrev M := StateT State ByteArrayParser
def M.run (act : M α) (b : ByteArray) (stats : Stats := {}) := StateT.run act { stats } |>.run b

def find (objs : Lean.HashMap UInt64 Obj) (ptr : UInt64) : M Obj := do
  if ptr &&& 1 = 1 then
    pure <| Obj.scalar <| ptr.toNat >>> 1
  else if let some obj := objs.find? ptr then
    modify fun st => { st with stats.numPointers := st.stats.numPointers + 1 }
    pure <| obj
  else
    error s!"object not found: {ptr}"

def parseArrayElems (objs : Lean.HashMap UInt64 Obj) (n : Nat) : M (Array Obj) := do
  let mut arr := #[]
  for i in [0:n] do
    arr := arr.push (← find objs (← read64LE))
  pure <| arr

def parseObj (objs : Lean.HashMap UInt64 Obj) : M Obj := do
  let initOff ← getOffset
  let rc ← read32LE
  unless rc = 0 do error s!"nonpersistent object: rc={rc}"
  let cs_sz ← read16LE
  let other ← read8
  let tag ← read8
  let obj ← match tag with
  | 245 => error "closure" -- cannot be compacted
  | 246 =>
    let size ← read64LE
    let capacity ← read64LE
    unless size = capacity do
      error s!"array has different capacity={capacity} than size={size}"
    let fields ← parseArrayElems objs size.toNat
    pure <| Obj.array fields
  | 247 => error "struct array"
  | 248 =>
    let size ← read64LE
    let capacity ← read64LE
    unless size = capacity do
      error s!"scalar array has different capacity={capacity} than size={size}"
    let data ← readBytes capacity.toNat
    pure <| Obj.sarray <| data.extract 0 size.toNat
  | 249 =>
    let size ← read64LE
    let capacity ← read64LE
    let _length ← read64LE
    unless size = capacity do
      error s!"string has different capacity={capacity} than size={size}"
    let utf8 ← readBytes size.toNat
    let utf8 := utf8.extract 0 (utf8.size - 1) -- drop zero terminator
    pure <| Obj.string <| String.fromUTF8Unchecked utf8 -- TODO
  | 250 =>
    let capacity ← read32LE
    let signSize ← read32LE
    let (size, sign) := -- TODO: implement Int32
      if (signSize >>> 31) == 0 then
        (signSize, 1)
      else
        (0-signSize, -1)
    unless size = capacity do
      error s!"mpz has different capacity={capacity} than size={size}"
    let limbsPtr ← read64LE
    -- TODO: verify that limbsPtr starts here
    let limbs ← readArray size.toNat read64LE -- mb_limb_t
    let nat : Nat := limbs.foldr (init := 0) -- limbs are little-endian
      fun limb acc => (acc <<< 64) ||| limb.toNat
    pure <| Obj.mpz (sign * nat)
  | 251 =>
    let value ← read64LE
    let closure ← read64LE
    unless closure = 0 do
      error s!"thunk has non-zero closure"
    pure <| Obj.thunk <|<- find objs value
  | 252 =>
    let value ← read64LE
    let imp ← read64LE
    unless imp = 0 do
      error s!"task has non-zero implementation"
    pure <| Obj.task <|<- find objs value
  | 253 =>
    pure <| Obj.ref (← find objs (← read64LE))
  | 254 => error "external"
  | 255 => error "reserved"
  | ctor =>
    let numFields := other.toNat
    let lenExceptSFields := 8 + 8*numFields
    if lenExceptSFields > cs_sz.toNat then
      error s!"cs_sz={cs_sz} not correct, should be larger than 8+8*{numFields}"
    let lenSFields := cs_sz.toNat - lenExceptSFields
    let mut fields := #[]
    for _ in [0:numFields] do
      let ptr ← read64LE
      fields := fields.push (← find objs ptr)
    let sfields ← readBytes lenSFields
    -- dbg_trace s!"obj {cs_sz} {ctor} {numFields} {lenSFields} {other} {tag}"
    pure <| Obj.ctor ctor.toNat fields sfields
  let endOff ← getOffset
  modify fun st => { st with
    stats.bytesPerTag := st.stats.bytesPerTag.insert tag (st.stats.bytesPerTag.findD tag 0 + endOff - initOff)
    stats.objsPerTag := st.stats.objsPerTag.insert tag (st.stats.objsPerTag.findD tag 0 + 1)
  }
  return obj

def advanceToAlignment : ByteArrayParser Unit := do
  modify fun pos =>
    let rem := pos % 8
    if rem = 0 then pos else pos + (8 - rem)

def parseOLean : M Obj := do
  let pos0 ← getOffset
  expectBs "oleanfile!!!!!!!".toUTF8
  let base ← read64LE
  let mut objs : Lean.HashMap UInt64 Obj := {}
  let root ← read64LE
  for _ in [0:←remaining] do
    advanceToAlignment
    unless (← remaining) > 0 do break
    let pos ← getOffset
    let obj ← parseObj objs
    let memPtr := base + (pos - pos0).toUInt64
    -- dbg_trace (pos, memPtr, reprStr obj)
    -- dbg_trace memPtr
    objs := objs.insert (base + (pos - pos0).toUInt64) obj
  find objs root

open Process

-- #eval show IO Obj from do
--   let cnt ← IO.FS.readBinFile "/home/gebner/lean4/build/release/stage1/lib/lean/Init/Core.olean"
--   let (obj, last) ← ofExcept <| parseOLean.run cnt
--   obj
