import Oleanparser.Object
import Oleanparser.ByteArrayParser
import Std
open IO
open ByteArrayParser

def find (objs : Std.HashMap UInt64 Obj) (ptr : UInt64) : ByteArrayParser Obj := do
  if ptr &&& 1 = 1 then
    Obj.scalar <| ptr.toNat >>> 1
  else if let some obj := objs.find? ptr then
    obj
  else
    error s!"object not found: {ptr}"

def parseArrayElems (objs : Std.HashMap UInt64 Obj) (n : Nat) : ByteArrayParser (Array Obj) := do
  let mut arr := #[]
  for i in [0:n] do
    arr := arr.push (← find objs (← read64LE))
  arr

def parseObj (objs : Std.HashMap UInt64 Obj) : ByteArrayParser Obj := do
  let rc ← read32LE
  unless rc = 0 do error s!"nonpersistent object: rc={rc}"
  let cs_sz ← read16LE
  let other ← read8
  let tag ← read8
  match tag with
  | 245 => error "closure" -- cannot be compacted
  | 246 =>
    let size ← read64LE
    let capacity ← read64LE
    unless size = capacity do
      error s!"array has different capacity={capacity} than size={size}"
    let fields ← parseArrayElems objs size.toNat
    Obj.array fields
  | 247 => error "struct array"
  | 248 =>
    let size ← read64LE
    let capacity ← read64LE
    unless size = capacity do
      error s!"scalar array has different capacity={capacity} than size={size}"
    let data ← readBytes capacity.toNat
    Obj.sarray <| data.extract 0 size.toNat
  | 249 =>
    let size ← read64LE
    let capacity ← read64LE
    let _length ← read64LE
    unless size = capacity do
      error s!"string has different capacity={capacity} than size={size}"
    let utf8 ← readBytes size.toNat
    let utf8 := utf8.extract 0 (utf8.size - 1) -- drop zero terminator
    Obj.string <| String.fromUTF8Unchecked utf8 -- TODO
  | 250 =>
    let capacity ← read32LE
    let signSize ← read32LE
    let limbsPtr ← read64LE
    -- 8 = sizeof(mp_limb_t) = sizeof(unsigned long int)  (except on Windows? 😱)
    let limbs ← readBytes (capacity.toNat * 8)
    Obj.mpz
  | 251 =>
    let value ← read64LE
    let closure ← read64LE
    unless closure = 0 do
      error s!"thunk has non-zero closure"
    Obj.thunk <|<- find objs value
  | 252 =>
    let value ← read64LE
    let imp ← read64LE
    unless imp = 0 do
      error s!"task has non-zero implementation"
    Obj.task <|<- find objs value
  | 253 =>
    Obj.ref (← find objs (← read64LE))
  | 254 => error "external"
  | 255 => error "reserved"
  | ctor =>
    let numFields := other.toNat
    let lenExceptSFields := 8 + 8*numFields
    if lenExceptSFields > cs_sz.toNat then
      error s!"cs_sz={cs_sz} not correct, should be larger than 8+8*{numFields}"
    let lenSFields := cs_sz.toNat - lenExceptSFields
    let mut fields := #[]
    for i in [0:numFields] do
      let ptr ← read64LE
      fields := fields.push (← find objs ptr)
    let sfields ← readBytes lenSFields
    -- dbg_trace s!"obj {cs_sz} {ctor} {numFields} {lenSFields} {other} {tag}"
    Obj.ctor ctor.toNat fields sfields

def advanceToAlignment : ByteArrayParser Unit := do
  modify fun pos =>
    let rem := pos % 8
    if rem = 0 then pos else pos + (8 - rem)

def parseOLean : ByteArrayParser Obj := do
  let pos0 ← get
  expectBs "oleanfile!!!!!!!".toUTF8
  let base ← read64LE
  let mut objs : Std.HashMap UInt64 Obj := {}
  let root ← read64LE
  for _ in [0:←remaining] do
    advanceToAlignment
    unless (← remaining) > 0 do break
    let pos ← get
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
