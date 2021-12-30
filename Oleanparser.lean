import Std
open IO

structure ByteArrayParser.Error where
  pos : Nat
  msg : String
  deriving Repr, Inhabited

instance : ToString ByteArrayParser.Error where
  toString err := reprStr err

abbrev ByteArrayParser :=
  ReaderT ByteArray <| StateT Nat <| Except ByteArrayParser.Error

-- def UInt8.toUInt32 (x : UInt8) : UInt32 :=
--   ⟨_, _⟩o
deriving instance Repr for Except

namespace ByteArrayParser

def run (p : ByteArrayParser α) (b : ByteArray) (pos := 0) : Except ByteArrayParser.Error (α × Nat) :=
  (ReaderT.run p b).run pos

def run' (p : ByteArrayParser α) (b : ByteArray) (pos := 0) : Except ByteArrayParser.Error α := do
  (← p.run b pos).1

@[inline]
def error (msg : String) : ByteArrayParser α := do
  Except.error ({ pos := ← get, msg } : Error)

def peek : ByteArrayParser UInt8 := do
  let ba ← read
  let pos ← get
  if h : pos < ba.size then
    ba.get ⟨pos, h⟩
  else
    error "eof"

def read8 : ByteArrayParser UInt8 :=
  peek <* modify (· + 1)

def expectB (b : UInt8) : ByteArrayParser Unit := do
  let b' ← read8
  unless b = b' do error s!"got {b'}, expected {b}"

def expectBs (bs : ByteArray) : ByteArrayParser Unit := do
  for b in bs do expectB b

def read16LE : ByteArrayParser UInt16 := do
  let a ← read8
  let b ← read8
  return a.toUInt16 ||| (b.toUInt16 <<< 8)

def read32LE : ByteArrayParser UInt32 := do
  let a ← read8
  let b ← read8
  let c ← read8
  let d ← read8
  return a.toUInt32 ||| (b.toUInt32 <<< 8) ||| (c.toUInt32 <<< 16) ||| (d.toUInt32 <<< 24)

def read64LE : ByteArrayParser UInt64 := do
  return (← read32LE).toUInt64 ||| ((← read32LE).toUInt64 <<< 32)

def readBytes (sz : Nat) : ByteArrayParser ByteArray := do
  let pos ← get
  let ba ← read
  if pos + sz <= ba.size then
    ba.extract pos (pos + sz) <* modify (· + sz)
  else
    error s!"eof before {sz} bytes"

-- #eval read32LE.run' (ByteArray.mk #[1,0,0,0])

def remaining : ByteArrayParser Nat :=
  return (← read).size - (← get)

end ByteArrayParser

instance : Repr ByteArray where
  reprPrec bs p := reprPrec bs.toList p

inductive Obj
  | scalar (n : Nat)
  | constructor (ctor : Nat) (fields : Array Obj) (sfields : ByteArray)
  | array (data : Array Obj)
  | sarray (data : ByteArray)
  | string (data : String)
  -- | thunk
  -- | task
  | ref (ref : Obj)
  -- | mpz
  deriving Inhabited

instance : Coe Nat Obj := ⟨Obj.scalar⟩
instance : Coe String Obj := ⟨Obj.string⟩

open Std Lean ShareCommon

unsafe structure ReprState where
  ids : @Std.HashMap Object Format ⟨Object.ptrEq⟩ ⟨Object.ptrHash⟩ := {}
  decls : Array Format := #[]

open Std Lean ShareCommon in
unsafe def Obj.reprCore : Obj → StateM ReprState Format
  | Obj.scalar n => repr n
  | o => do
    if let some res := (← get).ids.find? (unsafeCast o) then return res
    let res ← match o with
      | Obj.constructor ctor fields sfields =>
        s!"Obj.constructor {ctor} {← fields.mapM reprCore} {sfields}"
      | Obj.array fields => s!"{← fields.mapM reprCore}"
      | Obj.string s => toString <| repr s
      | _ => panic "unexpected case"
    let newDeclId := s!"x{(← get).ids.size + 1}"
    modify fun st => { st with
      ids := st.ids.insert (unsafeCast o) newDeclId
      decls := st.decls.push s!"let {newDeclId} := {res}"
    }
    return newDeclId

unsafe def Obj.repr (o : Obj) : StateM ReprState Format := do
  let fmt ← o.reprCore
  Format.joinSep ((← get).decls.push fmt |>.toList) "\n"

unsafe instance : Repr Obj where
  reprPrec o _ := (o.repr.run {}).1

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
  | 245 => error "closure"
  | 246 =>
    let size ← read64LE
    let capacity ← read64LE
    unless size = capacity do
      error s!"array has different capacity={capacity} than size={size}"
    let fields ← parseArrayElems objs size.toNat
    Obj.array fields
  | 247 => error "struct array"
  | 248 => error "scalar array"
  | 249 =>
    let size ← read64LE
    let capacity ← read64LE
    let _length ← read64LE
    unless size = capacity do
      error s!"string has different capacity={capacity} than size={size}"
    let utf8 ← readBytes size.toNat
    let utf8 := utf8.extract 0 (utf8.size - 1) -- drop zero terminator
    Obj.string <| String.fromUTF8Unchecked utf8 -- TODO
  | 250 => error "mpz"
  | 251 => error "thunk"
  | 252 => error "task"
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
    Obj.constructor ctor.toNat fields sfields

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
