import Std

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

open Std ShareCommon

unsafe structure ReprState where
  ids : @Std.HashMap Object Format ⟨Object.ptrEq⟩ ⟨Object.ptrHash⟩ := {}
  decls : Array Format := #[]

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
