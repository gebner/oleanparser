import Std

open Std ShareCommon
unsafe abbrev RefMap (α) := @Std.HashMap Object α ⟨Object.ptrEq⟩ ⟨Object.ptrHash⟩
unsafe def RefMap.find? (m : RefMap β) (a : α) : Option β := HashMap.find? m (unsafeCast a)
unsafe def RefMap.insert (m : RefMap β) (a : α) (b : β) : RefMap β := HashMap.insert m (unsafeCast a) b

inductive Obj
  | scalar (n : Nat)
  | ctor (ctor : Nat) (fields : Array Obj) (sfields : ByteArray)
  | array (data : Array Obj)
  | sarray (data : ByteArray)
  | string (data : String)
  | thunk (value : Obj)
  | task (value : Obj)
  | ref (ref : Obj)
  | mpz (value : Int)
  deriving Inhabited

namespace Obj

instance : Coe Nat Obj := ⟨Obj.scalar⟩
instance : Coe String Obj := ⟨Obj.string⟩

def sarray' (bs : List UInt8) := Obj.sarray bs.toByteArray

unsafe def countRefsCore (o : Obj) : StateM (RefMap Nat) Unit := do
  modify fun m => m.insert o (match m.find? o with | some i => i + 1 | none => 1)
  match o with
  | Obj.scalar .. => pure ()
  | Obj.ctor _ xs _ => xs.forM countRefsCore
  | Obj.array xs => xs.forM countRefsCore
  | Obj.sarray .. => pure ()
  | Obj.string .. => pure ()
  | Obj.thunk x => x.countRefsCore
  | Obj.task x => x.countRefsCore
  | Obj.ref x => x.countRefsCore
  | Obj.mpz .. => pure ()

unsafe def countRefs (o : Obj) : RefMap Nat :=
  o.countRefsCore.run {} |>.2

unsafe structure ReprState where
  ids : RefMap Format := {}
  decls : Array Format := #[]

unsafe abbrev ReprM := StateM ReprState

unsafe def ReprM.run (m : ReprM Format) : Format :=
  StateT.run' (s := {}) <| show StateM ReprState Format from do
  let fmt ← m
  pure <| Format.joinSep ((← get).decls.push fmt |>.toList) "\n"

unsafe def reprCore : Obj → ReprM Format
  | Obj.scalar n => pure <| repr n
  | o => do
    if let some res := (← get).ids.find? o then return res
    let res ← match o with
      | Obj.ctor idx fields sfields =>
        pure f!"Obj.ctor {idx}{Format.line}{← fields.mapM reprCore}{Format.line}{sfields}"
      | Obj.array fields => pure f!"{← fields.mapM reprCore}"
      | Obj.string s => pure <| repr s
      | Obj.thunk v => pure <| f!"Obj.thunk{Format.line}{← reprCore v}"
      | Obj.task v => pure <| f!"Obj.task{Format.line}{← reprCore v}"
      | Obj.ref r => pure <| f!"Obj.ref{Format.line}{← reprCore r}"
      | Obj.sarray bs => pure <| f!"Obj.sarray'{Format.line}{bs}"
      | Obj.mpz v => pure f!"Obj.mpz{Format.line}{v}"
      | Obj.scalar .. => unreachable!
    let res := res.fill.nest 2
    let newDeclId := s!"x{(← get).ids.size + 1}"
    modify fun st => { st with
      ids := st.ids.insert o newDeclId
      decls := st.decls.push f!"let {newDeclId} :={Format.line}{res}".group
    }
    return newDeclId

unsafe instance : Repr Obj where
  reprPrec o := Repr.addAppParen o.reprCore.run
