import Oleanparser
import Lean
open Lean

structure Any where
  α : Type
  a : α

instance : Inhabited Any := ⟨_, ()⟩
instance : OfNat Any n := ⟨_, n⟩

instance : Coe α Any := ⟨(⟨_, ·⟩)⟩

unsafe def testData : Array Any :=
  #[123, "123", #[1,2,3], [1,2,3,4].toByteArray,
    (-1 : Int), (3.14 : Float),
    123456789123456789123456789123456789,
    (-123456789123456789123456789123456789 : Int),
    (Thunk.mk fun () => 123),
    (Task.spawn fun () => 123),
    unsafeBaseIO (IO.mkRef ())]

-- TODO: add API to serialize olean files in-memory
#eval saveModuleData "Demo.notolean" `Demo (unsafeCast testData)

#eval parseOLean.run <$> IO.FS.readBinFile "Demo.notolean"
