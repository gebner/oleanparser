import Lake
open Lake DSL

package oleanparser

lean_lib Oleanparser

@[default_target]
lean_exe oleanparser where
  root := `Main
