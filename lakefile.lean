import Lake
open Lake DSL

package oleanparser

require Cli from git "https://github.com/mhuisi/lean4-cli.git" @ "nightly"
require std from git "https://github.com/leanprover/std4.git"

lean_lib Oleanparser

@[default_target]
lean_exe oleanparser where
  root := `Main
