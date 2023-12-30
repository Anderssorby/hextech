import Lake
open Lake DSL

package «HexTech» where
  moreLinkArgs := #["-L.lake/packages/SDL/.lake/build/lib", "-lsdl2-shim", "-lSDL2", "-lSDL2_image"]
  -- add package configuration options here

lean_lib «HexTech» where
  -- add library configuration options here

--require LeanCopilot from git "https://github.com/lean-dojo/LeanCopilot.git" @ "v1.0.1"
require std from git "https://github.com/leanprover/std4" @ "v4.4.0"
require SDL from git "https://github.com/Anderssorby/SDL.lean" @ "main"

@[default_target]
lean_exe «hextech» where
  root := `Main
