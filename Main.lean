/-
Copyright (c) 2024 Mario Carneiro. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mario Carneiro
-/
import Lean.Elab.Frontend
import AstExport

/-! # `lake exe ast-export` command

This command will export the AST of a lean file or project.

-/

def help : String := "AST export tool
Usage:
  lake exe ast-export <MODULE> # export the given module

Arguments:
  <MODULE>  A module path like `Mathlib.Tactic.Basic`.
"

open Lean

/-- The main entry point. See `help` for more information on arguments. -/
unsafe def main (args : List String) : IO UInt32 := do
  match args with
  | [mainModuleName] =>
    match (← IO.getEnv "LEAN_SRC_PATH") with
    | none => /- TODO: imitate Lake.CLI.env more robustly -/
      let child ← IO.Process.spawn {
        cmd := (← IO.appPath).toString
        args := #[ mainModuleName ]
        env := #[ ("LEAN_SRC_PATH", some "./.") ]
      }
      child.wait
    | _ => /- We have all necessary vars -/
      initSearchPath (← findSysroot)
      let srcSearchPath ← initSrcSearchPath
      let mainModuleName := String.toName mainModuleName
      enableInitializersExecution
      let ast ← ASTExport.getASTForModule srcSearchPath mainModuleName
      let stdout ← IO.getStdout
      stdout.writeJson (toJson ast)
      return 0
  | _ =>
    let stderr ← IO.getStderr
    stderr.putStrLn help
    return 1
