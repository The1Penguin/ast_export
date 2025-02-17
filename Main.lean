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
  initSearchPath (← findSysroot)
  let srcSearchPath ← initSrcSearchPath
  match args with
  | [mainModuleName] =>
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
