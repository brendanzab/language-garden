/-
  A proof of the correctness of an arithmetic expression compiler in Lean 4.

  Ported from [expcompile.v], which is part of Derek Dreyer and Gert Smolka's
  [course material].

  [expcompile.v]: https://www.ps.uni-saarland.de/courses/sem-ws17/expcompile.v
  [course material]: https://courses.ps.uni-saarland.de/sem_ws1718/3/Resources
-/

/- Simple arithmetic expressions -/
inductive Expr : Type
  | nat : Nat → Expr
  | add : Expr → Expr → Expr
  | sub : Expr → Expr → Expr

/- The semantics of arithmetic expressions -/
def Expr.eval : (expr : Expr) → Nat
  | Expr.nat n => n
  | Expr.add e₁ e₂ => e₁.eval + e₂.eval
  | Expr.sub e₁ e₂ => e₁.eval - e₂.eval


/- A stack machine language for arithmetic expressions -/

/- Stack machine commands -/
inductive Command : Type
  | nat : Nat → Command
  | add : Command
  | sub : Command

/- A stack machine program -/
abbrev Program :=
  List Command


/- The semantics of the stack machine language -/
def Program.eval : (program : Program) → (stack : List Nat) → List Nat
  | [], stack => stack
  | (Command.nat n :: program), stack => eval program (n :: stack)
  | (Command.add :: program), (n₁ :: n₂ :: stack) => eval program ((n₁ + n₂) :: stack)
  | (Command.sub :: program), (n₁ :: n₂ :: stack) => eval program ((n₁ - n₂) :: stack)
  | _, _ => []

example : Program.eval [Command.nat 1, Command.nat 2, Command.add] [] = [3] := rfl
example : Program.eval [Command.nat 1, Command.nat 2, Command.sub] [] = [1] := rfl


/- Compilation -/

/- Compile an arithmetic expression to a stack machine program -/
def Expr.compile : (expr : Expr) → Program
  | Expr.nat n => [Command.nat n]
  | Expr.add e₁ e₂ => e₂.compile ++ e₁.compile ++ [Command.add]
  | Expr.sub e₁ e₂ => e₂.compile ++ e₁.compile ++ [Command.sub]


/- Compilation preserves the semantics of arithmetic expressions -/
theorem Expr.compile.correctness (e : Expr) (program : Program) (stack : List Nat) :
    Program.eval (e.compile ++ program) stack = Program.eval program (e.eval :: stack)
  := by
    induction e generalizing program stack with
      | nat => rfl
      | add e₁ e₂ ih₁ ih₂ =>
          simp [Expr.compile]
          repeat rw [List.append_assoc]
          rw [ih₂, ih₁]
          rfl
      | sub e₁ e₂ ih₁ ih₂ =>
          simp [Expr.compile]
          repeat rw [List.append_assoc]
          rw [ih₂, ih₁]
          rfl

/- Compiler correctness for the nil program and stack -/
theorem Expr.compile.correctness_nil (e : Expr) :
    Program.eval e.compile [] = [e.eval]
  := by
    rw [← List.append_nil (Expr.compile e)]
    exact correctness e (program := []) (stack := [])


/- Decompilation -/

/- Decompile a stack machine program back into an arithmetic expression -/
def Program.decompile : (program : Program) → (stack : List Expr := []) → Option Expr
  | [], [expr] =>
      some expr
  | (Command.nat n :: program), stack =>
      decompile program (Expr.nat n :: stack)
  | (Command.add :: program), e₁ :: e₂ :: stack =>
      decompile program (Expr.add e₁ e₂ :: stack)
  | (Command.sub :: program), e₁ :: e₂ :: stack =>
      decompile program (Expr.sub e₁ e₂ :: stack)
  | _, _ =>
      none

example :
  Program.decompile [Command.nat 1]
    = some (Expr.nat 1) := rfl

example :
  Program.decompile [Command.nat 1, Command.nat 2, Command.add]
    = some (Expr.add (Expr.nat 2) (Expr.nat 1)) := rfl

example (n₁ n₂ n₃ : Nat) :
  Program.decompile [Command.nat n₁, Command.nat n₂, Command.nat n₃, Command.add, Command.sub]
    = some (Expr.sub (Expr.add (Expr.nat n₃) (Expr.nat n₂)) (Expr.nat n₁)) := rfl


theorem Program.decompile.compile_append (e : Expr) (p : Program) (stack : List Expr) :
    Program.decompile ((Expr.compile e) ++ p) stack = Program.decompile p (e :: stack)
  := by
    induction e generalizing p stack with
      | nat => rfl
      | add e₁ e₂ ih₁ ih₂ =>
          simp [Expr.compile]
          repeat rw [List.append_assoc]
          rw [ih₂, ih₁]
          rfl
      | sub e₁ e₂ ih₁ ih₂ =>
          simp [Expr.compile]
          repeat rw [List.append_assoc]
          rw [ih₂, ih₁]
          rfl

/- Decompilation preserves the semantics of arithmetic expressions -/
theorem Program.decompile.correctness (e : Expr) :
    Program.decompile e.compile = some e
  := by
    induction e with
      | nat => rfl
      | add e₁ e₂ ih₁ ih₂ =>
          simp [Expr.compile]
          repeat rw [List.append_assoc]
          repeat rw [Program.decompile.compile_append]
          rfl
      | sub e₁ e₂ ih₁ ih₂ =>
          simp [Expr.compile]
          repeat rw [List.append_assoc]
          repeat rw [Program.decompile.compile_append]
          rfl
