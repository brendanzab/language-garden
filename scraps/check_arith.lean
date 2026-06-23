/-! Verified type system implementations for a simple language of arithmetic
    expressions.
--/

/-!
    Syntax:

    ```
    T ::=
      | Int
      | Bool

    t ::=
      | 0
      | t₁ + t₂
      | t₁ == t₂
      | true
      | false
      | if t₁ then t₂ else t₃
    ```
-/

inductive Ty where
| bool : Ty
| int : Ty

inductive Tm where
| int : Int → Tm
| add : Tm → Tm → Tm
| equal : Tm → Tm → Tm
| true : Tm
| false : Tm
| ifThenElse : Tm → Tm → Tm → Tm


/-! Typing Rules

    ```
    ───────────
      n : Int

      t₁ : Int    t₂ : Int
    ────────────────────────
        t₁ + t₂  : Int

      t₁ : Int    t₂ : Int
    ────────────────────────
        t₁ == t₂  : Bool

    ───────────────     ────────────────
      true : Bool         false : Bool

      t₁ : Bool    t₂ : T    t₃ : T
    ─────────────────────────────────
        if t₁ then t₂ else t₃ : T
    ```
-/

inductive HasType : Tm → Ty → Prop where
| int {n} : HasType (.int n) .int
| add {tm₁ tm₂} :
    HasType tm₁ .int →
    HasType tm₂ .int →
    HasType (.add tm₁ tm₂) .int
| equal {tm₁ tm₂} :
    HasType tm₁ .int →
    HasType tm₂ .int →
    HasType (.equal tm₁ tm₂) .bool
| true : HasType .true .bool
| false : HasType .false .bool
| ifThenElse {tm₁ tm₂ tm₃ ty} :
    HasType tm₁ .bool →
    HasType tm₂ ty →
    HasType tm₃ ty →
    HasType (.ifThenElse tm₁ tm₂ tm₃) ty


/-! Type system implementations -/

def check : Tm → Ty → Bool
| .int _, .int  => true
| .add tm₁ tm₂, .int  => check tm₁ .int && check tm₂ .int
| .equal tm₁ tm₂, .bool  => check tm₁ .int && check tm₂ .int
| .true, .bool => true
| .false, .bool => true
| .ifThenElse tm₁ tm₂ tm₃, ty =>
    check tm₁ .bool && check tm₂ ty && check tm₃ ty
| _, _ => false

def infer : Tm → Option Ty
| .int _ => some .int
| .add tm₁ tm₂ =>
    match infer tm₁, infer tm₂ with
    | some .int, some .int => some .int
    | _, _ => none
| .equal tm₁ tm₂ =>
    match infer tm₁, infer tm₂ with
    | some .int, some .int => some .bool
    | _, _ => none
| .true => some .bool
| .false => some .bool
| .ifThenElse tm₁ tm₂ tm₃ =>
    match infer tm₁, infer tm₂, infer tm₃ with
    | some .bool, some .int, some .int => some .int
    | some .bool, some .bool, some .bool => some .bool
    | _, _, _ => none

theorem has_type_implies_check (tm : Tm) (ty : Ty) : HasType tm ty → check tm ty = true := by
  intros h
  induction h with
  | int | true | false => rfl
  | add _ _ ih₁ ih₂
  | equal _ _ ih₁ ih₂ =>
      simp [check]
      exact ⟨ih₁, ih₂⟩
  | ifThenElse _ _ _ ih₁ ih₂ ih₃ =>
      simp [check]
      exact ⟨⟨ih₁, ih₂⟩, ih₃⟩

theorem check_implies_has_type (tm : Tm) (ty : Ty) : check tm ty = true → HasType tm ty := by
  intros h
  induction tm generalizing ty with
  | int =>
      cases ty with
      | int => exact HasType.int
      | bool => simp [check] at h
  | add _ _ ih₁ ih₂ =>
      cases ty with
      | int =>
          simp [check] at h
          obtain ⟨h₁, h₂⟩ := h
          exact HasType.add (ih₁ _ h₁) (ih₂ _ h₂)
      | bool => simp [check] at h
  | equal _ _ ih₁ ih₂ =>
      cases ty with
      | int => simp [check] at h
      | bool =>
          simp [check] at h
          obtain ⟨h₁, h₂⟩ := h
          exact HasType.equal (ih₁ _ h₁) (ih₂ _ h₂)
  | true =>
      cases ty with
      | int => simp [check] at h
      | bool => exact HasType.true
  | false =>
      cases ty with
      | int => simp [check] at h
      | bool => exact HasType.false
  | ifThenElse _ _ _ ih₁ ih₂ ih₃ =>
      simp [check] at h
      obtain ⟨⟨h₁, h₂⟩, h₃⟩ := h
      exact HasType.ifThenElse (ih₁ _ h₁) (ih₂ _ h₂) (ih₃ _ h₃)

theorem check_correct (tm : Tm) (ty : Ty) : (check tm ty = true) = HasType tm ty :=
  Eq.propIntro (check_implies_has_type tm ty) (has_type_implies_check tm ty)

theorem has_type_implies_infer (tm : Tm) (ty : Ty) : HasType tm ty → infer tm = some ty := by
  intros h
  induction h with
  | int | true | false => rfl
  | add _ _ ih₁ ih₂
  | equal _ _ ih₁ ih₂ =>
      simp [infer]
      split
      . case h_1 => rfl
      . case h_2 hf => exact False.elim (hf ih₁ ih₂)
  | @ifThenElse _ _ _ ty h₁ h₂ h₃ ih₁ ih₂ ih₃ =>
      simp [infer]
      split
      .case h_1 _ he₂ _ => rw [← ih₂]; exact he₂.symm
      .case h_2 _ he₂ _ => rw [← ih₂]; exact he₂.symm
      .case h_3 hf₁ hf₂ =>
          cases ty with
          | int => exact False.elim (hf₁ ih₁ ih₂ ih₃)
          | bool => exact False.elim (hf₂ ih₁ ih₂ ih₃)

theorem infer_implies_has_type (tm : Tm) (ty : Ty) : (infer tm = some ty) → HasType tm ty := by
  intros h
  induction tm generalizing ty with
  | int =>
      cases ty with
      | int => exact HasType.int
      | bool => simp [infer] at h
  | add _ _ ih₁ ih₂ =>
      simp [infer] at h
      split at h
      . case h_1 h₁ h₂ =>
          injection h with hty
          rw [← hty]
          exact HasType.add (ih₁ _ h₁) (ih₂ _ h₂)
      . case h_2 =>
          nomatch h
  | equal _ _ ih₁ ih₂ =>
      simp [infer] at h
      split at h
      . case h_1 h₁ h₂ =>
          injection h with hty
          rw [← hty]
          exact HasType.equal (ih₁ _ h₁) (ih₂ _ h₂)
      . case h_2 =>
          nomatch h
  | true =>
      cases ty with
      | int => simp [infer] at h
      | bool => exact HasType.true
  | false =>
      cases ty with
      | int => simp [infer] at h
      | bool => exact HasType.false
  | ifThenElse  _ _ _ ih₁ ih₂ ih₃ =>
      simp [infer] at h
      split at h
      . case h_1 h₁ h₂ h₃ =>
          injection h with hty
          rw [← hty]
          exact HasType.ifThenElse (ih₁ _ h₁) (ih₂ _ h₂) (ih₃ _ h₃)
      . case h_2 h₁ h₂ h₃ =>
          injection h with hty
          rw [← hty]
          exact HasType.ifThenElse (ih₁ _ h₁) (ih₂ _ h₂) (ih₃ _ h₃)
      . case h_3 =>
          nomatch h

theorem infer_correct (tm : Tm) (ty : Ty) : (infer tm = some ty) = HasType tm ty :=
  Eq.propIntro (infer_implies_has_type tm ty) (has_type_implies_infer tm ty)

-- We can actually use the grind tactic here, but it’s not very illuminating
theorem check_correct' (tm : Tm) (ty : Ty) : (check tm ty = true) = HasType tm ty := by
  fun_induction check <;> grind [HasType]

-- theorem infer_correct' (tm : Tm) (ty : Ty) : (infer tm = some ty) = HasType tm ty := by
--   fun_induction infer <;> grind [HasType]
