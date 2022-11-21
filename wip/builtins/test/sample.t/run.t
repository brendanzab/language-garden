Test main.txt
  $ cat main.txt | builtins elab
  def Type : builtin.Type := builtin.Type;
  def Bool : Type := builtin.Bool;
  def true : Bool := builtin.true;
  def false : Bool := builtin.false;
  def I32 : Type := builtin.I32;
  def I64 : Type := builtin.I64;
  def F64 : Type := builtin.F64;
  def one : I32 := 1;
