val test-fact : I32 :=
  fact(5);

fun fact(n : I32) : I32 :=
  if n = 0 then 1 else n * fact(n - 1);

fun ackermann(m : I32, n : I32) : I32 :=
  if m = 0 then
    n + 1
  else if n = 0 then
    ackermann(m - 1, 1)
  else
    ackermann(m - 1, ackermann(m, n - 1));

fun is-even(n : I32) : Bool :=
  if n = 0 then true else is-odd(n - 1);

fun is-odd(n : I32) : Bool :=
  if n = 0 then false else is-even(n - 1);
