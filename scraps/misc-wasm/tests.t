Assert with exceptions:
  $ wasmtime --invoke main --wasm exceptions=y assert-throw.wat
  Error: failed to run main module `assert-throw.wat`
  
  Caused by:
      0: failed to invoke `main`
      1: thrown Wasm exception
  
  [1]

Assert with `unreachable` instruction:
  $ wasmtime --invoke main assert-unreachable.wat
  Error: failed to run main module `assert-unreachable.wat`
  
  Caused by:
      0: failed to invoke `main`
      1: error while executing at wasm backtrace:
      0:     0x39 - <unknown>!assert
      1:     0x4e - <unknown>!main
      2: wasm trap: wasm `unreachable` instruction executed
  
  [134]
