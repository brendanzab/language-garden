Help: top-level
  $ executable --help=plain
  NAME
         items
  
  SYNOPSIS
         items COMMAND …
  
  COMMANDS
         compile-anf [OPTION]…
             Compile a module from standard input to A-normal form
  
         compile-llvm [--output-format=FORMAT] [OPTION]…
             Compile a module from standard input to LLVM IR
  
         compile-wat [--enable-tail-call] [OPTION]…
             Compile a module from standard input to WAT (WebAssembly Text
             Format)
  
  COMMON OPTIONS
         --help[=FMT] (default=auto)
             Show this help in format FMT. The value FMT must be one of auto,
             pager, groff or plain. With auto, the format is pager or plain
             whenever the TERM env var is dumb or undefined.
  
  EXIT STATUS
         items exits with:
  
         0   on success.
  
         123 on indiscriminate errors reported on standard error.
  
         124 on command line parsing errors.
  
         125 on unexpected internal errors (bugs).
  

Help: compile-anf
  $ executable compile-anf --help=plain
  NAME
         items-compile-anf - Compile a module from standard input to A-normal
         form
  
  SYNOPSIS
         items compile-anf [OPTION]…
  
  COMMON OPTIONS
         --help[=FMT] (default=auto)
             Show this help in format FMT. The value FMT must be one of auto,
             pager, groff or plain. With auto, the format is pager or plain
             whenever the TERM env var is dumb or undefined.
  
  EXIT STATUS
         items compile-anf exits with:
  
         0   on success.
  
         123 on indiscriminate errors reported on standard error.
  
         124 on command line parsing errors.
  
         125 on unexpected internal errors (bugs).
  
  SEE ALSO
         items(1)
  

Help: compile-llvm
  $ executable compile-llvm --help=plain
  NAME
         items-compile-llvm - Compile a module from standard input to LLVM IR
  
  SYNOPSIS
         items compile-llvm [--output-format=FORMAT] [OPTION]…
  
  OPTIONS
         --output-format=FORMAT (absent=ll)
             The output format to emit. The value of FORMAT must be either ll
             for LLVM IR, or dot for Graphviz DOT.
  
  COMMON OPTIONS
         --help[=FMT] (default=auto)
             Show this help in format FMT. The value FMT must be one of auto,
             pager, groff or plain. With auto, the format is pager or plain
             whenever the TERM env var is dumb or undefined.
  
  EXIT STATUS
         items compile-llvm exits with:
  
         0   on success.
  
         123 on indiscriminate errors reported on standard error.
  
         124 on command line parsing errors.
  
         125 on unexpected internal errors (bugs).
  
  SEE ALSO
         items(1)
  

Help: compile-wat
  $ executable compile-wat --help=plain
  NAME
         items-compile-wat - Compile a module from standard input to WAT
         (WebAssembly Text Format)
  
  SYNOPSIS
         items compile-wat [--enable-tail-call] [OPTION]…
  
  OPTIONS
         --enable-tail-call
             Output tail-calls in generated WebAssembly
  
  COMMON OPTIONS
         --help[=FMT] (default=auto)
             Show this help in format FMT. The value FMT must be one of auto,
             pager, groff or plain. With auto, the format is pager or plain
             whenever the TERM env var is dumb or undefined.
  
  EXIT STATUS
         items compile-wat exits with:
  
         0   on success.
  
         123 on indiscriminate errors reported on standard error.
  
         124 on command line parsing errors.
  
         125 on unexpected internal errors (bugs).
  
  SEE ALSO
         items(1)
  
