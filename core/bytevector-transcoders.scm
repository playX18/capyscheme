
(library (core bytevector-transcoders)

  (export string->utf8 utf8->string
          string->utf16 utf16->string)

  (import (core intrinsics)
          (core optargs)
          (core io)
          (core primitives)
          (core bytevectors))) ;[end]
