# Examples

Run examples with a built Capy executable and the repository libraries on the
load path. For example:

```sh
CAPY_LOAD_PATH=./lib stage-0/capy -L lib -s examples/class-mop.scm
CAPY_LOAD_PATH=./lib stage-0/capy -L lib -s examples/class-redefinition.scm
```

`class-mop.scm` demonstrates Scheme classes with slot options including
Gauche-style initform aliases, Gauche-style built-in class bindings for the
current runtime class table,
Gauche-style generic coercion helpers, Gauche-style `make`/`initialize`,
generated accessor generics, generic procedure wrappers, specialized methods,
`next-method`, `:locked`, generic sealing, optional/keyword method arguments,
Gauche-style generic application, `describe`, `ref` over sequences, hash
tables, and boxes, and universal `~` helpers, the `define-class` `#:applicable`
option for closure-wrapper invocable instances,
custom Scheme slot hooks, class relationship predicates, method applicability
checks, class slot lookup, slot convenience helpers, class-explicit slot
existence checks, raw
indexed instance slot operations, slot-definition option introspection including
Gauche accessor option names, class, generic, method, next-method, and
slot-accessor option helpers, accessor-based slot operations including
accessor-driven slot initialization, class-explicit slot operations, and class
slot/accessor introspection.

`class-redefinition.scm` demonstrates replacing a Scheme class shape,
`class-of` versus `current-class-of` during lazy stale-instance touch, explicit
`touch-instance!`, changed direct superclasses, direct-subclass metadata
updates, and Gauche-style `define-class` replacement of an existing class
binding, including per-class sealing and malleability flags.
