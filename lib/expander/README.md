# portable-syntax-scopes

Portable Syntax Scopes. Based on Racket's expander and its minimized version from [expander](https://github.com/mflatt/expander).

## Requirements

To run this expander you need R5RS Scheme system with the following additions: hashtables and record types. You do not need
any macro facilities in this Scheme subset. Portable hashtable implementation is provided in compat.scm which simply
uses vector as backing storage.