;; A wrapper library for Clang API

#!r6rs 

(library (clang)
  (export)
  (import (core))
  (load-native-extension (string-append "libcapy_clang." %native-extension)))