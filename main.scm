(define-library (clang)
(begin
    (load-native-extension "./target/x86_64-unknown-linux-gnu/release/libcapy_clang.so")

    (format #t "Clang extension loaded successfully.~%")

    (clang-parse "./wrapper.h")
))