(lambda (return1)
  (fix ([proc2 (lambda (fac return3 n)
      (letk
           ((cont r4 (rv5)
             (letk
                ((cont kcons6 ()
                  (continue return3 1)))
              (letk
                 ((cont kalt7 ()
                   (letk
                      ((cont after_call8 (res9)
                        (let [rv10 (#%* n res9)]
                        (continue return3 rv10))))
                    (let [atom13 (#%- n 1)]
                    ((global fac) after_call8 atom13)))))
               (cond rv5 => kcons6 | kalt7)))))
         (let [atom14 (#%= n 0)]
         (continue r4 atom14))))])
    (let [rv15 (#%define (global fac) proc2)]
      (continue return1 rv15))))