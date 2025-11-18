(with-exception-handler (lambda (x) 0)
                                      (lambda () (error #f "bad")))