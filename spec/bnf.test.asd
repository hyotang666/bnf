; vim: ft=lisp et
(in-package :asdf)
(defsystem :bnf.test
  :depends-on
  (:jingoh "bnf")
  :components
  ((:file "bnf"))
  :perform
  (test-op (o c) (symbol-call :jingoh :examine :bnf)))