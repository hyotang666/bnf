; vim: ft=lisp et
(in-package :asdf)
(defsystem :bnf.test
  :version "0.0.2"
  :depends-on
  (:jingoh "bnf")
  :components
  ((:file "bnf"))
  :perform
  (test-op (o c) (symbol-call :jingoh :examine :bnf)))
