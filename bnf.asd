; vim: ft=lisp et
(in-package :asdf)
(defsystem "bnf"
  :depends-on
  ("endaira")
  :components
  ((:file "bnf")))
