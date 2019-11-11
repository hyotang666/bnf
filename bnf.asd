; vim: ft=lisp et
(in-package :asdf)
(defsystem "bnf"
  :version "0.0.0"
  :depends-on
  (
   "trestrul" ; Utilities for tree structured list.
   )
  :components
  ((:file "bnf")))

(defmethod component-depends-on ((o test-op) (c (eql (find-system "bnf"))))
  (append (call-next-method)'((test-op "bnf.test"))))
