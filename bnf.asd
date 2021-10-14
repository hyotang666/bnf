; vim: ft=lisp et
(in-package :asdf)
(defsystem "bnf"
  :version "0.1.1"
  :author "SATO Shinichi"
  :license "MIT"
  :source-control (:git "git@github.com:hyotang666/bnf")
  :bug-tracker "https://github.com/hyotang666/bnf/issues"
  :description "BNF example generator. Useful for testing parser."
  :depends-on
  (
   "uiop"       ; Utilities implicily depends on via asdf.
   "trestrul"   ; Utilities for tree structured list.
   )
  :components
  ((:file "bnf")))

(defmethod component-depends-on ((o test-op) (c (eql (find-system "bnf"))))
  (append (call-next-method)'((test-op "bnf.test"))))
