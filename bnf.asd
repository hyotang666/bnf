; vim: ft=lisp et
(in-package :asdf)
(defsystem "bnf"
  :depends-on
  ("endaira" "trestrul")
  :components
  ((:file "bnf")))

;; Perform method below is added by JINGOH.GENERATOR.
(defmethod perform ((o test-op) (c (eql (find-system "bnf"))))
  (test-system :bnf.test))
