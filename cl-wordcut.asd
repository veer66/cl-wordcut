#-asdf3.1 (error "cl-wordcut requires ASDF 3.1")
(defsystem "cl-wordcut"
  :description "Word segmentation tools for ASEAN languages written in Common Lisp"
    :author "Vee Satayamas"
    :license "LLGPL"
    :depends-on ("asdf")
    :in-order-to ((test-op (test-op "cl-wordcut/test")))
    :pathname "src"
    :components ((:file "packages")
		 (:file "wordcut" :depends-on ("packages"))))

(defsystem "cl-wordcut/test"
  :description "cl-wordcut test"
  :author "Vee Satayamas"
  :license "LLGPL"
  :depends-on ("cl-wordcut" "fiveam")
  :pathname "test"
  :components ((:file "packages")
	       (:file "main" :depends-on ("packages")))
  :perform (test-op (o s) (symbol-call :fiveam '#:run!)))
