(in-package :cl-wordcut-test)

(def-suite main :description "main tests")

(in-suite main)

(test titi-tutu
      (is (string= "@@@" (wordcut:titi))))
