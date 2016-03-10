(in-package :cl-wordcut-test)

(def-suite main :description "main tests")

(in-suite main)

(defun create-dict ()
  (mapcar #'list (list "กา" "ขา" "ขาม" "อม")))

(test dict-seek-basic
  (is (eq 1
	  (cl-wordcut:dict-seek (create-dict) :LEFT 0 3 0 #\ข))))

(test dict-seek-basic-offset-1-right
  (is (eq 2
	  (cl-wordcut:dict-seek (create-dict) :RIGHT 1 3 1 #\า))))

(test dict-pointer-update-basic
      (let ((new-p (cl-wordcut:update (make-instance 'cl-wordcut:dict-pointer
						     :r 3 :dict (create-dict))
				      #\ข)))
	(is (eq 1
		(cl-wordcut:l new-p)))))
