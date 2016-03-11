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

(defun default-do-pointers ()
  (cl-wordcut:create-pointers-updater
   'cl-wordcut:dict-pointer
   (create-dict)))

(test update-pointers
      (let* ((do-update-pointers (default-do-pointers))
	     (text "กา")
	     (pointers (funcall do-update-pointers 0 text nil))
	     (pointers (funcall do-update-pointers 1 text pointers)))
	(is (eq 1 (length pointers)))))

(test update-pointers-not-found
      (let* ((do-update-pointers (default-do-pointers))
	     (text "ยา")
	     (pointers (funcall do-update-pointers 0 text nil)))
	(is (eq 0 (length pointers)))))
