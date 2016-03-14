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
	(is (and (eq 1 (length pointers))
		 (eq 2 (cl-wordcut:offset (car pointers)))))))

(test update-pointers-not-found
      (let* ((do-update-pointers (default-do-pointers))
	     (text "ยา")
	     (pointers (funcall do-update-pointers 0 text nil)))
	(is (eq 0 (length pointers)))))

(test build-candidate-edges
      (let* ((dag (vector (make-instance 'cl-wordcut:edge)
			  nil))
	     (pointers (list (make-instance 'cl-wordcut:dict-pointer
					    :is-final t
					    :offset 1)))
	     (build-edges (cl-wordcut:create-edges-builder 'cl-wordcut:edge))
	     (edges (funcall build-edges dag pointers)))
	(is (and (eq 1 (length edges))
		 (eq 0 (cl-wordcut:s (car edges)))))))
					
(test is-better-than
      (let ((p0 (make-instance 'cl-wordcut:edge
			       :unk 1
			       :chunk 5))
	    (p1 (make-instance 'cl-wordcut:edge
			       :unk 2
			       :chunk 1)))
	(is (cl-wordcut:is-better-than p0 p1))))

(test is-better-than-eq
      (let ((p0 (make-instance 'cl-wordcut:edge
			       :unk 1
			       :chunk 1))
	    (p1 (make-instance 'cl-wordcut:edge
			       :unk 1
			       :chunk 1)))
	(is (not (cl-wordcut:is-better-than p0 p1)))))

(test best-edge
      (is (eq 1
	      (cl-wordcut:unk
	       (cl-wordcut:best-edge
		(list (make-instance 'cl-wordcut:edge
				     :unk 2
				     :chunk 1)
		      (make-instance 'cl-wordcut:edge
				     :unk 1
				     :chunk 1)))))))

(test build-dag
      (let* ((dict (create-dict))
	     (build-edges (cl-wordcut:create-edges-builder
			   'cl-wordcut:edge))
	     (update-pointers (cl-wordcut:create-pointers-updater
			       'cl-wordcut:dict-pointer
			       dict))
	     (dag (cl-wordcut:build-dag
		   "ขามกา"
		   dict
		   build-edges
		   update-pointers
		   #'cl-wordcut:basic-update-dag)))
	(is (eq 6 (length dag)))
	(is (eq :INIT (cl-wordcut:etype (elt dag 0))))
	(is (eq :INIT (cl-wordcut:etype (elt dag 0))))
	(is (eq :UNK  (cl-wordcut:etype (elt dag 1))))
	(is (eq :DICT  (cl-wordcut:etype (elt dag 2))))
	(is (eq 0 (cl-wordcut:s (elt dag 2))))
	(is (eq 3 (cl-wordcut:s (elt dag 5))))))

(test dag-to-list
      (let ((dag (vector (make-instance 'cl-wordcut:edge)
			 (make-instance 'cl-wordcut:edge
					:s 0
					:etype :UNK
					:unk 1
					:chunk 1)
			 (make-instance 'cl-wordcut:edge
					:s 0
					:etype :DICT
					:unk 0
					:chunk 1)
			 (make-instance 'cl-wordcut:edge
					:s 0
					:etype :DICT
					:unk 0
					:chunk 1)
			 (make-instance 'cl-wordcut:edge
					:s 3
					:etype :UNK
					:unk 1
					:chunk 2)
			 (make-instance 'cl-wordcut:edge
					:s 3
					:etype :DICT
					:unk 0
					:chunk 2))))
	(is (equal (list "ขาม" "กา")
		   (cl-wordcut:dag-to-list dag "ขามกา")))))

(test wordcut-basic
      (let ((wordcut
	      (cl-wordcut:create-basic-wordcut (create-dict))))
	(is (equal (list "ขาม" "กา")
		   (funcall wordcut "ขามกา")))))
