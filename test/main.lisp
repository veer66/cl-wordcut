(in-package :cl-wordcut-test)

(def-suite main :description "main tests")

(in-suite main)

(defun create-dict ()
  (map 'vector #'list (vector "กา" "ขา" "ขาม" "อม")))

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
		   build-edges
		   update-pointers
		   #'cl-wordcut:basic-update-dag
		   'cl-wordcut:edge)))
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

(test load-dict-from-bundle
      (let ((dict (cl-wordcut:load-dict-from-bundle "tdict-std.txt")))
	(is (eq 15374
		(length dict)))))


(test seek-std-dict
      (let* ((dict (cl-wordcut:load-dict-from-bundle "tdict-std.txt")))
	(is (not (null (cl-wordcut:dict-seek
			dict :LEFT 0 (- (length dict) 1) 0 #\ม))))))

(test wordcut-with-std-dict
      (let* ((dict (cl-wordcut:load-dict-from-bundle "tdict-std.txt"))
	     (wordcut
	      (cl-wordcut:create-basic-wordcut dict)))
	(is (equal (list "ม้า" "ไก่" "เป็ด")
		   (funcall wordcut "ม้าไก่เป็ด")))))

(test space-info-start-non-space
      (let* ((space-info (make-instance 'cl-wordcut:space-info :s 10))
	     (space-info (cl-wordcut:update-lookahead space-info #\A #\B)))
	(is (null (cl-wordcut:is-final space-info)))
	(is (eq 11 (cl-wordcut:s space-info)))
	(is (eq 0 (cl-wordcut:offset space-info)))))

(test space-info-start-non-space-end
      (let* ((space-info (make-instance 'cl-wordcut:space-info :s 10))
	     (space-info (cl-wordcut:update-lookahead space-info #\A nil)))
	(is (null (cl-wordcut:is-final space-info)))
	(is (eq 11 (cl-wordcut:s space-info)))
	(is (eq 0 (cl-wordcut:offset space-info)))))

(test space-info-space-non-space
      (let* ((space-info (make-instance 'cl-wordcut:space-info :s 10))
	     (space-info (cl-wordcut:update-lookahead space-info #\Space #\A)))
	(is (eq 10 (cl-wordcut:s space-info)))
	(is (cl-wordcut:is-final space-info))
	(is (eq 1 (cl-wordcut:offset space-info)))))

(test space-info-space-space
      (let* ((space-info (make-instance 'cl-wordcut:space-info :s 10))
	     (space-info (cl-wordcut:update-lookahead space-info #\Space #\Space)))
	(is (eq 10 (cl-wordcut:s space-info)))
	(is (not (cl-wordcut:is-final space-info)))
	(is (eq 1 (cl-wordcut:offset space-info)))))



(test wordcut-latin-alphabet
      (let* ((dict (cl-wordcut:load-dict-from-bundle "tdict-std.txt"))
	     (wordcut
	      (cl-wordcut:create-basic-wordcut dict)))
	(is (equal (list "que" " " "sera" " " "sera")
		   (funcall wordcut "que sera sera")))))


(test pointer-khmer
      (let* ((dict (cl-wordcut:load-dict-from-bundle "khmerwords.txt"))
	     (p (make-instance 'cl-wordcut:dict-pointer
			       :r (- (length dict) 1)
			       :dict dict)))
	(is (cl-wordcut:update p #\KHMER_LETTER_PHO))
	(setq p (cl-wordcut:update p #\KHMER_LETTER_PHO))
	(is (cl-wordcut:update p #\KHMER_VOWEL_SIGN_AA))
	(setq p (cl-wordcut:update p #\KHMER_VOWEL_SIGN_AA))
	(is (cl-wordcut:update p #\KHMER_LETTER_SA))
	(setq p (cl-wordcut:update p #\KHMER_LETTER_SA))
	(is (cl-wordcut:update p #\KHMER_VOWEL_SIGN_AA))))


(test pointer-khmer
      (let* ((dict (cl-wordcut:load-dict-from-bundle "khmerwords.txt"))
	     (p (make-instance 'cl-wordcut:dict-pointer
			       :r (- (length dict) 1)
			       :dict dict)))
	(loop for i from 0 to 3 do
	     (let ((ch (char "ភាសា" i)))
	       (setq p (cl-wordcut:update p ch))
	       (is (not (null p)))))))
		


(test build-dag-khmer
      (let* ((dict (cl-wordcut:load-dict-from-bundle "khmerwords.txt"))
	     (build-edges (cl-wordcut:create-edges-builder
			   'cl-wordcut:edge))
	     (update-pointers (cl-wordcut:create-pointers-updater
			       'cl-wordcut:dict-pointer
			       dict))
	     (dag (cl-wordcut:build-dag
		   "ភាសា"
		   build-edges
		   update-pointers
		   #'cl-wordcut:basic-update-dag
		   'cl-wordcut:edge)))
	(is (eq 5 (length dag)))
	(is (eq :DICT
		(cl-wordcut:etype (elt dag 4))))
	(is (eq 0
		(cl-wordcut:unk (elt dag 4))))))


(test wordcut-khmer
      (let* ((dict (cl-wordcut:load-dict-from-bundle "khmerwords.txt"))
	     (wordcut
	      (cl-wordcut:create-basic-wordcut dict)))
	(is (equal (list "ភាសា" "ខ្មែរ")
		   (funcall wordcut "ភាសាខ្មែរ")))))
