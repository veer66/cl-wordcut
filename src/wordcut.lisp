(in-package :wordcut)

;; (defun dict-seek (dict policy l r offset ch)
;;   (defun dict-seek-rec (l r ans)
;;     (if (<= l r)
;; 	(let* ((m (floor (+ l r) 2))
;; 	       (w (car (elt dict m)))
;; 	       (wlen (length w)))
;; 	  (if (<= wlen offset)
;; 	      (dict-seek-rec (+ 1 m) r ans)
;; 	      (let ((ch-w (char w offset)))
;; 		(cond ((char< ch-w ch) (dict-seek-rec (+ 1 m) r ans))
;; 		      ((char> ch-w ch) (dict-seek-rec l (- m 1) ans))
;; 		      ((eq policy 'LEFT) (dict-seek-rec l (- m 1) m))
;; 		      ((eq policy 'RIGHT) (dict-seek-rec (+ 1 m) m))))))
;; 	ans))
;;   (dict-seek-rec l r nil))

;; ;; (dict-seek (list (list "A") (list "B") (list "C")) 'LEFT
;; ;; 	   0 2 0 #\B)

;; (defclass dict-pointer ()
;;   ((s :accessor s :initarg :s :initform 0)
;;    (l :accessor l :initarg :l :initform 0)
;;    (r :accessor r :initarg :r :initform 0)
;;    (offset :accessor offset :initarg :offset :initform 0)
;;    (is-final :accessor is-final :initarg :is-final :initform nil)
;;    (dict :accessor dict :initarg dict)))

;; (defgeneric update (pointer ch))
;; (defmethod update ((pointer dict-pointer) (ch character))
;;   (let* ((p pointer)
;; 	 (offset (offset p))
;; 	 (l (dict-seek (dict p) 'LEFT (l p) (r p) offset ch)))
;;     (when l
;;       (let* ((r (dict-seek (dict p) 'RIGHT l (r p) offset ch))
;; 	     (w (car (elt (dict pointer) m)))
;; 	     (w-len (length w)))
;; 	(make-instance 'dict-pointer :s (s p) :l l :r r
;; 		       :offset (+ (offset pointer) 1)
;; 		       :is-final (eq w-len (+ 1 offset)))))))
				   				       
(defun titi () "@@@")
