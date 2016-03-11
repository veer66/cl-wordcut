(in-package :cl-wordcut)

(defun dict-seek (dict policy l r offset ch)
  (macrolet ((inc (i) `(+ ,i 1))
  	     (dec (i) `(- ,i 1)))
    (labels ((recur (l r ans)
  	       (if (<= l r)
  		   (let* ((m (floor (+ l r) 2))
  			  (w (car (elt dict m)))
  			  (wlen (length w)))
  		     (if (<= wlen offset)
  			 (recur (inc m) r ans)
  			 (let ((ch-w (char w offset)))
  			   (cond ((char< ch-w ch) (recur (inc m) r ans))
  				 ((char> ch-w ch) (recur l (dec m)  ans))
  				 ((eq policy :LEFT) (recur l (dec m) m))
  				 ((eq policy :RIGHT) (recur (inc m) r m))))))
  		   ans)))
      (recur l r nil))))

(defclass dict-pointer ()
  ((s :accessor s :initarg :s :initform 0)
   (l :accessor l :initarg :l :initform 0)
   (r :accessor r :initarg :r :initform 0)
   (offset :accessor offset :initarg :offset :initform 0)
   (is-final :accessor is-final :initarg :is-final :initform nil)
   (dict :accessor dict :initarg :dict :initform nil)))

(defgeneric update (pointer ch))
(defmethod update ((pointer dict-pointer) (ch character))
  (let* ((p pointer)
	 (offset (offset p))
	 (dict (dict p))
	 (l (dict-seek dict :LEFT (l p) (r p) offset ch)))
    (when l
      (let* ((r (dict-seek (dict p) :RIGHT l (r p) offset ch))
	     (w (car (elt (dict pointer) l)))
	     (w-len (length w)))
	(make-instance 'dict-pointer :s (s p) :l l :r r
		       :offset (+ (offset pointer) 1)
		       :dict dict
		       :is-final (eq w-len (+ 1 offset)))))))

(defun create-pointers-updater (pointer-class dict)
  (lambda (i text _pointers)
    (let ((ch (char text i))
	  (pointers (cons (make-instance pointer-class
					 :s i
					 :r (- (length dict) 1)
					 :dict dict)
			  _pointers)))

      (delete nil
	      (mapcar #'(lambda (pointer) (update pointer ch))
		      pointers)))))

(defclass edge ()
  ((s :accessor s :initarg :s :initform 0)
   (unk :accessor unk :initarg :unk :initform 0)
   (chunk :accessor chunk :initarg :chunk :initform 0)
   (etype :accessor etype :initarg :etype :initform :INIT)))

(defun create-edges-builder (edge-class)
  (lambda (dag pointers)
      (labels ((build (pointer)
		 (let* ((s (s pointer))
			(src (elt dag s)))
		   (make-instance edge-class
				  :s s
				  :unk (unk src)
				  :chunk (+ (chunk src) 1)))))
	(mapcar #'build pointers))))

(defgeneric is-better-than (o1 o2))
(defmethod is-better-than ((o1 edge) (o2 edge))
  (labels ((check (attrs)
	     (if (null attrs)
		 nil
		 (let* ((attr (car attrs))
			(v1 (slot-value o1 attr))
			(v2 (slot-value o2 attr)))
		   (cond
		     ((< v1 v2) t)
		     ((> v1 v2) nil)
		     (t (check (cdr attrs)))))))) 
    (check '(unk chunk))))

(defun best-edge (all-edges)
  (labels ((find-best (edges best)
   	     (if (null edges)
		 best
   		 (if (null best)
  		     (find-best (cdr edges) (car edges))
  		     (let ((edge (car edges)))
  		       (if (is-better-than edge best)
  			   (find-best (cdr edges) edge)
  			   (find-best (cdr edges) best)))))))
  (find-best all-edges nil)))
