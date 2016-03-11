(defpackage :cl-wordcut
  (:use :cl)
  (:export #:dict-seek
	   #:dict-pointer
	   #:update
	   #:s
	   #:l
	   #:unk
	   #:chunk
	   #:offset
	   #:create-pointers-updater
	   #:edge
	   #:create-edges-builder
	   #:is-better-than
	   #:best-edge))
