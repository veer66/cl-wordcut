(defpackage :cl-wordcut
  (:use :cl)
  (:export #:dict-seek
	   #:dict-pointer
	   #:dict
	   #:update
	   #:s
	   #:l
	   #:unk
	   #:chunk
	   #:offset
	   #:create-pointers-updater
	   #:edge
	   #:etype
	   #:create-edges-builder
	   #:is-better-than
	   #:best-edge
	   #:build-dag
	   #:basic-update-dag
	   #:dag-to-list
	   #:proto-dag-to-list
	   #:create-basic-wordcut
	   #:load-dict-from-bundle
	   #:space-info
	   #:update
	   #:update-lookahead
	   #:is-final))
