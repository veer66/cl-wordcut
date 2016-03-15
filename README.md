# cl-wordcut

cl-wordcut is a word segmentation tool for ASEAN languages written in Common Lisp.


## Example

### Thai
```commonlisp
(require 'cl-wordcut)
(defvar *dict* (cl-wordcut:load-dict-from-bundle "tdict-std.txt"))
(defvar *wordcut* (cl-wordcut:create-basic-wordcut *dict*))
(funcall *wordcut* "กากาม")
```
