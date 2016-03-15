# cl-wordcut

cl-wordcut is a word segmentation tool for ASEAN languages written in Common Lisp.


## Example

### Khmer (Cambodian)
```lisp
(require 'cl-wordcut)
(defvar *dict* (cl-wordcut:load-dict-from-bundle "khmerwords.txt"))
(defvar *wordcut* (cl-wordcut:create-basic-wordcut *dict*))
(funcall *wordcut* "ភាសាខ្មែរ")
```


### Lao
```lisp
(require 'cl-wordcut)
(defvar *dict* (cl-wordcut:load-dict-from-bundle "laowords.txt"))
(defvar *wordcut* (cl-wordcut:create-basic-wordcut *dict*))
(funcall *wordcut* "ພາສາລາວມີ")
```

### Thai
```lisp
(require 'cl-wordcut)
(defvar *dict* (cl-wordcut:load-dict-from-bundle "tdict-std.txt"))
(defvar *wordcut* (cl-wordcut:create-basic-wordcut *dict*))
(funcall *wordcut* "กากาม")
```
