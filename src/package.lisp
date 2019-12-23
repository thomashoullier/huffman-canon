(defpackage :huffman-canon
  (:documentation "Canonical Huffman encoder/decoder.")
  (:use :cl)
  (:export #:make-huffman
           #:encode
           #:decode))
