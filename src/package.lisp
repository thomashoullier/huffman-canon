(defpackage :huffman-canon
  (:documentation "Canonical Huffman encoder/decoder.")
  (:use :cl)
  (:export #:make-huffman-canon-from-code-lengths
           #:encode
           #:decode))
