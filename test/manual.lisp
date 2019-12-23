;;;; Manual tests for huffman-canon.
(require :huffman-canon)

;;; RFC1951 example
(defparameter *lengths* #(2 3 3 3 3 3 4 4))
(defparameter *hfm*
  (huffman-canon:make-huffman-canon-from-code-lengths *lengths*))

(format t "~&~A~%" (huffman-canon:encode *hfm* #(0 1 2 3)))
(format t "~&~A~%" (huffman-canon:decode *hfm* #*00010011100))
