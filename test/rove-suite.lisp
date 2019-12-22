;;;; Rove test suite for huffman-canon.
(in-package :huffman-canon/test)

;;; RFC1951 example
(deftest RFC1951
  (let ((code-lengths #(2 3 3 3 3 3 4 4))
        (hfm)
        ;; Canonically ordered encoded dictionary for the symbols to encode.
        (valid-dictionary '(#*00 #*010 #*011 #*100 #*101 #*110
                            #*1110 #*1111))
        (index-array-to-encode (make-array 1 :element-type 'fixnum)))
    (setf hfm (make-huffman-canon-from-code-lengths code-lengths))
    (pass "huffman-canon constructed from code lengths.")
    (testing "Encoded dictionary."
      (loop for valid-code in valid-dictionary
            for i from 0 do
              (setf (aref index-array-to-encode 0) i)
              ;; bit-vectors are compared element by element with #'equal.
              (ok (equal valid-code (encode hfm index-array-to-encode))
                  (format nil "~A" valid-code))))
    (testing "Identity on random data."

      )))

;;; Alphabet of one element.
