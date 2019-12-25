;;;; Rove test suite for huffman-canon.
(require :array-operations)
(in-package :huffman-canon/test)

;;; RFC1951 example
(deftest RFC1951
  (let* ((code-lengths #(2 3 3 3 3 3 4 4))
         (n-chars (length code-lengths))
         (hfm)
         ;; Canonically ordered encoded dictionary for the symbols to encode.
         (valid-dictionary '(#*00 #*010 #*011 #*100 #*101 #*110
                             #*1110 #*1111))
         (index-array-to-encode (make-array 1 :element-type 'fixnum)))
    (setf hfm (make-huffman code-lengths))
    (pass "huffman-canon constructed from code lengths.")
    (testing "Encoded dictionary."
      (loop for valid-code in valid-dictionary
            for i from 0 do
              (setf (aref index-array-to-encode 0) i)
              ;; bit-vectors are compared element by element with #'equal.
              (ok (equal valid-code (encode hfm index-array-to-encode))
                  (format nil "~A" valid-code))))
    (testing "Identity on random data."
      (let* ((n-syms 1000)
             (random-characters (aops:generate (lambda () (random n-chars))
                                               (list n-syms)))
             (id-random-characters))
        (setf id-random-characters
              (decode hfm (encode hfm random-characters)))
        (ok (loop for char across id-random-characters
                  for valid-char across random-characters
                  always (= char valid-char))
            "Self-coherent.")))))

;;; Alphabet of one element.
(deftest one-element
  (let ((code-lengths #(1))
        (hfm))
    (setf hfm (make-huffman code-lengths))
    (pass "huffman-canon constructed from code lengths.")
    (ok (equal #*0 (encode hfm #(0))) (format nil "~A" #*0))))

;;; Errors - User input checking.
(deftest user-input-checking
  (testing "Code lengths in increasing order."
    (ok (signals (make-huffman #(1 2 2 3 2 5))) ""))
  (testing "No zero code length."
    (ok (signals (make-huffman #(0 1 2 3 3))) "")))

;;; Errors - Decoding
(deftest corrupted-data
  (let ((code-lengths #(2 3 3 3 3 3))
        ;; The dictionary for these code-lengths is:
        ;; #*00 #*010 #*011 #*100 #*101 #*110
        (hfm)
        (corrupted-message #*00100111)
        ;; This translates to #(0 3 *error*)
        )
    (setf hfm (make-huffman code-lengths))
    (pass "huffman-canon constructed.")
    ;; Let us give the decoder an encoded message which does
    ;; not correspond to anything in the dictionary.
    (ok (signals (decode hfm corrupted-message)) "Data corrupted.")))
