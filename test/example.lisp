;;;; Example of using huffman-canon.
(require :huffman-canon)
(require :bpm)

;;; Initializing the huffman encoder/decoder from code lengths.
;; Taking the example of RFC1951, you are given a series of characters to encode
;; and the corresponding code lengths:
(defparameter *chars* #(#\A #\B #\C #\D #\E #\F #\G #\H))
(defparameter *code-lengths* #(3 3 3 3 3 2 4 4))

;; First sort the code lengths in increasing order while keeping
;; the order of the characters in sync. A stable sort will keep
;; the original orders of symbols to encode in the same length bin.
(defparameter *joint* (make-array 0 :fill-pointer 0))
(loop for char across *chars*
      for length across *code-lengths* do
        (vector-push-extend (list length char) *joint*))
;; => #((3 #\A) (3 #\B) (3 #\C) (3 #\D) (3 #\E) (2 #\F) (4 #\G) (4 #\H))
(setf *joint* (stable-sort *joint* #'< :key #'car))
;; #((2 #\F) (3 #\A) (3 #\B) (3 #\C) (3 #\D) (3 #\E) (4 #\G) (4 #\H))

;; The huffman encoder/decoder speaks in terms of indices. The index 0 will
;; refer to F, 1 to A, 2 to B and so on...
;; As a huffman dictionary is usually represented only by the list
;; of code lengths (as specified in RFC1951), you need some sort of additional
;; convention to store the relation between symbol indices and symbols to
;; encode.

;; Now that we have our correspondance table between index and symbols,
;; we can generate the huffman encoder/decoder with sorted code lengths.
(defparameter *huffman* (huffman-canon:make-huffman-canon-from-code-lengths
                         (sort *code-lengths* #'<)))

;; We can print the whole encoded dictionary by encoding each symbol
;; individually:
(defparameter *symbols-to-encode* #(0))
(loop for i-sym from 0 below (length *joint*) do
  (setf (aref *symbols-to-encode* 0) i-sym)
  (format t "~&~A - ~A: ~A~%" i-sym (cadr (aref *joint* i-sym))
          (huffman-canon:encode *huffman* *symbols-to-encode*)))
;; 0 - F: #*00
;; 1 - A: #*010
;; 2 - B: #*011
;; 3 - C: #*100
;; 4 - D: #*101
;; 5 - E: #*110
;; 6 - G: #*1110
;; 7 - H: #*1111

;; A whole message is encoded in this way:
(setf *symbols-to-encode* #(7 0 3))
(defparameter *encoded-message*
  (huffman-canon:encode *huffman* *symbols-to-encode*))
;; => #*111100100

;; A whole message is decoded in this way:
(defparameter *decoded-message*
  (huffman-canon:decode *huffman* *encoded-message*))
;; => #(7 0 3)
;; You then need to convert these indices yourself into the corresponding
;; symbols.
(defparameter *decoded-message-chars* (make-array 0 :fill-pointer 0))
(loop for index across *decoded-message* do
  (vector-push-extend (cadr (aref *joint* index)) *decoded-message-chars*))
;; => #(#\H #\F #\C)

;;; Initializing a huffman encoding from character frequencies.
