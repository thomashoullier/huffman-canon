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
(defparameter *huffman* (huffman-canon:make-huffman
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
;; Let's say you initialize a Huffman encoding from scratch, ie you only have
;; character frequencies of occurence and want to generate a series of code
;; lengths defining the encoding.
(defparameter *chars* #(#\A #\B #\C #\D #\E #\F #\G #\H))
(defparameter *frequencies* #(10 10 10 10 10 15 5 5))
;; You can use the boundary package-merge implementation for that:
;; The frequencies need to be sorted in increasing order.
(defparameter *joint* (make-array 0 :fill-pointer 0))
(loop for char across *chars*
      for freq across *frequencies* do
        (vector-push-extend (list freq char) *joint*))
;; => #((10 #\A) (10 #\B) (10 #\C) (10 #\D) (10 #\E) (50 #\F) (5 #\G) (5 #\H))
(setf *joint* (stable-sort *joint* #'< :key #'car))
;; => #((5 #\G) (5 #\H) (10 #\A) (10 #\B) (10 #\C) (10 #\D) (10 #\E) (50 #\F))
(defparameter *length-limit* 4)
(defparameter *code-lengths*
  (bpm:a-to-l (bpm:encode-limited (sort *frequencies* #'<) *length-limit*)))
;; => #(4 4 3 3 3 3 3 2)

;; You can then assign these lengths to your characters.
(loop for length across *code-lengths*
      for cell across *joint* do
        (setf (car cell) length))
;; => #((4 #\G) (4 #\H) (3 #\A) (3 #\B) (3 #\C) (3 #\D) (3 #\E) (2 #\F))

;; Once you're there, you can continue by instantiating your huffman
;; encoder/decoder from the code lengths as shown above.

;;; Decoding a message in several parts.
(defparameter *code-lengths* #(2 3 3 3 3 3 4 4))
(defparameter *huffman* (huffman-canon:make-huffman *code-lengths*))
;; Dictionary is: #*00 #*010 #*011 #*100 #*101 #*110 #*1110 #*1111

;; Now let's say you want to decode a message chopped in parts, maybe because
;; the message is too long to fit in memory.
;; Complete message: #*000111110000100111111
;; It decodes to: #*00|011|1110|00|010|011|1111
;;                  0  2   6    0  1   2   7
;; Let us cut it in multiple parts, we do not know where the boundaries between
;; symbols are a priori:
;; #*0001111 .. 1000010 .. 0111111
(defparameter *message1* #*0001111)
(defparameter *message2* #*1000010)
(defparameter *message3* #*0111111)

;; We can decode the messages by calling the decoder sequentially on them.
(defparameter *decoded1* (huffman-canon:decode *huffman* *message1*))
;; => #(0 2)
(defparameter *decoded2* (huffman-canon:decode *huffman* *message2*))
;; => #(6 0 1)
(defparameter *decoded3* (huffman-canon:decode *huffman* *message3*))
;; => #(2 7)
