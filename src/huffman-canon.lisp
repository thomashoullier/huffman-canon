;;;; Canonical Huffman encoder/decoder class definition.
(in-package :huffman-canon)

(defclass huffman-canon ()
  ((length-counts
    :documentation "Number of codes of each code-length. The first element is
the number of codes of length 1, then of length 2 etc.. up to the maximum code
length. Used when decoding canonical codes."
    :accessor length-counts :initarg :length-counts)
   (encoded-dictionary
    :documentation "Encoded message for each symbol in the alphabet. Used when
encoding messages."
    :accessor encoded-dictionary :initarg :encoded-dictionary)))

;;; Constructors.
(defun make-huffman-canon-from-code-lengths (code-lengths)
  "Build a huffman-canon instance from a set of code lengths.
code-lengths: (l0 l1 ... ln-1) with li the length in bits of each encoded
element of the alphabet. Sorted in increasing order. The index of the lengths
in code-lengths will be used in lieu of symbol in encoding/decoding messages."
  ;; Checking the user input.
  (let ((last-length (aref code-lengths 0)))
    ;; Increasing order of code lengths.
    (loop for length across code-lengths do
      (when (< length last-length)
        (error "code-lengths must be sorted in increasing order."))
      (setf last-length length))
    ;; No code of zero length (would technically work but avoids problems).
    (when (<= (aref code-lengths 0) 0)
      (error "code-lengths must be strictly above zero.")))
  (let ((encoded-dictionary) (length-counts))
    (multiple-value-setq (encoded-dictionary length-counts)
      (encoded-dictionary-from-lengths code-lengths))
    ;; We need to drop the bin of zero length in length-counts,
    ;; it is useless for decoding.
    (setf length-counts (subseq length-counts 1))
    (make-instance 'huffman-canon
                   :length-counts length-counts
                   :encoded-dictionary encoded-dictionary)))
