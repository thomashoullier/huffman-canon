;;;; Canonical Huffman encoder/decoder class definition.
(in-package :huffman-canon)

(defstruct decoding-state
  ;; Structure used to save a decoding state, to decode a message
  ;; chopped up in multiple parts with multiple calls to the decoder.
  ;; The current length bin we are in during the decoding.
  (cur-length 1)
  ;; First encoded integer symbol value in the current length bin.
  (first 0)
  ;; Index of the first encoded symbol in the dictionary for each given length.
  (pos-first 0)
  ;; The currently decoded value, represented as an integer.
  (read-code 0))

(defun decoding-state-reset (decode-state)
  "Reinitialize the struct decoding-state."
  (psetf (decoding-state-first decode-state) 0
         (decoding-state-pos-first decode-state) 0
         (decoding-state-read-code decode-state) 0
         (decoding-state-cur-length decode-state) 1))

(defclass huffman-canon ()
  ((length-counts
    :documentation "Number of codes of each code-length. The first element is
the number of codes of length 1, then of length 2 etc.. up to the maximum code
length. Used when decoding canonical codes."
    :accessor length-counts :initarg :length-counts)
   (encoded-dictionary
    :documentation "Encoded message for each symbol in the alphabet. Used when
encoding messages."
    :accessor encoded-dictionary :initarg :encoded-dictionary)
   (last-decode-state
    :documentation "Saved decoding state from the last interrupted decoding."
    :accessor last-decode-state
    :initform (make-decoding-state))))

;;; Constructors.
(defun make-huffman (code-lengths)
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
