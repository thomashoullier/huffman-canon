;;;; Canonical Huffman encoder/decoder class definition.

(in-package :huffman-canon)

(defclass huffman-canon ()
  ((length-counts
    :documentation "Number of codes of each code-length. The first element is
the number of codes of length 1, then of length 2 etc.. up to the maximum code
length. Used when decoding canonical codes."
    :accessor length-counts :initarg length-counts
    :initform nil)
   (encoded-dictionary
    :documentation "Encoded message for each symbol in the alphabet. Used when
encoding messages."
    :accessor encoded-dictionary :initarg encoded-dictionary
    :initform nil)
   ))

(defun make-huffman-canon-from-code-lengths (code-lengths)
  "Build a huffman-canon instance from a set of code lengths.
code-lengths: (l0 l1 ... ln-1) with li the length in bits of each encoded
element of the alphabet."

  )

(defun make-huffman-canon-from-frequencies (frequencies)
  "Build a huffman-canon instance from a set of symbol frequencies.
Uses the boundary package-merge algorithm to build the huffman codes.
frequencies: "

  )

(defmethod decode ((hfm huffman-canon) bits-array)
  "Decode an array of bits containing the symbols in encoded bit format."
  ;; We took the method from https://stackoverflow.com/a/29579194/10376845.

  )

(defmethod encode ((hfm huffman-canon) indices-array)
  "Encode an array of indices pointing to symbols to encode into an encoded
array of bits."
  (with-slots ((encoded-dictionary encoded-dictionary)) hfm
    (let ((encoded-bits (make-array 0 :fill-pointer 0 :element-type 'bit)))
      ;; We simply recopy the entries of the encoded dictionary.
      (loop for index across indices-array do
        (loop for bit-i across (aref encoded-dictionary index) do
          (vector-push-extend bit-i encoded-bits)))
      encoded-bits)))
