;;;; Canonical Huffman encoder/decoder class definition.

(in-package :huffman-canon)

(defclass huffman-canon ()
  ((length-counts
    :documentation "Number of codes of each code-length. The first element is
the number of codes of length 1, then of length 2 etc.. up to the maximum code
length. Used when decoding canonical codes."
    :accessor length-counts :initarg length-counts
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

(defmethod decode ((huffman-canon hfm) str-in-enc str-out-dec)
  "Decode a stream of bits into a stream of symbol positions from the alphabet.
hfm:
str-in-enc:
str-out-dec:"

  )

(defmethod encode ((huffman-canon hfm) str-in str-out-enc)
  "Encode a stream of symbol positions in the alphabet into a stream of bits.
hfm:
str-in:
str-out-enc:"

  )
