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
   ;; IS IN FACT NOT NEEDED ?
   ;; We could just return purely the index into this array while decoding.
   ;; We assume the user provided symbols are ordered by decreasing frequency
   ;; and lexicographically when they have the same frequency.
   ;; maybe needed in fact
   (canon-order-symbols
    :documentation "Canonically ordered indices of symbols to encode.
Used when decoding messages."
    :accessor canon-order-symbols :initarg canon-order-symbols
    :initform nil)))

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
  "Decode an array of bits containing the symbols in encoded bit format.
Return an array of indices into the symbols dictionary.
bits-array: Array of bits constituting an encoded message. eg. #*110110100..."
  ;; We took the method from https://stackoverflow.com/a/29579194/10376845.
  ;; The method uses integers to represent the codes.
  ;; Since the encoded messages for each symbol in each length bin are
  ;; numerically consecutive in value, we can quickly deduce whether a
  ;; read message is present in a length bin or not just by its length, its
  ;; value, the value of the first encoded message in the length bin, and the
  ;; size of the length bin.
  (with-slots ((length-counts length-counts)
               (canon-order-symbols canon-order-symbols)) hfm
    (let (;; Decoded message represented as indices into the symbols dictionary.
          (decoded-indices (make-array 0 :fill-pointer 0 :element-type 'fixnum))
          ;; Current bit in the encoded message.
          (i-bit 0)
          ;; First encoded symbol value [integer] of a given length in each bin.
          ;; It is faster to reconstruct it along with the decoding rather
          ;; than looking it up.
          (first 0)
          ;; Position of the first encoded value of a given length for each bin.
          (pos-first 0)
          ;; Current read encoded message value, as an integer.
          (read-code 0)
          ;; Relative position of the encoded message in the current length bin.
          (pos-in-length-bin 0))
      (loop while (< i-bit (length bits-array)) do
        (psetf first 0 pos-first 0 read-code 0)
        (loop for count across length-counts do
          ;; Set the LSB of the current code.
          (incf read-code (aref bits-array i-bit))
          (incf i-bit)
          ;; When there are codes in the length bin.
          (when (/= 0 count)
            (setf pos-in-length-bin (- read-code first))
            ;; When the current code is present in the current length bin.
            (when (< pos-in-length-bin count)
              ;; Return the decoded symbol index.
              (vector-push-extend
               (aref canon-order-symbols (+ pos-first pos-in-length-bin))
               decoded-indices)
              (return))
            ;; Go to the next length bin.
            (incf pos-first count)
            (incf first count))
          (psetf first (ash first 1)
                 read-code (ash read-code 1))))
      decoded-indices)))

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

(defun lengths-to-length-counts (code-lengths)
  "Simply count the number of elements of each length. Return length-counts[N],
the number of symbols with length N.
code-lengths: The length of the encoded message for each symbol in the alphabet.
              Sorted in decreasing order. eg. #[3 3 3 2 1]."
  (let ((length-counts
          (make-array (1+ (aref code-lengths 0)) :element-type 'fixnum))
        ;; Position in code-lengths.
        (i-sym 0))
    (loop for len from (aref code-lengths 0) downto 0 do
      (loop while (and (< i-sym (length code-lengths))
                       (= len (aref code-lengths i-sym)))
            do (incf (aref length-counts len))
               (incf i-sym)))
    length-counts))
