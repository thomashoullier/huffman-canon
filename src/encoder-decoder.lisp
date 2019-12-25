;;;; Encoding and decoding methods for the huffman-canon class.
(in-package :huffman-canon)

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
  (with-slots ((length-counts length-counts)) hfm
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
          (pos-in-length-bin 0)
          ;; Maximum length of a code in the dictionary.
          (max-length (length length-counts)))
      (loop while (< i-bit (length bits-array)) do
        (psetf first 0 pos-first 0 read-code 0)
        (loop for count across length-counts
              for cur-length from 1 do
                ;; Set the LSB of the current code.
                (incf read-code (aref bits-array i-bit))
                (incf i-bit)
                ;; When there are codes in the length bin.
                (when (/= 0 count)
                  (setf pos-in-length-bin (- read-code first))
                  ;; When the current code is present in the current length bin.
                  (when (< pos-in-length-bin count)
                    ;; Return the decoded symbol index.
                    (vector-push-extend (+ pos-first pos-in-length-bin)
                                        decoded-indices)
                    (return))
                  ;; Go to the next length bin.
                  (incf pos-first count)
                  (incf first count))
                (psetf first (ash first 1)
                       read-code (ash read-code 1))
                ;; If this point in the loop is reached at the last length bin,
                ;; then no symbol was found in the dictionary for the given
                ;; bit-vector.
                (when (>= cur-length max-length)
                  (error "No symbol found. Data may be corrupted."))))
      decoded-indices)))
