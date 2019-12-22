;;;; Computing a canonical encoded dictionary from code lengths, with the method
;;;; from RFC1951.
(in-package :huffman-canon)

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

(defun smallest-code-each-length-bin (length-counts)
  "Compute the smallest code value for each length bin. Return array[N] the
smallest integer code value for the length bin N."
  (let ((smallest-code-per-length
          (make-array (length length-counts) :element-type 'fixnum))
        (code 0))
    (loop for len from 1 below (length length-counts) do
      (setf (aref smallest-code-per-length len)
            (setf code (ash (+ code (aref length-counts (1- len))) 1))))
    smallest-code-per-length))

(defun integer-codes (code-lengths smallest-code-per-length)
  "Compute integer values of canonical codes.
code-lengths: The code length of each encoded symbol.
smallest-code-per-length: The smallest integer code value for each length bin.
                          Is modified in place while running the function.
Output the integer values of the canonically ordered codes."
  (let ((int-codes (make-array (length code-lengths) :element-type 'fixnum)))
    (loop for len across code-lengths
          for i-sym from 0 do
            (when (/= len 0)
              (setf (aref int-codes i-sym) (aref smallest-code-per-length len))
              (incf (aref smallest-code-per-length len))))
    int-codes))

(defun integer->bit-vector (integer)
  "Create a bit-vector from a positive integer."
  ;; This is taken from https://www.lispforum.com/viewtopic.php?f=2&t=1205
  (labels ((integer->bit-list (int &optional accum)
             (cond ((> int 0)
                    (multiple-value-bind (i r) (truncate int 2)
                      (integer->bit-list i (push r accum))))
                   ((null accum) (push 0 accum))
                   (t accum))))
    (coerce (integer->bit-list integer) 'bit-vector)))

(defun integer-to-bitarray-codes (int-codes code-lengths)
  "Convert the integer codes to bit-array format.
int-codes: Integer code values.
code-lengths: The code length of each encoded symbol."
  (let ((encoded-dictionary (make-array (length code-lengths)
                                        :element-type 'bit-vector
                                        :initial-element #*0))
        (temp-code #*0)
        ;; Number of zeroes to put in front of the encoded message.
        (n-zeroes 0))
    (loop for len across code-lengths
          for int-code across int-codes
          for i-sym from 0 do
            (setf (aref encoded-dictionary i-sym)
                  (make-array len :element-type 'bit))
            (setf temp-code (integer->bit-vector int-code))
            ;; Pad the bit-vector with appropriate number of zeroes and recopy
            ;; the bits of the converted integer.
            (setf n-zeroes (- len (length temp-code)))
            (loop for i-bit from 0 below len do
              (setf (aref (aref encoded-dictionary i-sym) i-bit)
                    (if (< i-bit n-zeroes)
                        0
                        (aref temp-code (- i-bit n-zeroes))))))
    encoded-dictionary))

(defun encoded-dictionary-from-lengths (code-lengths)
  "Compute the canonical encoded dictionary from the code lengths of the symbols
in the dictionary.
code-lengths: The length of each encoded symbol in the dictionary.
              Sorted in increasing order. eg. #[2 3 3 3 3 3 4 4].
Output:
  * The encoded dictionary.
  * The length-counts array (including the length bin 0)."
  ;; We follow closely the method outlined in RFC1951 for ease of
  ;; understanding.
  (let (;; The number of codes present in each length bin.
        (length-counts)
        ;; Smallest integer code value for each length bin.
        (smallest-code-per-length)
        ;; Integer values of the encoded symbols.
        (int-codes))
    (setf length-counts (lengths-to-length-counts (reverse code-lengths)))
    (setf smallest-code-per-length
          (smallest-code-each-length-bin length-counts))
    (setf int-codes (integer-codes code-lengths smallest-code-per-length))
    (values (integer-to-bitarray-codes int-codes code-lengths)
            length-counts)))
