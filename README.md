# Canonical Huffman encoder/decoder
`huffman-canon` is a canonical Huffman encoder/decoder for Common Lisp. It
converts `bit-vector` messages like `#*01001111100101010001000` into arrays of
indices into an alphabet, eg. `#(0 14 4 17 3 4)` and vice-versa.

See [1] if you don't see what I am talking about.

## Scope
`huffman-canon` is a Huffman encoder/decoder in a very strict sense. The
encoder/decoder is instantiated from an already made series of code lengths.
Series of code lengths are a typical way of representing canonical Huffman codes
(see [2]) and exchanging them between programs.

`huffman-canon` employs canonical-only codes. This is to make the implementation
as small as possible. As far as I am aware, non-canonical encoding is not used
in production. Managing arbitrary Huffman encodings is fun for research
but has no application justifying the headache-inducing ambiguities it 
introduces.

Code lengths are either found attached to already compressed data
(as in the DEFLATE data format [2]) or must be created from scratch from a set
of character frequencies of occurence. The task of creating series of code 
lengths from frequencies is offloaded to the user, or to our boundary
package-merge implementation for the creation of Huffman length-limited codes
[3].

No file or stream utilities are included. It is up to the user to chop its
messages into blocks that fit in memory and manage the writing of the output.

## Exported functions
### make-huffman
**make-huffman** *code-lengths* => *huffman-canon-instance*

**make-huffman** is the constructor for the class *huffman-canon*.
* *code-lengths*: An *array* of code lengths sorted in increasing order. Code
lengths of zero are not accepted. The indexing into *code-lengths* is used in
lieu of characters to designate individual symbols of the alphabet when encoding
and decoding messages. eg. `#(2 3 3 3 3 3 4 4)`.

As indices into *code-lengths* are used to represent characters to encode, you
will have doubtlessly noted that you actually need some sort of additional
convention to distinguish symbols that belong to the same length bin. The
usual convention is just to define some sort of lexicographical order on the
elements of the alphabet and stick to it. This is how it is done in [2] for
example. This way, in `#(2 3 3 3 3 3 4 4)`, the first character to encode with
length 3 will be the first in lexicographical order, the second will be second
in lexicographical order etc. When such a convention is clearly defined, you can
get away with describing your canonical Huffman encoding just with a series of
code lengths, one for each character to encode. The examples below illustrate
this point.

### encode
**encode** *huffman-canon-instance* *indices-array* => *encoded-bits*

**encode** converts an *array* of indices into *code-lengths* into an
encoded *bit-vector* message.
* *indices-array*: An *array* of indices into *code-lengths*.
eg. `#(3 1 0 6)`
* *encoded-bits*: A *bit-vector* containing the encoded message.
eg. `#*11010100111`.

### decode
**decode** *huffman-canon-instance* *bits-array* => *decoded-indices*

**decode** converts the encoded message *bits-array* into an *array* of decoded
indices into *code-lengths*, *decoded-indices*.
* *bits-array*: An encoded *bit-vector*. eg. `#*11010100111`.
* *decoded-indices*: An *array* of indices into *code-lengths* corresponding to
the decoded message. eg. `#(3 1 0 6)`.

## Usage
See the file `test/example.lisp` to run the code.
### Using with an already made series of code lengths

```common-lisp
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
```

### Generating code lengths from frequencies of occurence

```common-lisp
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
```

## Dependencies
* `huffman-canon`: None. [bpm](https://github.com/thomashoullier/bnd-pkg-merge)
is useful for creating code lengths from frequencies.
* `huffman-canon/test`:
  * [rove](https://github.com/fukamachi/rove)
  * [array-operations](https://github.com/bendudson/array-operations)

## Test
A simple rove test suite is written. To launch it:

```common-lisp
(asdf:test-system "huffman-canon")
```

## Issues
* No performance optimization was done. I'll probably put some type hinting
  at some point in the future as I start using the library.
* Decoding from a corrupted bit-vector, meaning that some sequence of bits
will possibly not be present in the Huffman codes, will in most cases land you
in the debugger. I have written no errors for this case yet.

## Licensing
See the license file. Citations are appreciated but not needed.

## References
1. https://en.wikipedia.org/wiki/Canonical\_Huffman\_code
2. https://www.w3.org/Graphics/PNG/RFC-1951
3. https://github.com/thomashoullier/bnd-pkg-merge

I borrowed things from:
* https://stackoverflow.com/a/29579194/10376845
* https://www.lispforum.com/viewtopic.php?f=2&t=1205
