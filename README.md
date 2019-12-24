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
the decoded message.

## Usage
### Using with an already made series of code lengths

### Generating code lengths from frequencies of occurence


## Dependencies
* `huffman-canon`: None. [bpm](https://github.com/thomashoullier/bnd-pkg-merge)
is useful for creating code lengths from frequencies.
* `huffman-canon/test`:
  * [rove](https://github.com/fukamachi/rove)
  * [array-operations](https://github.com/bendudson/array-operations)

## Issues
* No performance optimization was done. I'll probably put some type hinting
  at some point in the future as I start using the library.

## References
1. https://en.wikipedia.org/wiki/Canonical\_Huffman\_code
2. https://www.w3.org/Graphics/PNG/RFC-1951
3. https://github.com/thomashoullier/bnd-pkg-merge

I borrowed things from:
* https://stackoverflow.com/a/29579194/10376845
* https://www.lispforum.com/viewtopic.php?f=2&t=1205
