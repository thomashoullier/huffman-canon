# Canonical Huffman encoder/decoder
`huffman-canon` is a canonical Huffman encoder/decoder for Common Lisp. It
converts `bit-vector` messages like `#*01001111100101010001000` into arrays of
indices into an alphabet, eg. `#(0 14 4 17 3 4)`.

See [1] if you don't see what I am talking about.

## Scope
`huffman-canon` is a Huffman encoder/decoder in a very strict sense. The
encoder/decoder is instantiated from an already made series of code lengths.
Series of code lengths are a typical way of representing Huffman codes
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

## Usage
### Exported functions

### Using with an already made series of code lengths

### Generating code lengths from frequencies of occurence


## Dependencies
* `huffman-canon`: None. [bpm](https://github.com/thomashoullier/bnd-pkg-merge)
is useful for creating code lengths from frequencies.
* `huffman-canon/test`:
  * [rove](https://github.com/fukamachi/rove)
  * [array-operations](https://github.com/bendudson/array-operations)

## References
1. https://en.wikipedia.org/wiki/Canonical\_Huffman\_code
2. https://www.w3.org/Graphics/PNG/RFC-1951
3. https://github.com/thomashoullier/bnd-pkg-merge

I borrowed things from:
* https://stackoverflow.com/a/29579194/10376845
* https://www.lispforum.com/viewtopic.php?f=2&t=1205
