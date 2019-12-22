(defsystem huffman-canon
  :name "huffman-canon"
  :version "0.1"
  :author "karl"
  :license "UNLICENSE"
  :description "Canonical Huffman encoder/decoder."
  :components
  ((:module "src"
    :components ((:file "package")
                 (:file "huffman-canon" :depends-on ("package"))
                 (:file "encoder-decoder"
                  :depends-on ("package" "huffman-canon"))))))
