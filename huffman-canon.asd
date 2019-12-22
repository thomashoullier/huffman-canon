(defsystem huffman-canon
  :name "huffman-canon"
  :version "0.1"
  :author "karl"
  :license "UNLICENSE"
  :description "Canonical Huffman encoder/decoder."
  :components
  ((:module "src"
    :components ((:file "package")
                 (:file "encoded-dictionary" :depends-on ("package"))
                 (:file "huffman-canon" :depends-on ("package"
                                                     "encoded-dictionary"))
                 (:file "encoder-decoder"
                  :depends-on ("package" "huffman-canon"))))))
