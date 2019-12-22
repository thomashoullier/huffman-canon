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
                  :depends-on ("package" "huffman-canon")))))
  :in-order-to ((test-op (test-op "huffman-canon/test"))))

(defsystem huffman-canon/test
  :name "huffman-canon/test"
  :version "0.1"
  :author "karl"
  :license "UNLICENSE"
  :description "Rove test suite for huffman-canon."
  :depends-on ("huffman-canon" "rove" "array-operations")
  :components
  ((:module "test"
    :components ((:file "package")
                 (:file "rove-suite" :depends-on ("package")))))
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
