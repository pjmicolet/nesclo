(ns nesclo.rom)

(require '[clojure.java.io :refer [file output-stream input-stream]])

(defn unsigned-byte [b] (bit-and 0xff b))

;;This is taken from clones clojure emulator (NES)
(defn romread [filepath]
(with-open [in (input-stream (file filepath))]
  (let [buf (.length (file filepath))
        buffer (byte-array buf)]
    (.read in buffer)
    (map unsigned-byte (sequence buffer)))))
