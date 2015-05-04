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


;; Ok so this is way not clean for now because I
;; completely disregard the iNes headers.
;; TODO: Make iNes headers mean something
(defn getrom [filepath]
  (let [ rom (romread filepath) ]
    (drop 16 rom)))
