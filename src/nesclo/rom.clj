(ns nesclo.rom)

(require '[clojure.java.io :refer [file output-stream input-stream]])

(defn unsigned-byte [b] (unchecked-byte (bit-and 0xff b)))

;;This is taken from clones clojure emulator (NES)
(defn romread [filepath]
(with-open [in (input-stream (file filepath))]
  (let [buf (.length (file filepath))
        buffer (byte-array buf)]
    (.read in buffer)
    (map unsigned-byte (sequence buffer)))))

(defn which-mapper [flag6 flag7]
  (let [ lower6 (bit-and flag6 0xF0)
         uper7 (bit-and flag7 0xF0)
         mapper (bit-or (bit-shift-right lower6 4) uper7)]
    (printf "Mapper %d \n" mapper)))

(defn header-read [rom]
  (let [ prg-size (nth rom 4)
         chr-size (nth rom 5)
         flags6 (nth rom 6)
         flags7 (nth rom 7)
         prg-ram (nth rom 8)
         flags9 (nth rom 9)
         flags10 (nth rom 10) ]
    (printf "Size of prg %x\n" prg-size)
    (which-mapper flags6 flags7)
     ))

;; Ok so this is way not clean for now because I
;; completely disregard the iNes headers.
;; TODO: Make iNes headers mean something
(defn getrom [filepath]
  (let [ rom (romread filepath) 
         header (take 16 rom)
         headless-data (drop 16 rom)
         prg-data (take 16384 headless-data)
         mem-head (vec (repeat 32768 0xFF))
         prg-data-vec (vec (concat mem-head prg-data prg-data))]
         (printf "Fuck this %x\n" (get prg-data-vec 0xC5F5))
         (byte-array prg-data-vec)))
    
