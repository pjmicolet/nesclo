(ns nesclo.core
  (:gen-class))

(require 'nesclo.cpu)
(refer 'nesclo.cpu)

(require 'nesclo.apu)
(refer 'nesclo.apu)

(require 'nesclo.ppu)
(refer 'nesclo.ppu)

(require 'nesclo.rom)
(refer 'nesclo.rom)

(def filepath "C:/Users/pjm/Downloads/mario/supermario.nes")

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let[buffer (romread filepath)] (start-rom buffer 16)))

