(ns nesclo.cpu)

;;Define registers A,X,Y, Program Counter, Stack Pointer and Status Register
(def a 0x00)
(def x 0x00)
(def y 0x00)
(def pc 0x00)
(def s 0x00)
(def p 0x00)

(def instr-size  [1, 2, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0,
                 1, 2, 0, 0, 0, 2, 2, 0, 1, 2, 0, 0, 0, 2, 2, 0,
                 1, 2, 0, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0,
                 1, 2, 0, 0, 0, 2, 2, 0, 1, 2, 0, 0, 0, 2, 2, 0,
                 1, 2, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0,
                 1, 2, 0, 0, 0, 2, 2, 0, 1, 2, 0, 0, 0, 2, 2, 0,
                 1, 2, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0,
                 1, 2, 0, 0, 0, 2, 2, 0, 1, 2, 0, 0, 0, 2, 2, 0,
                 0, 2, 0, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 0,
                 1, 2, 0, 0, 2, 2, 2, 0, 1, 2, 1, 0, 0, 2, 0, 0,
                 1, 2, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0,
                 1, 2, 0, 0, 2, 2, 2, 0, 1, 2, 1, 0, 2, 2, 2, 0,
                 1, 2, 0, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0,
                 1, 2, 0, 0, 0, 2, 2, 0, 1, 2, 0, 0, 0, 2, 2, 0,
                 1, 2, 0, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0,
                 1, 2, 0, 0, 0, 2, 2, 0, 1, 2, 0, 0, 0, 2, 2, 0])

(def instr ["brk","ora","kil","slo","nop","ora","asl","slo","php","ora","asl","anc","nop","ora","asl","slo",
         "bpl","ora","kil","slo","nop","ora","asl","slo","clc","ora","nop","slo","nop","ora","asl","slo",
         "jsr","and","kil","rla","bit","and","rol","rla","plp","and","rol","anc","bit","and","rol","rla",
         "bmi","and","kil","rla","nop","and","rol","rla","sec","and","nop","rla","nop","and","rol","rla",
         "rti","eor","kil","sre","nop","eor","lsr","sre","pha","eor","lsr","alr","jmp","eor","lsr","sre",
         "bvc","eor","kil","sre","nop","eor","lsr","sre","cli","eor","nop","sre","nop","eor","lsr","sre",
         "rts","adc","kil","rra","nop","adc","ror","rra","pla","adc","ror","arr","jmp","adc","ror","rra",
         "bvs","adc","kil","rra","nop","adc","ror","rra","sei","adc","nop","rra","nop","adc","ror","rra",
         "nop","sta","nop","sax","sty","sta","stx","sax","dey","nop","txa","xaa","sty","sta","stx","sax",
         "bbc","sta","kil","ahx","sty","sta","stx","sax","tya","sta","txs","tas","shy","sta","shx","ahx",
         "ldy","lda","ldx","lax","ldy","lda","ldx","lax","tay","lda","tax","lax","ldy","lda","ldx","lax",
         "bcs","lda","kil","lax","ldy","lda","ldx","lax","clv","lda","tsx","las","ldy","lda","ldx","lax",
         "cpy","cmp","nop","dcp","cpy","cmp","dec","dcp","iny","cmp","dex","axs","cpy","cmp","dec","dcp",
         "bne","cmp","kil","dcp","nop","cmp","dec","dcp","cld","cmp","nop","dcp","nop","cmp","dec","dcp",
         "cpx","sbc","nop","isc","cpx","sbc","inc","isc","inx","sbc","nop","sbc","cpx","sbc","inc","isc",
         "beq","sbc","kil","isc","nop","sbc","inc","isc","sed","sbc","nop","isc","nop","sbc","inc","isc"])

(defn hello []
  (println "hey"))

(defn disassemble [rom pc]
  (def inst (get instr (nth rom pc)))
  (def size (get instr-size (nth rom pc)))
  (println (format "0x%x => %s " pc inst))
  (def newpc (+ size pc))
  (recur rom (inc newpc)))
