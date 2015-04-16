(ns nesclo.cpu)

;;Define how large an instruction is beyond the instruction itself
(def instr-size  [0, 2, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0,
                 1, 2, 0, 0, 0, 2, 2, 0, 0, 2, 0, 0, 0, 2, 2, 0,
                 1, 2, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0,
                 1, 2, 0, 0, 0, 2, 2, 0, 0, 2, 0, 0, 0, 2, 2, 0,
                 0, 2, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0,
                 1, 2, 0, 0, 0, 2, 2, 0, 0, 2, 0, 0, 0, 2, 2, 0,
                 0, 2, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0,
                 1, 2, 0, 0, 0, 2, 2, 0, 0, 2, 0, 0, 0, 2, 2, 0,
                 0, 2, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0,
                 1, 2, 0, 0, 2, 2, 2, 0, 0, 2, 0, 0, 0, 2, 0, 0,
                 1, 2, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0,
                 1, 2, 0, 0, 2, 2, 2, 0, 0, 2, 0, 0, 2, 2, 2, 0,
                 1, 2, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0,
                 1, 2, 0, 0, 0, 2, 2, 0, 0, 2, 0, 0, 0, 2, 2, 0,
                 1, 2, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0,
                 1, 2, 0, 0, 0, 2, 2, 0, 0, 2, 0, 0, 0, 2, 2, 0])

;;Define name of each instruction
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

;;Define addressing type of each instruction
;; 0 implied
;; 1 immediate
;; 2 zero page
;; 3 zero page, X
;; 4 zero page, Y
;; 5 indirect index, X
;; 6 indirect index, Y
;; 7 absolute
;; 8 absolute, X
;; 9 absolute, Y
;; 10 indirect
;; 11 relative
(def type-addr [0,5,0,5,2,2,2,2,0,1,0,1,7,7,7,7,
           11,6,0,6,3,3,3,3,0,9,0,9,8,8,8,8,
           7,5,0,5,2,2,2,2,0,1,0,1,7,7,7,7,
           11,6,0,6,3,3,3,3,0,9,0,9,8,8,8,8,
           0,5,0,5,2,2,2,2,0,1,0,1,7,7,7,7,
           11,6,0,6,3,3,3,3,0,9,0,9,8,8,8,8,
           0,5,0,5,2,2,2,2,0,1,0,1,10,7,7,7,
           11,6,0,6,3,3,3,3,0,9,0,9,8,8,8,8,
           1,5,1,5,2,2,2,2,0,1,0,1,7,7,7,7,
           11,6,0,6,3,3,3,3,0,9,0,9,8,8,8,8,
           1,5,0,5,2,2,2,2,0,1,0,1,7,7,7,7,
           11,6,0,6,3,3,3,3,0,9,0,9,8,8,8,8,
           1,5,0,5,2,2,2,2,0,1,0,1,7,7,7,7,
           11,6,0,6,3,3,3,3,0,9,0,9,8,8,8,8,
           1,6,1,5,2,2,2,2,0,1,0,1,7,7,7,7,
           11,6,0,6,3,3,3,3,0,9,0,9,8,8,8,8,
           1,6,1,5,2,2,2,2,0,1,0,1,7,7,7,7,
           11,6,0,6,3,3,3,3,0,9,0,9,8,8,8,8])

(defn hello []
  (println "hey"))

(defn addr [rom pc size incr]
  (if (== size 0) 0
  (if (= size incr)
    (nth rom (+ pc incr))
    (+(bit-shift-left (nth rom (+ pc incr)) 2) (addr rom pc size (inc incr))))))

(defn disassemble [rom pc]
  (def inst (get instr (nth rom pc "No more PC") "Last Instruction"))
  (when (not= inst "Last Instruction")
    (do
        (def addressing (get type-addr (nth rom pc "No pc") "Nil"))
        (def size (get instr-size (nth rom pc)))
        (if (= addressing 0) 
          (println (format "0x%05x => %s " pc inst)))
        (if (= addressing 1) 
          (println (format "0x%05x => %s #$%02x" pc inst (addr rom pc size 1))))
        (if (= addressing 2)
          (println (format "0x%05x => %s $%02x" pc inst (addr rom pc size 1))))
        (if (= addressing 3)
          (println (format "0x%05x => %s $%02x,X" pc inst (addr rom pc size 1))))
        (if (= addressing 4)
          (println (format "0x%05x => %s $%02x,Y" pc inst (addr rom pc size 1))))
        (if (= addressing 5) 
          (println (format "0x%05x => %s ($%02x,X)" pc inst (addr rom pc size 1))))
        (if (= addressing 6)
          (println (format "0x%05x => %s ($%02x,Y)" pc inst (addr rom pc size 1))))
        (if (= addressing 7)
          (println (format "0x%05x => %s $%04x" pc inst (addr rom pc size 1))))
        (if (= addressing 8)
          (println (format "0x%05x => %s $%04x,X" pc inst (addr rom pc size 1))))
        (if (= addressing 9)
          (println (format "0x%05x => %s $%04x,Y" pc inst (addr rom pc size 1))))
        (if (= addressing 10)
          (println (format "0x%05x => %s ($%04x)" pc inst (addr rom pc size 1))))
        (if (= addressing 11)
          (println (format "0x%05x => %s ($%04x)" pc inst (addr rom pc size 1))))
        (recur rom (+ size (inc pc))))))

