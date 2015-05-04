(ns nesclo.cpu)

;; So this is a mutable state, which is BAD in FUNCTIONAL PROGRAMMING
(def instr-ops (atom {}))

;;Define how large an instruction is beyond the instruction itself
(def instr-size  [0 2 0 0 0 1 1 0 0 1 0 0 0 2 2 0
                 1 2 0 0 0 1 1 0 0 2 0 0 0 2 2 0
                 1 2 0 0 1 1 1 0 0 1 0 0 2 2 2 0
                 1 2 0 0 0 1 1 0 0 2 0 0 0 2 2 0
                 0 2 0 0 0 1 1 0 0 1 0 0 2 2 2 0
                 1 2 0 0 0 1 1 0 0 2 0 0 0 2 2 0
                 0 2 0 0 0 1 1 0 0 1 0 0 2 2 2 0
                 1 2 0 0 0 1 1 0 0 2 0 0 0 2 2 0
                 0 2 0 0 1 1 1 0 0 0 0 0 2 2 2 0
                 1 2 0 0 1 1 1 0 0 2 0 0 0 2 0 0
                 1 2 1 0 1 1 1 0 0 1 0 0 2 2 2 0
                 1 2 0 0 2 1 1 0 0 2 0 0 2 2 2 0
                 1 2 0 0 1 1 1 0 0 1 0 0 2 2 2 0
                 1 2 0 0 0 1 1 0 0 2 0 0 0 2 2 0
                 1 2 0 0 1 1 1 0 0 1 0 0 2 2 2 0
                 1 2 0 0 0 1 1 0 0 2 0 0 0 2 2 0])

;;Define name of each instruction
(def instr ["brk" "ora" "kil" "slo" "nop" "ora" "asl" "slo" "php" "ora" "asl" "anc" "nop" "ora" "asl" "slo"
         "bpl" "ora" "kil" "slo" "nop" "ora" "asl" "slo" "clc" "ora" "nop" "slo" "nop" "ora" "asl" "slo"
         "jsr" "and" "kil" "rla" "bit" "and" "rol" "rla" "plp" "and" "rol" "anc" "bit" "and" "rol" "rla"
         "bmi" "and" "kil" "rla" "nop" "and" "rol" "rla" "sec" "and" "nop" "rla" "nop" "and" "rol" "rla"
         "rti" "eor" "kil" "sre" "nop" "eor" "lsr" "sre" "pha" "eor" "lsr" "alr" "jmp" "eor" "lsr" "sre"
         "bvc" "eor" "kil" "sre" "nop" "eor" "lsr" "sre" "cli" "eor" "nop" "sre" "nop" "eor" "lsr" "sre"
         "rts" "adc" "kil" "rra" "nop" "adc" "ror" "rra" "pla" "adc" "ror" "arr" "jmp" "adc" "ror" "rra"
         "bvs" "adc" "kil" "rra" "nop" "adc" "ror" "rra" "sei" "adc" "nop" "rra" "nop" "adc" "ror" "rra"
         "nop" "sta" "nop" "sax" "sty" "sta" "stx" "sax" "dey" "nop" "txa" "xaa" "sty" "sta" "stx" "sax"
         "bbc" "sta" "kil" "ahx" "sty" "sta" "stx" "sax" "tya" "sta" "txs" "tas" "shy" "sta" "shx" "ahx"
         "ldy" "lda" "ldx" "lax" "ldy" "lda" "ldx" "lax" "tay" "lda" "tax" "lax" "ldy" "lda" "ldx" "lax"
         "bcs" "lda" "kil" "lax" "ldy" "lda" "ldx" "lax" "clv" "lda" "tsx" "las" "ldy" "lda" "ldx" "lax"
         "cpy" "cmp" "nop" "dcp" "cpy" "cmp" "dec" "dcp" "iny" "cmp" "dex" "axs" "cpy" "cmp" "dec" "dcp"
         "bne" "cmp" "kil" "dcp" "nop" "cmp" "dec" "dcp" "cld" "cmp" "nop" "dcp" "nop" "cmp" "dec" "dcp"
         "cpx" "sbc" "nop" "isc" "cpx" "sbc" "inc" "isc" "inx" "sbc" "nop" "sbc" "cpx" "sbc" "inc" "isc"
         "beq" "sbc" "kil" "isc" "nop" "sbc" "inc" "isc" "sed" "sbc" "nop" "isc" "nop" "sbc" "inc" "isc"])

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
(def type-addr-diss [0 5 0 5 2 2 2 2 0 1 0 1 7 7 7 7 
           11 6 0 6 3 3 3 3 0 9 0 9 8 8 8 8 
           7 5 0 5 2 2 2 2 0 1 0 1 7 7 7 7 
           11 6 0 6 3 3 3 3 0 9 0 9 8 8 8 8 
           0 5 0 5 2 2 2 2 0 1 0 1 7 7 7 7 
           11 6 0 6 3 3 3 3 0 9 0 9 8 8 8 8 
           0 5 0 5 2 2 2 2 0 1 0 1 10 7 7 7 
           11 6 0 6 3 3 3 3 0 9 0 9 8 8 8 8 
           1 5 1 5 2 2 2 2 0 1 0 1 7 7 7 7 
           11 6 0 6 3 3 3 3 0 9 0 9 8 8 8 8 
           1 5 1 5 2 2 4 4 0 1 0 1 7 7 7 7 
           11 6 0 6 3 3 3 3 0 9 0 9 8 8 8 8 
           1 5 0 5 2 2 4 4 0 1 0 1 7 7 7 7 
           11 6 0 6 3 3 3 3 0 9 0 9 8 8 8 8 
           1 6 1 5 2 2 2 2 0 1 0 1 7 7 7 7 
           11 6 0 6 3 3 3 3 0 9 0 9 8 8 8 8 
           1 6 1 5 2 2 2 2 0 1 0 1 7 7 7 7 
           11 6 0 6 3 3 3 3 0 9 0 9 8 8 8 8])

(def type-addr ["imp" "iix" "imp" "iix" "zp" "zp" "zp" "zp" "imp" "imm" "imp" "imm" "abs" "abs" "abs" "abs" 
           "rel" "iiy" "imp" "iiy" "zpx" "zpx" "zpx" "zpx" "imp" "absy" "imp" "absy" "absx" "absx" "absx" "absx" 
           "abs" "iix" "imp" "iix" "zp" "zp" "zp" "zp" "imp" "imm" "imp" "imm" "abs" "abs" "abs" "abs" 
           "rel" "iiy" "imp" "iiy" "zpx" "zpx" "zpx" "zpx" "imp" "absy" "imp" "absy" "absx" "absx" "absx" "absx" 
           "imp" "iix" "imp" "iix" "zp" "zp" "zp" "zp" "imp" "imm" "imp" "imm" "abs" "abs" "abs" "abs" 
           "rel" "iiy" "imp" "iiy" "zpx" "zpx" "zpx" "zpx" "imp" "absy" "imp" "absy" "absx" "absx" "absx" "absx" 
           "imp" "iix" "imp" "iix" "zp" "zp" "zp" "zp" "imp" "imm" "imp" "imm" "ind" "abs" "abs" "abs" 
           "rel" "iiy" "imp" "iiy" "zpx" "zpx" "zpx" "zpx" "imp" "absy" "imp" "absy" "absx" "absx" "absx" "absx" 
           "imm" "iix" "imm" "iix" "zp" "zp" "zp" "zp" "imp" "imm" "imp" "imm" "abs" "abs" "abs" "abs" 
           "rel" "iiy" "imp" "iiy" "zpx" "zpx" "zpx" "zpx" "imp" "absy" "imp" "absy" "absx" "absx" "absx" "absx" 
           "imm" "iix" "imm" "iix" "zp" "zp" "zpy" "zpy" "imp" "imm" "imp" "imm" "abs" "abs" "abs" "abs" 
           "rel" "iiy" "imp" "iiy" "zpx" "zpx" "zpx" "zpx" "imp" "absy" "imp" "absy" "absx" "absx" "absx" "absx" 
           "imm" "iix" "imp" "iix" "zp" "zp" "zpy" "zpy" "imp" "imm" "imp" "imm" "abs" "abs" "abs" "abs" 
           "rel" "iiy" "imp" "iiy" "zpx" "zpx" "zpx" "zpx" "imp" "absy" "imp" "absy" "absx" "absx" "absx" "absx" 
           "imm" "iiy" "imm" "iix" "zp" "zp" "zp" "zp" "imp" "imm" "imp" "imm" "abs" "abs" "abs" "abs" 
           "rel" "iiy" "imp" "iiy" "zpx" "zpx" "zpx" "zpx" "imp" "absy" "imp" "absy" "absx" "absx" "absx" "absx" 
           "imm" "iiy" "imm" "iix" "zp" "zp" "zp" "zp" "imp" "imm" "imp" "imm" "abs" "abs" "abs" "abs" 
           "rel" "iiy" "imp" "iiy" "zpx" "zpx" "zpx" "zpx" "imp" "absy" "imp" "absy" "absx" "absx" "absx" "absx"])

(defn addr [rom pc size]
  (if (= size 0)
    0
    (condp = size
      1 (nth rom (+ pc size))
      2 (+ (bit-shift-left (nth rom (+ pc size)) 8) (addr rom pc (- size 1))))))

(defmacro def-instr
  [name opcode args & code]
  `(do (defn ~name ~args
         ~@code)
       (swap! instr-ops assoc ~opcode ~name)))

(defn dis-once [rom pc]
  (let [ inst (get instr (nth rom pc "No more PC") "Last Instruction") ]
  (when (not= inst "Last Instruction")
    (let [ addressing (get type-addr-diss (nth rom pc "No pc") "Nil")
          size (get instr-size (nth rom pc))]
    (condp = addressing
         0 (printf "0x%05x => %s\n" pc inst)
         1 (printf "0x%05x => %s #$%02x\n" pc inst (addr rom pc size))
         2 (printf "0x%05x => %s $%02x\n" pc inst (addr rom pc size))
         3 (printf "0x%05x => %s $%02x,X\n" pc inst (addr rom pc size))
         4 (printf "0x%05x => %s $%02x,Y\n" pc inst (addr rom pc size))
         5 (printf "0x%05x => %s ($%02x,X)\n" pc inst (addr rom pc size))
         6 (printf "0x%05x => %s ($%02x,Y)\n" pc inst (addr rom pc size))
         7 (printf "0x%05x => %s $%04x\n" pc inst (addr rom pc size))
         8 (printf "0x%05x => %s $%04x,X\n" pc inst (addr rom pc size))
         9 (printf "0x%05x => %s $%04x,Y\n" pc inst (addr rom pc size))
         10 (printf "0x%05x => %s ($%04x)\n" pc inst (addr rom pc size))
         11 (printf "0x%05x => %s ($%04x)\n" pc inst (addr rom pc size)))))))

(defn disassemble [rom pc]
  (let [ inst (get instr (nth rom pc "No more PC") "Last Instruction") ]
  (when (not= inst "Last Instruction")
    (let [ addressing (get type-addr-diss (nth rom pc "No pc") "Nil")
    (let [ addressing (get type-addr (nth rom pc "No pc") "Nil")
          size (get instr-size (nth rom pc))]
    (condp = addressing
         0 (printf "0x%05x => %s\n" pc inst)
         1 (printf "0x%05x => %s #$%02x\n" pc inst (addr rom pc size))
         2 (printf "0x%05x => %s $%02x\n" pc inst (addr rom pc size))
         3 (printf "0x%05x => %s $%02x,X\n" pc inst (addr rom pc size))
         4 (printf "0x%05x => %s $%02x,Y\n" pc inst (addr rom pc size))
         5 (printf "0x%05x => %s ($%02x,X)\n" pc inst (addr rom pc size))
         6 (printf "0x%05x => %s ($%02x,Y)\n" pc inst (addr rom pc size))
         7 (printf "0x%05x => %s $%04x\n" pc inst (addr rom pc size))
         8 (printf "0x%05x => %s $%04x,X\n" pc inst (addr rom pc size))
         9 (printf "0x%05x => %s $%04x,Y\n" pc inst (addr rom pc size))
         10 (printf "0x%05x => %s ($%04x)\n" pc inst (addr rom pc size))
         11 (printf "0x%05x => %s ($%04x)\n" pc inst (addr rom pc size)))
    (recur rom (+ size (inc pc)))))))

