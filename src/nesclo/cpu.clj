(ns nesclo.cpu)

;; So this is a mutable state, which is BAD in FUNCTIONAL PROGRAMMING
(def instr-ops (atom {}))
(def ram (atom (byte-array 65535)))

;;Define how large an instruction is beyond the instruction itself
(def instr-size  [0 2 0 0 0 1 1 0 0 1 0 0 0 2 2 0
                 1 2 0 0 0 1 1 0 0 2 0 0 0 2 2 0
                 2 2 0 0 1 1 1 0 0 1 0 0 2 2 2 0
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
         "bcc" "sta" "kil" "ahx" "sty" "sta" "stx" "sax" "tya" "sta" "txs" "tas" "shy" "sta" "shx" "ahx"
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
           1 5 1 5 2 2 2 2 0 1 0 1 7 7 7 7 
           11 6 0 6 3 3 3 3 0 9 0 9 8 8 8 8 
           1 5 0 5 2 2 2 2 0 1 0 1 7 7 7 7 
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

(def-instr ora-indx 0x01 [rom regs]
  (let [ pc (get regs :pc)
         p (get regs :p)
         x (get regs :x)
         operand (addr rom pc 1)
         addr-address (bit-and (+ operand x) 0x0FF)
         address-part1 (get @ram (bit-and addr-address 0x0FF))
         address-part2 (get @ram (bit-and (+ addr-address 1) 0x0FF))
         address (+ (bit-shift-left address-part2 8) (bit-and address-part1 0x00FF))
         data (bit-and (get @ram address) 0x0FF)
         new-a (bit-or (get regs :a) data)
         zero (if (= 0 new-a) 0x2 0x0)
         overflow (if (= (bit-and new-a 0x80) 0x80) 0x80 0x0)
         overzero (bit-or overflow zero)
         status (bit-and (bit-or p overzero) (bit-or overzero 0x7D))]
    (println data)
    (-> regs
        (assoc-in [:p] status)
        (assoc-in [:a] new-a)
        (assoc-in [:pc] (+ (get regs :pc) 2)))))

(def-instr php 0x08 [rom regs]
  (let [ preg (get regs :p) ]
    (aset-byte @ram (bit-or 0x100 (get regs :s))  (unchecked-byte preg))
    (-> regs
        (assoc-in [:s] (- (get regs :s) 1))
        (assoc-in [:pc] (+ (get regs :pc) 1)))))

(def-instr ora-imm 0x09 [rom regs]
  (let [ data (addr rom (get regs :pc) 1)
         new-a (bit-or (get regs :a) data)
         p (get regs :p)
         zero (if (= 0 new-a) 0x2 0x0)
         overflow (if (= (bit-and new-a 0x80) 0x80) 0x80 0x0)
         overzero (bit-or overflow zero)
         status (bit-and (bit-or p overzero) (bit-or overzero 0x7D))]
    (-> regs
        (assoc-in [:p] status)
        (assoc-in [:a] new-a)
        (assoc-in [:pc] (+ (get regs :pc) 2)))))

(def-instr asl-acc 0x0A [rom regs]
  (let [ a (get regs :a)
         shifted-a (bit-and (bit-shift-left a 1) 0x0FF)
         p (get regs :p)
         zero (if (= shifted-a 0x0) 0x02 0x00)
         carry (if (= (bit-and a 0x80) 0x80) 0x01 0x00)
         neg (if (= (bit-and shifted-a 0x80) 0x80) 0x80 0x00)
         status (bit-or zero carry neg)
         meta-status (bit-and (bit-or p status) (bit-or status 0x7C))]
    (-> regs
        (assoc-in [:p] meta-status)
        (assoc-in [:a] shifted-a)
        (assoc-in [:pc] (+ (get regs :pc) 1)))))

(def-instr bpl-rel 0x10 [rom regs]
  (if (= (bit-and (get regs :p) 0x80) 0x00)
    (assoc-in regs [:pc] (+ (+ (get regs :pc) (addr rom (get regs :pc) 1)) 2))
    (assoc-in regs [:pc] (+ (get regs :pc) 2))))

(def-instr clc 0x18 [rom regs]
  (-> regs
      (assoc-in [:p] (bit-and (get regs :p) 0xFE))
      (assoc-in [:pc] (+ (get regs :pc) 1))))

(def-instr jsr-abs 0x20 [rom regs]
  (let [ new-stack (get regs :s) 
        size (get instr-size 0x20)
        return-address (+ (get regs :pc) 0xC002)
        top-addr (bit-shift-right (bit-and return-address 0xFF00) 8)
        low-addr (bit-and return-address 0x00FF)]
    (aset-byte @ram (bit-or 0x100 new-stack) (unchecked-byte top-addr))
    (aset-byte @ram (bit-or 0x100 (- new-stack 1)) (unchecked-byte low-addr))
    (-> regs
        (assoc-in [:s] (- new-stack 2))
        (assoc-in [:pc] (- (addr rom (get regs :pc) size) 0xC000)))))

(def-instr and-indx 0x21 [rom regs]
  (let [ p (get regs :p)
         x (get regs :x)
         pc (get regs :pc)
         operand (addr rom pc 1)
         addr-address (bit-and (+ operand x) 0x0FF)
         address-part1 (get @ram (bit-and addr-address 0x0FF))
         address-part2 (get @ram (bit-and (+ addr-address 1) 0x0FF))
         address (+ (bit-shift-left address-part2 8) (bit-and address-part1 0x00FF))
         data (bit-and (get @ram address) 0x0FF)
         anded-a (bit-and data (get regs :a))
         overflow (bit-and anded-a 0x80)
         zero (if (= anded-a 0) 0x02 0x00)
         apply-stat (bit-or zero overflow)
         stat (bit-and (bit-or  p apply-stat) (bit-or apply-stat 0x7D))]
    (-> regs
        (assoc-in [:a] anded-a)
        (assoc-in [:p] stat)
        (assoc-in [:pc] (+ (get regs :pc) 2)))))

(def-instr bit-zp 0x24 [rom regs]
  (let [ data (bit-and (get @ram (addr rom (get regs :pc) 1)) 0xFF)
         p (get regs :p)
         six-seven (bit-or (bit-and data 0x80) (bit-and data 0x40))
         a (get regs :a)
         toset (if (= (bit-and data a) 0x00) 0x02 0x00)
         final-stat (bit-or six-seven toset)
         stat (bit-and (bit-or p final-stat) (bit-or final-stat 0x3D))]
    (-> regs
        (assoc-in [:p] stat)
        (assoc-in [:pc] (+ (get regs :pc) 2)))))

(def-instr plp 0x28 [rom regs]
  (let [ new-stack (+ (get regs :s) 1)
         data (bit-or (bit-and (bit-and (get @ram (bit-or 0x100 new-stack)) 0xFF) 0xEF) 0x20) ]
    (-> regs
        (assoc-in [:p] data)
        (assoc-in [:s] new-stack)
        (assoc-in [:pc] (+ (get regs :pc) 1)))))

(def-instr and-imm 0x29 [rom regs]
  (let [ p (get regs :p)
         data (addr rom (get regs :pc) 1)
         anded-a (bit-and data (get regs :a))
         overflow (bit-and anded-a 0x80)
         zero (if (= anded-a 0) 0x02 0x00)
         apply-stat (bit-or zero overflow)
         stat (bit-and (bit-or  p apply-stat) (bit-or apply-stat 0x7D))]
    (-> regs
        (assoc-in [:a] anded-a)
        (assoc-in [:p] stat)
        (assoc-in [:pc] (+ (get regs :pc) 2)))))

(def-instr rol-acc 0x2A [rom regs]
  (let [ a (get regs :a)
         p (get regs :p)
         shifted-a (bit-or (bit-and (bit-shift-left a 1) 0x0FF) (bit-and 0x01 p))
         zero (if (= shifted-a 0x0) 0x02 0x00)
         carry (if (= (bit-and a 0x80) 0x80) 0x01 0x00)
         neg (if (= (bit-and shifted-a 0x80) 0x80) 0x80 0x00)
         status (bit-or zero carry neg)
         meta-status (bit-and (bit-or p status) (bit-or status 0x7C))]
    (-> regs
        (assoc-in [:p] meta-status)
        (assoc-in [:a] shifted-a)
        (assoc-in [:pc] (+ (get regs :pc) 1)))))
         
(def-instr bmi 0x30 [rom regs]
  (let [ data (addr rom (get regs :pc) 1)
         state (get regs :p)]
    (-> regs
        (assoc-in [:pc] ( if (= (bit-and state 0x80) 0x80) (+ (+ (get regs :pc) data) 2) (+ (get regs :pc) 2))))))

(def-instr sec 0x38 [rom regs]
  (-> regs
    (assoc-in [:p] (bit-or (get regs :p) 0x01))
    (assoc-in [:pc] (+ (get regs :pc) 1))))

(def-instr rti 0x40 [rom regs]
  (let [ s (get regs :s)
         p-reg-pos (+ s 1)
         pc-part1 (+ s 2)
         pc-part2 (+ s 3)
         p (bit-or (bit-and (get @ram (bit-or 0x100 p-reg-pos)) 0x0FF) 0x20)
         low-addr (bit-and (get @ram (bit-or 0x100 pc-part1)) 0x0FF)
         top-addr (bit-and (get @ram (bit-or 0x100 pc-part2)) 0x0FF)
         new-pc (- (+ (bit-shift-left top-addr 8) (bit-and low-addr 0x00FF)) 0xC000)]
    (-> regs
        (assoc-in [:p] p)
        (assoc-in [:s] pc-part2)
        (assoc-in [:pc] new-pc))))

(def-instr eor-indx 0x41 [rom regs]
  (let [ pc (get regs :pc)
         p (get regs :p)
         x (get regs :x)
         operand (addr rom pc 1)
         addr-address (bit-and (+ operand x) 0x0FF)
         address-part1 (get @ram (bit-and addr-address 0x0FF))
         address-part2 (get @ram (bit-and (+ addr-address 1) 0x0FF))
         address (+ (bit-shift-left address-part2 8) (bit-and address-part1 0x00FF))
         data (bit-and (get @ram address) 0x0FF)
         a (get regs :a)
         new-a (bit-xor data a)
         p (get regs :p)
         zero (if (= 0 new-a) 0x2 0x0)
         overflow (if (= (bit-and new-a 0x80) 0x80) 0x80 0x0)
         overzero (bit-or overflow zero)
         status (bit-and (bit-or p overzero) (bit-or overzero 0x7D))]
    (-> regs
        (assoc-in [:p] status)
        (assoc-in [:a] new-a)
        (assoc-in [:pc] (+ (get regs :pc) 2)))))

(def-instr pha 0x48 [rom regs]
  (aset-byte @ram (bit-or 0x100 (get regs :s)) (unchecked-byte (get regs :a)))
  (-> regs
      (assoc-in [:s] (- (get regs :s) 1))
      (assoc-in [:pc] (+ (get regs :pc) 1))))

(def-instr lsr-acc 0x4A [rom regs]
  (let [ a (get regs :a)
         shifted-a (bit-shift-right a 1)
         p (get regs :p)
         zero (if (= shifted-a 0x0) 0x02 0x00)
         carry (if (= (bit-and a 0x01) 0x01) 0x01 0x00)
         neg (if (= (bit-and shifted-a 0x80) 0x80) 0x80 0x00)
         status (bit-or zero carry neg)
         meta-status (bit-and (bit-or p status) (bit-or status 0x7C))]
    (-> regs
        (assoc-in [:p] meta-status)
        (assoc-in [:a] shifted-a)
        (assoc-in [:pc] (+ (get regs :pc) 1)))))

(def-instr jmp-abs 0x4C [rom regs]
  (let [ pc (get regs :pc)
         size (get instr-size 0x4C)
         address (addr rom pc size)]
    (assoc-in regs [:pc] (- address 0xC000))))

(def-instr eor-imm 0x49 [rom regs]
  (let [ data (addr rom (get regs :pc) 1)
         a (get regs :a)
         new-a (bit-xor data a)
         p (get regs :p)
         zero (if (= 0 new-a) 0x2 0x0)
         overflow (if (= (bit-and new-a 0x80) 0x80) 0x80 0x0)
         overzero (bit-or overflow zero)
         status (bit-and (bit-or p overzero) (bit-or overzero 0x7D))]
    (-> regs
        (assoc-in [:p] status)
        (assoc-in [:a] new-a)
        (assoc-in [:pc] (+ (get regs :pc) 2)))))

(def-instr bvc-rel 0x50 [rom regs]
  (if (= (bit-and (get regs :p) 0x40) 0x00)
    (assoc-in regs [:pc] (+ (+ (get regs :pc) (addr rom (get regs :pc) 1)) 2))
    (assoc-in regs [:pc] (+ (get regs :pc) 2))))

(def-instr rts 0x60 [rom regs]
  (let [ new-stack (+ (bit-and (get regs :s) 0x0FF) 1)
         low-addr (bit-and (get @ram (bit-or 0x100 new-stack)) 0x0FF)
         top-addr (bit-and (get @ram (bit-or 0x100 (+ new-stack 1))) 0x0FF)
         new-pc (- (+ (bit-shift-left top-addr 8) (bit-and low-addr 0x00FF)) 0xC000)]
    (-> regs
      (assoc-in [:pc] (+ new-pc 1))
      (assoc-in [:s] (+ new-stack 1)))))

(def-instr adc-indx 0x61 [rom regs]
  (let [ pc (get regs :pc)
         p (get regs :p)
         x (get regs :x)
         operand (addr rom pc 1)
         addr-address (bit-and (+ operand x) 0x0FF)
         address-part1 (get @ram (bit-and addr-address 0x0FF))
         address-part2 (get @ram (bit-and (+ addr-address 1) 0x0FF))
         address (+ (bit-shift-left address-part2 8) (bit-and address-part1 0x00FF))
         data (bit-and (get @ram address) 0x0FF)
         carry (if (= (bit-and (get regs :p) 0x1) 0x1) 1 0)
         a (get regs :a)
         p (get regs :p)
         adc (bit-and (+ data a carry) 0xFF)
         zero (if (= 0 adc) 0x02 0x00)
         overflow (if (= 0 (bit-and (bit-not (bit-xor a data)) (bit-xor a adc) 0x80)) 0 0x40)
         negative (if (= (bit-and adc 0x80) 0x80) 0x80 0)
         carry-flag (if (= 
                         (bit-and (+ (bit-and a 0xFFF) (bit-and data 0xFFF) (bit-and carry 0xFFF)) 0x100) 0x100)
                      0x01 0x00)
         status (bit-or overflow negative carry-flag zero)
         meta-status (bit-and (bit-or p status) (bit-or status 0x3C))]
    (-> regs
        (assoc-in [:p] meta-status)
        (assoc-in [:a] adc)
        (assoc-in [:pc] (+ (get regs :pc) 2)))))

(def-instr pla 0x68 [rom regs]
  (let [ new-stack (+ (get regs :s) 1)
         p (get regs :p)
         data (bit-and (get @ram (bit-or 0x100 new-stack)) 0xFF)
         zero (if (= 0 data) 0x2 0x0)
         overflow (if (= (bit-and data 0x80) 0x80) 0x80 0x0)
         overzero (bit-or overflow zero)
         status (bit-and (bit-or p overzero) (bit-or overzero 0x7D))]
    (-> regs
        (assoc-in [:a] data)
        (assoc-in [:p] status)
        (assoc-in [:s] new-stack)
        (assoc-in [:pc] (+ (get regs :pc) 1)))))

(def-instr adc-imm 0x69 [rom regs]
  (let [ data (addr rom (get regs :pc) 1)
         carry (if (= (bit-and (get regs :p) 0x1) 0x1) 1 0)
         a (get regs :a)
         p (get regs :p)
         adc (bit-and (+ data a carry) 0xFF)
         zero (if (= 0 adc) 0x02 0x00)
         overflow (if (= 0 (bit-and (bit-not (bit-xor a data)) (bit-xor a adc) 0x80)) 0 0x40)
         negative (if (= (bit-and adc 0x80) 0x80) 0x80 0)
         carry-flag (if (= 
                         (bit-and (+ (bit-and a 0xFFF) (bit-and data 0xFFF) (bit-and carry 0xFFF)) 0x100) 0x100)
                      0x01 0x00)
         status (bit-or overflow negative carry-flag zero)
         meta-status (bit-and (bit-or p status) (bit-or status 0x3C))]
    (-> regs
        (assoc-in [:p] meta-status)
        (assoc-in [:a] adc)
        (assoc-in [:pc] (+ (get regs :pc) 2)))))

(def-instr ror-acc 0x6A [rom regs]
  (let [ a (get regs :a)
         p (get regs :p)
         shifted-a (bit-or (bit-shift-right a 1) (bit-shift-left (bit-and p 0x01) 7))
         zero (if (= shifted-a 0x0) 0x02 0x00)
         carry (if (= (bit-and a 0x01) 0x01) 0x01 0x00)
         neg (if (= (bit-and shifted-a 0x80) 0x80) 0x80 0x00)
         status (bit-or zero carry neg)
         meta-status (bit-and (bit-or p status) (bit-or status 0x7C))]
    (-> regs
        (assoc-in [:p] meta-status)
        (assoc-in [:a] shifted-a)
        (assoc-in [:pc] (+ (get regs :pc) 1)))))

(def-instr bvs-rel 0x70 [rom regs]
  (if (= (bit-and (get regs :p) 0x40) 0x40)
    (assoc-in regs [:pc] (+ (+ (get regs :pc) (addr rom (get regs :pc) 1)) 2))
    (assoc-in regs [:pc] (+ (get regs :pc) 2))))

(def-instr sei-imp 0x78 [rom regs]
  (let [ int-flag (bit-or (get regs :p) 0x4)]
    (-> regs
        (assoc-in [:p] int-flag)
        (assoc-in [:pc] (+ (get regs :pc) 1)))))

(def-instr sta-indx 0x81 [rom regs]
  (let [ pc (get regs :pc)
         p (get regs :p)
         x (get regs :x)
         operand (addr rom pc 1)
         addr-address (bit-and (+ operand x) 0x0FF)
         address-part1 (get @ram (bit-and addr-address 0x0FF))
         address-part2 (get @ram (bit-and (+ addr-address 1) 0x0FF))
         address (+ (bit-shift-left address-part2 8) (bit-and address-part1 0x00FF))]
  (aset-byte @ram address (unchecked-byte (get regs :a)))
  (assoc-in regs [:pc] (+ (get regs :pc) 2))))

(def-instr sta-zp 0x85 [rom regs]
  (aset-byte @ram (addr rom (get regs :pc) 1) (unchecked-byte (get regs :a)))
  (assoc-in regs [:pc] (+ (get regs :pc) 2)))

(def-instr stx-zp 0x86 [rom regs]
  (aset-byte @ram (addr rom (get regs :pc) 1) (unchecked-byte (get regs :x)))
  (assoc-in regs [:pc] (+ (get regs :pc) 2)))

(def-instr dey 0x88 [rom regs]
  (let [y (get regs :y)
        p (get regs :p)
        decy (bit-and (dec y) 0x0FF)
        zero (if (= decy 0) 0x02 0x00)
        neg-flag (if (= (bit-and decy 0x80) 0x80) 0x80 0x00)
        status (bit-or zero neg-flag)
        meta-status (bit-and (bit-or p status) (bit-or status 0x7D))]
    (-> regs
        (assoc-in [:p] meta-status)
        (assoc-in [:y] decy)
        (assoc-in [:pc] (+ (get regs :pc) 1)))))

(def-instr txa 0x8A [rom regs]
  (let [x (get regs :x)
        p (get regs :p)
        zero (if (= x 0) 0x02 0x00)
        neg-flag (if (= (bit-and x 0x80) 0x80) 0x80 0x00)
        status (bit-or zero neg-flag)
        meta-status (bit-and (bit-or p status) (bit-or status 0x7D))]
    (-> regs
        (assoc-in [:p] meta-status)
        (assoc-in [:a] x)
        (assoc-in [:pc] (+ (get regs :pc) 1)))))

(def-instr stx-abs 0x8E [rom regs]
  (aset-byte @ram (addr rom (get regs :pc) 2) (unchecked-byte (get regs :x)))
  (assoc-in regs [:pc] (+ (get regs :pc) 3)))

(def-instr stx-abs 0x8D [rom regs]
  (aset-byte @ram (addr rom (get regs :pc) 2) (unchecked-byte (get regs :a)))
  (assoc-in regs [:pc] (+ (get regs :pc) 3)))

; Still need to figure out how to do signed byte
(def-instr bcc 0x90 [rom regs]
  (if (= (bit-and (get regs :p) 0x01) 0x00)
    (assoc-in regs [:pc] (+ (+ (get regs :pc) (addr rom (get regs :pc) 1)) 2))
    (assoc-in regs [:pc] (+ (get regs :pc) 2))))

(def-instr tya 0x98 [rom regs]
  (let [y (get regs :y)
        p (get regs :p)
        zero (if (= y 0) 0x02 0x00)
        neg-flag (if (= (bit-and y 0x80) 0x80) 0x80 0x00)
        status (bit-or zero neg-flag)
        meta-status (bit-and (bit-or p status) (bit-or status 0x7D))]
    (-> regs
        (assoc-in [:p] meta-status)
        (assoc-in [:a] y)
        (assoc-in [:pc] (+ (get regs :pc) 1)))))

(def-instr txs 0x9A [rom regs]
  (let [x (get regs :x)
        p (get regs :p)]
    (-> regs
        (assoc-in [:s] x)
        (assoc-in [:pc] (+ (get regs :pc) 1)))))

(def-instr ldy-imm 0xA0 [rom regs]
  (let [ pc (get regs :pc)
         p (get regs :p)
         data (addr rom pc 1)
         status (bit-or (if(= data 0x0) 0x02 0x0) (if(= (bit-and data 0x80) 0x80) 0x80 0x0))
         meta-stat (bit-and (bit-or p status) (bit-or status 0x7D))]
    (-> regs
        (assoc-in [:y] data)
        (assoc-in [:p] meta-stat)
        (assoc-in [:pc] ( + (get regs :pc) 2)))))

(def-instr lda-indx 0xA1 [rom regs]
  (let [ pc (get regs :pc)
         p (get regs :p)
         x (get regs :x)
         operand (addr rom pc 1)
         addr-address (bit-and (+ operand x) 0x0FF)
         address-part1 (get @ram (bit-and addr-address 0x0FF))
         address-part2 (get @ram (bit-and (+ addr-address 1) 0x0FF))
         address (+ (bit-shift-left address-part2 8) (bit-and address-part1 0x00FF))
         data (get @ram address)
         status (bit-or (if(= data 0x0) 0x02 0x0) (if(= (bit-and data 0x80) 0x80) 0x80 0x0))
         meta-stat (bit-and (bit-or p status) (bit-or status 0x7D))]
    (-> regs
        (assoc-in [:a] data)
        (assoc-in [:p] meta-stat)
        (assoc-in [:pc] ( + (get regs :pc) 2)))))

(def-instr ldx-imm 0xA2 [rom regs]
  (let [ pc (get regs :pc)
         p (get regs :p)
         data (addr rom pc 1)
         status (bit-or (if(= data 0x0) 0x02 0x0) (if(= (bit-and data 0x80) 0x80) 0x80 0x0))
         meta-stat (bit-and (bit-or p status) (bit-or status 0x7D))]
    (-> regs
        (assoc-in [:x] data)
        (assoc-in [:p] meta-stat)
        (assoc-in [:pc] ( + (get regs :pc) 2)))))

(def-instr lda-imm 0xA5 [rom regs]
  (let [ pc (get regs :pc)
         p (get regs :p)
         address (addr rom pc 1)
         data (get @ram address)
         status (bit-or (if(= data 0x0) 0x02 0x0) (if(= (bit-and data 0x80) 0x80) 0x80 0x0))
         meta-stat (bit-and (bit-or p status) (bit-or status 0x7D))]
    (-> regs
        (assoc-in [:a] data)
        (assoc-in [:p] meta-stat)
        (assoc-in [:pc] ( + (get regs :pc) 2)))))

(def-instr tay 0xA8 [rom regs]
  (let [a (get regs :a)
        p (get regs :p)
        zero (if (= a 0) 0x02 0x00)
        neg-flag (if (= (bit-and a 0x80) 0x80) 0x80 0x00)
        status (bit-or zero neg-flag)
        meta-status (bit-and (bit-or p status) (bit-or status 0x7D))]
    (-> regs
        (assoc-in [:p] meta-status)
        (assoc-in [:y] a)
        (assoc-in [:pc] (+ (get regs :pc) 1)))))

(def-instr lda-imm 0xA9 [rom regs]
  (let [ pc (get regs :pc)
         p (get regs :p)
         data (addr rom pc 1)
         status (bit-or (if(= data 0x0) 0x02 0x0) (if(= (bit-and data 0x80) 0x80) 0x80 0x0))
         meta-stat (bit-and (bit-or p status) (bit-or status 0x7D))]
    (-> regs
        (assoc-in [:a] data)
        (assoc-in [:p] meta-stat)
        (assoc-in [:pc] ( + (get regs :pc) 2)))))

(def-instr tax 0xAA [rom regs]
  (let [a (get regs :a)
        p (get regs :p)
        zero (if (= a 0) 0x02 0x00)
        neg-flag (if (= (bit-and a 0x80) 0x80) 0x80 0x00)
        status (bit-or zero neg-flag)
        meta-status (bit-and (bit-or p status) (bit-or status 0x7D))]
    (-> regs
        (assoc-in [:p] meta-status)
        (assoc-in [:x] a)
        (assoc-in [:pc] (+ (get regs :pc) 1)))))

(def-instr lda-abs 0xAD [rom regs]
  (let [ pc (get regs :pc)
         p (get regs :p)
         where (addr rom pc 2)
         data (get @ram where)
         status (bit-or (if(= data 0x0) 0x02 0x0) (if(= (bit-and data 0x80) 0x80) 0x80 0x0))
         meta-stat (bit-and (bit-or p status) (bit-or status 0x7D))]
    (-> regs
        (assoc-in [:a] data)
        (assoc-in [:p] meta-stat)
        (assoc-in [:pc] ( + (get regs :pc) 3)))))

(def-instr ldx-abs 0xAE [rom regs]
  (let [ pc (get regs :pc)
         p (get regs :p)
         where (addr rom pc 2)
         data (get @ram where)
         status (bit-or (if(= data 0x0) 0x02 0x0) (if(= (bit-and data 0x80) 0x80) 0x80 0x0))
         meta-stat (bit-and (bit-or p status) (bit-or status 0x7D))]
    (-> regs
        (assoc-in [:x] data)
        (assoc-in [:p] meta-stat)
        (assoc-in [:pc] ( + (get regs :pc) 3)))))

; Still need to figure out how to do signed byte
(def-instr bcs 0xB0 [rom regs]
  (if (= (bit-and (get regs :p) 0x01) 0x01)
    (assoc-in regs [:pc] (+ (+ (get regs :pc) (addr rom (get regs :pc) 1) 2)))
    (assoc-in regs [:pc] (+ (get regs :pc) 2))))

(def-instr clv 0xB8 [rom regs]
  (let [ p (bit-and (get regs :p) 0xBF) ]
    (-> regs
        (assoc-in [:p] p)
        (assoc-in [:pc] (+ (get regs :pc) 1)))))

(def-instr tsx 0xBA [rom regs]
  (let [s (get regs :s)
        p (get regs :p)
        zero (if (= s 0) 0x02 0x00)
        neg-flag (if (= (bit-and s 0x80) 0x80) 0x80 0x00)
        status (bit-or zero neg-flag)
        meta-status (bit-and (bit-or p status) (bit-or status 0x7D))]
    (-> regs
        (assoc-in [:p] meta-status)
        (assoc-in [:x] s)
        (assoc-in [:pc] (+ (get regs :pc) 1)))))
         

(def-instr cpy-imm 0xC0 [rom regs]
  (let [ data (addr rom (get regs :pc) 1)
         p (get regs :p)
         is-bigger (if (> (get regs :y) data) 0x01 0x00)
         is-equal (if (= (get regs :y) data) 0x03 0x00)
         is-neg (if (= (bit-and (- (get regs :y) data) 0x80)0x80) 0x80 0x00)
         set-status (bit-or is-bigger is-equal is-neg)
         status (bit-and (bit-or p set-status) (bit-or set-status 0x7C))]
    (-> regs
        (assoc-in [:p] status)
        (assoc-in [:pc] (+ (get regs :pc) 2)))))

(def-instr iny 0xC8 [rom regs]
  (let [y (get regs :y)
        p (get regs :p)
        incy (bit-and (inc y) 0x0FF)
        zero (if (= incy 0) 0x02 0x00)
        neg-flag (if (= (bit-and incy 0x80) 0x80) 0x80 0x00)
        status (bit-or zero neg-flag)
        meta-status (bit-and (bit-or p status) (bit-or status 0x7D))]
    (-> regs
        (assoc-in [:p] meta-status)
        (assoc-in [:y] incy)
        (assoc-in [:pc] (+ (get regs :pc) 1)))))

(def-instr cmp-imm 0xC9 [rom regs]
  (let [ data (addr rom (get regs :pc) 1)
         p (get regs :p)
         is-bigger (if (> (get regs :a) data) 0x01 0x00)
         is-equal (if (= (get regs :a) data) 0x03 0x00)
         is-neg (if (= (bit-and (- (get regs :a) data) 0x80)0x80) 0x80 0x00)
         set-status (bit-or is-bigger is-equal is-neg)
         status (bit-and (bit-or p set-status) (bit-or set-status 0x7C))]
    (-> regs
        (assoc-in [:p] status)
        (assoc-in [:pc] (+ (get regs :pc) 2)))))

(def-instr dex 0xCA [rom regs]
  (let [x (get regs :x)
        p (get regs :p)
        decx (bit-and (dec x) 0x0FF)
        zero (if (= decx 0) 0x02 0x00)
        neg-flag (if (= (bit-and decx 0x80) 0x80) 0x80 0x00)
        status (bit-or zero neg-flag)
        meta-status (bit-and (bit-or p status) (bit-or status 0x7D))]
    (-> regs
        (assoc-in [:p] meta-status)
        (assoc-in [:x] decx)
        (assoc-in [:pc] (+ (get regs :pc) 1)))))

(def-instr bed 0xD0 [rom regs]
  (if (= (bit-and (get regs :p) 0x02) 0x00)
    (assoc-in regs [:pc] (+ (+ (get regs :pc) (addr rom (get regs :pc) 1)) 2))
    (assoc-in regs [:pc] (+ (get regs :pc) 2))))

(def-instr cld 0xD8 [rom regs]
  (let [ status (bit-and (get regs :p) 0xF7)]
    (-> regs
        (assoc-in [:p] status)
        (assoc-in [:pc] (+ (get regs :pc) 1)))))

(def-instr cpx-imm 0xE0 [rom regs]
  (let [ data (addr rom (get regs :pc) 1)
         p (get regs :p)
         is-bigger (if (> (get regs :x) data) 0x01 0x00)
         is-equal (if (= (get regs :x) data) 0x03 0x00)
         is-neg (if (= (bit-and (- (get regs :x) data) 0x80)0x80) 0x80 0x00)
         set-status (bit-or is-bigger is-equal is-neg)
         status (bit-and (bit-or p set-status) (bit-or set-status 0x7C))]
    (-> regs
        (assoc-in [:p] status)
        (assoc-in [:pc] (+ (get regs :pc) 2)))))

(def-instr inx 0xE8 [rom regs]
  (let [x (get regs :x)
        p (get regs :p)
        incx (bit-and (inc x) 0x0FF)
        zero (if (= incx 0) 0x02 0x00)
        neg-flag (if (= (bit-and incx 0x80) 0x80) 0x80 0x00)
        status (bit-or zero neg-flag)
        meta-status (bit-and (bit-or p status) (bit-or status 0x7D))]
    (-> regs
        (assoc-in [:p] meta-status)
        (assoc-in [:x] incx)
        (assoc-in [:pc] (+ (get regs :pc) 1)))))

(def-instr sbc-imm 0xE9 [rom regs]
  (let [ data (addr rom (get regs :pc) 1)
         carry (if (= (bit-and (get regs :p) 0x01) 0x01) 0x01 0x00)
         not-carry (bit-and (bit-not carry) 0x01)
         a (get regs :a)
         p (get regs :p)
         sbc (bit-and (- a data not-carry) 0xFF)
         zero (if (= 0 sbc) 0x02 0x00)
         overflow (if (= 0 (bit-and (bit-xor a data) (bit-xor a sbc) 0x80)) 0x00 0x40)
         negative (if (= (bit-and sbc 0x80) 0x80) 0x80 0)
         carry-flag (if (= 
                         (bit-and (- (bit-and a 0xFFF) (bit-and data 0xFFF) (bit-and not-carry 0xFFF)) 0x100) 0x100)
                      0x00 0x01)
         status (bit-or overflow negative carry-flag zero)
         meta-status (bit-and (bit-or p status) (bit-or status 0x3C))]
    (-> regs
        (assoc-in [:p] meta-status)
        (assoc-in [:a] sbc)
        (assoc-in [:pc] (+ (get regs :pc) 2)))))

(def-instr nop 0xEA [rom regs]
  (assoc-in regs [:pc] (+ (get regs :pc) 1)))

(def-instr beq 0xF0 [rom regs]
  (if (= (bit-and (get regs :p) 0x02) 0x02)
    (assoc-in regs [:pc] (+ (+ (get regs :pc) (addr rom (get regs :pc) 1)) 2))
    (assoc-in regs [:pc] (+ (get regs :pc) 2))))

(def-instr sed 0xF8 [rom regs]
  (let [ dec-flag (bit-or (get regs :p) 0x8)]
    (-> regs
        (assoc-in [:p] dec-flag)
        (assoc-in [:pc] (+ (get regs :pc) 1)))))

(defn disassemble [rom pc regs recurse]
  (let [ inst (get instr (nth rom pc "No more PC") "Last Instruction") ]
  (when (not= inst "Last Instruction")
    (let [ addressing (get type-addr-diss (nth rom pc "No pc") "Nil")
          size (get instr-size (nth rom pc))]
    (condp = addressing
         0 (printf "0x%05x => %s " pc inst)
         1 (printf "0x%05x => %s #$%02x " pc inst (addr rom pc size))
         2 (printf "0x%05x => %s $%02x " pc inst (addr rom pc size))
         3 (printf "0x%05x => %s $%02x,X " pc inst (addr rom pc size))
         4 (printf "0x%05x => %s $%02x,Y " pc inst (addr rom pc size))
         5 (printf "0x%05x => %s ($%02x,X) " pc inst (addr rom pc size))
         6 (printf "0x%05x => %s ($%02x,Y) " pc inst (addr rom pc size))
         7 (printf "0x%05x => %s $%04x " pc inst (addr rom pc size))
         8 (printf "0x%05x => %s $%04x,X " pc inst (addr rom pc size))
         9 (printf "0x%05x => %s $%04x,Y " pc inst (addr rom pc size))
         10 (printf "0x%05x => %s ($%04x) " pc inst (addr rom pc size))
      ;; 11 is wrong right now
         11 (printf "0x%05x => %s ($%04x) " pc inst (addr rom pc size)))
    (if (= addressing 0)
    (printf "\t\t\tA:%02x X:%02x Y:%02x P:%02x SP:%02x\n" (get regs :a) (get regs :x)
            (get regs :y) (get regs :p) (get regs :s))
    (printf "\t\tA:%02x X:%02x Y:%02x P:%02x SP:%02x\n" (get regs :a) (get regs :x)
            (get regs :y) (get regs :p) (get regs :s)))
    (when (= recurse 1)
    (recur rom (+ size (inc pc)) regs 1))))))

(defn run [op rom regs]
  (op rom regs))

(defn execute-instr [rom regs]
  (let [ inst (get @instr-ops (nth rom (get regs :pc)) "Last") ]
  (disassemble rom (get regs :pc) regs 0)
  (when (not= inst "Last")
    (recur rom (run inst rom regs)))))

(defn start-rom [rom pc]
  ;; Will do some initialisation here
  (let [ registers {:pc 0 :a 0x00 :x 0x00 :y 0x00 :s 0xFD :p 0x24}]
    (printf "Starting at 0x%02x\n" pc)
    (printf "Registers are %s\n" (get registers :pc))
    (execute-instr rom (assoc-in registers [:pc] pc))
    (printf "DONE\n")))
