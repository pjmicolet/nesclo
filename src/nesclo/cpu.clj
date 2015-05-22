(ns nesclo.cpu)

;; So this is a mutable state, which is BAD in FUNCTIONAL PROGRAMMING
(def instr-ops (atom {}))
(def mem (atom (byte-array 65536)))

;;Define how large an instruction is beyond the instruction itself
(def instr-size  [0 1 0 1 1 1 1 1 0 1 0 0 2 2 2 2 
                  1 1 0 1 1 1 1 1 0 2 0 2 2 2 2 2
                  2 1 0 1 1 1 1 1 0 1 0 0 2 2 2 2
                  1 1 0 1 1 1 1 1 0 2 0 2 2 2 2 2
                  0 1 0 1 1 1 1 1 0 1 0 0 2 2 2 2
                  1 1 0 1 1 1 1 1 0 2 0 2 2 2 2 2
                  0 1 0 1 1 1 1 1 0 1 0 0 2 2 2 2
                  1 1 0 1 1 1 1 1 0 2 0 2 2 2 2 2
                  1 1 0 1 1 1 1 1 0 0 0 0 2 2 2 2
                  1 1 0 0 1 1 1 1 0 2 0 0 0 2 0 0
                  1 1 1 1 1 1 1 1 0 1 0 0 2 2 2 2
                  1 1 0 1 1 1 1 1 0 2 0 0 2 2 2 2
                  1 1 0 1 1 1 1 1 0 1 0 0 2 2 2 2 
                  1 1 0 1 1 1 1 1 0 2 0 2 2 2 2 2 
                  1 1 0 1 1 1 1 1 0 1 0 1 2 2 2 2 
                  1 1 0 1 1 1 1 1 0 2 0 1 2 2 2 2])

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
;; 12 zp-addr
;; 13 abs-addr
;; 14 absx-addr
;; 15 absy-addr
;; 16 zpx-addr
;; 17 zpy-addr 
;; 18 indirect index, X address
;; 19 indirect index, Y address

(def type-addr-diss [0 5 0 5 2 2 2 2 0 1 0 1 7 7 7 7 
                    11 6 0 6 3 3 3 3 0 9 0 9 8 8 8 8 
                     7 5 0 5 2 2 2 2 0 1 0 1 7 7 7 7 
                    11 6 0 6 3 3 3 3 0 9 0 9 8 8 8 8 
                     0 5 0 5 2 2 2 2 0 1 0 1 7 7 7 7 
                    11 6 0 6 3 3 3 3 0 9 0 9 8 8 8 8 
                     0 5 0 5 2 2 2 2 0 1 0 1 10 7 7 7 
                    11 6 0 6 3 3 3 3 0 9 0 9 8 8 8 8 
                     1 18 1 18 12 12 12 12 0 1 0 1 13 13 13 13 
                    11 19 0 6 16 16 17 17 0 15 0 9 8 14 8 8 
                     1 5 1 5 2 2 2 2 0 1 0 1 7 7 7 7 
                    11 6 0 6 3 3 4 4 0 9 0 9 8 8 9 9 
                     1 5 0 5 2 2 2 2 0 1 0 1 7 7 7 7 
                    11 6 0 6 3 3 3 3 0 9 0 9 8 8 8 8 
                     1 5 1 5 2 2 2 2 0 1 0 1 7 7 7 7 
                    11 6 0 6 3 3 3 3 0 9 0 9 8 8 8 8 
                     1 5 1 5 2 2 2 2 0 1 0 1 7 7 7 7 
                    11 6 0 6 3 3 3 3 0 9 0 9 8 8 8 8])

 
(defn assoc-all [vect keys value]
  (reduce #(assoc %1 %2 value) vect keys))

(defmacro def-instr
  [name opcode args & code]
  `(do (defn ~name ~args
         ~@code)
       (swap! instr-ops assoc-all ~opcode ~name)))

(defmacro get-byte
  [memory & size]
  `(bit-and (get ~memory ~@size) 0xFF))

(defn addr [mem pc size]
  (let [ byte (bit-and (get-byte @mem (+ pc size)) 0xFF)]
  (if (= size 0)
    0
    (condp = size
      1 byte
      2 (+ (bit-shift-left byte 8) (addr mem pc (- size 1)))))))

(defn grab-data [mode regs mem]
  (condp = mode
    1 (addr mem (get regs :pc) 1)
    2 (get-byte @mem (addr mem (get regs :pc) 1))
    3 (do
        (let [ pc (get regs :pc)
               x (get regs :x)
               zp-addr (addr mem pc 1)
               addr (bit-and (+ x zp-addr) 0xFF)]
          (bit-and (get-byte @mem addr) 0xFF)))
    4 (do
        (let [ pc (get regs :pc)
               y (get regs :y)
               zp-addr (addr mem pc 1)
               addr (bit-and (+ y zp-addr) 0xFF)]
          (bit-and (get-byte @mem addr) 0xFF)))
    5 (do 
        (let [ pc (get regs :pc)
               x (get regs :x)
               operand (addr mem pc 1)
               addr-address (bit-and (+ operand x) 0x0FF)
               address-part1 (get-byte @mem (bit-and addr-address 0x0FF))
               address-part2 (get-byte @mem (bit-and (+ addr-address 1) 0x0FF))
               address (+ (bit-shift-left address-part2 8) (bit-and address-part1 0x00FF))]
        (bit-and (get-byte @mem address) 0xFF)))
    6 (do
        (let [ pc (get regs :pc)
               y (get regs :y)
               operand (addr mem pc 1)
               address-part1 (get-byte @mem (bit-and operand 0x0FF))
               address-part2 (get-byte @mem (bit-and (+ operand 1) 0x0FF))
               address (bit-and (+ (+ (bit-shift-left address-part2 8) (bit-and address-part1 0x00FF)) y) 0xFFFF)]
          (bit-and (get-byte @mem address) 0xFF)))
    7 (get-byte @mem (addr mem (get regs :pc) 2))
    8 (do
        (let [ pc (get regs :pc)
               x (get regs :x)
               abs-addr (addr mem pc 2)
               address (bit-and (+ abs-addr x) 0xFFFF)]
          (bit-and (get-byte @mem address) 0xFF)))
    9 (do
        (let [ pc (get regs :pc)
               y (get regs :y)
               abs-addr (addr mem pc 2)
               address (bit-and (+ abs-addr y) 0xFFFF)]
          (bit-and (get-byte @mem address) 0xFF)))
    10 (do
        (let [ pc (get regs :pc)
               operand (addr mem pc 2)
               address-part1 (get-byte @mem (bit-and operand 0x0FFFF))
               address-part2 (get-byte @mem (bit-or (bit-and operand 0xFF00) (bit-and (+ operand 1) 0x00FF)))]
          (+ (bit-shift-left address-part2 8) (bit-and address-part1 0x00FF))))
    12 (addr mem (get regs :pc) 1)
    13 (addr mem (get regs :pc) 2)
    14 (do
        (let [ pc (get regs :pc)
               x (get regs :x)
               abs-addr (addr mem pc 2)]
          (bit-and (+ abs-addr x) 0xFFFF)))
    15 (do
        (let [ pc (get regs :pc)
               y (get regs :y)
               abs-addr (addr mem pc 2)]
          (bit-and (+ abs-addr y) 0xFFFF)))
    16 (do
        (let [ pc (get regs :pc)
               x (get regs :x)
               zp-addr (addr mem pc 1)]
           (bit-and (+ x zp-addr) 0xFF)))
    17 (do
        (let [ pc (get regs :pc)
               y (get regs :y)
               zp-addr (addr mem pc 1)]
           (bit-and (+ y zp-addr) 0xFF)))
    18 (do 
        (let [ pc (get regs :pc)
               x (get regs :x)
               operand (addr mem pc 1)
               addr-address (bit-and (+ operand x) 0x0FF)
               address-part1 (get-byte @mem (bit-and addr-address 0x0FF))
               address-part2 (get-byte @mem (bit-and (+ addr-address 1) 0x0FF))]
               (+ (bit-shift-left address-part2 8) (bit-and address-part1 0x00FF))))
    19 (do
        (let [ pc (get regs :pc)
               y (get regs :y)
               operand (addr mem pc 1)
               address-part1 (get-byte @mem (bit-and operand 0x0FF))
               address-part2 (get-byte @mem (bit-and (+ operand 1) 0x0FF))]
               (bit-and (+ (+ (bit-shift-left address-part2 8) (bit-and address-part1 0x00FF)) y) 0xFFFF)))))

(def-instr ora '(0x01 0x05 0x09 0x0D 0x11 0x19 0x15 0x1D) [mem regs]
  (let [ pc (get regs :pc)
         p (get regs :p)
         x (get regs :x)
         mode (get type-addr-diss (get-byte @mem pc))
         data (grab-data mode regs mem)
         new-a (bit-or (get regs :a) data)
         zero (if (= 0 new-a) 0x2 0x0)
         overflow (if (= (bit-and new-a 0x80) 0x80) 0x80 0x0)
         overzero (bit-or overflow zero)
         status (bit-and (bit-or p overzero) (bit-or overzero 0x7D)) ]
    (-> regs
        (assoc-in [:p] status)
        (assoc-in [:a] new-a)
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr slo '(0x03 0x07 0x0F 0x13 0x17 0x1B 0x1F) [mem regs]
  (let [ pc (get regs :pc)
         p (get regs :p)
         mode (get type-addr-diss (get-byte @mem pc))
         data (grab-data mode regs mem)
         address (condp = mode
                     2 (addr mem (get regs :pc) 1)
                     5 (grab-data 18 regs mem)
                     6 (grab-data 19 regs mem)
                     7 (addr mem (get regs :pc) 2)
                     3 (grab-data 16 regs mem)
                     8 (grab-data 14 regs mem)
                     9 (grab-data 15 regs mem))
         shifted-zp (bit-and (bit-shift-left data 1) 0x0FF)
         new-a (bit-or (get regs :a) shifted-zp)
         zero (if (= 0 new-a) 0x2 0x0)
         carry (if (= (bit-and data 0x80) 0x80) 0x01 0x00)
         overflow (if (= (bit-and new-a 0x80) 0x80) 0x80 0x0)
         overzero (bit-or overflow zero carry)
         status (bit-and (bit-or p overzero) (bit-or overzero 0x7C)) ]
    (aset-byte @mem address (unchecked-byte shifted-zp))
    (-> regs
        (assoc-in [:p] status)
        (assoc-in [:a] new-a)
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr asl-zp '(0x06 0x16 0x1E) [mem regs]
  (let [ pc (get regs :pc)
         mode (get type-addr-diss (get-byte @mem pc))
         address (condp = mode
                     2 (addr mem (get regs :pc) 1)
                     7 (addr mem (get regs :pc) 2)
                     3 (grab-data 16 regs mem)
                     8 (grab-data 14 regs mem))
         zp (grab-data mode regs mem)
         shifted-zp (bit-and (bit-shift-left zp 1) 0x0FF)
         p (get regs :p)
         zero (if (= shifted-zp 0x0) 0x02 0x00)
         carry (if (= (bit-and zp 0x80) 0x80) 0x01 0x00)
         neg (if (= (bit-and shifted-zp 0x80) 0x80) 0x80 0x00)
         status (bit-or zero carry neg)
         meta-status (bit-and (bit-or p status) (bit-or status 0x7C))]
    (aset-byte @mem address (unchecked-byte shifted-zp))
    (-> regs
        (assoc-in [:p] meta-status)
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr php '(0x08) [mem regs]
  (let [ p (get regs :p) ]
    (aset-byte @mem (bit-or 0x100 (get regs :s))  (bit-or (unchecked-byte p) 0x10))
    (-> regs
        (assoc-in [:s] (- (get regs :s) 1))
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr asl-acc '(0x0A) [mem regs]
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
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr asl-abs '(0x0E) [mem regs]
  (let [ address (addr mem (get regs :pc) 2)
         abs (bit-and (get-byte @mem address) 0x0FF)
         shifted-abs (bit-and (bit-shift-left abs 1) 0x0FF)
         p (get regs :p)
         zero (if (= shifted-abs 0x0) 0x02 0x00)
         carry (if (= (bit-and abs 0x80) 0x80) 0x01 0x00)
         neg (if (= (bit-and shifted-abs 0x80) 0x80) 0x80 0x00)
         status (bit-or zero carry neg)
         meta-status (bit-and (bit-or p status) (bit-or status 0x7C))]
    (aset-byte @mem address (unchecked-byte shifted-abs))
    (-> regs
        (assoc-in [:p] meta-status)
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr bpl '(0x10) [mem regs]
  (if (= (bit-and (get regs :p) 0x80) 0x00)
    (assoc-in regs [:pc] (+ (+ (get regs :pc) (addr mem (get regs :pc) 1)) 2))
    (assoc-in regs [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1)))))

(def-instr clc '(0x18) [mem regs]
  (-> regs
      (assoc-in [:p] (bit-and (get regs :p) 0xFE))
      (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1)))))

(def-instr jsr '(0x20) [mem regs]
  (let [ new-stack (get regs :s) 
        size (get instr-size 0x20)
        return-address (+ (get regs :pc) 0x2)
        top-addr (bit-shift-right (bit-and return-address 0xFF00) 8)
        low-addr (bit-and return-address 0x00FF)]
    (aset-byte @mem (bit-or 0x100 new-stack) (unchecked-byte top-addr))
    (aset-byte @mem (bit-or 0x100 (- new-stack 1)) (unchecked-byte low-addr))
    (-> regs
        (assoc-in [:s] (- new-stack 2))
        (assoc-in [:pc] (addr mem (get regs :pc) size)))))

(def-instr and-inst '(0x21 0x25 0x29 0x2D 0x31 0x35 0x39 0x3D) [mem regs]
  (let [ p (get regs :p)
         x (get regs :x)
         pc (get regs :pc)
         mode (get type-addr-diss (get-byte @mem pc))
         data (grab-data mode regs mem)
         anded-a (bit-and data (get regs :a))
         overflow (bit-and anded-a 0x80)
         zero (if (= anded-a 0) 0x02 0x00)
         apply-stat (bit-or zero overflow)
         stat (bit-and (bit-or  p apply-stat) (bit-or apply-stat 0x7D))]
    (-> regs
        (assoc-in [:a] anded-a)
        (assoc-in [:p] stat)
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr rla '(0x23 0x27 0x2F 0x33 0x37 0x3B 0x3F) [mem regs]
  (let [ pc (get regs :pc)
         p (get regs :p)
         mode (get type-addr-diss (get-byte @mem pc))
         data (grab-data mode regs mem)
         address (condp = mode
                     2 (addr mem (get regs :pc) 1)
                     5 (grab-data 18 regs mem)
                     6 (grab-data 19 regs mem)
                     7 (addr mem (get regs :pc) 2)
                     3 (grab-data 16 regs mem)
                     8 (grab-data 14 regs mem)
                     9 (grab-data 15 regs mem))
         shifted-zp (bit-or (bit-and (bit-shift-left data 1) 0x0FF) (bit-and 0x01 p))
         carry (if (= (bit-and data 0x80) 0x80) 0x01 0x00)
         anded-a (bit-and shifted-zp (get regs :a))
         neg (bit-and anded-a 0x80)
         zero (if (= anded-a 0) 0x02 0x00)
         apply-stat (bit-or zero neg carry)
         stat (bit-and (bit-or  p apply-stat) (bit-or apply-stat 0x7C))]
    (aset-byte @mem address (unchecked-byte shifted-zp))
    (-> regs
        (assoc-in [:a] anded-a)
        (assoc-in [:p] stat)
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr bit '(0x24 0x2C) [mem regs]
  (let [ pc (get regs :pc)
         mode (get type-addr-diss (get-byte @mem pc))
         data (grab-data mode regs mem)
         p (get regs :p)
         six-seven (bit-or (bit-and data 0x80) (bit-and data 0x40))
         a (get regs :a)
         toset (if (= (bit-and data a) 0x00) 0x02 0x00)
         final-stat (bit-or six-seven toset)
         stat (bit-and (bit-or p final-stat) (bit-or final-stat 0x3D))]
    (-> regs
        (assoc-in [:p] stat)
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr rol-zp '(0x26 0x36 0x3E) [mem regs]
  (let [ pc (get regs :pc)
         mode (get type-addr-diss (get-byte @mem pc))
         address (condp = mode
                     2 (addr mem (get regs :pc) 1)
                     7 (addr mem (get regs :pc) 2)
                     3 (grab-data 16 regs mem)
                     8 (grab-data 14 regs mem))
         zp (grab-data mode regs mem)
         p (get regs :p)
         shifted-zp (bit-or (bit-and (bit-shift-left zp 1) 0x0FF) (bit-and 0x01 p))
         zero (if (= shifted-zp 0x0) 0x02 0x00)
         carry (if (= (bit-and zp 0x80) 0x80) 0x01 0x00)
         neg (if (= (bit-and shifted-zp 0x80) 0x80) 0x80 0x00)
         status (bit-or zero carry neg)
         meta-status (bit-and (bit-or p status) (bit-or status 0x7C))]
    (aset-byte @mem address (unchecked-byte shifted-zp))
    (-> regs
        (assoc-in [:p] meta-status)
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr plp '(0x28) [mem regs]
  (let [ new-stack (+ (get regs :s) 1)
         data (bit-or (bit-and (bit-and (get-byte @mem (bit-or 0x100 new-stack)) 0xFF) 0xEF) 0x20) ]
    (-> regs
        (assoc-in [:p] data)
        (assoc-in [:s] new-stack)
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr rol-acc '(0x2A) [mem regs]
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
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr rol-abs '(0x2E) [mem regs]
  (let [ address (addr mem (get regs :pc) 2)
         abs (bit-and (get-byte @mem address) 0x0FF)
         p (get regs :p)
         shifted-abs (bit-or (bit-and (bit-shift-left abs 1) 0x0FF) (bit-and 0x01 p))
         zero (if (= shifted-abs 0x0) 0x02 0x00)
         carry (if (= (bit-and abs 0x80) 0x80) 0x01 0x00)
         neg (if (= (bit-and shifted-abs 0x80) 0x80) 0x80 0x00)
         status (bit-or zero carry neg)
         meta-status (bit-and (bit-or p status) (bit-or status 0x7C))]
    (aset-byte @mem address (unchecked-byte shifted-abs))
    (-> regs
        (assoc-in [:p] meta-status)
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr bmi '(0x30) [mem regs]
  (let [ data (addr mem (get regs :pc) 1)
         state (get regs :p)]
    (-> regs
        (assoc-in [:pc] ( if (= (bit-and state 0x80) 0x80) (+ (+ (get regs :pc) data) 2) (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1)))))))

(def-instr sec '(0x38) [mem regs]
  (-> regs
    (assoc-in [:p] (bit-or (get regs :p) 0x01))
    (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1)))))

(def-instr rti '(0x40) [mem regs]
  (let [ s (get regs :s)
         p-reg-pos (+ s 1)
         pc-part1 (+ s 2)
         pc-part2 (+ s 3)
         p (bit-or (bit-and (get-byte @mem (bit-or 0x100 p-reg-pos)) 0x0FF) 0x20)
         low-addr (bit-and (get-byte @mem (bit-or 0x100 pc-part1)) 0x0FF)
         top-addr (bit-and (get-byte @mem (bit-or 0x100 pc-part2)) 0x0FF)
         new-pc (+ (bit-shift-left top-addr 8) (bit-and low-addr 0x00FF))]
    (-> regs
        (assoc-in [:p] p)
        (assoc-in [:s] pc-part2)
        (assoc-in [:pc] new-pc))))

(def-instr eor '(0x41 0x45 0x49 0x4D 0x51 0x55 0x59 0x5D) [mem regs]
  (let [ p (get regs :p)
         x (get regs :x)
         pc (get regs :pc)
         mode (get type-addr-diss (get-byte @mem pc))
         data (grab-data mode regs mem)
         a (get regs :a)
         new-a (bit-xor data a)
         zero (if (= 0 new-a) 0x2 0x0)
         overflow (if (= (bit-and new-a 0x80) 0x80) 0x80 0x0)
         overzero (bit-or overflow zero)
         status (bit-and (bit-or p overzero) (bit-or overzero 0x7D))]
    (-> regs
        (assoc-in [:p] status)
        (assoc-in [:a] new-a)
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr sre '(0x43 0x47 0x4F 0x53 0x57 0x5B 0x5F) [mem regs]
  (let [ pc (get regs :pc)
         p (get regs :p)
         mode (get type-addr-diss (get-byte @mem pc))
         data (grab-data mode regs mem)
         address (condp = mode
                     2 (addr mem (get regs :pc) 1)
                     5 (grab-data 18 regs mem)
                     6 (grab-data 19 regs mem)
                     7 (addr mem (get regs :pc) 2)
                     3 (grab-data 16 regs mem)
                     8 (grab-data 14 regs mem)
                     9 (grab-data 15 regs mem))
         shifted-zp (bit-shift-right data 1)
         a (get regs :a)
         carry (if (= (bit-and data 0x01) 0x01) 0x01 0x00)
         new-a (bit-xor shifted-zp a)
         zero (if (= 0 new-a) 0x2 0x0)
         overflow (if (= (bit-and new-a 0x80) 0x80) 0x80 0x0)
         overzero (bit-or overflow zero carry)
         status (bit-and (bit-or p overzero) (bit-or overzero 0x7C))]
    (aset-byte @mem address (unchecked-byte shifted-zp))
    (-> regs
        (assoc-in [:p] status)
        (assoc-in [:a] new-a)
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr lsr-zp '(0x46 0x56 0x5E) [mem regs]
  (let [ pc (get regs :pc)
         mode (get type-addr-diss (get-byte @mem pc))
         address (condp = mode
                     2 (addr mem (get regs :pc) 1)
                     7 (addr mem (get regs :pc) 2)
                     3 (grab-data 16 regs mem)
                     8 (grab-data 14 regs mem))
         zp (grab-data mode regs mem)
         shifted-zp (bit-shift-right zp 1)
         p (get regs :p)
         zero (if (= shifted-zp 0x0) 0x02 0x00)
         carry (if (= (bit-and zp 0x01) 0x01) 0x01 0x00)
         neg (if (= (bit-and shifted-zp 0x80) 0x80) 0x80 0x00)
         status (bit-or zero carry neg)
         meta-status (bit-and (bit-or p status) (bit-or status 0x7C))]
    (aset-byte @mem address (unchecked-byte shifted-zp))
    (-> regs
        (assoc-in [:p] meta-status)
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr pha '(0x48) [mem regs]
  (aset-byte @mem (bit-or 0x100 (get regs :s)) (unchecked-byte (get regs :a)))
  (-> regs
      (assoc-in [:s] (- (get regs :s) 1))
      (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1)))))

(def-instr lsr-acc '(0x4A) [mem regs]
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
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr jmp-abs '(0x4C) [mem regs]
  (let [ pc (get regs :pc)
         size (get instr-size 0x4C)
         address (addr mem pc size)]
    (assoc-in regs [:pc] address)))

(def-instr lsr-abs '(0x4E) [mem regs]
  (let [ address (addr mem (get regs :pc) 2)
         abs (bit-and (get-byte @mem address) 0x0FF)
         shifted-abs (bit-shift-right abs 1)
         p (get regs :p)
         zero (if (= shifted-abs 0x0) 0x02 0x00)
         carry (if (= (bit-and abs 0x01) 0x01) 0x01 0x00)
         neg (if (= (bit-and shifted-abs 0x80) 0x80) 0x80 0x00)
         status (bit-or zero carry neg)
         meta-status (bit-and (bit-or p status) (bit-or status 0x7C))]
    (aset-byte @mem address (unchecked-byte shifted-abs))
    (-> regs
        (assoc-in [:p] meta-status)
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr bvc '(0x50) [mem regs]
  (if (= (bit-and (get regs :p) 0x40) 0x00)
    (assoc-in regs [:pc] (+ (+ (get regs :pc) (addr mem (get regs :pc) 1)) 2))
    (assoc-in regs [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1)))))

(def-instr rts '(0x60) [mem regs]
  (let [ new-stack (+ (bit-and (get regs :s) 0x0FF) 1)
         low-addr (bit-and (get-byte @mem (bit-or 0x100 new-stack)) 0x0FF)
         top-addr (bit-and (get-byte @mem (bit-or 0x100 (+ new-stack 1))) 0x0FF)
         new-pc (+ (bit-shift-left top-addr 8) (bit-and low-addr 0x00FF))]
    (-> regs
      (assoc-in [:pc] (+ new-pc 1))
      (assoc-in [:s] (+ new-stack 1)))))

(def-instr adc '(0x61 0x65 0x69 0x6D 0x71 0x75 0x79 0x7D) [mem regs]
  (let [ pc (get regs :pc)
         p (get regs :p)
         x (get regs :x)
         mode (get type-addr-diss (get-byte @mem pc))
         data (grab-data mode regs mem)
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
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr rra '(0x63 0x67 0x6F 0x73 0x77 0x7B 0x7F) [mem regs]
  (let [ pc (get regs :pc)
         p (get regs :p)
         mode (get type-addr-diss (get-byte @mem pc))
         data (grab-data mode regs mem)
         address (condp = mode
                     2 (addr mem (get regs :pc) 1)
                     5 (grab-data 18 regs mem)
                     6 (grab-data 19 regs mem)
                     7 (addr mem (get regs :pc) 2)
                     3 (grab-data 16 regs mem)
                     8 (grab-data 14 regs mem)
                     9 (grab-data 15 regs mem))
         shifted (bit-or (bit-shift-right data 1) (bit-shift-left (bit-and p 0x01) 7))
         shifted-zp (if ( > (double (/ data 2)) (double shifted)) (+ shifted 1) shifted)
         carry (if (= (bit-and (get regs :p) 0x1) 0x1) 1 0)
         a (get regs :a)
         p (get regs :p)
         adc (bit-and (+ shifted-zp a carry) 0xFF)
         zero (if (= 0 adc) 0x02 0x00)
         overflow (if (= 0 (bit-and (bit-not (bit-xor a shifted-zp)) (bit-xor a adc) 0x80)) 0 0x40)
         negative (if (= (bit-and adc 0x80) 0x80) 0x80 0)
         carry-flag (if (= 
                         (bit-and (+ (bit-and a 0xFFF) (bit-and shifted-zp 0xFFF) (bit-and carry 0xFFF)) 0x100) 0x100)
                      0x01 0x00)
         status (bit-or overflow negative carry-flag zero)
         meta-status (bit-and (bit-or p status) (bit-or status 0x3C))]
    (aset-byte @mem address (unchecked-byte shifted))
    (-> regs
        (assoc-in [:p] meta-status)
        (assoc-in [:a] adc)
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr ror-zp '(0x66 0x76 0x7E) [mem regs]
  (let [ pc (get regs :pc)
         mode (get type-addr-diss (get-byte @mem pc))
         address (condp = mode
                     2 (addr mem (get regs :pc) 1)
                     7 (addr mem (get regs :pc) 2)
                     3 (grab-data 16 regs mem)
                     8 (grab-data 14 regs mem))
         zp (grab-data mode regs mem)
         p (get regs :p)
         shifted-zp (bit-or (bit-shift-right zp 1) (bit-shift-left (bit-and p 0x01) 7))
         zero (if (= shifted-zp 0x0) 0x02 0x00)
         carry (if (= (bit-and zp 0x01) 0x01) 0x01 0x00)
         neg (if (= (bit-and shifted-zp 0x80) 0x80) 0x80 0x00)
         status (bit-or zero carry neg)
         meta-status (bit-and (bit-or p status) (bit-or status 0x7C))]
    (aset-byte @mem address (unchecked-byte shifted-zp))
    (-> regs
        (assoc-in [:p] meta-status)
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr pla '(0x68) [mem regs]
  (let [ new-stack (+ (get regs :s) 1)
         p (get regs :p)
         data (bit-and (get-byte @mem (bit-or 0x100 new-stack)) 0xFF)
         zero (if (= 0 data) 0x2 0x0)
         overflow (if (= (bit-and data 0x80) 0x80) 0x80 0x0)
         overzero (bit-or overflow zero)
         status (bit-and (bit-or p overzero) (bit-or overzero 0x7D))]
    (-> regs
        (assoc-in [:a] data)
        (assoc-in [:p] status)
        (assoc-in [:s] new-stack)
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr ror-acc '(0x6A) [mem regs]
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
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr jmp-ind '(0x6C) [mem regs]
  (let [ pc (get regs :pc)
         operand (addr mem pc 2)
         address-part1 (get-byte @mem (bit-and operand 0x0FFFF))
         address-part2 (get-byte @mem (bit-or (bit-and operand 0xFF00) (bit-and (+ operand 1) 0x00FF)))
         address (+ (bit-shift-left address-part2 8) (bit-and address-part1 0x00FF))]
    (assoc-in regs [:pc] (bit-and address 0x0FFFF))))

(def-instr ror-abs '(0x6E) [mem regs]
  (let [ address (addr mem (get regs :pc) 2)
         abs (bit-and (get-byte @mem address) 0x0FF)
         p (get regs :p)
         shifted-abs (bit-or (bit-shift-right abs 1) (bit-shift-left (bit-and p 0x01) 7))
         zero (if (= shifted-abs 0x0) 0x02 0x00)
         carry (if (= (bit-and abs 0x01) 0x01) 0x01 0x00)
         neg (if (= (bit-and shifted-abs 0x80) 0x80) 0x80 0x00)
         status (bit-or zero carry neg)
         meta-status (bit-and (bit-or p status) (bit-or status 0x7C))]
    (aset-byte @mem address (unchecked-byte shifted-abs))
    (-> regs
        (assoc-in [:p] meta-status)
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr bvs '(0x70) [mem regs]
  (if (= (bit-and (get regs :p) 0x40) 0x40)
    (assoc-in regs [:pc] (+ (+ (get regs :pc) (addr mem (get regs :pc) 1)) 2))
    (assoc-in regs [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1)))))

(def-instr sei '(0x78) [mem regs]
  (let [ int-flag (bit-or (get regs :p) 0x4)]
    (-> regs
        (assoc-in [:p] int-flag)
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr sta '(0x81 0x85 0x8D 0x91 0x95 0x99 0x9D) [mem regs]
  (let [ pc (get regs :pc)
         mode (get type-addr-diss (get-byte @mem pc))
         address (grab-data mode regs mem)]
  (aset-byte @mem address (unchecked-byte (get regs :a)))
  (assoc-in regs [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1)))))

(def-instr sax '(0x83 0x87 0x8F 0x97) [mem regs]
  (let [ pc (get regs :pc)
         mode (get type-addr-diss (get-byte @mem pc))
         address (grab-data mode regs mem)]
  (aset-byte @mem address (bit-and (unchecked-byte (get regs :a)) (unchecked-byte (get regs :x))))
  (assoc-in regs [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1)))))

(def-instr sty '(0x84 0x8C 0x94) [mem regs]
  (let [ pc (get regs :pc)
         mode (get type-addr-diss (get-byte @mem pc))
         address (grab-data mode regs mem)]
  (aset-byte @mem address (unchecked-byte (get regs :y)))
  (assoc-in regs [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1)))))

(def-instr stx '(0x86 0x8E 0x96) [mem regs]
  (let [ pc (get regs :pc)
         mode (get type-addr-diss (get-byte @mem pc))
         address (grab-data mode regs mem)]
  (aset-byte @mem address (unchecked-byte (get regs :x)))
  (assoc-in regs [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1)))))

(def-instr dey '(0x88) [mem regs]
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
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr txa '(0x8A) [mem regs]
  (let [x (get regs :x)
        p (get regs :p)
        zero (if (= x 0) 0x02 0x00)
        neg-flag (if (= (bit-and x 0x80) 0x80) 0x80 0x00)
        status (bit-or zero neg-flag)
        meta-status (bit-and (bit-or p status) (bit-or status 0x7D))]
    (-> regs
        (assoc-in [:p] meta-status)
        (assoc-in [:a] x)
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr bcc '(0x90) [mem regs]
  (if (= (bit-and (get regs :p) 0x01) 0x00)
    (assoc-in regs [:pc] (+ (+ (get regs :pc) (addr mem (get regs :pc) 1)) 2))
    (assoc-in regs [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1)))))

(def-instr tya '(0x98) [mem regs]
  (let [y (get regs :y)
        p (get regs :p)
        zero (if (= y 0) 0x02 0x00)
        neg-flag (if (= (bit-and y 0x80) 0x80) 0x80 0x00)
        status (bit-or zero neg-flag)
        meta-status (bit-and (bit-or p status) (bit-or status 0x7D))]
    (-> regs
        (assoc-in [:p] meta-status)
        (assoc-in [:a] y)
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr txs '(0x9A) [mem regs]
  (let [x (get regs :x)
        p (get regs :p)]
    (-> regs
        (assoc-in [:s] x)
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr ldy '(0xA0 0xA4 0xAC 0xB4 0xBC) [mem regs]
  (let [ pc (get regs :pc)
         p (get regs :p)
         mode (get type-addr-diss (get-byte @mem pc))
         data (grab-data mode regs mem)
         status (bit-or (if(= data 0x0) 0x02 0x0) (if(= (bit-and data 0x80) 0x80) 0x80 0x0))
         meta-stat (bit-and (bit-or p status) (bit-or status 0x7D))]
    (-> regs
        (assoc-in [:y] data)
        (assoc-in [:p] meta-stat)
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr lda '(0xA1 0xA5 0xA9 0xAD 0xB1 0xB5 0xB9 0xBD) [mem regs]
  (let [ pc (get regs :pc)
         p (get regs :p)
         mode (get type-addr-diss (get-byte @mem pc))
         data (grab-data mode regs mem)
         status (bit-or (if(= data 0x0) 0x02 0x0) (if(= (bit-and data 0x80) 0x80) 0x80 0x0))
         meta-stat (bit-and (bit-or p status) (bit-or status 0x7D))]
    (-> regs
        (assoc-in [:a] data)
        (assoc-in [:p] meta-stat)
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr ldx '(0xA2 0xA6 0xAE 0xB6 0xBE) [mem regs]
  (let [ pc (get regs :pc)
         p (get regs :p)
         mode (get type-addr-diss (get-byte @mem pc))
         data (grab-data mode regs mem)
         status (bit-or (if(= data 0x0) 0x02 0x0) (if(= (bit-and data 0x80) 0x80) 0x80 0x0))
         meta-stat (bit-and (bit-or p status) (bit-or status 0x7D))]
    (-> regs
        (assoc-in [:x] data)
        (assoc-in [:p] meta-stat)
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr lax '(0xA3 0xA7 0xAF 0xB3 0xB7 0xBF) [mem regs]
  (let [ pc (get regs :pc)
         p (get regs :p)
         mode (get type-addr-diss (get-byte @mem pc))
         data (grab-data mode regs mem)
         status (bit-or (if(= data 0x0) 0x02 0x0) (if(= (bit-and data 0x80) 0x80) 0x80 0x0))
         meta-stat (bit-and (bit-or p status) (bit-or status 0x7D))]
    (-> regs
        (assoc-in [:x] data)
        (assoc-in [:a] data)
        (assoc-in [:p] meta-stat)
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr tay '(0xA8) [mem regs]
  (let [a (get regs :a)
        p (get regs :p)
        zero (if (= a 0) 0x02 0x00)
        neg-flag (if (= (bit-and a 0x80) 0x80) 0x80 0x00)
        status (bit-or zero neg-flag)
        meta-status (bit-and (bit-or p status) (bit-or status 0x7D))]
    (-> regs
        (assoc-in [:p] meta-status)
        (assoc-in [:y] a)
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr tax '(0xAA) [mem regs]
  (let [a (get regs :a)
        p (get regs :p)
        zero (if (= a 0) 0x02 0x00)
        neg-flag (if (= (bit-and a 0x80) 0x80) 0x80 0x00)
        status (bit-or zero neg-flag)
        meta-status (bit-and (bit-or p status) (bit-or status 0x7D))]
    (-> regs
        (assoc-in [:p] meta-status)
        (assoc-in [:x] a)
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr bcs '(0xB0) [mem regs]
  (if (= (bit-and (get regs :p) 0x01) 0x01)
    (assoc-in regs [:pc] (+ (+ (get regs :pc) (addr mem (get regs :pc) 1) 2)))
    (assoc-in regs [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1)))))

(def-instr clv '(0xB8) [mem regs]
  (let [ p (bit-and (get regs :p) 0xBF) ]
    (-> regs
        (assoc-in [:p] p)
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr tsx '(0xBA) [mem regs]
  (let [s (get regs :s)
        p (get regs :p)
        zero (if (= s 0) 0x02 0x00)
        neg-flag (if (= (bit-and s 0x80) 0x80) 0x80 0x00)
        status (bit-or zero neg-flag)
        meta-status (bit-and (bit-or p status) (bit-or status 0x7D))]
    (-> regs
        (assoc-in [:p] meta-status)
        (assoc-in [:x] s)
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr cpy '(0xC0 0xC4 0xCC) [mem regs]
  (let [ pc (get regs :pc)
         p (get regs :p)
         mode (get type-addr-diss (get-byte @mem pc))
         data (grab-data mode regs mem)
         is-bigger (if (> (get regs :y) data) 0x01 0x00)
         is-equal (if (= (get regs :y) data) 0x03 0x00)
         is-neg (if (= (bit-and (- (get regs :y) data) 0x80)0x80) 0x80 0x00)
         set-status (bit-or is-bigger is-equal is-neg)
         status (bit-and (bit-or p set-status) (bit-or set-status 0x7C))]
    (-> regs
        (assoc-in [:p] status)
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr cmp '(0xC1 0xC5 0xC9 0xCD 0xD1 0xD5 0xD9 0xDD) [mem regs]
  (let [ x (get regs :x)
         pc (get regs :pc)
         p (get regs :p)
         mode (get type-addr-diss (get-byte @mem pc))
         data (grab-data mode regs mem)
         is-bigger (if (> (get regs :a) data) 0x01 0x00)
         is-equal (if (= (get regs :a) data) 0x03 0x00)
         is-neg (if (= (bit-and (- (get regs :a) data) 0x80)0x80) 0x80 0x00)
         set-status (bit-or is-bigger is-equal is-neg)
         status (bit-and (bit-or p set-status) (bit-or set-status 0x7C))]
    (-> regs
        (assoc-in [:p] status)
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr dcp '(0xC3 0xC7 0xCF 0xD3 0xD7 0xDB 0xDF) [mem regs]
  (let [ pc (get regs :pc)
         p (get regs :p)
         mode (get type-addr-diss (get-byte @mem pc))
         data (grab-data mode regs mem)
         address (condp = mode
                     2 (addr mem (get regs :pc) 1)
                     5 (grab-data 18 regs mem)
                     6 (grab-data 19 regs mem)
                     7 (addr mem (get regs :pc) 2)
                     3 (grab-data 16 regs mem)
                     8 (grab-data 14 regs mem)
                     9 (grab-data 15 regs mem))
         
         dcp (bit-and (dec data) 0x0FF)
         is-neg (if (= (bit-and (- (get regs :a) dcp) 0x80)0x80) 0x80 0x00)
         is-bigger (if (> (get regs :a) dcp) 0x01 0x00)
         is-equal (if (= (get regs :a) dcp) 0x03 0x00)
         status (bit-or is-bigger is-neg is-equal)
         meta-status (bit-and (bit-or p status) (bit-or status 0x7C))]
    (aset-byte @mem address (unchecked-byte dcp))
    (-> regs
        (assoc-in [:p] meta-status)
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr dec-zp '(0xC6 0xD6 0xDE) [mem regs]
  (let [pc (get regs :pc)
        mode (get type-addr-diss (get-byte @mem pc))
        data (grab-data mode regs mem)
        address (condp = mode
                    2 (addr mem (get regs :pc) 1)
                    7 (addr mem (get regs :pc) 2)
                    3 (grab-data 16 regs mem)
                    8 (grab-data 14 regs mem))
        p (get regs :p)
        dec-zp (bit-and (dec data) 0x0FF)
        zero (if (= dec-zp 0) 0x02 0x00)
        neg-flag (if (= (bit-and dec-zp 0x80) 0x80) 0x80 0x00)
        status (bit-or zero neg-flag)
        meta-status (bit-and (bit-or p status) (bit-or status 0x7D))]
    (aset-byte @mem address (unchecked-byte dec-zp))
    (-> regs
        (assoc-in [:p] meta-status)
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr iny '(0xC8) [mem regs]
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
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr dex '(0xCA) [mem regs]
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
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr dec-abs '(0xCE) [mem regs]
  (let [pc (get regs :pc)
        address (addr mem pc 2)
        data (bit-and (get-byte @mem address) 0x0FF)
        p (get regs :p)
        dec-abs (bit-and (dec data) 0x0FF)
        zero (if (= dec-abs 0) 0x02 0x00)
        neg-flag (if (= (bit-and dec-abs 0x80) 0x80) 0x80 0x00)
        status (bit-or zero neg-flag)
        meta-status (bit-and (bit-or p status) (bit-or status 0x7D))]
    (aset-byte @mem address (unchecked-byte dec-abs))
    (-> regs
        (assoc-in [:p] meta-status)
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr bne '(0xD0) [mem regs]
  (let [ address (addr mem (get regs :pc) 1)]
  (if (= (bit-and (get regs :p) 0x02) 0x00)
    (assoc-in regs [:pc] (bit-and (+ (+ (get regs :pc) (unchecked-byte address)) 2) 0xFFFF) )
    (assoc-in regs [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr cld '(0xD8) [mem regs]
  (let [ status (bit-and (get regs :p) 0xF7)]
    (-> regs
        (assoc-in [:p] status)
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr cpx '(0xE0 0xE4 0xEC) [mem regs]
  (let [ pc (get regs :pc)
         p (get regs :p)
         mode (get type-addr-diss (get-byte @mem pc))
         data (grab-data mode regs mem)
         is-bigger (if (> (get regs :x) data) 0x01 0x00)
         is-equal (if (= (get regs :x) data) 0x03 0x00)
         is-neg (if (= (bit-and (- (get regs :x) data) 0x80)0x80) 0x80 0x00)
         set-status (bit-or is-bigger is-equal is-neg)
         status (bit-and (bit-or p set-status) (bit-or set-status 0x7C))]
    (-> regs
        (assoc-in [:p] status)
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr sbc-instr '(0xE1 0xE5 0xE9 0xED 0xF1 0xF5 0xF9 0xFD 0xEB) [mem regs]
  (let [ pc (get regs :pc)
         p (get regs :p)
         mode (get type-addr-diss (get-byte @mem pc))
         data (grab-data mode regs mem)
         carry (if (= (bit-and (get regs :p) 0x01) 0x01) 0x01 0x00)
         not-carry (bit-and (bit-not carry) 0x01)
         a (get regs :a)
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
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr isc '(0xE3 0xE7 0xEF 0xF3 0xF7 0xFB 0xFF) [mem regs]
  (let [ pc (get regs :pc)
         p (get regs :p)
         mode (get type-addr-diss (get-byte @mem pc))
         data (grab-data mode regs mem)
         address (condp = mode
                     2 (addr mem (get regs :pc) 1)
                     5 (grab-data 18 regs mem)
                     6 (grab-data 19 regs mem)
                     7 (addr mem (get regs :pc) 2)
                     3 (grab-data 16 regs mem)
                     8 (grab-data 14 regs mem)
                     9 (grab-data 15 regs mem))
         incr (bit-and (inc data) 0x0FF)
         a (get regs :a)
         carry (if (= (bit-and (get regs :p) 0x01) 0x01) 0x01 0x00)
         not-carry (bit-and (bit-not carry) 0x01)
         sbc (bit-and (- a incr not-carry) 0xFF)
         zero (if (= 0 sbc) 0x02 0x00)
         overflow (if (= 0 (bit-and (bit-xor a incr) (bit-xor a sbc) 0x80)) 0x00 0x40)
         negative (if (= (bit-and sbc 0x80) 0x80) 0x80 0)
         carry-flag (if (= 
                         (bit-and (- (bit-and a 0xFFF) (bit-and incr 0xFFF) (bit-and not-carry 0xFFF)) 0x100) 0x100)
                      0x00 0x01)
         status (bit-or overflow negative carry-flag zero)
         meta-status (bit-and (bit-or p status) (bit-or status 0x3C))]
    (aset-byte @mem address (unchecked-byte incr))
    (-> regs
        (assoc-in [:p] meta-status)
        (assoc-in [:a] sbc)
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))


(def-instr inc-zp '(0xE6 0xF6 0xFE) [mem regs]
  (let [p (get regs :p)
        pc (get regs :pc)
        mode (get type-addr-diss (get-byte @mem pc))
        data (grab-data mode regs mem)
        address (condp = mode
                    2 (addr mem (get regs :pc) 1)
                    7 (addr mem (get regs :pc) 2)
                    3 (grab-data 16 regs mem)
                    8 (grab-data 14 regs mem))
        inczp (bit-and (inc data) 0x0FF)
        zero (if (= inczp 0) 0x02 0x00)
        neg-flag (if (= (bit-and inczp 0x80) 0x80) 0x80 0x00)
        status (bit-or zero neg-flag)
        meta-status (bit-and (bit-or p status) (bit-or status 0x7D))]
    (aset-byte @mem address (unchecked-byte inczp))
    (-> regs
        (assoc-in [:p] meta-status)
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr inx '(0xE8) [mem regs]
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
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr nop '(0xEA 0x04 0x44 0x64 0x0C 0x14 0x34 0x54 0x74 0xD4 0xF4
                      0x1A 0x3A 0x5A 0x7A 0xDA 0xFA 0x80 0x1C 0x3C 0x5C
                      0x7C 0xDC 0xFC) [mem regs]
  (assoc-in regs [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))

(def-instr inc-abs '(0xEE) [mem regs]
  (let [p (get regs :p)
        pc (get regs :pc)
        address (addr mem pc 2)
        data (bit-and (get-byte @mem address) 0x0FF)
        incabs (bit-and (inc data) 0x0FF)
        zero (if (= incabs 0) 0x02 0x00)
        neg-flag (if (= (bit-and incabs 0x80) 0x80) 0x80 0x00)
        status (bit-or zero neg-flag)
        meta-status (bit-and (bit-or p status) (bit-or status 0x7D))]
    (aset-byte @mem address (unchecked-byte incabs))
    (-> regs
        (assoc-in [:p] meta-status)
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(def-instr beq '(0xF0) [mem regs]
  (if (= (bit-and (get regs :p) 0x02) 0x02)
    (assoc-in regs [:pc] (+ (+ (get regs :pc) (addr mem (get regs :pc) 1)) 2))
    (assoc-in regs [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1)))))

(def-instr sed '(0xF8) [mem regs]
  (let [ dec-flag (bit-or (get regs :p) 0x8)]
    (-> regs
        (assoc-in [:p] dec-flag)
        (assoc-in [:pc] (+ (get regs :pc) (+ (get instr-size (get-byte @mem (get regs :pc))) 1))))))

(defn disassemble [mem pc regs recurse]
  (let [ inst (get instr (get-byte @mem pc "No more PC") "Last Instruction") ]
  (when (not= inst "Last Instruction")
    (let [ addressing (get type-addr-diss (get-byte @mem pc "No pc") "Nil")
          size (get instr-size (get-byte @mem pc))]
    (condp = addressing
         0 (printf "0x%05x => %s " pc inst)
         1 (printf "0x%05x => %s #$%02x " pc inst (addr mem pc size))
         2 (printf "0x%05x => %s $%02x " pc inst (addr mem pc size))
         12 (printf "0x%05x => %s $%02x " pc inst (addr mem pc size))
         3 (printf "0x%05x => %s $%02x,X " pc inst (addr mem pc size))
         4 (printf "0x%05x => %s $%02x,Y " pc inst (addr mem pc size))
         16 (printf "0x%05x => %s $%02x,X " pc inst (addr mem pc size))
         17 (printf "0x%05x => %s $%02x,Y " pc inst (addr mem pc size))
         5 (printf "0x%05x => %s ($%02x,X) " pc inst (addr mem pc size))
         18 (printf "0x%05x => %s ($%02x,X) " pc inst (addr mem pc size))
         6 (printf "0x%05x => %s ($%02x,Y) " pc inst (addr mem pc size))
         19 (printf "0x%05x => %s ($%02x,Y) " pc inst (addr mem pc size))
         7 (printf "0x%05x => %s $%04x " pc inst (addr mem pc size))
         13 (printf "0x%05x => %s $%04x " pc inst (addr mem pc size))
         8 (printf "0x%05x => %s $%04x,X " pc inst (addr mem pc size))
         9 (printf "0x%05x => %s $%04x,Y " pc inst (addr mem pc size))
         14 (printf "0x%05x => %s $%04x,X " pc inst (addr mem pc size))
         15 (printf "0x%05x => %s $%04x,Y " pc inst (addr mem pc size))
         10 (printf "0x%05x => %s ($%04x) " pc inst (addr mem pc size))
      ;; 11 is wrong right now
         11 (printf "0x%05x => %s ($%04x) " pc inst (addr mem pc size)))
    (if (= addressing 0)
    (printf "\t\t\tA:%02x X:%02x Y:%02x P:%02x SP:%02x\n" (get regs :a) (get regs :x)
            (get regs :y) (get regs :p) (get regs :s))
    (printf "\t\tA:%02x X:%02x Y:%02x P:%02x SP:%02x\n" (get regs :a) (get regs :x)
            (get regs :y) (get regs :p) (get regs :s)))
    (when (= recurse 1)
    (recur @mem (+ size (inc pc)) regs 1))))))

(defn run [op mem regs]
  (op mem regs))

(defn execute-instr [mem regs]
  (let [ inst (get @instr-ops (get-byte @mem (get regs :pc)) "Last") ]
  (disassemble mem (get regs :pc) regs 0)
  (when (not= inst "Last")
    (recur mem (run inst mem regs)))))

(defn start-rom [nes-mem pc]
  ;; Will do some initialisation here
  (let [ registers {:pc 0xC000 :a 0x00 :x 0x00 :y 0x00 :s 0xFD :p 0x24}]
    (reset! mem nes-mem)
    (execute-instr mem (assoc-in registers [:pc] pc))
    (printf "DONE\n")))
