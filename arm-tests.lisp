;;;; -*- mode: Lisp; Syntax: ANSI-Common-Lisp; Package: ("ARM-TESTS" ("CL" "ARM" "RT")); -*-
;;;;
;;;; arm-tests.lisp
;;;;
;;;; Uses RT regression test framework

(cl:defpackage "ARM-TESTS"
  (:use "COMMON-LISP" "ARM" #+sbcl "SB-RT" #-sbcl "RT"))

(cl:in-package "ARM-TESTS")

(deftest and-encode-1
    (arm::encode (arm::opcode-to-instruction '(arm:and arm:r0 arm:r1
					       arm:r2)))
					;;and r0, r1, r2
  #b11100000000000010000000000000010)
   ;cond000 AND0nnnndddd00000000mmmm

(deftest and-encode-2
    (arm::encode (arm::opcode-to-instruction '(arm:and arm:r15 arm:r14
					       arm:r13)))
					;;and r15, r14, r13
  #b11100000000011101111000000001101)
   ;cond000 AND0nnnndddd00000000mmmm					     

(deftest and-encode-3
    (arm::encode (arm::opcode-to-instruction '((arm:and arm:s) arm:r15 arm:r14
					       arm:r13)))
					;;ands r15, r14, r13
  #b11100000000111101111000000001101)
   ;cond000 ANDSnnnndddd00000000mmmm					     

(deftest mvn-encode-1
    (arm::encode (arm::opcode-to-instruction '(arm:mvn arm:r3 arm:r4
					       arm:r5)))
					;;mvn r3, r4, r5
  #b11100001111001000011000000000101)
   ;cond000 MVN0nnnndddd00000000mmmm

(deftest imm-32-0 (arm::encode-32-bit-immediate 0)
  0 0)

(deftest imm-32-2 (arm::encode-32-bit-immediate 2)
  2 0)

(deftest imm-32-4 (arm::encode-32-bit-immediate 4)
  4 0)
; note: 2 15 would be invalid. < 256 must be encoded with 0 
; rotation

(deftest imm-32-ff (arm::encode-32-bit-immediate #xff)
  #b11111111 0)
						 
(deftest imm-32-104 (arm::encode-32-bit-immediate #b100000100)
  #b1000001 15)

(deftest imm-32-3f0 (arm::encode-32-bit-immediate #b1111110000)
  #b111111 14) 
; #b11111100 15 would also be valid, but prefer rotate_imm to be smallest 
; possible

(deftest imm-32-f000000f (arm::encode-32-bit-immediate #xf000000f)
  #xff 2)

(deftest imm-32-ff000000 (arm::encode-32-bit-immediate #xff000000)
  #xff 4)

;; impossible values
(deftest imm-32-101 (arm::encode-32-bit-immediate #x101)
  nil)

(deftest imm-32-102 (arm::encode-32-bit-immediate #x102)
  nil)

(deftest imm-32-ff1 (arm::encode-32-bit-immediate #xff1)
  nil)

(deftest imm-32-ff04 (arm::encode-32-bit-immediate #xff04)
  nil)

(deftest imm-32-ff003 (arm::encode-32-bit-immediate #xff003)
  nil)

(deftest imm-32-ffffffff (arm::encode-32-bit-immediate #xffffffff)
  nil)

(deftest imm-32-f000001f (arm::encode-32-bit-immediate #xf000001f)
  nil)

(deftest and-encode-4
    (arm::encode (arm::opcode-to-instruction '((arm:and arm:s) arm:r15 arm:r14
					       (arm:\# #xff00))))
					;;ands r15, r14, #xff00
  #b11100010000111101111110011111111)
   ;cond00I ANDSnnnnddddrot_IMMEDIAT					     

(deftest and-encode-5 ; update condition flags with s
    (arm::encode (arm::opcode-to-instruction '((arm:and arm:s) arm:r0 arm:r12
					       (arm:\# #x3f0))))
					;;ands r0, r12, #x3f0
  #b11100010000111000000111000111111)
   ;cond00I ANDSnnnnddddrot_IMMEDIAT					     

(deftest and-encode-6
    (arm::encode (arm::opcode-to-instruction '((arm:and arm:s arm:vs) 
					       arm:r0 arm:r12
					       (arm:\# #xab))))
					;;andvss r0, r12, #xab
  #b01100010000111000000000010101011)
   ;cond00I ANDSnnnnddddrot_IMMEDIAT					     

(deftest and-encode-7
    (arm::encode (arm::opcode-to-instruction '((arm:and arm:hs) 
					       arm:r0 arm:r12
					       (arm:\# #x3f0))))
					;;andhs r0, r12, #x3f0
  #b00100010000011000000111000111111)
   ;cond00I ANDSnnnnddddrot_IMMEDIAT					     

(deftest and-encode-8 ; s can occur after condition
    (arm::encode (arm::opcode-to-instruction '((arm:and arm:hi arm:s) 
					       arm:r0 arm:r12
					       (arm:\# #x3f0))))
					;;andvss r0, r12, #x3f0
  #b10000010000111000000111000111111)
   ;cond00I ANDSnnnnddddrot_IMMEDIAT					     

(deftest adc-encode-1 ; LSL immediate
    (arm::encode (arm::opcode-to-instruction '(arm:adc
					       arm:r0 arm:r12
					       (arm:r2 arm:lsl (arm:\# 1)))))
					;;adc r0, r12, r2 LSL #1
  #b11100000101011000000000010000010)
   ;cond000 ADCSnnnnddddSHFIMsh0mmmm					     

(deftest adc-encode-2 ; LSL register
    (arm::encode (arm::opcode-to-instruction '(arm:adc
					       arm:r0 arm:r12
					       (arm:r2 arm:lsl arm:r4))))
					;;adc r0, r12, r2 LSL r4
  #b11100000101011000000010000010010)
   ;cond000 ADCSnnnnddddssss0sh1mmmm					     

(deftest adc-encode-3 ; LSR immediate
    (arm::encode (arm::opcode-to-instruction '(arm:adc
					       arm:r0 arm:r12
					       (arm:r2 arm:lsr (arm:\# #x1a)))))
					;;adc r0, r12, r2 LSR #x1a
  #b11100000101011000000110100100010)
   ;cond000 ADCSnnnnddddSHFIMsh0mmmm					     


(deftest eor-encode-1 ; LSR register
    (arm::encode (arm::opcode-to-instruction '((arm:eor arm:eq)
					       arm:r15 arm:r10
					       (arm:r1 arm:lsr arm:r2))))
					;;eoreq r15, r10, r1 LSR r2
  #b00000000001010101111001000110001)
   ;cond000 EORSnnnnddddssss0sh1mmmm					     

(deftest sub-encode-1 ; ASR immediate
    (arm::encode (arm::opcode-to-instruction '((arm:sub arm:ne)
					       arm:r1 arm:r11
					       (arm:r3 arm:asr (arm:\# 31)))))
					;;subne r1, r11, r3 ASR #31
  #b00010000010010110001111111000011)
   ;cond000 SUBSnnnnddddSHFIMsh0mmmm					     

(deftest rsb-encode-1 ; ASR register
    (arm::encode (arm::opcode-to-instruction '((arm:rsb arm:lo)
					       arm:r3 arm:r8
					       (arm:r2 arm:asr arm:r9))))
					;;rsblo r3, r8, r2 ASR r9
  #b00110000011010000011100101010010)
   ;cond000 RSBSnnnnddddssss0sh1mmmm					     

(deftest add-encode-1 ; ROR immediate
    (arm::encode (arm::opcode-to-instruction '((arm:add arm:mi arm:s)
					       arm:r4 arm:r7
					       (arm:r3 arm:ror (arm:\# 19)))))
					;; addmi r4, r7, r3 ROR #19
  #b01000000100101110100100111100011)
   ;cond000 ADDSnnnnddddSHFIMsh0mmmm					     

(deftest adc-encode-4 ; ROR register
    (arm::encode (arm::opcode-to-instruction '((arm:adc arm:pl)
					       arm:r5 arm:r6
					       (arm:r4 arm:ror arm:r7))))
					;; adcpl r5, r6, r4 ROR r7
  #b01010000101001100101011101110100)
   ;cond000 ADCSnnnnddddssss0sh1mmmm					     

(deftest sbc-encode-1 ; RRX
    (arm::encode (arm::opcode-to-instruction '((arm:sbc arm:s arm:vc)
					       arm:r6 arm:r6
					       (arm:r5 arm:rrx))))
					;; sbcvs r6, r6, r5 RRX
  #b01110000110101100110000001100101)
   ;cond000 SBCSnnnndddd00000sh0mmmm					     

