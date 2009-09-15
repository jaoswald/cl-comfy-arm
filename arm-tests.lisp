;;;; -*- mode: Lisp; Syntax: ANSI-Common-Lisp; Package: ("ARM-TESTS" ("CL" "ARM" "RT")); -*-
;;;;
;;;; arm-tests.lisp
;;;;
;;;; Uses RT regression test framework

(cl:defpackage "ARM-TESTS"
  (:use "COMMON-LISP" "ARM" #+sbcl "SB-RT" #-sbcl "RT"))

(cl:in-package "ARM-TESTS")

(defmacro test-encoding (test-name asm-sexp value)
  `(deftest ,test-name (arm::encode (arm::opcode-to-instruction
				     ,asm-sexp))
     ,value))

(test-encoding and-encode-1 
	       '(arm:and arm:r0 arm:r1 arm:r2) ;;and r0, r1, r2
	       ;;   e   0   0   1   0   0   0   2
	       ;;cond000 AND0nnnndddd00000000mmmm
	       #b11100000000000010000000000000010)

(test-encoding and-encode-2
	       '(arm:and arm:r15 arm:r14 arm:r13) ;; and r15, r14, r13
	       ;;   e   0   0   e   f   0   0   d
	       ;;cond000 AND0nnnndddd00000000mmmm
	       #b11100000000011101111000000001101)


(test-encoding and-encode-3
	       '((arm:and arm:s) arm:r15 arm:r14 arm:r13) ;; ands r15, r14, r13
	       ;;   e   0   1   e   f   0   0   d
	       ;;cond000 ANDSnnnndddd00000000mmmm
	       #b11100000000111101111000000001101)



(test-encoding mvn-encode-1 ;; note: MVN only takes two args
	       '(arm:mvn arm:r3 arm:r4)  ;; mvn r3, r4
	       ;;   e   1   e   0   3   0   0   4
	       ;;cond000 MVN00000dddd00000000mmmm
	       #b11100001111000000011000000000100)


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

(test-encoding and-encode-4
	       '((arm:and arm:s) arm:r15 arm:r14 (arm:\# #xff00))
	       ;;ands r15, r14, #xff00
	       ;;   e   2   1   e   f   c   f   f
	       ;;cond00I ANDSnnnnddddrot_IMMEDIAT
	       #b11100010000111101111110011111111)

(test-encoding and-encode-5 ; update condition flags with s
	       '((arm:and arm:s) arm:r0 arm:r12 (arm:\# #x3f0))
	       ;;ands r0, r12, #x3f0
	       ;;   e   2   1   c   0   e   3   f
	       ;;cond00I ANDSnnnnddddrot_IMMEDIAT
	       #b11100010000111000000111000111111)


(test-encoding and-encode-6
	       '((arm:and arm:s arm:vs) arm:r0 arm:r12 (arm:\# #xab))
	       ;;andvss r0, r12, #xab
	       ;;   6   2   1   c   0   0   a   b
	       ;;cond00I ANDSnnnnddddrot_IMMEDIAT
	       #b01100010000111000000000010101011)


(test-encoding and-encode-7
	       '((arm:and arm:hs) arm:r0 arm:r12 (arm:\# #x3f0))
	       ;;andhs r0, r12, #x3f0
	       ;;   2   2   0   c   0   e   3   f
	       ;;cond00I ANDSnnnnddddrot_IMMEDIAT
	       #b00100010000011000000111000111111)


(test-encoding and-encode-8 ; s can occur after condition
	       '((arm:and arm:hi arm:s) arm:r0 arm:r12 (arm:\# #x3f0))
	       ;;andvss r0, r12, #x3f0
	       ;;   8   2   1   c   0   e   3   f
	       ;;cond00I ANDSnnnnddddrot_IMMEDIAT
	       #b10000010000111000000111000111111)


(test-encoding adc-encode-1 ; LSL immediate
	       '(arm:adc arm:r0 arm:r12 (arm:r2 arm:lsl (arm:\# 1)))
	       ;;adc r0, r12, r2 LSL #1
	       ;;   e   0   a   c   0   0   8   2
	       ;;cond000 ADCSnnnnddddSHFIMsh0mmmm
	       #b11100000101011000000000010000010)


(test-encoding adc-encode-2 ; LSL register
	       '(arm:adc arm:r0 arm:r12 (arm:r2 arm:lsl arm:r4))
	       ;;adc r0, r12, r2 LSL r4
	       ;;   3   0   a   c   0   4   1   2
	       ;;cond000 ADCSnnnnddddssss0sh1mmmm
	       #b11100000101011000000010000010010)


(test-encoding adc-encode-3 ; LSR immediate
	       '(arm:adc arm:r0 arm:r12 (arm:r2 arm:lsr (arm:\# #x1a)))
	       ;;adc r0, r12, r2 LSR #x1a
	       ;;   e   0   a   c   0   d   2   2
	       ;;cond000 ADCSnnnnddddSHFIMsh0mmmm
	       #b11100000101011000000110100100010)



(test-encoding eor-encode-1 ; LSR register
	       '((arm:eor arm:eq) arm:r15 arm:r10 (arm:r1 arm:lsr arm:r2))
	       ;;eoreq r15, r10, r1 LSR r2
	       ;;   0   0   2   a   f   2   3   1
	       ;;cond000 EORSnnnnddddssss0sh1mmmm
	       #b00000000001010101111001000110001)


(test-encoding sub-encode-1 ; ASR immediate
	       '((arm:sub arm:ne) arm:r1 arm:r11 (arm:r3 arm:asr (arm:\# 31)))
	       ;;subne r1, r11, r3 ASR #31
	       ;;   1   0   4   b   1   f   c   3
	       ;;cond000 SUBSnnnnddddSHFIMsh0mmmm
	       #b00010000010010110001111111000011)


(test-encoding rsb-encode-1 ; ASR register
	       '((arm:rsb arm:lo) arm:r3 arm:r8 (arm:r2 arm:asr arm:r9))
	       ;;rsblo r3, r8, r2 ASR r9
	       ;;   3   0   6   8   3   9   5   2
	       ;;cond000 RSBSnnnnddddssss0sh1mmmm
	       #b00110000011010000011100101010010)


(test-encoding add-encode-1 ; ROR immediate
	       '((arm:add arm:mi arm:s) 
		 arm:r4 
		 arm:r7 
		 (arm:r3 arm:ror (arm:\# 19)))
	       ;; addmis r4, r7, r3 ROR #19
	       ;;   4   0   9   7   4   9   e   3
	       ;;cond000 ADDSnnnnddddSHFIMsh0mmmm
	       #b01000000100101110100100111100011)

(test-encoding adc-encode-4 ; ROR register
	       '((arm:adc arm:pl) arm:r5 arm:r6 (arm:r4 arm:ror arm:r7))
	       ;; adcpl r5, r6, r4 ROR r7
	       ;;   5   0   a   6   5   7   7   4
	       ;;cond000 ADCSnnnnddddssss0sh1mmmm
	       #b01010000101001100101011101110100)


(test-encoding sbc-encode-1 ; RRX
	       '((arm:sbc arm:s arm:vc) arm:r6 arm:r6 (arm:r5 arm:rrx))
	       ;; sbcvcs r6, r6, r5 RRX
	       ;;   7   0   d   6   6   0   6   5
	       ;;cond000 SBCSnnnndddd00000sh0mmmm
	       #b01110000110101100110000001100101)

;; tests from hello world GCC compiler output + objdump

(test-encoding sub-encode-2
	       '(arm:sub arm:fp arm:ip (arm:\# 4))
	       #xe24cb004)

(test-encoding and-encode-9
	       '((arm:and arm:eq) arm:r0 arm:r0 arm:r0)
	       0)

(test-encoding mov-encode-1
	       '(arm:mov arm:ip arm:sp)
	       #xe1a0c00d)


(test-encoding cmp-encode-1
	       '(arm:cmp arm:r1 (arm:\# #x1f))
	       ;;cond00I CMP1nnnn0000rot_IMMEDIAT
	       #b11100011010100010000000000011111)


(test-encoding cmp-encode-2
	       '(arm:cmp arm:r2 arm:r10)
	       ;;cond000 CMP1nnnn0000SHFIMsh0mmmm
	       #b11100001010100100000000000001010)
  

(test-encoding cmp-encode-3
	       '(arm:cmp arm:r1 (arm:\# #x1e))
	       #xe351001e)

(test-encoding cmp-encode-4
	       '(arm:cmp arm:r0 arm:r1)
	       #xe1500001)
;;      e   1   5   0   0   0   0   1
;; #b11100001010100000000000000000001
;;   cond000 CMP1nnnn0000SHFIMsh0mmmm					     

(test-encoding stmdb-encode-1
	       '(arm:stmdb (arm:sp arm:!) arm:fp arm:ip arm:lr arm:pc)
	       #xe92dd800)

(test-encoding ldmia-encode-1
	       '(arm:ldmia arm:sp arm:fp arm:sp arm:pc)
	       #xe89da800)

(test-encoding bl-encode-1
	       '(arm:bl 2)
	       #xeb000000)

(test-encoding bl-encode-2
	       '(arm:bl -1)
	       #xebfffffd)

(test-encoding b-encode-1
	       '(arm:b 4)
	       #xea000002)

(test-encoding b-encode-2
	       '((arm:b arm:gt) -3)
	       #xcafffffb)

(test-encoding bx-encode-1
	       '(arm:bx arm:r4)
	       #xe12fff14)

(test-encoding bx-encode-2
	       '((arm:bx arm:le) arm:r14)
	       #xd12fff1e)

(test-encoding blx2-encode-1 
	       ;; BLX(2) uses register, calls ARM or Thumb, 
	       ;; can have condition code
	       '((arm:blx arm:lt) arm:r13)
	       #xb12fff3d)

(test-encoding blx1-encode-1
	       ;; BLX(1) uses half-word offset, calls Thumb, 
	       ;; CANNOT have condition code
	       '(arm:blx 10)
	       ;;1111101H<  signed   immed    24>
	       #b11111010000000000000000000001000)
					

;; should define some macro to take away the m-v-b, ignore-errors,
;;  (not success) and (class-name)
;; 
(deftest blx1-bogus-1
    (multiple-value-bind (success cond)
	(ignore-errors
	  (arm::encode (arm::opcode-to-instruction '((arm:blx arm:lt) 10))))
      (values (not success) 
	      (class-name (class-of cond))
	      (arm::opcode cond)))
  T arm::bad-opcode (arm:blx arm:lt))
      

#||
(test-encoding blx1-encode-2
	       ;; BLX(1) uses half-word offset, calls Thumb, 
	       ;; CANNOT have condition code
	       (list 'arm:blx (+ 10 1/2))
	       ;;1111101H<  signed   immed    24>
	       #b11111011000000000000000000001000)


;; NOT YET IMPLEMENTED
||#

(test-encoding ldr-encode-1
	       '(arm:ldr arm:r1 arm:r0)
	       #xe5901000)