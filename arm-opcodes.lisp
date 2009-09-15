;;; -*- mode: Lisp; Package: ("ARM" "CL"); Syntax: ANSI-Common-Lisp; -*-
;;;
;;; arm-opcodes.lisp
;;;

(cl:defpackage "ARM"
  (:use "CL")
  (:export
   "R0" "R1" "R2" "R3" "R4" "R5" "R6" "R7"
   "R8" "R9" "R10" "R11" "R12" "R13" "R14" "R15"
   "FP" ; alias for R11
   "IP" ; alias for R12
   "SP" ; alias for R13
   "LR" ; alias for R14
   "PC" ; alias for R15
   "EQ" "NE" "CS" "HS" "CC" "LO" "MI" "PL"
   "VS" "VC" "HI" "LS" "GE" "LT" "GT" "LE"
   "AL"
   "S" "LSL" "LSR" "ASR" "ROR" "RRX" "#"
   "!" "^"
   "MVN" "MOV" "ORR" "CMN" "BIC" "CMP" "TEQ" "TST" "RSC"
   "SBC" "ADC" "ADD" "RSB" "SUB" "EOR" "AND"
    "LDMDA" "LDMFA" "LDMIA" "LDMFD"
    "LDMDB" "LDMEA" "LDMIB" "LDMED"
    "STMDA" "STMED" "STMIA" "STMEA"
    "STMDB" "STMFD" "STMIB" "STMFA"
    "B" "BL" "BLX" "BX"
    "LDR"
))

(cl:in-package "ARM")

(define-condition arm-error (error) ())

(define-condition bad-argument-count (arm-error)
  ((opcode :reader opcode :initarg opcode)
   (arglist :reader arglist :initarg arglist)
   (expected-count :reader expected-count :initarg expected-count))
  (:report (lambda (condition stream)
	     (format stream "opcode ~A accepts only ~D arguments, given ~A."
		     (opcode condition)
		     (expected-count condition)
		     (arglist condition)))))

(define-condition bad-condition-code (arm-error)
  ((condition-code :reader condition-code :initarg condition-code))
  (:report (lambda (condition stream)
	     (format stream "~A is not a valid condition code."
		     (condition-code condition)))))

(define-condition bad-opcode-modifiers (arm-error)
  ((modifier-list :reader modifier-list :initarg modifier-list))
  (:report (lambda (condition stream)
	     (format stream "~A is not a valid opcode modifier list."
		     (modifier-list condition)))))

(define-condition bad-update (arm-error)
  ((opcode :reader opcode :initarg opcode))
  (:report (lambda (condition stream)
	     (format stream "~A does not allow an update flag."
		     (opcode condition)))))

(define-condition bad-register (arm-error)
  ((register :reader register :initarg register))
  (:report (lambda (condition stream)
	     (format stream "~A is not a valid register."
		     (register condition)))))

(define-condition bad-base-register-form (arm-error)
  ((register :reader register :initarg register))
  (:report (lambda (condition stream)
	     (format stream "~A is not a valid base register."
		     (register condition)))))

(define-condition bad-opcode (arm-error)
  ((opcode :reader opcode :initarg opcode))
  (:report (lambda (condition stream)
	     (format stream "~A is not a valid opcode for this instruction type."
		     (opcode condition)))))

(define-condition bad-immediate-32 (arm-error)
  ((immediate :reader immediate :initarg immediate))
  (:report (lambda (condition stream)
	     (format stream "~A cannot be encoded as an immediate value."
		     (immediate condition)))))

(define-condition bad-shifter-op (arm-error)
  ((shifter-op :reader shifter-op :initarg shifter-op))
  (:report (lambda (condition stream)
	     (format stream "~A is not a valid shifter operand."
		     (shifter-op condition)))))

(define-condition bad-shift-type (arm-error)
  ((shift-type :reader shift-type :initarg shift-type))
  (:report (lambda (condition stream)
	     (format stream "~A is not a valid shift type."
		     (shift-type condition)))))

(defclass instruction ()
  ((opcode :accessor opcode :initarg opcode)
   (condition :accessor condition :initarg condition
	      :initform 'AL
	      :documentation "Conditional execution: NIL or ARM:AL indicate always")
   (update :accessor update :initarg update :initform NIL
	   :documentation "Non-nil indicates the instruction updates the condition flag")))

(defclass data-processing (instruction)
  ((rn :accessor rn :initarg rn :documentation "First source operation")
   (rd :accessor rd :initarg rd :documentation "Destination register")))

(defclass immediate (data-processing)
  ((rotate_imm :accessor rotate_imm :initarg rotate_imm)
   (immed_8 :accessor immed_8 :initarg immed_8)))

(defclass immediate-shift (data-processing)
  ((rm :accessor rm :initarg rm :documentation "Shifter operand register")
   (shift_imm :accessor shift_imm :initarg shift_imm)
   (shift :accessor shift :initarg shift)))

(defclass register-shift (data-processing)
  ((rm :accessor rm :initarg rm :documentation "Shifter operand register")
   (rs :accessor rs :initarg rs :documentation "Shifter shift register")
   (shift :accessor shift :initarg shift)))

(defclass load/store-multiple (instruction)
  ((rn :accessor rn :initarg rn :documentation "Storage pointer")
   (update-rn :accessor update-rn :initarg update-rn :documentation 
	      "non-NIL if Rn is updated after the load/store.")
   (regs :accessor regs :initarg regs :documentation "List of registers")))
  
;; Ignore functionality to invoke Thumb or Jazelle instructions.
;; full 32-bit ARM only for now.

(defclass branch (instruction)
  ((use-link-register :accessor use-link-register :initarg use-link-register
		      :documentation "L bit: store PC in R14 before branch.")))

;; relative: have relative address in instruction word
;;
;; B: does not use link register
;; BL: uses link register.
;; ignore BLX(1) for now, which switches to Thumb, and can branch by
;; half-word offsets using the H bit, and *does not use the cond field.*

(defclass branch-relative (branch)
  ((word-offset :accessor word-offset :initarg word-offset 
		:documentation "Branch destination offset,
relative to *this* instruction in 32-bit words.")))

;; branch target in register (if Rm[0] is 1, also switches to Thumb, but
;;         that is a run-time feature, and I am ignoring Thumb for now anyway.
;;
;; BX: does not use link register
;; BLX(2): uses link register 
;; BXJ: to Jazelle, ignore for now.
;;

(defclass branch-register (branch)
  ((rn :accessor rn :initarg rn 
       :documentation "Register containing branch destination.")))

(defclass load-store (instruction)
  ((rd :accessor rd :initarg rd
       :documentation "Register being loaded/stored")
   (rn :accessor rn :initarg rn
       :documentation "Register containing base address")
   (load/store :accessor load/store :initarg load/store
	       :documentation "LOAD or STORE")
   (size :accessor size :initarg size 
	 :documentation "Size: BYTE, WORD, etc.")
   (offset-sign :accessor offset-sign :initarg offset-sign
		:initform 1
		:documentation "U-bit 1 for positive, 0 for negative")))

(defclass load-immediate-offset (load-store)
  ;; note offset 0 is load/store rd [rn]
  ((offset-12 :accessor offset-12 :initarg offset-12
	      :initform 0
	      :documentation "Unsigned offset, in bytes")))

(defclass load-register-offset (load-store)
  ((rm :accessor rm :initarg rm
       :documentation "Register containing offset, in bytes")
   ;; note: LSL #0 is encoding for load/store rd [Rn +/- Rm]
   (shift :accessor shift :initarg shift
	  :initform 0
	  :documentation "shift code: #b00=LSL, etc.")
   (shift_imm :accessor shift_imm :initarg shift_imm
	      :initform 0
	      :documentation "Amount to shift: 0 to 32")))

(defclass load-immediate-preindex (load-immediate-offset) ())
(defclass load-register-preindex (load-register-offset) ())
(defclass load-immediate-postindex (load-immediate-offset) ())
(defclass load-register-postindex (load-register-offset) ())

(defun condition-p (cond)
  (member cond '(eq ne cs hs cc lo mi pl vs vc hi ls ge lt gt le al nil)))

(defun encode-condition (cond)
  (case cond
    (EQ #b0000)
    (NE #b0001)
    ((CS HS) #b0010)
    ((CC LO) #b0011)
    (MI #b0100)
    (PL #b0101)
    (VS #b0110)
    (VC #b0111)
    (HI #b1000)
    (LS #b1001)
    (GE #b1010)
    (LT #b1011)
    (GT #b1100)
    (LE #b1101)
    ((AL nil) #b1110)
    (t (error 'bad-condition-code 'condition-code cond))))

(defun register-p (reg)
  (member reg '(r0 r1 r2 r3 r4 r5 r6 r7 r8 
		r9 r10 r11 r12 r13 r14 r15
		fp ip sp lr pc)))

(defun encode-register (reg)
  (let ((r (position reg
		     '(r0 r1 r2 r3 r4 r5 r6 r7 r8 
		       r9 r10 r11 r12 r13 r14 r15))))
    (cond 
      (r r)
      ((eq reg 'FP) 11)
      ((eq reg 'IP) 12)
      ((eq reg 'SP) 13)
      ((eq reg 'LR) 14)
      ((eq reg 'PC) 15)
      (t (error 'bad-register 'register reg)))))

;;; 32-bit immediates.
;;; if IMMEDIAT is the eight-bit value
;;; 0000 0000 0000 0000 0000 0000 IMME DIAT  rotate_imm = 0
;;; (mandatory if VAL in range 0 to 255)
;;;
;;; values of rotate_imm from 1 to 3 split immed_8
;;;
;;; AT00 0000 0000 0000 0000 0000 00IM MEDI  rotate_imm = 1
;;; DIAT 0000 0000 0000 0000 0000 0000 IMME               2
;;; MEDI AT00 0000 0000 0000 0000 0000 00IM               3
;;;
;;; values of rotate_imm from 4 to 15 are equivalent to 
;;; left shifts by (- 32 (* 2 rotate_imm))
;;;
;;; IMME DIAT 0000 0000 0000 0000 0000 0000               4
;;; 00IM MEDI AT00 0000 0000 0000 0000 0000               5
;;; 0000 IMME DIAT 0000 0000 0000 0000 0000               6
;;; 0000 00IM MEDI AT00 0000 0000 0000 0000               7
;;; 0000 0000 IMME DIAT 0000 0000 0000 0000               8
;;; 0000 0000 00IM MEDI AT00 0000 0000 0000               9
;;; 0000 0000 0000 IMME DIAT 0000 0000 0000              10
;;; 0000 0000 0000 00IM MEDI AT00 0000 0000              11
;;; 0000 0000 0000 0000 IMME DIAT 0000 0000              12
;;; 0000 0000 0000 0000 00IM MEDI AT00 0000              13
;;; 0000 0000 0000 0000 0000 IMME DIAT 0000              14
;;; 0000 0000 0000 0000 0000 00IM MEDI AT00              15

(defun encode-32-bit-immediate (val)
  "Returns two values: an 8-bit IMMED_8 and 4 bit ROTATE_IMM, 
such that VAL is equal to IMMED_8 rotated right by (* 2 ROTATE_IMM) bits
or NIL if VAL cannot be so encoded."
  (cond
    ((not (integerp val)) nil)
    ((<= 0 val #xFF) (values val 0))
    (t
     (loop for shift from 2 to 30 by 2
	with immed-high = 0 and immed-low = 0 and immed = 0
	do (if (< shift 8) 
	       (setf immed-high (ldb (byte shift (- 32 shift))
				     val)
		     immed-low (ldb (byte (- 8 shift) 0) val))
	       (setf immed (ldb (byte 8 (- 32 shift)) val)))
	when (or (and (< shift 8)
		      (= val (dpb immed-high (byte shift (- 32 shift))
				  (dpb immed-low (byte (- 8 shift) 0) 0))))
		 (and (>= shift 8)
		      (= val (dpb immed (byte 8 (- 32 shift)) 0))))
	return (values
		(if (< shift 8) (dpb immed-high (byte shift (- 8 shift)) 
				     (dpb immed-low (byte (- 8 shift) 0) 0))
		    immed)
		(/ shift 2))
	finally (return nil)))))

(defun data-processing-opcode-p (sym)
  (member sym '(AND EOR SUB RSB ADD ADC SBC RSC 
		TST TEQ CMP CMN ORR MOV BIC MVN)))

(defun load/store-multiple-opcode-p (sym)
  (member sym '(LDMDA LDMFA LDMIA LDMFD LDMDB LDMEA LDMIB LDMED
		STMDA STMED STMIA STMEA STMDB STMFD STMIB STMFA)))

(defun branch-opcode-p (sym)
  (member sym '(B BL BX BLX)))  ; BXJ

(defun load-store-opcode-p (sym)
  (member sym '(LDR))) ; etc.

(defun load-opcode-p (sym)
  (member sym '(LDR)))

(defun encode-data-processing-opcodes (opcode)
  "bits 24..21 of a data-processing instruction."
  (case opcode
    (and #b0000)
    (eor #b0001)
    (sub #b0010)
    (rsb #b0011)
    (add #b0100)
    (adc #b0101)
    (sbc #b0110)
    (rsc #b0111)
    (tst #b1000)
    (teq #b1001)
    (cmp #b1010)
    (cmn #b1011)
    (orr #b1100)
    (mov #b1101)
    (bic #b1110)
    (mvn #b1111)
    (t (error 'bad-opcode 'opcode opcode))))

(defun encode-update (s)
  (if s 1 0))

(defmethod encode ((insn data-processing))
  (let ((cond (encode-condition (condition insn)))
	(rn (encode-register (rn insn)))
	(rd (encode-register (rd insn)))
	(opcode (encode-data-processing-opcodes (opcode insn)))
	(s (encode-update (update insn)))
	(word 0))
    (setf (ldb (byte 4 28) word) cond
	  (ldb (byte 4 21) word) opcode
	  (ldb (byte 1 20) word) s
	  (ldb (byte 4 16) word) rn
	  (ldb (byte 4 12) word) rd)
    word))

(defmethod encode ((insn immediate))
  (let ((word (call-next-method)))
    (setf (ldb (byte 3 25) word) #b001
	  (ldb (byte 4 8) word) (rotate_imm insn)
	  (ldb (byte 8 0) word) (immed_8 insn))
    word))

;; shift = 0, shift_imm = 0: register
;; shift = 0, shift_imm != 0: LSL #shift_imm

(defmethod encode ((insn immediate-shift))
  (let ((word (call-next-method))
	(rm (encode-register (rm insn))))
    (setf (ldb (byte 3 25) word) #b000
	  (ldb (byte 5 7) word) (shift_imm insn)
	  (ldb (byte 2 5) word) (shift insn)
	  (ldb (byte 1 4) word) 0
	  (ldb (byte 4 0) word) rm)
    word))

;; shift = #b00: LSL
;; shift = #b01: LSR
;; shift = #b10: ASR
;; shift = #b11: ROR
;; ROR #0 -> RRX

(defun encode-shift (shift-sym)
  "Returns the integer encoding for a shift-symbol; 
NIL equivalent to LSL #0, RRX equivalent to ROR #0"
  (case shift-sym
    ((LSL nil) #b00)
    (LSR #b01)
    (ASR #b10)
    ((ROR RRX) #b11)
    (t (error 'bad-shift-type 'shift-type shift-sym))))

(defmethod encode ((insn register-shift))
  (let ((word (call-next-method))
	(rs (encode-register (rs insn)))
	(rm (encode-register (rm insn))))
    (setf (ldb (byte 3 25) word) #b000
	  (ldb (byte 4 8) word) rs
	  (ldb (byte 1 7) word) 0
	  (ldb (byte 2 5) word) (shift insn)
	  (ldb (byte 1 4) word) 1
	  (ldb (byte 4 0) word) rm)
    word))

(defmethod encode ((insn load/store-multiple))
  (let ((cond (encode-condition (condition insn)))
	(rn (encode-register (rn insn)))
	(s (encode-update (update insn)))
	(w (encode-update (update-rn insn)))
	(word 0))
    (multiple-value-bind (l p u)
	(load/store-multiple-bits (opcode insn))
      (setf (ldb (byte 4 28) word) cond
	    (ldb (byte 3 25) word) #b100
	    (ldb (byte 1 24) word) p
	    (ldb (byte 1 23) word) u
	    (ldb (byte 1 22) word) s
	    (ldb (byte 1 21) word) w
	    (ldb (byte 1 20) word) l
	    (ldb (byte 4 16) word) rn)
      (mapc #'(lambda (reg)
		(setf (ldb (byte 1 (encode-register reg)) word) 1))
	    (regs insn))
      word)))

;; branch relative
;; B, BL, BLX non-register

(defmethod encode ((insn branch-relative))
  ;; branches computed with respect to PC=current-instruction + 8 bytes
  ;; = current+2 words
  (let ((branch-immed (- (word-offset insn) 2))
	(cond (if (eq (opcode insn) 'blx)
		  #b1111
		  (encode-condition (condition insn))))
	(word 0))
    (setf (ldb (byte 4 28) word) cond
	  (ldb (byte 24 0) word) (truncate branch-immed)
	  (ldb (byte 3 25) word) #b101
	  (ldb (byte 1 24) word) (case (opcode insn)
				   (b 0) ; L bit
				   (bl 1) ; L bit
				   (blx 0))) ; H bit
    ;; H bit can be 1 for Thumb instruction mid-32-bit-word...
    word))

;; bx, blx(2), bxj (ignore for now)

(defmethod encode ((insn branch-register))
  (let ((cond (encode-condition (condition insn)))
	(l (ecase (opcode insn)
	     (bx #b0001)
	     (blx #b0011)
	     (bxj #b0010)))
	(word 0))
    (setf (ldb (byte 4 28) word) cond
	  (ldb (byte 8 20) word) #b00010010
	  (ldb (byte 12 8) word) #b111111111111 ; SBO :should be one
	  (ldb (byte 4 4) word) l
	  (ldb (byte 4 0) word) (encode-register (rn insn)))
    word))

;;; S-expression instruction syntax
;;;
;;; Data-processing instructions
;;; ----------------------------
;;;
;;; opcodes with no flags, or S = 0, cond = always
;;;
;;; (opcode <Rd> <Rn> <shifter_operand>)
;;; 
;;; when only two arguments after the opcode
;;; 
;;;   for MOV, MVN (opcode <Rd> <shifter_operand>) Rn is always encoded as R0
;;;   for CMP, CMN, TST, TEQ S=1 always, and it is 
;;;                (opcode <Rn> <shifter_operand>), Rd is always encoded as R0
;;; 
;;; opcodes with S=1, cond = always
;;;
;;; ((opcode :s) <Rd> <Rn> <shifter_operand>)
;;;
;;; opcodes with S=0, cond other than always
;;;
;;; ((opcode <cond>) <Rd> <Rn> <shifter_operand>)
;;;
;;; opcodes with S=1, cond other always
;;; 
;;; ((opcode arm:s <cond>) <Rd> <Rn> <shifter_operand>)
;;; ((opcode <cond> arm:s) <Rd> <Rn> <shifter_operand>)
;;;
;;;
;;; Branch instructions
;;; -------------------
;;;
;;; (opcode <Rn>) ((opcode <cond>) <Rn>)
;;; (opcode <relative-address>) ((opcode <cond>) <relative-address>)
;;;
;;; Load/store multiple register instructions
;;; -----------------------------------------
;;;
;;; (opcode <Rn> &rest <register-list>)
;;;
;;; (opcode (<Rn> arm:!) &rest <register-list>)  sets the W bit, updating Rn
;;;
;;; opcode can be symbol (LDMIA, STMDB, etc.) or
;;; (opcode <cond>) (opcode arm:s) (opcode <cond> arm:s) or 
;;;                                (opcode arm:s <cond>) 
;;; where arm:^ can be used as a synonym for arm:s
;;; (The S-bit in LDM with R15/PC in the register list is used to indicate 
;;;  loading of CPSR from the SPSR; in privileged mode 
;;;  for LDM without R15/PC or STM, the S-bit set indicates the 
;;;  load/store affects user-mode registers)
;;;
;;;
;;; Load/store instructions
;;; -----------------------
;;;
;;; (opcode <Rd> <Rn>) = opcode <Rd>, [<Rn> , 0]
;;; (opcode <Rd> <Rn> (arm:# <immediate>)) 
;;;                    = opcode <Rd>, <Rn> #+/-offset_12
;;;   ...or should the (arm:# <immediate>) simply be <immediate>,
;;;      i.e., if not a register symbol
;;;
;;; (opcode <Rd> <Rn> (arm:+ <Rm>)) 
;;; (opcode <Rd> <Rn> (arm:- <Rm>))
;;;                    = opcode <Rd>, [<Rn> +/-<Rm>]
;;; or should the arm:+/- form enclose both <Rn> and <Rm>?
;;;
;;; (opcode <Rd> <Rn> (arm:+ <Rm> arm:LSL <immediate>))
;;;
;;; (opcode <Rd> (arm:! <Rn> <offset12>)) <offset12> defaults to 0?
;;; (opcode <Rd> (arm:! <Rn> (arm:+ <Rm>))
;;; (opcode <Rd> (arm:! <Rn> (arm:- <Rm>))
;;; (opcode <Rd> (arm:! <Rn>) <immediate>)
;;; (opcode <Rd> (arm:! <Rn>) (arm:+ <Rm>))
;;; (opcode <Rd> (arm:! <Rn>) (arm:- <Rm>))
;;; (opcode <Rd> (arm:! <Rn>) (arm:+ <Rm> arm:LSL <shift_imm>))
;;; (opcode <Rd> (arm:! <Rn>) (arm:- <Rm> arm:LSL <shift_imm>))

(defun split-sexp-opcode (opcode-list)
  "Splits s-expression form of opcodes.

   Returns three values: the bare opcode symbol, 
                         the symbol representing the condition,
                         and T/NIL the S (update) bit is set/not-set

    opcode                 -> AND
    (opcode <cond>)        -> AND<cond>
    (opcode ARM:S)         -> ANDS
    (opcode <cond> ARM:S)  -> AND<cond>S
    (opcode ARM:S <cond>)  -> AND<cond>S" 
  (cond 
    ((symbolp opcode-list) (values opcode-list 'AL nil)) ; bare opcode: S=0, cond=always
    ((not (consp opcode-list)) (error 'bad-opcode 'opcode opcode-list))
    ((data-processing-opcode-p (car opcode-list))
     (let (s cond)
       (mapcar #'(lambda (decorator)
		   (cond ((and (eq decorator 's) (not s)) (setq s t))
			 ((and (encode-condition decorator) (not cond))
			  (setq cond decorator))
			 (t (error 'bad-condition 'condition decorator))))
	       (rest opcode-list))
       (values (car opcode-list) (or cond 'AL) s)))
    ((load/store-multiple-opcode-p (car opcode-list))
     (let (s cond)
       (mapcar #'(lambda (decorator)
		   (cond ((and (member decorator '(s ^)) (not s)) (setq s t))
			 ((and (encode-condition decorator) (not cond))
			  (setq cond decorator))
			 (t (error 'bad-condition 'condition 
				   (rest opcode-list)))))
	       (rest opcode-list))
       (values (car opcode-list) (or cond 'AL) s)))
    ((branch-opcode-p (car opcode-list))
     (if (and (encode-condition (cadr opcode-list))
	      (null (cddr opcode-list)))
	 (values (car opcode-list) (cadr opcode-list) nil)
	 (error 'bad-condition 'condition (rest opcode-list))))
    (t (error "split-sexp-opcode: Not implemented"))
    ))

;;; <shifter_operand>
;;;
;;; immediate: (ARM:\# immediate-value)
;;; register: <Rm>
;;; <Rm>, LSL #<shift_imm>: (<Rm> arm:lsl (arm:\# immediate-value))
;;; <Rm>, LSL <Rs>: (<Rm> arm:lsl <Rs>)
;;; <Rm>, LSR #<shift_imm>: (<Rm> arm:lsr (arm:\# immediate-value))
;;; <Rm>, LSR <Rs>: (<Rm> arm:lsr <Rs>)
;;; <Rm>, ASR #<shift_imm>: (<Rm> arm:asr (arm:\# immediate-value))
;;; <Rm>, ASR <Rs>: (<Rm> arm:asr <Rs>)
;;; <Rm>, ROR #<shift_imm>: (<Rm> arm:ror (arm:\# immediate-value))
;;; <Rm>, ROR <Rs>: (<Rm> arm:ror <Rs>)
;;; <Rm>, RRX: (<Rm> arm:rrx)
;;; 

(defun split-sexp-shifter (shifter-op)
  "Returns three values
    Rm 
    Shift-type encoding bits
    Shift-value integer or symbol for Rs.

TODO: shift-value integers should be checked for magnitude"
  (cond
    ((symbolp shifter-op) ; Rm alone, equivalent to LSL #0
     (values shifter-op (encode-shift 'LSL) 0))
    ((and (consp shifter-op) 
	  (null (cddr shifter-op))
	  (eq (second shifter-op) 'RRX)) ; RRX = ROR #0
     (values (first shifter-op) (encode-shift 'ROR) 0))
    ((and (consp shifter-op)
	  (symbolp (third shifter-op)))
     (if (eq (second shifter-op)
	     'RRX)
	 (error 'bad-shift-type 'shift-type shifter-op)
	 (values (first shifter-op) (encode-shift (second shifter-op))
		 (third shifter-op))))
    ((and (consp shifter-op)
	  (consp (third shifter-op))
	  (eq (car (third shifter-op)) '\#))
     (values (first shifter-op) (encode-shift (second shifter-op))
	     (second (third shifter-op))))
    (t (error 'bad-shift-type 'shift-type shifter-op))))

;; load/store-multiple bits

(defun load/store-multiple-bits (opcode)
  "Returns three values: (1/0 respectively)
L (load/store)
P (address included in storage/not-included)
U (transfer made upwards/downwards)"

  (case opcode 
    ((LDMDA LDMFA) (values 1 0 0))
    ((LDMIA LDMFD) (values 1 0 1))
    ((LDMDB LDMEA) (values 1 1 0))
    ((LDMIB LDMED) (values 1 1 1))
    ((STMDA STMED) (values 0 0 0))
    ((STMIA STMEA) (values 0 0 1))
    ((STMDB STMFD) (values 0 1 0))
    ((STMIB STMFA) (values 0 1 1))
    (t (error 'bad-opcode 'opcode opcode))))

(defun split-load/store-rn (rn)
  "Decodes a base-address argument S-expression, returning multiple values
First value: the base register ARM:R0, R1, etc.
Second value: non-nil if indexed (i.e., (arm:! <Rn> ...))
Third value: the offset form."
  (cond
    ((symbolp rn) 
     (if (register-p rn)
	 (values rn nil nil)
	 (error 'bad-register 'register rn)))
    ((consp rn)
     (unless (and (eq (car rn) 'arm:!)
		  (register-p (second rn)))
       (error 'bad-base-register-form :register rn))
     (values (second rn) t (cddr rn)))
    (t (error 'bad-base-register-form :register rn))))
     
(defun opcode-to-instruction (symbolic-opcode)
  (let ((opcode (first symbolic-opcode)))
    (multiple-value-bind (op condition update)
	(split-sexp-opcode opcode)
      (cond 
	((branch-opcode-p op)
	 (when update
	   (error 'bad-update 'opcode symbolic-opcode))
	 (unless (null (cddr symbolic-opcode))
	   (error 'bad-argument-count
		  'opcode op
		  'arglist (rest symbolic-opcode)
		  'expected-count 1))
	 (case op
	   ((b bl) (make-instance 'branch-relative 
				  'condition condition
				  'opcode op
				  'word-offset (second symbolic-opcode)
				  'use-link-register (eq op 'bl)))
	   (blx (if (register-p (second symbolic-opcode))
		    ; BLX(2)
		    (make-instance 'branch-register
				   'condition condition
				   'opcode op
				   'rn (second symbolic-opcode)
				   'use-link-register t)
		    ; BLX(1): no condition code allowed
		    (if (eq condition 'arm:al)
			(make-instance 'branch-relative
				       'opcode op
				       'word-offset (second symbolic-opcode)
				       'use-link-register t)
			(error 'bad-opcode 'opcode
			       (car symbolic-opcode)))))
		
	   (bx (make-instance 'branch-register
			      'opcode op
			      'condition condition
			      'rn (second symbolic-opcode)
			      'use-link-register nil))
	   (t (error 'bad-opcode 'opcode op))))

	((data-processing-opcode-p op)	
	 
	 ;; basic checks for argument count
	 (cond ((member op '(MOV MVN CMP CMN TST TEQ))
		(unless (null (cdddr symbolic-opcode))
		  (error 'bad-argument-count
			 'opcode op 
			 'arglist (rest symbolic-opcode)
			 'expected-count 2)))
	       (t (unless (null (cddddr symbolic-opcode))
		  (error 'bad-argument-count
			 'opcode op 
			 'arglist (rest symbolic-opcode)
			 'expected-count 3))))
	 (let ((shifter-op
		(cond ((member op '(MOV MVN CMP CMN TST TEQ))
		       (third symbolic-opcode))
		      (t (fourth symbolic-opcode))))
	       (rd 
		(cond ((member op '(CMP CMN TST TEQ))
		       'arm:r0) ; encoded as zero
		      (t (second symbolic-opcode))))
	       (rn
		(cond ((member op '(MOV MVN)) 'arm:r0) ; encoded as zero
		      ((member op '(CMP CMN TST TEQ))
		       (second symbolic-opcode))
		      (t (third symbolic-opcode))))
	       (s (cond ((member op '(CMP CMN TST TEQ)) ; imply S=1
			 t)
			(t update))))
	   (cond 
	     ((symbolp shifter-op)
	      (make-instance 'immediate-shift
			     'opcode op
			     'condition condition
			     'update s
			     'rd rd
			     'rn rn
			     'rm shifter-op
			     'shift 0
			     'shift_imm 0))
	     ((and (consp shifter-op)
		   (eq (car shifter-op) 'arm:\#))
	      (multiple-value-bind (immed-8 rotate-imm)
		  (encode-32-bit-immediate (cadr shifter-op))
		(if immed-8
		    (make-instance 'immediate
				   'condition condition
				   'update s
				   'opcode op
				   'rd rd
				   'rn rn
				   'rotate_imm rotate-imm 
				   'immed_8 immed-8)
		    (error 'bad-immediate-32 'immediate 
			   (cadr shifter-op)))))
	     ((and (consp shifter-op)
		   (member (second shifter-op) 
			   '(LSL LSR ASR ROR RRX)))
	      (multiple-value-bind (rm shift-code shift-reg-or-imm)
		  (split-sexp-shifter shifter-op)
		(if (symbolp shift-reg-or-imm)
		    (make-instance 'register-shift
				   'opcode op
				   'update s
				   'condition condition
				   'rd rd
				   'rn rn
				   'rm rm
				   'shift shift-code
				   'rs shift-reg-or-imm)
		    (make-instance 'immediate-shift
				   'opcode op
				   'update s
				   'condition condition
				   'rd rd
				   'rn rn
				   'rm rm
				   'shift shift-code
				   'shift_imm shift-reg-or-imm))))
	     (t (error "Bad data-processing opcode.")))))
	((load/store-multiple-opcode-p op)
	 ;; rn
	 (multiple-value-bind (rn update-rn)
	     (let ((rn-symb (second symbolic-opcode))) ;; <rn> or (rn arm:!)
	       (cond
		 ((symbolp rn-symb) (values rn-symb nil))
		 ((and (consp rn-symb) (null (cddr rn-symb))
		       (eq (second rn-symb) '!)) (values (car rn-symb) t))
		 (t (error 'bad-register 'register 'rn-symb))))
	   (make-instance 'load/store-multiple
			  'opcode op
			  'rn rn
			  'update-rn update-rn
			  'condition condition
			  'update update
			  'regs (cddr symbolic-opcode))))

	((load-store-opcode-p op)
	 (let ((load/store (load-opcode-p op))
	       (rd (second symbolic-opcode))
	       (rn (third symbolic-opcode))
	       (post-index-offset (fourth symbolic-opcode)))
	   (multiple-value-bind (rn indexed pre-index-offset)
	       (split-load/store-rn rn)
	     
	     (error "Load/store not yet implemented."))))
	(t (error "Not yet implemented."))))))
    
