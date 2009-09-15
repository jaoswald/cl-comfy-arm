(cl:in-package "ARM")

;;; strategy for parsing ARM symbolic opcodes:
;;; cons up a list of arguments for make-instance

(defvar *arm-instance-args* nil)

;;; parse opcodes allowing S, condition
;;; returns list ('condition <cond> 'update <s>)

#||
(defun occurs-once (list item &key (test #'eql))
  (cond 
    ((null list) nil)
    ((not (consp list)) nil)
    ((funcall test (car list) item)
     (not (member item (cdr list) :test test)))
    (t (occurs-once (cdr list) item))))
||#

(defun parse-opcode-cond (opcode-or-opcode-list)
  (if (symbolp opcode-or-opcode-list) 
      (list 'condition 'al)
      (let (c 
	    (modifier-list (cdr opcode-or-opcode-list)))
	(dolist (item modifier-list
		 (list 'condition (or c 'AL)))
	  (cond 
	    ((and (condition-p item) (not c))
	     (setq c item))
	    (t (error 'bad-opcode-modifier 'modifier-list modifier-list)))))))

(defun parse-opcode-cond-s (opcode-or-opcode-list)
  (if (symbolp opcode-or-opcode-list) 
      (list 'condition 'al 'update nil)
      (let (s 
	    cond 
	    (modifier-list (cdr opcode-or-opcode-list)))
	(dolist (item modifier-list
		 (list 'condition (or cond 'AL)
		       'update s))
	  (cond 
	    ((and (eq item 's) (not s))
	     (setq s t))
	    ((and (condition-p item) (not cond))
	     (setq cond item))
	    (t (error 'bad-opcode-modifier 'modifier-list modifier-list)))))))

(defun parse-opcode-cond-s^ (opcode-or-opcode-list)
  (if (symbolp opcode-or-opcode-list) 
      (list 'condition 'al 'update nil)
      (let (s 
	    cond 
	    (modifier-list (cdr opcode-or-opcode-list)))
	(dolist (item modifier-list
		 (list 'condition (or cond 'AL)
		       'update s))
	  (cond 
	    ((and (or (eq item 's) (eq item '^))
		  (not s))
	     (setq s t))
	    ((and (condition-p item) (not cond))
	     (setq cond item))
	    (t (error 'bad-opcode-modifier 'modifier-list modifier-list)))))))

(defun move-args (arg-list)
  "Processes args for MOV, MVN
   Two values: the Rd/Rn arg list and the shifter-op"
  (values (list 'rd (first arg-list)
		'rn 'arm:r0)
	  (second arg-list)))

(defun cmp/test-args (arg-list)
  "Processes args for CMP, CMN, TST, TEQ
   Two values: the Rd/Rn arg list and the shifter-op"
  (values
   (list 'rd 'arm:r0 'rn (first arg-list))
   (second arg-list)))

(defun data-processing-args (arg-list)
  "Processes args for other DP instructions
   Two values: the Rd/Rn arg list and the shifter-op"
  (values
   (list 'rd (first arg-list)
	 'rn (second arg-list))
   (third arg-list)))
	 
(defun parse-shifter-op (shifter-op)
  "Parse a data-processing instruction's shifter-op, 
   returning two values: 
   The class of DP instruction shifter-op represents,
   The list of initargs and values"
  (cond ((symbolp shifter-op)
	 (values 
	  'immediate-shift
	  (list 'rm shifter-op 'shift 0 'shift_imm 0)))
	((and (consp shifter-op) (eq (car shifter-op) 'arm:\#))
	 (multiple-value-bind (immed-8 rotate-imm)
	     (encode-32-bit-immediate (cadr shifter-op))
	   (if immed-8
	       (values 'immediate
		       (list 'rotate_imm rotate-imm 
			     'immed_8 immed-8))
	       (error 'bad-immediate-32 'immediate 
		      (cadr shifter-op)))))
	((and (consp shifter-op)
	      (member (second shifter-op) 
		      '(LSL LSR ASR ROR RRX)))
	 (multiple-value-bind (rm shift-code shift-reg-or-imm)
	     (split-sexp-shifter shifter-op)
	   (if (symbolp shift-reg-or-imm)
	       (values 'register-shift
		       (list 'rm rm
			     'shift shift-code
			     'rs shift-reg-or-imm))
	       (values 'immediate-shift
		       (list 'rm rm
			     'shift shift-code
			     'shift_imm shift-reg-or-imm)))))
	(t (error 'bad-shifter-op
		  'shifter-op shifter-op))))

  

#|

;;; example
;;; AND is a data-processing op code that
;;; takes S, COND
;;; arguments rd rn shifter-op

(defun parse-and (symbolic-opcode)
  (let* ((op (car symbolic-opcode))
	 (opcode (cond
		   ((or (eq op 'arm:and)
			(and (consp op)
			     (eq (car op) 'arm:and)))
		    'arm:and)
		   (t (error 'arm::bad-opcode 'opcode op))))
	 (args-1 (parse-opcode-cond-s op)))
    (multiple-value-bind (args-2 shifter-op)
	(data-processing-args (cdr symbolic-opcode))
      (multiple-value-bind (class args-3)
	  (parse-shifter-op shifter-op)
	(apply #'make-instance class 'arm::opcode opcode
	       (append args-1 args-2 args-3))))))

;; prototype result of 
;; inst-definition-to-matcher ((AND COND S) RD RN SHIFTER-OP)
;; 
(defun parse-and-2 (symbolic-opcode)
  (let ((class nil)
	(op-symbol 'AND)
	;;         ^^  first value of (opcode-parse '(AND COND S))
	(opcode (car symbolic-opcode))
	(shifter-arg-list nil))
    (let ((opcode-args (parse-opcode-cond-s opcode))
	  ;;           ^^ second value of (opcode-parse '(AND COND S))
	  (gensym-2 (list 'rd (nth 1 symbolic-opcode)))
	  (gensym-3 (list 'rn (nth 2 symbolic-opcode)))
	  (gensym-4 (progn
		      (multiple-value-setq (class shifter-arg-list)
			(parse-shifter-op (nth 3 symbolic-opcode)))
		      shifter-arg-list)))
      ;; each val     ^^ is the result of (parse-arg <arg> '(nth <arg-num> s-o))
      (apply #'make-instance class 
	     'arm::opcode op-symbol
	     (append opcode-args gensym-2 gensym-3 gensym-4)))))
    
||#

(defun opcode-parse (opcode-definition)	     
  ;; returns values: opcode, opcode-parsing expression
  (cond ((symbolp opcode-definition)
	 (values opcode-definition nil))
	((consp opcode-definition)
	 (values (car opcode-definition)
		 (list 
		  (cond
		    ((equal (cdr opcode-definition) '(COND))
		     'parse-opcode-cond)
		    ((equal (cdr opcode-definition) '(COND S))
		     'parse-opcode-cond-s)
		    ((equal (cdr opcode-definition) '(COND ^))
		     'parse-opcode-cond-s^)
		    (t (error "Bad opcode definition ~A"
			      opcode-definition)))
		  'opcode)))
	(t (error "Bad opcode definition ~A"
		  opcode-definition))))

(defun arg-parse (arg arg-expr)
  (cond
    ((member arg '(rd rn))
     (list 'list arg arg-expr))
    ((eq arg 'shifter-op)
     `(progn
	(multiple-value-setq (class shifter-arg-list)
	  (parse-shifter-op ,arg-expr))
	shifter-arg-list))
    ((and (consp arg)
	  (member (car arg) '(rd rn)))
     (list 'list (car arg) (second arg)))
    (t (error "Bad argument definition ~A for ~A" arg arg-expr))))

(defmacro inst-definition-to-matcher (opcode-def &rest arg-defs)
  "Given a stylized ARM instruction definition, returns a lambda expression
  (lambda (symbolic-opcode))
  which returns a ARM instruction object.

  This parser understands for the opcode-def
  OPCODE
  (OPCODE COND)
  (OPCODE COND S)
  (OPCODE COND ^)
  
  and for args
  RD, RN, SHIFTER-OP
  (RD R0) (RN R0) for cases where there is a fixed encoding."

  (multiple-value-bind (opcode opcode-parse-expression)
      (opcode-parse opcode-def)
    (do ((setf-args nil setf-args)
	 (prologue nil prologue)
	 (sym (gensym) (gensym))
	 (arg-number 1 (1+ arg-number))
	 (arg-def arg-defs (cdr arg-def)))
	((null arg-def) )
      (push (arg-parse arg-def `(nth ,arg-number symbolic-opcode)
	    sym
	    
	 
	   (loop for arg-def in arg-defs
	      and arg-number from 1
	      collect (arg-parse arg-def `(nth ,arg-number 'symbolic-opcode)))))
      `(lambda (symbolic-opcode)
	 (let ((class nil)
	       (arg-list 
	   (let ((opcode ,opcode-parse-expression))
	     (apply #'make-instance class
       
	     
			  
  
		      