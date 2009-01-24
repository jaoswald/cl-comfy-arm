;;; -*- mode: Lisp; Syntax: ANSI-Common-Lisp; Package: (COMFY-ARM ("ARM" "CL")); -*-
;;;
;;; comfy-arm.lisp 
;;;
;;; This file is distributed under the terms of the BSD license. 
;;; See LICENSE.txt
;;;
;;; COMFY assembler for the ARM architecture, implemented in 
;;; Common Lisp.
;;;
;;; See Henry G. Baker, `The COMFY 6502 Compiler,' 
;;      Garbage In, Garbage Out, Sigplan Notices, 
;;;     September 1997
;;; available in TeX format as 
;;; http://home.pipeline.com/~hbaker1/sigplannotices/column04.tex.gz
;;;

(cl:defpackage "COMFY-ARM"
  (:use "ARM" "CL")
  (:shadow "COMPILE" "CONSTANTP" "VARIABLEP" "BREAK"
	   "IF" "+" "-" "1+" "1-" "NOT" "LOOP" ;; "PROG"
	   "FOR"
	   ;; "WHEN" "UNLESS" ; candidates for control macros
	   )
  (:export "COMPILE" "INIT" "*MEM*" "RELOCATE"
	   ;; "WHEN" "UNLESS"
	   "EQU"

	   "COMFY-ARM-ERROR" "ERROR-FORM" 
	   "DEFINE-CMACRO"

	   "SEQ" "ALT" "IF" "NOT" "WHILE" "LOOP"
	   "STAR" ;; "PROG"
	   "FORI" "FORJ" "MOVE"
	   "LISP"
	   ))

(cl:in-package "COMFY-ARM")


