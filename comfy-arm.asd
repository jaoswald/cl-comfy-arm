;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          comfy-arm.asd
;;;; Purpose:       ASDF definition file for COMFY-ARM
;;;; Programmer:    Joseph A. Oswald, III 
;;;; Date Started:  January 2009
;;;;
;;;; This file, part of comfy-arm, is Copyright (c) 2009
;;;; by Joseph A. Oswald, III <josephoswald@gmail.com>
;;;;
;;;; COMFY-ARM is licensed under the terms of the BSD software
;;;; license (see LICENSE.txt).
;;;; *************************************************************************

(asdf:defsystem :comfy-arm
  :name "comfy-arm"
  :version "2009.01.04"
  :author "Joseph A. Oswald, III <josephoswald@gmail.com>"
  :maintainer "Joseph A. Oswald, III <josephoswald@gmail.com>"
  :licence "BSD"
  :description "Baker's COMFY Assembler for ARM"
  :long-description "A 'medium-level' assembly language for the ARM CPU, inspired by Baker's COMFY assembler."
  :components
  ((:file "arm-opcodes")
   (:file "comfy-arm" :depends-on ("arm-opcodes"))))

(asdf:defsystem :comfy-arm-test
  :name "comfy-arm-test"
  :version "2009.01.04"
  :author "Joseph A. Oswald, III <josephoswald@gmail.com>"
  :maintainer "Joseph A. Oswald, III <josephoswald@gmail.com>"
  :description "Tests for comfy-arm"
  :components
  ((:file "comfy-arm-tests"))
  :depends-on (#+sbcl "sb-rt" #-sbcl "rt" "comfy-arm"))

(asdf:defsystem :arm-test
  :name "arm-test"
  :version "2009.01.04"
  :author "Joseph A. Oswald, III <josephoswald@gmail.com>"
  :maintainer "Joseph A. Oswald, III <josephoswald@gmail.com>"
  :description "Tests for ARM opcodes"
  :components
  ((:file "arm-opcodes")
   (:file "arm-tests" :depends-on ("arm-opcodes")))
  :depends-on (#+sbcl "sb-rt" #-sbcl "rt"))
