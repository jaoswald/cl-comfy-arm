;;;; -*- mode: Lisp; Syntax: ANSI-Common-Lisp; Package: ("COMFY-ARM-TESTS" ("CL" "ARM" "RT")); -*-
;;;;
;;;; comfy-arm-tests.lisp
;;;;
;;;; Uses RT regression test framework

(cl:defpackage "COMFY-ARM-TESTS"
  (:use "COMMON-LISP" "ARM" #+sbcl "SB-RT" #-sbcl "RT"))

(cl:in-package "COMFY-ARM-TESTS")


