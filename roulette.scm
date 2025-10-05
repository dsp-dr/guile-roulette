;;; roulette.scm --- Main entry point for guile-roulette
;;; Commentary:
;;; This is the main module that re-exports all public APIs from the
;;; roulette library, providing probability and measure theory primitives.

(define-module (roulette)
  #:use-module (roulette core)
  #:use-module (roulette inference)
  #:re-export (;; From core
               measurable-space
               measurable-space?
               make-measurable-space
               measurable-space-point
               measure
               measure?
               make-measure
               engine
               engine?
               make-engine
               immutable-set/c

               ;; From inference
               infer
               support
               density
               define-measurable
               define-measurable*))

;;; roulette.scm ends here
