;;; roulette/core.scm --- Core measurable space and measure types
;;; Commentary:
;;; This module provides the foundational types for measurable spaces,
;;; measures, and related operations. Ported from Racket's roulette library.

(define-module (roulette core)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 match)
  #:export (measurable-space
            measurable-space?
            make-measurable-space
            measurable-space-predicate
            measurable-space-point

            measure
            measure?
            make-measure
            measure-space
            measure-func

            engine
            engine?
            make-engine
            engine-id
            engine-compute

            immutable-set/c))

;; Measurable space: represents a domain with a predicate
(define-record-type <measurable-space>
  (make-measurable-space-internal predicate)
  measurable-space?
  (predicate measurable-space-predicate))

(define (make-measurable-space pred)
  "Create a measurable space with predicate PRED."
  (make-measurable-space-internal pred))

(define (measurable-space pred)
  "Syntactic sugar for make-measurable-space."
  (make-measurable-space pred))

(define (measurable-space-point space)
  "Return the predicate for points in SPACE."
  (measurable-space-predicate space))

;; Measure: maps from measurable space to a commutative monoid
(define-record-type <measure>
  (make-measure-internal space func)
  measure?
  (space measure-space)
  (func measure-func))

(define (make-measure space func)
  "Create a measure over SPACE with function FUNC."
  (make-measure-internal space func))

(define (measure space func)
  "Syntactic sugar for make-measure."
  (make-measure space func))

;; Engine: enables inference on measurable values
(define-record-type <engine>
  (make-engine-internal id compute-func)
  engine?
  (id %engine-id)
  (compute-func %engine-compute))

(define (engine-id eng)
  "Get the ID of an engine."
  (%engine-id eng))

(define (engine-compute eng)
  "Get the compute function of an engine."
  (%engine-compute eng))

(define (make-engine id compute-func)
  "Create an inference engine with ID and COMPUTE-FUNC."
  (make-engine-internal id compute-func))

(define (engine id compute-func)
  "Syntactic sugar for make-engine."
  (make-engine id compute-func))

;; Contract-like predicates
(define (immutable-set/c elem-pred)
  "Create a measurable space for immutable sets containing elements satisfying ELEM-pred."
  (make-measurable-space
   (lambda (s)
     (and (list? s)
          (every elem-pred s)))))

;;; core.scm ends here
