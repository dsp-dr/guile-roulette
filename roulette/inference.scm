;;; roulette/inference.scm --- Inference and measure operations
;;; Commentary:
;;; Provides the main API functions: infer, support, density, and the
;;; define-measurable macros for binding measurable values.

(define-module (roulette inference)
  #:use-module (roulette core)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (infer
            support
            density
            define-measurable
            define-measurable*))

;; Global table for measurable values
(define *measurable-table* (make-hash-table))

;; Default inference engine (simple enumeration)
(define *default-engine*
  (make-engine 'default
               (lambda (value)
                 ;; Simple enumeration-based inference
                 (hash-ref *measurable-table* value #f))))

(define* (infer value #:key (engine *default-engine*) (lazy? #f))
  "Reify a measure from the meta level for VALUE.

   Returns the measure associated with a measurable value.
   ENGINE specifies the inference engine (default: enumeration).
   LAZY? enables lazy evaluation of the measure."
  (if engine
      ((engine-compute engine) value)
      (error "No engine provided for inference")))

(define (support measure-obj)
  "Return the largest set with positive measure for MEASURE-OBJ.

   For lazy measures, may return a superset of the actual support.
   Returns #f if support cannot be determined."
  (if (measure? measure-obj)
      (let ((space (measure-space measure-obj)))
        ;; This is a simplified implementation
        ;; A full implementation would compute the actual support
        space)
      (error "support: expected a measure" measure-obj)))

(define (density measure-obj)
  "Return the derivative (density function) of MEASURE-OBJ.

   Returns a function that maps measurable space points to density values."
  (if (measure? measure-obj)
      (measure-func measure-obj)
      (error "density: expected a measure" measure-obj)))

;; Helper for define-measurable
(define (register-measurable! id value measure-obj)
  "Register a measurable value with its measure."
  (hash-set! *measurable-table* value measure-obj)
  value)

(define-syntax define-measurable
  (syntax-rules ()
    ((_ (id ...) measure-expr)
     (begin
       (define temp-measure measure-expr)
       (define id
         (let ((val (gensym "measurable")))
           (register-measurable! 'id val temp-measure)
           val))
       ...))))

(define-syntax define-measurable*
  (syntax-rules ()
    ((_ (id ...) measure-expr)
     (begin
       (define id
         (let ((val (gensym "measurable"))
               (meas measure-expr))
           (register-measurable! 'id val meas)
           val))
       ...))))

;;; inference.scm ends here
