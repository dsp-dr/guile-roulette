#!/usr/bin/env guile
!#

;;; examples/basic.scm --- Basic examples of using guile-roulette

(add-to-load-path (dirname (dirname (current-filename))))

(use-modules (roulette))

;; Example 1: Simple measurable space
(display "Example 1: Measurable Spaces\n")
(define int-space (make-measurable-space integer?))
(define int-pred (measurable-space-point int-space))
(display (format #f "  Is 42 an integer? ~a\n" (int-pred 42)))
(display (format #f "  Is 3.14 an integer? ~a\n" (int-pred 3.14)))

;; Example 2: Measures
(display "\nExample 2: Measures\n")
(define uniform-measure
  (make-measure
   (make-measurable-space number?)
   (lambda (x) 1.0)))  ; Constant density

(define density-fn (density uniform-measure))
(display (format #f "  Density at 5: ~a\n" (density-fn 5)))

;; Example 3: Discrete probability with define-measurable
(display "\nExample 3: Discrete Probability\n")
(define-measurable (coin)
  (make-measure
   (make-measurable-space boolean?)
   (lambda (outcome) 0.5)))

(display (format #f "  Coin flip measurable value created\n"))

;; Example 4: Multiple measurable values
(display "\nExample 4: Multiple Measurable Values\n")
(define-measurable* (die1 die2)
  (make-measure
   (make-measurable-space
    (lambda (x) (and (integer? x) (<= 1 x 6))))
   (lambda (n) (/ 1 6))))

(display (format #f "  Two independent dice created: ~a and ~a\n"
                 (equal? die1 die2)
                 (eq? die1 die2)))

;; Example 5: Immutable sets
(display "\nExample 5: Immutable Sets\n")
(define num-set-space (immutable-set/c number?))
(define set-pred (measurable-space-point num-set-space))
(display (format #f "  Is '(1 2 3) a valid number set? ~a\n"
                 (set-pred '(1 2 3))))
(display (format #f "  Is '(1 \"a\" 3) a valid number set? ~a\n"
                 (set-pred '(1 "a" 3))))

(display "\nAll examples completed!\n")
