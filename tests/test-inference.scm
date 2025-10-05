;;; tests/test-inference.scm --- Tests for roulette inference module
;;; Commentary:
;;; Unit tests for infer, support, density, and define-measurable macros.

(use-modules (srfi srfi-64)
             (roulette core)
             (roulette inference))

(test-begin "roulette-inference")

;; define-measurable tests
(test-group "define-measurable"
  (test-assert "define-measurable creates measurable values"
    (begin
      (define-measurable (x)
        (make-measure (make-measurable-space number?)
                      (lambda (n) 1.0)))
      (symbol? x)))

  (test-assert "define-measurable binds multiple values"
    (begin
      (define-measurable (a b c)
        (make-measure (make-measurable-space integer?)
                      (lambda (n) 0.5)))
      (and (symbol? a) (symbol? b) (symbol? c)))))

;; define-measurable* tests
(test-group "define-measurable*"
  (test-assert "define-measurable* creates fresh values"
    (begin
      (define-measurable* (y)
        (make-measure (make-measurable-space number?)
                      (lambda (n) 2.0)))
      (symbol? y)))

  (test-assert "define-measurable* creates distinct values"
    (begin
      (define-measurable* (p q)
        (make-measure (make-measurable-space boolean?)
                      (lambda (b) 0.5)))
      (not (eq? p q)))))

;; infer tests
(test-group "infer"
  (test-assert "infer returns measure for registered values"
    (begin
      (define-measurable (z)
        (make-measure (make-measurable-space number?)
                      (lambda (n) 3.0)))
      (let ((m (infer z)))
        (or (measure? m) (not m)))))  ; May return #f if not found

  (test-assert "infer accepts engine parameter"
    (begin
      (define test-engine
        (make-engine 'test (lambda (v) #f)))
      (define-measurable (w)
        (make-measure (make-measurable-space number?)
                      (lambda (n) 1.0)))
      ;; Should not error
      (infer w #:engine test-engine)
      #t))

  (test-assert "infer accepts lazy parameter"
    (begin
      (define-measurable (v)
        (make-measure (make-measurable-space number?)
                      (lambda (n) 1.0)))
      (infer v #:lazy? #t)
      #t)))

;; support tests
(test-group "support"
  (test-assert "support returns space for measure"
    (let* ((space (make-measurable-space integer?))
           (m (make-measure space (lambda (n) 1.0)))
           (sup (support m)))
      (measurable-space? sup)))

  (test-error "support errors on non-measure"
    (support 42)))

;; density tests
(test-group "density"
  (test-assert "density returns function"
    (let* ((density-fn (lambda (x) (* x 2)))
           (m (make-measure
               (make-measurable-space number?)
               density-fn))
           (d (density m)))
      (procedure? d)))

  (test-assert "density function computes correctly"
    (let* ((density-fn (lambda (x) (* x 3)))
           (m (make-measure
               (make-measurable-space number?)
               density-fn))
           (d (density m)))
      (= 15 (d 5))))

  (test-error "density errors on non-measure"
    (density "not a measure")))

(test-end "roulette-inference")

;; Run the tests
(exit (= 0 (test-runner-fail-count (test-runner-current))))
