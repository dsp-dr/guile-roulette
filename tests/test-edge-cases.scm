;;; tests/test-edge-cases.scm --- Edge case and integration tests
;;; Commentary:
;;; Tests for boundary conditions, error handling, and composition patterns
;;; that are not covered by the basic test-core and test-inference suites.

(use-modules (srfi srfi-64)
             (roulette core)
             (roulette inference))

(test-begin "roulette-edge-cases")

;;; ---- Measurable space edge cases ----

(test-group "measurable-space-edge-cases"

  ;; Spaces with compound predicates
  (test-assert "measurable-space with compound predicate"
    (let* ((space (make-measurable-space
                   (lambda (x) (and (number? x) (>= x 0)))))
           (pred (measurable-space-point space)))
      (and (pred 0)
           (pred 42)
           (pred 3.14)
           (not (pred -1))
           (not (pred "hello")))))

  ;; Spaces with string predicates
  (test-assert "measurable-space with string predicate"
    (let* ((space (make-measurable-space string?))
           (pred (measurable-space-point space)))
      (and (pred "hello")
           (pred "")
           (not (pred 42))
           (not (pred #t)))))

  ;; Spaces with lambda predicates for finite domains
  (test-assert "measurable-space for finite domain"
    (let* ((space (make-measurable-space
                   (lambda (x) (memv x '(1 2 3 4 5 6)))))
           (pred (measurable-space-point space)))
      (and (pred 1) (pred 6)
           (not (pred 0)) (not (pred 7)))))

  ;; measurable-space syntactic sugar is equivalent
  (test-assert "measurable-space sugar equals make-measurable-space"
    (let* ((s1 (measurable-space number?))
           (s2 (make-measurable-space number?)))
      (and (measurable-space? s1)
           (measurable-space? s2))))

  ;; measurable-space? rejects non-spaces
  (test-assert "measurable-space? rejects non-spaces"
    (and (not (measurable-space? 42))
         (not (measurable-space? "space"))
         (not (measurable-space? '()))
         (not (measurable-space? #t)))))

;;; ---- Measure edge cases ----

(test-group "measure-edge-cases"

  ;; Measure with zero density everywhere
  (test-assert "measure with zero density"
    (let* ((m (make-measure
               (make-measurable-space number?)
               (lambda (x) 0)))
           (d (density m)))
      (and (= 0 (d 0))
           (= 0 (d 100))
           (= 0 (d -50)))))

  ;; Measure returning fractional densities
  (test-assert "measure with fractional density"
    (let* ((m (make-measure
               (make-measurable-space integer?)
               (lambda (x) (/ 1 6))))
           (d (density m)))
      (= 1/6 (d 3))))

  ;; measure syntactic sugar is equivalent
  (test-assert "measure sugar creates valid measure"
    (let ((m (measure (make-measurable-space boolean?)
                      (lambda (b) 0.5))))
      (measure? m)))

  ;; measure? rejects non-measures
  (test-assert "measure? rejects non-measures"
    (and (not (measure? 42))
         (not (measure? "measure"))
         (not (measure? (make-measurable-space number?)))
         (not (measure? (make-engine 'test (lambda (x) x)))))))

;;; ---- Density function patterns ----

(test-group "density-patterns"

  ;; Exponential distribution (from API docs)
  (test-assert "exponential distribution density"
    (let* ((m (make-measure
               (make-measurable-space
                (lambda (x) (and (real? x) (>= x 0))))
               (lambda (x) (* 2 (exp (* -2 x))))))
           (d (density m)))
      (and (< (abs (- (d 0) 2.0)) 1e-10)
           (< (abs (- (d 0.5) (* 2 (exp -1)))) 1e-10))))

  ;; Uniform distribution over unit interval
  (test-assert "uniform distribution density"
    (let* ((m (make-measure
               (make-measurable-space
                (lambda (x) (and (real? x) (<= 0 x 1))))
               (lambda (x) 1.0)))
           (d (density m)))
      (and (= 1.0 (d 0))
           (= 1.0 (d 0.5))
           (= 1.0 (d 1)))))

  ;; Fair die distribution
  (test-assert "fair die density"
    (let* ((m (make-measure
               (make-measurable-space
                (lambda (x) (and (integer? x) (<= 1 x 6))))
               (lambda (x) (/ 1 6))))
           (d (density m)))
      (and (= 1/6 (d 1))
           (= 1/6 (d 6)))))

  ;; Biased coin (Bernoulli)
  (test-assert "biased coin density"
    (let* ((m (make-measure
               (make-measurable-space boolean?)
               (lambda (outcome)
                 (if outcome 0.7 0.3))))
           (d (density m)))
      (and (= 0.7 (d #t))
           (= 0.3 (d #f))))))

;;; ---- Engine edge cases ----

(test-group "engine-edge-cases"

  ;; engine syntactic sugar
  (test-assert "engine sugar creates valid engine"
    (let ((e (engine 'test-sugar (lambda (x) x))))
      (engine? e)))

  ;; engine? rejects non-engines
  (test-assert "engine? rejects non-engines"
    (and (not (engine? 42))
         (not (engine? "engine"))
         (not (engine? (make-measurable-space number?)))
         (not (engine? (make-measure
                        (make-measurable-space number?)
                        (lambda (x) 1.0))))))

  ;; engine with symbol id
  (test-assert "engine id can be any value"
    (let ((e (make-engine "string-id" (lambda (x) x))))
      (equal? "string-id" (engine-id e)))))

;;; ---- immutable-set/c edge cases ----

(test-group "immutable-set/c-edge-cases"

  ;; immutable-set/c rejects non-lists
  (test-assert "immutable-set/c rejects non-list"
    (let* ((space (immutable-set/c number?))
           (pred (measurable-space-point space)))
      (and (not (pred 42))
           (not (pred "not a list"))
           (not (pred #t)))))

  ;; immutable-set/c with string elements
  (test-assert "immutable-set/c with string elements"
    (let* ((space (immutable-set/c string?))
           (pred (measurable-space-point space)))
      (and (pred '("hello" "world"))
           (pred '())
           (not (pred '(1 2 3))))))

  ;; immutable-set/c with compound predicate
  (test-assert "immutable-set/c with compound predicate"
    (let* ((space (immutable-set/c
                   (lambda (x) (and (integer? x) (> x 0)))))
           (pred (measurable-space-point space)))
      (and (pred '(1 2 3))
           (not (pred '(0 1 2)))
           (not (pred '(-1 1)))))))

;;; ---- Inference integration ----

(test-group "inference-integration"

  ;; infer returns #f for unregistered values
  (test-assert "infer returns #f for unregistered value"
    (not (infer (gensym "never-registered"))))

  ;; infer with custom engine that always returns a measure
  (test-assert "infer uses custom engine compute function"
    (let* ((custom-measure
            (make-measure (make-measurable-space number?)
                          (lambda (x) 42.0)))
           (custom-engine
            (make-engine 'custom
                         (lambda (v) custom-measure)))
           (result (infer 'anything #:engine custom-engine)))
      (and (measure? result)
           (= 42.0 ((density result) 0)))))

  ;; support returns the same space used in measure construction
  (test-assert "support returns original space"
    (let* ((space (make-measurable-space integer?))
           (m (make-measure space (lambda (n) 1.0)))
           (sup (support m)))
      (eq? space sup)))

  ;; density returns the same function used in measure construction
  (test-assert "density returns original function"
    (let* ((fn (lambda (x) (* x x)))
           (m (make-measure (make-measurable-space number?) fn))
           (d (density m)))
      (and (= 0 (d 0))
           (= 1 (d 1))
           (= 25 (d 5))))))

;;; ---- define-measurable patterns ----

(test-group "define-measurable-patterns"

  ;; define-measurable with single variable
  (test-assert "define-measurable single variable"
    (begin
      (define-measurable (single-var)
        (make-measure (make-measurable-space number?)
                      (lambda (n) 1.0)))
      (symbol? single-var)))

  ;; define-measurable values can be inferred
  (test-assert "define-measurable values are inferable"
    (begin
      (define-measurable (inferable)
        (make-measure (make-measurable-space number?)
                      (lambda (n) 7.0)))
      (let ((m (infer inferable)))
        (and (measure? m)
             (= 7.0 ((density m) 0))))))

  ;; define-measurable* values are all inferable independently
  (test-assert "define-measurable* all values inferable"
    (begin
      (define-measurable* (d1 d2 d3)
        (make-measure (make-measurable-space number?)
                      (lambda (n) 5.0)))
      (let ((m1 (infer d1))
            (m2 (infer d2))
            (m3 (infer d3)))
        (and (measure? m1) (measure? m2) (measure? m3)))))

  ;; define-measurable shares the same measure
  (test-assert "define-measurable shares measure structure"
    (begin
      (define-measurable (shared-a shared-b)
        (make-measure (make-measurable-space boolean?)
                      (lambda (b) 0.5)))
      (let ((m-a (infer shared-a))
            (m-b (infer shared-b)))
        ;; Both should return the same measure object
        (and (measure? m-a)
             (measure? m-b)
             (eq? m-a m-b)))))

  ;; define-measurable* creates distinct measures
  (test-assert "define-measurable* creates distinct measures"
    (begin
      (define-measurable* (distinct-a distinct-b)
        (make-measure (make-measurable-space boolean?)
                      (lambda (b) 0.5)))
      ;; Each call to measure-expr creates a fresh measure
      (let ((m-a (infer distinct-a))
            (m-b (infer distinct-b)))
        (and (measure? m-a)
             (measure? m-b)
             (not (eq? m-a m-b)))))))

;;; ---- Error handling ----

(test-group "error-handling"

  ;; support on non-measure raises error
  (test-error "support on number"
    (support 42))

  (test-error "support on string"
    (support "not a measure"))

  (test-error "support on list"
    (support '(1 2 3)))

  ;; density on non-measure raises error
  (test-error "density on number"
    (density 42))

  (test-error "density on boolean"
    (density #t))

  (test-error "density on measurable-space"
    (density (make-measurable-space number?))))

(define exit-status (test-runner-fail-count (test-runner-current)))
(test-end "roulette-edge-cases")
(exit (= 0 exit-status))
