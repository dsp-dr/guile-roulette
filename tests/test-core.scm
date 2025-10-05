;;; tests/test-core.scm --- Tests for roulette core module
;;; Commentary:
;;; Unit tests for measurable spaces, measures, and engines using SRFI-64.

(use-modules (srfi srfi-64)
             (roulette core))

(test-begin "roulette-core")

;; Measurable space tests
(test-group "measurable-space"
  (test-assert "measurable-space? recognizes spaces"
    (measurable-space? (make-measurable-space number?)))

  (test-assert "measurable-space-point returns predicate"
    (let* ((pred number?)
           (space (make-measurable-space pred)))
      (eq? pred (measurable-space-point space))))

  (test-assert "measurable-space accepts values in domain"
    (let* ((space (make-measurable-space integer?))
           (pred (measurable-space-point space)))
      (pred 42)))

  (test-assert "measurable-space rejects values outside domain"
    (let* ((space (make-measurable-space integer?))
           (pred (measurable-space-point space)))
      (not (pred 3.14)))))

;; Measure tests
(test-group "measure"
  (test-assert "measure? recognizes measures"
    (measure? (make-measure
               (make-measurable-space number?)
               (lambda (x) 1.0))))

  (test-assert "measure-space returns correct space"
    (let* ((space (make-measurable-space number?))
           (m (make-measure space (lambda (x) 1.0))))
      (eq? space (measure-space m))))

  (test-assert "measure-func returns density function"
    (let* ((density-fn (lambda (x) (* 2 x)))
           (m (make-measure
               (make-measurable-space number?)
               density-fn)))
      (= 10 ((measure-func m) 5)))))

;; Engine tests
(test-group "engine"
  (test-assert "engine? recognizes engines"
    (engine? (make-engine 'test (lambda (x) x))))

  (test-assert "engine-id returns correct id"
    (let ((eng (make-engine 'my-engine (lambda (x) x))))
      (eq? 'my-engine (engine-id eng))))

  (test-assert "engine-compute returns compute function"
    (let* ((compute (lambda (x) (* x 2)))
           (eng (make-engine 'test compute)))
      (= 20 ((engine-compute eng) 10)))))

;; immutable-set/c tests
(test-group "immutable-set/c"
  (test-assert "immutable-set/c accepts valid sets"
    (let* ((space (immutable-set/c number?))
           (pred (measurable-space-point space)))
      (pred '(1 2 3))))

  (test-assert "immutable-set/c rejects invalid sets"
    (let* ((space (immutable-set/c number?))
           (pred (measurable-space-point space)))
      (not (pred '(1 "two" 3)))))

  (test-assert "immutable-set/c accepts empty set"
    (let* ((space (immutable-set/c number?))
           (pred (measurable-space-point space)))
      (pred '()))))

(test-end "roulette-core")

;; Run the tests
(exit (= 0 (test-runner-fail-count (test-runner-current))))
