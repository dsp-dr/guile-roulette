;;; tests/test-pbt-properties.scm --- Property-based distribution invariant tests
;;; Commentary:
;;; SRFI-64 tests encoding property-based thinking: parameterized inputs,
;;; exhaustive checks over finite domains, and algebraic invariants that
;;; should hold for all valid measures. Written without hegel-guile
;;; dependency; uses explicit enumeration as a PBT substitute.
;;;
;;; Properties tested:
;;;   1. Measure normalization (densities sum correctly over finite support)
;;;   2. Discrete density invariants (non-negativity)
;;;   3. Infer/support round-trips
;;;   4. define-measurable macro correctness
;;;   5. Composition properties (measures preserve structure)

(use-modules (srfi srfi-1)
             (srfi srfi-64)
             (roulette core)
             (roulette inference))

(test-begin "roulette-pbt-properties")

;;; ---- Helpers ----

(define epsilon 1e-10)

;;; for-all/list: run a property check over every element of a list.
;;; Returns #t only if PROP holds for all elements.
(define (for-all/list lst prop)
  (every prop lst))

;;; make-discrete-measure: create a measure over a finite domain
;;; with a given density-alist ((value . weight) ...).
(define (make-discrete-measure domain density-alist)
  (let ((space (make-measurable-space
                (lambda (x) (memv x domain)))))
    (make-measure
     space
     (lambda (x)
       (let ((pair (assv x density-alist)))
         (if pair (cdr pair) 0))))))

;;; sum-densities: sum the density function over every element of domain.
(define (sum-densities measure-obj domain)
  (fold (lambda (x acc) (+ acc ((density measure-obj) x)))
        0
        domain))

;;; =========================================================================
;;; Property 1: Measure normalization
;;; For any proper discrete probability distribution, densities sum to 1.
;;; =========================================================================

(test-group "property:measure-normalization"

  ;; Fair die: uniform 1/6 over {1..6} => sums to 1
  (test-assert "fair die densities sum to 1"
    (let* ((domain '(1 2 3 4 5 6))
           (alist (map (lambda (x) (cons x 1/6)) domain))
           (m (make-discrete-measure domain alist))
           (total (sum-densities m domain)))
      (< (abs (- total 1)) epsilon)))

  ;; Fair coin: 1/2 each => sums to 1
  (test-assert "fair coin densities sum to 1"
    (let* ((domain '(#t #f))
           (alist (list (cons #t 1/2) (cons #f 1/2)))
           (m (make-discrete-measure domain alist))
           (total (sum-densities m domain)))
      (< (abs (- total 1)) epsilon)))

  ;; Biased coin: 0.7 / 0.3 => sums to 1
  (test-assert "biased coin densities sum to 1"
    (let* ((domain '(#t #f))
           (alist (list (cons #t 0.7) (cons #f 0.3)))
           (m (make-discrete-measure domain alist))
           (total (sum-densities m domain)))
      (< (abs (- total 1)) epsilon)))

  ;; Parameterized: for every n in {2..10}, uniform 1/n over {1..n} sums to 1
  (test-assert "uniform(1/n) over {1..n} sums to 1 for n=2..10"
    (for-all/list
     (iota 9 2)  ; (2 3 4 5 6 7 8 9 10)
     (lambda (n)
       (let* ((domain (iota n 1))
              (alist (map (lambda (x) (cons x (/ 1 n))) domain))
              (m (make-discrete-measure domain alist))
              (total (sum-densities m domain)))
         (< (abs (- total 1)) epsilon)))))

  ;; Parameterized: weighted distributions that sum to 1
  ;; For weights (w1, w2, w3) where w1+w2+w3=1, measure normalizes
  (test-assert "three-outcome weighted distributions normalize"
    (for-all/list
     '((1/3 1/3 1/3)
       (1/2 1/4 1/4)
       (1/6 1/6 2/3)
       (0 0 1)
       (1/10 2/10 7/10))
     (lambda (weights)
       (let* ((domain '(a b c))
              (alist (map cons domain weights))
              (m (make-discrete-measure domain alist))
              (total (sum-densities m domain)))
         (< (abs (- total 1)) epsilon))))))

;;; =========================================================================
;;; Property 2: Discrete density invariants
;;; For any valid probability measure, density(x) >= 0 for all x in support.
;;; =========================================================================

(test-group "property:density-non-negativity"

  ;; All elements of a fair die have non-negative density
  (test-assert "fair die: all densities non-negative"
    (let* ((domain '(1 2 3 4 5 6))
           (alist (map (lambda (x) (cons x 1/6)) domain))
           (m (make-discrete-measure domain alist)))
      (for-all/list domain
                    (lambda (x) (>= ((density m) x) 0)))))

  ;; Bernoulli with various p values: density always non-negative
  (test-assert "bernoulli: density non-negative for p in {0, 0.1, ..., 1.0}"
    (for-all/list
     (map (lambda (i) (/ i 10)) (iota 11))  ; 0, 1/10, ..., 1
     (lambda (p)
       (let* ((domain '(#t #f))
              (alist (list (cons #t p) (cons #f (- 1 p))))
              (m (make-discrete-measure domain alist)))
         (and (>= ((density m) #t) 0)
              (>= ((density m) #f) 0))))))

  ;; Zero density is valid and non-negative
  (test-assert "zero density is non-negative"
    (let* ((m (make-measure (make-measurable-space number?)
                            (lambda (x) 0))))
      (for-all/list '(-100 -1 0 1 42 999)
                    (lambda (x) (>= ((density m) x) 0)))))

  ;; Density at points outside support should be 0 (non-negative)
  (test-assert "density outside support is zero"
    (let* ((domain '(1 2 3))
           (alist '((1 . 1/3) (2 . 1/3) (3 . 1/3)))
           (m (make-discrete-measure domain alist)))
      (for-all/list '(0 4 5 -1 100)
                    (lambda (x) (= 0 ((density m) x)))))))

;;; =========================================================================
;;; Property 3: Infer/support round-trips
;;; Inferring a measurable value and extracting support gives back the space.
;;; =========================================================================

(test-group "property:infer-support-roundtrip"

  ;; For each measurable space type, infer then support should return a space
  (test-assert "infer+support round-trip for number space"
    (begin
      (define-measurable (rt-num)
        (make-measure (make-measurable-space number?)
                      (lambda (n) 1.0)))
      (let* ((m (infer rt-num))
             (sup (support m)))
        (and (measurable-space? sup)
             ((measurable-space-point sup) 42)
             ((measurable-space-point sup) 3.14)
             (not ((measurable-space-point sup) "hello"))))))

  ;; Round-trip preserves the predicate identity
  (test-assert "infer+support preserves predicate behavior for integer space"
    (begin
      (define-measurable (rt-int)
        (make-measure (make-measurable-space integer?)
                      (lambda (n) 0.5)))
      (let* ((m (infer rt-int))
             (sup (support m))
             (pred (measurable-space-point sup)))
        (and (pred 0) (pred -1) (pred 100)
             (not (pred 1.5)) (not (pred "abc"))))))

  ;; Round-trip for boolean space
  (test-assert "infer+support round-trip for boolean space"
    (begin
      (define-measurable (rt-bool)
        (make-measure (make-measurable-space boolean?)
                      (lambda (b) 0.5)))
      (let* ((m (infer rt-bool))
             (sup (support m))
             (pred (measurable-space-point sup)))
        (and (pred #t) (pred #f)
             (not (pred 0)) (not (pred "true"))))))

  ;; Round-trip for finite domain space
  (test-assert "infer+support round-trip for finite domain"
    (let ((die-pred (lambda (x) (and (integer? x) (<= 1 x 6)))))
      (define-measurable (rt-die)
        (make-measure (make-measurable-space die-pred)
                      (lambda (x) 1/6)))
      (let* ((m (infer rt-die))
             (sup (support m))
             (pred (measurable-space-point sup)))
        (for-all/list '(1 2 3 4 5 6)
                      (lambda (x) (pred x))))))

  ;; Density is also recoverable after round-trip
  (test-assert "infer round-trip preserves density values"
    (begin
      (define-measurable (rt-dens)
        (make-measure (make-measurable-space number?)
                      (lambda (x) (* x x))))
      (let* ((m (infer rt-dens))
             (d (density m)))
        (for-all/list '(0 1 2 3 5 10)
                      (lambda (x) (= (d x) (* x x))))))))

;;; =========================================================================
;;; Property 4: define-measurable macro correctness
;;; Measurable values created by the macro should be retrievable and
;;; have the expected properties.
;;; =========================================================================

(test-group "property:define-measurable-correctness"

  ;; Property: define-measurable creates symbols (gensyms)
  (test-assert "define-measurable produces symbols"
    (begin
      (define-measurable (mac-sym1 mac-sym2 mac-sym3)
        (make-measure (make-measurable-space number?)
                      (lambda (n) 1.0)))
      (for-all/list (list mac-sym1 mac-sym2 mac-sym3)
                    symbol?)))

  ;; Property: define-measurable shares a single measure across all bindings
  (test-assert "define-measurable all bindings share same measure"
    (begin
      (define-measurable (mac-sh1 mac-sh2 mac-sh3)
        (make-measure (make-measurable-space number?)
                      (lambda (n) 1.0)))
      (let ((m1 (infer mac-sh1))
            (m2 (infer mac-sh2))
            (m3 (infer mac-sh3)))
        (and (eq? m1 m2) (eq? m2 m3)))))

  ;; Property: define-measurable* creates distinct measures per binding
  (test-assert "define-measurable* each binding has distinct measure"
    (begin
      (define-measurable* (mac-d1 mac-d2 mac-d3)
        (make-measure (make-measurable-space number?)
                      (lambda (n) 1.0)))
      (let ((m1 (infer mac-d1))
            (m2 (infer mac-d2))
            (m3 (infer mac-d3)))
        (and (not (eq? m1 m2))
             (not (eq? m2 m3))
             (not (eq? m1 m3))))))

  ;; Property: define-measurable* bindings all produce valid measures
  (test-assert "define-measurable* all bindings produce valid measures"
    (begin
      (define-measurable* (mac-v1 mac-v2 mac-v3 mac-v4)
        (make-measure (make-measurable-space integer?)
                      (lambda (n) 0.25)))
      (for-all/list (list mac-v1 mac-v2 mac-v3 mac-v4)
                    (lambda (v)
                      (let ((m (infer v)))
                        (and (measure? m)
                             (= 0.25 ((density m) 99))))))))

  ;; Property: each gensym from define-measurable* is unique
  (test-assert "define-measurable* all gensyms are distinct"
    (begin
      (define-measurable* (mac-u1 mac-u2 mac-u3 mac-u4 mac-u5)
        (make-measure (make-measurable-space number?)
                      (lambda (n) 1.0)))
      (let ((vals (list mac-u1 mac-u2 mac-u3 mac-u4 mac-u5)))
        ;; All pairs are distinct
        (let loop ((rest vals))
          (if (null? rest)
              #t
              (and (not (any (lambda (v) (eq? (car rest) v))
                             (cdr rest)))
                   (loop (cdr rest)))))))))

;;; =========================================================================
;;; Property 5: Composition properties
;;; Composing measures should preserve structural invariants.
;;; =========================================================================

(test-group "property:composition"

  ;; Property: composing density functions preserves non-negativity
  ;; If f(x) >= 0 and g(x) >= 0 then f(x) * g(x) >= 0
  (test-assert "product of non-negative densities is non-negative"
    (let* ((m1 (make-measure (make-measurable-space number?)
                             (lambda (x) (abs x))))
           (m2 (make-measure (make-measurable-space number?)
                             (lambda (x) (* x x))))
           (d1 (density m1))
           (d2 (density m2))
           (product-density (lambda (x) (* (d1 x) (d2 x)))))
      (for-all/list '(-5 -1 0 1 5 100)
                    (lambda (x) (>= (product-density x) 0)))))

  ;; Property: scaling a measure by a positive constant preserves non-negativity
  (test-assert "scaled density preserves non-negativity"
    (for-all/list
     '(0.001 0.5 1 2 10 100)
     (lambda (scale)
       (let* ((m (make-measure (make-measurable-space number?)
                               (lambda (x) (/ 1 6))))
              (d (density m))
              (scaled-d (lambda (x) (* scale (d x)))))
         (for-all/list '(-10 0 1 5 42)
                       (lambda (x) (>= (scaled-d x) 0)))))))

  ;; Property: measure-space is invariant under density transformation
  ;; Changing the density function doesn't change the space
  (test-assert "measure-space is independent of density function"
    (let* ((space (make-measurable-space integer?))
           (m1 (make-measure space (lambda (x) 1.0)))
           (m2 (make-measure space (lambda (x) (* x x))))
           (m3 (make-measure space (lambda (x) 0))))
      (and (eq? (measure-space m1) (measure-space m2))
           (eq? (measure-space m2) (measure-space m3)))))

  ;; Property: composing measurable space predicates via conjunction
  ;; intersection of two spaces should only accept points in both
  (test-assert "intersection of spaces accepts only shared points"
    (let* ((s1 (make-measurable-space integer?))
           (s2 (make-measurable-space (lambda (x) (and (number? x) (> x 0)))))
           (p1 (measurable-space-point s1))
           (p2 (measurable-space-point s2))
           (intersection-pred (lambda (x) (and (p1 x) (p2 x))))
           (s-inter (make-measurable-space intersection-pred))
           (pred (measurable-space-point s-inter)))
      (and
       ;; Positive integers pass
       (for-all/list '(1 2 3 100)
                     (lambda (x) (pred x)))
       ;; Non-positive integers fail
       (for-all/list '(-1 0 -100)
                     (lambda (x) (not (pred x))))
       ;; Non-integers fail
       (for-all/list '(0.5 1.1 3.14)
                     (lambda (x) (not (pred x)))))))

  ;; Property: composing measurable space predicates via disjunction
  ;; union of two spaces should accept points in either
  (test-assert "union of spaces accepts points in either"
    (let* ((s1 (make-measurable-space string?))
           (s2 (make-measurable-space number?))
           (p1 (measurable-space-point s1))
           (p2 (measurable-space-point s2))
           (union-pred (lambda (x) (or (p1 x) (p2 x))))
           (s-union (make-measurable-space union-pred))
           (pred (measurable-space-point s-union)))
      (and
       ;; Strings pass
       (for-all/list '("hello" "" "world")
                     (lambda (x) (pred x)))
       ;; Numbers pass
       (for-all/list '(0 1 -1 3.14)
                     (lambda (x) (pred x)))
       ;; Other types fail
       (for-all/list '(#t #f)
                     (lambda (x) (not (pred x)))))))

  ;; Property: measure over union space has well-defined density at all union points
  (test-assert "measure over union space has consistent density"
    (let* ((union-pred (lambda (x) (or (string? x) (number? x))))
           (space (make-measurable-space union-pred))
           (m (make-measure space
                            (lambda (x)
                              (cond
                               ((string? x) (string-length x))
                               ((number? x) (abs x))
                               (else 0)))))
           (d (density m)))
      (and (= 5 (d "hello"))
           (= 0 (d ""))
           (= 42 (d 42))
           (= 3 (d -3))))))

(define exit-status (test-runner-fail-count (test-runner-current)))
(test-end "roulette-pbt-properties")
(exit (= 0 exit-status))
