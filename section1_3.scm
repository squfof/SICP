; EXERCISE 1.29
;
; We are asked to implement Simpson's Rule for approximating int(f(x), x=a..b) by the formula:
;    h / 3 * (y_0 + 4*y_1 + 2*y_2 + ... + 2*y_n-2 + 4*y_n-1 + y_n_
;
; First we implement the summation and increment procedures introduced in the book:
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n)
  (+ n 1))

; For Simpson's Rule, it's pretty straightforward. We define h [which is constant throughout the
; calculation, then y_k, which we need to define each term [which is a function of k] in the sum:
(define (simpson-integral f a b n)
  (define h
    (/ (- b a)
       n))
  (define (y k)
    (f (+ a
          (* k h))))
  (define (simpson-term k)
    (cond
     ((or (= k 0)
          (= k n)) (y k))
     ((= 0 (remainder k 2)) (* 2 (y k)))
     (else (* 4 (y k)))))
  (* (/ h 3.0)
     (sum simpson-term 0 inc n)))

; The book asks us to test out our procedure on x^3 and compare it to the approximations generated
; therein. However, this is cheating since Simpson's Rule is exact for all polynomials of degree three
; or less!

(define (cube x)
  (* x x x))

(simpson-integral cube 0 1 100)
; 0.25, compared to 0.24998750000000042 from the book

(simpson-integral cube 0 1 1000)
; 0.25, compared to 0.249999875000001 from the book


; EXERCISE 1.30
;
; The (sum ...) procedure above is linear recursion, and we are asked to convert it into an iterative
; procedure.
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))


; EXERCISE 1.31
;
; We are asked to write a (product ...) precedure analogous to (sum ...), both recursive and iterative.
; I'll start with the recursive version:
(define (product factor a next b)
  (if (> a b)
      1
      (* (factor a)
         (product factor (next a) next b))))

; Before I write the iterative version, let's test out (product ...) by using it to create a
; (factorial ...) procedure:
(define (factorial n)
  (define (inc n)
    (+ n 1))
  (define (identity n)
    n)
  (product identity 1 inc n))

; Finally, here is the iterative version of (product ...):
(define (product factor a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (factor a)))))
  (iter a 1))

; Of course, we can use this version to re-do (factorial ...), but instead let's use it to construct
; approximations of pi via Wallis' formula:
;   pi / 4 = (2 * 4 * 4 * 6 * 6 * 8 * ...) / (3 * 3 * 5 * 5 * 7 * 7 * ...)
;
; We need to discover a pattern on the nth factor of this product. One way to do this would be to note
; that it can be rewritten as:
;    = (2 / 3) * (4 / 3) * (4 / 5) * (6 / 5) * (6 / 7) * (8 / 7) * ...
; From this we can see that either the numerator or denominator (but not both) increase by two from
; one factor to the next.
;
; However, this is a bit complicated, so let's looks for a pattern that is a simple function of the
; index, n, corresponding to th nth factor in the product. Let's start by multiplying both sides by 2:
;   pi / 2 = (2 * 2 * 4 * 4 * 6 * 6 * 8 * ...) / (1 * 3 * 3 * 5 * 5 * 7 * 7 * ...)
; Note that we inserted a factor of 1 at the begining of the denominator product. Now we note that if
; we group two consecutive factors in the numerator, the pattern is:
;   (2 * n)^2, for n = 1, 2, 3, ...
; If we group two consecutive factors in the denominator, we get the product of consecutive odds:
;   (2 * n - 1) * (2 * n + 1), for n = 1, 2, 3, ...
; Putting this all together and simplifying a bit, the nth factor is:
;   f(n) = 4 * n^2 / (4 * n^2 - 1), for n = 1, 2, 3, ...
;
; OK, let's implement Wallis' approximation as a function of the number of factors determine by the
; function f(n):
(define (wallis-pi-approx n)
  (define (inc n)
    (+ n 1))
  (define (wallis-factor n)
    (/ (* 4 n n)
       (- (* 4 n n) 1)))
  (* 2.0 (product wallis-factor 1 inc n)))

; While the approximation does converge to pi, it seems rather slow:
(wallis-pi-approx 1000)
; 3.1408077460303945
