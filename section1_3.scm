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
