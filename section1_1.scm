; EXERCISE 1.1
;
; Just a buch of expressions to type in and evaluate...


; EXERCISE 1.2
;
(/
(+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
(* 3 (- 6 2) (- 2 7)))


; EXERCISE 1.3
;
(define (my-sum-of-squares a b c)
  (cond
    ((and (<= a b) (<= a c)) (+ (* b b) (* c c)))
    ((and (<= b a) (<= b c)) (+ (* a a) (* c c)))
    ((and (<= c a) (<= c b)) (+ (* a a) (* b b)))))


; EXERCISE 1.4
;
; The following function checks whether or not b is positive.
; If is it, the result a+b is returned, otherwise a-b is returned,
; which is equivalent to a+|b|.
;
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))


; EXERCISE 1.5
;
; If we define the following procedures
;
(define (p) (p))
;
; and
;
(define (test x y)
  (if (= x 0)
      0
      y))
;
; and we evaluate
;
(test 0 (p))
;
; then an interpreter using applicative-order evaluation will enter an infinite loop.
; This is because the interpreter will first try to evaluate 0, which causes no problem,
; then (p), which causes the inifinte loop.
;
; Normal-order evaluation shouldn't, since 0 and (p) will be passed to test,
; which will first check whether or not x=0, which is does, so it should return 0.
;
; A better explanation, in light of 1.6 [see below] is that (test ...) is a user-defined compound
; procedure. Therefore the arguments of (test ...) will first be evaluated before passed to the
; (if ...). However, evaluating (p) will cause an infinite loop.


; EXERCISE 1.6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
; This user-defined version of the (if ...) procedure, which uses (cond ...), will work as long
; as neither the then-clause nor the else-clause involves a call to (new-if ...). The reason is
; that this is a user-defined compound procedure, and therefore each of its arguments will be
; evaluated before passed to the (cond ...) body. If (new-if ...) is in either argument, there
; will be an infinite loop. This is why
(new-if (= 2 3) 0 5)
; works, but using it in the (sqrt-iter ...) function does not.


; EXERCISE 1.7
;
; Suppose that a and b are very large numbers, and let us suppose for the sake of illustration that
; our machine can only keep track of 5 significant digits. In this case, the machine cannot tell the
; difference between a-b with a=1234500001 and b=1234500000, which is a-b=1, and a-b with
; a=1234549999 and b=1234500000, which is a-b=49999. It follows that for very large numbers, the
; book's (sqrt-iter ...) procedure might never terminate.
;
; On the other hand, if a and b are very small positive numbers less than one, then their squares
; are even smaller numbers. It is therefore possible y is a relatively bad approximation of the
; square-root of x, while y^2 is a relatively good approximation of x. In other words, the book's
; (sqrt-iter ...) procedure might terminate with an approximation that isn't very good, even
; though the square of this approximation is a good approximation of the radicand.
;
; A better way is to define (good-enough? ...) so that it returns #t if the change from one step
; to the next is a very small fraction of the guess. For example, if a is the previous approximation
; and b is the current approximation, then stop if |(a-b)/b| is less than, say, 0.001.
; Here is an implementation of the square-root procedure that uses this idea:
(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? previous-guess current-guess)
  (< (abs (/ (- previous-guess current-guess) current-guess)) 0.001))

(define (sqrt-iter previous-guess current-guess x)
  (if (good-enough? previous-guess current-guess)
      current-guess
      (sqrt-iter current-guess
                 (improve current-guess x)
                 x)))

(define (sqrt x)
  (sqrt-iter 0.5 1.0 x))
;
; Here are the book's naive definitions, if you want to compare:
(define (books-good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (books-sqrt-iter guess x)
  (if (books-good-enough? guess x)
      guess
      (books-sqrt-iter (improve guess x)
                       x)))

(define (books-sqrt x)
  (books-sqrt-iter 1.0 x))


; EXERCISE 1.8
;
; The following is a cube-root approximation procedure based on Newton's method and the basic
; functionality of the square-root procedure defined above.
(define (improve guess x)
  (/ (+ (/ x
           (* guess guess))
        (* 2 guess))
     3))

(define (good-enough? previous-guess current-guess)
  (< (abs (/ (- previous-guess current-guess) current-guess)) 0.001))

(define (cubert-iter previous-guess current-guess x)
  (if (good-enough? previous-guess current-guess)
      current-guess
      (cubert-iter current-guess
                 (improve current-guess x)
                 x)))

(define (cubert x)
  (cubert-iter 0.5 1.0 x))
