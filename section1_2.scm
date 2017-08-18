; EXERCISE 1.9
;
; First, if want to actually implement this code, we need to do the following:
(define (inc x) (+ x 1))
;
; and
;
(define (dec x) (- x 1))

; The first procedure for adding positive integers is recursive: it calls itself until the first
; argument is zero, at which point b will be incremented a times, one for each deferred evaluation
; of (r+ ...).
(define (r+ a b)
  (if (= a 0)
      b
      (inc (r+ (dec a) b))))

; The second procedure is iterative: at each step, (i+ ...) gets a decreased by one and b increased
; by one. This continues until a=0, at which point b has the value b+a, which is then returned.
(define (i+ a b)
  (if (= a 0)
      b
      (i+ (dec a) (inc b))))


; EXERCISE 1.10
;
; The following procedure computes Ackermann's function, A(x,y):
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))
; For example:
(A 1 10)
; 1024

(A 2 4)
; 65536

(A 3 3)
; 65536

; The following procedure defines the function f(n) = 2*n, for n > 0:
(define (f n) (A 0 n))

; The following procedure defines the function g(n) = 2^n, for n > 0:
(define (g n) (A 1 n))

; The following procedure defines the function h(n) = 2^2^2^...^2, for n > 0, where there are n
; twos in the expression. Alternatively, the function can be defined recursively as
; h(n) = 2^h(n-1), for n > 1, and h(1) = 2.
(define (h n) (A 2 n))


; EXERCISE 1.11
;
; f(n) = ( n,                             if n < 3;
;        ( f(n-1) + 2*f(n-2) + 3*f(n-3),  if n >= 3.
;
; Recursive *process* version:
(define (fR n)
  (if (< n 3)
      n
      (+ (fR (- n 1))
         (* 2 (fR (- n 2)))
         (* 3 (fR (- n 3))))))
; Iterative *process* version:
(define (fI n)
  (fI-iter 2 1 0 n))

(define (fI-iter a b c count)
  (if (= count 0)
      c
      (fI-iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))


; EXERCISE 1.12
;
; Compute the jth element of the ith row of Pascal's triangle using a recursive process:
(define (PT i j)
  (cond
   ((= j 1) 1)
   ((= i j) 1)
   (else (+ (PT (- i 1) (- j 1))
            (PT (- i 1) j)))))


; EXERCISE 1.13
;
; Prove that the nth Fibonacci number is the closest integer to phi^n / sqrt(5), where phi is defined
; to be (1 + sqrt(5)) / 2.
;
; Proof: It is not hard to solve the recurrence relation of the Fibonacci sequence to see that the nth
; term is: (phi^n - psi^n) / sqrt(5), where psi is defined to be (1 - sqrt(5)) / 2 = -0.61803.... We
; therefore have |f_n - phi^n / sqrt(5)| = |psi^n / sqrt(5)| <= |psi / sqrt(5)| = 0.2763..., for
; n >= 1. It follows that f_n is the closest integer to phi^n / sqrt(5), for n >= 1. The case of n = 0
; is trivial. QED.
;
; By the way, this shows that the number of steps needed to comput f_n is Theta(phi^n), so exponential
; growth, since at the bottom of the tree are a bunch of zeros and ones (each representing a "step")
; that add up to f_n, which we have just shown is the integer closest to phi^n / sqrt(5).


; EXERCISE 1.14
;
; Here is the (count-change ...) program, for reference.
(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond
   ((= amount 0) 1)
   ((or (< amount 0) (= kinds-of-coins 0)) 0)
   (else (+ (cc amount (- kinds-of-coins 1))
            (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond
   ((= kinds-of-coins 1) 1)
   ((= kinds-of-coins 2) 5)
   ((= kinds-of-coins 3) 10)
   ((= kinds-of-coins 4) 25)
   ((= kinds-of-coins 5) 50)))
;
; Here is the tree for (count-change 11):
;
; 11 -- (10) 1 -- (10 1).
;   \-- (5) 6 -- (5 5) 1 -- (5 5 1).
;    \       \-- (5 1) 5 -- (5 1 1) 4 -- ... -- (5 1 1 1 1 1 1).
;     \-- (1) 10 -- (1 1) 9 -- ... -- (1 1 1 1 1 1 1 1 1 1 1).
;
; The space complexity should be proportional to the max depth (longest branch) of the tree since once
; we get to a leaf we can increment the counter, so to speak, and release the memory used. (Not sure
; about this logic, but...) Since the longest branch will be (1 1 ... 1), which has length n, we can
; conclude that the space complexity is O(n).
;
; For the time complexity, let's first suppose we only use pennies. Clearly the time complexity would
; be O(n). Now if we add in nickels, at each step we decide whether or not we will use a nickel for
; what remains. Continuing to use nickels produces a branch that is length n/5, approx; but at each of
; these decision points, we could stop using nickels and use only pennies. Each of these would produce
; a branch that is no greater than length n. So, by the multiplication rule, the result is O(n/5 * n)
; = O(n^2). Continuing with more denominations available, we should arrive at O(n^5), if we have 5
; denominations available.


; EXERCISE 1.15
;
(define (cube x)
  (* x x x))

(define (p x)
  (- (* 3 x)
     (* 4 (cube x))))

(define (sine theta)
  (if (not (> (abs theta) 0.1))
      theta
      (p (sine (/ theta 3.0)))))
; Part a: If we were to compute (sine 12.15), then we would need to call (p ...) 5 times. This is
; because we  cannot use the approx sin(x) ~ x until x <= 0.1; so in our case, we must find the
; smallest integer n such that 12.15 / 3^n <= 0.1. Solving for n, we get ln(121.5)/ln(3) <= n, the
; left-hand side being 4.369..., so we must take n = 5.
;
; Part b: More generally, we would need theta / 3^n <= 0.1, hence ln(10*theta) / ln(3) <= n.
;
; Note that at each point in the recursion, we either stop and output theta, or we call (p ...). This
; means that both space complexity (max length of branch) and time complexity (number of steps) will
; be O(log(n)), from the general calculation above.


; EXERCISE 1.16
;
; Here is the fast-exponentiation function, recursive process version [note: (even? ...) is primitive
; in Gambit-Scheme]:
(define (fast-expt b n)
  (cond
   ((= n 0) 1)
   ((even? n) (square (fast-expt b (/ n 2))))
   (else (* b (fast-expt b (- n 1))))))

(define (square x)
  (* x x))
; Now we write a fast-exponentiation function, iterative process version. The odd case is
; straightforward: multiply a, the result so far, by b and decrement the counter [power]. The even case
; is a bit trickier. We replace the base by its square (evaluated!), divide the counter by 2, and leave
; a alone.
; For example: 2^4 = (2^2)^2 = (4)^2 = (4^2)^1 = (16)^1 = 16*16^0 = 16*1 = 16.
(define (fast-expt b n)
  (fast-expt-iter b n 1))

(define (fast-expt-iter b counter a)
  (cond
   ((= counter 0) a)
   ((even? counter) (fast-expt-iter (square b) (/ counter 2) a))
   (else (fast-expt-iter b (- counter 1) (* b a)))))


; EXERCISE 1.17
;
; The following function defines multiplication by repeated addition:
(define (my* a b)
  (if (= b 0)
      0
      (+ a (my* a (- b 1)))))
; To write something analogous to (fast-expt ...), we need a couple of helper functions:
(define (double x)
  (+ x x))

(define (halve x y)
  (if (= x (double y))
      y
      (halve x (+ y 1))))

; then:
(define (fast-mult a b)
  (cond
   ((= b 0) 0)
   ((even? b) (fast-mult (double a) (halve b 1)))
   (else (+ a (fast-mult a (- b 1))))))
;
; It is clear that if we double b, we only need one more step. Therefore, this algorithm is O(log(b)).


; EXERCISE 1.18
;
; Now we build an iterative process version of (fast-mult ...)
(define (fast-mult a b)
  (fast-mult-iter a b 0))

(define (fast-mult-iter a b c)
  (cond
   ((= b 0) c)
   ((even? b) (fast-mult-iter (double a) (halve b 1) c))
   (else (fast-mult-iter a (- b 1) (+ a c)))))
; as above, this is O(log(b)), since doubling b only increases the number of steps by one.


; EXERCISE 1.19
;
; Let T(a, b) = (a+b, a). Then it is not hard to see that T^n(0, 1) = (f_n+1, f_n), where f_n is the
; nth Fibonacci number.
;
; Now, let T_{p, q}(a, b) = (bq + aq + ap, bp + aq). Note that T = T_{0, 1}. Again, without too much
; effort, it is easy to show that T^2_{p, q}(a, b) = T_{p', q'}(a, b), where p' = p^2 + q^2 and
; q' = 2*p*q + q^2. It follows, for example, that T^2 = T_{1, 1}.
;
; We can now use T_{p', q'} to perform the analoguous operation to (square ...) in (fast-expt ...) to
; produce an algorithm that produces the nth Fibonacci number in O(log(n)) steps, starting with
; p = 0, q = 1, and the first two Fibonacci numbers 1, 0 [read right-to-left]:
(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond
   ((= count 0) b)
   ((even? count) (fib-iter a
                            b
                            (+ (square p) (square q)) ; 'p
                            (+ (* 2 p q) (square q)) ; q'
                            (/ count 2)))
   (else (fib-iter (+ (* b q) (* a q) (* a p))
                   (+ (* b p) (* a q))
                   p
                   q
                   (- count 1)))))


; EXERCISE 1.20
;
; Here is Euclid's algorithm to compute GCD(a,b):
(define
  (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
; If we use applicaitve-order evaluation, we evaluate arguments and then apply functions. So, for
; example:
; (gcd 206 40) = (gcd 40 (remainder 206 40)) = (gcd 40 6) = (gcd 6 (remainder 40 6)) = (gcd 6 4)
; = (gcd 4 (remainder 6 4)) = (gcd 4 2) = (gcd 2 (remainder 4 2)) = (gcd 2 0) = 2.
; As you can see, (remainder ...) is called four times.
;
; On the other hand, if we use normal-order evaluation, we get a big mess:
; (gcd 206 40), need to compute (if (= 40 0) 206 (gcd 40 (remainder 206 40))), yields:
; (gcd 40 (remainder 206 40)), need to compute:
; (if (= (remainder 206 40) 0) 40 (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))
; we now need the first remainder calculation, which yields 6, which is not zero, so:
; (gcd (remainder 206 40) (remainder 40 (remainder 206 40))), which requires:
; (if (= (remainder 40 (remainder 206 40)) 0)
;     (remainder 206 40)
;     (gcd (remainder 40 (remainder 206 40))
;          (remainder (remainder 206 40)
;                     (remainder 40 (remainder 206 40)))))
; This is getting out of hand. Working through this on a sheet of paper, I believe we need 18 evals
; of (remainder ...).


; EXERCISE 1.21
;
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond
   ((> (square test-divisor) n) n)
   ((divides? test-divisor n) test-divisor)
   (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))
; We can use these to find the smallest divisor of each of: 199, 1999, 19999:
(smallest-divisor 199)
; returns 199, since 199 is prime.
(smallest-divisor 1999)
; returns 1999, since 1999 is prime
(smallest-divisor 19999)
; returns 7, since 19999 = 7 * 2857, where 2957 is prime.


; EXERCISE 1.22
;
; Gambit-Scheme doesn't have (runtime), so I replaced with with (time->seconds (current-time)). I also
; added two (newline)s to move the prompt to a new line, no matter the result of the primality test:
(define (timed-prime-test n)
  (newline)
  (display n)
  (newline)
  (start-prime-test n (time->seconds (current-time))))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (time->seconds (current-time)) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  (newline))

(define (prime? n)
  (= n (smallest-divisor n)))
; Note: Might be easier to just use (time ...).
(define (search-for-primes-body index end n l)
  (cond
   ((or (> index end)
        (= (length l) n)) l)
   ((prime? index) (search-for-primes-body (+ index 2) end n (cons index l)))
   (else (search-for-primes-body (+ index 2) end n l))))

(define (search-for-primes index end n l)
  (if (even? index)
      (search-for-primes-body (+ index 1) end n l)
      (search-for-primes-body index end n l)))

; I needed to work with much larger numbers to see any differences in the output of (time ...):
(time (search-for-primes (expt 10 8) (expt 10 9) 3 '()))
;(time (search-for-primes (expt 10 8) (expt 10 9) 3 '()))
;    29 ms real time
;    29 ms cpu time (27 user, 2 system)
;    3 collections accounting for 1 ms real time (1 user, 0 system)
;    7558400 bytes allocated
;    245 minor faults
;    no major faults
;(100000039 100000037 100000007)

(time (search-for-primes (expt 10 9) (expt 10 10) 3 '()))
;(time (search-for-primes (expt 10 9) (expt 10 10) 3 '()))
;    71 ms real time
;    71 ms cpu time (70 user, 1 system)
;    12 collections accounting for 3 ms real time (3 user, 0 system)
;    23006480 bytes allocated
;    no minor faults
;    no major faults
;(1000000021 1000000009 1000000007)

(time (search-for-primes (expt 10 10) (expt 10 11) 3 '()))
;(time (search-for-primes (expt 10 10) (expt 10 11) 3 '()))
;    247 ms real time
;    246 ms cpu time (244 user, 2 system)
;    38 collections accounting for 9 ms real time (9 user, 0 system)
;    79017808 bytes allocated
;    no minor faults
;    no major faults
;(10000000061 10000000033 10000000019)

(time (search-for-primes (expt 10 11) (expt 10 12) 3 '()))
;(time (search-for-primes (expt 10 11) (expt 10 12) 3 '()))
;    676 ms real time
;    675 ms cpu time (672 user, 3 system)
;    111 collections accounting for 22 ms real time (22 user, 0 system)
;    225868432 bytes allocated
;    no minor faults
;    no major faults
;(100000000057 100000000019 100000000003)

; As we can see, increasing the starting index by a factor of 10 increases the run-time roughly by a
; factor of 3, which is itself approximately sqrt(10). This is a reflection of the time complexity of
; the naive primality test we are using, which is O(sqrt(n)). It also suggests that programs that run
; on my machine do so in time proportional to the number of steps required for computation.


; EXERCISE 1.23
;
; In order to skip over the evens in our primality test, we define:
(define (next x)
  (if (= x 2)
      3
      (+ x 2)))
; and then replace (+ test-divisor 1) with (next test-divisor) in (find-divisor ...):
(define (find-divisor n test-divisor)
  (cond
   ((> (square test-divisor) n) n)
   ((divides? test-divisor n) test-divisor)
   (else (find-divisor n (next test-divisor)))))
;
; Let's re-execute the tests in the previous exercise to see if the times are roughly cut in half:
(time (search-for-primes (expt 10 8) (expt 10 9) 3 '()))
;(time (search-for-primes (expt 10 8) (expt 10 9) 3 '()))
;    18 ms real time
;    18 ms cpu time (18 user, 0 system)
;    2 collections accounting for 1 ms real time (1 user, 0 system)
;    4866720 bytes allocated
;    no minor faults
;    no major faults
;(100000039 100000037 100000007)

(time (search-for-primes (expt 10 9) (expt 10 10) 3 '()))
;(time (search-for-primes (expt 10 9) (expt 10 10) 3 '()))
;    43 ms real time
;    42 ms cpu time (42 user, 1 system)
;    8 collections accounting for 2 ms real time (2 user, 0 system)
;    14794224 bytes allocated
;    no minor faults
;    no major faults
;(1000000021 1000000009 1000000007)

(time (search-for-primes (expt 10 10) (expt 10 11) 3 '()))
;(time (search-for-primes (expt 10 10) (expt 10 11) 3 '()))
;    151 ms real time
;    150 ms cpu time (149 user, 1 system)
;    25 collections accounting for 6 ms real time (5 user, 0 system)
;    50809072 bytes allocated
;    no minor faults
;    no major faults
;(10000000061 10000000033 10000000019)

(time (search-for-primes (expt 10 11) (expt 10 12) 3 '()))
;(time (search-for-primes (expt 10 11) (expt 10 12) 3 '()))
;    433 ms real time
;    431 ms cpu time (429 user, 3 system)
;    71 collections accounting for 16 ms real time (16 user, 0 system)
;    145212720 bytes allocated
;    no minor faults
;    no major faults
;(100000000057 100000000019 100000000003)

; We can see that the original times are roughly double those of the new times . Looking at ratios,
; it's actually closer to 1.6. The only explanation for why we are not getting 2 lies in (next ...).
; First, it is user-defined and will therefore likely be slower than the primitive (+ ...). Secondly,
; the (if ...) we used in (next ...) is evaluated every time it is called, but this extra work is
; uneccesary -- if/when 2 is used as an input, then next value is 3, and from that point on adding 2
; can never produce 2 [or any other even], so the test really is needed only once. It would be better
; to just use (+ test-divisor 2) and to check 2 vs odd a single time at the beginning of the search.


; EXERCISE 1.24
;
; First, let's implement the probabilistic primality test based on Fermat's Little Theorem. Note:
; Gambit Scheme does not have (random ...), so I've downloaded "random.scm" which must first be loaded
; with (load "random.scm"):
(define (expmod b e m)
  (cond
   ((= e 0) 1)
   ((even? e) (remainder (square (expmod b (/ e 2) m))
                         m))
   (else (remainder (* b (expmod b (- e 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond
   ((= times 0) #t)
   ((fermat-test n) (fast-prime? n (- times 1)))
   (else #f)))

; Now let's modify (timed-prime-test ...) to use (fast-prime? ...):
(define (timed-prime-test n)
  (newline)
  (display n)
  (newline)
  (start-prime-test n (time->seconds (current-time))))

(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
      (report-prime (- (time->seconds (current-time)) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  (newline))

; Again, I'm just going to use (time ...).
(define (search-for-primes-body index end n l)
  (cond
   ((or (> index end)
        (= (length l) n)) l)
   ((fast-prime? index 10) (search-for-primes-body (+ index 2) end n (cons index l)))
   (else (search-for-primes-body (+ index 2) end n l))))

(define (search-for-primes index end n l)
  (if (even? index)
      (search-for-primes-body (+ index 1) end n l)
      (search-for-primes-body index end n l)))
; Here is the data we get from testing the same ranges as in Exercise 1.22:
(time (search-for-primes (expt 10 8) (expt 10 9) 3 '()))
;(time (search-for-primes (expt 10 8) (expt 10 9) 3 '()))
;    2 ms real time
;    2 ms cpu time (2 user, 0 system)
;    no collections
;    484224 bytes allocated
;    62 minor faults
;    6 major faults
;(100000039 100000037 100000007)

(time (search-for-primes (expt 10 9) (expt 10 10) 3 '()))
;(time (search-for-primes (expt 10 9) (expt 10 10) 3 '()))
;    3 ms real time
;    3 ms cpu time (2 user, 0 system)
;    no collections
;    420720 bytes allocated
;    52 minor faults
;    no major faults
;(1000000021 1000000009 1000000007)

(time (search-for-primes (expt 10 10) (expt 10 11) 3 '()))
;(time (search-for-primes (expt 10 10) (expt 10 11) 3 '()))
;    6 ms real time
;    6 ms cpu time (5 user, 0 system)
;    no collections
;    1411040 bytes allocated
;    175 minor faults
;    2 major faults
;(10000000061 10000000033 10000000019)

(time (search-for-primes (expt 10 11) (expt 10 12) 3 '()))
;(time (search-for-primes (expt 10 11) (expt 10 12) 3 '()))
;    7 ms real time
;    7 ms cpu time (6 user, 0 system)
;    1 collection accounting for 1 ms real time (1 user, 0 system)
;    1660800 bytes allocated
;    160 minor faults
;    no major faults
;(100000000057 100000000019 100000000003)

; We can see that increasing the start index by a factor of 10 increases the run time by approximately
; a constant amount (~1 ms), which is the expected behavior of an O(log(n)) algorithm.


; EXERCISE 1.25
;
; Consider the following version of (expmod ...):
(define (expmod b e m)
  (remainder (fast-expt b e) m))

; The problem with this version is that it first computes b^e, which may be huge. We then mod a huge
; number by m, which itself will take a bit of time. In contrast, the original version exponentiates
; with much smaller parameters, then mods m, etc., producing much smaller numebrs to work with along
; the way to the result.


; EXERCISE 1.26
;
; Consider the following version of (expmod ...):
(define (expmod b e m)
  (cond
   ((= e 0) 1)
   ((even? e) (remainder (* (expmod b (/ e 2) m)
                            (expmod b (/ e 2) m))
                         m))
   (else (remainder (* b (expmod b (- e 1) m))
                    m))))

; The only difference between this version and the original is that this version computes (square ...)
; explicitly as (* (expmod ...) (expmod ...)). However, this leads to an exponential number of calls
; to (expmod ...).
;
; For example, (expmod 2 4 2) could call (expmod 2 2 2) twice, each of which would call (expmod 2 1 2)
; twice, each of which would call (expmod 2 0 2) twice. So one call of (exp 2 4 2) leads to two calls
; of (expmod 2 2 2), which leads to four calls of (expmod 2 1 2), which leads to eight calls of
; (expmod 2 0 2). This is an exponential growth in the number of calls to (expmod ...), compared to the
; linear growth in the original version.
;
; Therefore an O(log(n)) algorithm is turned into a O(log(2^n)) = O(n) algorithm.


; EXERCISE 1.27
;
; We demonstrate that the Carmichael numbers 561, 1105, 1729, 2465, 2821, and 6601 are "pseudo-primes"
; in that they fool the Fermat test, but are not prime:
(define (carmichael-test n)
  (carmichael-test-body 1 n))

(define (carmichael-test-body a n)
  (cond
   ((= a n) #t)
   ((not (= a (expmod a n n))) #f)
   (else (carmichael-test-body (+ a 1) n))))

(carmichael-test 561)
; #t

(carmichael-test 1105)
; #t

(carmichael-test 1729)
; #t

(carmichael-test 2465)
; #t

(carmichael-test 2821)
; #t

(carmichael-test 6601)
; #t


; EXERCISE 1.28
;
; We begin with the Miller-Rabin test, an alternate form of Fermat's Little Theorem: if p is prime and
; 0 < a < p, then a^(p-1) = 1 mod p. Basically, since a != 0 mod p, we can cancel an a from Fermat's
; Little Theorem a^p = a mod p.
;
; As with Fermat's Little Theorem, we can pick a random a with 0 < a < n and compute a^(n-1) mod n. If
; we do not get 1, then n cannot be prime.
;
; However, if, as we are computing a^(n-1), we ever square a number that is not 1 or n-1 and we get
; 1 mod n as a result of the squaring, then we have found a non-trivial square-root of 1 mod n. It can
; be shown that primes do not have non-trivial square-roots of 1, so the existence of such a number
; proves that n cannot be prime.
;
; It can also be shown that if n is odd and not prime, then computing a^(n-1) will reveal a non-trivial
; square-root of 1 mod n for at least half of 0 < a < n, and therefore the Miller-Rabin test cannot be
; fooled.
;
; Let's use these ideas to implement the Miller-Rabin test, analagous to (fermat-test ...). We'll
; modify (square ...) to check for a non-trivial square-root of 1 mod n and have it return zero if it
; finds on. Finally, we'll include this check with the a^(n-1) = 1 mod n check:
(define (expmod b e m)
  (cond
   ((= e 0) 1)
   ((even? e) (remainder (square-with-check (expmod b (/ e 2) m) m)
                         m))
   (else (remainder (* b (expmod b (- e 1) m))
                    m))))

(define (square-with-check x m)
  (if (and (< 1 x)
           (< x (- m 1))
           (= 1 (* x x)))
      0
      (* x x)))

(define (miller-rabin-test n)
  (miller-rabin-test-body 2 n))

(define (miller-rabin-test-body a n)
  (cond
   ((= a n) #t)
   ((= 0 (expmod a (- n 1) n)) #f)
   ((not (= 1 (expmod a (- n 1) n))) #f)
   (else (miller-rabin-test-body (+ a 1) n))))

; Here are the results when the Miller-Rabin test is used on the Carmichael numbers listed above:
(miller-rabin-test 561)
; #f

(miller-rabin-test 1105)
; #f

(miller-rabin-test 1729)
; #f

(miller-rabin-test 2465)
; #f

(miller-rabin-test 2821)
; #f

(miller-rabin-test 6601)
; #f
; This test returns #f [not prime] for each of these Carmichael numbers!
