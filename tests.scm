(load "compiler.scm")
(load "folder.scm")
(import
	(rnrs)
	(rough-draft unit-test)
	(rough-draft console-test-runner))

(define-test-suite parse-tests

	(define-test test-<lines>-1
		(assert-equal? (parse '(lambda (x y z) (if x y z))) '(lambda-simple (x y z) (if3 (var x) (var y) (var z))))
	)
	;TODO add test for parse let*
)

(define-test-suite compiler-tests

	(define-test test-improper-list?-1
		(assert-false (improper-list? '(1 2 3)))
		(assert-false (improper-list? 'a))
		(assert-true (improper-list? '(1 2 3 . 4)))
		(assert-true (improper-list? '(a b c . d)))
	)

	(define-test test-opt-lambda-args-list-1
		(assert-equal? (car (opt-lambda-args-list `(1 2 3 . 4 ) (lambda (x) x) )) `(1 2 3) )
		(assert-equal? (cdr (opt-lambda-args-list `(1 2 3 . 4 ) (lambda (x) x) )) 4 )
	)

	(define-test test-expand-letstar
		(assert-equal? (expand-letstar '((a 5) (b (+ a 5))) '(+ a b) `() )  `((lambda (a) ((lambda (b) (+ a b)) (+ a 5))) 5))
		(assert-equal? (expand-letstar '((a 5) (b (+ a 5))) '(+ a b) `((+ a b)) )  `((lambda (a) ((lambda (b) (begin (+ a b) (+ a b))) (+ a 5))) 5))
	)

	(define-test test-cond
		(assert-equal? (parse `(cond (a b c) (d e f) (else g h)))
			`(if3 (var a)
				 (seq ((var b) (var c)))
				 (if3 (var d)
							(seq ((var e) (var f)))
							(seq ((var g) (var h))))))
	)

	(define-test test-letrec
		(assert-equal? (format "~a" (parse '(letrec ((f1 (lambda (x) (+ 1 x))) (f2 (lambda (y) (* 1 y)))) 1)))
						"(applic (var Ym) ((lambda-simple (g0 f1 f2) (const 1)) (lambda-simple (g0 f1 f2) (lambda-simple (x) (applic (var +) ((const 1) (var x))))) (lambda-simple (g0 f1 f2) (lambda-simple (y) (applic (var *) ((const 1) (var y)))))))")
	)

	(define-test test-beginify
		(assert-equal? (beginify 'a) 'a)
		(assert-equal? (beginify 'a `b) `(begin a b))
		(assert-equal? (beginify 'a `b) `(begin a b))
		(assert-equal? (beginify `(+ 1 2) ) `(+ 1 2))
		(assert-equal? (beginify `(+ 1 2) `(+ 3 4) ) `(begin (+ 1 2) (+ 3 4)))
	)
)

(define-test-suite folder-tests
	(define-test test-flatten
		(assert-equal? (flatten `+ `(+ 1 2)) `(+ 1 2))
		(assert-equal? (flatten `* `(+ 1 2)) `(+ 1 2))
		(assert-equal? (flatten `+ `(+ 1 2 (+ 1 2) )) `(+ 1 2 1 2))
		(assert-equal? (flatten `+ `(+ 1 2 (+ 3 4 (+ 5 6)) )) `(+ 1 2 3 4 (+ 5 6))) ; flatten only *one* level
	)

	(define-test test-fold-plus
		(assert-equal? (fold `(+ 1 2 (+ (+ 1 2) a b))) `(+ 6 a b) )
		(assert-equal? (fold `(+ 1 2 (+ a (+ 1 2) b))) `(+ 6 a b) )
		(assert-equal? (fold `(+ 1 2 (+ (* 10 2) a a b))) `(+ 23 a a b) )
	)

	(define-test test-fold-add1
		(assert-equal? (fold `(add1 (+ 2 3))) 6)
		(assert-equal? (fold `(add1 #t)) '(+ 1 #t))
		(assert-equal? (fold `(add1 (+ 3 4 (* 2 3) ))) 14)
	)

	(define-test test-fold-sub1
		(assert-equal? (fold `(sub1 (+ 2 3))) 4)
		(assert-equal? (fold `(sub1 #t)) '(+ -1 #t))
		(assert-equal? (fold `(sub1 (+ 3 4 (* 2 3) ))) 12)
	)

	(define-test test-fold-number?
		(assert-equal?  (fold `(number? 1)) #t )
		(assert-equal?  (fold `(number? '1)) #f)
		(assert-equal?  (fold `(number? (car '(1 a)))) #t )
		(assert-equal?  (fold `(number? (car (cdr '(1 a))))) #f )
	)

	(define-test test-fold-null?
		(assert-equal? (fold `(null? '())) #t )
		(assert-equal? (fold `(null? (car (list '())))) #t )
		(assert-equal? (fold `(null? '(1))) #f )
		(assert-equal? (fold `(null? (car (list '(1) '(2) '(3) )))) #f )
		(assert-equal? (fold `(null? (append '() '() ) )) #t )
	)

	(define-test test-fold-zero?
		(assert-equal? (fold `(zero? 0)) #t )
		(assert-equal? (fold `(zero? (sub1 1))) #t )
		(assert-equal? (fold `(zero? 'a)) `(zero? 'a) )
		(assert-equal? (fold `(zero? "a")) `(zero? "a") )
		(assert-equal? (fold `(zero? (car '(1 a)) )) #f )
		(assert-equal? (fold `(zero? (car '(0 a)) )) #t )
	)

	(define-test test-fold-string?
		(assert-equal? (fold `(string? "abc")) #t )
		(assert-equal? (fold `(string? (car '("abc")))) #t )
		(assert-equal? (fold `(string? 1)) #f )
		(assert-equal? (fold `(string? 'a)) #f )
		(assert-equal? (fold `(string? x)) '(string? x) )
	)

	(define-test test-fold-string-append
		(assert-equal? (fold `(string-append "a" "b"))  "ab")
		(assert-equal? (fold `(string-append "a" a))  '(string-append "a" a))
		(assert-equal? (fold `(string-append))  "")
		(assert-equal? (fold `(string-append "a" (string-append "b" "c")))  "abc")
	)

	(define-test test-fold-if
		;;;;;; Tests for (if pred dit dif) ;;;;;;;
		(assert-equal? (fold `(if a b c)) '(if a b c) )
		(assert-equal? (fold `(if #t b c)) 'b )
		(assert-equal? (fold `(if #f b c)) 'c )
		(assert-equal? (fold `(if (add1 1) (+ 3 4) c)) 7 )
		(assert-equal? (fold `(if (car '(#t #f)) 1 2)) 1)
		(assert-equal? (fold `(if (car (cdr '(#t #f))) 1 2)) 2)
		;;;;;; Tests for (if pred dit) ;;;;;;;
		(assert-equal? (fold `(if a b)) '(if a b) )
		(assert-equal? (fold `(if #t b )) 'b )
		(assert-equal? (fold `(if #f b )) *void-object* )
		(assert-equal? (fold `(if (add1 1) (+ 3 4) )) 7 )
		(assert-equal? (fold `(if (car '(#t #f)) 1 )) 1)
		(assert-equal? (fold `(if (car ( cdr'(#t #f))) 1 )) *void-object*)
	)

	(define-test test-fold-list
		(assert-equal? (fold '(list)) ''() )
		(assert-equal? (fold '(list 1 2 3)) ''(1 2 3) )
		(assert-equal? (fold '(list 1 x 3)) '(list 1 x 3) )
	)

	(define-test test-cars-and-cdrs
		(assert-equal? (fold '(car (cdr '(1 2 3)))) 2)
		(assert-equal? (fold '(car (cdr (list 1 2 3)))) 2)
		(assert-equal? (fold '(car (cdr (cons 'a (cons 'b 'c))))) ''b)
	)
)

(exit (+ (run-test-suites
							parse-tests
							compiler-tests
							folder-tests
						)))
