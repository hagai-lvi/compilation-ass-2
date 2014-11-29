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
		(assert-equal? (fold `(add1 #t)) `(+ 1 #t))
		(assert-equal? (fold `(add1 (+ 3 4 (* 2 3) ))) 14)
	)

	(define-test test-fold-number?
		(assert-equal?  (fold `(number? 1)) #t )
		(assert-equal?  (fold `(number? '1)) #f)
		(assert-equal?  (fold `(number? (car `(1 a)))) #t )
		(assert-equal?  (fold `(number? (cadr `(1 a)))) #f )
	)
)
;(run-test-suites foo)
;(run-test compiler-tests test-letstar-1)
;(run-tests foo test-one)

(exit (+ (run-test-suites parse-tests compiler-tests folder-tests)))
