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

(define-test-suite general-tests

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

	(define-test test-letstar-1
		(assert-equal? (letstar '((a 5) (b (+ a 5))) '(+ a b) )  `((lambda (a) ((lambda (b) (+ a b)) (+ a 5))) 5))
	)
)

(define-test-suite folder-tests
	(define-test test-flatten
		(assert-equal? (flatten `+ `(+ 1 2)) `(+ 1 2))
		(assert-equal? (flatten `* `(+ 1 2)) `(+ 1 2))
		(assert-equal? (flatten `+ `(+ 1 2 (+ 1 2) )) `(+ 1 2 1 2))
		(assert-equal? (flatten `+ `(+ 1 2 (+ 3 4 (+ 5 6)) )) `(+ 1 2 3 4 (+ 5 6))) ; flatten only *one* level
	)


	(define-test test-cond
		(assert-equal? (parse `(cond (a b c) (d e f) (else g h))) 	`(if3 (var a)
																     (seq ((var b) (var c)))
																     (if3 (var d)
																          (seq ((var e) (var f)))
																          (seq ((var g) (var h))))))
	)

)
;(run-test-suites foo)
;(run-test general-tests test-letstar-1)
;(run-tests foo test-one)

(exit (+ (run-test-suites parse-tests general-tests folder-tests)))
