(load "compiler.scm")
(import
	(rnrs)
	(rough-draft unit-test)
	(rough-draft console-test-runner))

(define-test-suite foo

	(define-test test-<lines>-1
		(assert-equal? (parse '(lambda (x y z) (if x y z))) '(lambda-simple (x y z) (if3 (var x) (var y) (var z))))
	)

)

(define-test-suite opt-lambda-tests

	(define-test improper-list?-test-1
		(assert-false (improper-list? '(1 2 3)))
		(assert-false (improper-list? 'a))
		(assert-true (improper-list? '(1 2 3 . 4)))
		(assert-true (improper-list? '(a b c . d)))
	)

	(define-test opt-lambda-args-list-test-1
		(assert-equal? (car (opt-lambda-args-list `(1 2 3 . 4 ) (lambda (x) x) )) `(1 2 3) )
		(assert-equal? (cdr (opt-lambda-args-list `(1 2 3 . 4 ) (lambda (x) x) )) 4 )
	)

)

;(run-test-suites foo)
;(run-test foo first-test)
;(run-tests foo test-one)


(exit (+ (run-test-suites foo opt-lambda-tests)))
