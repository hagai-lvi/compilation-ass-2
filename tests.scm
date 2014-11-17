(load "")
(import
	(rnrs)
	(rough-draft unit-test)
	(rough-draft console-test-runner))

(define-test-suite foo

	(define-test test-<lines>-1
		(assert-equal? (<lines> (string->list "---") (lambda (x y) (list->string x)) (lambda(x) 'fail)) "---")
		(assert-equal? (<lines> (string->list "---") (lambda (x y) (list->string y)) (lambda(x) 'fail)) "")
	)

	(define-test test-<lines-nat-lines>-1
		(assert-equal? (<lines-nat-lines> (string->list "---5-") (lambda (x y)  x) (lambda(x) 'fail)) 5)
		(assert-equal? (<lines-nat-lines> (string->list "---5-") (lambda (x y)  (list->string y)) (lambda(x) 'fail)) "")
	)

)

;(run-test-suites foo)
;(run-test foo first-test)
;(run-tests foo test-one)


(exit (run-test-suites foo))





















<string>