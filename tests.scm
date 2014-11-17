(load "compiler.scm")
(import
	(rnrs)
	(rough-draft unit-test)
	(rough-draft console-test-runner))

(define-test-suite foo

	(define-test test-<lines>-1
		(assert-equal? 1 1)
	)

)

;(run-test-suites foo)
;(run-test foo first-test)
;(run-tests foo test-one)


(exit (run-test-suites foo))
