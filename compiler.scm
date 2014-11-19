(load "pattern-matcher.scm")

;;;;;;;;;;;
;; const ;;
;;;;;;;;;;;

(define (^const? x)
	(or 	(boolean? x)
			(char? x)
			(number? x)
			(string? x)
			))

(define (^var? x)
(and (symbol? x)(let ((p (member x *reserved-words*)))
	(if p #f #t))))

(define *reserved-words*
  '(and begin cond define do else if lambda
    let let* letrec or quasiquote unquote 
    unquote-splicing quote set!))			;TODO


;;;;;;;;;;;;;;;;;;
;; conditionals ;;
;;;;;;;;;;;;;;;;;;
; (define (^conditional? x)
; 	(or 	(^if2? x)
; 			(^if3? x)))

; ;(define (^if2? x))				;TODO
; ;(define (^if3? x))				;TODO

; ;;;;;;;;;;;;
; ;; lambda ;;
; ;;;;;;;;;;;;
; (define (^lambda? x)
; 	(or 	(^reg-lambda? x)
; 			(^lambda-opt? x)
; 			(^lambda-variadic? x)))
; ;
; ;(define (^reg-lambda? x))		;TODO
; ;(define (^lambda-opt? x))		;TODO
; ;(define (^lambda-variadic? x))	;TODO

; ;;;;;;;;;;;;
; ;; define ;;
; ;;;;;;;;;;;;
; (define (^define? x)
; 	(or 	(^define-regular? x)
; 			(^define-mit? x)))

; (define (^define-regular? x))	;TODO
; (define (^define-mit? x))		;TODO

; ;;;;;;;;;;;;;;;;;
; ;; application ;;
; ;;;;;;;;;;;;;;;;;
; (define (^application? x))		;TODO

; ;;;;;;;;;
; ;; seq ;;
; ;;;;;;;;;
; (define (^seq? x))				;TODO


(define (^reg-lambda-args-list? list) ; TODO exclude lists that contain the symbol . (dot)
	(if (not (list? list))
	    #f
	    (andmap ^var? list)))

(define parse
  (let ((run
	 (compose-patterns
	  (pattern-rule
	   `(,(? 'c ^const?))
	   (lambda (c) `(const ,c)))
	  (pattern-rule
	   `(quote ,(? 'c))
	   (lambda (c) `(const ,c)))
		(pattern-rule
	   `,(? 'v ^var?)
	   (lambda (v) `(var ,v)))
		  (pattern-rule
	   `(if ,(? 'test) ,(? 'dit))
	   (lambda (test dit)
	     `(if3 ,(parse test) ,(parse dit) (const ,*void-object*))))
	  (pattern-rule
	   `(if ,(? 'test) ,(? 'dit) ,(? 'dif))
	   (lambda (test dit dif)
	     `(if3 ,(parse test) ,(parse dit) ,(parse dif))))
	  (pattern-rule
	  `(lambda ,(? 'opt-arg-list ^opt-lambda-args-list?) ,(? 'body))
	  (lambda (opt-arg-list body)
	  		(let ( 	(mandatory-args (get-mandatory-args opt-arg-list))
	  				(optional-arg (get-optional-args opt-arg-list)))
	  			`(lambda-opt ,mandatory-args ,optional-arg ,(parse `(begin e1 ... em)))))	
	  )
	  (pattern-rule
	  `(lambda ,(? 'arg-list ^reg-lambda-args-list?) ,(? 'body))
	  (lambda (arg-list body) `(lambda-simple ,arg-list ,(parse body))))
	  )))
    (lambda (e)
      (run e
	   (lambda ()
	     (error 'parse
		    (format "I can't recognize this: ~s" e)))))))
