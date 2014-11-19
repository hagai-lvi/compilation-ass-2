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

;(define (^var? x))				;TODO

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


(define parse
  (let ((run
	 (compose-patterns
	  (pattern-rule
	   `(,(? 'c ^const?))
	   (lambda (c) `(const ,c)))
	  (pattern-rule
	   `(quote ,(? 'c))
	   (lambda (c) `(const ,c)))
	
	  )))
    (lambda (e)
      (run e
	   (lambda ()
	     (error 'parse
		    (format "I can't recognize this: ~s" e)))))))