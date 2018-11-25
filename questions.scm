(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.
(define (map proc items)
  (if (not (null? items))
    (cons (proc (car items)) (map proc (cdr items)) )
    nil
  )
)


(define (cons-all first rests)
  (if (null? rests)
    (cons (cons first nil) nil)
    (map (lambda (lst) (cons first lst)) rests)
  )
)

(define (zip pairs)
  (cond
  ((null? pairs) '(nil nil))
  (else (cons (map car pairs) (cons (map cadr pairs) nil)))
  )
  )

;; Problem 17
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 17
  (define (enum-with-index s k)
    (if (null? s)
      nil
      (cons (cons k (cons (car s) nil)) (enum-with-index (cdr s) (+ k 1)))
    )
  )
  (enum-with-index s 0)
)
  ; END PROBLEM 17

;; Problem 18
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 18
  (cond
    ((null? denoms)
    nil
    )
    ((> (car denoms) total)
      (list-change total (cdr denoms))
    )
    ((= total 0)
      nil
    )
    ((null? (cdr denoms))
      (define (remaining_smallest_denom total denom)
        (if (= total 0)
          nil
          (cons denom (remaining_smallest_denom (- total 1) denom))
        )
      )
      (cons (remaining_smallest_denom total (car denoms)) nil)
    )
    (else
        (append (cons-all (car denoms) (list-change (- total (car denoms)) denoms)) (list-change total (cdr denoms)))
    )
  )
)

  ; END PROBLEM 18

;; Problem 19
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
      (define newbody (map let-to-lambda body))
        (cons form (cons params newbody))
           ; END PROBLEM 19
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           (define names (car (zip values)))
           (define values (cdr (zip values)))
           (cons (cons 'lambda (cons (let-to-lambda names) (let-to-lambda body))) (let-to-lambda (car values)))
           ; END PROBLEM 19
           ))
        (else
         ; BEGIN PROBLEM 19
         (map let-to-lambda expr)
         ; END PROBLEM 19
         )))
