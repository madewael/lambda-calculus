#lang racket
 
(provide read-λ-expr
         string->λ-expr
         λ-expr->string
         variable?
         abstraction abstraction?
         abstraction-parameter abstraction-expression
         application application?
         application-f application-parameter)

(define variable? symbol?)

(struct abstraction (parameter expression) #:transparent)
(struct application (f parameter) #:transparent)

(define (string->λ-expr str)
  (read-λ-expr (open-input-string str)))

(define (λ-expr->string expr)
  (match expr
    ((abstraction v b)(format "λ~a.~a" (λ-expr->string v)(λ-expr->string b)))
    ((application f p)(format "(~a ~a)" (λ-expr->string f)(λ-expr->string p)))
    ((and v (? variable?))(format "~a" v))
    (_ (error 'xxx))))
    
  

(define (read-λ-expr in)
  (let ((peeked (peek-char in)))
    (cond ((eof-object? peeked) peeked)
          ((equal? peeked     #\λ) (read-abstraction in))
          ((equal? peeked     #\() (read-application in))
          (else (read-variable in)))))
          
          
(define (read-abstraction in)
  (let* ((the-lambda-char (read-char in))
         (variable (read-variable in))
         (the-dot-char (read-dot-char in))
         (expression (read-λ-expr in)))
    (abstraction variable expression)))

(define (read-close-char in)
  (let ((the-close-char (read-char in)))
    (if (equal? the-close-char #\))
        the-close-char
        (error 'read-close-char "Expected to read ')', but found '~a'" the-close-char))))

(define (read-dot-char in)
  (let ((the-dot-char (read-char in)))
    (if (equal? the-dot-char #\.)
        the-dot-char
        (error 'read-dot-char "Expected to read '.', but found '~a'" the-dot-char))))
    
                 
(define (read-application in)
   (let* ((the-open-char (read-char in))
          (f (read-λ-expr in))
          (parameter (read-λ-expr in))
          (the-close-char  (read-close-char in)))
     (application f parameter)))
  
  
(define (read-variable in)
  (define (return chars)
    (if (null? chars)
        (error 'read-variable "Expected to read a variable, found nothing ('~a')" (read-line in))
        (string->symbol (apply string (reverse chars)))))
  
  (let loop ((peeked (peek-char in))(chars '()))
    (cond
      ((eof-object? peeked) (read-char in)  (return chars))
      ((equal? peeked #\space) (read-char in)  (return chars))
      ((equal? peeked #\.) (return chars))
      ((equal? peeked #\λ) (return chars))
      ((equal? peeked #\() (return chars))
      ((equal? peeked #\)) (return chars))
      (else (let ((char (read-char in)))
              (loop (peek-char in) (cons char chars)))))))