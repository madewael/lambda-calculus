#lang racket

(require "lambda-expressions.rkt")

(define (gen-sym v)
  (define (split str)
    (define (split prefix-rev suffix-rev)
      (cond ((null? prefix-rev)(values str #f))
            ((equal? (car prefix-rev) #\-)(values (list->string(reverse prefix-rev))
                                                  (list->string(reverse suffix-rev))))
            (else (split (cdr prefix-rev)
                         (cons (car prefix-rev) suffix-rev)))))
    (split (reverse (string->list str)) '()))
  
  (string->symbol (let-values (((prefix suffix) (split (symbol->string v))))
                    (cond ((and suffix (string->number suffix)) => (λ(n)(format "~a~a" prefix (+ n 1))))
                          (else (format "~a-1" v))))))

(define named-terms '())
(define (add-term*! name term)(set! named-terms (cons (cons name term) named-terms)))
(define-syntax add-term!
  (syntax-rules ()
    ((add-term! name term)(add-term*! (quote name) (string->λ-expr term)))))


(define-syntax get-term
  (syntax-rules ()
    ((get-term name)(cond ((assoc (quote name) named-terms) => cdr)
                          (else (error 'get-term "Did not find term ~a" (quote name)))))))



(define (substitue from to λ-expr)
  (match λ-expr
    ((and v (? variable?)) (if (equal? from v) to v))
    ((application f p)     (application (substitue from to f) (substitue from to p)))
    ((abstraction v b)     (cond ((equal? from v) λ-expr)
                                 ((member v (free-variables to))(substitue from to (α-rename λ-expr (gen-sym v))))
                                 (else (abstraction v (substitue from to b)))))
    (_                     (error 'xxx))))

(define (free-variables λ-expr)
  (match λ-expr
    ((and v (? variable?)) (list v))
    ((application f p)(append (free-variables f)(free-variables p)))
    ((abstraction v b) (filter (λ (w)(not (eq? v w))) (free-variables b)))
    (_ (error 'xxx))))



(define (α-rename λ-expr to)
  (match λ-expr
    ((abstraction v b) (if (member to (free-variables λ-expr))
                           #f
                           (abstraction to (substitue v to b))))
    (_ #f)))

(define (ß-reduce λ-expr)
  (match λ-expr
    ((application (abstraction v b) p)(substitue v p b))
    (_ #f)))

(define (η-convert λ-expr)
  (error 'η-convert "NYI"))

(define (reduce-step λ-expr)
  (match λ-expr
    ((application f p)(cond ((reduce-step p) => (λ (r-p) (application f r-p)))
                            ((reduce-step f) => (λ (r-f) (application r-f p)))
                            ((ß-reduce λ-expr))
                            (else #f)))
    ((abstraction v b)(cond ((reduce-step b) => (λ (r-b)(abstraction v r-b)))
                            (else #f)))
    ((and v (? variable?)) #f)
    (x (error 'reduce λ-expr))))

(define (simple-reduce λ-expr)
  (let loop ((λ-expr λ-expr)(reduction (reduce-step λ-expr)))
    (if (and reduction (not (equal? λ-expr reduction)))
        (loop reduction (reduce-step reduction))
        λ-expr)))

(add-term! church-zero "λf.λx.x")
(add-term! church-incr "λn.λf.λx.(f ((n f)x))")
(add-term! church-plus "λm.λn.λf.λx.((m f)((n f)x))")
(add-term! church-times "λm.λn.λf.(m (n f))")

(define (church n)
  (when (or (not (number? n)) (< n 0))
    (error 'church "Expects non-negative number, given ~a." n))
  (let ((church-zero (get-term church-zero))
        (church-incr (get-term church-incr)))
    (define (church n)
      (if (= n 0)
          church-zero
          (application church-incr (church (- n 1)))))
    (simple-reduce (church n))))


(add-term! true  "λt.λf.t")
(add-term! false "λt.λf.f")

(add-term! and "λb1.λb2.((b1 b2)b1)")
(add-term! or  "λb1.λb2.((b1 b1)b2)")
(add-term! if  "λb.λthen.λelse.((b then)else)")

(define (make-if test consequent alternative)
  (application  (application (application (get-term if) test) consequent) alternative))

(add-term! church-zero? "λn.((n λx.λt.λf.f)λt.λf.t)")
(add-term! church-decr  "λn.λf.λx.(((n λg.λh.(h(g f)))λu.x)λu.u)")

(add-term! cons  "λcar.λcdr.λcons.((cons car)cdr)")
(add-term! car   "λcons.(cons λcar.λcdr.car)")
(add-term! cdr   "λcons.(cons λcar.λcdr.cdr)")
(add-term! empty "λx.λt.λf.t")
(add-term! null? "λp.(p λx.λy.λt.λf.f)")