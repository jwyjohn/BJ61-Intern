#lang racket

(require racket/trace)


(provide run run*
         == =/=
         fresh eigen defrel
         conde conda condu
         symbolo numbero ;; not-pairo
         absento
         project)

;; extra stuff for racket
;; due mostly to samth
(define (list-sort f l) (sort l f))
(define (remp f l) (filter-not f l))
(define (call-with-string-output-port f)
  (define p (open-output-string))
  (f p)
  (get-output-string p))
(define (exists f l) (ormap f l))
(define for-all andmap)
(define (find f l)
  (cond [(memf f l) => car] [else #f]))
(define memp memf)
(define (var*? v) (var? (car v)))

;;.............................................................. Streams ....

;; Wrapper around (lambda () ...) which is used to create suspensions of
;; streams to better see where these suspensions happen (copied from
;; https://github.com/michaelballantyne/faster-miniKanren/blob/master/mk.scm)

(define-syntax suspend
  (syntax-rules ()
    ((_ body) (lambda () body))))

;; Interleave values from two (possibly infinite) streams.  TRS calls this
;; `append-inf` -- NOTE that this is implemented differently than `append-inf`
(define (interleave s-inf t-inf)
  (cond
    ((null? s-inf) t-inf)
    ((pair? s-inf)
     (cons (car s-inf) (suspend (interleave t-inf (cdr s-inf)))))
    ((procedure? s-inf)
     ;; Force the stream, but only once, this prevents streams that keep
     ;; producing only procedures to overtake the evaluation.
     (suspend
      (let ([forced (s-inf)])
        (if (or (null? forced) (pair? forced))
            (interleave forced t-inf)
            (interleave t-inf forced)))))
    (else
     (error (format "interleave: unexpected value for s-inf: ~a" s-inf)))))


;; Disjunction of two goals: the two goals must hold, but not at the same time.
(define (disj/2 g1 g2)
  (lambda (s)
    (interleave (suspend (g1 s)) (suspend (g2 s)))))

;; Disjunction of any number of goals.
(define-syntax disj
  (syntax-rules ()
    ((disj) fail)
    ((disj g) g)
    ((disj g0 g ...) (disj/2 g0 (disj g ...)))))

;; Apply the goal G to every substitution from S-INF (a possibly infinite
;; stream) and interleave the resulting streams together.  TRS calls this
;; `append-map-inf`
(define (map-and-interleave g s-inf)
  (cond
    ((null? s-inf) '())
    ((pair? s-inf)
     (interleave (g (car s-inf))
                 (map-and-interleave g (cdr s-inf))))
    (else
     (suspend (map-and-interleave g (s-inf))))))

;; Conjunction of two goals.  G1 and G2 must hold together.
(define (conj/2 g1 g2)
  (lambda (s)
    (map-and-interleave g2 (suspend (g1 s)))))

;; Conjunction of any number of goals.
(define-syntax conj
  (syntax-rules ()
    ((conj) succeed)
    ((conj g) g)
    ((conj g0 g ...) (conj/2 g0 (conj g ...)))))

;; Syntax sugar to define new relations, so the user does not need to know
;; about substitutions and streams.
(define-syntax defrel
  (syntax-rules ()
    ((defrel (name x ...) g ...)
     (define (name x ...)
       (lambda (s)
         (suspend ((conj g ...) s)))))))

;; actual code

(include "mk.scm")
