#lang racket/base

;; miniKanren logic programming Domain Specific Language (DSL) for Racket.
;;
;; There are many miniKanren implementations out there, but this one is mine.
;; See http://minikanren.org/ for more info about miniKanren.

;; This implementation is based on the Second Edition of "The Reasoned Schemer
;; (TRS), with some small modifications to adapt it to Racket -- these
;; modifications are described in the comments throughout the code.

(provide

 ;; The basic goals
 fail succeed ==

 ;; Goal combiners
 once conj disj ifte cond/e cond/a cond/u

 ;; variables and relations
 fresh defrel project ground? ground*?

 ;; Running miniKanren programs
 run run*

 ;; A small miniKanren library
 car/o cdr/o pair/o cons/o null/o list/o member/o proper-member/o append/o
 always/o never/o

 ;; Debugging aids
 show)

;;;; IF YOU ARE FAMILIAR WITH THE FIRST EDITION

;; Note that the second edition is a slightly different language than the
;; first edition.  If you are familiar with the first edition, this message
;; contains a summary of the differences:
;;
;; https://groups.google.com/g/minikanren/c/fMKSBmOm_XM/m/V0K30F8lBwAJ
;;
;; Also note that this code uses a different spelling for the key terms.  In
;; particular the superscripts of the names in the book are translated as a
;; "/".  That is, we use "cond/e" instead of "conde" and "member/o" instead of
;; "membero"

;;;; KEY CONCEPTS

;; You probably need to read the TRS book or one of the miniKanren papers to
;; fully understand the implementation but here are a few key concepts:
;;
;; * a VARIABLE is a unique name within the language.  Variables are
;;   represented as an instance of a `var` structure containing a name, but
;;   the name is ignored.  Two variables are the same if they are
;;   `eq?`. I.e. we want (eq? (var 'foo) (var 'foo)) to be #f
;;
;; * a TERM represents data: it can be an immediate value, like 1, "hello" or
;;   'foo, a variable, like (var 'foo), or a nested list of these, e.g. (list
;;   1 'foo (var 'bar))
;;
;; * a SUBSTITUTION is a mapping from VARIABLES to TERMS.  These are
;;   represented as immutable hash tables, which are extended functionally.
;;
;; * a STREAM has the same meaning as streams in Racket, but we build our own
;;   here.  It is a "generalized list", where the CDR can be a procedure
;;   producing other streams.  This allows representing infinite streams using
;;   finite resources.  See `stream-cons` in the Racket documentation if you
;;   want to better understand the concept.  In miniKanren, all values
;;   produced by a stream are substitutions, that is, streams are only used as
;;   substitution streams
;;
;; * a GOAL is a function which accepts a SUBSTITUTION and returns a STREAM of
;;   substitutions.  The basic goals are `fail`, `succeed` and `==` (see their
;;   implementation).  Other goals, like `conj` and `disj` can be used to
;;   combine goals, `defrel` can be used to create new goals and `fresh` and
;;   `cond/e` can be used to provide some "flow control"
;;
;; * a miniKanren program can be run using `run` or `run*` which takes a goal
;;   and feeds it an empty substitution and extracts the substitutions it
;;   produces, which are the result of the program.  RUN finds a specified
;;   number of solutions, while RUN* finds all the possible solutions.  `(run*
;;   q goal)` is equivalent to `(run #f q goal)`

;;;; EMACS RACKET-MODE INTEGRATION

;; If you are using racket-mode
;; (https://github.com/greghendershott/racket-mode), the add the following to
;; your emacs config to properly indent miniKanren programs

;; (defun setup-miniKanren-indentation ()
;;   (put 'fresh 'racket-indent-function 1)
;;   (put 'project 'racket-indent-function 1)
;;   (put 'defrel 'racket-indent-function 1)
;;   (put 'run 'racket-indent-function 2)
;;   (put 'run* 'racket-indent-function 1))

;; (add-hook 'racket-mode-hook #'setup-miniKanren-indentation)

;;;; IMPLEMENTATION

(module+ test
  ;; NOTE: rackunit defines `fail`, which we don't use and also we define a
  ;; goal named `fail`, so don't import that name..
  (require (except-in rackunit fail))
  (printf "Running the test module in miniKanren~%"))


;;.......................................... Variables and Substitutions ....

;; A logic variable.  TRS represents them as vectors, but in Racket we can
;; just use a struct.  Note that the NAME is not used, and var instances are
;; compared using EQV?, that is (eqv? (var 'foo) (var 'foo)) is #f and we make
;; use of that fact.
(struct var (name) #:transparent)

(module+ test

  ;; We rely on the fact that two var instances of the same name are not
  ;; equal, encode that in a test...
  (check-false (eqv? (var 'x) (var 'x)))

  ;; Make some variables which will be used in the tests below
  (define u (var 'u))
  (define v (var 'v))
  (define w (var 'w))
  (define x (var 'x))
  (define y (var 'y))
  (define z (var 'z)))

;; A substitution maps variables to terms.  TRS represents them as lists and
;; uses ASSV? to search them, we represent them as immutable hash tables.
(define the-empty-substitution (hasheq))

;; Sentry value to differentiate a "not found" walked variable, since
;; variables can be associated with #f.  This is simply a value that is not
;; `eq?` with anything else.
(define walk-sentry (var 'walk-sentry))

;; Walks the value V in the substitution S.  If V is not a variable, it is
;; simply returned, otherwise it is looked up in the substitution.  If the
;; result of the lookup is also a variable, it is looked up again in the same
;; substitution, until we either reach a value (i.e. a non-variable) or a
;; variable which is not in the substitution (i.e. it is fresh)
(define (walk v s)
  (if (var? v)
      (let ([a (hash-ref s v walk-sentry)])
        (if (eq? a walk-sentry)
            v
            (walk a s)))
      v))

(module+ test
  (define f13 (hasheq z 'a x w y z))
  (check-equal? (walk y f13) 'a)
  (check-equal? (walk z f13) 'a)
  (check-equal? (walk x f13) w)

  (define f16 (hasheq x y v x w x))
  (check-equal? (walk y f16) y)
  (check-equal? (walk v f16) y)
  (check-equal? (walk w f16) y)

  (define f80 (hasheq x #f))
  (check-equal? (walk x f80) #f)
  
  )

;; Check if the variable U occurs anywhere in the term V based on the
;; substitution S.  OCCURS? is used to prevent adding cycles of variable
;; references to substitutions.
(define (occurs? u v s)
  (let ([v (walk v s)])
    (cond ((var? v) (eqv? v u))
          ((pair? v)
           (or (occurs? u (car v) s)
               (occurs? u (cdr v) s)))
          (else #f))))

(module+ test
  (check-equal? (occurs? x y (hasheq y 1)) #f)
  ;; A var cannot reference itself, even through cycles or nested in terms...
  (check-equal? (occurs? x x the-empty-substitution) #t)
  (check-equal? (occurs? x `(,y) (hasheq y x)) #t)
  (check-equal? (occurs? x `(,x) the-empty-substitution) #t))

;; Extent the substitution S with a mapping from U to V, but only if this
;; would not create a cycle (according to OCCURS?)
(define (extend u v s)
  (and (not (occurs? u v s))
       (hash-set s u v)))

(module+ test
  ;; Cannot extend if cycles are created.
  (check-equal? (extend x `(,y) (hasheq y x)) #f)

  (check-equal? (let ([s (hasheq z x y z)])
                  (let ([s (extend x 'e s)])
                    (and s (walk y s))))
                'e))

;; Unify the two terms U and V extending the substitution S with new variable
;; mappings as needed.  Returns an updated substitution, or #f if the two
;; values cannot be unified.
(define (unify u v s)
  (let ([u (walk u s)]
        [v (walk v s)])
    (cond ((eqv? u v) s)
          ((var? u) (extend u v s))
          ((var? v) (extend v u s))
          ((and (pair? u) (pair? v))
           (let ([s (unify (car u) (car v) s)])
             (and s (unify (cdr u) (cdr v) s))))
          (else #f))))

(module+ test
  (check-equal?
   (unify `(,x ,y, z) `(,u . ,v) the-empty-substitution)
   (hasheq x u v `(,y ,z)))

  (check-equal?
   (unify `(,x ,y ,z) `(,u ,v, w . ()) the-empty-substitution)
   (hasheq x u y v z w)))


;;.............................................................. Streams ....

;; Wrapper around (lambda () ...) which is used to create suspensions of
;; streams to better see where these suspensions happen (copied from
;; https://github.com/michaelballantyne/faster-miniKanren/blob/master/mk.scm)
(define-syntax suspend
  (syntax-rules ()
    ((_ body) (lambda () body))))

;; Take N values from the stream S -- takes less values if the stream has less
;; than N values.  If N is #t it takes all values from the stream S -- this is
;; bad if the stream is infinite
(define (take n s)
  (cond ((and n (zero? n)) '())
        ((null? s) '())
        ((pair? s)
         (cons (car s)
               (take (and n (sub1 n)) (cdr s))))
        (else (take n (s)))))

(module+ test
  ;; Lists are streams
  (check-equal? (take 0 '(1 2 3))  '())
  (check-equal? (take 2 '(1 2 3)) '(1 2))
  (check-equal? (take 5 '(1 2 3)) '(1 2 3))
  (check-equal? (take #f '(1 2 3)) '(1 2 3))

  (define number-stream
    (lambda (n)
      (cons n (suspend (number-stream (add1 n))))))

  (check-equal? (take 0 (number-stream 0)) '())
  (check-equal? (take 3 (number-stream 0)) '(0 1 2)))

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

(module+ test
  (check-equal? (take #f (interleave '(a b c) '(1 2 3)))
                '(a 1 b 2 c 3))

  (check-equal? (take 6
                      (interleave (number-stream 0)
                                  (number-stream 100)))
                '(0 100 1 101 2 102))
  (check-equal? (take 8
                      (interleave '(0 1 2 3) (number-stream 100)))
                '(0 100 1 101 2 102 3 103))
  (check-equal? (take 10
                      (interleave (number-stream 100) '(0 1 2 3)))
                '(100 0 101 1 102 2 103 3 104 105))

  ;; This is an infinite stream which does not produce any values, every time
  ;; it is forced, it just produces another procedure.
  (define infinite-stream-of-nothing
    (lambda ()
      (suspend (infinite-stream-of-nothing))))

  ;; We should still be able to interleave such streams...
  (check-equal? (take 3 (interleave infinite-stream-of-nothing
                                    (number-stream 0)))
                '(0 1 2))

  (check-equal? (take 3 (interleave (number-stream 0)
                                    infinite-stream-of-nothing))
                '(0 1 2))

  ;; No infinite recursion on this one...
  (check-equal? (take 0 (interleave infinite-stream-of-nothing
                                    infinite-stream-of-nothing))
                '()))

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

(module+ test

  (define number-stream/prefix
    (lambda (prefix n)
      (cons (list prefix n) (suspend (number-stream/prefix prefix (add1 n))))))
  
  (define letter-stream-stream
    (lambda (n)
      (number-stream/prefix (integer->char(+ (char->integer #\a) n)) 0)))
  
  (check-equal? (take 20 (map-and-interleave letter-stream-stream (number-stream 0)))
                '((#\a 0) (#\b 0) (#\a 1) (#\a 2) (#\c 0) (#\a 3) (#\a 4)
                  (#\b 1) (#\a 5) (#\a 6) (#\a 7) (#\b 2) (#\a 8) (#\a 9)
                  (#\d 0) (#\a 10) (#\a 11) (#\b 3) (#\a 12) (#\a 13))))


;;................................................................ Goals ....

;; SUCCEED is a goal that always succeeds, returning a stream of the single
;; substitution that is passed in.
(define succeed
  (lambda (s)
    (list s)))

;; FAIL is a goal that always fails, returning the empty stream, regardless of
;; what substitutions are passed in.
(define fail
  (lambda (_s)
    '()))

;; == is a goal that attempts to unify the terms U and V using `unify`.  If
;; unification succeeds, the goal succeeds with the extended substitution,
;; otherwise this goal fails.
(define (== u v)
  (lambda (s)
    (let ([s (unify u v s)])
      (if s (succeed s) (fail s)))))

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

;; if,then,else for logic programming: If G1 holds, G2 must also hold, with
;; any unifications made by G1, otherwise G3 holds.
(define (ifte g1 g2 g3)
  (lambda (s)
    (let loop ((s-inf (g1 s)))
      (cond ((null? s-inf)
             (g3 s))
            ((pair? s-inf)
             (map-and-interleave g2 s-inf))
            (else
             (suspend (loop (s-inf))))))))

(module+ test
  
  (check-equal? (take #f ((ifte succeed (== #f y) (== #t y)) the-empty-substitution))
                (list (hasheq y #f)))

  (check-equal? (take #f ((ifte fail (== #f y) (== #t y)) the-empty-substitution))
                (list (hasheq y #t)))
  
  (check-equal? (take #f ((ifte (== #t x) (== #f y) (== #t y)) the-empty-substitution))
                (list (hasheq x #t y #f)))

  (check-equal? (take #f ((ifte (disj (== #t x) (== #f x)) (== #f y) (== #t x))
                          the-empty-substitution))
                ;; NOTE: two substitutions are produced by this if statement
                (list
                 (hasheq x #t y #f)
                 (hasheq x #f y #f))))

(define (once g)
  (lambda (s)
    (let loop ((s-inf (g s)))
      (cond ((null? s-inf)
             '())
            ((pair? s-inf)
             (cons (car s-inf) '()))
            (else
             (suspend (loop (s-inf))))))))

(define-syntax fresh
  (syntax-rules ()
    ((fresh () g ...)
     (conj g ...))
    ((fresh (x ...) g ...)
     ((lambda (x ...)
        (conj g ...))
      (var 'x) ...))))

;; A "COND" goal -- all branches contribute values
(define-syntax cond/e
  (syntax-rules ()
    ((conde (g ...) ...)
     (disj (conj g ...) ...))))

;; Similar to cond/e, but only the first branch that succeeds contributes
;; values.
(define-syntax cond/a
  (syntax-rules ()
    ((cond/a (g0 g ...)) (conj g0 g ...))
    ((cond/a (g0 g ...) ln ...)
     (ifte g0 (conj g ...) (cond/a ln ...)))))

;; Similar to cond/a, but a successful question succeeds once
(define-syntax cond/u
  (syntax-rules ()
    ((cond/u (g0 g ...) ...)
     (cond/a ((once g0) g ...) ...))))


;; Run a toplevel goal G and take N substitutions (results) from it using
;; TAKE.
(define (run-goal n g)
  (take n (g the-empty-substitution)))

;; Produce a reified symbol (one that is unique) based on the number N.  This
;; is used for variables which remained fresh (unbound) after running the
;; goals.
(define (reify-name n)
  (string->symbol (string-append "_." (number->string n))))

;; Same as WALK, but V is a term (e.g. a list of other variables) and all the
;; elements of V are walked.
(define (walk* v s)
  (let ([v (walk v s)])
    (cond ((var? v) v)
          ((pair? v)
           (cons
            (walk* (car v) s)
            (walk* (cdr v) s)))
          (else v))))

(define (reify-s v r)
  (let ((v (walk v r)))
    (cond ((var? v)
           (let ([n (hash-count r)])
             (let ((rn (reify-name n)))
               (hash-set r v rn))))
          ((pair? v)
           (let ([r (reify-s (car v) r)])
             (reify-s (cdr v) r)))
          (else
           r))))

(define (reify v)
  (lambda (s)
    (let ([v (walk* v s)])
      (let ([r (reify-s v the-empty-substitution)])
        (walk* v r)))))

(module+ test
  (check-equal? (reify-s `(,x ,y ,x ,z ,z) the-empty-substitution)
                (hasheq x '_.0 y '_.1 z '_.2))
  (check-equal? ((reify `(,x ,y ,x ,z ,z)) the-empty-substitution)
                '(_.0 _.1 _.0 _.2 _.2)))


;; Run a goal with a toplevel fresh variable (or list of variables) that are
;; available to the goals.  substitutions are taken out of the goal using
;; RUN-GOAL and the variable (or list of variables) are reified (their vales
;; resolved and returned.
(define-syntax run
  (syntax-rules ()
    ((run n (x0 x ...) g ...)
     (run n q (fresh (x0 x ...)
                (== `(,x0 ,x ...) q) g ...)))
    ((run n q g ...)
     (let ([q (var 'q)])
       (map (reify q)
            (run-goal n (conj g ...)))))))

;; Same as RUN, but fetches all the results from a goal -- this will run
;; forever for goals that produce an infinite number of values.
(define-syntax run*
  (syntax-rules ()
    ((run* q g ...)
     (run #f q g ...))))

;; Project is similar to fresh, except that it binds walked variables to the
;; names
(define-syntax project
  (syntax-rules ()
    ((project (x ...) g ...)
     (lambda (s)
       (let ([x (walk* x s)] ...)
         ((conj g ...) s))))))

;; Return true if X is a ground value (i.e. not a variable)
(define (ground? x) (not (var? x)))

;; Return true if X is a ground value all the way down, that is, X is not a
;; variable nor a list containing variables.
(define (ground*? x)
  (cond ((var? x) #f)
        ((pair? x) (and (ground*? (car x)) (ground*? (cdr x))))
        (else #t)))


;;........................................................ Debgging Aids ....

;; A goal used to print out values of miniKanren variables when running
;; programs. Does not change the substitution, just prints the reified values
;; if the variables passed in.  Note that reification happens just for a
;; single show call, so reified values from different show calls are not the
;; same...

;; This is the internal implementation, see show for the actual macro
(define show-impl
  (lambda (heading vars vals)
    (lambda (s)
      (printf "*** ~a~%" heading)
      (let loop ([vars vars]
                 [vals vals]
                 [reify-substitution the-empty-substitution])
        (unless (or (null? vars) (null? vals))
          (let* ([v (walk* (car vals) s)]
                 [r (reify-s v reify-substitution)])
            (printf "    ~a -> ~a~%" (car vars) (walk* v r))
            (loop (cdr vars) (cdr vals) r))))
      (list s))))

(define-syntax show
  (syntax-rules ()
    ((show heading)
     (show-impl heading '() '()))
    ((show heading x ...)
     (show-impl heading '(x ...) (list x ...)))))


;;........................................... A small miniKanren library ....

(defrel (car/o p a)
  (fresh (d)
    (== (cons a d) p)))

(defrel (cdr/o p d)
  (fresh (a)
    (== (cons a d) p)))

(defrel (cons/o a d p)
  (== (cons a d) p))

(defrel (null/o p)
  (== '() p))

(defrel (pair/o p)
  (fresh (a d)
    (cons/o a d p)))

;; NOTE: this is the faster version of member/o, with all "fail" relations
;; removed, since they are not needed.  Also we use disj/2 instead of cond/e

(defrel (member/o x l)
  (disj/2
   (car/o l x)
   (fresh (d)
     (cdr/o l d)
     (member/o x d))))

(defrel (list/o l)
  (cond/e
   ((null/o l))
   ((fresh (d)
      (cdr/o l d)
      (list/o d)))))

(defrel (proper-member/o x l)
  (cond/e
   ((car/o l x)
    (fresh (d)
      (cdr/o l d)
      (list/o d)))
   ((fresh (d)
      (cdr/o l d)
      (proper-member/o x d)))))

(defrel (append/o l t out)
  (cond/e
   ((null/o l) (== t out))
   ((fresh (a d res)
      (cons/o a d l)
      (cons/o a res out)
      ;; NOTE: recursive goal is last, see the "First Commandment"
      (append/o d t res)))))

(defrel (always/o)
  (cond/e
   (succeed)
   ((always/o))))

(defrel (never/o)
  (never/o))

(module+ test
  ;; This hangs if defrel is not defined to suspend relations
  (check-equal? (run 1 q (cond/e ((never/o)) (succeed))) '(_.0)))
