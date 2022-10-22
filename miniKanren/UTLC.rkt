#lang racket

(require "mk/mk.rkt")

(defrel (lookupo ctx x t)
  (fresh (x1 t1 ctx1)
         (== `((,x1 . ,t1) . ,ctx1) ctx)
         (conde
          [(== x1 x) (== t1 t)]
          [(lookupo ctx1 x t)])))
;#;
(define CTX_0
  '((x . Nat) (x . Bool) (y . Nat)))
#;
(run 2 (res)
     (lookupo CTX_0 'x res))

(defrel (novaro var env)
  (conde
   [(== '() env)]
   [(fresh (env1 c1 name x)
           (== `(,c1 . ,env1) env)
           (== `(,name . ,x) c1)
           (=/= name var)
           (novaro var env1))]))

#;
(run 1 (r)
     (novaro r CTX_0))
#;
(run 10 (r)
     (novaro 'list r))

(defrel (val*o env es val)
  (conde
   [(== '() es)
    (== '() val)]
   [(fresh (x xs)
           (== `(,x . ,xs) es)
           (fresh (xo xso)
                  (valo env x xo)
                  (val*o env xs xso)
                  (== `(,xo . ,xso) val)))]))

(defrel (valo env exp val)
  (conde
   #;
   [(numbero exp)
    (== exp val)]
   [(symbolo exp)
    (lookupo env exp val)]
   [(absento 'clos val)
    (== `(quote ,val) exp)]
   [(fresh (xs)
           (novaro 'list env)
           (== `(list . ,xs) exp)
           (val*o env xs val))]
   [(fresh (x body)
           (=/= x 'qoute)
           (=/= x 'lambda)
           (symbolo x)
           (novaro 'list env)
           (== `(lambda (,x) ,body) exp)
           (== `(clos ,env ,x ,body) val))]
   [(fresh (rator rand)
           (== `(,rator ,rand) exp)
           (fresh (env1 x body valrand)
                  (valo env rator `(clos ,env1 ,x ,body))
                  (valo env rand valrand)
                  (valo `((,x . ,valrand) . ,env1) body val)))]))

(run 2 (quine)
     (valo '()
           quine
           quine
           ))

#;
(run 1 (x y)
     (valo '()
           `(((lambda (x) (lambda (y) x)) ,x) ,y)
           5))
#;
(run 2 (quine)
     (valo '()
           quine
           '(list 1 2 3 'cat)
           ))
#;
(run 1 (q)
     (valo '()
           '(((lambda (x) (lambda (y) (list x y))) 'meow) 'wolf)
           q))