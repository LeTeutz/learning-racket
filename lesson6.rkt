#lang plai-typed

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : symbol) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)])

(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

(define-type Binding
  [bind (name : symbol) (val : number)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

(define (lookup [for : symbol] [env : Env]) : number
  (cond
    [(empty? env) (error 'lookup "name not found")]
    [else (cond
            [(symbol=? for (bind-name (first env)))
             (bind-val (first env))]
            [else (lookup for (rest env))])]))

(define (interp [expr : ExprC] [env : Env] [fds : (listof FunDefC)]) : number
  (type-case ExprC expr
    [numC (n) n]
    [idC (n) (lookup n env)]
    [appC (f a) (local ([define fd (get-fundef f fds)])
                  (interp (fdC-body fd
                                    (extend-env (bind (fdC-arg fd)
                                                      (interp a env fds))
                                                mt-env)
                                    fds)))]
    [plusC (l r) (+ (interp l env fds) (interp r env fds))]
    [multC (l r) (* (interp l env fds) (interp r env fds))]))

(fdC ’double ’x (plusC (idC ’x) (idC ’x)))
(fdC ’quadruple ’x (appC ’double (appC ’double (idC ’x))))
(fdC ’const5 ’_ (numC 5))

    (test (interp (plusC (numC 10) (appC 'const5 (numC 10)))
                  mt-env
                  (list (fdC 'const5 '_ (numC 5))))
          15)
     
    (test (interp (plusC (numC 10) (appC 'double (plusC (numC 1) (numC 2))))
                  mt-env
                  (list (fdC 'double 'x (plusC (idC 'x) (idC 'x)))))
          16)
     
    (test (interp (plusC (numC 10) (appC 'quadruple (plusC (numC 1) (numC 2))))
                  mt-env
                  (list (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x))))
                        (fdC 'double 'x (plusC (idC 'x) (idC 'x)))))
          22)



