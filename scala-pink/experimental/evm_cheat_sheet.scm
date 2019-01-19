cadr/cdr => factorial result
car => decrement
cddr => 0s

(cons_ (cons_ (car_ s) (cadr_ s)) (cddr_ s))

((dec, fac), 0s)

((_, result), _)

(ref (cdr_ (cadr_ s))) before if branch AP

(ref (cdr_ (car_ e)) at beginning of if statement after AP from branch AND still present before jmp to branches

(ref (cdr_ (car_ e))) before innermost RET

#######

(maybe-lift (lambda f x
    (eval (cadddr exp)
        (lambda _ y
            (if (eq? y (cadr exp)) f
                (if (eq? y (caddr exp))
                    x
                    (env y)))))))

lam (x, y, ...) body



(car exp) => identifier
(cadr exp) => args
(caddr exp) => func

(maybe-lift (lambda (args)
    (eval (caddr exp)
        (lambda (y)
            (if (eq? <...check args...> (cadr exp)) // env should try all arguments and then fall back to environment
                <...return arg...>
                (env y)))))))))


; update : (a -> b) a b -> (a -> b)
(define (update f x y)
  (λ (x*)
    (if (equal? x x*)
        y
        (f x*)))

### VM compiler factorial should produce below code but produces LD 3 2 instead of LD 1 2 in if branch:
val exp = Tup(Str("NIL"),
                    Tup(Str("LDC"),Tup(Cst(1),
                    Tup(Str("CONS"),
                    Tup(Str("LDC"),Tup(Cst(3),
                    Tup(Str("CONS"),
                    Tup(Str("LDF"),
                        Tup(Tup(Str("DUM"),Tup(Str("NIL"),
                        Tup(Str("LDF"),
                            Tup(Tup(Str("LDC"),Tup(Cst(0),
                            Tup(Str("LD"),Tup(Tup(Cst(1),Tup(Cst(1),Str("."))),
                            Tup(Str("EQ"),
                            Tup(Str("SEL"),
                                Tup(Tup(Str("LD"),Tup(Tup(Cst(1),Tup(Cst(2),Str("."))),
                                    Tup(Str("JOIN"),Str(".")))),
                                Tup(Tup(Str("NIL"),
                                    Tup(Str("LD"),Tup(Tup(Cst(1),Tup(Cst(2),Str("."))),
                                    Tup(Str("LD"),Tup(Tup(Cst(1),Tup(Cst(1),Str("."))),
                                    Tup(Str("MPY"),Tup(Str("CONS"),
                                    Tup(Str("LD"),Tup(Tup(Cst(3),Tup(Cst(2),Str("."))),
                                    Tup(Str("LD"),Tup(Tup(Cst(1),Tup(Cst(1),Str("."))),
                                    Tup(Str("SUB"),Tup(Str("CONS"),
                                    Tup(Str("LD"),Tup(Tup(Cst(2),Tup(Cst(1),Str("."))),
                                    Tup(Str("AP"),Tup(Str("JOIN"),Str(".")))))))))))))))))),
                            Tup(Str("RTN"),Str(".")))))))))),
                            Tup(Str("CONS"),Tup(Str("LDF"),
                                            Tup(Tup(Str("NIL"),
                                                Tup(Str("LD"),Tup(Tup(Cst(2),Tup(Cst(2),Str("."))),
                                                Tup(Str("CONS"),
                                                Tup(Str("LD"),Tup(Tup(Cst(2),Tup(Cst(1),Str("."))),
                                                Tup(Str("CONS"),Tup(Str("LD"),Tup(Tup(Cst(1),Tup(Cst(1),Str("."))),
                                                Tup(Str("AP"),Tup(Str("RTN"),Str(".")))))))))))),
                            Tup(Str("RAP"),
                            Tup(Str("RTN"),Str(".")))))))))),
                    Tup(Str("AP"),
                    Tup(Str("STOP"),Str("."))))))))))))
        // val comp_exp = compile(exp, Nil, Tup(Str("STOP"), N))
        val res = evalms(applyProc(vmc_src_state.v, List(exp), vmc_src_state.s, Halt()).asInstanceOf[State]).asInstanceOf[Answer].v
        println(deref(res))

####

(lambda (x) (eval (caddr exp) 
    (lambda (y)
        (if (eq? y (car (cadr exp)))
            x
            (env y)))))

with multi-argument support:

(lambda ([args]) (eval (caddr exp) 
    (lambda (y)
        (if (eq? y (car (cadr exp)))
            x
            (env y)))))

args (f x y)
    lam z
        if x == z
            y
        else
            f(z)

(define (update* f xs ys)
    (match* (xs ys)
        [['() '()]    f]
        [[(cons x xs*) (cons y ys*)]
        (update* (update f x y) xs* ys*)]))

(letrec
    (
        (update (lambda (f x y) (lambda (z) (if (eq? z x) y (f z)))))
        (updateMany (lambda (f xs ys) (lambda (if (eq? xs '())
                                                (if (eq? ys '())
                                                    f
                                                    (updateMany (update f (car xs) (car ys)) (cdr xs) (cdr ys)))))))
    ) ...)

(let fargs
    (updateMany (lambda (x) 'init) (cadr exp) (cadr exp)) 
        (lambda (x) x))

(letrec
    ((update (lambda (f x y) (lambda (z) (if (eq? z x) y (f z)))))
        (updateMany (lambda (f xs ys) (if (and (eq? xs '()) (eq? ys '()))
                                            f
                                            (updateMany (update f (car xs) (car ys)) (cdr xs) (cdr ys)))))))


// Lift all numeric inputs to VM
(let start (lambda (ops) (machine vm-stack env-list op-list call-stack ops))
        (start program)
        
(let start (lambda (ops)
(letrec ((liftNums (lambda (x)
(if (pair? x)
(if (num? (car x))
(cons (maybe-lift (car x)) (liftNums (cdr x)))
(if (pair? (car x))
(cons (liftNums (car x)) (liftNums (cdr x)))
(cons (car x) (liftNums (cdr x)))))
x))))
(machine vm-stack env-list op-list call-stack (liftNums ops))))
(start program)
                