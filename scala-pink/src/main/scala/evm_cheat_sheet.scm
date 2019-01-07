cadr/cdr => factorial result
car => decrement
cddr => 0s

(cons_ (cons_ (car_ s) (cadr_ s)) (cddr_ s))

((dec, fac), 0s)

((_, result), _)

(ref (cdr_ (cadr_ s))) before if branch AP

(ref (cdr_ (car_ e)) at beginning of if statement after AP from branch AND still present before jmp to branches

(ref (cdr_ (car_ e))) before innermost RET