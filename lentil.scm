(require-extension srfi-1)
(require-extension srfi-4)
(require-extension srfi-14)
(require-extension srfi-13)
(require-extension fmt)
(require-extension matchable)
; (require-extension type-extensions)

(export compile)

(define (compile raw next)
  (match raw
    [(? symbol?) `(refer ,raw ,next)]

    [('quote val) `(constant ,val ,next)]

    [('lambda vars body)
     (list 'close vars (compile body '(return)) next)]

    [('if test then else)
     (let ([thenc (compile then next)]
	   [elsec (compile else next)])
       (compile test `(test ,thenc ,elsec)))]

    [('set! var raw-val)
     (compile raw-val `(assign ,var ,next))]

    [('call/cc raw-cb)
     (let ([compiled `(conti (argument ,(compile raw-cb '(apply))))])
       (if (tail-instr? next)
	   compiled
	   `(frame ,next ,compiled)))]

    [('let vars body)
     (compile (cons (list 'lambda (map car vars) body) (map cadr vars)) next)]

    [(? pair?)
     (let loop ([args (cdr raw)]
		[compiled (compile (car raw) '(apply))])
       (if (null? args)
	   (if (tail-instr? next)
	       compiled
	       `(frame ,next ,compiled))
	   (loop (cdr args)
		 (compile (car args)
			  `(argument ,compiled)))))]

    [_ `(constant ,raw ,next)]))

(define (interpret-asm acc next-expr env val-rib stack)
  (match next-expr
    [('halt) acc]
    
    [('refer var next-expr)
     (interpret-asm (car (lookup var env)) next-expr env val-rib stack)]
    
    [('constant val next-expr)
     (interpret-asm val next-expr env val-rib stack)]
    
    [('close vars body next-expr)
     (interpret-asm (make-closure body env vars) next-expr env val-rib stack)]

    [('test then else)
     (interpret-asm acc (if acc then else) env val-rib stack)]

    [('assign var next-expr)
     (set-car! (lookup var env) acc)
     (interpret-asm acc next-expr env val-rib stack)]

    [('conti next-expr)
     (interpret-asm (make-continuation stack) next-expr env val-rib stack)]

    [('nuate stack var)
     (interpret-asm (car (lookup var env)) '(return) env val-rib stack)]

    [('frame ret next-expr)
     (interpret-asm acc next-expr env '() (call-frame ret env val-rib stack))]

    [('argument next-expr)
     (interpret-asm acc next-expr env (cons acc val-rib) stack)]

    [('apply)
     (match acc
       [#('closure body env vars) acc
	(interpret-asm acc body (extend env vars val-rib) '() stack)]
       [(? procedure?)
	(acc val-rib)])]

    [('return)
     (match-let ([(next-expr env val-rib stack) stack])
       (interpret-asm acc next-expr env val-rib stack))]))

(define (make-closure body env vars)
  (vector 'closure body env vars))

(define (make-continuation stack)
  (make-closure `(nuate ,stack v) '() '(v)))

(define (lookup var env)
  (let next-rib ([env env])
    (let next-elem ([vars (caar env)] [vals (cdar env)])
      (cond
       [(null? vars) (next-rib (cdr env))]
       [(eq? (car vars) var) vals]
       [else (next-elem (cdr vars) (cdr vals))]))))

(define (call-frame next-expr env val-rib stack)
  (list next-expr env val-rib stack))

(define (extend env vars vals)
  (cons (cons vars vals) env))

(define (tail-instr? next) (eq? (car next) 'return))

#|(interpret-asm #f (compile '(+ 1 2) '(halt))
	       (list (cons '(+)
			   (list (lambda (rib) (+ (car rib) (cadr rib))))))
	       '()
	       '())|#
