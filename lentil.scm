(require-extension srfi-1)
(require-extension srfi-4)
(require-extension srfi-14)
(require-extension srfi-13)
(require-extension fmt)
(require-extension matchable)
; (require-extension type-extensions)

(export compile)

(define (compile raw env next)
  (match raw
    [(? symbol?) `(refer ,(compile-lookup raw env) ,next)]

    [('quote val) `(constant ,val ,next)]

    [('lambda vars body)
     (list 'close vars (compile body (extend env vars) '(return)) next)]

    [('if test then else)
     (let ([thenc (compile then env next)]
	   [elsec (compile else env next)])
       (compile test env `(test ,thenc ,elsec)))]

    [('set! var raw-val)
     (let ([access (compile-lookup raw env)])
       (compile raw-val env `(assign ,access ,next)))]

    [('call/cc raw-cb)
     (let ([compiled `(conti (argument ,(compile raw-cb env '(apply))))])
       (if (tail-instr? next)
	   compiled
	   `(frame ,next ,compiled)))]

    [('let vars body)
     (compile (cons (list 'lambda (map car vars) body) (map cadr vars)) env next)]

    [(? pair?)
     (let loop ([args (cdr raw)]
		[compiled (compile (car raw) env '(apply))])
       (if (null? args)
	   (if (tail-instr? next)
	       compiled
	       `(frame ,next ,compiled))
	   (loop (cdr args)
		 (compile (car args)
			  env
			  `(argument ,compiled)))))]

    [_ `(constant ,raw ,next)]))

(define (interpret-asm acc next-expr env val-rib stack)
  (match next-expr
    [('halt) acc]
    
    [('refer access next-expr)
     (interpret-asm (car (lookup access env)) next-expr env val-rib stack)]
    
    [('constant val next-expr)
     (interpret-asm val next-expr env val-rib stack)]
    
    [('close vars body next-expr)
     (interpret-asm (make-closure body env) next-expr env val-rib stack)]

    [('test then else)
     (interpret-asm acc (if acc then else) env val-rib stack)]

    [('assign access next-expr)
     (set-car! (lookup access env) acc)
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
       [#('closure body env) acc
	(interpret-asm acc body (extend env val-rib) '() stack)]
       [(? procedure?)
	(acc val-rib)])]

    [('return)
     (match-let ([(next-expr env val-rib stack) stack])
       (interpret-asm acc next-expr env val-rib stack))]))

(define (make-closure body env)
  (vector 'closure body env))

(define (make-continuation stack)
  (make-closure `(nuate ,stack (0 . 0)) '()))

(define (lookup access env)
  (let next-rib ([env env] [rib (car access)])
    (if (= rib 0)
	(let next-elem ([rib (car env)] [elem (cdr access)])
	  (if (= 0 elem)
	      rib
	      (next-elem (cdr rib) (- elem 1))))
	(next-rib (cdr env) (- rib 1)))))

(define (compile-lookup var env)
  (let next-rib ([env env] [rib 0])
    (let next-elem ([vars (car env)] [elem 0])
      (cond
       [(null? vars) (next-rib (cdr env) (+ rib 1))]
       [(eq? (car vars) var) (cons rib elem)]
       [else (next-elem (cdr vars) (+ elem 1))]))))

(define (call-frame next-expr env val-rib stack)
  (list next-expr env val-rib stack))

(define (extend env rib)
  (cons rib env))

(define (tail-instr? next) (eq? (car next) 'return))

#|(interpret-asm #f (compile '(let ([a 10] [b 20]) (+ a b)) '((+)) '(halt))
	       (list (list (lambda (rib) (+ (car rib) (cadr rib)))))
	       '()
	       '())|#
