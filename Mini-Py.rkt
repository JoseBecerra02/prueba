#lang eopl
; Sergio Escudero Tabares - 2040801
; Jose Miguel Becerra Casierra - 2043246
; Natalia Andrea Marín Hernandez - 2041622
; Esteban Andres Hernandez - 2042817
; Juan Esteban Brand Tovar - 2043291
; Link github: https://github.com/JoseBecerra02/Proyecto-final-FLP

;<programa> :=  <expresion>
;               un-programa (exp)
;<expresion> := <numero>
;               numero-lit (num)
;            := "\""<texto> "\""
;               texto-lit (txt)
;            := <boolExp>
;               bool-lit (bool)
;            := <identificador>
;               var-exp (id)
;            := (<expresion> <primitiva-binaria> <expresion>)
;               primapp-bin-exp (exp1 prim-binaria exp2)
;            := <primitiva-unaria> (<expresion>)
;               primapp-un-exp (prim-unaria exp)
;<primitiva-binaria> :=  + (primitiva-suma)
;                    :=  ~ (primitiva-resta)
;                    :=  / (primitiva-div)
;                    :=  * (primitiva-multi)
;                    :=  concat (primitiva-concat)
;
;<primitiva-unaria>:=  longitud (primitiva-longitud)
;                  :=  add1 (primitiva-add1)
;                  :=  sub1 (primitiva-sub1)

;Especificación Léxica

(define scanner-spec-simple-interpreter
  
'((white-sp (whitespace) skip)
  
  (comment (";" (arbno (not #\newline))) skip)
  
  (identifier (letter (arbno (or letter digit))) symbol)
  
  (number (digit (arbno digit)) number)
  
  (number ("-" digit (arbno digit)) number)
  
  (number (digit (arbno digit) "." digit (arbno digit)) number)
  
  (number ("-" digit (arbno digit) "." digit (arbno digit)) number)
  
  (text ("'" (arbno (or digit letter whitespace)) "'") string)
  
 )
)

;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter

  '((program (expression) a-program)
    
    ;;Declarar variables y constantes
    (expression ("var" (separated-list identifier "=" expression ",") "in" expression) variable-exp)
    (expression ("const" (separated-list identifier "=" expression ",") "in" expression) constante-exp)

    
    ;;Identificador 
    (expression (identifier) var-exp)


    ;;Valores
    (expression (number) lit-exp)
    (expression (text) txt-exp)
    

    ;;Booleanos
    (expression (expr-bool) boolean-exp)

    (expr-bool (boolean) booleano)
    (expr-bool (oper-un-bool "(" expression")") una-bool-exp)
    (expr-bool (oper-bin-bool "(" expression "," expression")") bin-bool-exp)
    (expr-bool (pred-prim "(" expression "," expression ")") pred-bool-exp)

    (boolean ("true") true->boolean)
    (boolean ("false") false->boolean)

    (oper-un-bool ("not") not-bool-prim)

    (oper-bin-bool ("or")  or-bool-prim)
    (oper-bin-bool ("and") and-bool-prim)

    (pred-prim ("<") less-prim)
    (pred-prim (">") more-prim)
    (pred-prim ("==") equal-prim)
    (pred-prim ("<>") unequal-prim)
    (pred-prim (">=") more-equal-prim)
    (pred-prim ("<=") less-equal-prim)


     ;;Lista
    (expression ("[" (separated-list expression ",") "]") list-exp)


    ;;Tupla
    (expression ("tupla" "[" expression "," expression "]") tupla-exp)


    ;;Registro
    (expression ("{" (separated-list identifier "=" expression ",") "}")  registro-exp)


    ;;Prima
    (expression ("(" expression primitive expression ")") primapp-bin-exp)
    (expression (primitive-un "(" expression ")") primapp-un-exp)

    
    ;;Primitivas aritmeticas para enteros
    (primitive ("+") primitiva-suma)
    (primitive ("~") primitiva-resta)
    (primitive ("*") primitiva-multi)
    (primitive ("/") primitiva-div)
    ;;(primitive ("add1") primitiva-add1)
    ;;(primitive ("sub1") primitiva-sub1)
    
    (primitive ("concat") primitiva-concat)

    
    ;;Primitivas unitarias
    (primitive-un ("longitud") primitiva-longitud)
    (primitive-un ("add1") primitiva-add1)
    (primitive-un ("sub1") primitiva-sub1)
    (primitive-un ("cero") primitiva-cero)
    

    ;;Estructura begin
    (expression ("begin" "{" expression (arbno "," expression) "}" "end")  begin-exp)
    
    
    ;;Estructura if 
    (expression ("if" expression "then" expression "[" "else" expression "]" "end") condicional-exp)
    
    
    ;;Estructura while 
    (expression ("while" expression "do" expression "done") while-exp)



    
    ;;(expression ("declararRec" "(" (separated-list identifier "(" (separated-list identifier ",") ")" "=" expression ";") ")"  "{" expression "}")
               ;;recur-exp)

    
    )
  )

;Construidos automáticamente:

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;*******************************************************************************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> " (lambda (pgm) (eval-program  pgm))(sllgen:make-stream-parser scanner-spec-simple-interpreter grammar-simple-interpreter)))

;*******************************************************************************************
;El Interprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (eval-expression body (init-env)))
      )
    )
  )

; Ambiente inicial
;(define init-env
;  (lambda ()
;    (extend-env
;     '(x y z)
;     '(4 2 5)
;     (empty-env))))
(define init-env
  (lambda ()
    (extend-env '(a) '(2)
     (empty-env)
     )
    )
  )

;eval-expression: <expression> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada
(define eval-expression
  (lambda (exp env)
    (cases expression exp

      (var-exp (id) (apply-env env id))
      
      (variable-exp (vars vals body) (creacion-variable vars vals body env))
      (constante-exp (vars vals body) (creacion-constante vars vals body env))
      
      (txt-exp (text) (creacion-texto text env))
      (lit-exp (num) num)
      
      (boolean-exp (expres-bool) (creacion-bool expres-bool env))            

      (list-exp (list) (creacion-listas list env))

      (tupla-exp (exp1 exp2) (creacion-tuplas exp1 exp2 env))

      (registro-exp  (identificadores registros) (creacion-registros identificadores registros env))

      (primapp-bin-exp (num1 prim num2) (apply-primitive prim (cons (eval-rand num1 env) (cons (eval-rand num2 env) '()))))
      
      (primapp-un-exp (prim num) (apply-primitive-un prim (eval-rand num env)))
      
      (condicional-exp (test-exp true-exp false-exp) (creacion-if test-exp true-exp false-exp env))

      (begin-exp (exp exps) (creacion-begin exp exps env))

      (while-exp (exp-cond exp-do) (creacion-while exp-cond exp-do env))
      
      ;;(recur-exp (procs idss bodies principalBody)
                 ;;(eval-expression principalBody
                              ;;(extend-env-recursively procs idss bodies env))

      )

    )

  )
  
  
; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)

(define prueba
  (lambda (ides rando)
    rando))

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

;apply-primitive: <primitiva> <list-of-expression> -> numero
(define apply-primitive
  (lambda (prim num)
    (cases primitive prim
      (primitiva-suma () (+ (car num) (cadr num)))
      (primitiva-resta () (- (car num) (cadr num)))
      (primitiva-multi () (* (car num) (cadr num)))
      (primitiva-div () (/ (car num) (cadr num)))
      (primitiva-concat () (string-append (car num)(cadr num)))
      )
    ))
    
(define apply-primitive-un
  (lambda (prim num)
    (cases primitive-un prim
      (primitiva-longitud () (string-length num))
      (primitiva-add1 () (+ num 1))
      (primitiva-sub1 () (- num 1))
      (primitiva-cero () (zero? num))
     ))
    )

;true-value?: determina si un valor dado corresponde a un valor booleano falso o verdadero
(define true-value?
  (lambda (x)
    (not (zero? x))))

;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))

                       (env environment?))
  (recursively-extended-env-record (proc-names (list-of symbol?))
                                   (idss (list-of (list-of symbol?)))
                                   (bodies (list-of expression?))
                                   (env environment?)))

(define scheme-value? (lambda (v) #t))

(define-datatype procVal procVal?
  (cerradura
   (lista-ID (list-of symbol?))
   (exp expression?)
   (env environment?)
   )

  )

(define apply-procedure
  (lambda (proc args)
    (cases procVal proc
      (cerradura (ids body env)
               (eval-expression body (extend-env ids args env))))))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (recursively-extended-env-record
     proc-names idss bodies old-env)))

;función que busca un símbolo en un ambiente

(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'empty-env "No binding for ~s" sym))
      (extended-env-record (syms vals old-env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (apply-env old-env sym))))
      (recursively-extended-env-record (proc-names idss bodies old-env)
                                       (let ((pos (list-find-position sym proc-names)))
                                         (if (number? pos)
                                             (cerradura (list-ref idss pos)
                                                      (list-ref bodies pos)
                                                      env)
                                             (apply-env old-env sym)))))))

;****************************************************************************************
;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de unambiente

(define true?
  (lambda (x)
    (equal? x 'true)
  )
)

(define creacion-variable
  (lambda (vars vals body env)
    (eval-expression body (map (lambda (var) (env var)) vars) (creacion-listas vals env) env)
    )
  )

(define creacion-constante
  (lambda (vars vals body env)
    (eval-expression body (map (lambda (var) (env var)) vars) (creacion-listas vals env) env)
    )
  )

(define creacion-texto
  (lambda (txt env)
    (apply string-append (reverse (cdr (reverse (cdr (map string (string->list txt)))))))
    )
  )


(define creacion-listas
  (lambda (expre env)
    (cond
      ((null? expre) empty)
      (else
       (cons (eval-expression (car expre) env) (creacion-listas (cdr expre) env))
       )
      )
    )
  )


(define creacion-tuplas
  (lambda (expre1 expre2 env)
    (cond
      ((null? expre1) empty)
      (else
       (cons (eval-expression expre1 env) (eval-expression expre2 env))
       )
      )
    )
  )


(define creacion-registros
  (lambda (identi expre env)
    (cond
      ((null? expre) empty)
      (else
       (cons (append (list (car identi) '=) (cons (eval-expression (car expre) env) empty)) (creacion-registros (cdr identi) (cdr expre) env))
       )
      )
    )
  )


(define creacion-bool
  (lambda (expression env)
    (cases  expr-bool expression
      (booleano (bool)
                (cases boolean bool
                  (true->boolean () 'true)
                  (false->boolean () 'false)
                  )
                )
      (una-bool-exp (unary-prim bool-exp)
                    (cases oper-un-bool unary-prim
                      (not-bool-prim () (if (true? (eval-expression bool-exp env)) 'false 'true))
                      )
                    )
      (bin-bool-exp (pred first-expr second-expr)
                    (cases oper-bin-bool pred
                      (and-bool-prim () (if (and (true? (eval-expression first-expr env)) (true? (eval-expression second-expr env))) 'true 'false))
                      (or-bool-prim () (if (or  (true? (eval-expression first-expr env)) (true? (eval-expression second-expr env))) 'true 'false))
                      )
                    )
      (pred-bool-exp (pred first-expr second-expr)
                     (cases pred-prim pred
                       (less-prim () (if (< (eval-expression first-expr env) (eval-expression second-expr env)) 'true 'false))
                       (more-prim () (if (> (eval-expression first-expr env) (eval-expression second-expr env)) 'true 'false))
                       (equal-prim () (if (equal? (eval-expression first-expr env) (eval-expression second-expr env)) 'true 'false))
                       (unequal-prim () (if (not (equal? (eval-expression first-expr env) (eval-expression second-expr env))) 'true 'false))
                       (more-equal-prim () (if (>= (eval-expression first-expr env) (eval-expression second-expr env)) 'true 'false))
                       (less-equal-prim () (if (<= (eval-expression first-expr env) (eval-expression second-expr env)) 'true 'false))
                       )
                     )
      )
    )
  )                                                                


(define creacion-begin
  (lambda (exp exps env)
     (if (null? exps) (eval-expression exp env) (begin (eval-expression exp env) (creacion-begin (car exps) (cdr exps) env)))
    )
  )


(define creacion-if
  (lambda (test-exp true-exp false-exp env)
    (if (true? (eval-expression test-exp env)) (eval-expression true-exp env) (eval-expression false-exp env))
    )
  )

(define creacion-while
  (lambda (exp-cond exp-do env)
    (if (true? (eval-expression exp-cond env)) (begin (eval-expression exp-do env) (creacion-while exp-cond exp-do env)) 'done)
    )
  )

(define creacion-for
  (lambda (test-exp true-exp false-exp env)
    (if (true? (eval-expression test-exp env)) (eval-expression true-exp env) (eval-expression false-exp env))
    )
  )



(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)
    )
  )

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f)))
      )
    )
  )

(interpretador)