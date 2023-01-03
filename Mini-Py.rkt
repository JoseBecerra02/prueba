#lang eopl
; Sergio Escudero Tabares - 2040801
; Jose Miguel Becerra Casierra - 2043246
; Natalia Andrea Marín Hernandez - 2041622
; Esteban Andres Hernandez - 2042817
; Juan Esteban Brand Tovar - 2043291
; Link github: https://github.com/JoseBecerra02/Taller3-FLP.git

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
    (expression ("var" (arbno (identifier "=" expression ",")) "in" expression) variable-exp)

    (expression ("const" (arbno (identifier "=" expression ",")) "in" expression) constante-exp)

    

    ;;Identificador 
    (expression (identifier) var-exp)


    ;;Valores
    (expression (number) lit-exp)
    (expression (text) txt-exp)

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

    
    ;;Lista
    (expression ("[" (separated-list expression ";") "]") list-exp)

    
    ;;Características adicionales
    (expression ("if" expression "then" expression "[" "else" expression "]" "end") condicional-exp)

    
   
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
    (extend-env '() '()
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

      (list-exp (list) (creacion-listas list env))

      (primapp-bin-exp (num1 prim num2)
                    (apply-primitive prim (cons (eval-rand num1 env) (cons (eval-rand num2 env) '()))))
      
      (primapp-un-exp (prim num)
                   (apply-primitive-un prim (eval-rand num env)))
      
      (condicional-exp (test-exp true-exp false-exp)
              (if (true-value? (eval-expression test-exp env))
                  (eval-expression true-exp env)
                  (eval-expression false-exp env)))
      
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

(define calcularHexa
  (lambda (base Lista)
    (Lista)
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
 (lambda (exprs env)
  (cond
   ((null? exprs) empty)
   (else
      (cons (eval-expression (car exprs) env) (creacion-listas (cdr exprs) env))
   )
  )
 )
)

                                                                   
  
(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

(interpretador)