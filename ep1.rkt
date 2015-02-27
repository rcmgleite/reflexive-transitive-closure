; -------------- EP1 Lógica computacional --------------
; Grupo: 
;   Lucas Soriani
;   Rafael Camargo Leite

#lang racket

;######################################################################################
;######################################################################################
;####################################### EX 1 #########################################
;######################################################################################
;######################################################################################
;
; Construa uma função em Scheme com um único argumento que permita percorrer
; recursivamente um conjunto, recebido como uma lista de pares ordenados (o conjunto
; é uma lista cujos elementos são listas com dois elementos ou dotted pairs),
; e, a cada recursão, retorne um dos elementos do conjunto.
(define (traverse-pairs-list _list)
  (if (null? _list)
      null ; case _list is empty, terminate
      (display-and-reccall _list)
    )
)

; Aux function. Displays the parcial result and execute de recursive call
(define display-and-reccall
  (lambda (_list)
    (display (car _list))
    (traverse-pairs-list (cdr _list))
  )
)

;######################################################################################
;######################################################################################
;####################################### EX 2 #########################################
;######################################################################################
;######################################################################################
;
; Construa uma função em Scheme com dois argumentos que permita percorrer recursivamente
; um conjunto de pares ordenados representando uma relação binária
; (1o argumento) procurando pelos pares ordenados da relação que comecem com
; o segundo argumento. Os pares ordenados encontrados devem ser agrupados em
; uma lista e retornados ao final da função.
(define select-pairs-by-key
  (lambda (_list selector)
    (if (null? _list)
        null
        (if (= selector (car (car _list)))
            (cons (car _list) (select-pairs-by-key (cdr _list) selector))
            (select-pairs-by-key (cdr _list) selector)
        )
    )
  )
)

;######################################################################################
;######################################################################################
;####################################### EX 3 #########################################
;######################################################################################
;######################################################################################
;
; Construa um sistema em Scheme que implemente o algoritmo de fecho solicitado.
; O seu sistema deverá utilizar as funções desenvolvidas anteriormente.

; i) - Fecho reflexivo:
;    Exemplo:
;    R = { 〈1, 2〉, 〈1, 5〉, 〈2, 3〉, 〈3, 4〉 } -> { 〈1, 1〉, 〈1, 2〉, 〈1, 5〉, 〈2, 2〉, 〈2, 3〉, 〈3, 3〉, 〈3, 4〉, 〈4, 4〉, 〈5, 5〉 }
(define (reflexive-closure _relation _set)
  (if (null? _set)
      _relation
      (reflexive-closure-aux _relation _set)
  )
)

(define (reflexive-closure-aux _relation _set)
  (define curr-pair (cons (car _set) (car _set)))
  (reflexive-closure (add-pair-to-list _relation curr-pair) (cdr _set))
)

; ii) - Fecho transitivo:
;     Exemplo:
;     R = { 〈1, 2〉, 〈1, 5〉, 〈2, 3〉, 〈3, 4〉 } ->{ 〈1, 2〉, 〈1, 3〉, 〈1, 4〉, 〈1, 5〉, 〈2, 3〉, 〈2, 4〉, 〈3, 4〉 }
(define (transitive-closure _relation)
  (letrec ([transitive-closure-rec
            (lambda (relation-iterator return-list)
              
              (if (null? relation-iterator)
                  return-list
                  (let ([inside-func
                         (lambda (_return-list)
                           (define fi (car (car relation-iterator)))
                           (define si (cdr (car relation-iterator)))
                           (transitive-closure-rec (cdr relation-iterator) (add-related-pairs fi si _relation _return-list))
                         )
                        ])
                    (inside-func return-list)
                  )
              )
            )
           ]
          )
    (transitive-closure-rec _relation null)
  )
)

(define (add-related-pairs first-index second-index _relation curr-list)
  (do-add first-index (select-pairs-by-key _relation second-index) curr-list)
)

(define (do-add first-index selected-pairs return-list)
  (if (null? selected-pairs)
      return-list
      (do-add first-index (cdr selected-pairs) (add-pair-to-list return-list (cons first-index (cdr (car selected-pairs)))))
  )
)

; iii) fecho reflexivo e transitivo
; Apenas chama as duas funções anteriormente criadas e junta os dois sets resultantes
(define (reflexive-transitive-closure _relation _set)
  (append (reflexive-closure _relation _set) (transitive-closure _relation))
)

; ###################################################################################
; ################################# Aux functions ###################################
; ###################################################################################
;
;  Function that adds a pair to list only if its not present yet
;
(define (add-pair-to-list _list _pair)
  (if (null? _list)
      (list _pair)
      (if (member _pair _list)
          _list
          (append _list (list _pair))
      )
  )
)
; Just for tests
(define t (list (cons 1 2) (cons 1 5) (cons 2 3) (cons 3 4)))