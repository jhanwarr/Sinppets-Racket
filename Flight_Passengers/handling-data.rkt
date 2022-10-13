#lang typed/racket

(require "../include/cs151-core.rkt")
(require typed/test-engine/racket-tests)

(define-type Group (U 'A 'B 'C))

(define-struct Passenger
  ([name : String]
   [group : Group]
   [position : Integer]
   [family? : Boolean]
   [upgraded? : Boolean]))

(: A Passenger)
(: B Passenger)
(: C Passenger)
(: D Passenger)
(: E Passenger)
(: F Passenger)
(: G Passenger)
(: H Passenger)
(: I Passenger)
(: J Passenger)
(: K Passenger)
(: L Passenger)
(: M Passenger)
(: N Passenger)
(: O Passenger)

(: l (Listof Passenger))
(: a (Listof Passenger))
(: b (Listof Passenger))
(: c (Listof Passenger))
(: d (Listof Passenger))


(define A (Passenger "A" 'A 20 #t #t))
(define B (Passenger "B" 'A 20 #t #t))
(define C (Passenger "C" 'A 10 #t #t))
(define D (Passenger "D" 'A 30 #t #t))
(define E (Passenger "E" 'A 20 #f #t))
(define F (Passenger "F" 'A 20 #t #f))
(define G (Passenger "G" 'A 20 #f #f))
(define H (Passenger "H" 'A 10 #f #f))
(define I (Passenger "I" 'A 30 #f #f))
(define J (Passenger "J" 'B 20 #t #t))
(define K (Passenger "K" 'B 20 #t #f))
(define L (Passenger "L" 'B 20 #f #t))
(define M (Passenger "M" 'B 30 #f #f))
(define N (Passenger "N" 'B 10 #f #f))
(define O (Passenger "O" 'C 20 #t #t))

(define l (list A B C D E F G H I J K L M N O))
(define a (list A B C D E F G H I))
(define b (list J K L M N))
(define c (list O))
(define d '())

;; ========== Priority =========

(: check-pos : Integer Integer -> Boolean)
;; comparing two passengers to determine whether the first passenger will board
;; before the second by criterias other than upgraded and group
(define (check-pos p1 p2)
  (cond
    [(< p1 p2) #t]
    [(> p1 p2) #f]
    [else #f]))


(: check-group : Passenger Passenger -> Boolean)
;; comparing two passengers to determine whether the first passenger will
;; board before the second by criterias other than upgraded
(define (check-group pa1 pa2)
  (match* (pa1 pa2)
    [((Passenger n1 g1 p1 f1 u1) (Passenger n2 g2 p2 f2 u2))
     (cond
       [(and (symbol=? g1 'A) (symbol=? g1 g2)) (check-pos p1 p2)]
       [(symbol=? g1 'A) #t]
       [(symbol=? g2 'A) #f]
       [(and (not u1) f1 (not f2)) #t]
       [(and (not u1) (not f1) f2) #f]
       [(symbol<? g1 g2) #t]
       [(symbol=? g1 g2) (check-pos p1 p2)]
       [else #f])]))


(: passenger<? : Passenger Passenger -> Boolean)
;; comparing two passengers to determine whether the first 
;; passenger will board before the second or not
(define (passenger<? pa1 pa2)
  (match* (pa1 pa2)
    [((Passenger n1 g1 p1 f1 u1) (Passenger n2 g2 p2 f2 u2))
     (cond
       [(and u1 u2) (check-group pa1 pa2)]
       [u1 #t]
       [u2 #f]
       [else (check-group pa1 pa2)])]))


(check-expect (passenger<? A B) #f)
(check-expect (passenger<? A C) #f)
(check-expect (passenger<? A D) #t)
(check-expect (passenger<? E A) #f)
(check-expect (passenger<? F A) #f)
(check-expect (passenger<? G A) #f)
(check-expect (passenger<? H A) #f)
(check-expect (passenger<? I A) #f)
(check-expect (passenger<? J A) #f)
(check-expect (passenger<? J K) #t)
(check-expect (passenger<? K L) #f)
(check-expect (passenger<? L M) #t)
(check-expect (passenger<? M N) #f)
(check-expect (passenger<? N O) #f)
(check-expect (passenger<? A O) #t)

;; ========== Sorting =========

(: insert : Passenger (Listof Passenger) -> (Listof Passenger))
; Inserting an integer in a sorted list of integers 
; in such a way that the list remains sorted
; Precondition - The list must be SORTED
(define (insert p l)
  (match l
    ['() (list p)]
    [(cons head tail) (if (passenger<? p head)
                          (cons p (cons head tail)) 
                          (cons head (insert p tail)))]))

(check-expect (insert C (list E B A D)) (list C E B A D))
(check-expect (insert C (list B A D)) (list C B A D))
(check-expect (insert O (list L J H F M)) (list L J O H F M))
(check-expect (insert M (list L J O H F)) (list L J O H F M))
(check-expect (insert D '()) (list D))


(: insertion-sort-passengers : (Listof Passenger) -> (Listof Passenger))
;; Sorting a list of passengers using insertion sort
(define (insertion-sort-passengers l)
  (foldr insert '() l))

(check-expect (insertion-sort-passengers l)
              (list C E B A D L J O H G F I K N M))

(check-expect (insertion-sort-passengers a) (list C E B A D H G F I))
(check-expect (insertion-sort-passengers b) (list L J K N M))
(check-expect (insertion-sort-passengers c) (list O))
(check-expect (insertion-sort-passengers d) '())


(: passenger=? : Passenger Passenger -> Boolean)
;; Checking whether the two passengers have the same group, position and
;; upgrade or not
(define (passenger=? pa1 pa2)
  (match* (pa1 pa2)
    [((Passenger n1 g1 p1 f1 u1) (Passenger n2 g2 p2 f2 u2))
     (and (symbol=? g1 g2) (= p1 p2) (boolean=? u1 u2))]))


(check-expect (passenger=? A B) #t)
(check-expect (passenger=? A E) #t)
(check-expect (passenger=? E B) #t)
(check-expect (passenger=? F G) #t)
(check-expect (passenger=? A F) #f)



(: quicksort-passengers : (Listof Passenger) -> (Listof Passenger))
;; Sorting a list of passengers using quicksort
(define (quicksort-passengers l)
  (match l
    ['() '()]
    [(cons pivot tail)
     (local
       {(define smaller (filter (lambda ([x : Passenger])
                                  (or (passenger<? x pivot)
                                      (passenger=? x pivot)))
                                tail))
        
        (define larger (filter (lambda
                                   ([x : Passenger])
                                 (not (or (passenger<? x pivot)
                                          (passenger=? x pivot))))
                               tail))}
       
       (append (quicksort-passengers smaller)
               (list pivot)
               (quicksort-passengers larger)))]))


(check-expect (quicksort-passengers l)
              (list C E B A D L J O H G F I K N M))

(check-expect (quicksort-passengers a) (list C E B A D H G F I))
(check-expect (quicksort-passengers b) (list L J K N M))
(check-expect (quicksort-passengers c) (list O))
(check-expect (insertion-sort-passengers d) '())


(: take : All (A) Integer (Listof A) -> (Listof A))
; returning a list of the first half elements of the given list
(define (take half l)
  (if (= 0 half) '() (match l
                       ['() '()]
                       [(cons head tail) (cons head (take (- half 1) tail))])))


(: drop : All (A) Integer (Listof A) -> (Listof A))
; dropping the first half elements of the given list
; returning the processed list
(define (drop half l)
  (if (= 0 half) l (match l
                     [(cons head tail) (drop (- half 1) tail)])))

(check-expect (take 2 (list 1 2 3 4)) (list 1 2))
(check-expect (drop 2 (list 1 2 3 4)) (list 3 4))
(check-expect (take 3 (list 1 2 3 4 5 6 7)) (list 1 2 3))
(check-expect (drop 3 (list 1 2 3 4 5 6 7)) (list 4 5 6 7))
(check-expect (drop 3 (list A B C D E F G)) (list D E F G))



(: merge : (Listof Passenger) (Listof Passenger) -> (Listof Passenger))
; merging the two sorted lists of integers
; Precondition – the input lists must already be sorted
(define (merge l1 l2)
  (match* (l1 l2)
    [('() '()) '()]
    ;this line is redundant...its just for better understanding
    ;the program will work even without this
    [('() _) l2]
    [(_ '()) l1]
    [((cons h1 t1) (cons h2 t2)) (if (passenger<? h1 h2)
                                     (cons h1 (merge t1 l2))
                                     (cons h2 (merge l1 t2)))]))

(check-expect (merge '() '()) '())
(check-expect (merge '() (list E)) (list E))
(check-expect (merge (list C A N) '()) (list C A N))
(check-expect (merge (list O) (list D G)) (list D O G))
(check-expect (merge (list C A N) (list E)) (list C E A N))



(: merge-sort-passengers : (Listof Passenger) -> (Listof Passenger))
;; Sorting the list of passengers using merge sort
(define (merge-sort-passengers l)
  (match l
    ['() '()] 
    [(list _) l]
    [_ (local
         {(define half (quotient (length l) 2))
          (define left (take half l))
          (define right (drop half l))}
         (merge (merge-sort-passengers left) (merge-sort-passengers right)))]))


(check-expect (merge-sort-passengers l)
              (list C E B A D L J O H G F I K N M))

(check-expect (merge-sort-passengers a) (list C E B A D H G F I))
(check-expect (merge-sort-passengers b) (list L J K N M))
(check-expect (merge-sort-passengers c) (list O))
(check-expect (merge-sort-passengers d) '())


;; ========== Analyze (Optional) =========

(: random-passenger : -> Passenger)
;; Generating a random passenger
(define (random-passenger)
  (Passenger (number->string (random 0 2000000000))
             (match (random 0 3) [0 'A] [1 'B] [2 'C])
             (random -2000000000 2000000000)
             (= (random 0 4) 0)
             (= (random 0 4) 0)))

(: random-passenger-list : Integer -> (Listof Passenger))
;; Generating a list of n random passengers
(define (random-passenger-list n)
  (build-list n (lambda ([k : Integer]) (random-passenger))))

(: eat : All (A) A -> 'yum)
;; Taking any value and returns 'yum.
(define (eat a) 'yum)

(: my-passenger-list : (Listof Passenger))
(define my-passenger-list (random-passenger-list 1000))

(time (eat (insertion-sort-passengers my-passenger-list)))
(time (eat (quicksort-passengers my-passenger-list)))
(time (eat (merge-sort-passengers my-passenger-list)))

(test)
