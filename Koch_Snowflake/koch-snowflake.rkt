#lang typed/racket

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")

; Use (run INTEGER) to test the program, where the INTEGER is the length of
; one side of the triangle

(: koch-snowflake : Integer -> Image)
;Prinitng the koch-snowflake pattern
(define (koch-snowflake width)
  (cond
    [(< width 3) (line width 0 "black")]
    [else (beside/align "bottom"
                        (koch-snowflake (cast (exact-floor (/ width 3))
                                              Integer))
                        (rotate 60 (koch-snowflake
                                    (cast (exact-floor (/ width 3)) Integer)))
                        (rotate 300 (koch-snowflake
                                     (cast (exact-floor (/ width 3)) Integer)))
                        (koch-snowflake (cast (exact-floor (/ width 3))
                                              Integer)))]))


(: run : Integer -> Image)
; run the world given size of any one side of the triangle
(define (run length)
  (above (koch-snowflake length)
         (beside (rotate 120 (koch-snowflake length))
                 (rotate 240 (koch-snowflake length)))))


;(koch-snowflake 360)
;(run 200)
