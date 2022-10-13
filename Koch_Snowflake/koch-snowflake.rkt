#lang typed/racket

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")

(: koch-snowflake : Integer -> Image)
;Prinitng the koch-snowflake pattern
(define (koch-snowflake width)
  (cond
    [(< width 3) (line width 0 "purple")]
    [else (beside/align "bottom"
                        (koch-snowflake (cast (exact-floor (/ width 3))
                                              Integer))
                        (rotate 60 (koch-snowflake
                                    (cast (exact-floor (/ width 3)) Integer)))
                        (rotate 300 (koch-snowflake
                                     (cast (exact-floor (/ width 3)) Integer)))
                        (koch-snowflake (cast (exact-floor (/ width 3))
                                              Integer)))]))


(koch-snowflake 45)
(koch-snowflake 90)
(koch-snowflake 180)
(koch-snowflake 200)
(koch-snowflake 360)
