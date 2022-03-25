#lang play
(define (point x y)
  (lambda (z)
    (if (eq? z 'getX)
        x
        (if (eq? z 'getY)
            y
            (void)))))