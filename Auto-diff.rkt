#lang racket

(require 
  racket/match
  (except-in racket + - * / cos sin tan)
  (prefix-in old (only-in racket + - * / cos sin tan)))

;; Heavy inspiration for this program comes from:
;; Functional Differentiation of Computer Programs, Jerzy Karczmarczuk, University of Caen, 2000

(define-struct dual (x dx))
(define (dCst z)
  (dual z 0))
(define (dVar z)
  (dual z 1))

(define (dualize z y)
  (cond
    [(and (dual? z) (number? y)) (values z (dual y 0))]
    [(and (dual? y) (number? z)) (values (dual z 0) y)]
    [else (values z y)]))

(define (+ y z)
  (let-values ([(y z) (dualize y z)])
    (cond
      [(dual? y)
       (dual (old+ (dual-x y) (dual-x z))
            (old+ (dual-dx y) (dual-dx z)))]
      [else (old+ y z)])))

(dual-dx ((lambda (x) (+ x x)) (dVar 2)))

(define (- y z)
  (let-values ([(y z) (dualize y z)])
    (cond
      [(dual? y)
       (dual (old- (dual-x y) (dual-x z))
            (old- (dual-dx y) (dual-dx z)))]
      [else (old- y z)])))

;; (d/dx (x + x - 1)) = 1 + 1 = 2
;; So the following evaluates to 2.
(dual-dx ((lambda (x) (+ x (- x 1))) (dVar 0)))

(define (* y z)
  (let-values ([(y z) (dualize y z)])
    (cond
      [(dual? y)
       (dual (old* (dual-x y) (dual-x z))
             (old+
              (old* (dual-x y) (dual-dx z))
              (old* (dual-dx y) (dual-x z))))]
      [else (old* y z)])))

;; (d/dx (x * (x - 1))) = (d/dx ((x^2) - x)) = 2x - 1
;; The following should evaluate to 3, and it does.
(dual-dx ((lambda (x) (* x (- x 1))) (dVar 2)))

(define (/ y z)
  (let-values ([(y z) (dualize y z)])
    (cond
      [(dual? y)
       (dual (old/ (dual-x y) (dual-x z))
             (old/
              (old-
               (old* (dual-dx y) (dual-dx z))
               (old* (dual-x y) (dual-dx z)))
              (old* (dual-x y) (dual-x z))))]
      [else (old/ y z)])))

(define (cos x)
  (cond
    [(dual? x)
     (dual (oldcos (dual-x x))
           (old- 0 (oldsin (dual-dx x))))]
    [else (old- 0 (oldsin x))]))

(define (sin x)
  (cond
    [(dual? x)
     (dual (oldsin (dual-x x))
           (oldcos (dual-dx x)))]
    [else (oldcos x)]))

(define (tan x)
  (cond
    [(dual? x)
     (dual (oldtan (dual-x x))
           (old/ 1 (old* (oldcos (dual-dx x)) (oldcos (dual-dx x)))))]
    [else (old/ 1 (old* (oldcos x) (oldcos x)))]))

;; 
