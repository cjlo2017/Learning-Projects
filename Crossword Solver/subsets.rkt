;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname subsets) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (subsets1 lst)
  (cond [(empty? lst) empty]
        [(empty? (rest lst)) (list empty (first lst))]
        [(empty? (rest (rest lst)))
         (list empty
               (list (first lst))
               (list (second lst))
               (list (first lst)(second lst)))]
        [else (append (subsets1 (rest lst))
                      (map (lambda (x)
                             (cons (first lst) x))
                           (subsets1 (rest lst))))]))
(check-expect (subsets1 '(1 2 3)) (list '() (list 2) (list 3) (list 2 3) (list 1) (list 1 2) (list 1 3) (list 1 2 3)))


