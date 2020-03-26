; Name: Mrigank Doshy
; PSU ID: 911428894
; CMPSC 461: Project 2


; Question 1


; Question 2
(define (f x)
  (if (> x 3) #t #f))
(define (keep-if f l)
  (if (null? l) l
   (if (equal? #f (f (car l))) (keep-if f (cdr l))
       (cons (car l) (keep-if f (cdr l))))))


(keep-if f '(10 1 7 2))



  


