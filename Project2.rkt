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


; Question 3



; Question 4
(define (to-words n)
(define OneToNineteen '(one two three four five six seven eight nine ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen))
(define TenMultiples '(twenty thirty forty fifty sixty seventy eighty ninety))
(define hundreds '(hundred ))

(cond
  ((= n 0) '(zero))
  (else
   (let loop ((n n) (units hundreds) (res '()))
     (cond
       
       ((< n 0) (append '(negative) (to-words(abs n))))
       
       ((= n 0) res)
       
       ((< 0 n 20) (cons (list-ref OneToNineteen (- n 1)) res))
       ((< n 100) (cons (list-ref TenMultiples (- (quotient n 10) 2))
                        (loop (remainder n 10) '() res)))
      
       (else
        '(error)))))))


(to-words 13)
(to-words -55)
(to-words 0)
(to-words 120)


; Question 5

;Part (a)
(define (member? a l)
  (cond ((null? l) #f)
        ((equal? a (car l)) #t)
        (else (member? a (cdr l)))))

(define (filterWords lst i)
  (cond ((null? lst) lst)
    ((member? (car lst) i) (filterWords (cdr lst) i))
    (else (cons (car lst) (filterWords (cdr lst) i)))))

;Part (b)
(define (iniWordCountList lst)
  (if (null? lst) '()
      (cons (cons (car lst) '(1)) (iniWordCountList (cdr lst)))))

;Part (c)
(define (mergeWordCounts pair lst)
  (cond ((null? lst) lst)
        ((member? pair lst) (list (car lst) (list (car pair) (+ (car (cdr pair)) 1))))
        (else (append lst (list pair)))))

;Part (d)
(define (reduce f l v)
  (if (null? l) v
      (f (car l) (reduce f (cdr l) v))))
(define mergeByWord 

  

;Part (e)



(filterWords '(time is long but life is short) '(but))
(iniWordCountList '(time is long life is short))
(mergeWordCounts '(is 1) '((time 1) (is 1)))
(mergeWordCounts '(life 1) '((time 1) (is 2)))

  


