; Name: Mrigank Doshy
; PSU ID: 911428894
; CMPSC 461: Project 2


; Question 1
(define (dncall n f x)
  (cond ((equal? n 0) x)
        ((equal? n 1) (f (f x)))
        (else (dncall (- n 1) f (f (f x))))))
(define (add-one x)
  (+ x 1))

;;; Tests
(dncall 2 add-one 2)



; Question 2
(define (f x)
  (if (> x 3) #t #f))
(define (keep-if f l)
  (if (null? l) l
   (if (equal? #f (f (car l))) (keep-if f (cdr l))
       (cons (car l) (keep-if f (cdr l))))))

;;; Tests
(keep-if f '(10 1 7 2))



; Question 3
(define (least_helper k x)
  (cond ((null? x) k)
        ((equal? k (car x)) (least_helper k (cdr x)))
        ((< k (car x)) (least_helper k (cdr x)))
        ((> k (car x)) (least_helper (car x) (cdr x)))))
(define(least x)
  (cond ((null? x) 'empty)
        ((least_helper (car x) (cdr x)))))

;;; Tests
(least '(7 3 6 2))
(least_helper 5 '(4 5 6))



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

;;; Tests
(to-words 13)
(to-words -55)
(to-words 0)
(to-words 120)



; Question 5

;; Part (a)
(define (member? a l)
  (cond ((null? l) #f)
        ((equal? a (car l)) #t)
        (else (member? a (cdr l)))))

(define (filterWords wordList irrelevantWordList)
  (cond ((null? wordList) wordList)
    ((member? (car wordList) irrelevantWordList) (filterWords (cdr wordList) irrelevantWordList))
    (else (cons (car wordList) (filterWords (cdr wordList) irrelevantWordList)))))

;; Part (b)
(define (iniWordCountList wordList)
  (if (null? wordList) '()
      (cons (cons (car wordList) '(1)) (iniWordCountList (cdr wordList)))))

;; Part (c)
(define (mergeWordCounts wordCountPair wordCountList)
  (cond ((null?  wordCountList)  wordCountList)
        ((member? wordCountPair  wordCountList)
         (cond ((equal? wordCountPair (car  wordCountList)) (mergeWordCounts (list (car wordCountPair) (+ (car (cdr wordCountPair)) 1)) (cdr  wordCountList)))
               (else (cons (car  wordCountList) (mergeWordCounts wordCountPair (cdr  wordCountList))))))
        (else (append wordCountList (list  wordCountPair)))))

;; Part (d)
(define (reduce f l v)
  (if (null? l) v
      (f (car l) (reduce f (cdr l) v))))
(define (mergeByWord wordCountList)
  (cond ((null? wordCountList) wordCountList)
        (else (reduce mergeWordCounts (cdr wordCountList) (list (car wordCountList))))))

;; Part (e)
(define (relevantWordCount  wordList irrelevantWordList)
  (mergeByWord (iniWordCountList (filterWords  wordList irrelevantWordList))))

;;; Tests
(filterWords '(time is long but life is short) '(but))
(iniWordCountList '(time is long life is short))
(mergeWordCounts '(is 1) '((time 1) (is 1)))
(mergeWordCounts '(life 1) '((time 1) (is 2)))
(mergeByWord '((time 1) (is 1) (long 1) (but 1) (life 1) (is 1) (short 1)))