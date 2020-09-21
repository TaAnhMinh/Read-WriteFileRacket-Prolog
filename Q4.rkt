#lang racket
(define (list2 A)
  (define B(with-input-from-file A
  (lambda ()
    (let loop ((lines '())
               (next-line (read-line)))
       (if (eof-object? next-line) ; when we hit the end of file
           (reverse lines)         ; return the lines
           (loop (cons next-line lines) ; else loop, keeping this line
                 (read-line)))))))       ; and move to next one
  (define M (removeLine B))
  (define C (removeNew M))
  C
  )

(define (removeLine M)
  (cond
   ((null? M) '())
   (else (let ((C (car M))) (cons(regexp-split #rx"\r" C) (removeLine (cdr M)))))
   ))
  

(define (removeNew A)
  (cond
    ((null? A) '())
    ((equal? (cdr (car A)) '("")) (cons (car(car A)) (removeNew (cdr A))))
    (else (cons (car (car A)) (removeNew (cdr A))))
  )
  )

;;create list containing employer names.
(define (createEmployer EP)
  (cond
    ((null? EP) '())
    (else (cons (list (car(car EP)) "") (createEmployer (cdr EP))))
    ))

(define (writeTableau tb fileOut)
  (if (eqv? tb '())
      #t
      (begin (display-lines-to-file (car tb) fileOut #:separator #\space #:exists 'append)
             (display-to-file #\newline fileOut #:exists 'append)
             (writeTableau (cdr tb) fileOut))))

(define (getL L)
  (cond
    ((null? L) '())
    (else (let ((C (car L))) (cons(regexp-split #rx"," C) (getL (cdr L)))))
    )
  )

(define (has-duplicates? lst) 
  (cond
     [(empty? lst) #f] 
     [(not (not (member (first lst) (rest lst)))) #t]
     [else (has-duplicates? (rest lst)) ]))

(define (getAllTail L)
  (cond
    ((null? L) '())
    ((null? (car L)) '())
    (else (cons (car(cdr(car L))) (getAllTail (cdr L))))))

(define (index L E)
  (cond
    ((equal? (car L) E) 0)
    (else (+ 1 (index (cdr L) E)))))

(define (pref EP)
  (cond
    ((null? EP) '())
    (else (cons (cdr(car EP)) (pref (cdr EP))))
  ))

(define (duplicateTORF L)
  (set! L (getAllTail L))
  (has-duplicates? L)
  )

(define delete
  (lambda (item list)
    (cond
      ((null? list) '())
     ((equal? item (car list)) (cdr list))
     (else (cons (car list) (delete item (cdr list)))))))

(define (findStableMatch EF SF)
  (define e(list2 EF))
  (define s(list2 SF))
  (define EL(getL e))
  (define SL(getL s))
  (define L (stableMatching EL SL))
  (if (equal? (length EL) 3)
      (write-to-a-file L "matches_scheme_3x3.csv")
      (write-to-a-file L "matches_scheme_10x10.csv"))
  )

(define (length lst)
  (cond
    [(empty? lst)  0]
    [(cons? lst)   (+ 1 (length (rest lst)))]))      

(define (stableMatching EP SP)
  (define E (createEmployer EP)) ;;create employer E = '(("Thales" "") ("C P" "") ("C" ""))
  (define S (createEmployer SP)) ;;create student S.
  (define FL (stableMatching2 EP SP E S))
  FL
  )

(define (stableMatching2 EP SP E S)
  (if (duplicateTORF (match EP SP E S))
      (stableMatching2 (removeUsed (match EP SP E S) EP) SP E S);#t -> need to repeat the process
      (match EP SP E S));#f -> reach final answer
  )

(define (removeUsed L EP)
  (cond
    ((null? L) '())
    (else (cons (delete (car (cdr (car L))) (car EP)) (removeUsed (cdr L) (cdr EP))))
  ))

(define (match EP SP E S)
  (cond
    ((null? E) '())
 (else (cons (offer (car E) S EP SP) ;find student and offer the job)
             (match EP SP (cdr E) (update (offer (car E) S EP SP) S))) ;recursion, find student for the rest of the employers with updated student list
  ))
 )
(define (update L SL)
  (cond
    ((null? SL) '())
    ((equal? (car(cdr L)) (car (car SL))) (cons (list (car(cdr L)) (car L)) (update L (cdr SL))))
    (else (cons (list (car (car SL)) (car(cdr(car SL)))) (update L (cdr SL)))))
  )

(define (offer E1 S EP SP) ;E1 is Ex: ("Thales" "") (car E1) = "T"
    (if (equal? (cdr E1) '("")) ;if "" --> e is unmatched
        (evaluate (car E1) (findStudent EP E1) S SP)   ;if true then find the most prefered student on the list to whom the employer has not yet offer a job and evaluate
         E1);if already matched then do nothing and return the employer
 )

(define (evaluate E SN SL SP) ;E is only the name of the employer Ex: "T"
  (if (equal? SN (car (car SL))) ;compare "T" with "T" correct comparisons
      (if (equal? (cdr (car SL)) '("")) ; #t -> check if S is unmatched (correct comparison)
          (matches E (car (car SL))) ; #t -> create and return list (E S) (correct varaiable)
          (if (perferOver E (cadar SL) (car (car SL)) SP) ;#f -> check if student prefer new more than old. (correct variable
              (matches E (car (car SL)));#t new more favarable than old then create match between new and student
              (list E "")) ;#f
          ) 
      (evaluate E SN (cdr SL) SP));#f
  )

(define (perferOver NE OE SN SP)
  (if (equal? SN (car (car SP)))
      (if (< (index (cdr(car SP)) NE) (index (cdr(car SP)) OE));#t -> check if index of new employer is larger than old employer
          #t ;the new employer is more favarable
          #f)
      (perferOver NE OE SN (cdr SP))) ;#f
  )

(define (findStudent EP E1) ;this will return "T", not '("T")
  (cond
    ((null? EP) "") ;if cannot find the employer (never happen), just incase if something wrong with the program. return "" to indicate
    ((equal? (car E1) (car (car EP))) (car (cdr (car EP)))) ;this get the first element on the list
    (else (findStudent (cdr EP) E1)))
  )

(define (matches E  S) (list E S))

(require racket/trace)
(require csv-writing)

(define (write-to-a-file lst path)
  (call-with-output-file path
    (lambda (output-port)
      (display-table lst output-port))
    #:exists 'replace))











         