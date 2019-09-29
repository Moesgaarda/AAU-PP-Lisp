 #lang racket
(require racket/pretty)

;; Load the students file and convert it to a list
(define student-list (car (file->list "all-students.lsp")))

;; Helper method to find every nth student
(define (every-nth-student lst n)
  (if (null? lst)
      '()
      (cons (car lst) (every-nth-student (my-list-tail lst n) n))
   )
)


;; Own implementation of list-tail that does not throw an error if index is
;; larger than elements left.
(define (my-list-tail list amount)
  (if (or (= amount 0) (not (pair? list)))
        list
       (my-list-tail (cdr list) (- amount 1))
  )
)

;; Helper method to return the first n students of a list
(define (list-prefix lst n)
  (if (= n 0)
      '()
      (cons (car lst) (list-prefix (cdr lst) (- n 1)))
  )
)


;; Function that returns a given group from a grouping
(define (get-group-members sl groupNumber)
  (if (null? sl)
      '()
  (if (equal? (car (car sl)) groupNumber)
      (cons (car sl) (get-group-members (cdr sl) groupNumber))
      (get-group-members (cdr sl) groupNumber)
      )
  )
)

;; Function that returns the number of groups in a grouping

;; Function that returns the maximum group size in a grouping

;; Function that returns the minimum group size in a grouping



;; Constructor function for a single student

;; Predicate function for a single student

;; Selection function for a single student

;; Constructor function for a single group

;; Predicate function for a group

;; Selector function that returns a list of students in a group

;; Selector function that returns the group-id (group number)

;; Predicate that identifies a grouping object

;; Pretty printer for students

;; Pretty printer for groups
(define (pp-group groupList)
  (for ([i groupList])
    (pretty-print (caddr i)))
 )
  

;; Pretty printer for groupings

;; Random grouping
(define (group-random sl gsl)
    (random-group-constructor (shuffle sl) gsl 1))

;; Helper method to randomly group students
(define (random-group-constructor sl gsl count)
  (if (null? sl)
      '()
  (if (>= (length sl) gsl)
      (cons (cons count (list-prefix sl gsl)) (random-group-constructor (list-tail sl gsl) gsl (+ 1 count)))
      (cons (cons count (list-prefix sl (length sl))) (random-group-constructor (list-tail '() 0) gsl 1))
      )
  )
)

(define rg (group-random student-list 5))

;; Grouping by counting
; sl is a list of students
; gsl is the desired group size
(define (group-counting sl gsl)
    (count-group-constructor sl gsl 1)
)

(define (count-group-constructor sl gsl count)
  (if (null? sl)
      '()
  (if (< count gsl)
      (cons (cons count (car sl)) (count-group-constructor (cdr sl) gsl (+ count 1)))
      (cons (cons count (car sl)) (count-group-constructor sl gsl 1))
      )
  )
)
;; Example group
(define gc (group-counting student-list 5))

;; Balanced grouping by counting

;; Random grouping with group predicate
;;; At least N students in a group are A years old or older
;;; All students in the group are female
;;; No students in the group are of the same age