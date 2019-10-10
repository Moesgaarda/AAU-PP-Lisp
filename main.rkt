#lang racket

;; Load the students file and convert it to a list
(define student-list (car (file->list "all-students.lsp")))

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
(define (groups-in-grouping sl)
  (length (unique-groups sl)))

;; Helper method to find unique group numbers
(define (unique-groups sl)
  (if (null? sl)
      '()
      (cons (car sl)
            ;; Go through elements and filter all other groups
            ;; with the same ID.
            (unique-groups
             (filter
              (lambda (x) (not (equal? (car x) (caar sl)))) 
             (cdr sl))))
  )
)

;; Function that returns the maximum group size in a grouping
(define (maximum-group-size sl)
  (group-size-finder 1 sl > -inf.0))

;; Helper method that compares each group size based on cmp function input
(define (group-size-finder currentGroupId sl cmp currentBest)
  (if (null? (get-group-members sl currentGroupId))
      currentBest
      (if (cmp (length (get-group-members sl currentGroupId)) currentBest)
          (group-size-finder (+ currentGroupId 1) sl cmp (length (get-group-members sl currentGroupId)))
          (group-size-finder (+ currentGroupId 1) sl cmp currentBest)
      )
  )
)

;; Function that returns the minimum group size in a grouping
(define (minimum-group-size sl)
  (group-size-finder 1 sl < +inf.0))

;; Constructor function for a single student

;; Predicate function for a single student

;; Selection function for a single student

;; Constructor function for a single group

;; Predicate function for a group

;; Selector function that returns a list of students in a group

;; Selector function that returns the group-id (group number)

;; Predicate that identifies a grouping object

;; Pretty printer for students
(define (print-student student)
  (let ([group (car student)]
        [id (cadr student)]
        [name (caddr student)]
        [sex  (cadddr student)]
        [nationality (car (cddddr (stud)))])
    (printf "Group: ~a\nId: ~s\nName: ~v\nSex: ~v\nNationality: ~v\n\n"
            group id name sex nationality)
  )
)

;; Pretty printer for groups
(define (print-group gl)
  (for-each (lambda (student)
         (print-student student))
         gl)
)

;; Pretty printer for groupings
(define (print-grouping gl)
  (for-each (lambda (student)
         (print-student student))
         gl))

;; Random grouping


;; Grouping by counting
; sl is a list of students
; gsl is the desired amount of groups
(define (group-counting sl ga)
    (count-group-constructor sl ga 1)
)

(define (count-group-constructor sl ga count)
  (if (null? sl)
      '()
  (if (< count ga)
      (cons (cons count (car sl)) (count-group-constructor (cdr sl) ga (+ count 1)))
      (cons (cons count (car sl)) (count-group-constructor (cdr sl) ga 1))
      )
  )
)

;; Balanced grouping by counting
;; sl is student list
;; ga is the desired amount of groups
(define (group-count-balanced sl ga)
  (let ([sex caddr]
        [nationality cadddr])
    ;; Sort first by sex, then by nationality to ensure a fair distribution when counting
  (group-counting (sort (sort sl string<? #:key sex) string<? #:key nationality) ga))
)

;; Random grouping with group predicate
;;; At least N students in a group are A years old or older
;;; All students in the group are female
;;; No students in the group are of the same age



;; Example groups
(define gc (group-counting student-list 6))
(define gcb (group-count-balanced student-list 5))
(define (stud)
  car (car gc)
)