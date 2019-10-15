; Daniel Moesgaard Andersen
; 20164256
; dand16@student.aau.dk
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Made in racket.
;
; Status of program:
; The students are loaded from the "all-students.lsp" file and saved to a list.
; When a group is created, the group number is consed with the rest of the students
; information, such that it is in the form (groupnumber, id, name, ...)
; It has selectors for all details of a student and for getting members of a group by group id.
; 

#lang racket

;; Load the students file and convert it to a list
(define student-list (car (file->list "all-students.lsp")))

;; Selector function that returns a list of students in a group
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

;; Function that returns the maximum group size in a grouping
(define (maximum-group-size sl)
  (group-size-finder 1 sl > -inf.0)
)

;; Function that returns the minimum group size in a grouping
(define (minimum-group-size sl)
  (group-size-finder 1 sl < +inf.0)
)

;; Constructor function for a single student
(define (make-student id name sex nationality age)
  (list id name sex nationality age)
)

;; Selection functions for a single student
; Group number
(define (group-of-student student)
  (if (= (length student) 6)
      (car student)
      #f
  )
)

; Id 
(define (id-of-student student)
      (if (= (length student) 6)
          (cadr student)
          (car student)
          )
)

; Name
(define (name-of-student student)
      (if (= (length student) 6)
          (caddr student)
          (cadr student)
          )
)

; Sex
(define (sex-of-student student)
  (if (= (length student) 6)
      (cadddr student)
      (caddr student)
  )
)

; Nationality
(define (nationality-of-student student)
  (if (= (length student) 6)
      (car (cddddr student))
      (cadddr student)
  )
)

; Age
(define (age-of-student student)
  (if (= (length student) 6)
      (cadr (cddddr student))
      (car (cddddr student))
  )
)

;; Constructor function for a single group
(define (make-group id students)
  (make-group-helper id (car students) (cdr students))
)

; Helper to append group number to student
(define (make-group-helper id student remaining)
  (if (null? remaining)
      (cons (cons id student) '())
      (cons (cons id student) (make-group-helper id (car remaining) (cdr remaining)))
   )
)

;; Predicate function for a student
(define (student? student)
   (and (list? student)
   (and (string? (id-of-student student)) (string? (name-of-student student))
        (and (and (or (string-ci=? "female" (sex-of-student student)) (string-ci=? "male" (sex-of-student student)))
            (string? (nationality-of-student student)))
             (positive? (age-of-student student)))
        )
   )
)

;; Is student female?
(define (female? student)
  (string-ci=? "female" (sex-of-student student))
)

;; Is student male?
(define (male? student)
  (string-ci=? "male" (sex-of-student student))
)

;; Are all members in same group?
(define (all-in-same-group? group)
  (all-in-same-group-helper group (group-of-student (car group))) ;; call a helper
)

; Helper to check if all members are in same group as the first
(define (all-in-same-group-helper group groupNumber)
  (if (null? group)
      #t
      (if (= (group-of-student (car group)) groupNumber)
          (all-in-same-group-helper (cdr group) groupNumber)
          #f
       )
  )
)

;; Predicate function for a group
(define (is-group? group)
  (if (null? group) #f
     (and
      (andmap student? group)
      (all-in-same-group? group))
  )
)

;; Predicate to check if all group members are female
(define (all-female? group)
  (andmap female? group)
)

;; Predicate function for whether student is in a group
(define (is-in-group? student)
  (if (list? student)
      (if (= (length student) 6)
          #t
          #f
          )
      #f)
)

;; Selector function that returns the group-id (group number)
(define (id-of-group group)
  (if (number? (caar group))
      (caar group)
      #f
   )
)

;; Predicate that identifies a grouping object
(define (is-grouping? grouping)
  (if (null? grouping) #f
     (andmap is-in-group? grouping))
)

;; Pretty printer for students
(define (print-student student)
  (let ([group (group-of-student student)]
        [id (id-of-student student)]
        [name (name-of-student student)]
        [sex  (sex-of-student student)]
        [nationality (nationality-of-student student)]
        [age (age-of-student student)])
    (printf "Group: ~a\nId: ~s\nName: ~v\nSex: ~v\nNationality: ~v\nAge: ~v\n\n"
            group id name sex nationality age)
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
         gl)
)

;; Random grouping
(define (group-random sl gsl)
  (if (= (length sl) (sumList gsl))
      (generate-random-group (shuffle sl) gsl 1 0)
      (error "Error: Wrong amount of students in gsl")
  )
)

;; Helper to generate random group
; sl is students needing to be grouped
; nsl is students who have been grouped
; gn is current group number
(define (generate-random-group sl gsl gn count)
  ; Call function on first element then
  ; recursively call it on the remaining elements, increase group number
  ; when it reaches the wanted group size
  (if (or (null? sl) (null? gsl))
      '()
      (if (= count (car gsl))
          (generate-random-group sl (cdr gsl) (+ gn 1) 0)
          (cons (cons gn (car sl)) (generate-random-group (cdr sl) gsl gn (+ count 1))))
  )
)

;; Helper method to sum list elements if input is good
(define (sumList sum)
  (if (andmap exact-nonnegative-integer? sum)
    (apply + sum)
    (error "Group size contained non-positive integers")
  )
)

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
    ;; Sort first by nationality, then by sex to ensure a fair distribution when counting
  (group-counting (sort (sort sl string<? #:key nationality-of-student) string<? #:key sex-of-student) ga)
)

;; TODO Random grouping with group predicate
;;; At least N students in a group are A years old or older
;;; All students in the group are female
;;; No students in the group are of the same age
(define (group-random-predicate sl gsl predicate)
  (group-random-predicate-helper sl gsl predicate 0)
)

(define (group-random-predicate-helper sl gsl predicate attempts)
  (if (> attempts 1000)
      (error "Could not generate groups from predecate")

      (if (= (length sl) (sumList gsl))
          (let ([generated-group (generate-random-group-pred (shuffle sl) gsl 1 0 0)])
            (if (not (predicate generated-group))
                (group-random-predicate-helper sl gsl predicate (+ attempts 1))
                generated-group
                )
            )
          (error "Error: Wrong amount of students in gsl")
          )
      )
  )

; Helper to generate groups, giving it 1000 attempts
(define (generate-random-group-pred sl gsl gn count attempts)
  ; Call function on first element then
  ; recursively call it on the remaining elements, increase group number
  ; when it reaches the wanted group size
  (if (or (null? sl) (null? gsl))
      '()
      (if (= count (car gsl))
          (generate-random-group-pred sl (cdr gsl) (+ gn 1) 0 (+ attempts 1))
          (cons (cons gn (car sl)) (generate-random-group-pred (cdr sl) gsl gn (+ count 1) attempts))
          )
      )
  )

;; Example groups
(define gc (group-counting student-list 6))
(define gcb (group-count-balanced student-list 5))
(define (stud)
  car (car gc)
)

(define rg (group-random student-list '(50 50 50 50)))

(define john (make-student "15" "John Bob" "MalE" "Spacian" 15))
(define bob (make-student "16" "Bobby Johnson" "Female" "Outer" 19))
(define groupjohn (list 12 "15" "gj" "male" "earthling" 19))
(define jb (make-group 19 (list john bob)))