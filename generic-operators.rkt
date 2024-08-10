#lang sicp

; ------------------------------INTEGERS-PACKAGE--------------------------------------
(define (attach-tag tag x)
  (cons tag x))

(define (make-int a)
  (attach-tag 'Z (inexact->exact (floor a))))

(define (add-int x y)
  (make-int (+ (contents x)
               (contents y))))

(define (sub-int x y)
  (make-int (- (contents x)
               (contents y))))

(define (mul-int x y)
  (make-int (* (contents x)
               (contents y))))

(define (div-int x y)
  (make-int (/ (contents x)
               (contents y))))
; ------------------------------INTEGERS-PACKAGE--------------------------------------


; ------------------------------RATIONAL-PACKAGE--------------------------------------
(define (contents x)
  (cdr x))

(define (numerator x)
  (car (contents x)))

(define (denominator x)
  (cdr (contents x)))

(define (make-rat a b)
  (if (= 0 (contents b))
      (error "cannot divide by zero!")
      (let ((x (gcd (contents a) (contents b))))
        (attach-tag 'Q (cons (make-int (/ (contents a) x))
                             (make-int (/ (contents b) x)))))))

(define (add-rat x y)
  (make-rat (add-int (mul-int (numerator x)
                              (denominator y))
                     (mul-int (numerator y)
                              (denominator x)))
            (mul-int (denominator x)
                     (denominator y))))

(define (sub-rat x y)
  (make-rat (sub-int (mul-int (numerator x)
                              (denominator y))
                     (mul-int (numerator y)
                              (denominator x)))
            (mul-int (denominator x)
                     (denominator y))))

(define (mul-rat x y)
  (make-rat (mul-int (numerator x)
                     (numerator y))
            (mul-int (denominator x)
                     (denominator y))))

(define (div-rat x y)
  (make-rat (mul-int (numerator x)
                     (denominator y))
            (mul-int (denominator x)
                     (numerator y))))
; ------------------------------RATIONAL-PACKAGE--------------------------------------


; ------------------------------REALS-PACKAGE-----------------------------------------
(define (make-real a)
  (attach-tag 'R a))

(define (add-real x y)
  (make-real (+ (contents x)
                (contents y))))

(define (sub-real x y)
  (make-real (- (contents x)
                (contents y))))

(define (mul-real x y)
  (make-real (* (contents x)
                (contents y))))

(define (div-real x y)
  (make-real (/ (contents x)
                (contents y))))
; ------------------------------REALS-PACKAGE-----------------------------------------


; ------------------------------COMPLEXES-PACKAGE-------------------------------------
(define (imaginary-part x)
  (cdr (contents x)))

(define (real-part x)
  (car (contents x)))

(define (make-complex a b)
  (attach-tag 'C (cons a b)))

(define (add-complex x y)
  (make-complex (add-real (real-part x)
                          (real-part y))
                (add-real (imaginary-part x)
                          (imaginary-part y))))

(define (sub-complex x y)           
  (make-complex (sub-real (real-part x)
                          (real-part y))
                (sub-real (imaginary-part x)
                          (imaginary-part y))))

(define (mul-complex x y)           
  (make-complex (sub-real (mul-real (real-part x)
                                    (real-part y))
                          (mul-real (imaginary-part x)
                                    (imaginary-part y)))
                (sub-real (mul-real (real-part x)
                                    (imaginary-part y))
                          (mul-real (imaginary-part x)
                                    (real-part y)))))

(define (div-complex x y)
  (make-complex (div-real (add-real (mul-real (real-part x)
                                              (real-part y))
                                    (mul-real (imaginary-part x)
                                              (imaginary-part y)))
                          (add-real (mul-real (real-part y)
                                              (real-part y))
                                    (mul-real (imaginary-part y)
                                              (imaginary-part y))))
                (div-real (sub-real (mul-real (imaginary-part x)
                                              (real-part y))
                                    (mul-real (real-part x)
                                              (imaginary-part y)))
                          (add-real (mul-real (real-part y)
                                              (real-part y))
                                    (mul-real (imaginary-part y)
                                              (imaginary-part y))))))
; ------------------------------COMPLEXES-PACKAGE-------------------------------------


; -----------------------------POLYNOMIALS-PACKAGE------------------------------------
(define (coeff x)
  (car (car x)))

(define (exp x)
  (cdr (car x)))

(define (term x)
  (car x))

(define (make-term c e)
  (cons c e))

(define (make-poly var . l)
  (simplify-poly (attach-tag 'P (cons var l))))

(define (poly-var x)
  (car (contents x)))

(define (poly-terms x)
  (cdr (contents x)))

(define (add-poly a b)
  (define (same-var? a b)
    (eq? (poly-var a) (poly-var b)))

  (define (iter x y)
    (cond ((null? x) y)
          ((null? y) x)
          ((< (contents (exp x)) (contents (exp y)))
           (cons (term x) (iter (cdr x) y)))
          ((< (contents (exp y)) (contents (exp x)))
           (cons (term y) (iter x (cdr y))))
          ((= (contents (exp x)) (contents (exp y)))
           (cons (make-term (add (coeff x) (coeff y))
                            (exp x))
                 (iter (cdr x) (cdr y))))))

  (if (same-var? a b)
      (simplify-poly (attach-tag 'P (cons (poly-var a) (iter (poly-terms a) (poly-terms b)))))
      (error "cannot add polynomials with different variables")))

(define (simplify-poly x)
  (define (int? x)
    (eq? (car x) 'Z))

  (define (zero? x)
    (let ((a (simplify x)))
      (if (int? a)
          (= 0 (contents a))
          #f)))

  (define (simplify-terms p)
    (cond ((null? p) '())
          ((zero? (coeff p)) (simplify-terms (cdr p)))
          (else (cons (make-term (simplify (coeff p))
                                 (exp p))
                      (simplify-terms (cdr p))))))

  (let ((r (attach-tag 'P (cons (poly-var x) (simplify-terms (poly-terms x))))))
    (if (null? (poly-terms r))
        (make-int 0)
        r)))
; -----------------------------POLYNOMIALS-PACKAGE------------------------------------


; -------------------------------PUT!-AND-GET-PACKAGE---------------------------------
;(define global-array '())

;(define (put! op type item)
;  (define (key entry) (car entry))
;  (define (make-entry k v) (list k v))
;  (define (put-helper k array)
;    (cond ((null? array) (list (make-entry k item)))
;          ((equal? (key (car array)) k) array)
;          (else (cons (car array) (put-helper k (cdr array))))))
;  (set! global-array (put-helper (list op type) global-array)))

;(define (get op type)
;  (define (key entry) (car entry))
;  (define (value entry) (cadr entry))
;  (define (get-helper k array)
;    (cond ((null? array) #f)
;          ((equal? (key (car array)) k) (value (car array)))
;          (else (get-helper k (cdr array)))))
;  (get-helper (list op type) global-array))

(define local-table (list '*table*))

(define (get key-1 key-2)
  (let ((subtable (assoc key-1 (cdr local-table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              #f))
        #f)))

(define (put! key-1 key-2 value)
  (let ((subtable (assoc key-1 (cdr local-table))))
    (if subtable  ;  if it doesn't exist, subtable returns #f
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)  ; Updates the record if an old value is already there
              (set-cdr! subtable
                        (cons (cons key-2 value) (cdr subtable)))))
        (set-cdr! local-table
                  (cons (list key-1 (cons key-2 value)) (cdr local-table)))))
  (display "inserted in table! ")
  (display value)
  (newline))


; -------------------------------PUT!-AND-GET-PACKAGE---------------------------------


; -------------------------------TOWER-OF TYPES-PACKAGE-------------------------------

(define (tag x)
  (car x))

; -----------------RAISE--------------------------
(define (int->rat x)
  (make-rat x
            (make-int 1)))

(define (rat->real x)
  (div-real (make-real (contents (numerator x)))
            (make-real (contents (denominator x)))))

(define (real->complex x)
  (make-complex x
                (make-real 0)))

(define (complex->poly x)
  (make-poly 'x (make-term x (make-int 0))))

;-------------------RAISE-------------------------

;-------------------DROP--------------------------
(define (poly->complex x)
  (coeff (poly-terms x)))

(define (complex->real x)
  (real-part x))

(define (real->rat x)
  (define (% x y)
    (cond ((< x y) x)
          (else (% (- x y) y))))
  (define (iter num den)
    (if (= (% num 1) 0)
        (make-rat (make-int num) (make-int den))
        (iter (* num 10) (* den 10))))
  (iter (contents x) 1))

(define (rat->int x)
  (numerator x))
;------------------DROP---------------------------

;----------------CAN-DROP?------------------------
(define (poly->complex? x)
  (and (tag (coeff (poly-terms x)))
       (= (contents (exp (poly-terms x))) 0)
       (= (length (poly-terms x)) 1)))

(define (complex->real? x)
  (= 0 (contents (imaginary-part x))))

(define (real->rat? x)
  (< (decimal-places (contents x)) 3))

(define (rat->int? x)
  (= 1 (contents (denominator x))))
;----------------CAN-DROP?------------------------


;---------------------GENERIC-OPERATORS-HELPERS---------------------------------------

(define (decimal-places n)
  (define (string-index str char)
    (let loop ((i 0))
      (cond ((>= i (string-length str)) #f)
            ((char=? (string-ref str i) char) i)
            (else (loop (+ i 1))))))
  (let* ((num-str (number->string n))
         (dot-pos (string-index num-str #\.)))
    (if dot-pos
        (- (string-length num-str) dot-pos 1)
        0)))

(define (make-op op)
  (define dictionary
    (list (cons 'P 0)
          (cons 'C 1)
          (cons 'R 2)
          (cons 'Q 3)
          (cons 'Z 4)))
  
  (define (look-up tag)
    (define (iter dict)
      (cond ((null? dict)
             (error "tag not found in dict" tag))
            ((eq? (caar dict) tag)
             (cdar dict))
            (else (iter (cdr dict)))))
    (iter dictionary))

  (define (comp-n-times f n)
    (cond ((= n 0) (lambda (x) x))
          (else (lambda(x) (f ((comp-n-times f (- n 1)) x))))))
  
  (lambda (a b)
    (let ((a-num-tag (look-up (tag a)))
          (b-num-tag (look-up (tag b)))
          (raise (make-leveler 'raise)))
      (let ((x ((comp-n-times raise
                              (max 0 (- a-num-tag
                                        b-num-tag)))
                a))
            (y ((comp-n-times raise
                              (max 0 (- b-num-tag
                                        a-num-tag)))
                b)))
        ((get op (tag x)) x y)))))

(define (make-leveler op)
  (lambda (a)
    (let ((proc (get op (tag a))))
      (if proc
          (proc a)
          (error "something unexpected happened!" op a)))))

(define (simplify x)
  (let ((drop (make-leveler 'drop))
        (droppable? (make-leveler 'can-drop?)))
    (define (iter a)
      (if (droppable? a)
          (iter (drop a))
          a))
    (iter x)))

(define (foldl f l)
  (define (foldl-helper f result lst)
    (if (null? lst)
        result
        (foldl-helper f (f result (car lst)) (cdr lst))))
  (if (null? l)
      (error "Lista vazia!")
      (foldl-helper f (car l) (cdr l))))

(define (foldr f l)
  (define (foldr-helper f lst)
    (if (null? lst)
        (error "Lista vazia!")
        (if (null? (cdr lst))
            (car lst)
            (f (car lst) (foldr-helper f (cdr lst))))))
  (if (null? l)
      (error "Lista vazia!")
      (foldr-helper f l)))

;---------------------GENERIC-OPERATORS-HELPERS---------------------------------------


;---------------------ADDING-PROCEDURES-TO-THE-TABLE----------------------------------

(put! 'add 'P add-poly)
(put! 'add 'C add-complex)
(put! 'add 'R add-real)
(put! 'add 'Q add-rat)
(put! 'add 'Z add-int)

(put! 'sub 'C sub-complex)
(put! 'sub 'R sub-real)
(put! 'sub 'Q sub-rat)
(put! 'sub 'Z sub-int)

(put! 'mul 'C mul-complex)
(put! 'mul 'R mul-real)
(put! 'mul 'Q mul-rat)
(put! 'mul 'Z mul-int)

(put! 'div 'C div-complex)
(put! 'div 'R div-real)
(put! 'div 'Q div-rat)
(put! 'div 'Z div-int)

(put! 'raise 'P (lambda (x) (error "not possible to raise a poly!")))
(put! 'raise 'C complex->poly)
(put! 'raise 'R real->complex)
(put! 'raise 'Q rat->real)
(put! 'raise 'Z int->rat)

(put! 'drop 'P poly->complex)
(put! 'drop 'C complex->real)
(put! 'drop 'R real->rat)
(put! 'drop 'Q rat->int)
(put! 'drop 'Z (lambda (x) (error "not possible to drop integer")))

(put! 'can-drop? 'P poly->complex?)
(put! 'can-drop? 'C complex->real?)
(put! 'can-drop? 'R real->rat?)
(put! 'can-drop? 'Q rat->int?)
(put! 'can-drop? 'Z (lambda (x) #f))

;---------------------ADDING-PROCEDURES-TO-THE-TABLE----------------------------------


;----------------------INSTANCIATION-OF-THE-GENERIC-OPERATORS-------------------------

(define (add . l)
  (simplify (foldr (make-op 'add) l)))

(define (sub . l)
  (simplify (foldl (make-op 'sub) l)))

(define (mul . l)
  (simplify (foldr (make-op 'mul) l)))

(define (div . l)
  (simplify (foldl (make-op 'div) l)))

;----------------------INSTANCIATION-OF-THE-GENERIC-OPERATORS-------------------------


;---------------------------------------TESTING---------------------------------------

(define p1 (make-poly 'x (make-term (make-complex (make-real 5.423) (make-real 0)) (make-int 1))))

(define c1 (make-complex (make-real 2.3) (make-real 1)))

(add p1 p1)

(add c1 c1)

(add p1 c1)
