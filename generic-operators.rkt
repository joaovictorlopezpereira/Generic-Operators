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
      (error "division by 0" a b)
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
(define (exact->inexact x)
  (* x 1.0))

(define (make-real a)
  (attach-tag 'R (exact->inexact a)))

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
  (attach-tag 'C (cons (make-real (contents a))
                       (make-real (contents b)))))

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
(define (tag x)
  (car x))

(define (term x)
  (car x))

(define (coeff x)
  (car (term x)))

(define (exp x)
  (cdr (term x)))

(define (make-term c e)
  (cons c e))

(define (make-poly var . l)
  (simplify-poly
   (attach-tag 'P (cons var l))))

(define (remaining-terms x)
  (cdr x))

(define (poly-var x)
  (car (contents x)))

(define (poly-terms x)
  (cdr (contents x)))

(define (add-poly a b)
  (define (same-var? a b)
    (eq? (poly-var a) 
         (poly-var b)))

  (define (iter x y)
    (cond ((null? x) y)
          ((null? y) x)
          ((< (contents (exp x)) (contents (exp y)))
           (cons (term x) (iter (remaining-terms x) y)))
          ((< (contents (exp y)) (contents (exp x)))
           (cons (term y) (iter x (remaining-terms y))))
          ((= (contents (exp x)) (contents (exp y)))
           (cons (make-term (add (coeff x) (coeff y))
                            (exp x))
                 (iter (remaining-terms x) (remaining-terms y))))))

  (if (same-var? a b)
      (simplify-poly (attach-tag 'P (cons (poly-var a)
                                          (iter (poly-terms a) (poly-terms b)))))
      (error "cannot add polynomials with different variables")))

(define (simplify-poly x)
  (define (zero? x)
    (define (int? x)
      (eq? (tag x) 'Z))
    (let ((a (simplify x)))
      (if (int? a)
          (= 0 (contents a))
          #f)))
  
  (define (poly? x)
    (eq? (car x) 'P))

  (define (iter p)
    (cond ((null? p) '())
          ((zero? (coeff p)) (iter (cdr p)))
          ((element-present? (tag (coeff p)) tower-of-types-dictionary)
           (cons (make-term (simplify (coeff p))
                            (exp p))
                 (iter (cdr p))))
          ((poly? (coeff p))
           (cons (make-term (simplify-poly (coeff p))
                            (exp p))
                 (iter (cdr p))))))
  
  (let ((simplified-poly (attach-tag 'P (cons (poly-var x)
                                              (iter (poly-terms x))))))
    (if (null? (poly-terms simplified-poly))
        (attach-tag 'P (cons (poly-var x) (list (make-term (make-int 0) (make-int 0)))))
        simplified-poly)))
; -----------------------------POLYNOMIALS-PACKAGE------------------------------------


; -------------------------------MATRICES-PACKAGE-------------------------------------
(define (make-matrix . l)
  (define (right-length? lst)
    (cond ((null? (cdr lst)) #t)
          ((not (= (length (car lst))
                   (length (car (cdr lst)))))
           #f)
          (else (right-length? (cdr lst)))))
  (if (right-length? l)
      (cons 'M l)
      (error "the matrix has empty spaces" l)))


(define (make-row . l)
  l)


(define (same-dimension? m1 m2)
  (define (same-length? lst1 lst2)
    (= (length lst1) (length lst2)))
  
  (define (iter rows1 rows2)
    (cond ((and (null? rows1) (null? rows2)) #t)  ; no matrices remaining, they have the same dimension
          ((or (null? rows1) (null? rows2)) #f)   ; one matrix remaining, different dimensions
          ((not (same-length? (car rows1) (car rows2))) #f)  ; different length
          (else (iter (cdr rows1) (cdr rows2))))) ; verify the remaining lines
  
  (and (same-length? m1 m2)   ; verify if matrices have the same number of rows
       (iter m1 m2)))         ; verify if each row have the same length


(define (add-matrix m1 m2)
  (define (add-row row1 row2)
    (map add row1 row2))
  
  (define (iter rows1 rows2)
    (cond ((null? rows1) '())
          ((null? rows2) (error "Matrices have different dimensions"))
          (else (cons (add-row (car rows1) (car rows2))
                      (iter (cdr rows1) (cdr rows2))))))
  (if (same-dimension? (contents m1) (contents m2))
      (cons 'M (iter (contents m1) (contents m2)))
      (error "matrices do not have the same dimension")))
; -------------------------------MATRICES-PACKAGE-------------------------------------


; -------------------------------PUT!-AND-GET-PACKAGE---------------------------------
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
    (if subtable  ; if it doesn't exist, subtable returns #f
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)  ; updates the record if an old value is already there
              (set-cdr! subtable
                        (cons (cons key-2 value) (cdr subtable)))))
        (set-cdr! local-table
                  (cons (list key-1 (cons key-2 value)) (cdr local-table)))))

  ; adds key-1 in tower-of-types-dictionary if key-2 is 'drop ou 'raise
  (if (and (or (eq? key-1 'drop) (eq? key-1 'raise))
           (not (element-present? key-2 tower-of-types-dictionary)))  ; verifies if key-1 is in the dict
      (set! tower-of-types-dictionary (append tower-of-types-dictionary (list key-2)))))
; -------------------------------PUT!-AND-GET-PACKAGE---------------------------------


; -----------------------------LIST-MANIPULATION-PACKAGE------------------------------
(define (element-present? x lst)
  (cond ((null? lst) #f)
        ((eq? x (car lst)) #t)
        (else (element-present? x (cdr lst)))))


(define (list-pos lst el)
  (define (iter l e n)
    (if (eq? (car l) e)
        n
        (iter (cdr l) e (+ n 1))))
  (iter lst el 0))

(define (foldl f l)
  (define (iter result lst)
    (if (null? lst)
        result
        (iter (f result (car lst)) (cdr lst))))
  (if (null? l)
      (error "Empty List" l)
      (iter (car l) (cdr l))))
; -----------------------------LIST-MANIPULATION-PACKAGE------------------------------


; -------------------------------TOWER-OF TYPES-PACKAGE-------------------------------


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
;-------------------RAISE-------------------------

;-------------------DROP--------------------------
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


(define (make-leveler op)
  (lambda (a)
    (let ((proc (get op (tag a))))
      (if proc
          (proc a)
          (error "something unexpected happened!" op a)))))


(define (raise x n)
  (define (iter a m f)
    (if (= m 0)
        a
        (iter (f a) (- m 1) f)))
  (let ((raiser (make-leveler 'raise)))
    (iter x n raiser)))

        
(define (simplify x)
  (if (element-present? (tag x) tower-of-types-dictionary) 
      (let ((drop (make-leveler 'drop))
            (droppable? (make-leveler 'can-drop?)))
        (define (iter a)
          (if (droppable? a)
              (iter (drop a))
              a))
        (iter x))
      x))


(define tower-of-types-dictionary '())


(define (make-op op)
  (lambda (a b)
    (if (and (element-present? (tag a) tower-of-types-dictionary)
             (element-present? (tag b) tower-of-types-dictionary))
        (let ((a-pos (list-pos tower-of-types-dictionary (tag a)))
              (b-pos (list-pos tower-of-types-dictionary (tag b))))
          (cond ((> a-pos b-pos)
                 ((get op (tag b)) (raise a (- a-pos b-pos)) b))
                ((< a-pos b-pos)
                 ((get op (tag a)) a (raise b (- b-pos a-pos))))
                ((= a-pos b-pos)
                 ((get op (tag a)) a b))))
        ((get op (tag a)) a b))))
;---------------------GENERIC-OPERATORS-HELPERS---------------------------------------


;---------------------ADDING-PROCEDURES-TO-THE-TABLE----------------------------------
(put! 'add 'M add-matrix)
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

(put! 'raise 'C (lambda (x) (error "not possible to raise complex")))
(put! 'raise 'R real->complex)
(put! 'raise 'Q rat->real)
(put! 'raise 'Z int->rat)

(put! 'drop 'C complex->real)
(put! 'drop 'R real->rat)
(put! 'drop 'Q rat->int)
(put! 'drop 'Z (lambda (x) (error "not possible to drop integer")))

(put! 'can-drop? 'C complex->real?)
(put! 'can-drop? 'R real->rat?)
(put! 'can-drop? 'Q rat->int?)
(put! 'can-drop? 'Z (lambda (x) #f))
;---------------------ADDING-PROCEDURES-TO-THE-TABLE----------------------------------


;----------------------INSTANCIATION-OF-THE-GENERIC-OPERATORS-------------------------
(define (add . l)
  (simplify (foldl (make-op 'add) l)))

(define (sub . l)
  (simplify (foldl (make-op 'sub) l)))

(define (mul . l)
  (simplify (foldl (make-op 'mul) l)))

(define (div . l)
  (simplify (foldl (make-op 'div) l)))
;----------------------INSTANCIATION-OF-THE-GENERIC-OPERATORS-------------------------


;---------------------------------------TESTING---------------------------------------
(define p1 (make-poly 'x (make-term (make-int 3)(make-int 2))))

(define p2 (make-poly 'y (make-term p1 (make-int 2))))

(define m1 (make-matrix (make-row (make-int 1) (make-int 2) (make-int 3))
                        (make-row (make-int 1) p2 (make-int 3))
                        (make-row (make-int 1) (make-int 2) p1)))

(add m1 m1 m1)
;---------------------------------------TESTING---------------------------------------
