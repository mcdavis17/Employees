(define-structure (hourlyEmployee keyword-constructor copier)
  firstName lastName hours hourlyRate)

(define-structure (commissionEmployee keyword-constructor copier)
  firstName lastName minSalary sales commissionRate)

(define-structure (salariedEmployee keyword-constructor copier) 
  firstName lastName weeklySalary)

(define employees(list))

(define (compute . args)
  (set! employees ())
  (if (file-exists? (car args))
     (begin
      (cond
        ((eq? (length args) 2)
          (begin
            (let ((filename (car args)))
              (get-data filename)
              (set! args (cdr args)))
            (let ((function (car args)))
              (display #\newline)
              (set! args (cdr args))
              (cond
                ((string=? function "count")
                  (display (get-count "ge" 0 employees))
                  )
                ((string=? function "print")
                  (print "ge" 0 employees)
                  )
                ((string=? function "min")
                  (get-min "ge" 0 employees)
                  )
                ((string=? function "max")
                  (get-max "ge" 0 employees)
                  )
                ((string=? function "total")
                  (display (get-total "ge" 0 employees))
                  )
                ((string=? function "avg")
                  (display (get-avg "ge" 0 employees))
                  )
                ); end cond
              ); end let
            ) ;end begin
          )
        ((eq? (length args) 4)
          (begin
            (let ((filename (car args)))
              (get-data filename)
              (set! args (cdr args)))
            (let ((function (car args)))
              (set! args (cdr args))
              (cond
                ((string=? function "count")
                  (display (get-count (car args) (car (cdr args)) employees))
                  )
                ((string=? function "print")
                  (print (car args) (car (cdr args)) employees)
                  )
                ((string=? function "min")
                  (get-min (car args) (car (cdr args)) employees)
                  )
                ((string=? function "max")
                  (get-max (car args) (car (cdr args)) employees)
                  )
                ((string=? function "total")
                  (display (get-total (car args) (car (cdr args)) employees))
                  )
                ((string=? function "avg")
                  (display (get-avg (car args) (car (cdr args)) employees))
                  )
                ); end cond
              ); end let
            ) ;end begin
          )
        (else (display "\nUsage: (compute employee_file action)\nor\nUsage: (compute employee_file action operator threshold)\nValid actions: count print min max total avg\nValid operators: eq ne gt ge lt le\n"))
        ))
     (display "The file could not be opened\n"))
  ) ;end compute

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-data filename)
  (let ((port (open-input-file filename)))
    (get-data-helper port)
    (close-input-port port)
    'done)
  ) ;end get-data

(define (get-data-helper port)
  (let ((line (read-line port)))
    (if (eof-object? line)
      'done
      (begin
        (add-to-list line)
        (get-data-helper port)))
    )
  ) ;end get-data-helper

(define (add-to-list employee)
  (let ((port (open-input-string employee)))
    (let ((word (read port)))
      (if (string=? (string word) "salaried")
        (begin
          (set! employees
            (cons
              (make-salariedEmployee
                'weeklySalary (read port)
                'lastName (read port)
                'firstName (read port)) employees))
            ))
      (if (string=? (string word) "commission")
        (begin
          (set! employees
            (cons
              (make-commissionEmployee
                'commissionRate (read port)
                'sales (read port)
                'minSalary (read port)
                'lastName (read port)
                'firstName (read port)) employees))
            ))
      (if (string=? (string word) "hourly")
        (begin
          (set! employees
            (cons
              (make-hourlyEmployee
                'hourlyRate (read port)
                'hours (read port)
                'lastName (read port)
                'firstName (read port)) employees))
          ))
      )
    )
  ) ;end add-to-list

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (check value op threshold)
  (cond
    ((string=? op "eq") (= value threshold) )
    ((string=? op "ne") (not(= value threshold)) )
    ((string=? op "ge") (>= value threshold) )
    ((string=? op "le") (<= value threshold) )
    ((string=? op "gt") (> value threshold) )
    ((string=? op "lt") (< value threshold) )
    (else #f)
    )
  ); end check

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-earning emp)
  ( cond
    ((salariedEmployee? emp)
      (salariedEmployee-weeklySalary emp))
    ((commissionEmployee? emp)
      (let ((commissionWage (* (commissionEmployee-sales emp) (commissionEmployee-commissionRate emp) )))
        (if ( > commissionWage (commissionEmployee-minSalary emp) )
          commissionWage
          (commissionEmployee-minSalary emp))))
    ((hourlyEmployee? emp)
      (let ((total 0))
        ( do ((i (hourlyEmployee-hours emp) (- i 1))) ((eq? i 0))
          (cond
            ((> i 50) (set! total (+ total (* (hourlyEmployee-hourlyRate emp) 2))) )
            ((and (> i 40) (<= i 50)) (set! total (+ total (* (hourlyEmployee-hourlyRate emp) 1.5))) )
            (else (set! total (+ total (hourlyEmployee-hourlyRate emp))) )))
        total)))
  ) ;end get-earning

(define (get-info employee)
  ( cond
    ((salariedEmployee? employee)
      (let ((info (string "Salaried employee: ")))
        (set! info (string-append info (string (salariedEmployee-firstName employee)) " " (string (salariedEmployee-lastName employee)) (string #\newline) ))
        (set! info (string-append info "weekly salary: " (string (salariedEmployee-weeklySalary employee)) (string #\newline) ))
        (set! info (string-append info "earned: $" (string (get-earning employee)) (string #\newline) ))
        (set! info (string-append info (string #\newline)))
        info)
      )
    ((hourlyEmployee? employee)
      (let ((info (string "Hourly employee: ")))
        (set! info (string-append info (string (hourlyEmployee-firstName employee)) " " (string (hourlyEmployee-lastName employee)) (string #\newline) ))
        (set! info (string-append info "hours worked: " (string (hourlyEmployee-hours employee)) ", hourly rate: " (string (hourlyEmployee-hourlyRate employee)) (string #\newline) ))
        (set! info (string-append info "earned: $" (string (get-earning employee)) (string #\newline) ))
        (set! info (string-append info (string #\newline)))
        info)
      )
    ((commissionEmployee? employee)
      (let ((info (string "Commission employee: ")))
        (set! info (string-append info (string (commissionEmployee-firstName employee)) " " (string (commissionEmployee-lastName employee)) (string #\newline) ))
        (set! info (string-append info "minimum salary: " (string (commissionEmployee-minSalary employee)) ", sales amount: " (string (commissionEmployee-sales employee)) ", commission rate: " (string (* 100 (commissionEmployee-commissionRate employee))) "%" (string #\newline) ))
        (set! info (string-append info "earned: $" (string (get-earning employee)) (string #\newline) ))
        (set! info (string-append info (string #\newline)))
        info)
      )
    )
  ) ; end get-info

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-count operation threshold emps)
  (let ((count 0))
    (let ((j (length emps)))
      (do ((i 1 (1+ i))) ((> i j))
        (let ((emp (car emps)))
          (set! emps (cdr emps))
          (if (check (get-earning emp) operation threshold )
            (set! count (+ count 1))
            )
          )
        )
      )
    (string-append "There are " (string count) " emloyees\n")
    )
  ) ;end get-count

(define (print operation threshold emps)
  (let ((j (length emps)))
    (do ((i 1 (1+ i))) ((> i j))
      (let ((emp (car emps)))
        (set! emps (cdr emps))
        (if (check (get-earning emp) operation threshold )
          ;(display i)
          (display (get-info emp))
          ))))
  ) ;end print

(define (get-total operation threshold emps)
  (let ((totalPay 0))
    (let ((j (length emps)))
      (do ((i 1 (1+ i))) ((> i j))
        (let ((emp (car emps)))
          (set! emps (cdr emps))
          (if (check (get-earning emp) operation threshold )
            (set! totalPay (+ totalPay (get-earning emp)))
            )
          )
        )
      )
    (string-append "Total payment is $" (string totalPay) "\n")
    )
  ) ;end get-total

(define (get-avg operation threshold emps)
  (let ((totalPay 0))
    (let ((count 0))
      (let ((j (length emps)))
        (do ((i 1 (1+ i))) ((> i j))
          (let ((emp (car emps)))
            (set! emps (cdr emps))
            (if (check (get-earning emp) operation threshold )
               (begin 
                (set! totalPay (+ totalPay (get-earning emp)))
                (set! count (+ count 1)))))))
      (string-append "Average payment per employee is $" (string (/ totalPay count)) "\n")
      ))
  ) ;end get-avg

(define (get-min operation threshold emps)
  (let ( (minimum (get-earning (car emps) )))
    (let ((j (length emps)))
      (do ((i 1 (1+ i))) ((> i j))
        (let ((emp (car emps)))
          (set! emps (cdr emps))
          (if (and (check (get-earning emp) operation threshold) (< (get-earning emp) minimum) )
            (set! minimum (get-earning emp))))))
    (print "eq" minimum employees))
  ) ;end get-min

(define (get-max operation threshold emps)
  (let ( (maximum 0 ))
    (let ((j (length emps)))
      (do ((i 1 (1+ i))) ((> i j))
        (let ((emp (car emps)))
          (set! emps (cdr emps))
          (if (and (check (get-earning emp) operation threshold) (> (get-earning emp) maximum) )
            (set! maximum (get-earning emp))))))
    (print "eq" maximum employees))
  ) ;end get-max

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
