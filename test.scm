
(load "payroll.scm")

(compute "employees.dat")
(compute "employees.dat" "count")
(compute "employees.dat" "total")
(compute "employees.dat" "avg")
(compute "employees.dat" "min")
(compute "employees.dat" "max")
(compute "employees.dat" "max" "lt" 1800)
(compute "employees.dat" "print" "ge" 2000)