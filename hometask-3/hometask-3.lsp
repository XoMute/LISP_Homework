(defstruct row
  id1 name surname age id2 auto color year)

;; TODO: implement
(defun make-select (f1 f2 f3))

(defun select (selector table)
  (remove-if-not selector table))

(defun that (&key id1 name surname age id2 auto color year)
  (lambda (row)
    (and
     (if id1     (equal (row-id1 row) id1) t)
     (if name    (equal (row-name row) name) t)
     (if surname (equal (row-surname row) surname) t)
     (if age     (equal (row-age row) age) t)
     (if id2     (equal (row-id2 row) id2) t)
     (if auto    (equal (row-auto row) auto) t)
     (if color   (equal (row-color row) color) t)
     (if year    (equal (row-year row) year) t))))

;; TODO: implement reading from tables to single list of structures
(defun read-tables (f1 f2 f3) )
