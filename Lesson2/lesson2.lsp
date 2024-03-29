;; 1 task

(defun create-set (args)
  (lambda (elem)
    (when (find elem (remove-duplicates args)) T)))

(defun create-union (set1 set2)
  (lambda (elem)
    (or (funcall set1 elem)
	(funcall set2 elem))))

(defun create-intersect (set1 set2)
  (lambda (elem)
    (and (funcall set1 elem)
	 (funcall set2 elem))))

(defun create-difference (set1 set2)
  (lambda (elem)
    (not (and (funcall set1 elem)
	     (funcall set2 elem)))))

;; 2 task

(defun bubble-sort (lst &key (key #'<))
  (labels ((%is-sorted-p (lst)
	     (cond ((null (cdr lst)) t)
		   ((funcall key (car lst) (cadr lst)) (%is-sorted-p (cdr lst)))
		   (t nil)))
	   (%helper (lst)
	     (cond ((null (cdr lst)) lst)
		   ((funcall key (car lst) (cadr lst)) (cons (car lst) (%helper (cdr lst))))
		   (t (cons (cadr lst) (%helper (cons (car lst) (cddr lst))))))))
  (cond ((null lst) lst)
	((%is-sorted-p lst) lst)
	(t (bubble-sort (%helper lst) :key key)))))
  
(defun insertion-sort (lst &key (key #'<))
  (labels ((%insert (item lst)
	     (cond ((null lst) (list item))
		   ((funcall key item (car lst)) (cons item lst))
		   (t (cons (car lst) (%insert item (cdr lst)))))))
    (when lst (%insert (car lst) (insertion-sort (cdr lst))))))
 
(defun selection-sort (lst &key (key #'<))
  (labels ((%helper (lst)
	     (when lst
	       (let ((item (reduce (lambda (acc item) (if (funcall key item acc) item acc)) lst)))
		 (cons item (%helper (remove item lst :count 1)))))))
    (%helper lst)))

;;------------------------

(defun bubble-sort-imperative (lst)
  (let ((len (1- (length lst))))
    (dotimes (i len lst)
      (dotimes (j (- len i))
	(when (> (nth j lst) (nth (1+ j) lst))
	  (rotatef (nth j lst) (nth (1+ j) lst)))))))
	   
(defun insertion-sort-imperative (lst)
  (let ((len (length lst)))
    (loop
      for i from 1 to (1- len)
      do
	 (let ((key (nth i lst))
	       (j (1- i)))
	   (loop
	     repeat (1+ j)
	     do
		(when (> (nth j lst) key)
		  (setf (nth (1+ j) lst) (nth j lst)))
	     unless (> (nth j lst) key)
	       do (return)
	     do (decf j))
	   (setf (nth (1+ j) lst) key))))
  lst)

(defun selection-sort-imperative (lst)
  (let ((len (1- (length lst))))
    (dotimes (i len)
      (let ((min-index i))
	(loop for j from (1+ i) to len
	      do
		 (when (< (nth j lst) (nth min-index lst))
		   (setf min-index j)))
	(rotatef (nth min-index lst) (nth i lst)))))
  lst)

;; 3 task

(defun gen-my-sum ()
  (let ((sum 0))
    (lambda (x)
      (incf sum x))))

;; 4 task

(defun create-eraser (k)
  (lambda (lst)
    (labels ((%helper (acc lst)
	       (let ((item (car lst)))
		 (cond ((null lst) (reverse acc))
		       ((and (>= (count item lst) k)
			     (not (member item acc)))
			(%helper (cons item acc) (cdr lst)))
		       (t (%helper acc (cdr lst)))))))
      (%helper nil lst))))

;; 5 task

(defun num-orders (lst)
  (labels ((%fact (n)
	     (if (< n 2) 1 (* n (%fact (1- n)))))
	   (%fact-of-sums (lst)
	     (%fact (reduce #'+ lst)))
	   (%application-of-facts (lst)
	     (reduce #'* lst :key #'%fact)))
    (if lst
	(/ (%fact-of-sums lst) (%application-of-facts lst))
	0)))
				    

;; 6 task

(defun quicksort (lst)
  (when lst
    (let ((pivot (car lst))
	  (tail (cdr lst)))
      (append (quicksort
	       (remove-if (lambda (x) (> x pivot)) tail))
	      (list pivot)
	      (quicksort
	       (remove-if (lambda (x) (<= x pivot)) tail))))))

;; TESTS
;; 1 task

(defun check-create-set (input item expected)
  (let ((result (funcall (create-set input) item)))
    (eq result expected)))

(defun test-create-set (name input item expected)
  (or (check-create-set input item expected)
      (format T "~A failed.~%" name)))

(defun run-create-set-tests ()
  (test-create-set "create-set-1" nil 10 nil)
  (test-create-set "create-set-2" '(10) 10 T)
  (test-create-set "create-set-3" '(10) 21 nil)
  (test-create-set "create-set-4" '(10 15) 10 T)
  (test-create-set "create-set-5" '(10 15) 15 T)
  (test-create-set "create-set-6" '(10 15) 21 nil)
  (test-create-set "create-set-7" '(10 15 21) 21 T)
  (test-create-set "create-set-8" '(10 15 21) 0 nil))

(defun check-create-union (input item expected)
  (let ((result (funcall (create-union (create-set (first input)) (create-set (second input))) item)))
    (eq result expected)))

(defun test-create-union (name input item expected)
  (or (check-create-union input item expected)
      (format T "~A failed.~%" name)))

(defun run-create-union-tests ()
  (test-create-union "create-union-1" '((10) (15 13)) 10 T)
  (test-create-union "create-union-2" '((10) (15 13)) 15 T)
  (test-create-union "create-union-3" '((10) (15 13)) 14 nil))

(defun check-create-intersect (input item expected)
  (let ((result (funcall (create-intersect (create-set (first input)) (create-set (second input))) item)))
    (eq result expected)))

(defun test-create-intersect (name input item expected)
  (or (check-create-intersect input item expected)
      (format T "~A failed.~%" name)))

(defun run-create-intersect-tests ()
  (test-create-intersect "create-intersect-1" '((10 12 14) (11 14 12)) 12 T)
  (test-create-intersect "create-intersect-2" '((10 12 14) (11 14 12)) 14 T)
  (test-create-intersect "create-intersect-3" '((10 12 14) (11 14 12)) 13 nil))

(defun check-create-difference (input item expected)
  (let ((result (funcall (create-difference (create-set (first input)) (create-set (second input))) item)))
    (eq result expected)))

(defun test-create-difference (name input item expected)
  (or (check-create-difference input item expected)
      (format T "~A failed.~%" name)))

(defun run-create-difference-tests ()
  (test-create-difference "create-difference-1" '((10 12 14) (11 14 12)) 10 T)
  (test-create-difference "create-difference-2" '((10 12 14) (11 14 12)) 12 nil)
  (test-create-difference "create-difference-3" '((10 12 14) (11 14 12)) 14 nil))

(defun run-all-set-tests ()
  (run-create-set-tests)
  (run-create-union-tests)
  (run-create-intersect-tests)

  (run-create-difference-tests))
;; 2 task

(defun check-sort (func input expected)
  (let ((result (funcall func input)))
    (equal result expected)))

(defun test-sort (name func input expected)
  (or (check-sort func input expected)
      (format T "~A failed.~%" name)))

(defun run-sort-tests ()
  (test-sort "bubble-sort-1" #'bubble-sort '(1 3 7 8 6 5 4 2 9 0) '(0 1 2 3 4 5 6 7 8 9))
  (test-sort "bubble-sort-imperative-1" #'bubble-sort-imperative '(1 3 7 8 6 5 4 2 9 0) '(0 1 2 3 4 5 6 7 8 9))
  (test-sort "insertion-sort-1" #'insertion-sort '(1 3 7 8 6 5 4 2 9 0) '(0 1 2 3 4 5 6 7 8 9))
  (test-sort "insertion-sort-imperative-1" #'insertion-sort-imperative '(1 3 7 8 6 5 4 2 9 0) '(0 1 2 3 4 5 6 7 8 9))
  (test-sort "selection-sort-1" #'selection-sort '(1 3 7 8 6 5 4 2 9 0) '(0 1 2 3 4 5 6 7 8 9))
  (test-sort "selection-sort-imperative-1" #'selection-sort-imperative '(1 3 7 8 6 5 4 2 9 0) '(0 1 2 3 4 5 6 7 8 9)))

;; 3 task

(defun check-gen-my-sum (input expected)
  (let ((result (mapcar (gen-my-sum) input)))
    (equal result expected)))

(defun test-gen-my-sum (name input expected)
  (or (check-gen-my-sum input expected)
      (format T "~A failed.~%" name)))

(defun run-gen-my-sum-tests ()
  (test-gen-my-sum "gen-my-sum-1" '(1 1 1 1) '(1 2 3 4))
  (test-gen-my-sum "gen-my-sum-1" '(1 2 3 4) '(1 3 6 10)))

;; 4 task

(defun check-eraser (input n expected)
  (let ((result (funcall (create-eraser n) input)))
    (equal result expected)))

(defun test-eraser (name input n expected)
  (or (check-eraser input n expected)
      (format T "~A failed.~%" name)))

(defun run-eraser-tests ()
  (test-eraser "eraser-1" '(1 2 3 3 2 3 a a c a a a) 3 '(3 a))
  (test-eraser "eraser-2" '(1 1 1 1 1 1 1 1 1 1 1 1) 1 '(1))
  (test-eraser "eraser-3" '(1 2 3 3 nil nil 3 a nil c nil a a) 3 '(3 nil a)))

;; 5 task

(defun check-num-orders (input expected)
  (let ((result (num-orders input)))
    (eql result expected)))

(defun test-num-orders (name input expected)
  (or (check-num-orders input expected)
      (format T "~A failed.~%" name)))

(defun run-num-orders-tests ()
  (test-num-orders "num-orders-1" nil 0)
  (test-num-orders "num-orders-2" '(1 1) 2)
  (test-num-orders "num-orders-3" '(1 2) 3)
  (test-num-orders "num-orders-4" '(1 1 1) 6)
  (test-num-orders "num-orders-5" '(1 1 3) 20))

;; 6 task

(defun check-qsort (input expected)
  (let ((result (quicksort input)))
    (equal result expected)))

(defun test-qsort (name input expected)
  (or (check-qsort input expected)
      (format T "~A failed.~%" name)))

(defun run-qsort-tests ()
  (test-qsort "quicksort-1" '(0 1 9 5 8 3 2) '(0 1 2 3 5 8 9))
  (test-qsort "quicksort-2" '(0 0 0 0 0 0 0 -1) '(-1 0 0 0 0 0 0 0)))

;;------------------------------

(defun run-all-tests ()
  (run-all-set-tests)
  (run-sort-tests)
  (run-gen-my-sum-tests)
  (run-eraser-tests)
  (run-num-orders-tests)
  (run-qsort-tests))
