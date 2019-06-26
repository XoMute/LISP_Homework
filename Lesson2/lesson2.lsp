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
  (let ((result (gen-my-sum input)))
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
  (test-eraser "eraser-3" '(1 2 3 nil nil 3 a nil c nil a a) 3 '(3 a nil)))

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
  (test-num-orders "num-orders-2" '(1 2) 3)
  (test-num-orders "num-orders-2" '(1 1 1) 6))

;; 6 task

(defun check-qsort (input expected)
  (let ((result (quicksort input)))
    (equal result expected)))

(defun test-qsort (name input expected)
  (or (check-qsort input expected)
      (format T "~A failed.~%" name)))

(defun run-tests-qsort ()
  (test-qsort "quicksort-1" '(0 1 9 5 8 3 2) '(0 1 2 3 5 8 9))
  (test-qsort "quicksort-2" '(0 0 0 0 0 0 0 -1) '(-1 0 0 0 0 0 0 0)))
