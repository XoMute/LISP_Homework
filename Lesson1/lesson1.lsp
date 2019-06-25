;; 1 task

(defun sum (lst)
  (if (null lst)
      0
      (progn
	(assert (numberp (car lst)))
	(+ (car lst) (sum (cdr lst))))))

;; 2 task

(defun inc-lst (lst)
  (when lst (cons (1+ (car lst)) (inc-lst (cdr lst)))))

;; 3 task

(defun my-reverse (lst)
  (my-reverse-helper lst nil))

(defun my-reverse-helper (lst result)
  (if (null lst)
      result
      (my-reverse-helper (cdr lst) (cons (car lst) result))))

;; 4 task

(defun delete-repeats-helper (lst result)
  (let ((item (car lst)))
    (cond ((null lst) result)
	  ((find item result) (delete-repeats-helper (cdr lst) result))
	  (T (delete-repeats-helper (cdr lst) (cons item result))))))

(defun delete-repeats (lst)
  (reverse (delete-repeats-helper lst nil)))

;;; TESTS
;; 1 task
    
(defun check-sum (input expected)
  (let ((result (sum input)))
    (equal result expected)))

(defun test-sum (name input expected)
  (or (check-sum input expected)
      (format T "~A failed~%" name)))

(defun run-sum-tests()
  (test-sum "sum-1" '(1 2 3) 6)
  (test-sum "sum-2" nil 0)
  (test-sum "sum-3" '(-1 -2 -3) -6)
  (test-sum "sum-4" '(1 1 1 1 1 1) 6))

;; 2 task

(defun check-inc-lst (input expected)
  (let ((result (inc-lst input)))
    (equal result expected)))

(defun test-inc-lst (name input expected)
  (or (check-inc-lst input expected)
      (format T "~A falied~%" name)))

(defun run-inc-lst-tests ()
  (test-inc-lst "inc-lst-1" '(1 2 3) '(2 3 4))
  (test-inc-lst "inc-lst-2" '(1) '(2))
  (test-inc-lst "inc-lst-3" '(1 1 1 1 1 1) '(2 2 2 2 2 2))
  (test-inc-lst "inc-lst-4" nil nil))

;; 3 task
  
(defun check-my-reverse (input expected)
  (let ((result (my-reverse input)))
    (equal result expected)))

(defun test-my-reverse (name input expected)
  (or (check-my-reverse input expected)
      (format T "~A failed~%" name)))

(defun run-my-reverse-tests ()
  (test-my-reverse "my-reverse-1" '(1 2 3) '(3 2 1))
  (test-my-reverse "my-reverse-2" '(1) '(1))
  (test-my-reverse "my-reverse-3" '(a b c) '(c b a))
  (test-my-reverse "my-reverse-4" nil nil))

;; 4 task

(defun check-delete-repeats (input expected)
  (let ((result (delete-repeats input)))
    (equal result expected)))

(defun test-delete-repeats (name input expected)
  (or (check-delete-repeats input expected)
      (format T "~A failed~%" name)))

(defun run-delete-repeats-tests ()
  (test-delete-repeats "delete-repeats-1" '(1 2 2 3) '(1 2 3))
  (test-delete-repeats "delete-repeats-2" '(2 2 2 2 2 2 2) '(2))
  (test-delete-repeats "delete-repeats-3" '(1 2 3 4 5) '(1 2 3 4 5))
  (test-delete-repeats "delete-repeats-4" '(1 1 1 2 2 1 3 3 2 3 1) '(1 2 3))
  (test-delete-repeats "delete-repeats-5" nil nil))

;;;;

(defun run-all-tests ()
  (run-sum-tests)
  (run-inc-lst-tests)
  (run-my-reverse-tests)
  (run-delete-repeats-tests))
