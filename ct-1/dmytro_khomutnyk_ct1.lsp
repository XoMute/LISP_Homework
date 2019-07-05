(defstruct node value childs)

;; 1 task

(defun create-tree (lst)
  "Create tree from list"
  (labels ((%helper (lst)
	     "Create nodes recursively"
	     (let ((node (make-node :value (car lst))))
	       (mapcar (lambda (x) (if (atom x)
				       (push x (node-childs node))
				       (push (%helper x) (node-childs node))))
		       (reverse (cdr lst)))
	       node)))
    (%helper lst)))

		 
;; 2 task

(defun print-tree (tree)
  "Returns tree as s-expr"
  (labels ((%helper (node)
	     "Recursive function to 'print' each node with its childs"
	     (if (node-p node)
		 (cons (node-value node) (mapcar #'%helper (node-childs node)))
		 node)))
    (%helper tree)))
  
;; 3 task

(defun dfs-pre-order (tree &key (key #'identity))
  (labels ((%helper (node)
	     (if (node-p node) 
		 (cons (node-value node) (mapcar #'%helper (node-childs node)))
		 (funcall key node))))
    (%helper tree)))

;; 4 task

(defun dfs-post-order (tree &key (key #'identity))
  (labels ((%helper (node)
	     (if (node-p node)
		 (append (mapcar #'%helper (node-childs node)) (list (node-value node)))
		 (funcall key node))))
    (%helper tree)))

;; 5 task

(defun bfs (tree &key (key #'identity))
  (labels ((%helper (node)
	     "This function recursively traverses through each child of a current node. Firstly it prints their value and then calls herself on a childs, that are also nodes."
	     (if (node-p node)
		 (append (list (node-value node))
			 (mapcar (lambda (x)
				   (if (node-p x)
				       (node-value x)
				       (funcall key x)))
				 (node-childs node))
			 (mapcar (lambda (x)
				   (mapcar #'%helper (node-childs x)))
				 (remove-if-not #'node-p (node-childs node))))
		    (funcall key node))))
    (%helper tree)))

;; 6 task

(defun map-tree (tree traversal &key (key #'identity))
  (create-tree (funcall traversal tree :key key)))

;; 7 task

(defun calculate-tree (tree)
  (labels ((%helper (node)
	     (if (node-p node)
		 (apply (node-value node) (mapcar #'%helper (node-childs node)))
		 node)))
    (%helper tree)))

;;; TESTS

;; 1-2 tasks

(defun check-create-tree (input expected)
  (let ((result (print-tree (create-tree input))))
    (equal result expected)))


(defun test-create-tree (name input expected)
  (or (check-create-tree input expected)
      (format t "~A failed.~%" name)))

(defun run-create-tree-tests ()
  (test-create-tree "create-tree-1" '(1 2 3) '(1 2 3))
  (test-create-tree "create-tree-2" '(+ 1 2 (- 3 4)) '(+ 1 2 (- 3 4)))
  (test-create-tree "create-tree-3" nil '(nil))
  (test-create-tree "create-tree-4" '(+ 1 2 (* 3 4 5) (- 10 2)) '(+ 1 2 (* 3 4 5) (- 10 2))))

;; 3 task

(defun check-pre-order-dfs (input expected)
  (let ((result (dfs-pre-order (create-tree input))))
    (equal result expected)))

(defun test-pre-order-dfs (name input expected)
  (or (check-pre-order-dfs input expected)
      (format t "~A failed.~%" name)))

(defun run-pre-order-dfs-tests ()
  (test-pre-order-dfs "pre-order-dfs-1" '(1 2 3) '(1 2 3))
  (test-pre-order-dfs "pre-order-dfs-2" '(+ 1 2 (- 3 4)) '(+ 1 2 (- 3 4)))
  (test-pre-order-dfs "pre-order-dfs-3" nil '(nil))
  (test-pre-order-dfs "pre-order-dfs-4" '(+ 1 2 (* 3 4 5) (- 10 2)) '(+ 1 2 (* 3 4 5) (- 10 2))))

;; 4 task

(defun check-post-order-dfs (input expected)
  (let ((result (dfs-post-order (create-tree input))))
    (equal result expected)))

(defun test-post-order-dfs (name input expected)
  (or (check-post-order-dfs input expected)
      (format t "~A failed.~%" name)))

(defun run-post-order-dfs-tests ()
  (test-post-order-dfs "post-order-dfs-1" '(1 2 3) '(2 3 1))
  (test-post-order-dfs "post-order-dfs-2" '(+ 1 2 (- 3 4)) '(1 2 (3 4 -) +))
  (test-post-order-dfs "post-order-dfs-3" nil '(nil))
  (test-post-order-dfs "post-order-dfs-4" '(+ 1 2 (* 3 4 5) (- 10 2)) '(1 2 (3 4 5 *) (10 2 -) +)))

;; 5 task

(defun check-bfs (input expected)
  (let ((result (bfs (create-tree input))))
    (equal result expected)))

(defun test-bfs (name input expected)
  (or (check-bfs input expected)
      (format t "~A failed.~%" name)))

(defun run-bfs-tests ()
  (test-bfs "bfs-1" '(1 2 3) '(1 2 3))
  (test-bfs "bfs-2" '(+ 1 2 (- 3 4)) '(+ 1 2 - (3 4)))
  (test-bfs "bfs-3" nil '(nil))
  (test-bfs "bfs-4" '(+ 1 2 (* 3 4 5) (- 10 2)) '(+ 1 2 * - (3 4 5) (10 2))))

;; 6 task

(defun check-map-tree (input trav key expected)
  (let ((result (print-tree (map-tree (create-tree input) trav :key key))))
    (equal result expected)))

(defun test-map-tree (name input trav key expected)
  (or (check-map-tree input trav key expected)
      (format t "~A failed.~%" name)))

(defun run-map-tree-tests ()
  (test-map-tree "map-tree-1" '(+ 1 2 (* 3 4 5)) #'dfs-pre-order #'identity '(+ 1 2 (* 3 4 5)))
  (test-map-tree "map-tree-2" '(+ 1 2 (* 3 4 5)) #'dfs-pre-order #'1+ '(+ 2 3 (* 4 5 6)))
  (test-map-tree "map-tree-3" '(+ 1 2 (* 3 4 5)) #'dfs-post-order #'1- '(0 1 (2 3 4 *) +))
  (test-map-tree "map-tree-4" '(+ 1 2 (* 3 4 5)) #'bfs (lambda (x) (* x x)) '(+ 1 4 * (9 16 25))))

;; 7 task

(defun check-calculate-tree (input expected)
  (let ((result (calculate-tree (create-tree input))))
    (= result expected)))

(defun test-calculate-tree (name input expected)
  (or (check-calculate-tree input expected)
      (format t "~A failed.~%" name)))

(defun run-calculate-tree-tests ()
  (test-calculate-tree "calculate-tree-1" '(+ 1 2) 3)
  (test-calculate-tree "calculate-tree-2" '(+ 1 2 (- 3 4 5)) -3)
  (test-calculate-tree "calculate-tree-3" '(+ 1 2 (* 3 4 5) (- 10 2)) 71))

;;-------------

(defun run-all-tests ()
  (run-create-tree-tests)
  (run-pre-order-dfs-tests)
  (run-post-order-dfs-tests)
  (run-bfs-tests)
  (run-map-tree-tests)
  (run-calculate-tree-tests))
