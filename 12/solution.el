(defun aoc-parse-graph-edge (line)
  (let ((nodes (mapcar (lambda (s) (replace-regexp-in-string "\n" "" s)) (split-string line "-"))))
    (list 'a (car nodes) 'b (car (cdr nodes)))))

(defun aoc-update-node (nodes point-a point-b)
  (let ((edges (gethash point-a nodes '())))
    (if (and (not (equal point-a "end"))
             (not (equal point-b "start")))
        (puthash point-a (cons point-b edges) nodes))))

(defun aoc-parse-nodes ()
  (beginning-of-buffer)
  (let ((nodes (make-hash-table :test 'equal)))
    (while (not (eobp))
      (let* ((connection (aoc-parse-graph-edge (thing-at-point 'line nil)))
             (point-a (plist-get connection 'a))
             (point-b (plist-get connection 'b)))
        (aoc-update-node nodes point-a point-b)
        (aoc-update-node nodes point-b point-a))
      (forward-line 1))
    nodes))

(defun aoc-lowercase-nodep (node)
  (string= node (downcase node)))


(defun aoc-can-visitp (current-path node)
  (if (and (aoc-lowercase-nodep node)
           (seq-contains-p current-path node))
      nil
    t))

(defun aoc-can-visit-part-twop (current-path node)
  (let* ((l-nodes (seq-filter 'aoc-lowercase-nodep current-path))
         (l-node-counts (seq-reduce
                         (lambda (acc n)
                           (puthash (car n) (length (cdr n)) acc)
                           acc)
                         (seq-group-by 'identity l-nodes)
                         (make-hash-table :test 'equal))))
    (if (and (aoc-lowercase-nodep node)
             (seq-contains-p current-path node)
             (seq-some (lambda (n) (> (gethash n l-node-counts) 1)) l-nodes))
        nil
      t)))

(defun aoc-enumerate-node-paths (nodes visit-rule-func current-path node)
  (let* ((edges (gethash node nodes))
         (path (cons node current-path))
         (available-edges (seq-filter (apply-partially visit-rule-func path) edges)))
    (if (or (eq (length available-edges) 0)
            (equal nodes "end"))
        (mapconcat 'identity (seq-reverse path) ",")
      (seq-map
       (lambda (n) (aoc-enumerate-node-paths nodes visit-rule-func path n))
       available-edges))))

(defun aoc-enumerate-paths (nodes visit-rule-func)
  (let ((start (gethash "start" nodes)))
    (seq-map (apply-partially 'aoc-enumerate-node-paths nodes visit-rule-func '("start")) start)))

(defun aoc-good-pathp (path)
  (equal (car (seq-reverse (split-string path ","))) "end"))

(defun aoc-twelve-one ()
  (interactive)
  (let* ((nodes (aoc-parse-nodes))
         (paths (flatten-tree (aoc-enumerate-paths nodes 'aoc-can-visitp)))
         (good-paths (seq-filter 'aoc-good-pathp paths)))
    (message "%d" (length good-paths))))

(defun aoc-twelve-two ()
  (interactive)
  (let* ((nodes (aoc-parse-nodes))
         (paths (flatten-tree (aoc-enumerate-paths nodes 'aoc-can-visit-part-twop)))
         (good-paths (seq-sort 'string< (seq-uniq (seq-filter 'aoc-good-pathp paths) 'equal))))
    (message "%d" (length good-paths))))
