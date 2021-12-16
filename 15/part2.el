;; this is very similar to part 1 but due to the input size I needed to pull out the big guns
;; and import an actual priority queue (aka Heap aka self-sorting tree) instead of using a hash table
(require 'heap)

(defconst aoc-start (vector 0 0))

(defun aoc-string-to-char-vec (str)
  (seq-into
   (seq-map
    'string-to-number
    (seq-filter
     (lambda (s) (and (not (equal s "")) (not (equal s "\n"))))
     (split-string str "")))
   'vector))

(defun aoc-inc-item (i)
  (if (> (+ i 1) 9) 1 (+ i 1)))

(defun aoc-parse-row (line)
  (let ((row (list (aoc-string-to-char-vec line))))
    (dotimes (_n 4)
      (setq row (cons (seq-map 'aoc-inc-item (car row)) row)))
    (seq-reduce 'append (seq-reverse row) '())))

(defun aoc-parse-grid-two ()
  (beginning-of-buffer)
  (let (rows '())
    (while (not (eobp))
      (setq rows (cons (aoc-parse-row (thing-at-point 'line nil)) rows))
      (forward-line 1))
    (let ((g (list (seq-reverse rows))))
      (dotimes (_n 4)
        (setq g (cons (seq-map (lambda (r) (seq-map 'aoc-inc-item r)) (car g)) g)))
      (setq g (seq-reduce 'append (seq-reverse g) '()))
      (seq-into (seq-map (lambda (r) (seq-into r 'vector)) g) 'vector))))

(defun aoc-edge (grid row-idx col-idx)
  (let ((cost (aref (aref grid row-idx) col-idx)))
    (list 'coords (vector row-idx col-idx) 'cost cost)))

(defun aoc-neighbors (grid coords)
  (let ((row-idx (aref coords 0))
        (col-idx (aref coords 1)))
    (seq-map
     (lambda (coord) (aoc-edge grid (aref coord 0) (aref coord 1)))
     (seq-filter
      (lambda (loc)
        (and (>= (aref loc 0) 0)
             (>= (aref loc 1) 0)
             (< (aref loc 0) (length grid))
             (< (aref loc 1) (length (aref grid 0)))))
      (list
       (vector (- row-idx 1) col-idx)
       (vector (- row-idx 1) col-idx)
       (vector row-idx (- col-idx 1))
       (vector row-idx (+ col-idx 1))
       (vector (+ row-idx 1) col-idx))))))

(defun aoc-pqueue-update (pqueue coords cost from seen)
  (if (not (gethash coords seen nil))
      (progn
        (heap-add pqueue (list 'coords coords 'cost cost 'from from))
        (puthash coords t seen))
    (heap-modify
     pqueue
     (lambda (d) (and (equal (plist-get d 'coords) coords) (< cost (plist-get d 'cost))))
     (list 'coords coords 'cost cost 'from from))))

(defun aoc-pqueue-visited (pqueue coords)
  (let ((coord-info (gethash coords pqueue)))
    (plist-put coord-info 'visited t)))

(defun aoc-pqueue-min-unvisited (pqueue)
  (heap-delete-root pqueue))

(defun aoc-heap-compare (a b)
  (< (plist-get a 'cost) (plist-get b 'cost)))

(defun aoc-find-shortest-path (grid start destination)
  (let* ((node-count (* (length grid) (length (aref grid 0))))
         (pqueue (make-heap 'aoc-heap-compare))
         (visited (make-hash-table :test 'equal :size node-count))
         (seen (make-hash-table :test 'equal :size node-count))
         (result nil))
    (aoc-pqueue-update pqueue start 0 nil seen)
    (while (not result)
      (let* ((current-node (aoc-pqueue-min-unvisited pqueue))
             (current-coords (plist-get current-node 'coords))
             (current-cost (plist-get current-node 'cost))
             (neighbors (aoc-neighbors grid current-coords)))
        (puthash current-coords t visited)
        (if (equal destination current-coords)
            (setq result current-cost)
          (seq-do
           (lambda (n)
             (let ((n-coords (plist-get n 'coords))
                   (n-cost (plist-get n 'cost)))
               (if (not (gethash n-coords visited))
                   (aoc-pqueue-update pqueue n-coords (+ current-cost n-cost) current-coords seen))))
           neighbors))))
    result))

(defun aoc-fifteen-two ()
  (interactive)
  (let* ((grid (aoc-parse-grid-two))
         (destination (vector (- (length grid) 1) (- (length (aref grid 0)) 1))))
    (message "%d" (aoc-find-shortest-path grid aoc-start destination))))
