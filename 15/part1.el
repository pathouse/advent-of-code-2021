(defconst aoc-start (vector 0 0))

(defun aoc-string-to-char-vec (str)
  (seq-into
   (seq-map
    'string-to-number
    (seq-filter
     (lambda (s) (and (not (equal s "")) (not (equal s "\n"))))
     (split-string str "")))
   'vector))

(defun aoc-parse-grid ()
  (beginning-of-buffer)
  (let (rows '())
    (while (not (eobp))
      (setq rows (cons (aoc-string-to-char-vec (thing-at-point 'line nil)) rows))
      (forward-line 1))
    (seq-into (seq-reverse rows) 'vector)))

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

(defun aoc-pqueue-update (pqueue coords cost from)
  (let ((current-vals (gethash coords pqueue (list 'cost most-positive-fixnum 'from nil 'visited nil))))
    (if (and (not (plist-get current-vals 'visited)) (< cost (plist-get current-vals 'cost)))
        (puthash coords (list 'cost cost 'from from 'visited nil) pqueue))))

(defun aoc-pqueue-visited (pqueue coords)
  (let ((coord-info (gethash coords pqueue)))
    (plist-put coord-info 'visited t)))

(defun aoc-manhattan-distance (start dest)
  (+ (abs (- (aref dest 0) (aref start 0)))
     (abs (- (aref dest 1) (aref start 1)))))

(defun aoc-pqueue-min-unvisited (pqueue dest)
  (let ((coords nil)
        (cost most-positive-fixnum))
    (maphash
     (lambda (k v)
       (let ((current-cost (plist-get v 'cost))
             (visited (plist-get v 'visited)))
         (if (and (not visited) (< current-cost cost))
             (progn
               (setq coords k)
               (setq cost current-cost)))))
     pqueue)
    coords))

(defun aoc-find-shortest-path (grid start destination)
  (let ((pqueue (make-hash-table :test 'equal :size (* (length grid) (length (aref grid 0)))))
        (result nil))
    (aoc-pqueue-update pqueue start 0 nil)
    (while (not result)
      (let* ((current-coords (aoc-pqueue-min-unvisited pqueue destination))
             (current-cost (plist-get (gethash current-coords pqueue) 'cost))
             (neighbors (aoc-neighbors grid current-coords)))
        (aoc-pqueue-visited pqueue current-coords)
        (if (equal destination current-coords)
            (setq result current-cost)
          (seq-do
           (lambda (n)
             (let ((n-coords (plist-get n 'coords))
                   (n-cost (plist-get n 'cost)))
               (aoc-pqueue-update pqueue n-coords (+ current-cost n-cost) current-coords)))
           neighbors))))
    result))

(defun aoc-fifteen-one ()
  (interactive)
  (let* ((grid (aoc-parse-grid))
         (destination (vector (- (length grid) 1) (- (length (aref grid 0)) 1))))
    (message "%d" (aoc-find-shortest-path grid aoc-start destination))))
