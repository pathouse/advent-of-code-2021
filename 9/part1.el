(defun aoc-line-to-heights (line)
  (seq-into
   (seq-map
    'string-to-number
    (seq-filter
     (lambda (s) (and (not (equal s "")) (not (equal s "\n"))))
     (split-string line "")))
   'vector))

(defun aoc-parse-input ()
  (beginning-of-buffer)
  (let ((heightmap (list)))
    (while (not (eobp))
      (let ((row (aoc-line-to-heights (thing-at-point 'line nil))))
        (setq heightmap (cons row heightmap)))
      (forward-line 1))
    (seq-into (seq-reverse heightmap) 'vector)))

(defun aoc-neighbors (row-idx col-idx heightmap)
  (let ((locations (aoc-gen-adjacent-locations row-idx col-idx heightmap)))
    (seq-map (lambda (l) (aoc-height-at-location l heightmap)) locations)))

(defun aoc-height-at-location (loc heightmap)
  (aref (aref heightmap (aref loc 0)) (aref loc 1)))

(defun aoc-gen-adjacent-locations (row-idx col-idx heightmap)
  (seq-filter
   (lambda (loc)
     (and (>= (aref loc 0) 0)
          (>= (aref loc 1) 0)
          (< (aref loc 0) (length heightmap))
          (< (aref loc 1) (length (aref heightmap 0)))))
   (list (vector row-idx (+ col-idx 1))
         (vector row-idx (- col-idx 1))
         (vector (+ row-idx 1) col-idx)
         (vector (- row-idx 1) col-idx))))

(defun aoc-collect-low-points (heightmap)
  (let ((low-points '()))
    (seq-do-indexed
     (lambda (row row-idx)
       (seq-do-indexed
        (lambda (elt col-idx)
          (if (seq-every-p (lambda (n) (< elt n)) (aoc-neighbors row-idx col-idx heightmap))
              (setq low-points (cons elt low-points))))
        row))
     heightmap)
    low-points))

(defun aoc-collect-low-point-locs (heightmap)
  (let ((low-point-locs '()))
    (seq-do-indexed
     (lambda (row row-idx)
       (seq-do-indexed
        (lambda (elt col-idx)
          (if (seq-every-p (lambda (n) (< elt n)) (aoc-neighbors row-idx col-idx heightmap))
              (setq low-point-locs (cons (vector row-idx col-idx) low-point-locs))))
        row))
     heightmap)
    low-point-locs))

(defun aoc-sum-lowpoint-risk-levels ()
  (interactive)
  (let ((heightmap (aoc-parse-input)))
    (let ((low-points (aoc-collect-low-points heightmap)))
      (message
       "%d"
       (seq-reduce
        (lambda (acc elt) (+ acc (+ elt 1)))
        (aoc-collect-low-points heightmap)
        0)))))

(defun aoc-basins-in-row (row row-idx)
  (let ((row-basins '())
        (current-basin '()))
    (seq-do-indexed
     (lambda (elt col-idx)
       (if (eq elt 9)
           (progn
             (if (> (length current-basin) 0)
                 (setq row-basins (cons current-basin row-basins)))
             (setq current-basin '()))
         (setq current-basin (cons (vector row-idx col-idx) current-basin))))
     row)
    (if (> (length current-basin) 0)
        (cons current-basin row-basins)
      row-basins)))

(defun aoc-adjacent-row-locs (row-idx basin)
  (let ((adj-row-indices (list (+ row-idx 1) (- row-idx 1))))
    (seq-filter (lambda (loc) (seq-contains-p adj-row-indices (aref loc 0))) basin)))

(defun aoc-adjacent-basinp (basin-a basin-b)
  (let* ((row-idx (aref (car basin-a) 0))
         (adj-row-locs (aoc-adjacent-row-locs row-idx basin-b))
         (col-idxs (seq-map (lambda (loc) (aref loc 1)) basin-a))
         (adj-col-idxs (seq-map (lambda (loc) (aref loc 1)) adj-row-locs)))
    (> (length (seq-intersection col-idxs adj-col-idxs)) 0)))

(defun aoc-multirow-adjacent-basinp (basin-a basin-b)
  (seq-some (lambda (loc) (aoc-adjacent-basinp (list loc) basin-b)) basin-a))

(defun aoc-collect-row-basins (heightmap)
  (seq-reduce
   (lambda (acc row)
     (append (aoc-basins-in-row row (seq-position heightmap row)) acc))
   heightmap
   '()))

(defun aoc-basin-contains-locp (loc basin)
  (seq-some (lambda (loc2) (equal loc2 loc)) basin))

(defun aoc-consolidate-basins (low-points row-basins)
  (let ((row-basins-copy (copy-seq row-basins)))
    (seq-reduce
     (lambda (acc low-point)
       (let ((low-point-basin (seq-find (apply-partially 'aoc-basin-contains-locp low-point) row-basins-copy)))
         (setq row-basins-copy (seq-remove (lambda (b) (equal b low-point-basin)) row-basins-copy))
         (let ((adj-basin (seq-find (apply-partially 'aoc-multirow-adjacent-basinp low-point-basin) row-basins-copy)))
           (while (not (eq nil adj-basin))
             (setq low-point-basin (append adj-basin low-point-basin))
             (setq row-basins-copy (seq-remove (lambda (b) (equal b adj-basin)) row-basins-copy))
             (setq adj-basin (seq-find (apply-partially 'aoc-multirow-adjacent-basinp low-point-basin) row-basins-copy)))
           (cons low-point-basin acc))))
     low-points
     '())))

(defun aoc-collect-basins ()
  (interactive)
  (let* ((heightmap (aoc-parse-input))
        (low-points (aoc-collect-low-point-locs heightmap))
        (row-basins (aoc-collect-row-basins heightmap))
        (basins (aoc-consolidate-basins low-points row-basins)))
    (message
     "%d"
     (seq-reduce
      '*
      (seq-map
       'length
       (seq-take
        (seq-sort-by 'length '> basins)
        3))
      1))))
