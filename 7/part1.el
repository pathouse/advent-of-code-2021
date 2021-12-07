(defun aoc-get-crab-positions ()
   (beginning-of-buffer)
   (mapcar
    'string-to-number
    (seq-filter
     (lambda (n) (not (equal n "")))
     (split-string (thing-at-point 'line nil) ","))))

(defun aoc-pick-the-middle (nums)
  (let ((sorted-vec (seq-into (seq-sort-by 'identity #'< nums) 'vector)))
    (let ((midpoint (/ (length sorted-vec) 2)))
      (aref sorted-vec midpoint))))

(defun aoc-compute-mean (nums)
  (round (/ (seq-reduce '+ nums 0) (float (length nums)))))

(defun aoc-compute-fuel-cost (positions dest)
  (seq-reduce #'+ (seq-map (lambda (p) (abs (- p dest))) positions) 0))

(defun aoc-compute-fuel-cost-two (positions)
  (seq-min
   (seq-map
    (lambda (dest) (seq-reduce (lambda (acc p) (+ (fuel-sum (abs (- p dest))) acc)) positions 0))
    ;; using the mean as the destination point worked for the example but not for my real input
    ;; so i'm just using it to generate a likely range for a more brute-force type solution
    ;; of computing all the fuel costs and picking the min
    (number-sequence (- (aoc-compute-mean positions) 1) (+ (aoc-compute-mean positions) 1)))))

(defun aoc-organize-crab-submarines ()
  (interactive)
  (let ((crabs (aoc-get-crab-positions))
        (buf (get-buffer-create "aoc-day-seven")))
    (select-window (split-window-vertically))
    (switch-to-buffer buf)
    (erase-buffer)
    (insert (format "%s" (aoc-compute-fuel-cost crabs (aoc-pick-the-middle crabs))))))

(defun aoc-organize-crab-submarines-two ()
  (interactive)
  (let ((crabs (aoc-get-crab-positions))
        (buf (get-buffer-create "aoc-day-seven")))
    (select-window (split-window-vertically))
    (switch-to-buffer buf)
    (erase-buffer)
    (insert (format "%d\n" (aoc-compute-fuel-cost-two crabs)))))

(defun fuel-sum (pos)
  (seq-reduce #'+ (number-sequence 0 pos) 0))
