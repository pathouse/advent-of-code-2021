(defun aoc-parse-line-segments ()
  (beginning-of-buffer)
  (let ((segments '()))
    (while (not (eobp))
      (let ((current-seg (thing-at-point 'line nil)))
        (setq segments (cons (aoc-parse-line-segment current-seg) segments)))
      (forward-line 1))
    segments))

(defun aoc-parse-line-segment (str)
  (mapcar (lambda (p) (mapcar 'string-to-number (split-string p ","))) (split-string str " -> ")))



(defun aoc-horizontal-linep (segment)
  (let ((x (aoc-segment-x segment)))
    (eq (length (seq-uniq x)) 1)))

(defun aoc-vertical-linep (segment)
  (let ((y (aoc-segment-y segment)))
    (eq (length (seq-uniq y)) 1)))

;; (defun aoc-get-range (p1 p2)
;;   (if (> p1 p2)
;;       (number-sequence p2 p1)
;;     (number-sequence p1 p2)))

;; (defun aoc-segment-to-points (segment)
;;   (if (aoc-horizontal-linep segment)
;;       (aoc-h-segment-to-points segment)
;;     (aoc-v-segment-to-points segment)))

;; (defun aoc-h-segment-to-points (segment)
;;   (let ((a (car segment))
;;         (b (car (cdr segment))))
;;     (let ((x (car a))
;;           (y1 (car (cdr a)))
;;           (y2 (car (cdr b))))
;;       (mapcar (lambda (y) (list x y)) (aoc-get-range y1 y2)))))

;; (defun aoc-v-segment-to-points (segment)
;;   (let ((a (car segment))
;;         (b (car (cdr segment))))
;;     (let ((y (car (cdr a)))
;;           (x1 (car a))
;;           (x2 (car  b)))
;;       (mapcar (lambda (x) (list x y)) (aoc-get-range x1 x2)))))

(defun aoc-segment-y (segment)
  (let ((start (car segment))
        (end (car (cdr segment))))
    (sort (list (car (cdr start)) (car (cdr end))) #'<)))

(defun aoc-segment-x (segment)
  (let ((start (car segment))
        (end (car (cdr segment))))
    (sort (list (car start) (car end)) #'<)))

(defun calc-vertical-overlap (a b)
  (let ((a-y (aoc-segment-y a))
        (b-y (aoc-segment-y b)))
    (length (seq-intersection (apply 'number-sequence a-y) (apply 'number-sequence b-y)))))

(defun calc-horizontal-overlap (a b)
  (let ((a-x (aoc-segment-x a))
        (b-x (aoc-segment-x b)))
    (length (seq-intersection (apply 'number-sequence a-x) (apply 'number-sequence b-x)))))

(defun aoc-intersect-count (a b)
  (* (calc-horizontal-overlap a b) (calc-vertical-overlap a b)))

;; (defun aoc-intersect-count (a b)
;;   (cond ((and (aoc-vertical-linep a) aoc-vertical-linep b)
;;          (calc-vertical-overlap a b))
;;         ((and (aoc-horizontal-linep a) (aoc-horizontal-linep b))
;;          (calc-horizontal-overlap a b))
;;         (t (calc-perpendicular-intersect a b))))

(defun aoc-count-intersections (segments intersections)
  (if (eq nil segments)
      intersections
    (aoc-count-intersections
     (cdr segments)
     (+
      intersections
      (seq-reduce
       (lambda (acc other-segment) (+ acc (aoc-intersect-count (car segments) other-segment)))
       (cdr segments)
       0)))))

(defun aoc-count-overlapping-points ()
  (interactive)
  (let ((horizontal (seq-filter 'aoc-horizontal-linep (aoc-parse-line-segments)))
        (vertical (seq-filter 'aoc-vertical-linep (aoc-parse-line-segments)))
        (buf (get-buffer-create "day-five-output")))
    (select-window (split-window-vertically))
    (switch-to-buffer buf)
    (insert (format "%d" (aoc-count-intersections (append horizontal vertical) 0)))))

    ;; (let ((all-points
    ;;        (append
    ;;         (mapcan 'aoc-h-segment-to-points horizontal)
    ;;         (mapcan 'aoc-v-segment-to-points vertical))))
    ;;   (insert (format "%s" (seq-count (lambda (p) (> (length (cdr p)) 1)) (seq-group-by 'identity all-points)))))))
