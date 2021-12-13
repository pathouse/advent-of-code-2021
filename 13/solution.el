(defun aoc-parse-point (line)
  (seq-into
   (seq-map
    'string-to-number
    (split-string line ","))
   'vector))

(defun aoc-parse-fold (line)
  (let ((parts (split-string (car (seq-reverse (split-string line " "))) "=")))
    (list 'axis (car parts) 'value (string-to-number (car (cdr parts))))))

(defun aoc-parse-line (line)
  (if (string-match-p "," line)
      (aoc-parse-point line)
    (aoc-parse-fold line)))

(defun aoc-parse-input ()
  (beginning-of-buffer)
  (let ((points '())
        (folds '()))
    (while (not (eobp))
      (let ((line (aoc-parse-line (thing-at-point 'line nil))))
        (if (vectorp line)
            (setq points (cons line points))
          (setq folds (cons line folds))))
      (forward-line 1))
    (list 'points points 'folds folds)))

(defun aoc-apply-fold (fold points)
  (let ((axis (plist-get fold 'axis))
        (value (plist-get fold 'value)))
    (seq-uniq
     (if (equal axis "x")
         (aoc-fold-left points value)
       (aoc-fold-up points value)))))

(defun aoc-fold-up (points y)
  (seq-map
   (lambda (p)
     (let ((point-x (aref p 0))
           (point-y (aref p 1)))
       (if (> point-y y)
           (vector point-x (- point-y (* 2 (- point-y y))))
         p)))
   points))

(defun aoc-fold-left (points x)
  (seq-map
   (lambda (p)
     (let ((point-x (aref p 0))
           (point-y (aref p 1)))
       (if (> point-x x)
           (vector (- point-x (* 2 (- point-x x))) point-y)
         p)))
   points))

(defun aoc-thirteen-one ()
  (interactive)
  (let* ((input (aoc-parse-input))
         (first-fold (car (seq-reverse (plist-get input 'folds))))
         (points (plist-get input 'points)))
    (message "%s" points)
    (message "%s" first-fold)
    (message "%s" (length (aoc-apply-fold first-fold points)))))

(defun aoc-draw-points (points)
  (beginning-of-buffer)
  (erase-buffer)
  (message "%s" points)
  (let ((max-x (seq-max (seq-map (lambda (p) (aref p 0)) points)))
        (max-y (seq-max (seq-map (lambda (p) (aref p 1)) points))))
    (seq-do
     (lambda (r)
       (seq-do
        (lambda (c)
          (if (seq-contains-p points (vector c r) 'equal)
              (insert (propertize "X" 'face '(:foreground "#46A8FF")))
            (insert " ")))
        (number-sequence 0 max-x))
       (newline))
     (number-sequence 0 max-y))))

(defun aoc-thirteen-two ()
  (interactive)
  (let* ((input (aoc-parse-input))
         (folds (seq-reverse (plist-get input 'folds)))
         (points (plist-get input 'points))
         (buf (get-buffer-create "aoc-day-thirteen")))
    (select-window (split-window-vertically))
    (switch-to-buffer buf)
    (aoc-draw-points
     (seq-reduce (lambda (p fold) (aoc-apply-fold fold p)) folds points))))
