(defun aoc-get-lanternfish-start ()
  (beginning-of-buffer)
  (let ((age-bins (vector 0 0 0 0 0 0 0 0 0))
        (inputs (mapcar
                 'string-to-number
                 (seq-filter
                  (lambda (n) (not (equal n "")))
                  (split-string (thing-at-point 'line nil) ",")))))
    (dolist (i inputs)
      (aset age-bins i (+ 1 (aref age-bins i))))
    age-bins))

(defun aoc-simulate-fish (current-pop)
  (let ((new-pop (vector 0 0 0 0 0 0 0 0 0)))
    (seq-do
     (lambda (age)
       (cond ((eq age 0)
              (aset new-pop 6 (+ (aref new-pop 6) (aref current-pop age)))
              (aset new-pop 8 (aref current-pop age)))
             (t (aset new-pop (- age 1) (+ (aref new-pop (- age 1)) (aref current-pop age))))))
     (number-sequence 0 8))
    new-pop))

(defun aoc-lanternfish-generations ()
  (interactive)
  (let ((fish-pop (aoc-get-lanternfish-start))
        (generations (string-to-number (read-string "generations: ")))
        (buf (get-buffer-create "aoc-day-six-part-one")))
    (select-window (split-window-vertically))
    (switch-to-buffer buf)
    (erase-buffer)
    (dotimes (n generations)
      (setq fish-pop (aoc-simulate-fish fish-pop)))
    (insert (format "Final count: %d" (seq-reduce #'+ fish-pop 0)))))
