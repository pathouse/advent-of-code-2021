;; second implementation of part 2 to be more emacsy by using separate windows and buffers to do calculations
;; instead of parsing the buffer into a list

(defun aoc-oxygen-ratingp (most-common-bit current-bit)
  (let ((line (split-string (thing-at-point 'line nil) "")))
    (equal (nth (+ 1 current-bit) line) most-common-bit)))

(defun aoc-co2-ratingp (most-common-bit current-bit)
  (let ((line (split-string (thing-at-point 'line nil) "")))
    (not (equal (nth (+ 1 current-bit) line) most-common-bit))))

(defun aoc-calc-most-common-bit (current-bit)
  (delete-blank-lines)
  (beginning-of-buffer)
  (let ((sum 0)
        (line-count (count-lines (point-min) (point-max))))
    (while (not (eobp))
      (beginning-of-line)
      (let ((n (string-to-number (string (char-after (+ current-bit (point)))))))
        (setq sum (+ n sum)))
      (forward-line 1))
    (message "current bit: %s, sum: %s" current-bit sum)
    (if (> (- line-count sum) sum) "0" "1")))

(defun aoc-calc-rating (win linep current-bit)
  (select-window win)
  (if (> (count-lines (point-min) (point-max)) 1)
      (let ((most-common-bit (aoc-calc-most-common-bit current-bit)))
        (message "most common: %s current: %s" most-common-bit current-bit)
        (beginning-of-buffer)
        (while (not (eobp))
          (if (not (funcall linep most-common-bit current-bit))
              (progn
                (delete-region (point) (point-at-eol))
                (delete-blank-lines))
            (forward-line 1)))
        (aoc-calc-rating win linep (+ 1 current-bit)))
    (let ((result (string-to-number (thing-at-point 'line nil) 2)))
      (beginning-of-buffer)
      (end-of-line)
      (insert (format " (%d)" result))
      result)))

(defun aoc-calc-life-support-with-viz ()
  (interactive)
  (let ((main-window (get-buffer-window))
        (oxy-window (split-window-below))
        (oxy-buffer (get-buffer-create "oxygen-rating"))
        (co2-window (split-window-below))
        (co2-buffer (get-buffer-create "co2-rating"))
        (diagnostic-report (buffer-string)))
    (select-window oxy-window)
    (switch-to-buffer oxy-buffer)
    (insert diagnostic-report)
    (select-window co2-window)
    (switch-to-buffer co2-buffer)
    (insert diagnostic-report)
    (let ((oxy-rate (aoc-calc-rating oxy-window 'aoc-oxygen-ratingp 0))
          (co2-rate (aoc-calc-rating co2-window 'aoc-co2-ratingp 0)))
      (message "oxy: %s, co2: %s" oxy-rate co2-rate))))
