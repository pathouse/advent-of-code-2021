(defun aoc-calc-most-common-bit (report-lines current-bit)
  (let ((lines (length report-lines))
        (sum (seq-reduce
              (lambda (acc line)
                (progn
                   (+ (string-to-number (nth current-bit line)) acc)))
              report-lines
              0)))
    (if (< sum (- lines sum)) "0" "1")))

(defun aoc-oxygen-rating-filter (most-common-bit current-bit line)
  (equal (nth current-bit line) most-common-bit))

(defun aoc-co2-rating-filter (most-common-bit current-bit line)
  (not (aoc-oxygen-rating-filter most-common-bit current-bit line)))

(defun aoc-find-rating (report-lines current-bit filter-func)
  (if (eq (length report-lines) 1)
      (string-to-number (mapconcat 'identity (car report-lines) "") 2)
    (aoc-find-rating
     (seq-filter (apply-partially filter-func (aoc-calc-most-common-bit report-lines current-bit) current-bit) report-lines)
     (+ 1 current-bit)
     filter-func)))

(defun aoc-calc-life-support-ratings ()
  "Calculate oxygen generator and CO2 scurbber ratings using diagnostic report"
  (interactive)
  (let ((report-lines (seq-filter
                       (lambda (line) (not (equal line nil)))
                       (mapcar
                        (lambda (line) (seq-filter (lambda (char) (not (equal char ""))) (split-string line "")))
                        (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n")))))
    (message
     "%s"
     (*
      (aoc-find-rating report-lines 0 'aoc-oxygen-rating-filter)
      (aoc-find-rating report-lines 0 'aoc-co2-rating-filter)))))
