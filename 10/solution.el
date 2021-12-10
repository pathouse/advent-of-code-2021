(defconst aoc-opening-chars (list "(" "[" "{" "<"))

(defconst aoc-closing-match
  (let ((map (make-hash-table :test 'equal)))
    (puthash "(" ")" map)
    (puthash "[" "]" map)
    (puthash "{" "}" map)
    (puthash "<" ">" map)
    map))

(defconst aoc-error-scores
  (let ((map (make-hash-table :test 'equal)))
    (puthash ")" 3 map)
    (puthash "]" 57 map)
    (puthash "}" 1197 map)
    (puthash ">" 25137 map)
    map))

(defconst aoc-completion-scores
  (let ((map (make-hash-table :test 'equal)))
    (puthash ")" 1 map)
    (puthash "]" 2 map)
    (puthash "}" 3 map)
    (puthash ">" 4 map)
    map))

(defun aoc-line-syntax-error (line stack)
  (let ((next-char (car line))
        (prev-char (car stack)))
    (cond ((eq next-char nil) stack)
          ((seq-contains-p aoc-opening-chars next-char)
           (aoc-line-syntax-error (cdr line) (cons next-char stack)))
          ((equal next-char (gethash prev-char aoc-closing-match))
           (aoc-line-syntax-error (cdr line) (cdr stack)))
          (t
           ;; (message "Expected %s but found %s instead" (gethash prev-char aoc-closing-match) next-char)
           next-char))))

(defun aoc-parse-subsystem-line (line)
  (seq-filter
   (lambda (c) (and (not (equal c "")) (not (equal c "\n"))))
   (split-string line "")))

(defun aoc-parse-navigation-subsystem ()
  (beginning-of-buffer)
  (let ((lines '()))
    (while (not (eobp))
      (setq lines (cons (aoc-parse-subsystem-line (thing-at-point 'line nil)) lines))
      (forward-line 1))
    lines))

(defun aoc-compute-syntax-error-score ()
  (interactive)
  (let ((input (aoc-parse-navigation-subsystem)))
    (message
     "%d"
     (seq-reduce
      (lambda (acc line)
        (let ((err (aoc-line-syntax-error line '())))
          (+ acc (gethash err aoc-error-scores 0))))
      input
      0))))

(defun aoc-completion-score (stack)
  (seq-reduce
   (lambda (acc c)
     (let* ((closing-char (gethash c aoc-closing-match))
            (score (gethash closing-char aoc-completion-scores)))
       (+ (* 5 acc) score)))
   stack
   0))

(defun aoc-fix-incomplete-lines ()
  (interactive)
  (let* ((input (aoc-parse-navigation-subsystem))
         (syntax-results (seq-map (lambda (line) (aoc-line-syntax-error line '())) input))
         (incomplete (seq-filter (lambda (result) (listp result)) syntax-results))
         (completion-scores (seq-into (seq-sort '< (seq-map 'aoc-completion-score incomplete)) 'vector))
         (winner-idx (/ (length completion-scores) 2))
         (winner (aref completion-scores winner-idx)))
    (message "%d" winner)))
