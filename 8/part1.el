(defconst aoc-number-lookup
  (vector
   (vector "a" "b" "c" "e" "f" "g")     ;; 0
   (vector "c" "f")                     ;; 1
   (vector "a" "c" "d" "e" "g")         ;; 2
   (vector "a" "c" "d" "f" "g")         ;; 3
   (vector "b" "c" "d" "f")             ;; 4
   (vector "a" "b" "d" "f" "g")         ;; 5
   (vector "a" "b" "d" "e" "f" "g")     ;; 6
   (vector "a" "c" "f")                 ;; 7
   (vector "a" "b" "c" "d" "e" "f" "g") ;; 8
   (vector "a" "b" "c" "d" "f" "g")))   ;; 9

(defun aoc-parse-scrambled-signals ()
  (beginning-of-buffer)
  (let ((signals '()))
    (while (not (eobp))
      (let ((line (seq-filter (lambda (s) (not (equal s ""))) (split-string (thing-at-point 'line nil) " | "))))
        (setq signals
              (cons
               (list
                'patterns
                (split-string (car line) " ")
                'output
                (mapcar (lambda (s) (replace-regexp-in-string "\n" "" s)) (split-string (car (cdr line)) " ")))
               signals)))
      (forward-line))
    signals))

(defun aoc-count-output-digits ()
  (interactive)
  (let ((input (aoc-parse-scrambled-signals)))
    (message "%d" (seq-reduce
                   (lambda (acc output)
                     (+ acc (seq-count (lambda (digit) (seq-contains-p '[2 3 4 7] (length digit))) output)))
                   (mapcar (lambda (sig) (plist-get sig 'output)) input)
                   0))))

(defun aoc-match-by-segments (candidates mapping characters)
  (seq-find
   (lambda (candidate)
     (seq-every-p
      (lambda (char)
        (let ((prior-matches (gethash char mapping nil)))
          (if (eq prior-matches nil)
              t
            (seq-some (lambda (prior) (seq-contains-p candidate prior)) prior-matches))))
      characters))
   candidates))

(defun aoc-potential-match (mapping characters)
  (let ((count-matches (seq-filter (lambda (vec) (eq (length vec) (length characters))) aoc-number-lookup)))
    (if (eq (length count-matches) 1)
        (car count-matches)
      (aoc-match-by-segments count-matches mapping characters))))

;; TODO FIXME
(defun aoc-update-mappings (match mapping chars)
  (if (eq (car chars) nil)
      mapping
    (progn
      (let ((current-char (car chars))
            (matched-chars
             (seq-difference match (mapcan (lambda (c) (gethash c mapping '())) (cdr chars)))))
        (let ((current-matches (gethash current-char mapping nil)))
          (if (eq current-matches nil)
              (puthash current-char matched-chars mapping)
            (puthash current-char (seq-intersection current-matches matched-chars) mapping))
          (aoc-update-mappings current-matches mapping (cdr chars)))))))

(defun aoc-map-signals (patterns)
  (let ((mapping (make-hash-table)))
    (seq-do
     (lambda (sig)
       (let ((chars (seq-filter (lambda (s) (not (equal s ""))) (split-string sig ""))))
         (let ((potential-match (aoc-potential-match mapping chars)))
           (message "scrambled: %s\n match: %s\n mappings: %s\n" chars potential-match mapping)
           (aoc-update-mappings potential-match mapping chars))))
     (seq-sort-by 'length #'< patterns))
    mapping))

(defun aoc-solve-signals ()
  (interactive)
  (let ((input (aoc-parse-scrambled-signals)))
    (aoc-map-signals (plist-get (car input) 'patterns))))
