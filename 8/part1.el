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
