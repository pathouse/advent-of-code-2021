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

(defconst aoc-reverse-number-lookup
  (vector
   "abcefg"   ;; 0
   "cf"       ;; 1
   "acdeg"    ;; 2
   "acdfg"    ;; 3
   "bcdf"     ;; 4
   "abdfg"    ;; 5
   "abdefg"   ;; 6
   "acf"      ;; 7
   "abcdefg"  ;; 8
   "abcdfg")) ;; 9


(defun aoc-parse-input ()
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

(defun aoc-candidate-by-lengthp (pattern candidate)
  "identifies whether CANDIDATE is a match for PATTERN based on length"
  (eq (length pattern) (length candidate)))

(defun aoc-candidate-by-matchesp (pattern matches candidate)
  "identifies whether CANDIDATE is a match for PATTERN based on previously identified MATCHES

  e.g. if PATTERN is ab and MATCHES is { 'a' => ('c' 'f') 'b' => ('c' 'f') } then CANDIDATE must include both c and f"
  (let ((matched '()))
    (seq-do
     (lambda (char)
       (let ((potential (seq-difference (gethash char matches '()) matched)))
         (if (not (eq potential nil))
             (setq matched
                   (cons
                    (seq-find (lambda (c) (seq-contains-p candidate c 'equal)) potential)
                    matched)))))
     pattern)
    (seq-every-p (lambda (m) (not (eq m nil))) matched)))

(defun aoc-find-candidate (pattern matches)
  (let ((candidates-by-length (seq-filter
                              (apply-partially 'aoc-candidate-by-lengthp pattern)
                              aoc-number-lookup)))
    (if (eq (length candidates-by-length) 1)
        (car candidates-by-length)
      (seq-find
       (apply-partially 'aoc-candidate-by-matchesp pattern matches)
       candidates-by-length))))

(defun aoc-all-matches-for-chars (chars matches)
  (seq-uniq (seq-mapcat (lambda (char) (gethash char matches '())) chars)))

(defun aoc-consolidate-matches (matches)
  (maphash
   (lambda (k v)
     (if (eq (length v) 1)
         (maphash
          (lambda (k2 v2)
            (if (and (not (equal k k2)) (seq-contains-p v2 (car v) 'equal))
                (puthash k2 (seq-difference v2 v 'equal) matches)))
          matches)))
   matches)
  matches)

(defun aoc-update-matches (pattern old-matches candidate)
  (let ((new-matches (copy-hash-table old-matches)))
    (seq-do
     (lambda (char)
       (let ((other-char-matches (aoc-all-matches-for-chars (seq-difference pattern (list char)) old-matches)))
         (if (eq (gethash char old-matches nil) nil)
             (let ((available-matches (seq-difference candidate other-char-matches)))
               (puthash char available-matches new-matches))
           (let ((available-matches (seq-intersection (gethash char old-matches) candidate)))
             (puthash char available-matches new-matches)))))
     pattern)
    (aoc-consolidate-matches new-matches)))

(defun aoc-string-to-chars (str)
  (seq-filter (lambda (c) (not (equal c ""))) (split-string str "")))

(defun aoc-unscramble-signal (patterns)
  (let ((matches (make-hash-table :test 'equal)))
    (seq-do
     (lambda (pattern)
       (let ((candidate (aoc-find-candidate pattern matches)))
         (setq matches (aoc-update-matches pattern matches candidate))))
     (seq-sort-by 'length '< (mapcar 'aoc-string-to-chars patterns)))
    matches))

(defun aoc-unscramble-output (matches output)
  (mapcar
   (lambda (o)
     (mapcar
      (lambda (c) (car (gethash c matches)))
      (aoc-string-to-chars o)))
   output))

(defun aoc-chars-to-string (chars)
  (seq-reduce 'concat (seq-sort 'string< chars) ""))

(defun aoc-unscramble-signals ()
  (interactive)
  (let ((input (aoc-parse-input)))
    (message
     "%d"
     (seq-reduce
      (lambda (acc line)
        (let ((matches (aoc-unscramble-signal (plist-get line 'patterns))))
          (let ((unscrambled-output (aoc-unscramble-output matches (plist-get line 'output))))
            (let ((output-number (string-to-number
                                  (seq-reduce
                                   'concat
                                   (mapcar
                                    (lambda (s) (number-to-string (seq-position aoc-reverse-number-lookup s 'equal)))
                                    (mapcar 'aoc-chars-to-string unscrambled-output))
                                   ""))))
              (+ acc output-number)))))
      input
      0))))
