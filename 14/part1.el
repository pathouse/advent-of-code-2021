(defun aoc-string-to-char-vec (str)
  (seq-into (seq-filter (lambda (s) (not (equal s ""))) (split-string str "")) 'vector))

(defun aoc-parse-rule (line)
  (seq-into (split-string line " -> ") 'vector))

(defun aoc-add-rule (rule rules)
  (puthash (aoc-string-to-char-vec (aref rule 0))
           (aref rule 1)
           rules))

(defun aoc-parse-input ()
  (beginning-of-buffer)
  (let ((template '())
        (rules (make-hash-table :test 'equal)))
    (setq template
          (aoc-string-to-char-vec (replace-regexp-in-string "\n" "" (thing-at-point 'line nil))))
    (forward-line 2)
    (while (not (eobp))
      (aoc-add-rule
       (aoc-parse-rule
        (replace-regexp-in-string "\n" "" (thing-at-point 'line nil)))
       rules)
      (forward-line 1))
    (list 'template template 'rules rules)))

(defun aoc-apply-rule (vec rules counts aoc-chars)
  (let ((replacements '()))
    (seq-do-indexed
     (lambda (elt idx)
       (setq replacements (cons elt replacements))
       (if (< idx (- (length vec) 1))
           (let* ((pair (vector elt (aref vec (+ 1 idx))))
                  (insert (gethash pair rules)))
             (aoc-inc-count counts insert aoc-chars)
             (setq replacements (cons insert replacements)))))
     vec)
    (seq-into (seq-reverse replacements) 'vector)))

(defun aoc-inc-count (counts char aoc-chars)
  (let ((idx (seq-position aoc-chars char)))
    (aset counts idx (+ 1 (aref counts idx)))))

(defun aoc-init-counts (counts template chars)
  (seq-do
   (lambda (elt)
     (aoc-inc-count counts elt chars))
   template))

(defun aoc-all-chars (rules)
  (let ((chars '()))
    (maphash (lambda (_k v) (setq chars (cons v chars))) rules)
    (seq-into (seq-sort 'string< (seq-uniq chars)) 'vector)))

(defun aoc-fourteen-one ()
  (interactive)
  (let* ((parsed-input (aoc-parse-input))
         (polymer (plist-get parsed-input 'template))
         (rules (plist-get parsed-input 'rules))
         (chars (aoc-all-chars rules))
         (counts (make-vector (length chars) 0)))
    (aoc-init-counts counts polymer chars)
    (dotimes (_n 10)
      (setq polymer (aoc-apply-rule polymer rules counts chars)))
    (message "length: %d" (length polymer))
    (message "answer: %d" (- (seq-max counts) (seq-min counts)))))
